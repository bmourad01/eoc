#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define ENABLE_DEBUG

#ifdef ENABLE_DEBUG
#define DBGPRINT(x...) fprintf(stderr, x)
#else
#define DBGPRINT(x...)
#endif

typedef struct {
  union {
    struct {
      uint64_t forwarding : 1;
      uint64_t length : 6;
      uint64_t ptr_mask : 50;
      uint64_t reserved : 7;
    };
    uint64_t v;
  } tag;
  uint64_t int_mask;
  uint64_t bool_mask;
  uint64_t void_mask;
} vector_header_t;

enum {
  TAG_OFFSET = 0,
  INT_MASK_OFFSET,
  BOOL_MASK_OFFSET,
  VOID_MASK_OFFSET,

  TOTAL_TAG_OFFSET
};

#define VECTOR_ELEMENT(vec, i)                                                 \
  (*(int64_t *)((uint64_t)(vec) + ((i) * sizeof(int64_t)) +                    \
                sizeof(vector_header_t)))

#define LENGTH_BITS 6
#define PTRMASK_BITS 50

#define FORWARDING(x) (((uint64_t)(x)) & 1)
#define LENGTH(x) ((((uint64_t)(x)) >> 1) & ((1ULL << LENGTH_BITS) - 1))
#define PTRMASK(x) ((((uint64_t)(x)) >> 7) & ((1ULL << PTRMASK_BITS) - 1))

static uint64_t _heap_size;
static void *_heap_base;
int64_t *_free_ptr;
static int64_t *_fromspace_begin;
int64_t *_fromspace_end;
static int64_t *_tospace_begin;
static int64_t *_tospace_end;
int64_t **_rootstack_begin;
static int64_t **_rootstack_end;

int64_t _read_int() {
  char buf[64];
  fgets(buf, sizeof(buf), stdin);
  buf[strcspn(buf, "\n")] = '\0';
  return atoll(buf);
}

void _print_int(int64_t i) { printf("%ld\n", i); }

void _print_bool(int64_t i) {
  if (i) {
    printf("#t\n");
  } else {
    printf("#f\n");
  }
}

void _print_void() { printf("#<void>\n"); }

static void print_vector_aux(int64_t *vec, bool nested) {
  vector_header_t *hdr;
  int64_t ptr_mask, int_mask, bool_mask, void_mask, val;
  uint64_t i, length, bit;

  hdr = (vector_header_t *)vec;

  length = hdr->tag.length;
  ptr_mask = hdr->tag.ptr_mask;
  int_mask = hdr->int_mask;
  bool_mask = hdr->bool_mask;
  void_mask = hdr->void_mask;

  if (!nested) {
    printf("'");
  }
  printf("#(");
  for (i = 0; i < length; ++i) {
    val = VECTOR_ELEMENT(vec, i);
    bit = 1 << i;
    if (bit & ptr_mask) {
      print_vector_aux((int64_t *)val, true);
    } else if (bit & int_mask) {
      printf("%ld", val);
    } else if (bit & bool_mask) {
      if (val) {
        printf("#t");
      } else {
        printf("#f");
      }
    } else if (bit & void_mask) {
      printf("#<void>");
    }
    if (i < (length - 1)) {
      printf(" ");
    }
  }
  printf(")");
}

void _print_vector(int64_t *vec) {
  print_vector_aux(vec, false);
  printf("\n");
}

void _initialize(uint64_t rootstack_size, uint64_t heap_size) {
  _heap_size = heap_size;
  _heap_base = malloc(heap_size);
  assert(_heap_base);
  _fromspace_begin = (int64_t *)_heap_base;
  _fromspace_end = (int64_t *)((uint64_t)_fromspace_begin + (heap_size >> 1));
  _free_ptr = _fromspace_begin;
  _tospace_begin = _fromspace_end;
  _tospace_end = (int64_t *)((uint64_t)_fromspace_begin + heap_size);
  _rootstack_begin = (int64_t **)malloc(rootstack_size);
  assert(_rootstack_begin);
  _rootstack_end = (int64_t **)((uint64_t)_rootstack_begin + rootstack_size);
}

static int64_t *collect_copy(int64_t *obj) {
  uint64_t size;
  int64_t *new_obj;
  vector_header_t *vec;

  // has the object been copied yet?
  vec = (vector_header_t *)obj;
  if (vec->tag.forwarding) {
    size = (vec->tag.length << 3) + sizeof(vector_header_t);
    // copy the object
    new_obj = _free_ptr;
    memcpy(new_obj, obj, size);
    // bump the free pointer
    _free_ptr = (int64_t *)((uint64_t)_free_ptr + size);
    // mark it as being copied by storing the forwarding address
    // to where the tag used to be. we can do this because
    // it will set the least significant bit to 0 since all
    // of our pointers are aligned to even addresses.
    assert(!((uint64_t)new_obj & 1));
    vec->tag.v = (uint64_t)new_obj;
  } else {
    new_obj = (int64_t *)*obj;
  }

  return new_obj;
}

static void cheney(int64_t **rootstack_ptr) {
  int64_t *p, **r, *tmp, *scan_ptr;
  uint64_t i, length, ptr_mask;
  vector_header_t *vec;

  assert(rootstack_ptr >= _rootstack_begin && rootstack_ptr < _rootstack_end);

  // swap fromspace with tospace
  tmp = _fromspace_begin;
  _fromspace_begin = _tospace_begin;
  _tospace_begin = tmp;
  tmp = _fromspace_end;
  _fromspace_end = _tospace_end;
  _tospace_end = tmp;
  _free_ptr = _fromspace_begin;

  DBGPRINT("GC: copying roots\n");

  // copy all roots first
  for (r = rootstack_ptr; r >= _rootstack_begin; --r) {
    if ((p = *r)) {
      *r = collect_copy(p);
    }
  }

  DBGPRINT("GC: copying reachable objects\n");

  // do a breadth-first search for all objects reachable from the root stack
  for (scan_ptr = _fromspace_begin; scan_ptr < _free_ptr;) {
    vec = (vector_header_t *)scan_ptr;
    length = vec->tag.length;
    ptr_mask = vec->tag.ptr_mask;
    for (i = 0; i < length; ++i) {
      if (ptr_mask & (1 << i)) {
        VECTOR_ELEMENT(vec, i) =
            (int64_t)collect_copy((int64_t *)VECTOR_ELEMENT(vec, i));
      }
    }
    scan_ptr = (int64_t *)((uint64_t)scan_ptr +
                           ((length << 3) + sizeof(vector_header_t)));
  }
}

void _collect(int64_t **rootstack_ptr, uint64_t bytes) {
  int64_t *tmp;
  uint64_t size;

  cheney(rootstack_ptr);
  DBGPRINT("GC: checking for sufficient space\n");

  // check if we need to resize the heap
  while (((uint64_t)_free_ptr + bytes) >= (uint64_t)_fromspace_end) {
    DBGPRINT("GC: resizing the heap from %ld to %ld bytes\n", _heap_size,
             _heap_size << 1);
    // double the current size
    _heap_size <<= 1;
    // allocate a new heap and copy over the current fromspace
    tmp = (int64_t *)malloc(_heap_size);
    assert(tmp);
    size = (uint64_t)_fromspace_end - (uint64_t)_fromspace_begin;
    memcpy(tmp, _fromspace_begin, size);
    // update the new pointers
    _fromspace_begin = tmp;
    _fromspace_end = (int64_t *)((uint64_t)tmp + (_heap_size >> 1));
    _tospace_begin = _fromspace_end;
    _tospace_end = (int64_t *)((uint64_t)tmp + _heap_size);
    // run the GC again
    cheney(rootstack_ptr);
    // free the old heap and update
    free(_heap_base);
    _heap_base = tmp;
  }
}
