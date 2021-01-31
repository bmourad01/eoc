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

enum {
  TYPE_INTEGER,
  TYPE_BOOLEAN,
  TYPE_VOID,
  TYPE_VECTOR,
  TYPE_ARROW,
  TYPE_UNK,
};

static bool is_pointer_type(uint64_t ty, bool rec) {
  switch (ty) {
  case TYPE_INTEGER:
  case TYPE_BOOLEAN:
  case TYPE_VOID:
  case TYPE_ARROW:
  case TYPE_UNK:
    return false;
  case TYPE_VECTOR:
    return true;
  default:
    // assume that ty points to another type information struct.
    // use 'rec' to see ultimately what type is being pointed to.
    return rec ? is_pointer_type(*(uint64_t *)ty, rec) : true;
  }
}

/*
 * input/output
 */

int64_t _read_int() {
  char buf[64];
  fgets(buf, sizeof(buf), stdin);
  buf[strcspn(buf, "\n")] = '\0';
  return atoll(buf);
}

static void print_value_aux(uint64_t *ty, int64_t val, bool nested) {
  uint64_t i, len, tyv, v;

  switch (tyv = ty[0]) {
  case TYPE_INTEGER:
    printf("%ld", val);
    return;
  case TYPE_BOOLEAN:
    printf("%s", val ? "#t" : "#f");
    return;
  case TYPE_VOID:
    printf("#<void>");
    return;
  case TYPE_VECTOR:
    if (!nested) {
      printf("'");
    }
    printf("#(");
    len = ty[1];
    for (i = 0; i < len; ++i) {
      tyv = ty[i + 2];
      v = ((int64_t *)val)[i + 1];
      // NOTE: there shouldn't be any complex types (e.g. Vector)
      // inside of `ty`. those should reside in their own type
      // information structures. thus, if this is a nested vector,
      // `tyv` will just be a pointer.
      if (!is_pointer_type(tyv, false)) {
        print_value_aux(&tyv, v, true);
      } else {
        print_value_aux((uint64_t *)tyv, v, true);
      }
      if ((i + 1) < len) {
        printf(" ");
      }
    }
    printf(")");
    return;
  case TYPE_ARROW:
    printf("#<function>");
    return;
  case TYPE_UNK:
    printf("#<unk:0x%016lX>", val);
    return;
  default:
    // assume again that this is a pointer
    print_value_aux((uint64_t *)tyv, val, nested);
    return;
  }
}

void _print_value(uint64_t *ty, int64_t val) {
  print_value_aux(ty, val, false);
  printf("\n");
}

/*
 * garbage collection
 */

static uint64_t _heap_size;
static void *_heap_base;
int64_t *_free_ptr;
static int64_t *_fromspace_begin;
int64_t *_fromspace_end;
static int64_t *_tospace_begin;
static int64_t *_tospace_end;
int64_t **_rootstack_begin;
static int64_t **_rootstack_end;

void _initialize(uint64_t rootstack_size, uint64_t heap_size) {
  _heap_size = heap_size;
  _heap_base = malloc(heap_size);
  assert(_heap_base);
  _fromspace_begin = (int64_t *)_heap_base;
  _fromspace_end = (int64_t *)((uint64_t)_fromspace_begin + (heap_size >> 1));
  _free_ptr = _fromspace_begin;
  _tospace_begin = _fromspace_end;
  _tospace_end = (int64_t *)((uint64_t)_fromspace_begin + heap_size);
  // it's important that we zero the entire rootstack so
  // that we can mark locations that are uninitialized
  _rootstack_begin = (int64_t **)calloc(rootstack_size, sizeof(uint8_t));
  assert(_rootstack_begin);
  _rootstack_end = (int64_t **)((uint64_t)_rootstack_begin + rootstack_size);
}

static int64_t *collect_copy(int64_t *obj) {
  uint64_t fwd, size;
  int64_t *new_obj;

  // has the object been copied yet?
  fwd = (uint64_t)*obj;
  if (fwd < (uint64_t)_fromspace_begin ||
      fwd >= (uint64_t)_fromspace_end) {
    size = (((int64_t *)*obj)[1] + 1) << 3;
    // copy the object
    new_obj = _free_ptr;
    memcpy(new_obj, obj, size);
    // bump the free pointer
    _free_ptr = (int64_t *)((uint64_t)_free_ptr + size);
    // mark it as being copied by storing the forwarding address
    // to where the type tag pointer used to be. these pointers
    // reside in different regions of memory, so we will be able
    // to distinguish them as seen above.
    *obj = (int64_t)new_obj;
  } else {
    new_obj = (int64_t *)fwd;
  }

  return new_obj;
}

static void cheney(int64_t **rootstack_ptr) {
  int64_t *p, **r, *tmp, *scan_ptr, *ty;
  uint64_t i, length;

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
  for (r = _rootstack_begin; r < rootstack_ptr; ++r) {
    if ((p = *r)) {
      *r = collect_copy(p);
    }
  }

  DBGPRINT("GC: copying reachable objects\n");

  // do a breadth-first search for all objects reachable from the root stack
  for (scan_ptr = _fromspace_begin; scan_ptr < _free_ptr;) {
    ty = (int64_t *)*scan_ptr;
    length = (uint64_t)ty[1];
    for (i = 0; i < length; ++i) {
      if (is_pointer_type(ty[i + 2], true)) {
        scan_ptr[i + 1] = (int64_t)collect_copy((int64_t *)scan_ptr[i + 1]);
      }
    }
    scan_ptr = (int64_t *)((uint64_t)scan_ptr + (((length + 1) << 3)));
  }
}

void _collect(int64_t **rootstack_ptr, uint64_t bytes) {
  int64_t *tmp;
  uint64_t size;

  cheney(rootstack_ptr);

  DBGPRINT("GC: checking for sufficient space\n");

  // check if we need to resize the heap
  // NOTE:
  //  we don't actually need to wrap this in a while loop.
  //  we could just round up the new size of the heap to the
  //  next power of 2 which will fit the requested amount of
  //  data. this is purely just to stress-test the GC.
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
