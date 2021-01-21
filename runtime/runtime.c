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

typedef enum {
  TYPE_INTEGER,
  TYPE_BOOLEAN,
  TYPE_VOID,
  TYPE_VECTOR,
} type_t;

static bool is_pointer_type(uint64_t ty) {
  switch (ty) {
  case TYPE_INTEGER:
  case TYPE_BOOLEAN:
  case TYPE_VOID:
    return false;
  case TYPE_VECTOR:
    return true;
  default:
    // assume that ty points to another type information struct
    return is_pointer_type(*(uint64_t*)ty);
  }
}

static void print_value_aux(uint64_t *ty, int64_t val, bool nested) {
  uint64_t i, len, tyv;

  switch (ty[0]) {
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
      // complex type will be a pointer to another type info structure
      if (!is_pointer_type(tyv)) {
        print_value_aux(&tyv, ((int64_t *)val)[i + 1], true);
      } else {
        print_value_aux((uint64_t *)tyv, ((int64_t *)val)[i + 1], true);
      }
      if ((i + 1) < len) {
        printf(" ");
      }
    }
    printf(")");
    return;
  }
}

void _print_value(uint64_t *ty, int64_t val) {
  print_value_aux(ty, val, false);
  printf("\n");
}

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

  // has the object been copied yet?
  if ((uint64_t)*obj < (uint64_t)_fromspace_begin ||
      (uint64_t)*obj >= (uint64_t)_fromspace_end) {
    size = (((int64_t *)*obj)[1] + 1) << 3;
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
    *obj = (int64_t)new_obj;
  } else {
    new_obj = (int64_t *)*obj;
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
  for (r = rootstack_ptr; r >= _rootstack_begin; --r) {
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
      if (is_pointer_type(ty[i + 2])) {
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
