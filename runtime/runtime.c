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

#define LENGTH_BITS 6
#define PTRMASK_BITS 50

#define FORWARDING(x) (((uint64_t)(x)) & 1)
#define LENGTH(x) ((((uint64_t)(x)) >> 1) & ((1ULL << LENGTH_BITS) - 1))
#define PTRMASK(x) ((((uint64_t)(x)) >> 7) & ((1ULL << PTRMASK_BITS) - 1))

static uint64_t _heap_size;
int64_t *_free_ptr;
static int64_t *_fromspace_begin;
int64_t *_fromspace_end;
static int64_t *_tospace_begin;
static int64_t *_tospace_end;
int64_t **_rootstack_begin;

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

void _print_vector(int64_t *vec) {
  // TODO: we need type information about whether
  // an element is a boolean or an integer
  printf("_print_vector: 0x%016lX\n", (uint64_t)vec);
}

void _initialize(uint64_t rootstack_size, uint64_t heap_size) {
  _heap_size = heap_size;
  _fromspace_begin = malloc(heap_size);
  _fromspace_end = (int64_t *)((uint64_t)_fromspace_begin + (heap_size >> 1));
  _free_ptr = _fromspace_begin;
  _tospace_begin = _fromspace_end;
  _tospace_end = (int64_t *)((uint64_t)_fromspace_begin + heap_size);
  _rootstack_begin = (int64_t **)malloc(rootstack_size);
}

static int64_t *_collect_copy(int64_t *obj) {
  uint64_t size;
  int64_t *new_obj;

  // has the object been copied yet?
  if (FORWARDING(*obj)) {
    size = (LENGTH(*obj) + 1) << 3;
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

void _collect(int64_t **rootstack_ptr, uint64_t bytes) {
  int64_t *p, **r, *tmp, *scan_ptr, *obj;
  uint64_t i, length, size, mask;

  // swap fromspace with tospace
  tmp = _fromspace_begin;
  _fromspace_begin = _tospace_begin;
  _tospace_begin = tmp;
  tmp = _fromspace_end;
  _fromspace_end = _tospace_end;
  _tospace_end = tmp;

  _free_ptr = _fromspace_begin;
  scan_ptr = _free_ptr;

  DBGPRINT("GC: copying roots\n");

  // copy all roots first
  for (r = rootstack_ptr; (p = *r);) {
    *r-- = _collect_copy(p);
  }

  DBGPRINT("GC: copying reachable objects\n");

  // do a breadth-first search for all objects reachable from the root stack
  while (scan_ptr < _free_ptr) {
    obj = scan_ptr;
    length = LENGTH(*obj) + 1;
    mask = PTRMASK(*obj);
    for (i = 1; i < length; ++i) {
      if (mask & (1 << (i - 1))) {
        obj[i] = (int64_t)_collect_copy((int64_t *)obj[i]);
      }
    }
    scan_ptr = (int64_t *)((uint64_t)scan_ptr + (length << 3));
  }

  DBGPRINT("GC: checking for sufficient space\n");

  // check if we need to resize the heap
  if (((uint64_t)_free_ptr + bytes) >= (uint64_t)_fromspace_end) {
    DBGPRINT("GC: resizing the heap from %ld to %ld bytes\n", _heap_size,
             _heap_size << 1);
    // double the current size
    _heap_size <<= 1;
    // allocate a new heap and copy over the current fromspace
    tmp = (int64_t *)malloc(_heap_size);
    size = (uint64_t)_fromspace_end - (uint64_t)_fromspace_begin;
    memcpy(tmp, _fromspace_begin, size);
    // update the new pointers
    _fromspace_begin = tmp;
    _fromspace_end = (int64_t *)((uint64_t)tmp + (_heap_size >> 1));
    _tospace_begin = _fromspace_end;
    _tospace_end = (int64_t *)((uint64_t)tmp + _heap_size);
    // run the GC again
    _collect(rootstack_ptr, bytes);
  }
}
