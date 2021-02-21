#include <assert.h>
#include <math.h>
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
  TYPE_FLOAT,
};

static bool is_pointer_type(uint64_t ty, bool rec) {
  switch (ty) {
  case TYPE_INTEGER:
  case TYPE_BOOLEAN:
  case TYPE_VOID:
  case TYPE_ARROW:
  case TYPE_FLOAT:
    return false;
  case TYPE_VECTOR:
    return true;
  default:
    // assume that ty points to another type information struct.
    // use 'rec' to see ultimately what type is being pointed to.
    return !rec || is_pointer_type(*(uint64_t *)ty, rec);
  }
}

/*
 * helpers
 */

double _fmod(double a, double b) { return fmod(a, b); }

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
  double dv;

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
  case TYPE_FLOAT:
    dv = *(double *)&val;
    if (isinf(dv)) {
      if (signbit(dv)) {
        printf("-inf");
      } else {
        printf("+inf");
      }
    } else if (isnan(dv)) {
      if (signbit(dv)) {
        printf("-nan");
      } else {
        printf("+nan");
      }
    } else {
      printf("%lf", dv);
    }
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
int64_t **_floatstack_begin;
static int64_t **_floatstack_end;

void _initialize(uint64_t rootstack_size, uint64_t floatstack_size,
                 uint64_t heap_size) {
  // rootstack_size must be nonzero and aligned to an even boundary
  assert(rootstack_size && !(rootstack_size & 1ULL));
  // heap_size must be nonzero and a power of two
  assert(heap_size && !(heap_size & (heap_size - 1)));
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
  // we need an additional stack to spill floating point values to,
  // since using the regular x86 stack causes problems for us.
  _floatstack_begin = (int64_t **)malloc(floatstack_size);
  assert(_floatstack_begin);
  _floatstack_end = (int64_t **)((uint64_t)_floatstack_begin + floatstack_size);
}

void _finalize() {
  assert(_heap_base);
  assert(_rootstack_begin);
  assert(_floatstack_begin);
  free(_heap_base);
  free(_rootstack_begin);
  free(_floatstack_begin);
}

static int64_t *collect_copy(int64_t *obj) {
  uint64_t fwd, size;
  int64_t *new_obj;

  assert((uint64_t)obj >= (uint64_t)_heap_base &&
         (uint64_t)obj < ((uint64_t)_heap_base + _heap_size));

  // has the object been copied yet?
  fwd = (uint64_t)*obj;
  if (fwd < (uint64_t)_fromspace_begin || fwd >= (uint64_t)_fromspace_end) {
    size = (((int64_t *)*obj)[1] + 1) * sizeof(int64_t);
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
  int64_t *p, np, **r, *tmp, *scan_ptr, *ty;
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
      DBGPRINT("GC: copied root 0x%016lX to 0x%016lX\n", (uint64_t)p,
               (uint64_t)*r);
    }
  }

  DBGPRINT("GC: copying reachable objects\n");

  // do a breadth-first search for all objects reachable from the root stack
  for (scan_ptr = _fromspace_begin; scan_ptr < _free_ptr;) {
    ty = (int64_t *)*scan_ptr;
    length = (uint64_t)ty[1];
    for (i = 0; i < length; ++i) {
      if (is_pointer_type(ty[i + 2], true)) {
        p = (int64_t *)scan_ptr[i + 1];
        np = (int64_t)collect_copy(p);
        DBGPRINT("GC: copied reachable object 0x%016lX to 0x%016lX\n",
                 (uint64_t)p, np);
        scan_ptr[i + 1] = np;
      }
    }
    scan_ptr =
        (int64_t *)((uint64_t)scan_ptr + (((length + 1) * sizeof(int64_t))));
  }
}

static uint64_t next_power_of_two(uint64_t n) {
  assert(n);
  return !(n & (n - 1))
             ? n
             : 1ULL << ((sizeof(uint64_t) << 3) - __builtin_clzll(n));
}

void _collect(int64_t **rootstack_ptr, uint64_t bytes) {
  int64_t *tmp;
  uint64_t size, needed_size;

  // run the algorithm to free up space
  cheney(rootstack_ptr);

  // check if we need to resize the heap
  size = (uint64_t)_fromspace_end - (uint64_t)_fromspace_begin;
  needed_size = bytes + ((uint64_t)_free_ptr - (uint64_t)_fromspace_begin);
  if (needed_size >= size) {
    DBGPRINT("GC: insufficient space (size=%ld, needed=%ld)\n", size,
             needed_size);
    _heap_size = next_power_of_two(needed_size << 1);
    DBGPRINT("GC: resizing the heap to %ld bytes\n", _heap_size);
    // allocate a new heap and copy over the current fromspace
    tmp = (int64_t *)malloc(_heap_size);
    assert(tmp);
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
