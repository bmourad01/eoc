#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int64_t* _free_ptr;
static int64_t* _fromspace_begin;
int64_t* _fromspace_end;
static int64_t* _tospace_begin;
static int64_t* _tospace_end;
int64_t** _rootstack_begin;

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

void _print_void() {
  printf("#<void>\n");
}

void _print_vector(int64_t* vec) {
  printf("_print_vector: 0x%016lX\n", (uint64_t)vec);
}

void _initialize(uint64_t rootstack_size, uint64_t heap_size) {
  _fromspace_begin = malloc(heap_size);
  _fromspace_end = (int64_t*)((uint64_t)_fromspace_begin + (heap_size >> 1));
  _tospace_begin = _fromspace_end;
  _tospace_end = (int64_t*)((uint64_t)_fromspace_begin + heap_size);
  _rootstack_begin = (int64_t**)malloc(rootstack_size);
}

void _collect(void *rootstk, int64_t bytes) {
  (void)rootstk;
  (void)bytes;
}
