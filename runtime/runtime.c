#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int64_t read_int()
{
  char buf[64];
  
  fgets(buf, sizeof(buf), stdin);
  buf[strcspn(buf, "\n")] = '\0';

  return atoll(buf);
}

void print_int(int64_t i)
{
  printf("%ld\n", i);
}

void print_bool(int64_t i)
{
  if (i) {
    printf("#t");
  } else {
    printf("#f");
  }
}
