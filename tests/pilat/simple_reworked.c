#include <stdlib.h>
#include <limits.h>
#include <stdio.h>

/* /\*@ requires \valid(dst + (0..sz-1)); */
/*     assigns *(dst + (0..sz-1)); */
/*  *\/ */
/* void move (char *dst, const char *src, unsigned sz) */
/* { */
/*   // ghost unsigned i = 0; */
/*   /\* loop invariant dst == \at(dst, Pre) + i; */
/*       loop invariant sz == \at(sz, Pre) - i; */
/*       loop assigns *(\at(dst, Pre) + (0..i)), dst, src, sz, i; */
/*   *\/ */
/*   while (sz > 0) { */
/*     *(dst++) = *(src++); */
/*     sz -= 1; */
/*     // ghost i += 1; */
/*   } */
/* } */

/*@ requires x > z;
    requires y < z;
    requires z > 0;
    ensures \result == x + y - z;
 */
int foo (int x, int y, int z)
{
  //return x - z + y;
  return x + y - z;
}

int main (int argc, char *argv[]) {
  char *greeting = "Hello World";
  char msg[12];
  unsigned sz = 0, i;
  for (i = 0; i < 12; i += 1) {
    sz += 1;
  }
  /* move(msg, greeting, sz); */
  /* printf("%s\n", msg); */
  return 0;
}
