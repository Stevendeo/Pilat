#include <stdio.h>

int main(){
  int xn1 = 0, xn = 1, N = 0;
  while (N < 30)
    {
      N ++;
      xn1 = xn1 + xn;
      xn = xn1 - xn;
    }

  printf("%i", xn);
  return 0;

}
