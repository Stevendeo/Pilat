/*#include <time.h>
#include <limits.h>
float non_det(){
  return ((float) ((float)rand() / INT_MAX) /10);

}*/

int main(){
  float x=1,y=1;
  int N = 1000;
  float k;
  while(N > 0){
    N--;
    /*k = non_det();*/
    x = 0.68 * (x-y) /*+ k*/;
    y = 2*0.68*y + x /*+ k*/;

  }

  return 1;

}
