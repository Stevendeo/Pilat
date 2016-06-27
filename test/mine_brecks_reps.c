#include <time.h>

int main(){
  float x=float_interval(-1,1),y=float_interval(-1,1);
  int N = 1000;
  float k;
  while(N > 0){
    N--;
    k = 0;
    x = 0.68 * (x-y) + k;
    y = 2*0.68*y + x ;

  }

  return 1;

}
