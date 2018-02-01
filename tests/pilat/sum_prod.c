/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/

#include "__fc_builtin.h"

int petter(int N){
  int x = 0, y = 0;
  
  while(y < N)
    {
      x++;
      y+=x;
    }

  /*@ assert x > 0; */
  return x;
}

int main(){
  petter(Frama_C_interval(0,10));

}
