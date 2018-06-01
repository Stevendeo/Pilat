/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/

#include "__fc_builtin.h"

int petter(int N){
  int x = Frama_C_interval(-5,5), y = Frama_C_interval(-5,5);
  
  while(x < N)
    {
      x += y*y;
      y++;
    }

  /*@ assert x > 0; */
  return x;
}

int main(){
  petter(Frama_C_interval(0,10));

}
