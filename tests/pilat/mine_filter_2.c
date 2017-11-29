/*run.config
   OPT: -pilat-degree 2 -pilat-no-z
*/

#include "__fc_builtin.h"

int main(){
float s0,s1,r;

 while(1){
   r = 1.5*s0 - 0.7*s1 + Frama_C_float_interval(-0.1,0.1);
   s1 = s0;
   s0 = r;
 }
  return 0;
}
