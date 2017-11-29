/*run.config
   OPT: -pilat-no-z -pilat-degree 2
*/

float float_interval(float,float);

int main(){
  int x,y,k;
  while(1) {
    k = float_interval(-1,1);
    x = 0.8*x + k;
    y = 0.9*y + k;    
  }
  return 0;
}
