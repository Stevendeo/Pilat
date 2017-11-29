/*run.config
   OPT: -pilat-degree 2 -pilat-no-z
*/
int main(){
  float x,y;
  int N = 1000;
  float k;
  while(1){
    N = float_interval(-0.1,0.1);
    x = 0.68 * (x-y) + N;
    y = 2*0.68*y + x ;

  }

  return 1;

}
