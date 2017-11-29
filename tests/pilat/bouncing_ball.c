/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/
int main(){
  float x,v,d;
  while(1){
    while(x>=0){
      x = x + v*d;
      v = v-9.81*d;
    }
    v = -2*v;
    x = 0;
  }
  return 0;
}
