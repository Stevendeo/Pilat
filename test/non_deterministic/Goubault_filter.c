float float_interval(float, float);

int main(){
  float x,y;
  while(1){
    x = (0.75) * x - (0.125)* y;
    y = x;
  }
  return 0;
  
}

/* 
Invariants generated :
      |-6. * x + 1. * y| <= cst;

*/
