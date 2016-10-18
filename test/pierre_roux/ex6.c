float float_interval(float, float);

int main(){

  float x0,x1,x2,x3,x4,tx0,tx1,tx2,tx3,tx4,in0;
  
  while(1){
    in0 = float_interval(-1,1);
    x0 = 0.4250 * tx0 + 0.8131 * in0;
    x1 = 0.3167 * tx0 + 0.1016 * tx1 - 0.4444* tx2 + 0.1807 * in0;   
    x2 = 0.1278 * tx0 + 0.4444 * tx1 +0.8207 * tx2 + 0.0729 * in0;
    x3 = 0.0365 * tx0 + 0.1270 * tx1 + 0.5202 * tx2 + 0.4163 * tx3 - 0.5714 * tx4 + 0.0208 * in0;
    x4 = 0.0147 * tx0 + 0.0512 * tx1 + 0.2099 * tx2 + 0.57104 * tx3 + 0.7694 * tx4 + 0.0084 * in0;
    
    tx0 = x0;
    tx1 = x1;
    tx2 = x2;
    tx3 = x3;
    tx4 = x4;
  }

}

