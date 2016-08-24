float float_interval(float,float);

int main(){
  float x0,x1,x2,x3,tx0,tx1,tx2,tx3,in0,in1;
  while(1){
    in0 = float_interval(-1,1);
    in1 = float_interval(-1,1);
    tx0 = 0.6227 * x0 + 0.3871 * x1 - 0.113 * x2 + 0.0102 * x3 + 0.3064 * in0 + 0.1826 * in1;
    tx1 =  -0.3407 * x0 + 0.9103 * x1 - 0.3388 * x2 + 0.0649 * x3 - 0.0054 * in0 + 0.6731 * in1;
    tx2 =  0.0918 * x0 - 0.0265 * x1 - 0.7319 * x2 + 0.2669 * x3 + 0.0494 * in0 + 1.6138 * in1;
    tx3 = 0.2643 * x0 - 0.1298 * x1 - 0.9903 * x2 + 0.3331 * x3 - 0.0531 * in0 + 0.4012 * in1;

    x0 = tx0;
    x1 = tx1;
    x2 = tx2;
    x3 = tx3;
    
  }

  return 1;

}
