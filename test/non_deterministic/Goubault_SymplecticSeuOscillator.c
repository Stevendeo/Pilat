float float_interval(float,float);

int main(){
  float v,x;
  while (v >= 1/2) {
    x = (1 - 0.05) * x + (0.1 - 0.00025 ) * v;
    v = -0.1 *x+(1-0.05 )* v ;
  }
}

/* 
Loop invariant : 
for x = [0,1] and v = [0,1]

0.105 * (x * v) + 1.05 * (v * v) + 1. * (x * x) <= 2.155

Interval notation : 

  x=[-1.47,1.47];
  v=[-1.4345,1.4345];

*/
