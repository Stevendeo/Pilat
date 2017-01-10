float float_interval(float a,float b);

int main(){
  float s0 = 0,s1 = 0,r;
  int N = 50;
  while(N > 0){
    r = 1.5*s0 - 0.7*s1 + float_interval(-1.6,1.6);
    s1 = s0;
    s0 = r;
    N--;
  }

  return 0;

}

/* Result : 
(-2.14285714286 * (s1 * s0) + 1.42857142857 * (s0 * s0)) 
+ 1. * (s1 * s1) <= 137.451; 

s0 <-> [-1.72,1.72]
s1 <-> [-2.056,2.056]

*/
