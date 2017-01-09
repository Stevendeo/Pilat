float float_interval(float a,float b);

int main(){
  float s0 = 0,s1 = 0,r;
  int N = 50;
  while(N > 0){
    r = 1.5*s0 - 0.7*s1 + float_interval(-0.1,0.1);
    s1 = s0;
    s0 = r;
    N--;
  }

  return 0;

}

/* 
Invariant generated : 
(-2.14285714286 * (s1 * s0) + 1.42857142857 * (s0 * s0)) 
+ 1. * (s1 * s1) <= 0.830078125; 

Interval notation : 
s0 = [-2.05683,2.05683];
s1 = [-1.72087,1.72087];

*/
