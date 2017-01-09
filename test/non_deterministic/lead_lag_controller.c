float float_interval(float,float);

int main(){
  float x0p,x1p,x0,x1;
  while(1){/*
    x1 = 0.01*x0 + x1;
    x0 = 0.499*x0 - 0.05*x1 + 0.0005*x0 + float_interval(-1,1);
	   */
    x0p = x0; x1p = x1; 

    x0 = 0.499*x0p - 0.05*x1p + float_interval(-1,1);
    x1 = 0.010*x0p + x1p;


  }

  return 1;

}
/* Invariants generated : 
   0.02*x0+1.*x1 <= 70.1172
   10.*x0+1.*x1 <= 20.1172

   
   Interval notation :

   x = [-5.011,5.011];
   y = [-30.101,30.101];
 */
