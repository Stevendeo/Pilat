float float_interval(float,float);

int main(){
  float x0,x1,x2,x3,in;
  while(1){
    in = float_interval(-0.5,0.5);
    x1 = x0 - 0.7*x1 - 0.7*x2 + 0.4*x3;
    x0 = 1/2*x0 + x1;
    x1 = 2*x0 - 2*x1;
    x3 = x2;
    x2 = in;
    
  }

  return 1;

}

/* -6.5625*(x1*x0) - 3.52678571429*(x2*x0) + 3.75*(x3*x0) + 4.375*(x0*x0) + 
   2.375*(x2*x1)) -3.5*(x3*x1)) + 3.0625*(x1*x1) -1.35714285714* (x3*x2) + 0.741071428571*(x2*x2) +1.*(x3*x3) <= 4.9
*/
