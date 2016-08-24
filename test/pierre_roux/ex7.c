float float_interval(float,float);

int main(){
  float x0,x1,tx0,tx1;
  while(1){
    tx0 = x0 + 0.01 * x1;
    tx1 = -0.1 * x0 + 0.99*x1;
    
    x0 = tx0;
    x1 = tx1;
    
  }

  return 1;

}

/* Result : 
   
   (1.*(x1*x0)+10.*(x0*x0))+1.*(x1*x1) <= 12*/
