#include<stdio.h>

float float_interval(float,float);

int main(){
  int i = 0;
  float x0,x1,tx0,tx1;
  while(i < 1000){
    i++;
    tx0 = 0.95 * x0 + 0.09975 * x1;
    tx1 = -0.1 * x0 + 0.95*x1;
    
    
    x0 = tx0;
    x1 = tx1;
    printf("x = %f, y = %f\n",x0, x1);
    
  }

  return 1;

}

/* 1.00250626566*(x0*x0)+1.*(x1*x1) <= 2.00250626566 */
