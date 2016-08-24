float float_interval(float,float);

int main(){
  float x0,x1;
  while(1){
    x1 = 0.01*x0 + x1;
    x0 = 0.499*x0 - 0.05*x1 + 0.0005*x0 + float_interval(-1,1);
  }

  return 1;

}
/* Result : 
   0.02*x0+1.*x1 <= 70.1172
   10.*x0+1.*x1 <= 20.1172
 */
