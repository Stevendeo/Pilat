float float_interval(float,float);

int main(){
  float x,y;
  while(1){
    y = x - 0.7*y;
    x = 1/2*x + y;
    y = 2*x - 2*y;
    x += float_interval(-1.6,1.6);
  }

  return 1;

}

/* Result : (-2.14285714286*(y*x)+1.42857142857*(x*x))+1.*(y*y) <= 88.134765625 */
