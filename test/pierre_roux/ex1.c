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

/* Result : 88.134765625 */
