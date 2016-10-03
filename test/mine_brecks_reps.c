int main(){
  float x,y;
  while(x != y){
    x = 0.68 * (x-y);
    y = 2*0.68*y + x ;
  }

  return 1;

}
