int petter(int N){
  int x = 0, y = 0;
  
  while(y < N)
    {
      x += y;
      y++;
    }
  y--;
  return x;
}

int main(){
  petter(Frama_C_interval(0,10));

}
