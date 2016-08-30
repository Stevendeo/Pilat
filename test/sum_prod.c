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
  petter(12);

}

