int petter(int N){
  int x = 0, y = 0;
  
  while(y < N)
    {
      y++;
      x += y;
    }
  y--;
  return x;
}

int main(){
  petter(12);

}

