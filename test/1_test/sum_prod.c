int petter(int N){
  int x = 0, y = 0;
  
  while(y < N)
    {
      x += y*y;
      y++;
    }
  y--;
  return x;
}

int main(){
  petter(12);

}

/* CAV96 - 

SPL <- logic temp
Livre concur ++ : Jean Michel Ranyal (Springer)

*/
