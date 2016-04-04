int petter(int N){
  int x = 0, y = 0;
  
  //@ assigns x,y;
  while(y < N)
    {
      x += y*y;
      y++;
    }
  y--;
  //@ assert 6*x == (y*(y+1))*(2*y+1) ;
  return 0;
}

int main(){
  petter(12);

}

/* CAV96 - 

SPL <- logic temp
Livre concur ++ : Jean Michel Ranyal (Springer)

*/
