/*run.config
   OPT: -pilat-degree 2
*/

int petter(int N){
  int x = 0, y = 0,a,b,z;
  
  while(y < N)
    {
      a = y;
      b = y;
      z = 0;
      while (a != 0){
	if ( a % 2 ==1 )
	{
	  z = z+b;
	  a = a-1;
	}
      b = 2*b;
      a = a/2;
      }
      x += z;
      
      y++;
    }
  y--;
  return x;
}

int main(){
  petter(12);

}
