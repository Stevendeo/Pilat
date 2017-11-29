/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/
int main(){

  int A,B;

  int q,r,b;

  q = 0;
  r = A;
  b = B;

  while(1){

    if(A){
      
      q = 2*q;
      b = b/2;
      if (r >= b){
	q++;
	r -= b;
      }

    }
  
  }

  return 0;
}
