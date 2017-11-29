/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/
#include "__fc_builtin.h"


int gcd2 (int x, int y)
    {
      int a,b,p,q,r,s;
      
      a=x;
      b=y;
      p=1;
      q=0;
      r=0;
      s=1;
      
      while(a!=b) 
        { 
	  
	  if (a>b) {a = a-b; p = p-q; r=r-s;}
	  
	  else {b = b-a; q = q-p; s = s-r;}
	  
        }
      
      return a;
    }

int main(){
  int x = Frama_C_interval(0,10);
  int y = gcd2(x,23);
  /*@ assert (y == 1);*/
  return 0;

}
