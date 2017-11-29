/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/
int main(){
  int x,y;

  int a,b,p,q;
  while( a!=0 && b!=0 ) 
        { 
        if ( a % 2 ==0 && b% 2 ==0 )
            {
            a =a/2;
            b = b/2;
            p = 4*p;
            }
        else if ( a % 2 ==1 && b% 2 ==0 )
            {
            a =a-1;
            q = q+b*p;
            }
        else if ( a % 2 ==0 && b% 2 ==1 )
            {
            b =b-1;
            q = q+a*p;
            }
        else
            {
            a =a-1;
            b =b-1;
            q = q+(a+b-1)*p;
            }
        }
    return p; 


}
