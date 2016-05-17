int main(){

  int N, D;
  
  int r,ds,p,q;
  
  r=N;
  ds=D;
  p=1;
  q=0;

  while ( p!=1 )
    {
      ds=ds/2;
      p=p/2;
      
      if ( r>=ds ) 
	{
	  r=r-ds;
	  q=q+p;
	}
    }
  return 0;
}
