int main(){
  int a=50000,b=100000;
  int x,y,z=0;

  x = a;
  y = b;
  z = 0;
  
  while( y!=0 ) 
    { 
      
      if ( y % 2 ==1 )
	{
	  z = z+x;
	  y = y-1;
	}
      x = 2*x;
      y = y/2;
    }
  //@ assert z == a*b;
  return z; 
}


