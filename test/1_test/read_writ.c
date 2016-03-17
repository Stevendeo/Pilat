
int main (){
  int c1,c2,k0;
  int r,w,k;
  
  r = 0;
  w = 0;
  k = k0;

  while( true ) 
    { 
      r = r;
      w = w;
      k = k;
      k0 = k0;
      c1 = c1;
      c2 = c2;
      if ( w==0 )
	{ 
	  r = r+1;
	  k = k-c1;
	}
      else if ( r==0 )
	{ 
	  w = w+1;
	  k = k-c2;
	}
      else if ( w==0 )
	{ 
	  r = r-1;
	  k = k+c1;
	}
      else
	{
	  w = w-1;
	  k = k+c2;
	}
    }
  return 0;
}
