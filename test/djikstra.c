int main(){
  int n;
  int p = 0,q = 1,r = n,h;

  while (q!=1)
    {
      r = r;
      q=q/4;
      h=p+q;
      p=p/2;
      if (r>=h)
	{
	  p=p+q;
	  r=r-h;
	} 
    }
  
  return p;
}