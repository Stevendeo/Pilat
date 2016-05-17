

int main()
    {
      int N,R;
      int u,v,r;
      
      
      u=2*R+1;
      v=1;
      r=R*R-N;
      
      while (r!=0)
        {
	  if (r>0) 
            {
	      r=r-v;
	      v=v+2;
	      u = u;
            }
	  else
	  {
	      r=r+u;
	      u=u+2;
	      v=v;
	      }
        }
      
      return((u-v)/2);
    }
