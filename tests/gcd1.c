/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/

int main ()
    {
      int a,b;
            
      while(a!=b) 
        { 
	  
	  if (a>b) a = a-b;
	  
	  else {b = b-a;}
	  
        }
      
      return a;
    }
