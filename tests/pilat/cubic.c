/*run.config
   OPT: -pilat-degree 3 -pilat-lin
*/
int cubic (float a)
   {
   float x,s;
   int r;

   x=a;
   r=1;
   s=3.25;

   while ( x-s > 0)
      {
      x=x-s;
      s=s+6*r+3;
      r=r+1;
      }

   return r;
   }
