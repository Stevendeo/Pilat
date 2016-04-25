

/* algorithm searching for a divisor for factorization, by Knuth */

int main(){
  int n = 11 ;

    int r=1,rp=1,q=1,d=3,s=1,t=1;

    d=d*d;
    r=n % d;
    rp=n % (d-2);
    q=4*(n/(d-2) - n/d);

    //    inv(    d^2*q - 4*r*d + 4*rp*d - 2*q*d + 8*r == 8*n    &&    d % 2 == 1   );
    while((s>=d)&&(r!=0))
        {
	  if (2*r-rp+q<0)
            {
            t=r;
            r=2*r-rp+q+d+2;
            rp=t;
            q=q+4;
            d=d+2;
            } 
	    else if ((2*r-rp+q>=0)&&(2*r-rp+q<d+2))
	  {
            t=r;
            r=2*r-rp+q;
            rp=t;
            d=d+2;
            }
         else if ((2*r-rp+q>=0)&&(2*r-rp+q>=d+2)&&(2*r-rp+q<2*d+4)) 
	  {
            t=r;
            r=2*r-rp+q-d-2;
            rp=t;
            q=q-4;
            d=d+2;
            }
	 else  /*((2*r-rp+q>=0)&&(2*r-rp+q>=2*d+4))*/{
	  
            t=r;
            r=2*r-rp+q-2*d-4;
            rp=t;
            q=q-8;
            d=d+2;
            }
        }

    //assert(   r == 0 => ( n % d == 0 )   );

    return d;
    }
//post( true );










