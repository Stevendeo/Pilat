
int gcd (int x, int y)
    {
    int a,b,p,q,r,s;

    a=x;
    b=y;
    p=1;
    q=0;
    r=0; 
    s=1;
    int c,quot;

    while( b!=0 ) 
        { 

        c=a;
        quot=0;

        while( c>=b )
            {
            c=c-b;
            quot=quot+1;
            }
        a=b;
        b=c;
            {
            int temp;

            temp=p;
            p=q;
            q=temp-q*quot;
            temp=r;
            r=s;
            s=temp-s*quot;
            } 
        }

    return a;
    }
