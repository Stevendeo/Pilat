int division (int x, int y){
    int q,r;

    q=0;
    r=x;

    while( r>=y )
        {

        int d,dd;

        d=1;
        dd=y;

        while ( r>= 2*dd ) 
            {
            d = 2*d;
            dd = 2*dd;
            }
        r=r-dd;
        q=q+d;
        }


    return r;
    }
