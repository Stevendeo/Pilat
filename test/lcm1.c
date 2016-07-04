
int lcm (int a, int b)
    {
    int x,y,u,v;

    x=a;
    y=b;
    u=b;
    v=0;

    while(x!=y) 
        { 

        while (x>y)
            {
            x=x-y;
            v=v+u;
            }

        while (x<y)
            {
            y=y-x;
            u=u+v;
            }
        }
    return u+v;
    }
