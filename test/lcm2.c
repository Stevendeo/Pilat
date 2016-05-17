int main(){
  int a,b;

 int x,y,u,v;

    x=a;
    y=b;
    u=b;
    v=a;

    while(x!=y) 
        { 
        if (x>y)
            {
            x=x-y;
            v=v+u;
            }
        else 
            {
            y=y-x;
            u=u+v;
            }
        }


    return (u+v)/2;
}
