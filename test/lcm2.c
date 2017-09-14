#include "__fc_builtin.h"

int lcm2(int a, int b){

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

int main(){
  int x =  Frama_C_interval(1,22);
  int y = lcm(x,23);
  return 0;
  
}
