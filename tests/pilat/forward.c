/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/
void forward(int n) {
int i, a, b;
i=0;a=0;b=0;
while(i<n){
if(a) { a = a+1;
b = b+2;
} else {a = a+2;
b = b+1;
}
i = i+1;
}
}
