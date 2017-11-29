#include <stdio.h>

int main(){
  int N = 10, t[N];
  int i=0;
  
  while (i<N){
    t[i] = 0;
    i++;
  } 

  i=0;
  while (i<N){
    t[i] = t[N-i-1];
    t[N-i-1]++;
    i++;
  } 
  
  i=0;
  while(i<10){
    printf("%i\n", t[i]);
    i++;
  } 
  return 0;
}

