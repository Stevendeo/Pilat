/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/

int main(){
  int x1, x2, x2pred;
  int y1,y2,y3;
  
  y1 = 0;
  y2 = 0;
  y3 = x1;

  while(1){
    
    if (y2 + 1 == x2){
      y1++;
      y2 = 0;
      y3--;
      
    }
    else{
      y2++;
      y3--;
      
    }
    
  }
  return 0;
}
