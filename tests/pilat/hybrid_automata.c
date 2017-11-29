/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/

int main(){
  float x,y;
  while (1){
    while (x < 5){
      x = x+0.25;
      y = y+0.25;
      if(x>4){
        break;
      }
    }
    x = 0;
    while (y < 10){
      x = x+0.25;
      y = y+0.25;
      if (y>9){
	break;
      }
    }
	y = 0;

  }

}
