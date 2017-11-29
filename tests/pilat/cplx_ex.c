/*run.config
   OPT: -pilat-degree 2 -pilat-lin
*/
int main(){
  int x,y;
  while (1)
    {
      x = y + x;
      y = 2*x - 2*y;
      x = x - y / 2;
    }
  return 0;

}
