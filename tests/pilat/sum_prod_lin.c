/*run.config
   OPT: -pilat-degree 3 -pilat-lin -pilat-output "stdout"
*/

int petter(int N){
  int x = 0, y = 0;
  int y2 = y*y;
  while(y < N)
    {
      x += y*y;
      y++;
    }
  y--;
  return x;
}

int main(){
  petter(12);

}
