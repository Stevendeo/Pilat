float float_interval(float,float);

int main(){
  float x0,x1,x2,tx0,tx1,tx2,in;
  while(1){
    in = float_interval(-1,1);
    tx0 = 0.9379 * x0 - 0.0381 * x1 - 0.0414 * x2 + 0.0237 * in;
    tx1 = -0.0404 * x0 + 0.968 * x1 - 0.0179 * x2 + 0.0143 * in;
    tx2 = 0.0142 * x0 - 0.0197 * x1 + 0.9823 * x2 + 0.0077 * in;
    x0 = tx0;
    x1 = tx1;
    x2 = tx2;
    
  }

  return 1;

}
/* Invariants generated : 
(1.2187798948*x0+1.16137161588*x1)+1.*x2 <= 1.171875

((((-2.45498840354*(x1*x0)+-0.788574527791*(x2*x0))+0.868152061813*
              (x0*x0))+1.12295787049*(x2*x1))+1.73559323995*(x1*x1))+1.*
          (x2*x2) <= 10.7422

Interval notation : None
*/
