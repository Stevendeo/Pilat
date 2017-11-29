/*run.config
   OPT: -pilat-degree 2 -pilat-no-z
*/
float float_interval(float,float);

int main(int N) {
  double S,S0,S1,E,E0,E1;
  int i,j;
 
  S = 0.0;
  S0 = 0.0;
  S1 = 0.0;
  E = 0.125;
  E0 = -0.25;
  E1 = 0.5;
  N = 20;
 
  for (i=1;i<N;i++) {
    E = float_interval(-1,1);
    E1 = E0;
    E0 = E;
    // E = randum [-1,1]
    S1 = S0;
    S0 = S;
    S  = 0.7*E - 1.3*E0 + 1.1*E1 + 1.4*S0 - 0.7*S1;
  }
  return 0;
}
