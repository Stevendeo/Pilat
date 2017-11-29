/*run.config
   OPT: -pilat-degree 2 -pilat-no-z
*/
float float_interval(float,float);

int main(){
double S,S0,S1,E,E0,E1;
 while(1){
    E1 = E0;
    E0 = E;
    E  = float_interval(-1.0,1.0);
    S1 = S0;
    S0 = S;
    S  = 0.7*E - 1.3*E0 + 1.1*E1 + 1.4*S0 - 0.7*S1; 
 }
  return 0;
}
/*
((((-0.809917355372*(S0*S)+1.27272727273*(E0*S))+0.578512396694*
              (S*S))+-1.27272727273*(E0*S0))+0.404958677686*(S0*S0))+1.*
          (E0*E0) <= 13.1537519859

|S| < 8.70578
*/
