// From Inference of polynomial invariants for imperative programs:
// A farewell to Gröbner bases

int main(){
  int s = 0,e = 0,m = 0, i = 0;
  while (1)
    {
      if (i != 0){
	s = s + e + m;
	i--;
	e = 0;
	m = 0;	
      }
      else
      if (e != 0){
	s =s ;
	i = i;
	e --;
	m ++;
      }
      else{
	i = i + m + e + s -1;
	s = 0;
	e = 1;
	m = 0;
         
      }
    }
  return 0;

}
