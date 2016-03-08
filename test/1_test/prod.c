int main(){
  int x,y,z;
  while (1)
    {
      if (x){
        z = z+x;
	x *= 2;
	y = (y - 1)/2;
      }
      else 
	{
	x *= 2;
	y = y/2;
	z=z;
	}
    }
  return 0;

}
