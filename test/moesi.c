
int modified = 1 ,shared = 2,exclusive = 3,invalid = 4 ,owned = 5;
int main(){
  int N = 0;

  while (N < 100) {
    N++;
    if (invalid) {

      shared = shared + exclusive + 1;
      owned = owned + modified;
      invalid = invalid - 1;
      exclusive = 0;    
      modified = 0;
    }
    else if (exclusive) {

      exclusive = exclusive - 1;
      modified = modified + 1;
    }
    else if (shared) {

      invalid = invalid + modified + exclusive + shared + owned - 1;
      shared = 0;
      exclusive = 1;
      modified = 0;
      owned = 0;
    }
    else if (owned) {

      invalid = invalid + modified + exclusive + shared + owned - 1;
      shared = 0;
      exclusive = 1;
      modified = 0;
      owned = 0;
    }
    else {

      invalid = invalid + modified + shared + exclusive + owned - 1;
      shared = 0;
      exclusive = 1;
      modified = 0;
      owned = 0;
    }
  }
  return 0;
}

