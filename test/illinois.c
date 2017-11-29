/*run.config
   OPT: -pilat-degree 1
*/
int main (){
  int dirty,shared,exclusive,invalid;
  exclusive = 0;
  dirty = 0;
  shared = 0;

  while (1) {
    exclusive = exclusive;
    dirty = dirty ;
    shared = shared;
    invalid = invalid;
    if (shared) {

      invalid = invalid - 1;
      exclusive = exclusive + 1;    
    }
    else if (shared) {

      invalid = invalid - 1;
      dirty = dirty - 1;
      shared = shared + 2;
    }
    else if (shared) {

      invalid = invalid - 1;
      shared = shared + exclusive + 1;
      exclusive = 0;
    }
    else if (shared) {

      invalid = invalid - 1;
      shared = shared + exclusive + 1;
      exclusive = 0;
    }
    else if (shared) {

      exclusive = exclusive - 1;
      dirty = dirty + 1;
    }
    else if (shared) {

      invalid = invalid + shared - 1;
      dirty = dirty + 1;
      shared = 0;
    }
    else if (shared) {
      invalid = invalid + exclusive + dirty + shared - 1;
      exclusive = 0;
      shared = 0;
      dirty = 1;
    }
    else if (shared) {

      dirty = dirty - 1;
      invalid = invalid + 1;
    }
    else if (shared) {

      shared = shared - 1;
      invalid = invalid + 1;
    }
    else {

      exclusive = exclusive - 1;
      invalid = invalid + 1;
    }
  }
}

