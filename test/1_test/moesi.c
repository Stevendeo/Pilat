int main(){
  int modified,shared,exclusive,invalid,owned;
  exclusive = 0;
  modified = 0;
  shared = 0;

  while (1) {
    modified = modified;
    shared = shared;
    exclusive = exclusive;
    invalid = invalid;
    owned = owned;
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
}

