#include <stdio.h>
#include "api.h"

// compile with ::
// gcc attack.c -o attacker -L. -lruse
// execute with
// LD_LIBRARY_PATH=. ./attacker

int main(int argc, char* argv[]) {

    printf("Loading\n");
    sload(NULL,evaluate); 
    printf("Executing\n");
    void * ret = secure_eval(1); 
    printf("Done %lx \n",(unsigned long) ret);
    return 0;
}
