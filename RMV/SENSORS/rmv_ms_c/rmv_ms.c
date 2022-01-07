#define VERBOSITY 2
#define VERBOSE(L) if(VERBOSITY >= L)
#define VERBOSE_MSG(L,M) if(VERBOSITY >= L){printf(M);fflush(stdout);}
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "ms_vars.h"
#include "ms_conf.h"
#include "ms_iface.h"

/* MAIN - for testing takes the role of the monitored service (SUS) */

void service_logic(){
    VERBOSE_MSG(1,"service logic starting\n");
    // ... assignments using setters
    VERBOSE_MSG(1,"service logic ended\n");
}

int main(int argc, char *argv[]){
    char *filename = NULL;
 
    VERBOSE_MSG(1,"MS test\n");
    
    if( argc>1 ) filename = argv[1]; else filename = "monid_00002_conf.json";

    if( ms_startup(filename) == EXIT_FAILURE ){
        VERBOSE_MSG(1,"ms_startup returned failure\n");
        return(EXIT_FAILURE);
    };

    service_logic();

    ms_shutdown();

    VERBOSE_MSG(1,"end MS test\n");
}