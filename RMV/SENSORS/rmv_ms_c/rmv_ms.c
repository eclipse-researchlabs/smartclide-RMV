/* MAIN - for testing takes the role of the monitored service (SUS) */

#include "sensor.h"

void service_logic(){
    VERBOSE_MSG(1,"service logic starting\n");
    // ... assignments using setters
    responder();
    responder(); // do twice expect same result
    test_var_getr_setr();

    VERBOSE_MSG(1,"service logic ended\n");
}

int main(int argc, char *argv[]){
    char *filename = NULL;
 
    VERBOSE_MSG(0,"MS test\n");

    if( argc>1 ) filename = argv[1]; else filename = "monid_00002_conf.json";

    if( ms_startup(filename) == EXIT_FAILURE ) return(EXIT_FAILURE);

    service_logic();

    ms_shutdown();

    VERBOSE_MSG(0,"end MS test\n");
}