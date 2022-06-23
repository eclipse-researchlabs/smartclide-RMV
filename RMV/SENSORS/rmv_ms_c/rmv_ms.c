/* MAIN - for testing takes the role of the monitored service (SUS) */

#include "sensor.h"

void service_logic(){
    VERBOSE_MSG(0,"service logic starting\n");

    ms_responder(); // send heartbeat with initial values

    // run the behavior seq with triggered heartbeats
    ms_run_behavior();
     
    VERBOSE_MSG(0,"service logic ended\n");
}

int main(int argc, char *argv[]){
    char *filename = NULL;
 
    VERBOSE_MSG(0,"MS test\n");

    if( argc>1 ) strcpy(ms_configuration_file, argv[1]);

    if( ms_startup() == EXIT_FAILURE ) return(EXIT_FAILURE);

    service_logic();

    ms_shutdown();

    VERBOSE_MSG(0,"end MS test\n");
}