/* MAIN - for testing takes the role of the monitored service (SUS) */

#include "sensor.h"

void service_logic(){
    VERBOSE_MSG(1,"service logic starting\n");

    VERBOSE(2){
    dump_ms_cv( &monitor_interface.mi_cv );
    dump_shared_var_attributes(monitor_interface.mi_shared_vars,monitor_interface.mi_num_shared_vars);
    //for(monitor_atom *a=monitor_atoms;a<next_monitor_atom;a++) dump_parse(a->ma_aex);
    dump_compiled_atoms();
    dump_defined_vars();
    }

 /*     char *MEP_Reply;
    send_event("test_event", &MEP_Reply);
    //VERBOSE(1){printf("reply from send_event:\n%s\n",MEP_Reply); fflush(stdout);}
    sleep(1);
    send_event("test_event", &MEP_Reply);
    //VERBOSE(1){printf("reply from send_event:\n%s\n",MEP_Reply); fflush(stdout);}

 */
    ms_responder();
    ms_run_behavior();
    //dump_defined_vars();
     
    VERBOSE_MSG(1,"service logic ended\n");
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