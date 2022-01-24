/* MAIN - for testing takes the role of the monitored service (SUS) */

#include "sensor.h"

void service_logic(){
    VERBOSE_MSG(1,"service logic starting\n");

    dump_shared_var_attributes(monitor_interface.mi_shared_vars,monitor_interface.mi_num_shared_vars);

    for(monitor_atom *a=monitor_atoms;a<next_monitor_atom;a++){
        dump_parse(a->ma_aex);
    }
    dump_parse("");
\
    dump_compiled_atoms();
    dump_defined_vars();
    printf("%d\n",af_evaluate(&monitor_atoms[4]));

    // ... assignments using setters
    responder();
    responder(); // do twice expect same result
    //VERBOSE(3)test_var_getr_setr();

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