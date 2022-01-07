/*
 * external functions */

void mep_start_monitor(char *Mid, mstatus *Mstatus){*Mstatus = monitor_started;};
void mep_stop_monitor(char *Mid, mstatus *Mstatus){*Mstatus = monitor_stopping;};

/*
 * MONITOR INTERFACE DATA STRUCTURE
 */

// all the info the SUS may need to properly set up and interact with the monitor
typedef struct {
    mstatus mi_mstatus;
    shared_var_attributesp mi_shared_vars;
    int mi_num_shared_vars;
    char *mi_JSON_cv;
    ms_configuration_vector mi_cv;
} monitor_interface_t;

monitor_interface_t monitor_interface = {
    monitor_uninitialized,
    NULL,
    0,
    "\0",
    {
        "",
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        "unset_eval",
        NULL
    }
};

void dump_monitor_interface(monitor_interface_t *mip){
    printf("dump monitor interface:\n");
    printf("  %s\n",mstatus_string(mip->mi_mstatus));
    dump_shared_var_attributes(mip->mi_shared_vars, mip->mi_num_shared_vars);
    //mip->mi_shared_vars;
    //mip->mi_num_shared_vars;
    //mip->mi_JSON_cv;
    //mip->mi_cv;
}

/*
 * MS INITIALIZATION, UN-INIT and RE-INIT
 */

monitor_interface_t*
init_ms(monitor_interface_t *mip){
    if( mip->mi_mstatus != monitor_uninitialized )
        return mip; // only initialize once

    if( initialize_ms_configuration(&mip->mi_cv) == 0 ){
        return NULL;
    };
    dump_ms_cv(&mip->mi_cv);
    
    mip->mi_num_shared_vars = len_narr((void*)mip->mi_cv.observable_vars);
    mip->mi_shared_vars = build_sh_var_attributes(&mip->mi_cv);
    mip->mi_mstatus = monitor_initialized;

    mep_start_monitor(mip->mi_cv.monitor_id, &mip->mi_mstatus);
    return mip->mi_mstatus != monitor_started ? NULL : mip;
}

void
un_init_ms(monitor_interface_t *mip){
    clear_ms_configuration(&mip->mi_cv);

    mep_stop_monitor(mip->mi_cv.monitor_id, &mip->mi_mstatus);
    if( mip->mi_mstatus != monitor_stopping ){
        printf("error from mep_stop_monitor\n");
    }else printf("monitor stopped\n");

    mip->mi_mstatus = monitor_uninitialized;
}

void
re_init_ms(monitor_interface_t *mip){
    un_init_ms(mip); init_ms(mip);
}

/*
% MONITOR SENSOR functions exposed to the SUS business logic
%
% ms_startup is called by the SUS when it begins execution
% ms_shutdown is called by the SUS when it is shutting down
% ms_step is called by SUS logic to explicitly trigger the responder
*/

void responder(){
}

int ms_startup(char *filename){
    VERBOSE(1){printf("ms_startup: %s\n",filename);fflush(stdout);}
    if( filename ){
        FILE *fp;
        if( (fp = fopen(filename,"r")) == NULL ) {
            VERBOSE(1) printf("could not open json file %s\n",filename);
            return(EXIT_FAILURE);
        }
        int nbytes = fread(JSON_STRING, 1, JSON_STRING_SIZE, fp);
        fclose(fp);
        if( nbytes < JSON_STRING_SIZE-1 ){
            JSON_STRING[nbytes] = '\0';
        }else{
            VERBOSE_MSG(1,"json string too long\n");
            return(EXIT_FAILURE);
        }
    }else{
        strncpy(JSON_STRING, JSON_STRING1, JSON_STRING_SIZE);
    }

    VERBOSE(1){printf("JSON configuration string:\n%s\n", JSON_STRING);fflush(stdout);}

    if( init_ms(&monitor_interface) == NULL ){
        VERBOSE_MSG(1,"init_ms returned failure\n");
        return(EXIT_FAILURE);
    }
    dump_monitor_interface(&monitor_interface);
    return(EXIT_SUCCESS);
}

void ms_shutdown(){
    un_init_ms(&monitor_interface);
}

void ms_step(int var_idx, sh_var_val oldval,  sh_var_val newval){
    responder();
}