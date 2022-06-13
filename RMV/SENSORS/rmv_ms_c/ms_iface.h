/*
% MONITOR SENSOR functions exposed to the SUS business logic
%
% ms_startup is called by the SUS when it begins execution
% ms_shutdown is called by the SUS when it is shutting down
% ms_responder causes a MS message to be sent to the MEP - called by setters
% ms_step is called for state change to trigger the responder
% ms_run_behavior executes sequence of assigns in the behavior ms_cv element
% ms_start_timer starts (or resets) a repeating timer at specified interval
% ms_stop_timer stops repeating timer interrupt and cancels outstanding timer
% ms_service_timer the default timer interrupt service routine
% ms_recovery register a recovery service routine
*/

void ms_responder(){
    monitor_interface_t *mi = &monitor_interface;
    char *mid = mi->mi_cv.monitor_id;
    char *sid = mi->mi_sessid;
    monitor_atom *mas = monitor_atoms;
    char **ATl;
    char *ORl;
    int hb_response = 0;

    VERBOSE_MSG(0,"responder()\n");

    // TODO - ENTER RESPONDER CRITICAL SECTION (to avoid race with timer)

    ATl = aT_list_constructor();
    VERBOSE(4){dump_strings(" true atoms", ATl); fflush(stdout);}

    ORl = or_list_constructor(); // creates storage needing free()
    VERBOSE(4){printf(" reports (string):\n  %s\n",ORl); fflush(stdout);}

    ms_heartbeat(mid,sid,ATl,ORl,&hb_response);
    or_list_free();

    // activate recovery response based on hb_response
    // if the SUS has registered a recovery callback function

    if( sus_recovery_callback != NULL ) (*sus_recovery_callback)();

    // TODO - LEAVE RESPONDER CRITICAL SECTION
}

int ms_startup(){
    // the first part of this would change if config source is not a file
    if( ms_configuration_file[0] == '\0' ){
        strcpy(ms_configuration_file, global_monitor_id);
        strcat(ms_configuration_file, CONFIG_FILENAME_SUFFIX);
    }
    VERBOSE(0){printf("ms_startup configuration: %s\n",ms_configuration_file);fflush(stdout);}
    if( ms_configuration_file[0] != '\0' ){
        FILE *fp;
        if( (fp = fopen(ms_configuration_file,"r")) == NULL ) {
            VERBOSE(1) printf("could not open json file %s\n",ms_configuration_file);
            return(EXIT_FAILURE);
        }
        int nbytes = fread(JSON_STRING, 1, JSON_STRING_SZ, fp);
        fclose(fp);
        if( nbytes < JSON_STRING_SZ-1 ){
            JSON_STRING[nbytes] = '\0';
        }else{
            VERBOSE_MSG(1,"json string too long\n");
            return(EXIT_FAILURE);
        }
    }else{
        // strncpy(JSON_STRING, JSON_TEST_STRING, JSON_STRING_SZ);
        return(EXIT_FAILURE);
    }

    monitor_interface_t *mip = &monitor_interface;
    if( init_ms(mip) == NULL ){
        VERBOSE_MSG(1,"init_ms returned failure\n");
        return(EXIT_FAILURE);
    }

    url_encoder_rfc_tables_init();

    // open_MEP_comm(); // have to do this for every comm

    ms_global_trigger_enable = true;
    VERBOSE_MSG(2,"\ncalling mep_monitor_start\n");
    mep_monitor_start(mip->mi_cv.monitor_id, &mip->mi_sessid, &mip->mi_mstatus);
    if( mip->mi_mstatus != monitor_started ) return(EXIT_FAILURE);
    VERBOSE_MSG(1,"MS startup successful\n");
    return(EXIT_SUCCESS);
}

void ms_shutdown(){
    monitor_interface_t *mip = &monitor_interface;
    stop_timer();

    ms_global_trigger_enable = false;

    VERBOSE_MSG(0,"ms_shutdown()\n");
    VERBOSE_MSG(2,"\ncalling mep_monitor_stop\n");
    mep_monitor_stop(mip->mi_cv.monitor_id, mip->mi_sessid, &mip->mi_mstatus);
    if( mip->mi_mstatus != monitor_stopped ){
        printf("error from mep_monitor_stop\n"); fflush(stdout);
    }else VERBOSE_MSG(2,"monitor stopped\n");

    //close_MEP_comm(); // have to do this for every comm

    un_init_ms(mip);
}

// ms_step... not currently used, incomplete
void ms_step_idx(int var_idx, void* oldval, void* newval){
    // for now do nothing but call responder
    //sh_var_type type = shared_var_types[var_idx];
    sh_var_type type = shared_var_attrs[var_idx].va_type;
        switch(type){
		    case svt_Byte: break;
		    case svt_String: break;
		    case svt_Boolean:
                break;
		    case svt_Integer:
                break;
		    case svt_Float:
               break;
		    case svt_Char: break;
		    case svt_Symbol: break;
		    case svt_Address: break;
		    default: break;
        }
    ms_responder();
}

void ms_step_addr(void *addr, sh_var_type type, void* oldval, void* newval){
    // for now do nothing but call responder
        switch(type){
		    case svt_Byte: break;
		    case svt_String: break;
		    case svt_Boolean:
                break;
		    case svt_Integer:
                break;
		    case svt_Float:
               break;
		    case svt_Char: break;
		    case svt_Symbol: break;
		    case svt_Address: break;
		    default: break;
        }
    // responder preconditions met:
    //    ms_global_trigger_enable && trigger_var && (old!=new || ms_global_report_all)
    ms_responder();
}

void ms_start_timer(float interval){
    monitor_interface.mi_cv.timer = interval;
    initiate_timer(interval);
}

void ms_stop_timer(){
    stop_timer();
}

void ms_service_timer(){
    // TODO - make sure a responder() is not running
    // Make ms_responder a critical section.
    // Could just skip this timer interrupt if ms_responder is running,
    // but still need to assure mutual exclusion when not skipping.

    ms_responder();

    // no need to re-arm timer as setitimer is set to repeat until cancelled
}

void ms_recovery( void (*recoveryp)() ){
    sus_recovery_callback = recoveryp;
}

% ONLY FOR TESTING:

void ms_run_behavior(){
    VERBOSE_MSG(0,"ms_run_behavior()\n");
    run_sequence();
    VERBOSE_MSG(0,"behavior complete\n");
}

void ms_test(){
    monitor_interface_t *mip = &monitor_interface;

    VERBOSE_MSG(1,"\ncalling mep_monitor_test\n");
    mep_monitor_test(mip->mi_cv.monitor_id, mip->mi_sessid, &mip->mi_mstatus);
    if( mip->mi_mstatus != test_success ){
        printf("error from mep_monitor_test\n"); fflush(stdout);
    } //else VERBOSE_MSG(2,"monitor\n");
}

