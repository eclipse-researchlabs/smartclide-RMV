/*
 * external functions */

void mep_start_monitor(char *Mid, char **Msessid, mstatus *Mstatus){
    *Msessid = "11111";
    *Mstatus = monitor_started;
};

void mep_stop_monitor(char *Mid, char *Msessid, mstatus *Mstatus){
    *Mstatus = monitor_stopping;
};

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
VERBOSE_MSG(3,"complete initialization of cv\n");
dump_shared_var_attributes(shared_var_attrs,N_SHARED_VARS);

    mip->mi_JSON_cv = JSON_STRING;
    // following would be better to get directly from shared_var_decls
    // mip->mi_num_shared_vars = len_narr((void*)mip->mi_cv.observable_vars);
    mip->mi_num_shared_vars = N_SHARED_VARS;
    mip->mi_shared_vars = build_sh_var_attributes(&mip->mi_cv);
    mip->mi_mstatus = monitor_initialized;
//dump_defined_vars();

    declare_sus_vars(mip->mi_cv.shared_var_decls);
    set_ms_sus_vars(mip->mi_cv.shared_var_inits);

    return(mip); // should return NULL on failure
}

void
un_init_ms(monitor_interface_t *mip){
    clear_ms_configuration(&mip->mi_cv);
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
    monitor_interface_t *mi = &monitor_interface;
    char *mid = mi->mi_cv.monitor_id;
    char *sid = mi->mi_sessid;
    monitor_atom *mas = monitor_atoms;
    char **ATl;
    char *ORl;
    VERBOSE_MSG(1,"responder()\n");

    ATl = aT_list_constructor(mas);
    dump_strings("true atoms", ATl); fflush(stdout);

    ORl = or_list_constructor(mi->mi_cv.reportable_vars);
    printf("reports: %s\n",ORl); fflush(stdout);

    ms_heartbeat(mid,sid,ATl,ORl);
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
        // strncpy(JSON_STRING, JSON_TEST_STRING, JSON_STRING_SIZE);
        return(EXIT_FAILURE);
    }

    monitor_interface_t *mip = &monitor_interface;
    if( init_ms(mip) == NULL ){
        VERBOSE_MSG(1,"init_ms returned failure\n");
        return(EXIT_FAILURE);
    }

    mep_start_monitor(mip->mi_cv.monitor_id, &mip->mi_sessid, &mip->mi_mstatus);
    if( mip->mi_mstatus != monitor_started ) return(EXIT_FAILURE);

    return(EXIT_SUCCESS);
}

void ms_shutdown(){
    monitor_interface_t *mip = &monitor_interface;

    mep_stop_monitor(mip->mi_cv.monitor_id, mip->mi_sessid, &mip->mi_mstatus);
    if( mip->mi_mstatus != monitor_stopping ){
        printf("error from mep_stop_monitor\n");
    }else printf("monitor stopped\n");

    un_init_ms(mip);
}

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
    responder();
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
    responder();
}
