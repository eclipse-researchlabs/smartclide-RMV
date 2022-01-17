/* following eval_where is not used because the monitor_atom_eval parameter is now a string */
typedef enum {  
    unset_eval, ms_eval, mep_eval
} eval_where;

static char *eval_where_names[] = {
    "unset_eval","ms_eval","mep_eval"
};

typedef enum {
    not, eq, ne, neq, gt, lt, geq, leq, ge, le
} atom_op;

static char *atom_op_names[] = {
    "not", "eq", "ne", "neq", "gt", "lt", "geq", "leq", "ge", "le"
};

/*
% MS CONFIGURATION STRUCTURE
%   provided by Monitor Creation to accompany the generated monitor
%   imported by initialize_ms_configuration
%
%   monitor_id - established when the Monitor Sensor is defined
%   shared_vars - all variables shared by SUS and MS - superset of all other vars
%   observable_vars - SUS variables used in model, properties or reportable (may be same as shared)
%   model_vars (subset of shared_vars) - potentially observable SUS variables in the model
%   property_vars - observable vars used in property evaluation
%   reportable_vars - subset of monitor_observables to be reported in heartbeat
%   trigger_vars - variables for which setter should trigger responder
%   monitor_atoms - list of atoms {aid: , aex: } to be evaluated
%   monitor_atom_eval - where atoms are evaluated (unset_eval, ms_eval or mep_eval)
%   shared_vars_init - optional initializations list of {name: , value: }
*/
typedef struct { // TODO - check consistency w/ms_pl
    char *monitor_id;
    sh_var_decl *shared_var_decls;
    char **observable_vars;
    char **model_vars;
    char **property_vars;
    char **reportable_vars;
    char **trigger_vars;
    monitor_atom *monitor_atoms;
    char *monitor_atom_eval;
    sh_var_name_value *shared_var_inits;
} ms_configuration_vector;

/*
 * MONITOR INTERFACE STRUCTURE
 */

// all the info the SUS may need to properly set up and interact with the monitor
typedef struct {
    mstatus mi_mstatus;
    shared_var_attr_t *mi_shared_vars;
    int mi_num_shared_vars;
    char *mi_JSON_cv;
    ms_configuration_vector mi_cv;
    char *mi_sessid;
} monitor_interface_t;

monitor_interface_t monitor_interface = {
    /* mi_mstatus */            monitor_uninitialized,
    /* mi_shared_vars */        NULL,
    /* mi_num_shared_vars */    0,
    /* mi_JSON_cv */            "\0",
    /* mi_cv*/ {
        /* monitor id */        "",
        /* shared_var_decls */  NULL,
        /* observable_vars */   NULL,
        /* model_vars */        NULL,
        /* property_vars */     NULL,
        /* reportable_vars */   NULL,
        /* trigger_vars */      NULL,
        /* monitor_atoms */     NULL,
        /* monitor_atom_eval */ "unset_eval",
        /* shared_var_inits */  NULL
    },
    /* mi_sessid */             ""
};

/* get configuration vector information from the external JSON */
int
initialize_ms_configuration(ms_configuration_vector *cv){
    jsmn_parser p;
    jsmntok_t t[JSMN_TOKEN_ARRAY_SIZE];

    //VERBOSE(1){printf("JSON configuration string:\n%s\n", JSON_STRING);fflush(stdout);}

    /* parse the JSON external configuration vector */
    jsmn_init(&p);
    int r = jsmn_parse(&p, JSON_STRING, strlen(JSON_STRING), t,
                 sizeof(t) / sizeof(t[0]));

    //VERBOSE(1){printf("jsmn_parse complete=%d\n",r);fflush(stdout);}
    if (r < 0){
        printf("Failed to parse JSON: %s\n",
            r==-1?"NOMEM":r==-2?"INVAL":r==-3?"PART":"Unknown");
    }else{
        VERBOSE(1){printf("jsmn_parse returned %d tokens\n",r);fflush(stdout);}
        VERBOSE(3){range(t,0,r); fflush(stdout);}
    }
    if (r < 1 || t[0].type != JSMN_OBJECT) {
        VERBOSE(2) printf("Object expected\n"); fflush(stdout);
        return 0;
    }else{VERBOSE(2) printf("Object found\n"); fflush(stdout);}

    cv->monitor_id = get_str_attr("monitor_id", t, 0, JSON_STRING);
    cv->shared_var_decls = get_arr_of_decl_attr("shared_var_decls", t, 0, JSON_STRING);
    cv->observable_vars = get_arr_of_str_attr("observable_vars", t, 0, JSON_STRING);
    cv->model_vars = get_arr_of_str_attr("model_vars", t, 0, JSON_STRING);
    cv->property_vars = get_arr_of_str_attr("property_vars", t, 0, JSON_STRING);
    cv->reportable_vars = get_arr_of_str_attr("reportable_vars", t, 0, JSON_STRING);
    cv->trigger_vars = get_arr_of_str_attr("trigger_vars", t, 0, JSON_STRING);
    cv->monitor_atoms = get_arr_of_ma_attr("monitor_atoms", t, 0, JSON_STRING);
    cv->monitor_atom_eval = get_str_attr("monitor_atom_eval", t, 0, JSON_STRING);
    cv->shared_var_inits = get_arr_of_init_attr("shared_var_inits", t, 0, JSON_STRING);
    return 1;
}

void clear_ms_configuration(ms_configuration_vector *cv){
    cv->monitor_id = "";
    cv->shared_var_decls = NULL;
    cv->observable_vars = NULL;
    cv->model_vars = NULL;
    cv->property_vars = NULL;
    cv->reportable_vars = NULL;
    cv->trigger_vars = NULL;
    cv->monitor_atoms = NULL;
    cv->monitor_atom_eval = "unset_eval";
    cv->shared_var_inits = NULL;
}

void dump_sh_vars();
void dump_shared_var_attributes(shared_var_attr_t *, int);

// TODO - this could/should be done from the list of decls
// build the shared variable access table
// setters and getters can be looked up by name or index same as var list
// value can be directy accessed using va_addr pointer member
shared_var_attr_t*
build_sh_var_attributes(ms_configuration_vector *cv){
    //sh_var_names svns = cv->observable_vars;
	/* now use statically allocated shared_var_attrs
	shared_var_attr_t *shared_vars = (shared_var_attr_t*)
		calloc(n_shared_vars, sizeof(shared_var_attr_t));
	*/
	shared_var_attr_t *ap = shared_var_attrs;
	for(; ap<shared_var_attrs+N_SHARED_VARS; ap++){
        // TODO use statically allocated vars from generated monid_xxxxx_vars.h
		// ap->va_name, va_type, and va_addr set statically
		ap->va_trigger = 
            search_array(ap->va_name,cv->trigger_vars) >= 0;
		ap->va_report =
            search_array(ap->va_name,cv->reportable_vars) >= 0;
		ap->va_property_eval =
            search_array(ap->va_name,cv->property_vars) >= 0;
		//ap->va_getter = getter_by_type[ap->va_type];
		//ap->va_setter = setter_by_type[ap->va_type];
	}
VERBOSE_MSG(3,"complete build attributes\n");
	return(shared_var_attrs);
}

shared_var_attr_t *
sv_attr_by_name(char *name){
    for( shared_var_attr_t *sva = monitor_interface.mi_shared_vars;
        sva < monitor_interface.mi_shared_vars + monitor_interface.mi_num_shared_vars;
        sva++){
        if( strcmp(name, sva->va_name) == 0 ) return(sva);
    }
    return(NULL);
}

void declare_sus_vars(sh_var_decl *SVd){
    VERBOSE_MSG(2,"declaring sus vars\n");
}

void set_ms_sus_vars(sh_var_name_value *SVi){ // HERE
    VERBOSE_MSG(1,"setting sus vars\n");
    VERBOSE(1) dump_shared_var_attributes(monitor_interface.mi_shared_vars,monitor_interface.mi_num_shared_vars);
    for( sh_var_name_value *ip = SVi; ip < next_sh_var_name_value; ip++ ){
        shared_var_attr_t *sva = sv_attr_by_name(ip->name); 
        if( sva == NULL){printf("attr record for %s not found",ip->name); continue;}
        VERBOSE(2){printf("setting %s=%s at %lu\n",sva->va_name,ip->value,(unsigned long)sva->va_addr);fflush(stdout);}
        // interpret the value according to va_type and assign to *va_addr
        // alternatively:
        //   assign_sh_var_val(char *val_str, sh_var_type val_type, sh_var_val *val_addr);
        
        switch(sva->va_type){
		    case svt_Byte: break;
		    case svt_String: break;
		    case svt_Boolean:
                assign_Boolean(ip->value, (bool*)sva->va_addr);
                VERBOSE(2){
                printf("bool value: %s\n",((*(bool*)sva->va_addr) ? "true":"false"));
                fflush(stdout);
                break;}
		    case svt_Integer:
                assign_Integer(ip->value, (int*)sva->va_addr);
                VERBOSE(2){
                printf("int value: %d\n", *(int*)sva->va_addr);
                fflush(stdout);
                break;}
		    case svt_Float:
                assign_Float(ip->value, (float*)sva->va_addr);
                VERBOSE(2){
                printf("float value: %f\n", *(float*)sva->va_addr);
                fflush(stdout);
                break;}
		    case svt_Char: break;
		    case svt_Symbol: break;
		    case svt_Address: break;
		    default: break;
        }
        VERBOSE_MSG(2,"variable set\n");
    }
    VERBOSE(1) dump_defined_vars();
    VERBOSE(1) dump_sh_vars();
}

void af_instantiate(char *p, int *op, int *a1, int *a2){
    *op = 1; // TODO
    *a1 = 1;
    *a2 = 1;
}

int af_eval(int op, int a1, int a2){
    return 1; // TODO
    switch(op){

    }
}

int af_evaluate(char *p){
    int op, arg1, arg2;
    af_instantiate(p, &op, &arg1, &arg2);
    return af_eval(op,arg1,arg2);
}

char **
aT_list_constructor(monitor_atom *mas){
    static char *true_atoms[MON_ATOM_SZ];
    char **next_atom = true_atoms;
    for(monitor_atom *a = monitor_atoms; a < next_monitor_atom; a++){
        if( af_evaluate(a->aex) ){
            *next_atom++ = a->aid;
        }
    }
    *next_atom = NULL;
    return true_atoms;
}

typedef struct {
    char *or_name;
    char *or_value;
} or_report;

//void dump_shared_var_attributes(shared_var_attributes *svas, int nvars);
//void dump_sv_inits(char *label);

char *
or_list_constructor(char **Rv){
    static char reportout[STRINGS_SZ];
    reportout[0] = '\0';
    monitor_interface_t *mip = &monitor_interface;

    freenumstrings();
    next_sh_var_name_value = sh_var_name_values;
    for( shared_var_attr_t *sva = shared_var_attrs;
        sva < shared_var_attrs+N_SHARED_VARS; sva++){
    
        if( sva->va_report ){
            next_sh_var_name_value->name = sva->va_name;
            strcat(reportout, next_sh_var_name_value->name); strcat(reportout,"=");
            next_sh_var_name_value->value = value_str(sva->va_type, sva->va_addr);
            strcat(reportout, next_sh_var_name_value->value); strcat(reportout," ");
            ++next_sh_var_name_value;
        }
    
    }
    return reportout;
}

void ms_heartbeat(char *mid, char *sid, char **ATl, char *ORl){

}
