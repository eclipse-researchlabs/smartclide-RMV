/* following eval_where is not used because the monitor_atom_eval parameter is now a string */
typedef enum {  
    unset_eval, ms_eval, mep_eval
} eval_where;

static char *eval_where_names[] = {
    "unset_eval","ms_eval","mep_eval"
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
    cv->monitor_atoms = monitor_atoms;
    cv->n_monitor_atoms = get_arr_of_ma_attr("monitor_atoms", t, 0, JSON_STRING);

    // TODO - this is the first point at which monitor atoms could be compiled
    // unless each atom is compiled as it is read from the JSON in sto_ma
    // probably best as a separate pass - now in init_ms which is caller of this fn

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
    cv->n_monitor_atoms = 0;
    cv->monitor_atom_eval = "unset_eval";
    cv->shared_var_inits = NULL;
}

// Build the shared variable attributes table.
// Some of the information is statically initialized in the
// monitor-specific xxx_vars.h include file.
// Other info initialized dynamically from the JSON configuration.
// setters and getters can be looked up by name or index same as var list
// Variable value can be directy accessed using va_addr pointer member
// cast to the appropriate type.
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
    VERBOSE_MSG(3,"completed build attributes\n");
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
    // already done statically in monitor-specific _vars.h file
    // Could use the declarations from the JSON configuration
    // vector to check consistency here.
}

void set_ms_sus_vars(sh_var_name_value *SVi){ // HERE
    VERBOSE_MSG(1,"setting sus vars\n");
    VERBOSE(1) dump_shared_var_attributes(monitor_interface.mi_shared_vars,monitor_interface.mi_num_shared_vars);
    for( sh_var_name_value *ip = SVi; ip < next_sh_var_name_value; ip++ ){
        shared_var_attr_t *sva = sv_attr_by_name(ip->vnv_name); 
        if( sva == NULL){printf("attr record for %s not found",ip->vnv_name); continue;}
        VERBOSE(2){printf("setting %s=%s at %lu\n",sva->va_name,ip->vnv_value,(unsigned long)sva->va_addr);fflush(stdout);}
        // interpret the value according to va_type and assign to *va_addr
        // alternatively:
        //   assign_sh_var_val(char *val_str, sh_var_type val_type, sh_var_val *val_addr);
        
        switch(sva->va_type){
		    case svt_Boolean:
                assign_Boolean(ip->vnv_value, (bool*)sva->va_addr);
                VERBOSE(2){
                printf("bool value: %s\n",((*(bool*)sva->va_addr) ? "true":"false"));
                fflush(stdout);}
                break;
		    case svt_Integer:
                assign_Integer(ip->vnv_value, (int*)sva->va_addr);
                VERBOSE(2){
                printf("int value: %d\n", *(int*)sva->va_addr);
                fflush(stdout);}
                break;
		    case svt_Float:
                assign_Float(ip->vnv_value, (float*)sva->va_addr);
                VERBOSE(2){
                printf("float value: %f\n", *(float*)sva->va_addr);
                fflush(stdout);}
                break;
		    case svt_Byte: 
		    case svt_String: 
		    case svt_Char: 
		    case svt_Symbol: 
		    case svt_Address: 
		    default: // silently ignore
                VERBOSE_MSG(3,"unimplemented type in set_ms_sus_vars\n");
                break;
        }
        VERBOSE_MSG(2,"variable set\n");
    }
    VERBOSE(1) dump_defined_vars();
    VERBOSE(1) dump_sh_vars();
}

ext_atom_op
extend_op(atom_op op, a_arg_kind k1, sh_var_type t1, a_arg_kind k2, sh_var_type t2){
#define INV inval_op

    // extended set of operators encodes types of args in the eop
    // must be extended if types in addition to B, I, F are supported
    // indexed by type
    static ext_atom_op eq_map[] = {INV, INV, eq_B_B, eq_I_I, eq_F_F, INV, INV, INV, INV, INV};
    static ext_atom_op ne_map[] = {INV, INV, ne_B_B, ne_I_I, ne_F_F, INV, INV, INV, INV, INV};
    static ext_atom_op lt_map[] = {INV, INV, lt_I_I, lt_F_F, lt_I_F, INV, INV, INV, INV, INV};
    static ext_atom_op le_map[] = {INV, INV, le_I_I, le_F_F, le_I_F, INV, INV, INV, INV, INV};
    // indexed by atom_op:        var, not, bcon_f, bcon_t, eq, ne, gt, lt, ge, le, badop
    static ext_atom_op b_map[] = {INV, INV, INV, INV, eq_B_B, ne_B_B, INV, INV, INV, INV, INV};
    static ext_atom_op i_map[] = {INV, INV, INV, INV, eq_I_I, ne_I_I, -lt_I_I, lt_I_I, -le_I_I, le_I_I, INV};
    static ext_atom_op f_map[] = {INV, INV, INV, INV, eq_F_F, ne_F_F, -lt_F_F, lt_F_F, -le_F_F, le_F_F, INV};
    //return -1;

    ext_atom_op eop;

    // handle 0-ary and unary ops
    switch( op ){
        case var:   if(t1==svt_Integer) return var_I; // non-zero = true
                    if(t1==svt_Boolean) return var_B;

        case not:   if(t1==svt_Boolean) return not_B;
                    if(t1==svt_Integer) return not_I; // non-zero = false
                    
        case bcon_f: return Bfalse;
        case bcon_t: return Btrue;
        default: break;
    }

    // binary ops
    if(k1 == akind_unused || k2 == akind_unused ||
       t1 == svt_UNDEFINED || t2 == svt_UNDEFINED) return INV;

    if(t1 == t2){
        switch( t1 ){
            case svt_Boolean: return b_map[op];
            case svt_Integer: return i_map[op];
            case svt_Float:   return f_map[op];
            default: return INV;
        }
    }else{ // t1 =/= t2
        if(t1 == svt_Boolean || t2 == svt_Boolean) return INV;

        // narrowed to Integer and Float (for now) following assumes this
        switch( op ){
            case lt: eop = lt_I_F;
            case le: eop = le_I_F;
            default: eop = INV;
        }
        // reverse order of dissimilar types if necessary
        if( t1==svt_Float && t2==svt_Integer ) eop = -eop;
    }
/*
    if(t1 == t2){
        switch( op ){
            case eq: eop = eq_map[t1];
            case ne: eop = ne_map[t1];
            case lt: eop = lt_map[t1];
            case le: eop = le_map[t1];
            default: eop = inval_op;
        }
    }else{  // dissimilar types
        if(t1 == svt_Boolean || t2 == svt_Boolean) return inval_op;

        // narrowed to Integer and Float (for now) following assumes this
        switch( op ){
            case lt: eop = lt_I_F;
            case le: eop = le_I_F;
            default: eop = inval_op;
        }
        // reverse order of dissimilar types if necessary
        if( t1==svt_Float && t2==svt_Integer ) eop = -eop;
    }
*/
    return INV;
}

atom_op
lookup_op(char *s){
    int k = sizeof(atom_op_names)/sizeof(char*) - 2; // don't compare "badop"
    for( atom_op_names[0]=s; k >= 0; k-- )
        if( strcmp(s,atom_op_names[k]) == 0 )
            return atom_op_map[k];
    return var; // to make compiler happy
}

void
get_atom_sym(char **atom, char arg_str[], a_arg_kind *k, sh_var_type *t){
    char *arg;
    int a_sz = 0;
    arg = *atom; (*atom)++; a_sz++;
    *k = akind_variable;
    while( isalnum(**atom) || **atom=='_' ){ (*atom)++; a_sz++; }
    strncpy(arg_str,arg,a_sz);
    arg_str[a_sz] = '\0';
    if( strcmp(arg_str,"false")==0 || strcmp(arg_str,"true")==0 ){
        *k = akind_constant; *t = svt_Boolean;
    }
}

void
get_atom_num(char **atom, char arg_str[], a_arg_kind *k, sh_var_type *t){
    char *arg;
    int a_sz = 0;
    arg = *atom; (*atom)++; a_sz++;
    *k = akind_constant; *t = svt_Integer;
    while( isdigit(**atom) ){ (*atom)++; a_sz++; }
    if( **atom == '.' ){ *t = svt_Float; (*atom)++; a_sz++; }
    while( isdigit(**atom) ){ (*atom)++; a_sz++; }
    strncpy(arg_str,arg,a_sz);
    arg_str[a_sz] = '\0';
}

void parse_atom(char *atom, char **relop, char **arg1, char **arg2, atom_op *aop,
      a_arg_kind *a1_k, sh_var_type *a1_t, a_arg_kind *a2_k, sh_var_type *a2_t){

    char *save_atom = atom; // keep original atom string for printf
    static char op_str[ATOM_OP_MAX_LEN];
    static char a1_str[ATOM_ARG_MAX_LEN];
    static char a2_str[ATOM_ARG_MAX_LEN];
    char *op; int  op_sz=0;

    *relop = atom_op_names[badop]; *arg1 = *arg2 = ""; *aop = badop;
    *a1_k = *a2_k = akind_unused; *a1_t = *a2_t = svt_UNDEFINED;
    op_str[0] = a1_str[0] = a2_str[0] = '\0';
    if(atom==NULL || *atom=='\0') return;

    if( isalpha(*atom) || *atom=='_' ){
        // operator or variable name
        op = atom;
        atom++; op_sz++;
        while( isalnum(*atom) || *atom=='_' ){ atom++; op_sz++; }
        strncpy(op_str,op,op_sz);
        op_str[op_sz] = '\0';
        *aop = lookup_op(op_str); // normalizes names, e.g. neq->ne, geq->ge
    }
    while( isspace(*atom) ) atom++;
    if( *atom=='(' ){ // atom has args
        atom++;
        while( isspace(*atom) ) atom++;
    }else{
        if( *atom=='\0' && op_str[0]!='\0' ){
            // no args - atom is just a single identifier: false, true or varname
            if( strcmp(op_str,"false")==0 || strcmp(op_str,"true")==0 ){
                *relop = op_str; *arg1 = ""; *arg2 = "";
                *a1_k = akind_unused; *a2_k = akind_unused;
                *a1_t = svt_UNDEFINED; *a2_t = svt_UNDEFINED;
            }else{ // return as "var" "name" ""
                *relop = "var"; *arg1 = op_str; // *arg2 = "";
                *a1_k = akind_variable; // *a1_t = svt_UNDEFINED;
            }
            VERBOSE(3)printf("parsed %s: %s %s %s\n",save_atom,*relop,*arg1,*arg2);
        }else{ // unrecognized
            *relop = "badop";
            *aop = badop;
        }
        return;
    }

    if( isalpha(*atom) || *atom=='_' ){
        get_atom_sym(&atom,a1_str,a1_k,a1_t);
     }else{
        if( isdigit(*atom) || *atom=='-' ){
            get_atom_num(&atom,a1_str,a1_k,a1_t);
        } // else unrecognized
    }

    while( isspace(*atom) ) atom++;
    if( *atom==')' ){ // no arg2 - currently only explicit unary fn sym is "not"
        if( *aop != not ){ /* TODO unrecognized op */ }
        else{ }
    }else if( *atom==',' ){
        atom++;
        while( isspace(*atom) ) atom++;
        if( isalpha(*atom) || *atom=='_' ){
            get_atom_sym(&atom,a2_str,a2_k,a2_t);
        }else{
            if( isdigit(*atom) || *atom=='-' ){
                get_atom_num(&atom,a2_str,a2_k,a2_t);
            } // else unrecognized
        }
    } // else unrecognized

    *relop = op_str; *arg1 = a1_str; *arg2 = a2_str;
    VERBOSE(3)printf("parsed %s: %s %s %s\n",save_atom,*relop,*arg1,*arg2);
}

void compile_monitor_atom(monitor_atom *ma){
    char *op, *a1, *a2;
    shared_var_attr_t *a1a, *a2a;

    // upon entry monitor_atom fields already set: ma_aid, ma_aex

    parse_atom(ma->ma_aex, &op, &a1, &a2, &ma->ma_op,
        &ma->ma_arg1_knd, &ma->ma_arg1_typ, &ma->ma_arg2_knd, &ma->ma_arg2_typ);

    memset(&ma->ma_arg1_val,0,sizeof(sh_var_val)); // clear values
    memset(&ma->ma_arg2_val,0,sizeof(sh_var_val)); // clear values

    if( ma->ma_arg1_knd == akind_variable ){
        if( (a1a = lookup_var_attr_by_name(a1)) != NULL){
            ma->ma_arg1_typ = a1a->va_type;
            ma->ma_arg1_val.sv_addrval = a1a->va_addr;
       }else{ /* undeclared variable */ }
    }else if( ma->ma_arg1_knd == akind_constant ){
        switch( ma->ma_arg1_typ ){
            case svt_Boolean: ma->ma_arg1_val.sv_boolval = str2Boolean(a1); break;
            case svt_Integer: ma->ma_arg1_val.sv_intval = str2Integer(a1); break;
            case svt_Float: ma->ma_arg1_val.sv_floatval = str2Float(a1); break;
            default: break;
        }
    }

    if( ma->ma_arg2_knd == akind_variable ){
        if( (a2a = lookup_var_attr_by_name(a2)) != NULL){
            ma->ma_arg2_typ = a2a->va_type;
            ma->ma_arg2_val.sv_addrval = a2a->va_addr;
        }else{ /* undeclared variable */ }
    }else if( ma->ma_arg2_knd == akind_constant ){
        switch( ma->ma_arg2_typ ){
            case svt_Boolean: ma->ma_arg2_val.sv_boolval = str2Boolean(a2); break;
            case svt_Integer: ma->ma_arg2_val.sv_intval = str2Integer(a2); break;
            case svt_Float: ma->ma_arg2_val.sv_floatval = str2Float(a2); break;
            default: break;
        }
    }

    ma->ma_ext_op = extend_op(ma->ma_op, ma->ma_arg1_knd, ma->ma_arg1_typ,
                            ma->ma_arg2_knd, ma->ma_arg2_typ);

    if( ma->ma_op == var || ma->ma_op == not ){ // get the type and the address of the variable
        if( ma->ma_arg1_typ != svt_Boolean && ma->ma_arg1_typ != svt_Integer ){
            // not a valid argument for operator
            ma->ma_ext_op = inval_op;
        }
    }
}

void compile_monitor_atoms(){
    for(monitor_atom *a=monitor_atoms; a<next_monitor_atom; a++)
        compile_monitor_atom(a);
}

/*
void af_instantiate(char *op_str, char *arg1_str, char *arg2_str,
                    atom_op *op, sh_var_type *a1_t, void *a1_a,
                    sh_var_type *a2_t, void *a2_a){
    // lookup operator
    if( (*op = (atom_op)lookup_op(op_str)) == -1 ){ // not found
        *a1_t = *a2_t = svt_UNDEFINED;
        return;
    }

    if( isalpha(*arg1_str) ){ *a1_t = svt_Symbol; }
    if( isdigit(*arg1_str) || *arg1_str=='-' ){ *a1_t = svt_Integer; }
    if( isalpha(*arg2_str) ){ *a2_t = svt_Symbol; }
    if( isdigit(*arg2_str) || *arg2_str=='-' ){ *a2_t = svt_Integer; }


    if( *op == var ){} // single, presumably Boolean, variable

    if( *op == not ){} // negation - applicable only to a Boolean var

    switch(*op){
        case var:
        case not:
        case eq:
        case ne:
        case gt:
        case lt:
        case ge:
        case le:
            VERBOSE(2)printf("valid op (%d) in instantiate\n", *op);
            break;
        default:
            VERBOSE(2)printf("invalid op (%d) in instantiate\n", *op);
            break;
    }

    // int rop = 1;
    // *op = 1; // TODO
}

bool af_eval(atom_op op, sh_var_type arg1_t, void *arg1, sh_var_type arg2_t, void *arg2){
    // TODO
    switch(op){
        default: break;
    }
    return true;
}

bool af_evaluate(char *p){ // evaluate atomic formula
    atom_op op; void *arg1, *arg2;
    sh_var_type arg1_t, arg2_t;
    char *op_str, *arg1_str, *arg2_str;
    
    //parse_aex(p, &op_str, &arg1_str, &arg2_str); // TODO move to initialize

    af_instantiate(op_str, arg1_str, arg2_str,
                    &op, &arg1_t, &arg1, &arg2_t, &arg2);
VERBOSE(0)printf("instantiated: op=%s, arg1=%s:%d, arg2=%s:%d\n",
        atom_op_names[op], arg1_str, arg1_t, arg2_str, arg2_t);
    bool res = af_eval(op, arg1_t, arg1, arg2_t, arg2);
    return res;
}
*/

bool af_eval( ext_atom_op op, sh_var_val val1, sh_var_val val2 ){
    sh_var_val v1 = val1; sh_var_val v2 = val2;

   // inverted sign of operator means args should be reversed
    bool inverted;
    if( (inverted = ((int)op < 0)) ){ v1 = val2; v2 = val1; op = -op; }

    switch( op ){
        case not_B: return v1.sv_boolval ? false : true;
        case not_I: return v1.sv_intval == 0;
        case var_B: return v1.sv_boolval;
        case var_I: return v1.sv_intval != 0;
        case Bfalse: return false;
        case Btrue: return true;
        
        case eq_B_B: return v1.sv_boolval == v2.sv_boolval;
        case eq_I_I: return v1.sv_intval == v2.sv_intval;
        case eq_F_F: return v1.sv_floatval == v2.sv_floatval;
        case eq_I_F: return v1.sv_intval == v2.sv_floatval;   // F<->I

        case ne_B_B: return v1.sv_boolval != v2.sv_boolval;
        case ne_I_I: return v1.sv_intval != v2.sv_intval;
        case ne_F_F: return v1.sv_floatval != v2.sv_floatval;
        case ne_I_F: return v1.sv_intval != v2.sv_floatval;

        case lt_I_I: return v1.sv_intval < v2.sv_intval;
        case lt_F_F: return v1.sv_floatval < v2.sv_floatval;
        case lt_I_F: return v1.sv_intval < v2.sv_floatval;
        case lt_F_I: return v1.sv_floatval < v2.sv_intval;
        
        case le_I_I: return v1.sv_intval <= v2.sv_intval;
        case le_F_F: return v1.sv_floatval <= v2.sv_floatval;
        case le_I_F: return v1.sv_intval <= v2.sv_floatval;
        case le_F_I: return v1.sv_floatval <= v2.sv_intval;

        default: return false; // unknown operator
    }
}

bool af_evaluate(monitor_atom *at){
    sh_var_val val1, val2;

    //return true;

    if( at->ma_arg1_knd == akind_variable ){ // get the value from the var
        switch( at->ma_arg1_typ ){
            case svt_Boolean: val1.sv_boolval = *(bool*)(at->ma_arg1_val.sv_addrval); break;
            case svt_Integer: val1.sv_intval = *(int*)(at->ma_arg1_val.sv_addrval); break;
            case svt_Float: val1.sv_floatval = *(float*)(at->ma_arg1_val.sv_addrval); break;
            default: break;
        }
    }else if( at->ma_arg1_knd == akind_constant ){
        val1 = at->ma_arg1_val;
    }

    if( at->ma_arg2_knd == akind_variable ){ // get the value from the var
        switch( at->ma_arg1_typ ){
            case svt_Boolean: val2.sv_boolval = *(bool*)(at->ma_arg2_val.sv_addrval); break;
            case svt_Integer: val2.sv_intval = *(int*)(at->ma_arg2_val.sv_addrval); break;
            case svt_Float: val2.sv_floatval = *(float*)(at->ma_arg2_val.sv_addrval); break;
            default: break;
        }
    }else if( at->ma_arg2_knd == akind_constant ){
        val2 = at->ma_arg2_val;
    }
    return af_eval(at->ma_ext_op, val1, val2);
}

char **
aT_list_constructor(){
    static char *true_atoms[MON_ATOM_SZ];
    char **next_atom = true_atoms;

    monitor_interface_t *mi = &monitor_interface;
    monitor_atom *atm = monitor_atoms;
    VERBOSE(2)dump_compiled_atoms();
    for( int i=0; i<mi->mi_cv.n_monitor_atoms; i++, atm++ ){
        if( af_evaluate(atm) ){
            *next_atom++ = atm->ma_aid;
            // opt: other atom id saving action
        }
    }
/*
    VERBOSE(2){dump_matoms("in aT_list_constructor");}
    for(monitor_atom *a = monitor_atoms; a < next_monitor_atom; a++){
        if( af_evaluate(a->aex) ){
            *next_atom++ = a->ma_aid;
            // opt: other atom id saving action
        }
    }
*/
    *next_atom = NULL; // terminator necessary for dump_strings
    return true_atoms; // return list of strings
}

char *
or_list_constructor(){
    static char reportout[STRINGS_SZ];
    reportout[0] = '\0';

    freenumstrings();
    next_sh_var_name_value = sh_var_name_values; // reset
    for( shared_var_attr_t *sva = shared_var_attrs;
        sva < shared_var_attrs+N_SHARED_VARS; sva++){
    
        if( sva->va_report ){
            // name from the attribute record for the variable
            next_sh_var_name_value->vnv_name = sva->va_name;

            strcat(reportout, next_sh_var_name_value->vnv_name);
            strcat(reportout,"=");

            // if value string is a number (Integer/Float) it must be freed
            next_sh_var_name_value->vnv_value =
                value_str(sva->va_type, sva->va_addr);
            next_sh_var_name_value->vnv_type = sva->va_type;

            strcat(reportout, next_sh_var_name_value->vnv_value);
            strcat(reportout," ");
            ++next_sh_var_name_value;
        }
    
    }
    dump_sv_inits("post-or_list_constructor (name=value)");
    return reportout;
    // Side effect: reportable variable values in sh_var_name_values
}

void or_list_free(){
    sh_var_name_value *nv = sh_var_name_values;
    for( ; nv < next_sh_var_name_value; nv++){
        if( nv->vnv_type != svt_Boolean ) free( nv->vnv_value );
    }
    next_sh_var_name_value = sh_var_name_values;
}

void mep_heartbeat(int *Response){
    VERBOSE_MSG(1,"MEP received heartbeat\n");
    // must simulate some MEP behavior
    // ...
    *Response = 1;
}

void send_ms_heartbeat(char *mid, char *sid, char **ATl, char *ORl, int *Response){
    // synchronously send heartbeat to rmv_mf_mep and wait for response
    // The message is sent via the mepapi.
    // For standalone test we call a mep_heartbeat stub
    int mep_response = 0;
    VERBOSE_MSG(1,"send_ms_heartbeat to MEP\n");
    VERBOSE(1){dump_strings("  true atoms", ATl); fflush(stdout);}
    VERBOSE(1){dump_sv_inits("variable reports in heartbeat");}

    mep_heartbeat(Response);
    VERBOSE(1){printf("response from mep_heartbeat=%d\n",*Response);fflush(stdout);}
}

void ms_heartbeat(char *mid, char *sid, char **ATl, char *ORl, int *Response){
    // prepare the heartbeat message and send to MEP
    // mid identifies the monitor
    // sid identifies the NuRV session
    // ATl is the list of true atoms
    // ORl is the list of assignments of the reportable vars
    //
    // The JSON string looks like, e.g.:
    //   {"atoms":["p","a1"], "vars":[{"p":true},{"n":2}]}
    // This is represented as the json-ized Prolog term:
    //   json([atoms=[p,a1],vars=[json([p=true]),json([n=2])]])
    //
    // We use the ATl array but don't use the ORl string.
    // Instead we use sh_var_name_values, constructed as a side
    // effect by or_list_constructor, which must be consumed
    // before the next call.
    monitor_atom *ma = monitor_atoms;
    sh_var_name_value *nv = sh_var_name_values;
    int hb_response = 0;

    send_ms_heartbeat(mid,sid,ATl,ORl,Response);
}