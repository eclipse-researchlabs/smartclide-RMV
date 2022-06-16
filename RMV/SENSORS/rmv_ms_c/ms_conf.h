/* set up the monitor sensor configuration */

// is the evaluation of atoms done in the sensor or in the RMV event handler
typedef enum {  
    unset_eval, ms_eval, mep_eval
} eval_where;

static char *eval_where_names[] = {
    "unset_eval","ms_eval","mep_eval"
};

/* get configuration vector information from the externally provided JSON */
int
initialize_ms_configuration(ms_configuration_vector *cv){
    jsmn_parser p;
    jsmntok_t t[JSMN_TOKEN_ARRAY_SZ];

    //VERBOSE(1){printf("JSON configuration string:\n%s\n", JSON_STRING);fflush(stdout);}

    /* parse the JSON external configuration vector */
    jsmn_init(&p);
    int r = jsmn_parse(&p, JSON_STRING, strlen(JSON_STRING), t,
                 sizeof(t) / sizeof(t[0]));

    if (r < 0){
        printf("Failed to parse JSON: %s\n",
            r==-1?"NOMEM":r==-2?"INVAL":r==-3?"PART":"Unknown");
    }else{ // parse succeeded
        VERBOSE(1){printf("jsmn_parse returned %d tokens\n",r);fflush(stdout);}
        VERBOSE(4){range(t,0,r,JSON_STRING); fflush(stdout);}
    }
    if (r < 1 || t[0].type != JSMN_OBJECT) return 0;

    cv->monitor_id = get_str_attr("monitor_id", t, 0, JSON_STRING);
    cv->shared_var_decl = get_arr_of_decl_attr("shared_var_decl", t, 0, JSON_STRING);
    cv->observable_vars = get_arr_of_str_attr("observable_vars", t, 0, JSON_STRING);
    cv->model_vars = get_arr_of_str_attr("model_vars", t, 0, JSON_STRING);
    cv->property_vars = get_arr_of_str_attr("property_vars", t, 0, JSON_STRING);
    cv->reportable_vars = get_arr_of_str_attr("reportable_vars", t, 0, JSON_STRING);
    cv->trigger_vars = get_arr_of_str_attr("trigger_vars", t, 0, JSON_STRING);
    cv->monitor_atoms = monitor_atoms;
    cv->n_monitor_atoms = get_arr_of_ma_attr("monitor_atoms", t, 0, JSON_STRING);

    cv->monitor_atom_eval = get_str_attr("monitor_atom_eval", t, 0, JSON_STRING);
    cv->shared_var_init = get_arr_of_assign_attr("shared_var_init", t, 0, JSON_STRING);

    cv->op_seq = get_behavior_attr("behavior", t, 0, JSON_STRING);
    cv->timer = get_float_attr("timer", t, 0, JSON_STRING);
    cv->rmvhost = get_str_attr("rmvhost", t, 0, JSON_STRING);
    char *port = get_str_attr("rmvport", t, 0, JSON_STRING);
    cv->rmvport = str2Integer(port);

    return 1;
}

void clear_ms_configuration(ms_configuration_vector *cv){
    cv->monitor_id = "";
    cv->shared_var_decl = NULL;
    cv->observable_vars = NULL;
    cv->model_vars = NULL;
    cv->property_vars = NULL;
    cv->reportable_vars = NULL;
    cv->trigger_vars = NULL;
    cv->monitor_atoms = NULL;
    cv->n_monitor_atoms = 0;
    cv->monitor_atom_eval = "unset_eval";
    cv->shared_var_init = NULL;
    cv->op_seq = NULL;
    cv->timer = 0;
    cv->rmvhost = "";
    cv->rmvport = 0;
}

// Build the shared variable attributes table.
// Some of the information is statically initialized in the
// monitor-specific xxxxx_vars.h include file.
// Other info initialized dynamically from the JSON configuration.
// setters and getters could be looked up by name or index same as var list
// Variable value can be directy accessed using va_addr pointer member
// cast to the appropriate type.
shared_var_attr_t*
build_sh_var_attributes(ms_configuration_vector *cv){
	/* currently use statically allocated shared_var_attrs
	shared_var_attr_t *shared_vars = (shared_var_attr_t*)
		calloc(n_shared_vars, sizeof(shared_var_attr_t));
	*/
	shared_var_attr_t *ap = shared_var_attrs;
	for(; ap<shared_var_attrs+N_SHARED_VARS; ap++){
      // use statically allocated vars from monid_xxxxx_vars.h
		// where ap->va_name, va_type, and va_addr set statically
      // these could be dynamically allocated from the CV info
		ap->va_trigger = 
            search_array(ap->va_name,cv->trigger_vars) >= 0;
		ap->va_report =
            search_array(ap->va_name,cv->reportable_vars) >= 0;
		ap->va_property_eval =
            search_array(ap->va_name,cv->property_vars) >= 0;
	}
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

// already done statically in monitor-specific _vars.h file
// Could use the declarations from the JSON configuration
// vector to check consistency here.
void declare_sus_vars(sh_var_decl *SVd){
    if( SVd == NULL ) return;
    VERBOSE_MSG(3,"declaring sus vars\n");
}

void assign_val( char *valstr, shared_var_attr_t *sva ){
    switch(sva->va_type){
	    case svt_Boolean: assigna_Boolean( valstr, sva ); break;
	    case svt_Integer: assigna_Integer( valstr, sva ); break;
	    case svt_Float: assigna_Float( valstr, sva ); break;
	    case svt_Byte: 
	    case svt_String: 
	    case svt_Char: 
	    case svt_Symbol: 
	    case svt_Address: 
	    default: // silently ignore unless verbosity 3
            VERBOSE_MSG(3,"unimplemented type in assign_val\n");
            break;
    }
 
}

void set_ms_sus_vars( sh_var_name_value *SVi, sh_var_name_value *end ){
    VERBOSE_MSG(3,"setting sus vars\n");
    VERBOSE(3) dump_shared_var_attributes(monitor_interface.mi_shared_vars,monitor_interface.mi_num_shared_vars);
    for( sh_var_name_value *ip = SVi; ip < end; ip++ ){
        shared_var_attr_t *sva = sv_attr_by_name( ip->vnv_name );

        if( sva == NULL){printf("attr record for %s not found",ip->vnv_name); continue;}
        VERBOSE(3){printf("setting %s=%s at %lu\n",sva->va_name,ip->vnv_value,(unsigned long)sva->va_addr);fflush(stdout);}

        assign_val( ip->vnv_value, sva );

        // interpret the value according to va_type and assign to *va_addr
        // alternatively:
        //   assign_sh_var_val(char *val_str, sh_var_type val_type, sh_var_val *val_addr);
        // and delete entire following switch

        /*    
        switch(sva->va_type){
		    case svt_Boolean:
                assign_Boolean(ip->vnv_value, (bool*)sva->va_addr, false);
                VERBOSE(3){
                printf("bool value: %s\n",((*(bool*)sva->va_addr) ? "true":"false"));
                fflush(stdout);}
                break;
		    case svt_Integer:
                assign_Integer(ip->vnv_value, (int*)sva->va_addr, false);
                VERBOSE(3){
                printf("int value: %d\n", *(int*)sva->va_addr);
                fflush(stdout);}
                break;
		    case svt_Float:
                assign_Float(ip->vnv_value, (float*)sva->va_addr, false);
                VERBOSE(3){
                printf("float value: %f\n", *(float*)sva->va_addr);
                fflush(stdout);}
                break;
		    case svt_Byte: 
		    case svt_String: 
		    case svt_Char: 
		    case svt_Symbol: 
		    case svt_Address: 
		    default: // silently ignore unless verbosity 3
                VERBOSE_MSG(3,"unimplemented type in set_ms_sus_vars\n");
                break;
        }
        */
    }
    VERBOSE(2) dump_defined_vars();
    VERBOSE(2) dump_sh_vars();
}

// for each of the variable name/value pairs store the value at the var address
// given in the variable attribute table
void assign_ms_sus_vars( sh_var_name_value *SVo, sh_var_name_value *end ){
    if( SVo == NULL ) return;
    VERBOSE_MSG(2,"running assignment ops\n");
    VERBOSE(4) dump_shared_var_attributes(monitor_interface.mi_shared_vars,monitor_interface.mi_num_shared_vars);
    for( sh_var_name_value *op = SVo; op < end; op++ ){
        shared_var_attr_t *sva = sv_attr_by_name(op->vnv_name);

        if( sva == NULL){printf("attr record for %s not found",op->vnv_name); continue;}
        VERBOSE(2){printf("setting %s=%s at %lu\n",sva->va_name,op->vnv_value,(unsigned long)sva->va_addr);fflush(stdout);}

        assign_val( op->vnv_value, sva );
    }
}

ext_atom_op
extend_op(atom_op op, a_arg_kind k1, sh_var_type t1, a_arg_kind k2, sh_var_type t2){
// extend_op( atom_op, a_arg_kind, sh_var_type, a_arg_kind, sh_var_type )
//  extended set of operators encodes types of args in the operator
//
// In the following, the expansion => is handled during compilation
//  whereas the transposition --> is handled during evaluation
//  the expansion => applies the associated equivalence ===
//  negation of the operator triggers the transposition during evaluation
//  argument substripts i and f represent types Integer and Float respectively (e.g. Xi, Yf)
//  in the expanded operators B, I and F represent Boolean, Integer and Float (e.g. eq_B_B)
//
// For eq and ne can just swap args since relation is symmetric:
// 	Xi eq Yf => Xi eq_I_F Yf
// 	Xf eq Yi => Xf -eq_I_F Yi  —->  Yi eq_I_F Xf   (eq_F_I === -eq_I_F) (swap args)
// Same for ne:
// 	Xi ne Yf => Xi ne_I_F Yf
// 	Xf ne Yi => Xf -ne_I_F Yi  —->  Yi ne_I_F Xf   (ne_F_I === -ne_I_F) (swap)

// For lt, gt which are not symmetric:
// 	Xi lt Yf => Xi lt_I_F Yf
// 	Xf lt Yi => Xf -gt_I_F Yi —->   Yi gt_I_F Xf   (lt_F_I === -gt_I_F) (invert relation & swap args)
//  Xi gt Yf => Xi gt_I_F Yf 
//  Xf gt Yi => Xf -lt_I_Fl Yi —-> Yi lt_I_Fl Xf    (gt_F_I === -lt_I_F) (invert & swap)

// le, ge are also not symmetric and follow the pattern of lt, gt
// 	Xi le Yf => Xi le_I_F Yf
// 	Xf le Yi => Xf -ge_I_F Yi —->   Yi ge_I_Fl Xf   (le_F_I === -ge_I_F) (invert & swap)
// 	Xi ge Yf => Xi ge_I_F Yf
// 	Xf ge Yi => Xf -le_I_F Yi —->   Yi le_I_F Xf   (ge_F_I === -le_I_F) (invert & swap)

// expansion:
//  this scheme must be extended if types in addition to B, I, F are to be supported
#define INV inval_op

   static const ext_atom_op op_map [11 /*(int)badop+1*/] [2] [10 /*(int)svt_Symbol+1*/] = { // [op][arg#-1][type]
    // atom_op/type: UNDEF   VAR  bool    int      float   addr byte str  char sym    UNDEF VAR  bool int      float   addr byte str  char sym
    /* var */       {{INV,   INV, var_B,  var_I,   INV,    INV, INV, INV, INV, INV }, {INV, INV, INV, INV,     INV,    INV, INV, INV, INV, INV}},
    /* not */       {{INV,   INV, not_B,  not_I,   INV,    INV, INV, INV, INV, INV }, {INV, INV, INV, INV,     INV,    INV, INV, INV, INV, INV}},
    /* false */     {{Bfalse,INV, INV,    INV,     INV,    INV, INV, INV, INV, INV }, {INV, INV, INV, INV,     INV,    INV, INV, INV, INV, INV}},
    /* true */      {{Btrue, INV, INV,    INV,     INV,    INV, INV, INV, INV, INV }, {INV, INV, INV, INV,     INV,    INV, INV, INV, INV, INV}},
    /* eq  */       {{INV,   INV, eq_B_B, eq_I_I,  eq_F_F, INV, INV, INV, INV, INV }, {INV, INV, INV, eq_I_F, -eq_I_F, INV, INV, INV, INV, INV}},
    /* ne */        {{INV,   INV, ne_B_B, ne_I_I,  ne_F_F, INV, INV, INV, INV, INV }, {INV, INV, INV, ne_I_F, -ne_I_F, INV, INV, INV, INV, INV}},
    /* gt */        {{INV,   INV, INV,   -lt_I_I, -lt_F_F, INV, INV, INV, INV, INV }, {INV, INV, INV, gt_I_F, -lt_I_F, INV, INV, INV, INV, INV}},
    /* lt */        {{INV,   INV, INV,    lt_I_I,  lt_F_F, INV, INV, INV, INV, INV }, {INV, INV, INV, lt_I_F, -gt_I_F, INV, INV, INV, INV, INV}},
    /* ge */        {{INV,   INV, INV,   -le_I_I, -le_F_F, INV, INV, INV, INV, INV }, {INV, INV, INV, ge_I_F, -le_I_F, INV, INV, INV, INV, INV}},
    /* le */        {{INV,   INV, INV,    le_I_I,  le_F_F, INV, INV, INV, INV, INV }, {INV, INV, INV, le_I_F, -ge_I_F, INV, INV, INV, INV, INV}},
    /* badop */     {{INV,   INV, INV,    INV,     INV,    INV, INV, INV, INV, INV }, {INV, INV, INV, INV,     INV,    INV, INV, INV, INV, INV}}
    };

    ext_atom_op eop = INV;

    if( t1==t2 || k2==akind_unused ) // valid when var, not, bool_f, bool_t or binary relation with same types
        eop = op_map[op][0][t1];
    else // t1!=t2 && k2!=akind_unused
        eop = op_map[op][1][t1];

   return eop;
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
           }else{ // return as "var" "name" ""
                *relop = "var"; *arg1 = op_str; // *arg2 = "";
                *a1_k = akind_variable; // *a1_t = svt_UNDEFINED;
            }
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
    if( *atom==')' ){ // no arg2 - only explicit unary fn sym is "not"
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

void
compile_atom_arg(char *arg, a_arg_kind a_k, sh_var_type *a_t, sh_var_val *v){
    shared_var_attr_t *atr;

    memset(v,0,sizeof(sh_var_val)); // clear value
    switch( a_k ){
        case akind_variable:
            atr = lookup_var_attr_by_name(arg);
            if( atr != NULL){
                *a_t = atr->va_type;
                //v->sv_addrval = atr->va_addr;
                v->sv_addrval = atr;
            }else{ /* undeclared variable TODO */ }
            break;
        case akind_constant:
            switch( *a_t ){
                case svt_Boolean: v->sv_boolval = str2Boolean(arg); break;
                case svt_Integer: v->sv_intval = str2Integer(arg); break;
                case svt_Float: v->sv_floatval = str2Float(arg); break;
                default: break;
            }
            break;
        case akind_unused:
        default: break;
    }   
}

void
compile_monitor_atom(monitor_atom *ma){
    char *op, *a1, *a2;
    shared_var_attr_t *a1a, *a2a;

    parse_atom(ma->ma_aex, &op, &a1, &a2, &ma->ma_op,
        &ma->ma_arg1_knd, &ma->ma_arg1_typ, &ma->ma_arg2_knd, &ma->ma_arg2_typ);

    compile_atom_arg(a1, ma->ma_arg1_knd, &ma->ma_arg1_typ, &ma->ma_arg1_val);
    compile_atom_arg(a2, ma->ma_arg2_knd, &ma->ma_arg2_typ, &ma->ma_arg2_val);

    ma->ma_ext_op = extend_op(ma->ma_op, ma->ma_arg1_knd, ma->ma_arg1_typ,
                            ma->ma_arg2_knd, ma->ma_arg2_typ);

    if( ma->ma_op == var || ma->ma_op == not ) // get the type and the address of the variable
        if( ma->ma_arg1_typ != svt_Boolean && ma->ma_arg1_typ != svt_Integer )
            ma->ma_ext_op = inval_op; // not a valid argument for operator
}

void
compile_monitor_atoms(){
    for(monitor_atom *a=monitor_atoms; a<next_monitor_atom; a++)
        compile_monitor_atom(a);
}

bool
arg_instantiate(a_arg_kind a_k, sh_var_type a_t, sh_var_val *var, sh_var_val *val){
    if( a_k == akind_variable ){ // get the value from the var
        if( ! ((shared_var_attr_t*)(var->sv_addrval)) -> va_property_eval )
            return false; // return false for undefined arg
        switch( a_t ){
            case svt_Boolean: val->sv_boolval =
                *(bool*)(((shared_var_attr_t*)(var->sv_addrval))->va_addr); break;
            case svt_Integer: val->sv_intval =
                *(int*)(((shared_var_attr_t*)(var->sv_addrval))->va_addr);
                // we treate INT_MIN as the undefined int explicitly
                if( val->sv_intval == INT_MIN ) return false;
                break;
            case svt_Float: val->sv_floatval =
                *(float*)(((shared_var_attr_t*)(var->sv_addrval))->va_addr); break;
                // we treat NAN as undefined float, which takes care of itself in comparisons
            //case svt_Boolean: val->sv_boolval = *(bool*)(var->sv_addrval); break;
            //case svt_Integer: val->sv_intval = *(int*)(var->sv_addrval); break;
            //case svt_Float: val->sv_floatval = *(float*)(var->sv_addrval); break;
            default: break;
        }
    }else if( a_k == akind_constant ) *val = *var;
    return true;
}

bool
af_evaluate(monitor_atom *at){
    sh_var_val val1, val2;
    ext_atom_op op = at->ma_ext_op;
    bool d1, d2; // defined val1, val2

    memset(&val1,0,sizeof(sh_var_val)); // clear value
    memset(&val2,0,sizeof(sh_var_val)); // clear value

    d1 = arg_instantiate(at->ma_arg1_knd, at->ma_arg1_typ, &at->ma_arg1_val, &val1);
    d2 = arg_instantiate(at->ma_arg2_knd, at->ma_arg2_typ, &at->ma_arg2_val, &val2);

    if( !(d1 && d2) ) return false;

    sh_var_val *v1 = &val1; sh_var_val *v2 = &val2;

    bool inverted;
    if( (inverted = ((int)op < 0)) ){ v1 = &val2; v2 = &val1; op = -op; }

    switch( op ){
        case not_B: return v1->sv_boolval ? false : true;
        case not_I: return v1->sv_intval == 0;
        case var_B: return v1->sv_boolval;
        case var_I: return v1->sv_intval != 0;
        case Bfalse: return false;
        case Btrue: return true;
        
        case eq_B_B: return v1->sv_boolval == v2->sv_boolval;
        case eq_I_I: return v1->sv_intval == v2->sv_intval;
        case eq_F_F: return v1->sv_floatval == v2->sv_floatval;
        case eq_I_F: return v1->sv_intval == v2->sv_floatval;   // F<->I

        case ne_B_B: return v1->sv_boolval != v2->sv_boolval;
        case ne_I_I: return v1->sv_intval != v2->sv_intval;
        case ne_F_F: return v1->sv_floatval != v2->sv_floatval;
        case ne_I_F: return v1->sv_intval != v2->sv_floatval;

        case lt_I_I: return v1->sv_intval < v2->sv_intval;
        case lt_F_F: return v1->sv_floatval < v2->sv_floatval;
        case lt_I_F: return v1->sv_intval < v2->sv_floatval;

        case gt_I_F: return v1->sv_intval > v2->sv_floatval;
        
        case le_I_I: return v1->sv_intval <= v2->sv_intval;
        case le_F_F: return v1->sv_floatval <= v2->sv_floatval;
        case le_I_F: return v1->sv_intval <= v2->sv_floatval;

        case ge_I_F: return v1->sv_intval >= v2->sv_floatval;

        default: return false; // unknown operator
    }
}

char **
aT_list_constructor(){
    static char *true_atoms[MON_ATOM_SZ];
    char **next_atom = true_atoms;
    monitor_interface_t *mi = &monitor_interface;
    monitor_atom *atm = monitor_atoms;
    
    if( strcmp(mi->mi_cv.monitor_atom_eval, "ms_eval")==0 ){
        VERBOSE(3)dump_compiled_atoms();
        for( int i=0; i<mi->mi_cv.n_monitor_atoms; i++, atm++ ){
            bool e;
            if( (e = af_evaluate(atm)) ) *next_atom++ = atm->ma_aid;
            else {}
            VERBOSE(3){printf(" atom \"%s\":\"%s\" evaluated %s\n",
                atm->ma_aid, atm->ma_aex, e?"true":"false");fflush(stdout);}
        }
    }
    *next_atom = NULL; // terminator in case dump_strings used
    return true_atoms; // return list of strings
}

char *
or_list_constructor(){
    static char reportout[STRINGS_SZ];
    reportout[0] = '\0';

    freenumstrings(); // free past Integer/Float strings
    next_sh_var_name_value = sh_var_name_values; // reset
    for( shared_var_attr_t *sva = shared_var_attrs;
        sva < shared_var_attrs+N_SHARED_VARS; sva++){
    
        if( sva->va_report ){
            // name from the attribute record for the variable
            next_sh_var_name_value->vnv_name = sva->va_name;

            strcat(reportout, next_sh_var_name_value->vnv_name);
            strcat(reportout,"=");

            // if value string is a number (Integer/Float) it will be freed
            next_sh_var_name_value->vnv_value =
                value_str(sva->va_type, sva->va_addr);
            next_sh_var_name_value->vnv_type = sva->va_type;

            strcat(reportout, next_sh_var_name_value->vnv_value);
            strcat(reportout," ");
            ++next_sh_var_name_value;
        }
    }
    return reportout;
    // Side effect: reportable variable values in sh_var_name_values
}

void or_list_free(){
    sh_var_name_value *nv = sh_var_name_values;
    for( ; nv < next_sh_var_name_value; nv++){
        if( (nv->vnv_type==svt_Integer || nv->vnv_type==svt_Float) && strcmp("undefined",nv->vnv_value)!=0 )
            free( nv->vnv_value );
    }
    next_sh_var_name_value = sh_var_name_values;
}

bool is_number_str(char *s){
    // not rigorous about repeated/combined + - and .
    // assumes known well-formed numbers, e.g. 10 -5 3.14 +2 -0.4
    if( s==NULL || *s=='\0' ) return false;
    while( *s && ( ('0' <= *s && *s <= '9') || *s=='-' || *s=='+' || *s=='.' ) ) s++;
    return *s=='\0';
}

void ms_heartbeat(char *mid, char *sid, char **ATl, char *ORl, int *Response){
    // prepare the heartbeat message and send to MEP
    //   mid identifies the monitor
    //   sid identifies the NuRV session
    //   ATl is the list of ids of true atoms
    //   ORl is the list of current assignment of the reportable vars
    //
    // The JSON string looks like, e.g.:
    //   {
    //       "monid":"monid_00001", "sessid":"12345",
    //       "atoms":[ "p","a1" ],
    //       "vars":[ {"p":true}, {"n":2} ]
    //   }
    //
    // On the Prolog end this is converted to a json-ized Prolog term:
    //   json([... json([atoms=[p,a1],vars=[json([p=true]),json([n=2])]]) ])
    //
    // We use the ATl array but don't use the ORl string. (not needed anymore)
    // Instead we use sh_var_name_values, constructed as a side
    // effect by or_list_constructor (which must be consumed
    // before the next call).
    // This way we already have the name and value as individual strings

    VERBOSE(2){printf("ms_heartbeat: mid=%s, sid=%s\n",mid,sid);}
    VERBOSE(2){dump_strings(" true atoms", ATl); fflush(stdout);}
    VERBOSE(2){dump_sv_inits(" variable report in heartbeat");}

    char HB_message[JSON_STRING_SZ]; char *HB = HB_message; int nc;
    nc = sprintf(HB,"{\n\t\"monid\":\"%s\", \"sessid\":\"%s\",\n\t\"atoms\":[ ",mid,sid); HB += nc;
    for( int i=0; *ATl; ATl++, i++, HB+=nc ){
        nc = sprintf(HB,"%s\"%s\"",i?", ":"",*ATl);
    }
    nc = sprintf(HB," ],\n\t\"vars\":[ "); HB += nc;

    sh_var_name_value* vnv = sh_var_name_values; char *quote;
    for( ; vnv < next_sh_var_name_value; vnv++, HB+=nc ){
        quote = ( // don't quote any of the following
                strcmp(vnv->vnv_value,"true") == 0
                || strcmp(vnv->vnv_value,"false") == 0
                || strcmp(vnv->vnv_value,"null") == 0
                || is_number_str(vnv->vnv_value)
        ) ?  "" : "\"";
        nc = sprintf(HB,"%s{\"%s\":%s%s%s}",
                    vnv!=sh_var_name_values?", ":"",vnv->vnv_name, quote, vnv->vnv_value, quote);
   }
    nc = sprintf(HB," ]\n}\n");  HB += nc;
    VERBOSE(1){printf("Heartbeat message (JSON):\n%s\n", HB_message); fflush(stdout);}

    mep_heartbeat(HB_message,Response);
}

/*
 * MS INITIALIZATION, UN-INIT and RE-INIT
 */

// initialize the monitor interface structure and the ms config vector
// contained in it
monitor_interface_t*
init_ms(monitor_interface_t *mip){
    if( mip->mi_mstatus != monitor_uninitialized )
        return mip; // only initialize once

    if( initialize_ms_configuration(&mip->mi_cv) == 0 ){
        return NULL;
    };
    
    compile_monitor_atoms();

    VERBOSE_MSG(3,"complete initialization of cv\n");

    mip->mi_JSON_cv = JSON_STRING;
    // following could also be gotten directly from shared_var_decl
    // mip->mi_num_shared_vars = len_narr((void*)mip->mi_cv.observable_vars);
    // should be consistent unless something in generated files has manually been changed
    mip->mi_num_shared_vars = N_SHARED_VARS;
    mip->mi_shared_vars = build_sh_var_attributes(&mip->mi_cv);
    mip->mi_mstatus = monitor_initialized;
    VERBOSE(3)dump_shared_var_attributes(shared_var_attrs,N_SHARED_VARS);

    if( mip->mi_cv.shared_var_decl != NULL )
        declare_sus_vars( mip->mi_cv.shared_var_decl ); // noop because already done above

    if( mip->mi_cv.shared_var_init != NULL )
        assign_ms_sus_vars( sh_var_name_values, next_sh_var_name_value );

    VERBOSE(3)dump_sv_inits("sus var initializations (symbolic)");

    return(mip); // should return NULL on failure TODO check conditions
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

void split_time(float t, float* secs, float* usecs){
    float tus = round(t*1000000.0);
    *usecs = fmod(tus, 1000000);
    *secs = (tus - *usecs)/1000000.0;
}

void run_timer(){
    // set up itimerval for repeating interval and call library setitimer
    float timer_sec, timer_usec;
    static struct itimerval new, old;
    split_time(monitor_interface.mi_cv.timer, &timer_sec, &timer_usec);
    new.it_interval.tv_sec = timer_sec;
    new.it_interval.tv_usec = timer_usec;
    new.it_value.tv_sec = timer_sec;
    new.it_value.tv_usec = timer_usec;
    setitimer(ITIMER_REAL, &new, &old);
}

void
initiate_timer(float interval){
    monitor_interface.mi_cv.timer = interval;
    // TODO - set up callback to service_timer() on timer signal
    run_timer();
}

void
stop_timer(){
    struct itimerval tv;
    monitor_interface.mi_cv.timer = 0;
    // cancel outstanding timer
    tv.it_interval.tv_sec = 0.0;
    tv.it_interval.tv_usec = 0.0;
    tv.it_value.tv_sec = 0.0;
    tv.it_value.tv_usec = 0.0;
    setitimer(ITIMER_REAL, &tv, NULL);
}

void
run_sequence(){
    if( monitor_interface.mi_cv.op_seq != NULL )
        assign_ms_sus_vars(behavior_seq, next_behavior_op );
}