/*
 * dump functions to visualize configuration data
 * used for visibiltiy during development
 */

// print an array of strings (like an argv) on separate lines
void prt_array(char **array){ printf("\n");
    for(char **p=array; *p!=NULL; p++)
        printf("%s\n",*p);
    fflush(stdout);
}

// print array of strings on one line with a label:
void dump_strings(char *what, char *s[]){
    printf("%s:\n", what);
    for( char **p=s; *p!=NULL; p++ ){
        printf("  %s",*p);
    }
    printf("\n");
    fflush(stdout);
}

void dump_sv_decls(char *label){
    printf("%s:\n", label);
    for(sh_var_decl *dp = sh_var_decls; dp < next_sh_var_decl; dp++)
        printf("  %s:%s\n",dp->name,dp->type);
    fflush(stdout);
}

void dump_sv_inits(char *label){
    printf("%s:\n", label);
    for(sh_var_name_value *ip = sh_var_name_values; ip < next_sh_var_name_value; ip++)
        printf("  %s=%s\n",ip->vnv_name,ip->vnv_value);
    fflush(stdout);
}

void dump_assigns(char *label){
    printf("%s:\n", label);
    for(sh_var_name_value *op = behavior_seq; op < next_behavior_op; op++)
        printf("  %s=%s\n",op->vnv_name,op->vnv_value);
    fflush(stdout);
}

void dump_matoms(char *label){
    monitor_atom **p;
    printf("%s:\n", label);
    for( monitor_atom *p = monitor_atoms; p < next_monitor_atom; p++)
        printf("  %s:%s\n", p->ma_aid, p->ma_aex);
    fflush(stdout);
}

void interpret_arg_val(){
    // HERE HERE
}

void
dump_compiled_atom_arg(a_arg_kind a_k, sh_var_type a_t, sh_var_val *a_v){
    printf("kind=%s, type=%s, value=", akind_names[a_k], sv_type_names[a_t]);
    switch(a_k){
        case akind_constant:
            switch(a_t){
                case svt_Boolean:
                    printf("%s",a_v->sv_boolval? "true":"false");
                    break;
                case svt_UNDEFINED: printf("Undef"); break;
                case svt_Integer:
                    printf("%d",a_v->sv_intval); break;
                case svt_Float:
                    printf("%f",a_v->sv_floatval); break;
                default: break;
            } break;
        case akind_variable:
            //printf("(%lu)",(unsigned long)a_v->sv_addrval);
            printf("(%lu)",(unsigned long)( (shared_var_attr_t*)(a_v->sv_addrval) )->va_addr );
            break;
        case akind_unused:
            printf("%d",a_v->sv_intval);
        default: break;
    }
}

void dump_compiled_atom(int i, monitor_atom *at){
    char *sign; ext_atom_op eop;
        eop = at->ma_ext_op;
        if( (int)eop < 0 ){sign="-"; eop = -eop; }else{ sign=""; }
        printf("  %02d  aid=%s, aex=%s, op=%s, ext_op=%s%s,",
            i, at->ma_aid, at->ma_aex,
            (at->ma_op==0 ? "var" : atom_op_names[at->ma_op]),
            sign, ext_atom_op_names[ eop ]
        );
        printf("  arg1: [");
        dump_compiled_atom_arg(at->ma_arg1_knd,at->ma_arg1_typ,&at->ma_arg1_val);
        printf("]  arg2: [");
        dump_compiled_atom_arg(at->ma_arg2_knd,at->ma_arg2_typ,&at->ma_arg2_val);
        printf("]\n"); 
        fflush(stdout);
}

void dump_compiled_atoms(){
    monitor_interface_t *mi = &monitor_interface;
    monitor_atom *at = monitor_atoms;
    sh_var_val svv; char *sign; ext_atom_op eop;

    printf("compiled_atoms (ma_):\n");
    for( int i=0; i < mi->mi_cv.n_monitor_atoms; i++, at++ ){
        dump_compiled_atom(i,at);
        /*
        eop = at->ma_ext_op;
        if( (int)eop < 0 ){sign="-"; eop = -eop; }else{ sign=""; }
        //printf("eop=%d\n",eop);fflush(stdout);
        //continue;
        printf("  %02d  aid=%s, aex=%s, op=%s, ext_op=%s%s,",
            i, at->ma_aid, at->ma_aex,
            (at->ma_op==0 ? "var" : atom_op_names[at->ma_op]),
            sign, ext_atom_op_names[ eop ]
        );
        printf("  arg1: [");
        dump_compiled_atom_arg(at->ma_arg1_knd,at->ma_arg1_typ,&at->ma_arg1_val);
        printf("]  arg2: [");
        dump_compiled_atom_arg(at->ma_arg2_knd,at->ma_arg2_typ,&at->ma_arg2_val);
        printf("]\n"); 
        fflush(stdout);
        */
    }
    fflush(stdout);
}

void dump_parse(char *aex){
    char *op, *a1, *a2; atom_op aop;
    a_arg_kind arg1_k, arg2_k;
    sh_var_type arg1_t, arg2_t;

    parse_atom(aex, &op, &a1, &a2, &aop, &arg1_k, &arg1_t, &arg2_k, &arg2_t);
    printf("parsed \"%s\":   \"%s\" \"%s\" \"%s\"  ===>  %s%s %s:%s %s:%s\n",
        aex, op, a1, a2,
        (aop==var ? "var:" : ""),
        (aop <= le) ? atom_op_names[ aop ] : "badop",
        akind_names[arg1_k], sv_type_names[arg1_t],
        akind_names[arg2_k], sv_type_names[arg2_t] );
    fflush(stdout);
}

void dump_ms_cv(ms_configuration_vector *cv){
    printf("dump ms cv:\n");
    printf(" monitor_id: %s\n", cv->monitor_id);
    dump_sv_decls(" sh_var_decl");
    dump_strings(" observable_vars", cv->observable_vars );
    dump_strings(" model_vars", cv->model_vars );
    dump_strings(" property_vars", cv->property_vars );
    dump_strings(" reportable_vars", cv->reportable_vars );
    dump_strings(" trigger_vars", cv->trigger_vars );
    dump_matoms(" monitor_atoms");
    printf(" monitor_atom_eval: %s\n", cv->monitor_atom_eval);
    dump_sv_inits(" sh_var_init");
    if( cv->op_seq != NULL ) dump_assigns(" op_seq");
    printf(" timer: %f\n", cv->timer);
    printf(" rmvhost: %s\n", cv->rmvhost);
    printf(" rmvport: %d\n", cv->rmvport);
    fflush(stdout);
}

//extern void dump_strings(char *what, char *s[]);

void dump_one_shared_var_attributes(shared_var_attr_t *sva){
    int i = sva - shared_var_attrs; 
    printf("  %02d\tname=%s, type=%d, addr=%lu, trig=%d, report=%d, prop=%d, getter=%lu, setter=%lu\n",
	i, sva->va_name, sva->va_type,
	(unsigned long)sva->va_addr,
        sva->va_trigger, sva->va_report,
        sva->va_property_eval,
	(unsigned long)sva->va_getter,
	(unsigned long)sva->va_setter
	);
    fflush(stdout);
}

void dump_shared_var_attributes(void *sva, int nvars){
    shared_var_attr_t *sv = (shared_var_attr_t *) sva;
	printf(" dumping %d shared variable attributes:\n",nvars);
	for(int i=0; i<nvars; i++, sv++)
        dump_one_shared_var_attributes(sv);
}

void dump_monitor_interface(){
    monitor_interface_t *mip = &monitor_interface;
    printf("dump monitor interface:\n");
    printf(" mstatus=%s\n",mstatus_string(mip->mi_mstatus));
    printf(" JSON configuration vector:\n%s\n",mip->mi_JSON_cv);
    //printf(" number of shared vars=%d\n",mip->mi_num_shared_vars);
    dump_shared_var_attributes(mip->mi_shared_vars, mip->mi_num_shared_vars);
    dump_ms_cv(&mip->mi_cv);
    printf("  session ID: %s\n", mip->mi_sessid);
    fflush(stdout);
}

void dump_sh_vars(){ // HERE
    monitor_interface_t *mip = &monitor_interface;
    shared_var_attr_t *vap = mip->mi_shared_vars;
    printf("dumping %d shared vars:\n",mip->mi_num_shared_vars); fflush(stdout);
    for( int i=0; i < mip->mi_num_shared_vars; i++,vap++ ){
        char *s;
        dump_one_shared_var_attributes(vap);
        printf("  var %s=", vap->va_name); fflush(stdout);
        switch(vap->va_type){
		    case svt_Byte: break;
		    case svt_String: break;
		    case svt_Boolean:
                printf("at %lu: ",(unsigned long)(vap->va_addr));fflush(stdout);
			    printf("%s\n", Boolean2str(*(bool*)(vap->va_addr)));
                fflush(stdout);
                // true and false are static strings
                break;
		    case svt_Integer:
                printf("at %lu: ",(unsigned long)vap->va_addr);fflush(stdout);
			    s = Integer2str(*(int*)(vap->va_addr));
			    printf("%s\n", s); fflush(stdout); free(s);                
                break;
                //printf("%d\n",va_addr->sv_intval); break;
		    case svt_Float:
                printf("at %lu: ",(unsigned long)vap->va_addr);fflush(stdout);
			    s = Float2str(*(float*)(vap->va_addr));
			    printf("%s\n", s); fflush(stdout); free(s);
                fflush(stdout);
                break;
		    case svt_Char: break;
		    case svt_Symbol: break;
		    case svt_Address: break;
		    default: break;
        }
    }
}
