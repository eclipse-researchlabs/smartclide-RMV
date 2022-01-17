/*
 * dump functions to visualize configuration data
 * used for visibiltiy during development
 */

// print an array of strings (like an argv)
void prt_array(char **array){ printf("\n");
    for(char **p=array; *p!=NULL; p++)
        printf("%s\n",*p);
    fflush(stdout);
}

// print array of strings with a label:
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
        printf("  %s=%s\n",ip->name,ip->value);
    fflush(stdout);
}

void dump_matoms(char *label){
    monitor_atom **p;
    printf("%s:\n", label);
    for( monitor_atom *p = monitor_atoms; p < next_monitor_atom; p++)
        printf("  %s:%s\n", p->aid, p->aex);
    fflush(stdout);
}

void dump_ms_cv(ms_configuration_vector *cv){
    printf("dump ms cv:\n");
    printf(" monitor_id: %s\n", cv->monitor_id);
    dump_sv_decls(" sh_var_decls");
    dump_strings(" observable_vars", cv->observable_vars );
    dump_strings(" model_vars", cv->model_vars );
    dump_strings(" property_vars", cv->property_vars );
    dump_strings(" reportable_vars", cv->reportable_vars );
    dump_strings(" trigger_vars", cv->trigger_vars );
    dump_matoms(" monitor_atoms");
    printf(" monitor_atom_eval: %s\n", cv->monitor_atom_eval);
    dump_sv_inits(" sh_var_inits");
    fflush(stdout);
}

extern void dump_strings(char *what, char *s[]);

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

void dump_shared_var_attributes(shared_var_attr_t *sva, int nvars){
	printf(" dumping %d shared variable attributes:\n",nvars);
	for(int i=0; i<nvars; i++, sva++)
        dump_one_shared_var_attributes(sva);
}

void dump_monitor_interface(monitor_interface_t *mip){
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

