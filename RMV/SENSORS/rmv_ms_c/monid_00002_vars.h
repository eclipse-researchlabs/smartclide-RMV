// This file is created by RMV Monitor Creation for variables
// shared between the SUS and the Monitor Sensor monid_00002.
// These declarations must appear in the same order as in the
// shared variable list in the MS configuration vector generated
// by Monitor Creation.
// This file is included by ms_vars.h
int n;
int o;
bool p;
bool q;
float r;
float s;

sh_var_type shared_var_types[] = {
    svt_Integer,
    svt_Integer,
    svt_Boolean,
    svt_Boolean,
    svt_Float,
    svt_Float
};

sh_var_val *shared_var_addresses[] = {
    (sh_var_val *)(&n),
    (sh_var_val *)(&o),
    (sh_var_val *)(&p),
    (sh_var_val *)(&q),
    (sh_var_val *)(&r),
    (sh_var_val *)(&s)
};

// == && report_all         trigger with no change
// == && !report_all        no trigger
// != && report_all         trigger with change
// != !report_all           trigger with change
int report_all = 1; // report all triggers, even if no val change

extern void ms_step(int, sh_var_val, sh_var_val);

sh_var_val int_setter(int var_idx, sh_var_val newval){
    sh_var_val *var = shared_var_addresses[var_idx];
    sh_var_val oldval = *var;
    if((*var).sv_intval != newval.sv_intval || report_all ){
        (*var).sv_intval = newval.sv_intval;
        ms_step(var_idx, oldval, newval);
    }
    return oldval;
}

sh_var_val bool_setter(int var_idx, sh_var_val newval){
    sh_var_val *var = shared_var_addresses[var_idx];
    sh_var_val oldval = *var;
    if((*var).sv_boolval != newval.sv_boolval || report_all ){
        (*var).sv_boolval = newval.sv_boolval;
        ms_step(var_idx, oldval, newval);
    }
    return oldval;
}

sh_var_val float_setter(int var_idx, sh_var_val newval){
    sh_var_val *var = shared_var_addresses[var_idx];
    sh_var_val oldval = *var;
    if((*var).sv_floatval != newval.sv_floatval || report_all ){
       (*var).sv_floatval = newval.sv_floatval;
        ms_step(var_idx, oldval, newval);
    }
    return oldval;
}

sh_var_val int_getter(int var_idx){
    return *shared_var_addresses[var_idx];
}
sh_var_val bool_getter(int var_idx){
    return *shared_var_addresses[var_idx];
}
sh_var_val float_getter(int var_idx){
    return *shared_var_addresses[var_idx];
}

void setter_n(int newval){ int_setter(0,(sh_var_val)newval); }
int getter_n(){ return n; }

void setter_o(int newval){ int_setter(1,(sh_var_val)newval); }
int getter_o(){ return o; }

void setter_p(bool newval){ bool_setter(2,(sh_var_val)newval); }
bool getter_p(){ return p; }

void setter_q(bool newval){ bool_setter(3,(sh_var_val)newval); }
bool getter_q(){ return q; }

void setter_r(float newval){ float_setter(4,(sh_var_val)newval); }
float getter_r(){ return r;}

void setter_s(float newval){ float_setter(5,(sh_var_val)newval); }
float getter_s(){ return s;}
