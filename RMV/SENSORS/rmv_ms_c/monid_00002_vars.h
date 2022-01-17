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
/*
sh_var_type shared_var_types[] = {
    svt_Integer,
    svt_Integer,
    svt_Boolean,
    svt_Boolean,
    svt_Float,
    svt_Float};
sh_var_val *shared_var_addresses[] = {
    (sh_var_val *)(&n),
    (sh_var_val *)(&o),
    (sh_var_val *)(&p),
    (sh_var_val *)(&q),
    (sh_var_val *)(&r),
    (sh_var_val *)(&s)};
void *shared_var_addresses[] = {
    (&n),
    (&o),
    (&p),
    (&q),
    (&r),
    (&s)};
*/
//int set_n(int newval){ return int_setter_by_addr(&n,newval); }
//int get_n(){ return n; }
//void get_n(int*ip){ *ip = n; }
//void set_n(int*newp, int*oldp){ *oldp = int_setter_by_addr(&n,*newp); }
void get_n(void*ip){ *(int*)ip = n; }
void set_n(void*newp, void*oldp){*(int*)oldp = int_setter_by_addr(&n,*(int*)newp);}

void get_o(void*ip){ *(int*)ip = o; }
void set_o(void*newp, void*oldp){*(int*)oldp = int_setter_by_addr(&o,*(int*)newp);}

void get_p(void*ip){ *(bool*)ip = p; }
void set_p(void*newp, void*oldp){*(bool*)oldp = bool_setter_by_addr(&p,*(bool*)newp);}

void get_q(void*ip){ *(bool*)ip = q; }
void set_q(void*newp, void*oldp){*(bool*)oldp = bool_setter_by_addr(&q,*(bool*)newp);}

void get_r(void*ip){ *(float*)ip = r; }
void set_r(void*newp, void*oldp){*(float*)oldp = float_setter_by_addr(&r,*(float*)newp);}

void get_s(void*ip){ *(float*)ip = s; }
void set_s(void*newp, void*oldp){*(float*)oldp = float_setter_by_addr(&s,*(float*)newp);}

/*
int set_o(int newval){ return int_setter_by_addr(&o,newval); }
int get_o(){ return o; }

bool set_p(bool newval){ return bool_setter_by_addr(&p,newval); }
bool get_p(){ return p; }

bool set_q(bool newval){ return bool_setter_by_addr(&q,newval); }
bool get_q(){ return q; }

float set_r(float newval){ return float_setter_by_addr(&r,newval); }
float get_r(){ return r;}

float set_s(float newval){ return float_setter_by_addr(&s,newval); }
float get_s(){ return s;}
*/
//typedef void(*getr_t)();

shared_var_attr_t shared_var_attrs[] = {
//  va_name	va_type		va_addr va_trig va_rep va_prop va_getter va_setter
    {"n",	svt_Integer,	&n,	false,  false, false,  &get_n,   &set_n },
    {"o",	svt_Integer,	&o,	false,  false, false,  &get_o,   &set_o },
    {"p",	svt_Boolean,	&p,	false,  false, false,  &get_p,   &set_p },
    {"q",	svt_Boolean,	&q,	false,  false, false,  &get_q,   &set_q },
    {"r",	svt_Float,	&r,	false,  false, false,  NULL,     NULL },
    {"s",	svt_Float,	&s,	false,  false, false,  NULL,     NULL }
};

#define N_SHARED_VARS sizeof(shared_var_attrs)/sizeof(shared_var_attr_t)

int report_all = 1; // report all triggers, even if no val change

