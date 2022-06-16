// This file is created by RMV Monitor Creation for variables
// shared between the SUS and the Monitor Sensor.
// These declarations (must) appear in the same order as in the
// shared variable list in the MS configuration vector generated
// by Monitor Creation. This file is included as monitor_vars.h
// by sensor.h of the C monitor sensor.

int m;
int n;
int o;
bool p;
bool q;
float r;
float s;

void get_m(void*ip){*(int*)ip=m;}
void set_m(void*newp, void*oldp){*(int*)oldp = int_setter_by_addr(&m,*(int*)newp);}
void get_n(void*ip){*(int*)ip=n;}
void set_n(void*newp, void*oldp){*(int*)oldp = int_setter_by_addr(&n,*(int*)newp);}
void get_o(void*ip){*(int*)ip=o;}
void set_o(void*newp, void*oldp){*(int*)oldp = int_setter_by_addr(&o,*(int*)newp);}
void get_p(void*ip){*(bool*)ip=p;}
void set_p(void*newp, void*oldp){*(bool*)oldp = bool_setter_by_addr(&p,*(bool*)newp);}
void get_q(void*ip){*(bool*)ip=q;}
void set_q(void*newp, void*oldp){*(bool*)oldp = bool_setter_by_addr(&q,*(bool*)newp);}
void get_r(void*ip){*(float*)ip=r;}
void set_r(void*newp, void*oldp){*(float*)oldp = float_setter_by_addr(&r,*(float*)newp);}
void get_s(void*ip){*(float*)ip=s;}
void set_s(void*newp, void*oldp){*(float*)oldp = float_setter_by_addr(&s,*(float*)newp);}

shared_var_attr_t shared_var_attrs[] = {
//  va_name va_type va_addr va_trig va_rep va_prop va_getter va_setter
    {"m", svt_Integer, &m, false, false, false, &get_m, &set_m, &set_m},
    {"n", svt_Integer, &n, false, false, false, &get_n, &set_n, &set_n},
    {"o", svt_Integer, &o, false, false, false, &get_o, &set_o, &set_o},
    {"p", svt_Boolean, &p, false, false, false, &get_p, &set_p, &set_p},
    {"q", svt_Boolean, &q, false, false, false, &get_q, &set_q, &set_q},
    {"r", svt_Float, &r, false, false, false, &get_r, &set_r, &set_r},
    {"s", svt_Float, &s, false, false, false, &get_s, &set_s, &set_s},
    0
};

#define N_SHARED_VARS 7

void dump_defined_vars(){
    printf("defined vars DIRECT access:\n");
    printf(" m=%d\n",m);
    printf(" n=%d\n",n);
    printf(" o=%d\n",o);
    printf(" p=%s\n",p?"true":"false");
    printf(" q=%s\n",q?"true":"false");
    printf(" r=%f\n",r);
    printf(" s=%f\n",s);
    fflush(stdout);
}

static const char global_monitor_id[] = "monid_00005";

