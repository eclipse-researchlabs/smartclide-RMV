/*
 * Monitor Sensor Shared Variables Access
 *
 *   There are two distinct methods currently implemented
 *   and under consideration. The final approach will be
 *   some hybrid of the two, or perhaps choices will be
 *   preserved. There are interpretations of both methods
 *   contained in the curent implementation.
 * 
 *   The first method is generation of everything dynamically
 *   from the MS configuration vector. Carried to it's logical
 *   extreme, this method would require the service setter/
 *   getter to refer to the shared variables by their string
 *   name. A less extreme approach is to have a minimal
 *   include file to declare the shared variables as C variables.
 *   In this case the dynamic bit is used to establish the link
 *   between the string names and the C variable names.
 * 
 *   The second method depends more heavily on an include file,
 *   such as the example "monitor_vars.h" included by this
 *   file, it contains declarations specific to the service
 *   and monitor and more of the information to link to the
 *   shared vars.
 */

// gets value of argument named variable
typedef sh_var_val (*get_sh_var_by_name)(char *);

// sets argument named variable to new value returning old value
typedef sh_var_val (*set_sh_var_by_name)(char *, sh_var_val);

// gets value of argument indexed variable
typedef sh_var_val (*get_sh_var_by_index)(int);

// sets argument indexed variable to new value returning old value
//typedef sh_var_val (*set_sh_var_by_index)(int, sh_var_val);
//typedef void (*set_sh_var_by_index)(int, sh_var_val);

extern void ms_step_idx(int, void*, void*);
extern void ms_step_addr(void*, sh_var_type, void*, void*);

int int_setter(int var_idx, int newval);
bool bool_setter(int var_idx, bool newval);
float float_setter(int var_idx, float newval);
int int_getter(int var_idx);
bool bool_getter(int var_idx);
float float_getter(int var_idx);

//typedef void*(*getr_t)(int*);
//typedef void*(*setr_t)(void*,int*);
// TODO - use a macro here

int int_setter(int var_idx, int newval){
    int *var = shared_var_attrs[var_idx].va_addr;
    int oldval = *var;
    if( *var != newval || ms_global_report_all ){
        *var = newval;
        ms_step_idx(var_idx, (void*)&oldval, (void*)&newval);
    }
    return oldval;
}

bool bool_setter(int var_idx, bool newval){
    bool *var = shared_var_attrs[var_idx].va_addr;
    bool oldval = *var;
    if( *var != newval || ms_global_report_all ){
        *var = newval;
        ms_step_idx(var_idx, (void*)&oldval, (void*)&newval);
    }
    return oldval;
}

float float_setter(int var_idx, float newval){
    float *var = shared_var_attrs[var_idx].va_addr;
    float oldval = *var;
    if( *var != newval || ms_global_report_all ){
        *var = newval;
        ms_step_idx(var_idx, (void*)&oldval, (void*)&newval);
    }
    return oldval;
}

int int_getter(int var_idx){
    return (int) *(int*)shared_var_attrs[var_idx].va_addr;
}
bool bool_getter(int var_idx){
    return (bool) *(bool*)shared_var_attrs[var_idx].va_addr;
}
float float_getter(int var_idx){
    return (float) *(float*)shared_var_attrs[var_idx].va_addr;
}
/*
set_sh_var_by_index setter_by_type[] = {
	NULL, // &byte_setter,
	NULL, // &string_setter,
	&bool_setter,
	&int_setter,
	&float_setter,
	NULL, // &char_setter,
	NULL, // &symbol_setter,
	NULL, // &addr_setter
};

get_sh_var_by_index getter_by_type[] = {
	NULL, // &byte_getter,
	NULL, // &string_getter,
	&bool_getter,
	&int_getter,
	&float_getter,
	NULL, // &char_getter,
	NULL, // &symbol_getter,
	NULL, // &addr_getter
};
*/

// length of null-terminated array of pointers
int len_narr(void* arr[]){
	int i; for(i=0; *arr++; i++) ;
	return(i);
}

// supports dynamic allocation of shared variable objects
// can be used in place of static allocation by monitor_vars.h
sh_var_val*
alloc_sh_var(sh_var_type vartype){
	switch(vartype){
		case svt_Byte: break;
		case svt_String: break;
		case svt_Boolean:
			return( calloc(1,sizeof(bool)) );
		case svt_Integer: break;
			return( calloc(1,sizeof(int)) );
		case svt_Float: break;
			return( calloc(1,sizeof(float)) );
		case svt_Char: break;
		case svt_Symbol: break;
		case svt_Address: break;
		default: break;
	}
	return NULL;
}

//typedef sh_var_val (*get_sh_var_by_name)(char *);
//get_sh_var_by_name
void va_getter(char *vn){
	/*
	int nsv = monitor_interface.mi_num_shared_vars;
	shared_var_attributes *sva = monitor_interface.mi_shared_vars;
	shared_var_attributes *va = lookup_var(vn,monitor_interface.mi_shared_vars,nsv);
	
	sh_var_type type = sva[i].va_type;
	switch(type){
		case svt_Boolean:
		case svt_Integer:
		case svt_Float:
		default:
	}
	return sva->va_addr;
	*/
	vn++;
}

// conversions from string to type
bool str2Boolean(char *s){
	// where is stricmp (strcmpi)?
	if( strcmp(s, "true") == 0 ) return true;
	if( strcmp(s, "false") == 0 ) return false;
	if( strcmp(s, "TRUE") == 0 ) return true;
	if( strcmp(s, "FALSE") == 0 ) return false;
	return false;
}
int str2Integer(char *s){
	char *z;
	return (int) strtol(s, &z, 10);
}
float str2Float(char *s){
	char *z;
	return strtof(s, &z);
}

char *numstrings[SH_VAR_SZ];
char **nextnumstring = numstrings;

void freenumstrings(){
	for( char **s = numstrings; s<nextnumstring; s++ ) free(*s);
	nextnumstring = numstrings;
}

// returned string must NOT be freed
char *Boolean2str( bool b ){
	static char truestr[] = "true";
	static char falsestr[] = "false";
	if( b ) return truestr; else return falsestr;
}

// must free() returned string after consumed
char *Integer2str( int i ){
	char tmpstr[STRINGS_SZ]; char *sp;
	sprintf(tmpstr, "%d", i);
	int l = strlen(tmpstr);
	sp = (char*) calloc(l+1, sizeof(char));
	strncpy(sp, tmpstr, l);
	return sp;
}

// must free() returned string after consumed
char *Float2str( float f ){
	char tmpstr[STRINGS_SZ]; char *sp;
	gcvt(f,STRINGS_SZ-10,tmpstr);
	int l = strlen(tmpstr);
	sp = (char*) calloc(l+1, sizeof(char));
	strncpy(sp, tmpstr, l);
	return sp;
}

char *sh_var_val2str(sh_var_val v, a_arg_kind k, sh_var_type t){
	bool B; int I; float F; void *A; static char s[STRINGS_SZ];
	/*
	switch(t){
		case svt_Boolean:
			B = v.sv_boolval; break;
		case svt_Integer:
			I = v.sv_intval; break;
		case svt_Float:
			F = v.sv_floatval; break;
		case svt_Address:
			A = v.sv_addrval; // FIX THIS IF UNCOMMENTING
			break;
		default: break;
	}
	*/
	if( k==akind_variable && t!=svt_UNDEFINED){
		sprintf(s,"%lu", (unsigned long) v.sv_addrval);
		return s;
	}else return "$null$";
}

void
assign_sh_var_val(char *str, sh_var_type type, sh_var_val *val){
	switch(type){
		case svt_Byte: break;
		case svt_String: break;
		case svt_Boolean:
			val->sv_boolval = str2Boolean(str); return;
		case svt_Integer: break;
			val->sv_intval = str2Integer(str); return;
		case svt_Float: break;
			val->sv_floatval = str2Float(str); return;
		case svt_Char: break;
		case svt_Symbol: break;
		case svt_Address: break;
		default: break;
	}
}

// assign to variable by address
// assign with or without triggering responder
void assign_Boolean(char *val, bool *addr, bool trigger_var){
	bool value;
	if( strcmp(val,"undefined") == 0 ) value = false;
	else value = str2Boolean(val);
	if( trigger_var ) bool_setter_by_addr(addr, value);
	else *addr = value;
}

void assign_Integer(char *val, int *addr, bool trigger_var){
	int value;
	if( strcmp(val,"undefined") == 0 ) value = INT_MIN;
	else value = str2Integer(val);
	if( trigger_var ) int_setter_by_addr(addr, value);
	else *addr = value;
}

void assign_Float(char *val, float *addr, bool trigger_var){
	float value;
	if( strcmp(val,"undefined") == 0 ) value = NAN;
	else value = str2Float(val);
	if( trigger_var ) float_setter_by_addr(addr, value);
	else *addr = value;
}		

//------------------
// as above but receiving attr pointer instead of addr
void assigna_Boolean( char *val, shared_var_attr_t *sva ){
	bool new;
	if( strcmp(val,"undefined") == 0 ) new = false;
	else new = str2Boolean(val);
	bool old = *(bool*)sva->va_addr;
	*(bool*)sva->va_addr = new;
	if( sva->va_trigger && ms_global_trigger_enable )
		ms_step_addr(sva->va_addr,svt_Boolean,&old,&new);
}

void assigna_Integer(char *val, shared_var_attr_t *sva ){
	int new;
	if( strcmp(val,"undefined") == 0 ) new = INT_MIN;
	else new = str2Integer(val);
	int old = *(int*)sva->va_addr;
	*(int*)sva->va_addr = new;
	if( sva->va_trigger && ms_global_trigger_enable )
		ms_step_addr(sva->va_addr,svt_Integer,&old,&new);
}

void assigna_Float(char *val, shared_var_attr_t *sva ){
	float new;
	if( strcmp(val,"undefined") == 0 ) new = NAN;
	else new = str2Float(val);
	float old = *(float*)sva->va_addr;
	*(float*)sva->va_addr = new;
	if( sva->va_trigger && ms_global_trigger_enable )
		ms_step_addr(sva->va_addr,svt_Float,&old,&new);
}
//------------------

// string returned MUST be freed by caller
// unless it's Boolean
char *value_str(sh_var_type type, void *addr){
	//static char val_str[STRINGS_SZ];
	char *sp;
	switch(type){
		case svt_Byte: break;
		case svt_String: break;
		case svt_Boolean:
			return Boolean2str(*(bool*)addr);
		case svt_Integer:
			if( *(int*)addr == INT_MIN ) sp = "undefined";
			else sp=Integer2str(*(int*)addr);
			return sp;
			// strcpy(val_str,sp); free(sp);
			// return val_str;
		case svt_Float:
			if( isnan( *(float*)addr ) ) sp = "undefined";
			else sp=Float2str(*(float*)addr);
			return sp;
			// strcpy(val_str,sp); free(sp);
			// return val_str;
		case svt_Char: break;
		case svt_Symbol: break;
		case svt_Address: break;
		default: break;
	}
	return NULL;
}

void test_value_str(){
    bool b=true; int i=3; float f = 2.4;
    printf("%s\n", value_str(svt_Boolean, &b)); fflush(stdout);
    printf("%s\n", value_str(svt_Integer, &i)); fflush(stdout);
    printf("%s\n", value_str(svt_Float, &f)); fflush(stdout);
}

// LOOKUP
shared_var_attr_t*
lookup_var_attr_by_name(char *name){
    for(shared_var_attr_t *sv=shared_var_attrs;
			sv < shared_var_attrs+N_SHARED_VARS; sv++)
        if(strcmp(name,sv->va_name) == 0) return sv;
    return NULL;
}

void*
sv_addr_by_name(char *name){
    shared_var_attr_t *sv = lookup_var_attr_by_name(name);
    if( sv != NULL ) return sv->va_addr;
    else return NULL;
}

// Integer setters
//   int/bool/float_setter_by addr called directly by set_<var> defined in xxxxx_varx.h
//
void int_step_by_addr(void *addr, int old, int new){
    ms_step_addr(addr,svt_Integer,(void*)&old,(void*)&new);
}

int int_asetter( shared_var_attr_t *sva, int new ){
    int old = *(int*)sva->va_addr;
    if(old != new || ms_global_report_all){
        *(int*)sva->va_addr = new;
        int_step_by_addr(sva->va_addr,old,new);
    }
    return old;
}

int int_setter_by_addr(int *addr, int new){
    int old = *addr;
    if(old != new || ms_global_report_all){
        *addr = new;
        int_step_by_addr(addr,old,new);
    }
    return old;
}

int int_setter_by_idx(int idx, int new){
    return int_setter_by_addr(shared_var_attrs[idx].va_addr, new);
}

int int_setter_by_name(char *name, int new){
    return int_setter_by_addr(sv_addr_by_name(name), new);
}

// Boolean setters
void bool_step_by_addr(void *addr, bool old, bool new){
    ms_step_addr(addr,svt_Boolean,(void*)&old,(void*)&new);
}

bool bool_asetter( shared_var_attr_t *sva, bool new ){
    bool old = *(bool*)sva->va_addr;
    if(old != new || ms_global_report_all){
        *(bool*)sva->va_addr = new;
        bool_step_by_addr(sva->va_addr,old,new);
    }
    return old;
}

bool bool_setter_by_addr(bool *addr, bool new){
    bool old = *addr;
    if(old != new || ms_global_report_all){
        *addr = new;
        bool_step_by_addr(addr,old,new);
    }
    return old;
}

bool bool_setter_by_idx(int idx, bool new){
    return bool_setter_by_addr(shared_var_attrs[idx].va_addr, new);
}

bool bool_setter_by_name(char *name, bool new){
    return bool_setter_by_addr(sv_addr_by_name(name), new);
}

// Float setters
void float_step_by_addr(void *addr, float  old, float  new){
    ms_step_addr(addr,svt_Float,(void*)&old,(void*)&new);
}

float float_asetter( shared_var_attr_t *sva, float new ){
    float old = *(float*)sva->va_addr;
    if(old != new || ms_global_report_all){
        *(float*)sva->va_addr = new;
        float_step_by_addr(sva->va_addr,old,new);
    }
    return old;
}

float  float_setter_by_addr(float  *addr, float  new){
    float  old = *addr;
    if(old != new || ms_global_report_all){
        *addr = new;
        float_step_by_addr(addr,old,new);
    }
    return old;
}

float  float_setter_by_idx(int idx, float  new){
    return float_setter_by_addr(shared_var_attrs[idx].va_addr, new);
}

float  float_setter_by_name(char *name, float  new){
    return float_setter_by_addr(sv_addr_by_name(name), new);
}



void test_var_getr_setr(){
	dump_defined_vars();
	int new_n = 5; int old_n; int current_n;
	set_n(&new_n,&old_n);
	get_n(&current_n);
	printf("new_n=%d, old_n=%d, current_n=%d\n",new_n,old_n,current_n);
	dump_defined_vars();

	bool new_p = false; bool old_p; bool current_p;
	set_p(&new_p,&old_p);
	get_p(&current_p);
	printf("new_p=%d, old_p=%d, current_p=%d\n",new_p,old_p,current_p);
	dump_defined_vars();

	int new_o = 7; int old_o; int current_o;
	set_o(&new_o,&old_o);
	get_o(&current_o);
	printf("new_o=%d, old_o=%d, current_o=%d\n",new_o,old_o,current_o);
	dump_defined_vars();

	float new_r = 3.14159; float old_r; float current_r;
	set_r(&new_r,&old_r);
	get_r(&current_r);
	printf("new_r=%f, old_r=%f, current_r=%f\n",new_r,old_r,current_r);
	dump_defined_vars();
}
