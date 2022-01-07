#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define JSMN_PARENT_LINKS
#include "jsmn.h"

/* following eval_where is not used because the monitor_atom_eval parameter is now a string */
typedef enum {  
    unset_eval, ms_eval, mep_eval
} eval_where;

static char *eval_where_names[] = {"unset_eval","ms_eval","mep_eval"};

typedef enum {
    monitor_uninitialized,
    monitor_initialized,
    monitor_started,
    monitor_stopping
} mstatus;

typedef struct {
    char *name; char *type;
} sh_var_decl;

typedef struct {
    char *aid; char *aex;
} monitor_atom;

typedef struct {
    sh_var_name name; sh_var_val init_value;
} sh_var_init;

char *mstatus_string(mstatus ms){
    switch(ms){
        case monitor_uninitialized: return("monitor_uninitialized");
        case monitor_initialized: return("monitor_initialized");
        case monitor_started: return("monitor_started");
        case monitor_stopping: return("monitor_stopping");
    }
}

/*
% MS CONFIGURATION
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
    sh_var_init *shared_var_inits;
} ms_configuration_vector;

/*
 * JSON representation of configuration vector
 */

static char *cv_element_names[] = { // TODO check
  "monitor_id",
  "shared_var_decls",
  "observable_vars",
  "model_vars",
  "property_vars",
  "reportable_vars",
  "trigger_vars",
  "monitor_atoms",
  "monitor_atom_eval",
  "shared_var_inits",
  "name",
  "type",
  "value",
  "aid",
  "aex"
};

#define JSON_STRING_SIZE 2048
#define JSMN_TOKEN_ARRAY_SIZE 1024

// JSON string to be read in from file, stream or other argument
static char JSON_STRING[JSON_STRING_SIZE] = "";
static char *JSON_STRING1 = /* a test string TODO - no longer a valid JSON CV */
    "{\n  \"monitor_id\":\"mid_00001\",\n  \"shared_vars\": [\"aa\", \"bbb\", \"cccc\" ],\n \"monitor_atoms\": [ {\"aid\":\"a1\", \"aex\":\"eq(x,2)\"}, {\"aid\":\"a2\", \"aex\":\"lt(x,2)\"} ],\n  \"model_vars\": [\"s\", \"t\" ],\n }";

// compare a string s to a token tok appearing in json string
static int
jsoneq(const char *json, jsmntok_t *tok, const char *s) {
  if (tok->type == JSMN_STRING && (int)strlen(s) == tok->end - tok->start &&
      strncmp(json + tok->start, s, tok->end - tok->start) == 0)
    return 0;
  return -1;
}

// test whether token corresponds to one of the defined cv element keys
int
lookup_cv_element(jsmntok_t *t) {
  for(int k=0; k<sizeof cv_element_names; k++)
    if( jsoneq(JSON_STRING, t, cv_element_names[k]) == 0 ) 
      return k;
  return -1;
}

// size of a token and all of its descendants
int
tok_cnt(jsmntok_t t[], int i){
  int sz = 0;
  switch(t[i].type){
    case JSMN_STRING:
      if(t[i].size) sz += tok_cnt(t,i+1);
    case JSMN_PRIMITIVE: break;
    case JSMN_OBJECT:
    case JSMN_ARRAY:
      for(int j=0; j<t[i].size; j++)
        sz += tok_cnt(t,i+sz+1);
      break;
    case JSMN_UNDEFINED: break;
  }
  return(sz+1);
}

int
tok_cnt_rel(jsmntok_t *tp){
  int sz = 0;
  switch(tp->type){
    case JSMN_STRING:
      if(tp->size) sz += tok_cnt_rel(tp+1);
    case JSMN_PRIMITIVE: break;
    case JSMN_OBJECT:
    case JSMN_ARRAY:
      for(int j=0; j<tp->size; j++)
        sz += tok_cnt_rel(tp+sz+1);
      break;
    case JSMN_UNDEFINED: break;
  }
  return(sz+1);
}

// display all tokens in a range of indices
void
range(jsmntok_t t[], int low, int high){
    printf("\nrange %d-%d\n",low,high-1);
#ifdef JSMN_PARENT_LINKS
    printf("T# P#(ty)[sz] item tok_cnt\n");
    for(int i = low; i<high; i++){
        jsmntok_t *g = &t[i]; int r_cnt = tok_cnt(t,i);
        printf("%d->%d(%d)[%d] %.*s\ttok_cnt(%d)==%d\n", i, g->parent,
#else
    printf("T#(ty)[sz]\n");
    for(int i = low; i<high; i++){
        jsmntok_t *g = &t[i]; int r_cnt = tok_cnt(t,i);
        printf("%d(%d)[%d] %.*s\ttok_cnt(%d)==%d\n", i,
#endif
        g->type, g->size, g->end - g->start, JSON_STRING + g->start, i, r_cnt);
    }
}

// storage of converted config data from JSON
#define CHARS_SZ 4096
#define STRINGS_SZ 1024
char chars[CHARS_SZ]; // character storage
char *nextchar = chars; // next character to be allocated
char *strings[STRINGS_SZ]; // null-terminated string storage
char **nextstring = strings; // next string pointer to be allocated

#define SH_VAR_SZ 20
#define MON_ATOM_SZ 20
sh_var_decl sh_var_decls[SH_VAR_SZ];
sh_var_decl *next_sh_var_decl = sh_var_decls;
sh_var_init sh_var_inits[SH_VAR_SZ];
sh_var_init *next_sh_var_init = sh_var_inits;
monitor_atom monitor_atoms[MON_ATOM_SZ];
monitor_atom *next_monitor_atom = monitor_atoms;

// create a null-terminated string from characters in a json string
char *
sto_chars(int start, int end, char js[]){
    char *newchars = nextchar; char *sp = nextchar;
    for(int i=start; i<end; i++){
        *sp++ = js[i];
    }
    *sp++ = '\0';
    nextchar = sp;
    return newchars;
}

// add a null-terminated string to the string storage
char **
sto_string(char *s){
    char **newstring = nextstring++;
    *newstring = s;
    *nextstring = NULL;
    return newstring;
}

// create an array of strings from a JSMN_ARRAY token
char **
sto_array_of_str(jsmntok_t *tp){
    char **sa=nextstring;
    for(int i=1;i<=tp->size;i++){
        jsmntok_t *elt=tp+i;
        sto_string(sto_chars(elt->start,elt->end,JSON_STRING));
    }
    sto_string(NULL);
    return sa;
}

// print an array of strings pointed to by the arg (like an argv)
void
prt_array(char **array){ printf("\n");
    for(char **p=array; *p!=NULL; p++)
        printf("%s\n",*p);
}

int // return index of member or -1
search_array(char *s, char **array){
    char **ap = array;
    if( array == NULL ) return -1;
    for( int i=0; *ap; i++)
        if( strcmp(s,*ap++) == 0 ) return(i);
    return -1;
}

// create an object from a JSMN_OBJECT token
char ** // TODO (derive from sto_array as sequence of key:value pairs)
sto_object(jsmntok_t *tp){
    char **sa=nextstring;
    for(int i=1;i<=tp->size;i++){
        jsmntok_t *elt=tp+i;
        //sto_string(sto_chars(elt->start,elt->end,JSON_STRING));
        //sto_key_value(elt);
    }
    sto_string(NULL);
    return sa;
}

// create an array of objects from a JSMN_ARRAY token - TODO
char **
sto_array_of_obj(jsmntok_t *tp){
    char **sa=nextstring;
    for(int i=1;i<=tp->size;i++){
        jsmntok_t *elt=tp+i;
        //sto_string(sto_chars(elt->start,elt->end,JSON_STRING));
        sto_object(elt);
    }
    sto_string(NULL);
    return sa;
}

int
find_key_tok(char *key, jsmntok_t t[], int i, char *js){
    int key_len=strlen(key); int sz = 0;
    if(t[i].type==JSMN_OBJECT)
        for(int j=0; j<t[i].size; j++){
            int eltidx = i + sz + 1;
            jsmntok_t *keytok = t+eltidx; int keylen = keytok->end - keytok->start;
            if(keytok->type==JSMN_STRING && keylen==key_len && strncmp(key,&js[keytok->start],keylen)==0)
                return eltidx;
            sz += tok_cnt(t,i+sz+1);
        }
    return -1;
}

char ** // TODO (derive from get_arr_attr)
get_obj_attr(char *attr, jsmntok_t t[], int i, char *js){
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        return sto_object( &t[key_tok_idx+1] );
    else return NULL;
}

char ** // TODO (derive from get_arr_attr)
get_arr_of_obj_attr(char *attr, jsmntok_t t[], int i, char *js){
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        return sto_array_of_obj( &t[key_tok_idx+1] );
    else return NULL;
}

//-----------

char *
get_str_attr(char *attr, jsmntok_t t[], int i, char *js){
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) == -1 ) return NULL;
    jsmntok_t *tap = &t[key_tok_idx+1];
    if( tap->type==JSMN_STRING || tap->type==JSMN_PRIMITIVE )
        return sto_chars( tap->start, tap->end, js );
    else return "";
}
/*
char **
sto_array_of_str(jsmntok_t *tp){
    char **sa=nextstring;
    for(int i=1;i<=tp->size;i++){
        jsmntok_t *elt=tp+i;
        sto_string(sto_chars(elt->start,elt->end,JSON_STRING));
    }
    sto_string(NULL);
    return sa;
}
*/
char **
get_arr_of_str_attr(char *attr, jsmntok_t t[], int i, char *js){
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        return sto_array_of_str( &t[key_tok_idx+1] );
    else return NULL;
}

void
get_key_val_pair( jsmntok_t *tp, char **key, char **val, char *js ){
    if( tp->type == JSMN_STRING ){
        *key = sto_chars( tp->start, tp->end, js );
        *val = sto_chars( (tp+1)->start, (tp+1)->end, js );
    }
}

void
sto_decl( jsmntok_t *tp, char *js ){
    char *key, *val;
    get_key_val_pair(tp+1,&key,&val,js);
    next_sh_var_decl->name = val;
    get_key_val_pair(tp+3,&key,&val,js);
    next_sh_var_decl->type = val;
    ++next_sh_var_decl;
}

sh_var_decl *
sto_array_of_decl( jsmntok_t t[], int i, char *js ){
    int sz = 0; jsmntok_t *tp = &t[i];
    for(int j=1; j<=tp->size; j++){
        sto_decl(tp+sz+1, js);
        sz += tok_cnt(t, i+sz+1);
    }
    return sh_var_decls;
}

sh_var_decl *
get_arr_of_decl_attr(char *attr, jsmntok_t t[], int i, char *js){
    // each decl is an object, name: type:
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        sto_array_of_decl( t, key_tok_idx+1, js );
    return sh_var_decls;
}

sh_var_init *
get_arr_of_init_attr(char *attr, jsmntok_t t[], int i, char *js){
    // each init is an object, name: value:
    //sh_var_init *new_sh_var_init = next_sh_var_init++;
    //new_sh_var_init->name =
    //new_sh_var_init->init_value = 
    //return new_sh_var_init;
    return(NULL);
}

monitor_atom *
get_arr_of_ma_attr(char *attr, jsmntok_t t[], int i, char *js){
    // each monitor atom is an object, aid: aex:
    //monitor_atom *new_monitor_atom = next_monitor_atom++;
    //new_monitor_atom->aid =
    //new_monitor_atom->aex = 
    //return new_monitor_atom;
    return(NULL);
}

/* get configuration vector information from the external JSON */
int
initialize_ms_configuration(ms_configuration_vector *cv){
    jsmn_parser p;
    jsmntok_t t[JSMN_TOKEN_ARRAY_SIZE];

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
        VERBOSE(2){range(t,0,r); fflush(stdout);}
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

// build the shared variable access table
// setters and getters can be looked up by name or index same as var list
// value can be directy accessed using sv_addr pointer member
// shared_var_attributesp build_sh_var_attributes(sh_var_names svns){
//build_sh_var_attributes(monitor_interface_t *mip){
shared_var_attributesp
build_sh_var_attributes(ms_configuration_vector *cv){
    sh_var_names svns = cv->observable_vars;
	shared_var_attributesp shared_vars = (shared_var_attributesp)
		calloc(n_shared_vars, sizeof(shared_var_attributes));
	shared_var_attributesp ap = shared_vars;
	for(int i=0; *svns; i++, ap++, svns++){
		ap->sv_name = *svns;
		ap->sv_type = shared_var_types[i];
		ap->sv_addr = shared_var_addresses[i];
		ap->sv_trigger = 
            search_array(ap->sv_name,cv->trigger_vars) >= 0;
		ap->sv_report =
            search_array(ap->sv_name,cv->reportable_vars) >= 0;
		ap->sv_property_eval =
            search_array(ap->sv_name,cv->property_vars) >= 0;
		ap->sv_getter = getter_by_type[ap->sv_type];
		ap->sv_setter = setter_by_type[ap->sv_type];
	}
	return(shared_vars);
}

/* dump functions to visualize configuration data */

void dump_strings(char *what, char *s[]){
    printf("%s:\n", what);
    for( char **p=s; *p!=NULL; p++ ){
        printf("  %s",*p);
    }
    printf("\n");
}

void dump_matoms(){
    monitor_atom **p;
    printf("%s:\n", "monitor_atoms");
    for( monitor_atom *p = monitor_atoms; p < next_monitor_atom; p++)
        printf("  %s:%s\n", p->aid, p->aex);
}

void dump_sv_inits(sh_var_init *svis[]){ //TODO - change as above (no double indirection)
    sh_var_init **p;
    printf("%s:\n","shared var inits");
    for( p = svis; *p != NULL; p++ ){
        printf("name=%s, value=%d\n", (*p)->name,
            (*p)->init_value.sv_intval); // assume int for now
    }
}

void dump_sv_decls(){
    printf("sv_decls:\n");
    for(sh_var_decl *dp = sh_var_decls; dp < next_sh_var_decl; dp++)
        printf("  %s:%s\n",dp->name,dp->type);
}

void dump_ms_cv(ms_configuration_vector *cv){
    VERBOSE(1) printf("dump ms cv:\n");
    printf(" monitor_id: %s\n", cv->monitor_id);
    dump_sv_decls();
    dump_strings( " observable_vars", cv->observable_vars );
    dump_strings( " model_vars", cv->model_vars );
    dump_strings( " property_vars", cv->property_vars );
    dump_strings( " reportable_vars", cv->reportable_vars );
    dump_strings( " trigger_vars", cv->trigger_vars );
    //dump_matoms();
    printf(" monitor_atom_eval: %s\n", cv->monitor_atom_eval);
    //dump_sv_inits( cv->shared_var_inits );
    fflush(stdout);
}

extern void dump_strings(char *what, char *s[]);

void dump_shared_var_attributes(shared_var_attributesp svas, int nvars){
	printf("shared variable attributes:\n");
	for(int i=0; i<nvars; i++, svas++){
		printf(" %02d\tname=%s, type=%d, addr=%lu, trig=%d, report=%d, prop=%d, getter=%lu, setter=%lu\n",
			i, svas->sv_name, svas->sv_type,
			(unsigned long)svas->sv_addr,
          svas->sv_trigger, svas->sv_report,
          svas->sv_property_eval,
			(unsigned long)svas->sv_getter,
			(unsigned long)svas->sv_setter
		);
	}
}