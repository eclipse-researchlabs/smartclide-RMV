// NOTE: MUST EDIT THE MONITOR_VARS_H_FILENAME FOR A DIFFERENT MONITOR
// THE MONITOR ID IS IN THIE FILE AND THE MONITOR CONFIGURATION FILE
// NAME IS DERIVED FROM IT.
#define RUNTIME_MONITORS_PATH "../../../RUNTIME/MONITORS/"
#define MONITOR_VARS_H_FILENAME "../../../RUNTIME/MONITORS/monid_00004_vars.h"

//#define MS_TEST
#define VERBOSITY -1  // set to -1 for silence
#define VERBOSE(L) if(VERBOSITY >= L)
#define VERBOSE_MSG(L,M) if(VERBOSITY >= L){printf(M);fflush(stdout);}

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>

#define JSMN_PARENT_LINKS
#include "jsmn.h"

// TODO - check these limits (only) during configuration initialization
#define JSON_STRING_SZ 2048
#define JSMN_TOKEN_ARRAY_SZ 1024
#define CONFIG_FILENAME_SZ 128
#define CONFIG_FILENAME_SUFFIX "_conf.json"
#define CHARS_SZ 4096
#define STRINGS_SZ 1024
#define RESPONSE_BUF_SZ 2048
#define SH_VAR_SZ 20
#define MON_ATOM_SZ 20
#define BEHAVIOR_SEQ_SZ 50 // max length of a behavior sequence
#define ATOM_OP_MAX_LEN 20
#define ATOM_ARG_MAX_LEN 50
#define SESSION_ID_SZ 128
#define SIMULATED_MEP false   // set false to use real MEP
#define EPP_TOKEN "epp_token"
#define MEP_TOKEN "mep_token"
#define RMV_TOKEN "rmv_token"

typedef enum mstatus {
    monitor_uninitialized,
    monitor_initialized,
    monitor_started,
    monitor_stopped,
    test_success
} mstatus;

// TODO - make this an array
char *mstatus_string(mstatus ms){
    switch(ms){
        case monitor_uninitialized: return("monitor_uninitialized");
        case monitor_initialized: return("monitor_initialized");
        case monitor_started: return("monitor_started");
        case monitor_stopped: return("monitor_stopped");
        case test_success: return("test_success");
    }
}

typedef char **sh_var_names; // pointer to (null-term) array of names

typedef union {
	bool	sv_boolval;
	int		sv_intval;
	float	sv_floatval;
	char	sv_charval;
	void*	sv_addrval;
} sh_var_val;

typedef enum sh_var_type {
	svt_UNDEFINED,
	svt_VAR,
	svt_Boolean,
	svt_Integer,
	svt_Float,
	svt_Address,
	svt_Byte,       // unused
	svt_String,     // unused
	svt_Char,       // unused
	svt_Symbol      // unused
} sh_var_type;

static char* sv_type_names[] = {"UNDEFINED","VAR","Boolean","Integer",
				"Float","Address","Byte","String","Char","Symbol"};

// sh_var_name_value struct is
// used to stage sh var initializations specified in the JSON
// and variable value reports made for MS heartbeat messages.
typedef struct sh_var_name_value {
    char *vnv_name;
    char *vnv_value;
    sh_var_type vnv_type;
} sh_var_name_value;

typedef void(*getr_t)(void*);
typedef void(*setr_t)(void*,void*);

typedef struct {
	char *va_name;
	sh_var_type va_type;
	void* va_addr;
	bool va_trigger; // trigger responder() when var is set
	bool va_report; // include var in MS heartbeat reports
	bool va_property_eval; // var used in property eval
	getr_t va_getter;
	setr_t va_setter;
} shared_var_attr_t;

// string repr of declarations appering in JSON configuration vector
typedef struct {
    char *name; char *type;
} sh_var_decl;

typedef enum atom_op {
    var, not, bool_f, bool_t, eq, ne, gt, lt, ge, le, badop
} atom_op;

static char *atom_op_names[] = {
    "__var__", "not", "false", "true", // synonyms too much trouble
    "eq", "ne", "gt", "lt", "ge", "le", /*"neq", "geq", "leq",*/ "badop"
};

static atom_op atom_op_map[] = {
    var, not, bool_f, bool_t,
    eq, ne, gt, lt, ge, le, /* ne, ge, le,*/ badop
};

typedef enum ext_atom_op { inval_op,
    eq_B_B, eq_I_I, eq_F_F, eq_I_F,
    ne_B_B, ne_I_I, ne_F_F, ne_I_F,
            lt_I_I, lt_F_F, lt_I_F,
                            gt_I_F,
            le_I_I, le_F_F, le_I_F,
                            ge_I_F,
    var_B, var_I,
    not_B, not_I,
    Bfalse, Btrue
 } ext_atom_op;

static char *ext_atom_op_names[] = { "inval_op",
    "eq_B_B", "eq_I_I", "eq_F_F", "eq_I_F",
    "ne_B_B", "ne_I_I", "ne_F_F", "ne_I_F",
              "lt_I_I", "lt_F_F", "lt_I_F",
                                  "gt_I_F",
              "le_I_I", "le_F_F", "le_I_F",
                                  "ge_I_F",
    "var_B", "var_I",
    "not_B", "not_I",
    "Bfalse", "Btrue"
};

typedef enum a_arg_kind {
    akind_variable, akind_constant, akind_unused
} a_arg_kind;

static char *akind_names[] = {
    "variable", "constant", "unused"
};

// includes string representation of monitor atoms as appearing
// in JSON configuration vector and compiled representation
typedef struct monitor_atom { // NEW
    char        *ma_aid;
    char        *ma_aex;       // e.g.: "eq(n,2)" ==>

    atom_op     ma_op;          // literal op, e.g. eq
    ext_atom_op ma_ext_op;      // op with explicit types, e.g. eq_I_I

    a_arg_kind  ma_arg1_knd; // e.g. akind_var
    sh_var_type ma_arg1_typ;    // svt_Integer
    sh_var_val  ma_arg1_val;    // (sv_addrval)

    a_arg_kind  ma_arg2_knd; // e.g. akind_const
    sh_var_type ma_arg2_typ;    // svt_Integer
    sh_var_val  ma_arg2_val;    // (sv_intval)
} monitor_atom;

/*
% MS CONFIGURATION STRUCTURE
%   A JSON version of this structure is generated by Monitor Creation to accompany
%   the generated monitor. It is initialized by initialize_ms_configuration
%
%   monitor_id - established when the Monitor Sensor is defined
%   shared_var_decl - declarations of all variables shared by SUS and MS, a superset of union of other vars
%   observable_vars - SUS variables used in model, properties or reportable (may be same as shared)
%   model_vars (subset of shared_vars) - potentially observable SUS variables referenced in the model
%   property_vars - observable vars referenced in property expressions
%   reportable_vars - subset of monitor_observables to be reported in every heartbeat message
%   trigger_vars - variables for which setter should trigger responder
%   monitor_atoms - list of atoms {aid: , aex: } to be evaluated for properties
%   monitor_atom_eval - where atoms are evaluated (unset_eval, no_eval, ms_eval or mep_eval)
%   shared_vars_init - optional initializations list of {name: , value: } without triggers
%   op_seq - sequence of assignments executed on request with triggers
%   timer - period of timer-triggered responder
%   rmvhost - URL of RMV server
%   rmvport - port on rmvhost on which RMV server is listening
*/
typedef struct ms_configuration_vector { // TODO - compare w/ms_pl
    char *monitor_id;
    sh_var_decl *shared_var_decl;
    char **observable_vars;
    char **model_vars;
    char **property_vars;
    char **reportable_vars;
    char **trigger_vars;
    monitor_atom *monitor_atoms;
    int n_monitor_atoms;
    char *monitor_atom_eval;
    sh_var_name_value *shared_var_init;
    sh_var_name_value *op_seq;
    float timer;
    char *rmvhost;
    int rmvport;
} ms_configuration_vector;

static char *cv_element_names[] = {
  "monitor_id",
  "shared_var_decl",
  "observable_vars",
  "model_vars",
  "property_vars",
  "reportable_vars",
  "trigger_vars",
  "monitor_atoms",
  "n_monitor_atoms",
  "monitor_atom_eval",
  "shared_var_init",
  "name", // TODO fix the following
  "type",
  "value",
  "aid",
  "aex"
};

/*
 * MONITOR INTERFACE STRUCTURE
 */

// info the SUS/MS may need to properly set up and interact with the monitor
typedef struct {
    mstatus mi_mstatus;
    shared_var_attr_t *mi_shared_vars;
    int mi_num_shared_vars;
    char *mi_JSON_cv;
    ms_configuration_vector mi_cv;
    char *mi_sessid;
} monitor_interface_t;

////////////////////////////////////////////////////
// GLOBAL VARIABLES

// allocate and minimally initialize the global monitor_interface instance
monitor_interface_t monitor_interface = {
    /* mi_mstatus */            monitor_uninitialized,
    /* mi_shared_vars */        NULL,
    /* mi_num_shared_vars */    0,
    /* mi_JSON_cv */            "\0",
    /* mi_cv*/ {
        /* monitor id */        "",
        /* shared_var_decl */  NULL,
        /* observable_vars */   NULL,
        /* model_vars */        NULL,
        /* property_vars */     NULL,
        /* reportable_vars */   NULL,
        /* trigger_vars */      NULL,
        /* monitor_atoms */     NULL,
        /* n_monitor_atoms */   0,
        /* monitor_atom_eval */ "unset_eval",
        /* shared_var_init */  NULL,
        /* op_seq */            NULL,
        /* timer */             0,
        /* rmvhost */           NULL,
        /* rmvport */           0
    },
    /* mi_sessid */             ""
};

sh_var_decl sh_var_decls[SH_VAR_SZ];
sh_var_decl *next_sh_var_decl = sh_var_decls;
sh_var_name_value sh_var_name_values[SH_VAR_SZ];
sh_var_name_value *next_sh_var_name_value = sh_var_name_values;
sh_var_name_value behavior_seq[BEHAVIOR_SEQ_SZ];
sh_var_name_value *next_behavior_op = behavior_seq;
// TODO - dynamically allocate just what's needed using the count
// field in the parsed JSON structure; assign ptr in ms_cv;
// iterate over monitor_atom structs using n_monitor_atoms,
monitor_atom monitor_atoms[MON_ATOM_SZ];
monitor_atom *next_monitor_atom = monitor_atoms;
bool ms_global_trigger_enable = false;
int ms_global_report_all = 1; // report all trigger var assigns, even if no val change
char ms_configuration_file[CONFIG_FILENAME_SZ] = "";

// JSON string to be read in from file, stream or other argument
static char JSON_STRING[JSON_STRING_SZ] = "";

// MS to MEP communications
bool ms_mep_comm_is_open = false;
int ms_mep_comm_sock = -1;
//int ms_mep_session_num;
char ms_mep_session_id[SESSION_ID_SZ]; // session identifier as a string

// SUS can register a callback function to handle MEP recovery indicator
void (*sus_recovery_callback)() = NULL;

// forward declaration of dump functions
void dump_sh_vars();
void dump_one_shared_var_attributes(shared_var_attr_t *);
void dump_shared_var_attributes(void *, int);
void dump_sv_inits(char *);
void dump_assigns(char *);
void dump_matoms(char *);
void dump_strings(char*, char**);
void dump_compiled_atom(int, monitor_atom*);
void dump_compiled_atoms();
void dump_parse(char*);

// other forward function declarations
int int_setter_by_addr(int*, int);
int int_setter_by_idx(int, int);
int int_setter_by_name(char*, int);

bool bool_setter_by_addr(bool*, bool);
bool bool_setter_by_idx(int, bool);
bool bool_setter_by_name(char*, bool);

float float_setter_by_addr(float*, float);
float float_setter_by_idx(int, float);
float float_setter_by_name(char*, float);

//////////////////////////////////////////////////////////
// This include file provides service/monitor-specific
// declarations generated by monitor creation.
/////////////////////////////

// #include "monitor_vars.h"
#include MONITOR_VARS_H_FILENAME

// the file name above can be edited manually or automatically
// before compilation for the correct path name of the _vars.h file
//////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
// BEGIN INTERFACE SHIMS
//   These items represent the interface layer
//   They provide either a connection to the MEP interface of the RMV server
//   or stubs that allow the MS to be tested standalone.
//   A preprocessor symbol SIMULATED_MEP should be defined true or false
//
// Contents:
//   URL encoddf for HTTP requests to MEP
//   communication with the MEP
//   stubs for simulation of the MEP
//   

// URL encoder (modified by RJD from a snippet on stack overflow)
// use:
//   url_encoder_rfc_tables_init();
//   url_encode( rfc3986, url, url_encoded);

char rfc3986[256] = {0};
char html5[256] = {0};

void url_encoder_rfc_tables_init(){
    for (int i = 0; i < 256; i++){
        rfc3986[i] = isalnum(i) || i == '~' || i == '-' || i == '.' || i == '_' ? i : 0;
        html5[i] = isalnum(i) || i == '*' || i == '-' || i == '.' || i == '_' ? i : (i == ' ') ? '+' : 0;
    }
}

char *url_encode( char *table, char *s, char *enc){
    int c;
    if( enc == NULL ) return enc;
    *enc = '\0';
    for (int sz=0; *s; s++){
        c = (int)*s;
        if (table[c]) *enc++ = table[c];
        else{ sz = sprintf( enc, "%%%02X", c); enc += sz; }
    }
    return enc;
}
// end URL encode

// MEP communication

#include <unistd.h>
#include <fcntl.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
extern int h_errno;

#include <arpa/inet.h>

void open_MEP_comm(){
    struct hostent *hp;
    struct sockaddr_in addr; int on = 1;
    //char *host = RMV_HOST; in_port_t port = RMV_PORT;
    char *host = monitor_interface.mi_cv.rmvhost;
    in_port_t port = monitor_interface.mi_cv.rmvport;

/*     if( (hp = gethostbyname(host)) == NULL ){
        printf("error: gethostbyname\n"); return;
    }

    bcopy(hp->h_addr, &addr.sin_addr, hp->h_length);
 */
    addr.sin_addr.s_addr = inet_addr(host);
	addr.sin_port = htons(port);
	addr.sin_family = AF_INET;
	ms_mep_comm_sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if( ms_mep_comm_sock == -1 ){
        printf("error: create socket for RMV server comm\n");
        return;
    }

    setsockopt(ms_mep_comm_sock, IPPROTO_TCP, TCP_NODELAY, (const char *)&on, sizeof(int));
    //if(sock == -1){ printf("error: setsockopt\n"); return; }
    if(connect(ms_mep_comm_sock, (struct sockaddr *)&addr, sizeof(struct sockaddr_in)) < 0){
        printf("error: connect\n"); return;
    }
    ms_mep_comm_is_open = true;
    VERBOSE_MSG(2,"MEP communication open\n");
}

void close_MEP_comm(){
    if( ms_mep_comm_is_open == false ) return;
    shutdown(ms_mep_comm_sock, SHUT_RDWR);
    close(ms_mep_comm_sock);
    ms_mep_comm_is_open = false;
    ms_mep_comm_sock = -1;
    VERBOSE_MSG(2,"MEP communication closed\n");
}

int make_report_event_request(char req_buf[], char encode_buf[]){
    // GET /epp/report_event?token=rmv_token&event=ms_event(<url encoded payload>)
    char *req_pieces[] =
        {"GET ","/epp/report_event","?token=",RMV_TOKEN,"&event=ms_event(",encode_buf,")\r\n\r\n",0};

    char *rb = req_buf; *rb = '\0';
    for( char** p=req_pieces; *p; p++ ){
        for( char* ps=*p; (*rb = *ps); rb++, ps++) ;
    }
    return rb - req_buf;
}

int make_monitor_heartbeat_request(char req_buf[], char encode_buf[], char* sid){
// e.g. GET /mep/monitor_heartbeat?token=rmv_token&heartbeat=<url encoded ms message>
    char *req_pieces[] =
        {"GET ","/mep/monitor_heartbeat","?token=",RMV_TOKEN,"&session_id=",sid,
        "&heartbeat=",encode_buf,"\r\n\r\n",0};

    char *rb = req_buf; *rb = '\0';
    for( char** p=req_pieces; *p; p++ ){
        for( char* ps=*p; (*rb = *ps); rb++, ps++) ;
    }
    return rb - req_buf;
}

void send_MEP_comm(char *request_ep, char *mid, char *sid, char *payload, char **Response){
    char req_buf[STRINGS_SZ]; char encode_buf[CHARS_SZ];
    static char resp_buf[RESPONSE_BUF_SZ];

    VERBOSE_MSG(2,"send_MEP_comm\n");
    VERBOSE(2){
        printf("  payload=%s\n",payload);
        fflush(stdout);
    }
    if( payload ) url_encode(rfc3986, payload, encode_buf);
    else encode_buf[0] = '\0';
    VERBOSE(2){printf("encoded payload:\n %s\n",encode_buf); fflush(stdout);}

    // TODO - should request_ep parameter be used?
    //int sz=make_report_event_request(req_buf, encode_buf);
    int sz=make_monitor_heartbeat_request(req_buf, encode_buf, sid);

    open_MEP_comm();
        VERBOSE(2){printf("Heartbeat Request:\n %s\n",req_buf); fflush(stdout);}

        write(ms_mep_comm_sock, req_buf, sz);

        VERBOSE(2){printf("Heartbeat Response: \n"); fflush(stdout);}
        bzero( resp_buf, RESPONSE_BUF_SZ );
        if( read(ms_mep_comm_sock, resp_buf, RESPONSE_BUF_SZ-1) > 0 ){ 
            VERBOSE(2){printf("%s",resp_buf); fflush(stdout);}
            //bzero( resp_buf, STRINGS_SZ );
        }
    close_MEP_comm();

    if( Response != NULL ){
            *Response = resp_buf; // only valid until next send_MEP_comm
    }
}

void send_MEP_monitor_start(char *request_ep,char *mid,char **Response){
    char req_buf[STRINGS_SZ]; char encode_buf[CHARS_SZ];
    static char resp_buf[RESPONSE_BUF_SZ];
    char *req_pieces[] =
        {"GET ",request_ep,"?token=",RMV_TOKEN,"&monitor_id=",mid,"\r\n\r\n",0};

    char *rb = req_buf; *rb = '\0';
    for( char** p=req_pieces; *p; p++ ){
        for( char* ps=*p; (*rb = *ps); rb++, ps++) ;
    }
    int sz = rb - req_buf;
    open_MEP_comm();
        VERBOSE(2){printf("Start Request:\n %s\n",req_buf); fflush(stdout);}

        write(ms_mep_comm_sock, req_buf, sz);

        VERBOSE(2){printf("Start Response: \n"); fflush(stdout);}
        bzero( resp_buf, RESPONSE_BUF_SZ );
        if( read(ms_mep_comm_sock, resp_buf, RESPONSE_BUF_SZ-1) > 0 ){ 
            VERBOSE(2){printf(" %s",resp_buf); fflush(stdout);}
            //bzero( resp_buf, STRINGS_SZ );
        }
    close_MEP_comm();

    if( Response != NULL ){
            *Response = resp_buf; // only valid until next send_MEP_monitor_start
    }
}

void send_MEP_monitor_stop(char *request_ep,char *sid,char **Response){
    char req_buf[STRINGS_SZ]; char encode_buf[CHARS_SZ];
    static char resp_buf[RESPONSE_BUF_SZ];
    char *req_pieces[] =
        {"GET ",request_ep,"?token=",RMV_TOKEN,/*"&monitor_id=",mid,*/"&session_id=",sid,"\r\n\r\n",0};

    char *rb = req_buf; *rb = '\0';
    for( char** p=req_pieces; *p; p++ ){
        for( char* ps=*p; (*rb = *ps); rb++, ps++) ;
    }
    int sz = rb - req_buf;
    open_MEP_comm();
        VERBOSE(2){printf("Stop Request:\n %s\n",req_buf); fflush(stdout);}

        write(ms_mep_comm_sock, req_buf, sz);

        VERBOSE(2){printf("Stop Response: \n");  fflush(stdout);}
        bzero( resp_buf, RESPONSE_BUF_SZ );
        if( read(ms_mep_comm_sock, resp_buf, RESPONSE_BUF_SZ-1) > 0 ){ 
            VERBOSE(2){printf(" %s",resp_buf); fflush(stdout);}
            //bzero( resp_buf, STRINGS_SZ );
        }
    close_MEP_comm();

    if( Response != NULL ){
            *Response = resp_buf; // only valid until next send_MEP_monitor_stop
    }
}

void send_MEP_monitor_test(char *request_ep,char *sid,char **Response){
    char req_buf[STRINGS_SZ]; char encode_buf[CHARS_SZ];
    static char resp_buf[RESPONSE_BUF_SZ];
    char *req_pieces[] =
        {"GET ",request_ep,"?token=",RMV_TOKEN,"&session_id=",sid,"\r\n\r\n",0};

    char *rb = req_buf; *rb = '\0';
    for( char** p=req_pieces; *p; p++ ){
        for( char* ps=*p; (*rb = *ps); rb++, ps++) ;
    }
    int sz = rb - req_buf;
    open_MEP_comm();
        VERBOSE(2){printf("Test Request:\n %s\n",req_buf); fflush(stdout);}

        write(ms_mep_comm_sock, req_buf, sz);

        VERBOSE(2){printf("Test Response: \n"); fflush(stdout);}
        bzero( resp_buf, RESPONSE_BUF_SZ );
        if( read(ms_mep_comm_sock, resp_buf, RESPONSE_BUF_SZ-1) > 0 ){ 
            VERBOSE(2){printf(" %s",resp_buf); fflush(stdout);}
            //bzero( resp_buf, STRINGS_SZ );
        }
    close_MEP_comm();

    if( Response != NULL ){
            *Response = resp_buf; // only valid until next send_MEP_monitor_test
    }
}

void range(jsmntok_t*, int, int, char*);
char *get_str_attr(char*, jsmntok_t*, int, char*);

// generally useful for interpreting RMV API responses
void parse_mep_response(char *Response,
        char **respStatus, char **respMessage, char **respBody){
    char *rp = Response;
    jsmn_parser p; jsmntok_t t[100]; int r;

    //for(char *cp=Response; *cp!='\0'; cp++) printf("%d\n",(int)*cp);
    //fflush(stdout);

    while( *rp != '\0'){
        if( *rp != '{' ) rp++; else break;
    }
    VERBOSE(2){printf("JSON in Response:\n%s\n",rp); fflush(stdout);}
    if(*rp != '\0' && strncmp(rp-4,"\r\n\r\n{",5)==0){
        //printf("found start of JSON\n"); 
        jsmn_init(&p);
        r = jsmn_parse(&p, rp, strlen(rp), t, sizeof(t) / sizeof(t[0]));
        if(r < 0){ // failed to parse JSON
            //printf("Failed to parse JSON: %s\n",
            // r==-1?"NOMEM":r==-2?"INVAL":r==-3?"PART":"Unknown");
        }else{
            //printf("jsmn_parse returned %d tokens\n",r);
            //range(t,1,r,rp); fflush(stdout);
            *respStatus = get_str_attr("respStatus",t,0,rp);
            *respMessage = get_str_attr("respMessage",t,0,rp);
            *respBody = get_str_attr("respBody",t,0,rp);
            VERBOSE(2){
                printf("Response values:\nrespStatus=%s\nrespMessage=%s\nrespBody=%s\n",
                *respStatus, *respMessage, *respBody);fflush(stdout);}
        }
    }else{
        // there is no JSON in the response or not where it belongs!
        VERBOSE(2){printf("did not find JSON\n"); fflush(stdout);}
    }
}

int parse_session(char *s, char *session_id){
    //sprintf(session_id,"blahblahblah");
    char *prefix="session("; // session id may be between single quotes
    int start = strlen(prefix);
    if( strncmp(s,prefix,start)==0 ){
        char *p=s+start;
        if( *p == '\'' ) p++;
        for( ; *p!=')' && *p!='\''; p++) *session_id++ = *p;
        *session_id = '\0';
        return 1;
    }
    return 0;
}

/* MEP shims */
void mep_monitor_start(char *Mid, char **Msessid, mstatus *Mstatus){
    char *Response, *respStatus, *respMessage, *respBody;
    if( SIMULATED_MEP ){
         sprintf(ms_mep_session_id,"%5d",11111);
    }else{
        //send_MEP_comm("/mep/monitor_start",Mid,*Msessid,NULL,&Response);
        send_MEP_monitor_start("/mep/monitor_start",Mid,&Response);
        parse_mep_response(Response,&respStatus,&respMessage,&respBody);
        //char tmp_ms_mep_session_id[100];
        if( strcmp(respStatus,"success") != 0 ||
                parse_session(respBody, ms_mep_session_id) == 0){
            // could not find session in response
            *Mstatus = monitor_uninitialized;
            ms_mep_session_id[0]='\0';
        }else{
            VERBOSE(2){printf("session from response: %s\n", ms_mep_session_id);fflush(stdout);}
        }
    }

    *Msessid = ms_mep_session_id;
    *Mstatus = monitor_started;

    //VERBOSE(1){printf("Monitor session id: %s starting\n", *Msessid); fflush(stdout); }
};

void mep_monitor_stop(char *Msessid, mstatus *Mstatus){
    char *Response, *respStatus, *respMessage, *respBody;

    if( SIMULATED_MEP ){
    }else{
        send_MEP_monitor_stop("/mep/monitor_stop",Msessid,&Response);
        parse_mep_response(Response,&respStatus,&respMessage,&respBody);
        if( strcmp(respStatus,"success") != 0 ){
            VERBOSE(2){printf("unexpected failure of monitor_stop\n");fflush(stdout);}
        }else{
            VERBOSE(2){printf("monitor_stop success\n");fflush(stdout);}
        }
    }

    *Mstatus = monitor_stopped;

    VERBOSE(1){printf("Monitor session id: %s stopped\n", Msessid); fflush(stdout); }
};

void mep_monitor_test(char *Monid, char *Msessid, mstatus *Mstatus){
    char *Response, *respStatus, *respMessage, *respBody;

    if( SIMULATED_MEP ){
    }else{
        send_MEP_monitor_test("/mep/monitor_test",Msessid,&Response);
        parse_mep_response(Response,&respStatus,&respMessage,&respBody);
        if( strcmp(respStatus,"success") != 0 ){
            VERBOSE(2){printf("unexpected failure of monitor_test\n");fflush(stdout);}
        }else{
            VERBOSE(2){printf("monitor_test success\n");fflush(stdout);}
        }
    }

    *Mstatus = test_success;

    //VERBOSE(1){printf("Monitor session id: %s stopped\n", Msessid); fflush(stdout); }
};

// synchronously send heartbeat to rmv_mf_mep and wait for response
// The message is sent via the mepapi.
// For standalone test we call a mep_heartbeat stub
// build the HTTP call to MEP
void mep_heartbeat(char *HB_json, int *Response){
    char *MEP_Reply, *respStatus, *respMessage, *respBody;

    if( SIMULATED_MEP ){
        VERBOSE_MSG(1,"Simulated MEP received heartbeat\n");
        // must simulate some MEP behavior here
        // ...
        *Response = 1;
        VERBOSE(1){printf("response from simulated mep_heartbeat=%d\n",*Response);fflush(stdout);}
        return;
    }

    char* mid = monitor_interface.mi_cv.monitor_id;
    send_MEP_comm("/mep/monitor_heartbeat", mid, ms_mep_session_id, HB_json, &MEP_Reply);
    parse_mep_response(MEP_Reply,&respStatus,&respMessage,&respBody);
    VERBOSE(2){printf("Parsed response: respStatus=%s respMessage=%s respBody=%s\n",
            respStatus, respMessage, respBody);fflush(stdout);}
    if( strcmp(respStatus,"success") != 0 ){
        VERBOSE(2){printf("unexpected failure of monitor_heartbeat\n");fflush(stdout);}
        //[acknowledged,session(Sid),basis(no_eval,Reportables)]
    }
    *Response = 1; // TODO - get proper response out of MEP_Reply
    //VERBOSE(1){printf("reply from mep_heartbeat:\n%s\n",MEP_Reply); fflush(stdout);}
}

void send_event(char *event, char **Response){
    char req_buf[STRINGS_SZ]; char encode_buf[CHARS_SZ];
    static char resp_buf[CHARS_SZ];
    monitor_interface_t *mi = &monitor_interface;
    char *MEP_Reply;
    char *mid = mi->mi_cv.monitor_id;
    char *sid = mi->mi_sessid;
    int sz;
    resp_buf[0] = '\0'; // bzero( resp_buf, RESPONSE_BUF_SZ );
    if( Response != NULL ){
            *Response = resp_buf; // upon return only valid until next send_MEP_comm
    }

    char *req_pieces[] =
        {"GET ","/epp/report_event","?token=",RMV_TOKEN,"&event=",event,"\r\n",
        "HTTP/1.1\r\n",
    //    "Host: localhost\r\n",
        "Connection: keep-alive\r\n",
    //    "Content-type: application/x-www-form-urlencoded\r\n",
    //    "Content-length: 0\r\n",
        "\r\n",
        0};

    char *rb = req_buf; *rb = '\0';
    for( char** p=req_pieces; *p; p++ ){
        for( char* ps=*p; (*rb = *ps); rb++, ps++) ;
    }
    sz = rb - req_buf;

    VERBOSE(2){printf("Request (size %d):\n %s\n",sz,req_buf); fflush(stdout);}
    open_MEP_comm();

    if( (n = write(ms_mep_comm_sock, req_buf, sz)) < 0 ){
        VERBOSE_MSG(2,"socket write failed\n");
        return;
    }
    VERBOSE(2){ printf("write sock sent %d bytes\n",n); fflush(stdout);}

    VERBOSE(2){printf("Response: \n"); fflush(stdout);}
    if( (n = recv(ms_mep_comm_sock, resp_buf, CHARS_SZ-1, 0)) > 0 ){
        resp_buf[n] = 0;
        VERBOSE(2){ fputs(resp_buf,stdout); fflush(stdout); }
    }
    VERBOSE(2){ printf("read sock returned %d bytes\n",n); fflush(stdout);}
/*     while( (n=read(ms_mep_comm_sock, resp_buf, CHARS_SZ-1)) > 0 ){
        resp_buf[n] = '\0';
        //VERBOSE(2){printf("%s",resp_buf); fflush(stdout);} // try fputs(resp_buf,stdout)
        VERBOSE(2){ fputs(resp_buf,stdout); fflush(stdout); }
        //bzero( resp_buf, STRINGS_SZ );
    }
 */
    close_MEP_comm();
}

//
// END INTERFACE SHIMS
//////////////////////////////////////////////////////////


// bring in the other parts of the monitor sensor
#include "ms_vars.h"
#include "ms_json.h"
#include "ms_conf.h"
#include "ms_iface.h"
#include "ms_dump.h"

// currently not safe for multi-threaded service-under-scruting (SUS)
// if multiple threads call responder() or set trigger vars
// due to structures used during responder() processing

#ifdef MS_TEST
#include "ms_test.h"
#endif
