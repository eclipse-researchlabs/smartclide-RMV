/* instead of using Monitor Sensor, do direct calls to MEP */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <curl/curl.h>

#define MONITOR_ID "monid_00004"
#define TOKEN "rmv_token"
#define ARG_SIZE 256
#define VERBOSE 0

char monitor_id[] = {MONITOR_ID};
char *session_id;

// define the application's reportable variables and their initial values
int m = 0;
int n = 1;
int o = 2;
bool p = true;
bool q = false;
float r;
float s = 1.0;

struct memory {
  char *data;
  size_t size;
};

CURL *curl;
CURLcode res;
struct memory response;

char token_arg[ARG_SIZE] = {"token="}; // built during init
char start_args[ARG_SIZE] = {""}; // built during init "token= &monitor_id= "
char normal_args[ARG_SIZE] = {""}; // built during init "token= &session_id= "
char hb_args[ARG_SIZE*4]; // build by mk_hb_args "token= session_id= heartbeat= "

char teststr[ARG_SIZE] = {"abcdef"};

void init_memory(struct memory *m){
  m->size = 0;
  m->data = malloc(m->size+1);
  if( m->data == NULL ){
    fprintf(stderr,"malloc() failed\n");
    exit(EXIT_FAILURE);
  }
  m->data[0] = '\0';
}

static size_t cb(void *data, size_t size, size_t nmemb, struct memory *clientp){
  size_t realsize = size * nmemb + clientp->size;
  struct memory *mem = (struct memory *)clientp;
  clientp->data = realloc(clientp->data, realsize + 1);
  if(clientp->data == NULL){
    fprintf(stderr,"realloc() failed\n"); exit(EXIT_FAILURE);
  }
  memcpy(clientp->data+clientp->size, data, size*nmemb);
  clientp->data[realsize] = '\0';
  clientp->size = realsize;
  return size*nmemb;
}

// boolean functions representing the atomic formulas
bool f_p(){ return p; }
bool f_a1(){ return n==2; }
bool f_a2(){ return n<2; }
bool f_a3(){ return p==q; }
bool f_q(){ return q; }

//   Heartbeat API arguments include:
//   RMV token - default is "rmv_token"
//   Session ID - returned by monitor_start
//   Heartbeat structure:
//      monid - Monitor ID
//      sessid - Session ID
//      atoms - (ATl) list of the quoted names of true atoms
//      vars - (ORl) list of name:value pairs for reportable variables

char* mk_ATl(){ // make list the defined atomic formulas that evaluate to true
  int ntrue = 0;
  static char ATl[ARG_SIZE];
  sprintf(ATl, "[%s%s%s%s%s]",
    f_p() ? (ntrue++?",\"p\"":"\"p\"") : "",
    f_a1() ? (ntrue++?",\"a1\"":"\"a1\"") : "",
    f_a2() ? (ntrue++?",\"a2\"":"\"a2\"")  : "",
    f_a3() ? (ntrue++?",\"a3\"":"\"a3\"")  : "",
    f_q() ? (ntrue++?",\"q\"":"\"q\"") : ""
  );
  return ATl;
}

char* mk_ORl(){ // make list of the defined reportable variables
  static char ORl[ARG_SIZE];
  sprintf(ORl, "[{\"m\":%d},{\"n\":%d},{\"o\":%d},{\"p\":%s},{\"q\":%s},{\"r\":%f},{\"s\":%f}]",
    m,n,o,p?"true":"false",q?"true":"false",r,s);
  return ORl;
}

// build the heartbeat structure and the arguments to the heartbeat API
char* mk_hb_args(){
  static char hb_json[ARG_SIZE*2];
  sprintf(hb_json,"{\"monid\":\"%s\",\"sessid\":\"%s\",\"atoms\":%s,\"vars\":%s}\n",
        monitor_id, session_id, mk_ATl(), mk_ORl());
  strcpy(hb_args,normal_args); strcat(hb_args,"&heartbeat="); strcat(hb_args,hb_json);
  if(VERBOSE) fprintf(stderr,"heartbeat args: %s\n",hb_args);
  return hb_args;
}

// main program (which starts/stpos monitor) and service logic (which sends heartbeats)

void service_logic(){
  // do initial heartbeat with initial values of reportable variables
  mk_hb_args();
  curl_easy_reset(curl);
  init_memory(&response);
  curl_easy_setopt(curl, CURLOPT_VERBOSE, VERBOSE);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, cb);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&response);
  curl_easy_setopt(curl, CURLOPT_URL, "http://127.0.0.1:8005/mep/monitor_heartbeat");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, hb_args);
  if((res = curl_easy_perform(curl)) != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
  else fprintf(stderr, "result: %.*s\n", (int)response.size, response.data);

  // do some behavior that changes the variables, e.g.:
  n=5; p=false; o=7; r=3.14159; q=true;

  // do another heartbeat with new values
  mk_hb_args();
  curl_easy_reset(curl);
  init_memory(&response);
  curl_easy_setopt(curl, CURLOPT_VERBOSE, VERBOSE);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, cb);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&response);
  curl_easy_setopt(curl, CURLOPT_URL, "http://127.0.0.1:8005/mep/monitor_heartbeat");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, hb_args);
  if((res = curl_easy_perform(curl)) != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
  else fprintf(stderr, "result: %.*s\n", (int)response.size, response.data);
  return;
}
 
int main(int argc, char *argv[]){
  // initialization
  strcat(token_arg,TOKEN);
  strcpy(start_args,token_arg);
  strcat(start_args,"&monitor_id=");
  strcat(start_args,monitor_id);
  strcpy(normal_args,token_arg);
  strcat(normal_args,"&session_id=");

  curl_global_init(CURL_GLOBAL_ALL);
  curl = curl_easy_init(); // init once, reset thereafter
  if(!curl) {
    fprintf(stderr, "curl_easy_init() returned null\n"); fflush(stderr);
    return -1;
  }

  // start the monitoring session
  init_memory(&response);
  curl_easy_setopt(curl, CURLOPT_VERBOSE, VERBOSE);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, cb);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&response);
  curl_easy_setopt(curl, CURLOPT_URL, "http://127.0.0.1:8005/mep/monitor_start");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, start_args);
  if((res = curl_easy_perform(curl)) != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
  else fprintf(stderr, "result: %.*s\n", (int)response.size, response.data);

  // isolate the session ID in the response in a simple way, real app may parse the json
  session_id = strstr(response.data,"session(") + strlen("session(");
  if( session_id == NULL ){
    fprintf(stderr,"no session returned\n"); fflush(stderr);
    return -1;
  }
  *(strchr(session_id,')')) = '\0';
  strcat(normal_args,session_id); // do only once - add to normal_args

  service_logic();

  // stop the monitoring session
  curl_easy_reset(curl);
  init_memory(&response);
  curl_easy_setopt(curl, CURLOPT_VERBOSE, VERBOSE);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, cb);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&response);
  curl_easy_setopt(curl, CURLOPT_URL, "http://127.0.0.1:8005/mep/monitor_stop");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, normal_args);
  if((res = curl_easy_perform(curl)) != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
  else fprintf(stderr, "result: %.*s\n", (int)response.size, response.data);

  // cleanup
  curl_easy_cleanup(curl);
  curl_global_cleanup();
  return 0;
}
