#define VERBOSITY 3
#define VERBOSE(L) if(VERBOSITY >= L)
#define VERBOSE_MSG(L,M) if(VERBOSITY >= L){printf(M);fflush(stdout);}
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <float.h>
#include <limits.h>

#define JSMN_PARENT_LINKS
#include "jsmn.h"

// TODO - check these limits only during configuration initialization
#define JSON_STRING_SIZE 2048
#define JSMN_TOKEN_ARRAY_SIZE 1024
#define CHARS_SZ 4096
#define STRINGS_SZ 1024
#define SH_VAR_SZ 20
#define MON_ATOM_SZ 20

typedef enum {
    monitor_uninitialized,
    monitor_initialized,
    monitor_started,
    monitor_stopping
} mstatus;

char *mstatus_string(mstatus ms){
    switch(ms){
        case monitor_uninitialized: return("monitor_uninitialized");
        case monitor_initialized: return("monitor_initialized");
        case monitor_started: return("monitor_started");
        case monitor_stopping: return("monitor_stopping");
    }
}

// TODO include monitor-specific _vars.h here
#include "ms_vars.h"
#include "ms_json.h"
#include "ms_conf.h"
#include "ms_dump.h"
#include "ms_iface.h"

// currently not safe for multi-threaded service-under-scruting (SUS)
// due to structures used during responder() processing
// if multiple threads call responder() or set trigger vars
