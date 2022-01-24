/*
 * Import JSON representation of configuration vector
 */

// storage of converted config data from JSON
char chars[CHARS_SZ]; // character storage
char *nextchar = chars; // next character to be allocated
char *strings[STRINGS_SZ]; // null-terminated string storage
char **nextstring = strings; // next string pointer to be allocated

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
    printf("lookup_cv_element %lu entries\n",sizeof(cv_element_names)/sizeof(char*));
  for(int k=0; k<sizeof(cv_element_names)/sizeof(char*); k++)
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

int // TODO is this used?
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
        //jsmntok_t *elt=tp+i;
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

void
sto_init( jsmntok_t *tp, char *js ){
    char *key, *val;
    get_key_val_pair(tp+1,&key,&val,js);
    next_sh_var_name_value->vnv_name = val;
    get_key_val_pair(tp+3,&key,&val,js);
    next_sh_var_name_value->vnv_value = val;
    ++next_sh_var_name_value;
}

void compile_monitor_atom(monitor_atom*); // defined in conf.h

void
sto_ma( jsmntok_t *tp, char *js ){
    char *key, *val;
    get_key_val_pair(tp+1,&key,&val,js);
    next_monitor_atom->ma_aid = val;
    get_key_val_pair(tp+3,&key,&val,js);
    next_monitor_atom->ma_aex = val;
    //compile_monitor_atom(next_monitor_atom);
    ++next_monitor_atom;
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

sh_var_name_value *
sto_array_of_init( jsmntok_t t[], int i, char *js ){
    int sz = 0; jsmntok_t *tp = &t[i];
    for(int j=1; j<=tp->size; j++){
        sto_init(tp+sz+1, js);
        sz += tok_cnt(t, i+sz+1);
    }
    return sh_var_name_values;
}

int
sto_array_of_ma( jsmntok_t t[], int i, char *js ){
    int sz = 0; jsmntok_t *tp = &t[i];
    for(int j=1; j<=tp->size; j++){
        sto_ma(tp+sz+1, js);
        sz += tok_cnt(t, i+sz+1);
    }
    return tp->size;
}

sh_var_decl *
get_arr_of_decl_attr(char *attr, jsmntok_t t[], int i, char *js){
    // each decl is an object, name: type:
    next_sh_var_decl = sh_var_decls; // reset
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        sto_array_of_decl( t, key_tok_idx+1, js );
    return sh_var_decls;
}

sh_var_name_value *
get_arr_of_init_attr(char *attr, jsmntok_t t[], int i, char *js){
    // each init is an object, name: value:
    next_sh_var_name_value = sh_var_name_values; // reset
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        sto_array_of_init( t, key_tok_idx+1, js );
    return sh_var_name_values;
}

int
get_arr_of_ma_attr(char *attr, jsmntok_t t[], int i, char *js){
    int natoms;
    // each monitor atom is an object, aid: aex:
    next_monitor_atom = monitor_atoms; // reset
    int key_tok_idx;
    if( (key_tok_idx = find_key_tok(attr, t, i, js)) != -1)
        natoms = sto_array_of_ma( t, key_tok_idx+1, js );
    
    return natoms;
}