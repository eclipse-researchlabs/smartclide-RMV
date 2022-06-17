% RMV - Monitor Library
% Work in Progress

:- module(rmv_ml,[app/7, model/2, trc/1,
                  service/2, service/7, cons_service/8, is_service/1, is_service/8,
                  service_spec/2, is_service_spec/1, is_service_spec/3,
                  is_service_spec_body/1, is_service_spec_body/2,
                  is_cmd/1,
                  cons_deployment/4, is_deployment/1, is_deployment/3, is_deployment/4,
                  monitor/2, monitor/9, cons_monitor/10, is_monitor/1, is_monitor/2, monitor_atoms_eval/3,
                  model/2, model/4, cons_model/5, is_model/1,
                  ms_cv/2, ms_cv/14, cons_ms_cv/15,
                  property/2, is_property/1, is_property/3,
                  is_app/1, is_app/8, is_service/1, is_service/8, is_trace/1, is_trace/3,
                  is_service_creation_context/1,
                  ssid2modid/2, ssid2propid/2, ssid2scripid/2, modid2monid/2, monid2monvarshfile/2,
                  load_service_specification_from_file/2,
                  load_service_specification_immediate/2, load_service_specification/2,
                  unload_service_specification/1,
                  load_monitor/1, unload_monitor/1, truncate_trace/2,
                  target_lang_monitor_sensor/5, type_pl_n_e_c/4,
                  monid_sessid_suniq/3, monid_sessid_muniq_suniq/4,
                  pjson_ms_cv/1, json_var_val/2, pjson2monitor/2,
                  display_monitor/1, display_cv/1
	       ]).

:- use_module('COM/param').
:- use_module([rmv_ml_mt,rmv_ml_pst]).
:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).

%:- use_module(library(test_wizard)).
%:- set_prolog_flag(log_query_file, 'queries.pl').

%-------------------------------------------
%
% Parameters used from COM/param:
%

%-------------------------------------------
%
% Other Parameters:
%
% standard choices for monitor sensor implementation
% target_lang(ms_lang, LangName, Directory,           File,       EvalMode)
target_lang_monitor_sensor(ms_c,  'C',      'RMV/SENSORS/rmv_ms_c',  'sensor.h',  ms_eval) :- !.
target_lang_monitor_sensor(ms_pl, 'Prolog', 'RMV/SENSORS/rmv_ms_pl', 'rmv_ms.pl', ms_eval) :- !.
target_lang_monitor_sensor(  _  , x,        x,                       x,           x).

type_pl_n_e_c(undefined,0,svt_UNDEFINED,undefined).
type_pl_n_e_c(boolean,2,svt_Boolean,bool).
type_pl_n_e_c(integer,3,svt_Integer,int).
type_pl_n_e_c(float,4,svt_Float,float).
type_pl_n_e_c(address,5,svt_Address,'void*').
type_pl_n_e_c(byte,6,svt_Byte,char).        % unused
type_pl_n_e_c(string,7,svt_String,'char[]').% unused
type_pl_n_e_c(char,8,svt_Char,char).        % unused
type_pl_n_e_c(symbol,9,svt_Symbol,'char[]').% unused

% now in param as rmv_atom_eval_mode and rmv_monitor_sensor_lang
% atom_eval_mode, if defined, overrides monitor_atom_eval in the configuration vector
%:- dynamic atom_eval_mode/1, monitor_sensor_lang/1.
% atom_eval_mode(_) is one of unset_eval, no_eval, ms_eval or mep_eval
% monitor_sensor_lang(_) one of ms_pl or ms_c

atom_eval_mode_type([unset_eval, no_eval, ms_eval, mep_eval]).
monitor_sensor_lang_type([ms_pl, ms_c]).

%-------------------------------------------
%
% STORAGE of various structures
%

% structures stored in the monitor library
%
:- dynamic app/7, model/4, monitor/9, ms_cv/14,
        property/2, service/7, deployment/3,
        service_spec/2, trc/1.
%-------------------------------------------


%-------------------------------------------
%
% CHECKERS / CONSTRUCTORS
%

% APPLICATION
% is_app(App,AppId,AppVars,AppTS,AppInputVars,AppOutputVars,AppCurrentInput,AppCurrentOutput).
is_app(App) :- is_app(App,_,_,_,_,_,_,_).
is_app(App,_,_,_,_,_,_,_) :-
        App = app(_,_,_,_,_,_,_).

is_cmd(_C) :- true. % enumerate the valid commands

% DEPLOYMENT

cons_deployment(D,Did,S,M) :- atom(Did), is_service(S), is_monitor(M),
        D = deployment(Did,S,M).

is_deployment(D) :- is_deployment(D,_,_), !.
is_deployment(D,Did,S) :- is_service(S),
        D = deployment(Did,S).
is_deployment(D,Did,S,M) :-
        D = deployment(Did,S,M),
        is_service(S), is_monitor(M).

% MODEL

is_smv_struct(SMVstruct) :-
        SMVstruct = smv(_, SMVtext, SMVord, SMVvars),
        SMVvars = vars(Vdecls, Vo, Vm, Vp, Vr, Vt),
        atom(SMVtext), atom(SMVord),
        is_list(Vdecls), % check that decls are well formed v:t
        is_list(Vo), is_list(Vm),
        is_list(Vp), is_list(Vr), is_list(Vt).

model( ModelId, Model ) :-
        model(ModelId,SMVstruct,SMVmodelFile,SMVordFile),
        Model = model(ModelId,SMVstruct,SMVmodelFile,SMVordFile).

cons_model(ModelId,SMVstruct,SMVmodelFile,SMVordFile, Model) :-
        Model = model(ModelId,SMVstruct,SMVmodelFile,SMVordFile).

is_model(Model) :-
        Model = model(ModelId,SMVstruct,SMVmodelFile,SMVordFile),
        atom(ModelId), is_smv_struct(SMVstruct), atom(SMVmodelFile), atom(SMVordFile),
        %param:monitor_directory_name(MD),
        %atomic_list_concat([MD,'/',Name,'.smv'],SMVmodelFile),
        %exists_file(SMVmodelFile),
        true.

% MONITOR

monitor( MonId, Monitor ) :- % lookup stored monitor by id
        monitor( MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile ),
        Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile ).

cons_monitor(MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile, Monitor) :- % construct/deconstruct
        Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile ).

is_monitor(Monitor) :- is_monitor(Monitor,_). % is a well-formed monitor
is_monitor(Monitor,MonId) :-
        Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile ),
        atom(MonId), atom(SSpecId), atom(ModId), atom(MSfile), atom(MSCVfile), atom(SVfile),
        Properties = properties( PropVars, PropAtoms, PropFormulas ),
        is_list(PropVars), is_list(PropAtoms), is_list(PropFormulas),
        (MSlang == ms_c; MSlang == ms_pl), is_ms_cv(MScv),
        % further conditions on the arguments
        true.

monitor_atoms_eval( MonId, Atoms, AEval ) :- % from MonitorId extract atoms and where to evaluate
        ms_cv(MonId,MScv),
        MScv = ms_cv(MonId,_Vdecl,_Vo,_Vm,_Vp,_Vr,_Vt,Atoms,AEval,_Vinit,_Beh,_Timer,_Host,_Port),
        true.

% PROPERTY

is_property(P) :- is_property(P,_,_).
is_property(P,Pname,Pformula) :-
        P = property(Pname,Pformula).

% SERVICE

service( Sid, Service ) :-
        service(Sid,B,C,D,E,F,G),
        Service = service( Sid, B, C, D, E, F, G ).

cons_service( Sid, B, C, D, E, F, G, Service ) :-
        Service = service( Sid, B, C, D, E, F, G ).

is_service(S) :- is_service(S,_,_,_,_,_,_,_).
is_service(S,A,B,C,D,E,F,G) :-
        S = service(A,B,C,D,E,F,G),
        true.

% SERVICE SPECIFICATION

is_service_spec( SS ) :- is_service_spec(SS,_,_).
is_service_spec( ServiceSpec, Id, Sbody ) :-
        ServiceSpec = service_spec(Id,Sbody),
        atom(Id), is_service_spec_body(Sbody).

is_service_spec_body( SsB ) :- is_service_spec_body(SsB,_).
is_service_spec_body( ss_body(B), B ).

% TRACE

is_trace(T) :- is_trace(T,_,_).
is_trace(Trace, TraceId, States) :-
        Trace = trace(TraceId, States).

% SERVICE CREATION CONTEXT

is_service_creation_context(SCC) :- is_list(SCC).

% MONITOR SENSOR CONFIGURATION VECTOR

% ms_cv/2 retrieves an ms_cv/14 record by monitor Id
ms_cv( MonId, MScv ) :- % lookup stored monitor configuration vector by MonitorId
        monitor( MonId, _SSpecId, _ModId, _Properties, _MSlang, _MSid, MScv, _MSfile, _SVfile ).

% construct/deconstruct
cons_ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port, MScv) :-
        MScv = ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port).

% is a well-formed cv
is_ms_cv(MScv) :-
        MScv = ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port),
        atom(MonId), atom(AEval),
        is_list(Vdecl), is_list(Vo), is_list(Vm), is_list(Vp), is_list(Vr), is_list(Vt),
        is_list(Atoms),
        atom_eval_mode_type(EvalType), member(AEval,EvalType),
        is_list(Vinit), is_list(Beh),
        number(Timer), atom(Host), number(Port),
        % further conditions on the arguments
        true.


%-------------------------------------------
% Conversions among identifiers
ssid2modid(SSpecId,ModelId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(modid_,N,ModelId).

ssid2propid(SSpecId,PropId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(propid_,N,PropId).

ssid2scripid(SSpecId,ScripId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(scripid_,N,ScripId).

modid2monid(ModelId,MonitorId) :-
        atom_concat(modid_,N,ModelId), atom_concat(monid_,N,MonitorId).

monid2modid(MonitorId,ModelId) :-
        atom_concat(monid_,N,MonitorId), atom_concat(modid_,N,ModelId).

monid2monvarshfile(MonitorId,MonVarsFile) :-
        atom_concat(MonitorId,'_vars.h',MonVarsFile).

monitorid_nurvid(Mid,NuRVid) :- Mid = NuRVid. % define if necessary

monid_sessid_suniq(Mid,SessId,Suniq) :- % +Mid, +Sid, -Suniq
    param:rmv_monitor_id_prefix(Mpref), param:rmv_session_id_prefix(Spref),
	atom_concat(Mpref,Mu,Mid), atom_length(Spref,SpL), atom_length(Mu,MuL),
	SuS is SpL+MuL+1, sub_atom(SessId,SuS,_,0,Suniq).

monid_sessid_muniq_suniq(Monid,Sessid,Muniq,Suniq) :- % ?Mid, +Sid, ?Muniq, ?Suniq
	param:rmv_monitor_id_prefix(Mpref), param:rmv_session_id_prefix(Spref),
	atom_concat(Spref,ID,Sessid),
	atomic_list_concat([Muniq,Suniq],'_',ID),
	atom_concat(Mpref,Muniq,Monid).

%

%-------------------------------------------
% JSON CONVERSIONS
%

json_var_val( json([Var='@'(true)]), Var=true ) :- !.
json_var_val( json([Var='@'(false)]), Var=false ) :- !.
json_var_val( json([Var=Val]), Var=Val ).



%-------------------------------------------
% IMPORT / EXPORT of structures
%

% Service Specifications
%   service_spec( SpecId, SpecBody )
%

load_service_specification_from_file(_F,_Sid) :- writeln(unimplemented),
        true.

load_service_specification_immediate(SSAtom,Sid) :-
        read_term_from_atom(SSAtom,SSTerm,[]),
        load_service_specification(SSTerm,Sid).

load_service_specification(SSTerm,Sid) :-
        is_service_spec(SSTerm,Sid,Sbody),
        retractall( service_spec(Sid,_) ),
        assert( service_spec(Sid,Sbody) ),
        unpack_sspec(Sbody).

unload_service_specification(Sid) :-
        retractall( service_spec(Sid,_) ).

unpack_sspec(_Sbody).

% Monitors
load_monitor(Mon) :-
        Mon = monitor(MonitorId,_,_,_,_,_,_,_,_),
        retractall(monitor(MonitorId,_,_,_,_,_,_,_,_)),
        assert(Mon),
        true.

unload_monitor(MonId) :-
        retractall(monitor(MonId,_,_,_,_,_,_,_)).


%-------------------------------------------
% INITIALIZATION
%
:- dynamic ml_initialized/1.
ml_initialized(false).

init:- ml_initialized(true), !. % already initialized
init :-
	% ...
        retractall(ml_initialized(_)), assert(ml_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(ml_initialized(_)), assert(ml_initialized(false)).

%-------------------------------------------
%
% Examples
%
%  including built-ins used in some self-test cases
%

app(appid_00001,a,b,c,d,e,f).

% monitor( MonId, SSpecId, ModId, Properties, MSlang, MSid, MScv, MSfile )
%   MonitorId - unique monitor ID (suffix should be unique over all runs)
%   ModelId - unique model ID (same suffix as MonId)
%   MScv - mv_cv/14 initialization vector for monitor sensor (see below)
%   NuRVinfo - info for NuRV interaction for *this* monitor
%   MSlang - implementation language of the service & monitor sensor: ms_c, ms_pl, ...
%
% Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSid, MScv, MSfile ),
% Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSid, MScv, MSfile, SVfile )
/*
monitor(monid_00002, ssid_00002, modid_00002, [], ms_c, msid_00027,
        ms_cv( monid_00002, [m:integer, n:integer, o:integer, p:boolean, q:boolean, r:float, s:float],
                [m, n, o, p, q, r, s], [n, p, q, s], [m, n, o, p, q, r, s], [m, n, o, p, q, r, s], [q, s],
                [p:p, a1:eq(n,2), a2:lt(r,2.4), a3:eq(p,q), a4:false, a5:true, a6:eq(p,true),
                a7:ne(n,s), a7a:ne(s,n), a8:lt(o,2.4), a8a:ge(2.4,o), q:q, notp:not(p)],
                ms_eval, ms_pl, [], 0, '127.0.0.1', 8005),
        'sensor.h', 'monitor_vars.h' ).
*/
/*
monitor(monid_00004, ssid_00004, modid_00004, [], ms_pl, msid_00047,
        ms_cv( monid_00004, [n:integer, o:integer, p:boolean, q:boolean, r:float, s:float],
                [n, o, p, q, r, s], [n, p, q, s], [n, p, q], [n, o, s, p, q], [q, s],
                [p:p, a1:eq(n,2), a2:lt(s,2.4), a3:eq(p,q), a4:false, a5:true, a6:eq(p,true),
                a7:ne(n,s), a7a:ne(s,n), a8:lt(o,2.4), a8a:ge(2.4,o), q:q, notp:not(p)],
                ms_eval, ms_pl, [], 0, '127.0.0.1', 8005),
        'rmv_ms.pl', none ).
*/
/*
monitor(monid_00004, ssid_00004, modid_00004, [], ms_c, msid_00047,
        ms_cv( monid_00004, [m: integer, n:integer, o:integer, p:boolean, q:boolean, r:float, s:float],
                [m, n, o, p, q, r, s], [n, p, q, s], [n, p, q], [n, o, p, q, s], [q, s],
                [p:p, a1:eq(n,2), a2:lt(n,2), a3:eq(p,q), q:q], ms_eval,
                [m=0, n=1, o=2, p=true, q=false, r=undefined, s=1],
                [n=5, p=false, o=7, r=3.14159, q=true], 0, '127.0.0.1', 8005),
                'RMV/SENSORS/rmv_ms_c/sensor.h', 'RUNTIME/MONITORS/monid_00004_vars.h' ).
*/

property(propid_00001,true).

service(servid_00001,b,c,d,e,f,_T).

service_spec(ssid_00002, ss_body(x)).

trc( trace('counter-example',
      [state('1',[p='TRUE',q='FALSE']),state('2',[p='TRUE',q='FALSE']),state('3',[p='TRUE',q='FALSE']),
       state('4',[p='FALSE',q='TRUE']),state('5',[p='FALSE',q='TRUE']),state('6',[p='FALSE',q='TRUE']),
       state('7',[p='FALSE',q='TRUE']),state('8',[p='FALSE',q='TRUE']),state('9',[p='TRUE',q='FALSE']),
       state('10',[p='TRUE',q='FALSE']),state('11',[p='TRUE',q='FALSE']),state('12',[p='FALSE',q='TRUE']),
       state('13',[p='TRUE',q='FALSE']),state('14',[p='TRUE',q='FALSE']),state('15',[p='TRUE',q='FALSE']),
       state('16',[p='TRUE',q='FALSE'])])
   ).

truncate_trace(trace(N,[A,B,C,D,E,F|_]),trace(N,[A,B,C,D,E,F])) :- !. % truncate to 6 steps
truncate_trace(T,T).

% monitor sensor configuration vector
% ms_cv/14 is the repository of configuration vectors
%
% ms_cv( MonId, Vdecls, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port )
%
%:- dynamic ms_cv/4, ms_cv/14.

ex_cv(1, ms_cv( % old format
             /* monitor_id */         'mid_00001',
             /* shared vars */        [a,b,c,s,t,u,v,w,x,y,z],
             /* monitor_atoms */      [a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],
             /* monitor_vars */       [s,t,u,v,w,x,y,z],
             /* monitor_observable_vars */ [u,v,w,x,y,z],
             /* monitor_property_vars */   [v,w,x,y,z],
             /* monitor_reportable_vars */ [v,w,x,y,z],
             /* monitor_trigger_vars */    [x],
             /* monitor_atom_eval */  ms_eval,
             /* shared_vars_init */   [s=undefined,
                                       t=undefined,
                                       u=undefined,
                                       v=false,
                                       w=true,
                                       x=1,
                                       y=2,
                                       z=3
                                      ]
       )
).

ex_cv(2, ms_cv( % new format
             /* monitor_id */         'monid_00004',
             /* shared_var_decl */    [m:integer,
                                       n:integer,
                                       o:integer,
                                       p:boolean,
                                       q:boolean,
                                       r:float,
                                       s:float],
             /* observable_vars */    [m, n, o, p, q, r, s],
             /* model_vars */         [n, p, q, s],
             /* property_vars */      [n, p, q],
             /* reportable_vars */    [m, n, o, p, q, r, s],
             /* trigger_vars */       [q, s],
             /* monitor_atoms */      [p:p,a1:eq(n,2),a2:lt(n,2),a3:eq(p,q),q:q],
             /* monitor_atom_eval */  ms_eval,
             /* shared_var_init */    [m=0,
                                       n=1,
                                       o=2,
                                       p=true,
                                       q=false,
                                       r=undefined,
                                       s=1
                                      ],
             /* behavior */           [n=5,p=false,o=7,r=3.14159,q=true],
             /* timer */              0,
             /* rmvhost */            '127.0.0.1',
             /* rmvport */            8005
        )
).

% JSON representation of monitor structure
%
%

% example JSON monitors
% NOTE: if used for testing these must agree with the corresponding _conf.json file in RUNTIME/MONITORS/
jMon(monid_00004, '{
      \"monitor_id\":\"monid_00004\",
      \"service_spec_id\":\"ssid_00004\",
      \"model_id\":\"modid_00004\",
      \"properties\":{ \"property_vars\":[ ],
                       \"property_atoms\":[ ],
                       \"property_formulas\":[ ]
       },
      \"monitor_sensor_lang\":\"ms_c\",
      \"monitor_sensor_file\":\"RMV/SENSORS/rmv_ms_c/sensor.h\",
      \"configuration_vector\":{
            \"monitor_id\":\"monid_00004\",
            \"shared_var_decl\": [
              {\"name\":\"m\", \"type\":\"integer\"},
              {\"name\":\"n\", \"type\":\"integer\"},
              {\"name\":\"o\", \"type\":\"integer\"},
              {\"name\":\"p\", \"type\":\"boolean\"},
              {\"name\":\"q\", \"type\":\"boolean\"},
              {\"name\":\"r\", \"type\":\"float\"},
              {\"name\":\"s\", \"type\":\"float\"}
            ],
            \"observable_vars\": [\"m\", \"n\", \"o\", \"p\", \"q\", \"r\", \"s\" ],
            \"model_vars\": [\"n\", \"p\", \"q\", \"s\" ],
            \"property_vars\": [\"n\", \"p\", \"q\" ],
            \"reportable_vars\": [\"m\", \"n\", \"o\", \"p\", \"q\", \"r\", \"s\" ],
            \"trigger_vars\": [\"q\", \"s\" ],
            \"monitor_atoms\": [
              {\"aid\":\"p\", \"aex\":\"p\"},
              {\"aid\":\"a1\", \"aex\":\"eq(n,2)\"},
              {\"aid\":\"a2\", \"aex\":\"lt(n,2)\"},
              {\"aid\":\"a3\", \"aex\":\"eq(p,q)\"},
              {\"aid\":\"q\", \"aex\":\"q\"}
            ],
            \"monitor_atom_eval\":\"ms_eval\",
            \"shared_var_init\": [
              {\"name\":\"m\", \"value\":0},
              {\"name\":\"n\", \"value\":1},
              {\"name\":\"o\", \"value\":2},
              {\"name\":\"p\", \"value\":\"true\"},
              {\"name\":\"q\", \"value\":\"false\"},
              {\"name\":\"r\", \"value\":\"undefined\"},
              {\"name\":\"s\", \"value\":1}
            ],
            \"behavior\": [
              {\"name\":\"n\", \"value\":5},
              {\"name\":\"p\", \"value\":\"false\"},
              {\"name\":\"o\", \"value\":7},
              {\"name\":\"r\", \"value\":3.14159},
              {\"name\":\"q\", \"value\":\"true\"}
            ],
            \"timer\":0,
            \"rmvhost\":\"127.0.0.1\",
            \"rmvport\":8005
       },
      \"configuration_vector_file\":\"RUNTIME/MONITORS/monid_00004_conf.json\",
      \"shared_var_decl_file\":\"RUNTIME/MONITORS/monid_00004_vars.h\"
}').

jMon(monid_00005, '{
      \"monitor_id\":\"monid_00005\",
      \"service_spec_id\":\"ssid_00005\",
      \"model_id\":\"modid_00005\",
      \"properties\":{ \"property_vars\":[ ],
                       \"property_atoms\":[ ],
                       \"property_formulas\":[ ]
       },
      \"monitor_sensor_lang\":\"ms_c\",
      \"monitor_sensor_file\":\"RMV/SENSORS/rmv_ms_c/sensor.h\",
      \"configuration_vector\":{
            \"monitor_id\":\"monid_00005\",
            \"shared_var_decl\": [
              {\"name\":\"m\", \"type\":\"integer\"},
              {\"name\":\"n\", \"type\":\"integer\"},
              {\"name\":\"o\", \"type\":\"integer\"},
              {\"name\":\"p\", \"type\":\"boolean\"},
              {\"name\":\"q\", \"type\":\"boolean\"},
              {\"name\":\"r\", \"type\":\"float\"},
              {\"name\":\"s\", \"type\":\"float\"}
            ],
            \"observable_vars\": [\"m\", \"n\", \"o\", \"p\", \"q\", \"r\", \"s\" ],
            \"model_vars\": [\"n\", \"p\", \"q\", \"s\" ],
            \"property_vars\": [\"n\", \"p\", \"q\" ],
            \"reportable_vars\": [\"m\", \"n\", \"o\", \"p\", \"q\", \"r\", \"s\" ],
            \"trigger_vars\": [\"q\", \"s\" ],
            \"monitor_atoms\": [
              {\"aid\":\"p\", \"aex\":\"p\"},
              {\"aid\":\"a1\", \"aex\":\"eq(n,2)\"},
              {\"aid\":\"a2\", \"aex\":\"lt(n,2)\"},
              {\"aid\":\"a3\", \"aex\":\"eq(p,q)\"},
              {\"aid\":\"q\", \"aex\":\"q\"}
            ],
            \"monitor_atom_eval\":\"no_eval\",
            \"shared_var_init\": [
              {\"name\":\"m\", \"value\":0},
              {\"name\":\"n\", \"value\":1},
              {\"name\":\"o\", \"value\":2},
              {\"name\":\"p\", \"value\":\"true\"},
              {\"name\":\"q\", \"value\":\"false\"},
              {\"name\":\"r\", \"value\":\"undefined\"},
              {\"name\":\"s\", \"value\":1}
            ],
            \"behavior\": [
              {\"name\":\"n\", \"value\":5},
              {\"name\":\"p\", \"value\":\"false\"},
              {\"name\":\"o\", \"value\":7},
              {\"name\":\"r\", \"value\":3.14159},
              {\"name\":\"q\", \"value\":\"true\"}
            ],
            \"timer\":0,
            \"rmvhost\":\"127.0.0.1\",
            \"rmvport\":8005
       },
      \"configuration_vector_file\":\"RUNTIME/MONITORS/monid_00005_conf.json\",
      \"shared_var_decl_file\":\"RUNTIME/MONITORS/monid_00005_vars.h\"
}').

json2monitor(JM,M) :-
        open_string(JM,S),
        json_read(S,PJM,[]),
        close(S),
        pjson2monitor(PJM,M).

monitor2json(M,JM) :-
        monitor2pjson(M,PJM),
        atom_json_term(JM,PJM,[as(atom)]).


pjson2monitor(PJM,M) :-
        cons_monitor(MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile, M),
	PJM = json([
		monitor_id=MonId,
		service_spec_id=SSpecId,
		model_id=ModId,
		properties=JProperties,
		monitor_sensor_lang=MSlang,
		monitor_sensor_file=MSfile,
		configuration_vector=JMScv,
		configuration_vector_file=MSCVfile,
		shared_var_decl_file=SVfile
	]),
	JProperties = json([ property_vars=PropVars, property_atoms=PropAtoms,
                        property_formulas=PropFormulas ]),
	Properties = properties(PropVars,PropAtoms,PropFormulas),
	ms_cv_pjson(MScv,JMScv).


monitor2pjson(M,PJM) :-
        cons_monitor(MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile, M),
	PJM = json([
		monitor_id=MonId,
		service_spec_id=SSpecId,
		model_id=ModId,
		properties=JProperties,
		monitor_sensor_lang=MSlang,
		monitor_sensor_file=MSfile,
		configuration_vector=JMScv,
		configuration_vector_file=MSCVfile,
		shared_var_decl_file=SVfile
	]),
	JProperties = json([ property_vars=PropVars, property_atoms=PropAtoms,
                        property_formulas=PropFormulas ]),
	Properties = properties(PropVars,PropAtoms,PropFormulas),
	ms_cv_pjson(MScv,JMScv).


ms_cv_pjson(MScv,JMScv) :- var(MScv), !, rmv_ml:pjson_to_ms_cv(JMScv,MScv).
ms_cv_pjson(MScv,JMScv) :- var(JMScv), !, rmv_ml:ms_cv_to_pjson(MScv,JMScv).



assigns_to_pjson(A,JA) :- findall( json([name=Svn,value=Svv]), member(Svn=Svv, A), JA).

decls_to_pjson(D,JD) :- findall( json([name=Svn,type=Svt]), member(Svn:Svt, D), JD).

ms_cv_to_pjson(CV,PJCV) :-
        CV =  ms_cv(Mid,Svd,Vo,Vm,Vp,Vr,Vt,Atoms,Mae,Vinit,Beh,Timer,Host,Port),
        PJCV = json([
                monitor_id=Mid,
                shared_var_decl=JSvd,
                observable_vars=Vo,
                model_vars=Vm,
                property_vars=Vp,
                reportable_vars=Vr,
                trigger_vars=Vt,
                monitor_atoms=JAtoms,
                monitor_atom_eval=Mae,
                shared_var_init=JSvi,
                behavior=JBeh,
                timer=Timer,
                rmvhost=Host,
                rmvport=Port
        ]),
        findall(json([aid=Ai,aex=Eaf]),
                ( member(Ai:Af,Atoms), encode_as_atoms(Af,Eaf)),
                JAtoms),
        decls_to_pjson(Svd,JSvd),
        assigns_to_pjson(Vinit,JSvi),
        assigns_to_pjson(Beh,JBeh).

pjson_to_assigns(JA,A) :- findall( Svn=Svv, member( json([name=Svn,value=Svv]), JA), A ).

pjson_to_ms_cv(PJCV,CV) :-
        PJCV = json([
                monitor_id=Mid,
                shared_var_decl=JSvd,
                observable_vars=Ov,
                model_vars=Mv,
                property_vars=Pv,
                reportable_vars=Rv,
                trigger_vars=Tv,
                monitor_atoms=JAtoms,
                monitor_atom_eval=Mae,
                shared_var_init=JSvi,
                behavior=JBeh,
                timer=Timer,
                rmvhost=Host,
                rmvport=Port
        ]),
        CV =  ms_cv(Mid,Svd,Ov,Mv,Pv,Rv,Tv,Atoms,Mae,Svi,Beh,Timer,Host,Port),
        findall(Svn:Svt,
                ( member(json([name=Svn,type=Svt]), JSvd) ),
                Svd ),
        findall(Ai:Af,
                ( member(json([aid=Ai,aex=Eaf]), JAtoms),
                  reconstitute_atoms(Eaf,Af) ),
                Atoms ),
        pjson_to_assigns(JSvi,Svi),
        pjson_to_assigns(JBeh,Beh).

ms_hb_to_pjson(HB,PJHB) :-
        HB = (x),
        PJHB = json([]).

pjson_to_ms_hb(PJHB,HB) :-
        PJHB = json([]),
        HB = (x).

% ms_cv/2 retrieves an ms_cv/4 record by monitor Id
xms_cv(Mid,ms_cv(MonId,SV,MA,SVI)) :-
        MonId=monitor_id(Mid),
        ms_cv(MonId,SV,MA,SVI), !.

xms_cv_to_pjson(CV,PJCV) :-
        CV = ms_cv( monitor_id(Mid),
                    shared_variables(Sv),
                    monitor_atoms(Ma),
                    sus_variable_init(Svi)
                  ),
        findall(json([struct=atm,aid=Ai,aex=Eaf]),
                (member(atm(Ai,Af),Ma), encode_as_atoms(Af,Eaf)),
                JMa),
        PJCV = json([
                   monitor_id=Mid,
                   shared_variables=Sv,
                   monitor_atoms=JMa,
                   sus_variable_init=Svi
               ]),
        true.

pjson_ms_cv( % /4
    json([
        monitor_id='mid_00001',
        shared_variables=[a,b,c],
%        shared_variables=[a,b,c,s,t,u,v,w,x,y,z],
%        at=atm(a1,2),
%        monitor_atoms=[atm(a1,2),atm(a2,2),atm(a3,x),atm(a4,2)],
%        monitor_atoms=[atm(a1,eq(x,2)),atm(a2,lt(x,2)),atm(a3,lt(y,x)),atm(a4,leq(x,2))],
%        monitor_atoms=[a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],
%        monitor_atoms=json([atm(a1,rmv_ml:lt(z,x))]),
%        %,a2-json([op=lt,arg1=x,arg2=2])]),
%        monitor_variables=[s,t,u,v,w,x,y,z],
%        monitor_observable_vars=[u,v,w,x,y,z],
%        monitor_property_vars=[v,w,x,y,z],
%        monitor_reportable_vars=[v,w,x,y,z], monitor_trigger_vars=[x],
%        monitor_atom_eval=ms_eval,
        monitor_atoms=[
            json([struct=atm, aid=a2, aex='lt(x,2)'])
        ],
        sus_variable_init=[]
    ])
).

%monitor( MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile ),
monitor_elements([
        monitor_id,
        service_spec_id,
        model_id,
        properties,
        monitor_sensor_lang,
        monitor_sensor_file,
        configuration_vector,
        configuration_vector_file,
        shared_var_decl_file]).

pt(rmv_ml:lt(x,2)).
pt1( rmv_ml:atm(a,lt(x,2)) ).
pt2(rmv_ml:atm(a,b)).
pt3(atm(a,(rmv_ml:lt(x,2)))).

check_conversions :- Mid='mid_00001',
        ex_cv(2,MS_CV), format('ms_cv from library: ~q~n',MS_CV),
        nl,
        ms_cv_to_pjson(MS_CV,PJCV), format('ms_cv_to_pjson: ~q~n',PJCV),
        nl,
        atom_json_term(JSA,PJCV,[as(atom)]), format('atom_json_term atom from pjson_ms_cv: ~q~n',JSA),

        param:monitor_directory_name(MD), atomic_list_concat([MD,'/',Mid,'_conf.json'], CF),
        current_output(Old),
        open(CF,write,CFstream,[create([default])]),
        set_output(CFstream),
        writeln(CFstream,JSA),
        close(CFstream,[force(true)]),
        set_output(Old),

        is_json_term(JSA), format('the above is a valid JSON string, written to: ~s\n',CF),
        nl,nl,

        atom_json_term(JSA,JST,[]), format('atom_json_term term from prev conversion: ~q~n',JST),
        nl,
        pjson_to_ms_cv(JST,RCV), format('back to a CV term: ~q~n',RCV),

        ground(MS_CV), ground(RCV),
        MS_CV = RCV, % check that final round-trip result unifies with original
        true.

atom_ops([not,eq,ne,neq,gt,lt,geq,leq,ge,le]).

encode_as_atoms(A,A) :- atomic(A), !. % includes [] case
encode_as_atoms([A|As],[EA|EAs]) :- !,
        encode_as_atoms(A,EA), encode_as_atoms(As,EAs).
encode_as_atoms(A,E) :- compound(A), atom_ops(Ops),
        compound_name_arguments(A,N,_Args), memberchk(N,Ops), !,
        encode_as_atom(A,E).
encode_as_atoms(A,E) :- compound(A), !,
        compound_name_arguments(A,N,Args),
        encode_as_atoms(N,EN),
        maplist(encode_as_atoms,Args,EArgs),
        compound_name_arguments(E,EN,EArgs).

encode_as_atom(T,A) :- with_output_to(atom(A),write(T)).

%reconstitute_atoms(json(L),json(R)) :-!, reconstitute_atoms(L,R).
reconstitute_atoms([],[]) :- !.
reconstitute_atoms([A|As],[R|Rs]) :- !,
        reconstitute_atoms(A,R), reconstitute_atoms(As,Rs).
reconstitute_atoms(A1=A2,R1=R2) :- !,
        reconstitute_atoms(A1,R1), reconstitute_atoms(A2,R2).
reconstitute_atoms(A,R) :- atom(A), !,
        read_term_from_atom(A,T,[var_prefix(true)]), ground(T),
        (   T == A ->  R = A ; reconstitute_atoms(T,R) ).
reconstitute_atoms(A,A) :- number(A), !.
reconstitute_atoms(A,R) :- %compound(A), !,
        compound_name_arguments(A,N,Args),
        (       ( N == '@', (Args==[true] ; Args==[false]) )
        ->      Args = [R]
        ;       reconstitute_atoms(N,RN),
                maplist(reconstitute_atoms,Args,RArgs),
                compound_name_arguments(R,RN,RArgs)
        )
        ,true.

% LIBRARY DISPLAY OPS
%
% ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port)
display_cv(CV) :- display_cv(CV,'').

display_cv(CV,T) :- 
        CV = ms_cv(Monid,SVD,Ov,Mv,Pv,Rv,Tv,Ma,Mae,SVi,Beh,Timer,Host,Port),
        format('~amonitor_id: ~q~n', [T,Monid]),
        format('~ashared_var_decl: ',T), writeq(SVD), nl,
        format('~aobservable_vars: ',T), writeq(Ov), nl,
        format('~amodel_vars: ',T), writeq(Mv), nl,
        format('~aproperty_vars: ',T), writeq(Pv), nl,
        format('~areportable_vars: ',T), writeq(Rv), nl,
        format('~atrigger_vars: ',T), writeq(Tv), nl,
        format('~amonitor_atoms: ',T), writeq(Ma), nl,
        format('~amonitor_atom_eval: ~q~n', [T,Mae]),
        format('~ashared_var_init: ~q~n', [T,SVi]),
        format('~abehavior: ~q~n',[T,Beh]),
        format('~atimer: ~f~n',[T,Timer]),
        format('~ahost: ~a~n',[T,Host]),
        format('~aport: ~d~n',[T,Port]).

display_monitor(M) :-
        cons_monitor(MonId, SSpecId, ModId, Properties, MSlang, MSfile, MScv, MSCVfile, SVfile, M),
        format('Monitor Id: ~q~n',MonId),
        format('Service Spec: ~q~n',SSpecId),
        format('Model: ~q~n',ModId),
        format('Properties" ~q~n',[Properties]),
        format('Language: ~q~n',MSlang),
        format('MS file: ~q~n',MSfile),
        format('Configuration Vector:~n'),
        display_cv(MScv,'  '),
        format('MScv file: ~q~n',MSCVfile),
        format('SV file: ~q~n',SVfile).

