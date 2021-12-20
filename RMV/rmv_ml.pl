% RMV - Monitor Library
% Work in Progress

:- module(rmv_ml,[app/7, model/2, monitor/7, property/2, service/7, service_spec/2, trc/1,
                  is_service_spec/1, is_service_spec/3, is_service_spec_body/1, is_service_spec_body/2,
                  is_cmd/1, is_model/1, is_model/2,
                  is_deployment/1, is_deployment/2, is_deployment/3,
                  is_monitor/1, is_monitor/8,
                  is_property/1, is_property/3,
                  is_app/1, is_app/8, is_service/1, is_service/8, is_trace/1, is_trace/3,
                  is_service_creation_context/1,
                  ssid_modid/2, ssid_propid/2, ssid_scripid/2, modid_monid/2,
                  load_service_specification_from_file/2,
                  load_service_specification_immediate/2, load_service_specification/2,
                  unload_service_specification/1,
                  load_monitor/1, unload_monitor/1, truncate_trace/2,

                  pjson_ms_cv/1
	       ]).

:- use_module('COM/param').
:- use_module([rmv_ml_mt,rmv_ml_pst]).

:- use_module(library(test_wizard)).
:- set_prolog_flag(log_query_file, 'queries.pl').

%-------------------------------------------
%
% Parameters used from COM/param:
%


%-------------------------------------------
%
% STORAGE of various structures
%

% structures stored in the monitor library
%
:- dynamic app/7, model/2, monitor/7, ms_cv/10, property/2, service/7, service_spec/2, trc/1.
/*
% app(_,_,_._._._._)
% model(_,_)
% monitor(MonId,ModId,ModMScv._._._._)
%   MonId - unique monitor ID (suffix should be unique over all runs)
%   MonModId - unique model ID (same suffix as MonId)
%   MonMScv - mv_cv/10 abstract initialization vector for monitor sensor, example values:
        ms_cv(
             /* monitor_id */              'mid_00001',
             /* shared variables */        [a,b,c,s,t,u,v,w,x,y,z],
             /* monitor_atoms */           [a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],
             /* monitor_variables */       [s,t,u,v,w,x,y,z],
             /* monitor_observable_vars */ [u,v,w,x,y,z],
             /* monitor_property_vars */   [v,w,x,y,z],
             /* monitor_reportable_vars */ [v,w,x,y,z],
             /* monitor_trigger_vars */    [x],
             /* monitor_atom_eval */       ms_eval, /* where atoms evaluated: ms_eval,mep_eval,not_eval */
             /* SUS variable init */       [] /* list SUS variable initializations name=val */
             )
 %

ms_config_elements([monitor_id, shared_variables,
                    monitor_atoms, monitor_variables, monitor_observable_vars,
                    monitor_property_vars, monitor_reportable_vars, monitor_trigger_vars,
                    monitor_atom_eval
                   ]).

% ms_initial_abstract()
% ms_initial_concrete()
% property(PropId,Formula,Atoms,Variables)
% service(_,_,_._._._._)
% service_spec(_,_)
% trc(_)
*/

%-------------------------------------------


%-------------------------------------------
%
% TYPE CHECKERS / CONSTRUCTORS
%

% is_app(App,AppId,AppVars,AppTS,AppInputVars,AppOutputVars,AppCurrentInput,AppCurrentOutput).
is_app(App) :- is_app(App,_,_,_,_,_,_,_).
is_app(App,_,_,_,_,_,_,_) :-
        App = app(_,_,_,_,_,_,_).

is_cmd(_C) :- true.

is_deployment(D) :- is_deployment(D,_), !.
is_deployment(D) :- is_deployment(D,_,_), !.
is_deployment(D,S) :- is_service(S),
        D = deployment(S).
is_deployment(D,S,M) :- is_service(S), is_monitor(M),
        D = deployment(S,M).

is_model(M) :- is_model(M,_).
is_model(Model,Name) :- Model = model(Name),
        atom(Name),
        %param:monitor_directory_name(MD),
        %atomic_list_concat([MD,'/',Name,'.smv'],SMVmodelFile),
        %exists_file(SMVmodelFile),
        true.

% is_monitor(Monitor,MonId,MonState,MonTS,MonObservables,MonOut,MonCurrentInput,MonCurrentOutput) :-
%   1 MonId - monitor unique ID
%   2 ModId - model unique ID
%   3 MonObservables -
%   4 MonReportables -
%   5 MonAtoms -
%   6 MonAtomEval - ms | mep - where are atoms evaluated?
%   7 MonSensor -

is_monitor(Monitor) :- is_monitor(Monitor,_,_,_,_,_,_,_).
is_monitor(Monitor,MonId,ModId,_,_,_,_,_) :-
        Monitor = monitor(MonId,ModId,_,_,_,_,_).

is_property(P) :- is_property(P,_,_).
is_property(P,Pname,Pformula) :-
        P = property(Pname,Pformula).

is_service(S) :- is_service(S,_,_,_,_,_,_,_).
is_service(S,A,B,C,D,E,F,G) :-
        S = service(A,B,C,D,E,F,G),
        true. %service(A,B,C,D,E,F,G).

is_service_spec( SS ) :- is_service_spec(SS,_,_).
is_service_spec( ServiceSpec, Id, Sbody ) :-
        ServiceSpec = service_spec(Id,Sbody),
        atom(Id), is_service_spec_body(Sbody).

is_service_spec_body( SsB ) :- is_service_spec_body(SsB,_).
is_service_spec_body( ss_body(B), B ).

is_trace(T) :- is_trace(T,_,_).
is_trace(Trace, TraceId, States) :-
        Trace = trace(TraceId, States).

is_service_creation_context(SCC) :- is_list(SCC).


%-------------------------------------------
% Conversions among identifiers
ssid_modid(SSpecId,ModelId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(modid_,N,ModelId).

ssid_propid(SSpecId,PropId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(modid_,N,PropId).

ssid_scripid(SSpecId,ScripId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(scripid_,N,ScripId).

modid_monid(ModelId,MonitorId) :-
        atom_concat(modid_,N,ModelId), atom_concat(monid_,N,MonitorId).

monitorid_nurvid(Mid,NuRVid) :- Mid = NuRVid. % define if necessary


%-------------------------------------------
% JSON CONVERSION
%


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
        Mon = monitor(MonId,_,_,_,_,_,_),
        retractall(monitor(MonId,_,_,_,_,_,_)),
        assert(Mon),
        true.

unload_monitor(MonId) :-
        retractall(monitor(MonId,_,_,_,_,_,_)).


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

app(app001,a,b,c,d,e,f).

model(mod001,mmm).

monitor(monid_001,a,b,c,d,e,f).

property(propid_001,true).

service(servid_001,b,c,d,e,f,_T).

service_spec(ssid_001, ss_body(x)).

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
% ms_cv/10 is the repository of configuration vectors
% ms_cv/4 was an abbreviated test
%
% CV = ms_cv(Mid,Sv,Ma,Mv,Mo,Mp,Mr,Mt,Mae,Svi)
%
:- dynamic ms_cv/4, ms_cv/10.

ms_cv(
             /* monitor_id */         'mid_00001',
             /* shared variables */   [a,b,c,s,t,u,v,w,x,y,z],
             /* monitor_atoms */      [a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],
             /* monitor_variables */  [s,t,u,v,w,x,y,z],
             /* monitor_observable_vars */ [u,v,w,x,y,z],
             /* monitor_property_vars */   [v,w,x,y,z],
             /* monitor_reportable_vars */ [v,w,x,y,z],
             /* monitor_trigger_vars */    [x],
             /* monitor_atom_eval */  ms_eval,
             /* SUS variable init */  [s=undefined,
                                       t=undefined,
                                       u=undefined,
                                       v=false,
                                       w=true,
                                       x=1,
                                       y=2,
                                       z=3
                                      ]
       ).

% JSON conversions for MS configuration vectors
%

:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).

% ms_cv/2 retrieves an ms_cv/10 record by monitor Id
ms_cv(Mid, ms_cv(Mid,Sv,Ma,Mv,Mo,Mp,Mr,Mt,Mae,Svi)) :-
        ms_cv(Mid,Sv,Ma,Mv,Mo,Mp,Mr,Mt,Mae,Svi), !.

ms_cv_to_pjson(CV,PJCV) :-
        CV =  ms_cv(Mid,Sv,Ma,Mv,Mo,Mp,Mr,Mt,Mae,Svi),
%        findall(json([struct=atm,aid=Ai,aex=Eaf]),
        findall(json([aid=Ai,aex=Eaf]),
                ( member(Ai:Af,Ma), encode_as_atoms(Af,Eaf)),
                JMa),
%        findall(json([struct=sus_var,name=Svn,value=Svv]),
        findall(json([name=Svn,value=Svv]),
                ( member(Svn=Svv,Svi) ),
                JSvi),
        PJCV = json([
                   monitor_id=Mid,
                   shared_variables=Sv,
                   monitor_atoms=JMa,
                   monitor_variables=Mv,
                   monitor_observable_vars=Mo,
                   monitor_property_vars=Mp,
                   monitor_reportable_vars=Mr,
                   monitor_trigger_vars=Mt,
                   monitor_atom_eval=Mae,
                   sus_variable_init=JSvi
               ]).

pjson_to_ms_cv(PJCV,CV) :-
        PJCV = json([
                   monitor_id=Mid,
                   shared_variables=Sv,
                   monitor_atoms=JMa,
                   monitor_variables=Mv,
                   monitor_observable_vars=Mo,
                   monitor_property_vars=Mp,
                   monitor_reportable_vars=Mr,
                   monitor_trigger_vars=Mt,
                   monitor_atom_eval=Mae,
                   sus_variable_init=JSvi
               ]),
        findall(Ai:Af,
%                ( member(json([struct=atm,aid=Ai,aex=Eaf]), JMa),
                ( member(json([aid=Ai,aex=Eaf]), JMa),
                  reconstitute_atoms(Eaf,Af) ),
                Ma
        ),
        findall(Svn=Svv,
%                ( member(json([struct=sus_var,name=Svn,value=Svv]), JSvi) ),
                ( member(json([name=Svn,value=Svv]), JSvi) ),
                Svi
        ),
        CV =  ms_cv(Mid,Sv,Ma,Mv,Mo,Mp,Mr,Mt,Mae,Svi).

% ms_cv/4 example:
ms_cv(
    monitor_id('mid_00001'),
    shared_variables([a,b,c]),
    monitor_atoms([
        atm(a1,eq(x,2)),
        atm(a2,lt(x,2))
    ]),
    sus_variable_init([])
).

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

pt(rmv_ml:lt(x,2)).
pt1( rmv_ml:atm(a,lt(x,2)) ).
pt2(rmv_ml:atm(a,b)).
pt3(atm(a,(rmv_ml:lt(x,2)))).

check_conversions :- Mid='mid_00001',
        ms_cv(Mid,MS_CV), format('ms_cv from library: ~q~n',MS_CV),
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
reconstitute_atoms(A,R) :- compound(A), !,
        compound_name_arguments(A,N,Args),
        reconstitute_atoms(N,RN),
        maplist(reconstitute_atoms,Args,RArgs),
        compound_name_arguments(R,RN,RArgs).

