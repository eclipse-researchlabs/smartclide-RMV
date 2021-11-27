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
                  load_service_specification_immediate/2, load_service_specification/2,
                  unload_service_specification/1,
                  load_monitor/1, unload_monitor/1, truncate_trace/2
	       ]).

:- use_module('COM/param').
:- use_module([rmv_ml_mt,rmv_ml_pst]).

% structures stored in the monitor library
%
:- dynamic app/7, model/2, monitor/7, property/2, service/7, service_spec/2, trc/1.

%
% types checkers / constructors
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

% ids
ssid_modid(SSpecId,ModelId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(modid_,N,ModelId).

ssid_propid(SSpecId,PropId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(modid_,N,PropId).

ssid_scripid(SSpecId,ScripId) :-
    atom_concat(ssid_,N,SSpecId), atom_concat(scripid_,N,ScripId).

modid_monid(ModelId,MonitorId) :-
        atom_concat(modid_,N,ModelId), atom_concat(monid_,N,MonitorId).

monitorid_nurvid(Mid,NuRVid) :- Mid = NuRVid. % define if necessary

%
% some built-in samples:
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

%
% Service Specifications
%   service_spec( SpecId, SpecBody )
%

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

load_monitor(Mon) :-
        Mon = monitor(MonId,_,_,_,_,_,_),
        retractall(monitor(MonId,_,_,_,_,_,_)),
        assert(Mon),
        true.

unload_monitor(MonId) :-
        retractall(monitor(MonId,_,_,_,_,_,_)).

unpack_sspec(_Sbody).

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
