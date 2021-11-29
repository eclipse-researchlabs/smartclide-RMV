% RMV - Monitor Sensor
% Runs as part of the Service Under Scrutiny (SUS)

:- module(rmv_ms,[ms_startup/0, ms_shutdown/0, ms_step/0
	       ]).

:- use_module('COM/param').
:- use_module('RMV/rmv_mf_mep').

% MS CONFIGURATION
%   provided by Monitor Creation to accompany the generated monitor
%   imported by initialize_ms_configuration
%
%   monitor_id - established when the Monitor Sensor is defined
%   monitor_atoms - list of atomID:atomExpr to be evaluated
%   monitor_variables - all potentially observable SUS variables in the model
%   monitor_observables - all SUS variables used in properties or reportable
%   monitor_reportables - subset of monitor_observables to be reported in heartbeat
%   monitor_triggers - variables for which setter should trigger responder
%
:- dynamic monitor_id/1, monitor_atoms/1, monitor_variables/1,
        monitor_observables/1, monitor_reportables/1, monitor_triggers/1.

monitor_id('').
monitor_atoms([]).
monitor_variables([]).
monitor_observables([]).
monitor_reportables([]).
monitor_triggers([]).

% MONITOR SENSOR functions exposed to the SUS
%
% ms_startup is called by the SUS when it begins execution
ms_startup :- init.

% ms_shutdown is called by the SUS when it is shutting down
ms_shutdown :- shutdown.

% ms_step is called by SUS logic to explicitly trigger the responder
ms_step :- responder.

% MS STARTUP/SHUTDOWN INITIALIZATION
%

:- dynamic ms_initialized/1.
ms_initialized(false).

% internal startup / shutdown functions
%
init :- ms_initialized(true), !. % already initialized
init :-
        initialize_ms_configuration,
	% initiate Monitor M the monitor server for this service
        monitor_id(Mid),
        mep_start_monitor(Mid,Mstatus),
        (   Mstatus \== success
        ->  writeln('failed to initiate monitor'),
            fail
        ;   true
        ),
        retractall(ms_initialized(_)), assert(ms_initialized(true)),
        % return with success to indicate MS initialized and ready,
        true.

initialize_ms_configuration :-
        % this operation will import the MS configuration definitions:
        % monitor_id, monitor_atoms, monitor_variables, monitor_observables,
        % monitor_reportables, monitor triggers

        % For now these ad hoc definitions appear here as example
        retractall(monitor_id(_)), assert(monitor_id('NuRV/Monitor/Mid_00001')),
        retractall(monitor_atoms(_)),
        assert(monitor_atoms([a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)])),
        retractall(monitor_variables(_)), assert(monitor_variables([s,t,u,v,w,x,y,z])),
        retractall(monitor_observables(_)), assert(monitor_observables([w,x,y,z])),
        retractall(monitor_reportables(_)), assert(monitor_reportables([x,z])),
        retractall(monitor_triggers(_)), assert(monitor_triggers([x])),
        assert(sus_var(s, undefined)),
        assert(sus_var(t, undefined)),
        assert(sus_var(u, undefined)),
        assert(sus_var(v, undefined)),
        assert(sus_var(w, undefined)),
        assert(sus_var(x, 1)),
        assert(sus_var(y, 2)),
        assert(sus_var(z, 3)),
        true.

shutdown :-
        monitor_id(Mid),
        mep_stop_monitor(Mid).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(sus_var(_,_)),
        retractall(ms_initialized(_)), assert(ms_initialized(false)).

% Monitor Sensor Responder
% triggered by SUS, usually by a Observable variable setter
%
responder :-
        monitor_id(Mid),
        monitor_atoms(As),
        aT_list_constructor(As,ATl),
        or_list_constructor(ORl),
        ms_heartbeat(Mid,ATl,ORl),
        true.

aT_list_constructor(As,ATs) :-
        findall(Ap, (member(Ap:Af,As),af_evaluator(Af)), ATs).

af_evaluator(Af) :-
        aformula_instantiate(Af,IAf),
        a_eval(IAf).

aformula_instantiate(AF,IAF) :-
        compound_name_arguments(AF,F,Args),
        monitor_observables(O),
        maplist(arg_instantiate(O),Args,IArgs),
        compound_name_arguments(IAF,F,IArgs).

arg_instantiate(Obs,A,IA) :- atom(A), member(A,Obs), !, o_getter(A,IA).
arg_instantiate(_,A,A).

% basic set of atomic expressions for now
a_eval(eq(X,Y)) :- number(X), number(Y), !, X=:=Y.
a_eval(eq(X,Y)) :- atom(X), atom(Y), !, X==Y.
a_eval(neq(X,Y)) :- number(X), number(Y), !, X=\=Y.
a_eval(neq(X,Y)) :- atom(X), atom(Y), !, X\==Y.
a_eval(gt(X,Y)) :- number(X), number(Y), !, X>Y.
a_eval(lt(X,Y)) :- number(X), number(Y), !, X<Y.
a_eval(geq(X,Y)) :- number(X), number(Y), !, X>=Y.
a_eval(leq(X,Y)) :- number(X), number(Y), !, X=<Y.

% or_vector_constructor and or_list_constructor are alternatives
%
or_vector_constructor(_ORv) :- % create an ordered vector of values
        true.

or_list_constructor(ORl) :- % create a list of var=value pairs
        % monitor_observables(Ovars),
        monitor_reportables(R),
        findall(V=Val, (member(V,R), o_getter(V,Val)), ORl).


% MS HEARTBEAT
%   ATlist is a list of the atom identifiers that evaluated to true in
%   the current observable variables assignment
%
%   ORlist is a list of name=value pairs for the reportable variables
ms_heartbeat(MonitorID,ATlist,ORlist) :-
        format('HEARTBEAT: ~w ~w ~w~n',[MonitorID,ATlist,ORlist]),
        mep_heartbeat(MonitorID,ATlist,ORlist,_Response).

% functions to set/get SUS variable values
%
o_setter(Ovar,Oval) :-
        atom(Ovar), atomic(Oval), !,
        retractall(sus_var(Ovar,_)), assert(sus_var(Ovar,Oval)),
        monitor_triggers(Triggers),
        (   member(Ovar,Triggers)
        ->  responder % invoke ms_step if called from outside this module
        ;   true
        ).

o_getter(Ovar,Oval) :-
        atom(Ovar), var(Oval), sus_var(Ovar,Oval), !.
o_getter(_,undefined).


% simulation of the SUS variables
%
:- dynamic sus_var/2.
