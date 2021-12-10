% RMV - Monitor Sensor
% Runs as part of the Service Under Scrutiny (SUS)

:- module(rmv_ms,[ms_startup/0, ms_shutdown/0, ms_step/0
	       ]).

:- use_module('COM/param').
:- use_module('RMV/rmv_mf_mep').

% MONITOR SENSOR functions exposed to the SUS
%
% ms_startup is called by the SUS when it begins execution
ms_startup :- init.

% ms_shutdown is called by the SUS when it is shutting down
ms_shutdown :- shutdown.

% ms_step is called by SUS logic to explicitly trigger the responder
ms_step :- responder.

/*
% MS CONFIGURATION
%   provided by Monitor Creation to accompany the generated monitor
%   imported by initialize_ms_configuration
%
%   monitor_id - established when the Monitor Sensor is defined
%   shared_variables - all variables shared by SUS and MS
%   monitor_atoms - list of atoms {aid: , aex: } to be evaluated
%   monitor_variables - all potentially observable SUS variables in the model
%   monitor_observable_vars - all SUS variables used in properties or reportable
%   monitor_property_vars - all observable vars used in property evaluation
%   monitor_reportable_vars - subset of monitor_observables to be reported in heartbeat
%   monitor_trigger_vars - variables for which setter should trigger responder
%   monitor_atom_eval - where atoms are evaluated (ms_eval or mep_eval)
%   sus_variable_init - optional initializations list of {name: , value: }
*/

:- dynamic shared_variables/1, monitor_id/1, monitor_atoms/1, monitor_vars/1,
        monitor_observable_vars/1, monitor_property_vars/1, monitor_reportable_vars/1,
        monitor_trigger_vars/1, monitor_atom_eval/1, shared_vars_init/1.

shared_variables([]).

monitor_id('').
monitor_atoms([]).
monitor_vars([]).
monitor_observable_vars([]).
monitor_property_vars([]).
monitor_reportable_vars([]).
monitor_trigger_vars([]).
monitor_atom_eval(no_eval). % ms_eval, mep_eval or no_eval

% these configuration elements are unary functors, but most are ord sets
ms_config_elements([monitor_id, shared_vars,
                    monitor_atoms, monitor_vars, monitor_observable_vars,
                    monitor_property_vars, monitor_reportable_vars, monitor_trigger_vars,
                    monitor_atom_eval, shared_vars_init
                   ]).

% MS STARTUP/SHUTDOWN INITIALIZATION
%

:- dynamic ms_initialized/1.
ms_initialized(false).

% CONCRETE CONFIGURATION VECTOR
% These definitions are presented as examples and are used by the self tests
% It is also defined in the monitor library rmv_ml as ms_test_cv.

test_cv( ms_cv(
             /* monitor_id */         'Mid_00001',
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

% get_cv will import actual concrete MS configuration vector
% monitor_id, monitor_atoms, monitor_variables, monitor_observables,
% monitor_reportables, monitor triggers
%
% for now we use the test configuration vector
get_cv(CV) :- test_cv(CV).

% internal startup / shutdown functions
%
init :- ms_initialized(true), !. % already initialized
init :-
        initialize_ms_configuration,
	% initiate Monitor M the monitor server for this service
        monitor_id(Mid),
        mep_start_monitor(Mid,Mstatus),
        (   memberchk(monitor_started,Mstatus)
        ->  true
        ;   writeln('failed to initiate monitor'),
            fail
        ),
        retractall(ms_initialized(_)), assert(ms_initialized(true)),
        % return with success to indicate MS initialized and ready,
        true.

initialize_ms_configuration :-
        get_cv(CV),
        CV = ms_cv(Mid,SV,Ma,Mv,Mo,Mp,Mr,Mt,Mae,SVi),
        retractall(rmv_ml:monitor(Mid,_,_,_,_,_,_)),
        assert(rmv_ml:monitor(Mid,_,_,_,_,_,_)),

        clear_ms_configuration,
        set_ms_configuration([Mid,SV,Ma,Mv,Mo,Mp,Mr,Mt,Mae,SVi]),
        set_ms_sus_vars(SVi).

set_ms_configuration(CL) :-
        ms_config_elements(CE),
        maplist(set_ms_conf_elt,CE,CL).

set_ms_conf_elt(E,V) :- EV =.. [E,V], assert(EV), !.

set_ms_sus_vars([]).
set_ms_sus_vars([N=V|SVs]) :-
        assert( sus_var(N,V) ), set_ms_sus_vars(SVs).

clear_ms_configuration :-
        % clear all unary config elements
        ms_config_elements(CE),
        forall(member(F,CE), (R=..[F,_], retractall(R))),
        % clear all simulated SUS variables
        retractall(sus_var(_,_)),
        true.

shutdown :-
        monitor_id(Mid),
        mep_stop_monitor(Mid,Mstatus),
        (   memberchk(monitor_stopping,Mstatus)
        ->  true
        ;   writeln('error from mep_stop_monitor'),
            fail
        ),
        retractall(ms_initialized(_)), assert(ms_initialized(complete)).

re_init :- un_init, init.

un_init :-
        clear_ms_configuration,
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

% Note differences of these predicates to those in rmv_mf_mep
%   evaluate atoms for the monitor using values from o_getter map
%   Monitor configuration will have to make sure that all variables
%   needed for atom evaluation are in the Observables list
%
aT_list_constructor(As,ATs) :-
        findall(Ai, (member(Ai:Ap,As), af_evaluator(Ap)), ATs).

af_evaluator(Ap) :-
        aformula_instantiate(Ap,IAp),
        a_eval(IAp).

aformula_instantiate(AF,IAF) :- atom(AF), !, o_getter(AF,IAF).
aformula_instantiate(AF,IAF) :- compound(AF),
        compound_name_arguments(AF,F,Args),
        monitor_property_vars(PV),
        maplist(arg_instantiate(PV),Args,IArgs),
        compound_name_arguments(IAF,F,IArgs).

arg_instantiate(ObsNames,A,IA) :- atom(A), member(A,ObsNames), !, o_getter(A,IA).
arg_instantiate(_,A,A).

% basic set of atomic expressions
a_eval(true) :- !.
a_eval(not(X)) :- atom(X), !, X \== true.
a_eval(eq(X,Y)) :- number(X), number(Y), !, X=:=Y.
a_eval(eq(X,Y)) :- atom(X), atom(Y), !, X==Y.
a_eval(ne(X,Y)) :- number(X), number(Y), !, X=\=Y.
a_eval(ne(X,Y)) :- atom(X), atom(Y), !, X\==Y.
a_eval(neq(X,Y)) :- number(X), number(Y), !, X=\=Y.
a_eval(neq(X,Y)) :- atom(X), atom(Y), !, X\==Y.
a_eval(gt(X,Y)) :- number(X), number(Y), !, X>Y.
a_eval(lt(X,Y)) :- number(X), number(Y), !, X<Y.
a_eval(geq(X,Y)) :- number(X), number(Y), !, X>=Y.
a_eval(leq(X,Y)) :- number(X), number(Y), !, X=<Y.
a_eval(ge(X,Y)) :- number(X), number(Y), !, X>=Y.
a_eval(le(X,Y)) :- number(X), number(Y), !, X=<Y.
a_eval(_) :- !, fail.

% or_vector_constructor and or_list_constructor are alternatives
%
or_vector_constructor(ORv) :- % create an ordered vector of values
        (   monitor_reportable_vars(Rnames) -> true ; Rnames = []  ),
        maplist(o_getter,Rnames,Rvals),
        ORv =.. [or_v|Rvals],
        true.

or_list_constructor(ORl) :- % create a list of var=value pairs
        % monitor_observable_vars(Ovars),
        (   monitor_reportable_vars(Rnames) -> true ; Rnames = []  ),
        findall(V=Val, (member(V,Rnames), o_getter(V,Val)), ORl).


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
:- dynamic var_oldval_newval/3.

o_setter(Ovar,Onewval) :-
        atom(Ovar), atomic(Onewval), !,
        retractall(sus_var(Ovar,Ooldval)), assert(sus_var(Ovar,Onewval)),
        monitor_trigger_vars(Triggers),
        (   member(Ovar,Triggers)
        ->  % invoke ms_step if called from outside this module
            % temporarily store changed variable in case needed by atom evaluator
            assert( var_oldval_newval(Ovar,Ooldval,Onewval) ),
            responder,
            retractall( var_oldval_newval(_,_,_) )
        ;   true
        ).

o_getter(Ovar,Oval) :-
        atom(Ovar), var(Oval), sus_var(Ovar,Oval), !.
o_getter(_,undefined).


% simulation of the SUS variables
%
:- dynamic sus_var/2.
