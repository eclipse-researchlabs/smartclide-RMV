% RMV - Monitor Sensor
% Runs as part of the Service Under Scrutiny (SUS)

:- module(rmv_ms,[ms_startup/0, ms_shutdown/0, ms_step/0, ms_recovery/1,
                sv_setter/2, sv_getter/2
	       ]).

:- use_module('COM/param').
:- use_module('RMV/rmv_mf_mep').
:- use_module(library(http/json)).

% MONITOR SENSOR functions exposed to the SUS
%
% ms_startup is called by the SUS when it begins execution
ms_startup :- init_ms.

% ms_shutdown is called by the SUS when it is shutting down
ms_shutdown :- shutdown.

% ms_step is called by SUS logic to explicitly trigger the responder
ms_step :- responder.

% ms_recovery registers a callback with MS for recovery
% the predicate is assumed to be of arity 1 to accommodate
% the recovery value returned by the monitoring framework
ms_recovery(Pred) :-
        retractall(sus_recovery_callback(_)), assert(sus_recovery_callback(Pred)).

sus_recovery(R) :-
        %format('recovery(~q) received by MS~n',R),
        sus_recovery_callback(Callback),
        (   Callback \== null
        ->  RecoveryCall =.. [Callback,R],
            call(RecoveryCall)
        ;   true
        ).

:- dynamic sus_recovery_callback/1.
sus_recovery_callback(null).

invoke_SUS_recovery(_).

/*
% MS CONFIGURATION
%   provided by Monitor Creation to accompany the generated monitor
%   imported by initialize_ms_configuration
%   See test_cv below for definition of ms_cv structure.
*/

:- dynamic monitor_id/1, shared_var_decls/1,
        observable_vars/1, model_vars/1,
        property_vars/1, reportable_vars/1,
        trigger_vars/1, monitor_atoms/1,
        monitor_atom_eval/1, shared_var_inits/1,
        monitor_session/1.

monitor_id('monid_00002'). % TODO somehow must be set before init
shared_var_decls([]).
observable_vars([]).
model_vars([]).
property_vars([]).
reportable_vars([]).
trigger_vars([]).
monitor_atoms([]).
monitor_atom_eval(no_eval). % ms_eval, mep_eval or no_eval

monitor_session('').

% these configuration elements are unary functors, but most are ord sets
ms_config_elements([monitor_id, shared_var_decl,
                    observable_vars, model_vars,
                    property_vars, reportable_vars,
                    trigger_vars, monitor_atoms,
                    monitor_atom_eval, shared_vars_init ]).

% MS STARTUP/SHUTDOWN INITIALIZATION
%

:- dynamic ms_initialized/1.
ms_initialized(false).

% CONCRETE CONFIGURATION VECTOR
% These definitions are presented as examples and are used by the self tests
% It is also defined in the monitor library rmv_ml as ms_test_cv.

test_cv(1, ms_cv( % old format
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

test_cv(2, ms_cv( % new format - identical to ex_cv(2, CV) in rmv_ml
             /* monitor_id */         'monid_00002',
             /* shared_var_decls */   [n:integer,
                                       o:integer,
                                       p:boolean,
                                       q:boolean,
                                       r:float,
                                       s:float],
             /* observable_vars */    [n, o, p, q, r, s],
             /* model_vars */         [n, p, q, s],
             /* property_vars */      [n, p, q],
             /* reportable_vars */    [n, o, s, p, q],
             /* trigger_vars */       [q, s],
             /* monitor_atoms */      [p:p,a1:eq(n,2),a2:lt(n,2),a3:eq(p,q),q:q],
             /* monitor_atom_eval */  ms_eval,
             /* shared_var_inits */   [n=1,
                                       o=2,
                                       p=true,
                                       q=false,
                                       r=undefined,
                                       s=1
                                      ]
         )
       ).

% get_cv will import actual concrete MS configuration vector
% or use the test configuration vector
%
%get_cv(CV) :- test_cv(1,CV).
get_cv(CV) :- !, get_cv(CV,'monid_00002'). % default
get_cv(CV) :- test_cv(2,CV).
get_cv(CV,N) :- integer(N), !, test_cv(N,CV).
get_cv(CV,Mid) :- atom(Mid), !,
        param:monitor_directory_name(MD),
        atomic_list_concat([MD,'/',Mid,'_conf.json'], CF),
        open(CF,read,Stream),
        json_read(Stream,JSONterm),
        %writeln(JSONterm),
        rmv_ml:pjson_to_ms_cv(JSONterm,CV),
        %format('\n~q\n',CV),
        true.

% internal startup / shutdown functions
%
init_ms :- ms_initialized(true), !. % already initialized
init_ms :-
        initialize_ms_configuration,
	% initiate Monitor M the monitor server for this service
        monitor_id(Mid),
        mep_start_monitor(Mid,Mstatus), % TODO - call through mepapi
        (   memberchk(monitor_started,Mstatus)
        ->  true
        ;   writeln('failed to initiate monitor'),
            fail
        ),
        (   memberchk(session(Sid),Mstatus)
        ->  retractall(monitor_session(_)),
            assert(monitor_session(Sid))
        ;   writeln('mep_start_monitor did not return a session id'),
            fail
        ),
        retractall(ms_initialized(_)), assert(ms_initialized(true)),
        % return with success to indicate MS initialized and ready,
        true.

initialize_ms_configuration :-
        (       ( monitor_id(Mid), Mid \== '' )
        ->      get_cv(CV,Mid)
        ;       get_cv(CV)
        ),
        initialize_ms_configuration(CV).

initialize_ms_configuration(CV) :-
        %writeln(initializing), rmv_ml:display_cv(CV),
        CV = ms_cv(Monid,SVD,Ov,Mv,Pv,Rv,Tv,Ma,Mae,SVi),
        clear_ms_configuration,
        set_ms_configuration([Monid,SVD,Ov,Mv,Pv,Rv,Tv,Ma,Mae,SVi]),
        declare_sus_vars(SVD),
        set_ms_sus_vars(SVi).

set_ms_configuration(CL) :-
        ms_config_elements(CE),
        maplist(set_ms_conf_elt,CE,CL),
        % handle the overrides recorded in rmv_ml
        % currently only the atom evaluation mode
        % these are only for testing as the monitor library
        % is not available in the standalone.
        % If this is to be permitted dynamically then another
        % way of getting the override into the monitor sensor
        % must be devised. Probably direct modification of
        % the configuration vector before MS initialization.
        (		rmv_ml:atom_eval_mode(Emode)
        ->	retractall( monitor_atom_eval(_) ),
				assert( monitor_atom_eval(Emode) )
        ;   true
        ).

set_ms_conf_elt(E,V) :- EV =.. [E,V], assert(EV), !.

declare_sus_vars([]) :- !.
declare_sus_vars([Decl|Decls]) :- !,
        declare_sus_var(Decl), declare_sus_vars(Decls).

declare_sus_var(SVname:SVtype) :-
        assert( sus_var(SVname,SVtype,undefined) ).

set_ms_sus_vars([]).
set_ms_sus_vars([N=V|SVs]) :-
        set_ms_sus_var(N,V), set_ms_sus_vars(SVs).

set_ms_sus_var(N,V) :-
        (   sus_var(N,T,_)
        ->  retractall( sus_var(N,_,_) )
        ;   T = undeclared
        ),
        assert( sus_var(N,T,V) ).

clear_ms_configuration :-
        % clear all unary config elements
        ms_config_elements(CE),
        forall(member(F,CE), (R=..[F,_], retractall(R))),
        % clear all simulated SUS variables
         retractall(sus_var(_,_,_)),
        true.

shutdown :-
        monitor_id(Mid),
        monitor_session(Sid),
        mep_stop_monitor(Mid,Sid,Mstatus), % TODO - call through mepapi
        (   memberchk(monitor_stopping,Mstatus)
        ->  true
        ;   writeln('error from mep_stop_monitor'),
            fail
        ),
        retractall(ms_initialized(_)), assert(ms_initialized(complete)),
        rmv_mc_nui:display_session_log(Sid).

re_init :- un_init, init.

un_init :-
        clear_ms_configuration,
        retractall(ms_initialized(_)), assert(ms_initialized(false)).

% Monitor Sensor Responder
% triggered by SUS, usually by an Observable variable sv_setter
% most recently changed trigger var old and new values in var_oldval_newval/3

responder :-
        monitor_id(Mid),
        monitor_session(Sid),
        monitor_atoms(As),
        aT_list_constructor(As,ATl),
        or_list_constructor(ORl),
        ms_heartbeat(Mid,Sid,ATl,ORl),
        true.

% Note differences of these predicates to those in rmv_mf_mep
%   evaluate atoms for the monitor using values from sv_getter map
%   Monitor configuration will have to make sure that all variables
%   needed for atom evaluation are in the Observables list
%
% TODO - use var_oldval_newval/3 for reference to past values by var
%
aT_list_constructor(As,ATs) :-
        findall(Ai, (member(Ai:Ap,As), af_evaluator(Ap)), ATs).

af_evaluator(Ap) :-
        aformula_instantiate(Ap,IAp),
        a_eval(IAp).

aformula_instantiate(AF,IAF) :- atom(AF), !, sv_getter(AF,IAF).
aformula_instantiate(AF,IAF) :- compound(AF),
        compound_name_arguments(AF,F,Args),
        property_vars(PV),
        maplist(arg_instantiate(PV),Args,IArgs),
        compound_name_arguments(IAF,F,IArgs).

arg_instantiate(ObsNames,A,IA) :- atom(A), member(A,ObsNames), !, sv_getter(A,IA).
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
        (   reportable_vars(Rnames) -> true ; Rnames = []  ),
        maplist(sv_getter,Rnames,Rvals),
        ORv =.. [or_v|Rvals],
        true.

or_list_constructor(ORl) :- % create a list of var=value pairs
        % observable_vars(Ovars),
        (   reportable_vars(Rnames) -> true ; Rnames = []  ),
        findall(V=Val, (member(V,Rnames), sv_getter(V,Val)), ORl).


% MS HEARTBEAT
%   Construct and send the MS heartbeat message to the MEP
%
%   ATlist is a list of the atom identifiers that evaluated to true in
%   the current observable variables assignment
%
%   ORlist is a list of name=value pairs for the reportable variables
ms_heartbeat(MonitorID,SessionID,ATlist,ORlist) :-
%        format(atom(H),'MS-HEARTBEAT: ~w ~w ~w~n',[MonitorID,ATlist,ORlist]),
%        write(H),
        send_ms_heartbeat(MonitorID,SessionID,ATlist,ORlist,Response),
        (   memberchk(recovery(R),Response)
        ->  sus_recovery(R)
        ;   true
        ),
        true.

send_ms_heartbeat(Mid,Sid,ATl,ORl,Resp) :-
        % synchronously send heartbeat and wait for response
        % for standalone test bypass mepapi and send direct to MEP internal
        % normally would serialize the args and send via mepapi
        rmv_mf_mep:mep_heartbeat(Mid,Sid,ATl,ORl,Resp),
        true.

% :- json_object ms_heartbeat(monitor:integer, session:string,
% atoms:string, vars:string).
% TODO
% construct_ms_heartbeat_JSON(Mid,Sid,Atl,ORl,HB_JSON) :- true.


% functions to set/get SUS variable values
%
% TODO - expand use of var_oldval_newval to all property vars always?
% TODO - move manipulation of var_oldval_newval outside triggers below
% ΤΟDO - extend sus_var/4 to include pastval (may be tricky in C version)
% TODO - then retract only clause for current var before assert
:- dynamic var_oldval_newval/3. % cache old values

% TODO - edit local var names Ovar -> SVname, etc
sv_setter(Ovar,Onewval) :-
        atom(Ovar), atomic(Onewval), !,
        sus_var(Ovar,Otype,Ooldval),
        retractall(sus_var(Ovar,_,_)), assert(sus_var(Ovar,Otype,Onewval)),
        trigger_vars(Triggers),
        (   member(Ovar,Triggers)
        ->  % invoke ms_step if called from outside this module - TODO
            % temporarily store changed variable in case needed by atom evaluator
            retractall( var_oldval_newval(Ovar,_,_) ),
            assert( var_oldval_newval(Ovar,Ooldval,Onewval) ),
            responder
            % retractall( var_oldval_newval(_,_,_) )
        ;   true
        ).

sv_getter(Ovar,Oval) :-
        atom(Ovar), var(Oval), sus_var(Ovar,_Otype,Oval), !.
sv_getter(_,undefined).


% simulation of the SUS variables
%
:- dynamic sus_var/2, sus_var/3.

sus_var(N,V) :- sus_var(N,_,V), !.
