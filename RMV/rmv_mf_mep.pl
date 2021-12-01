% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_start_monitor/2, mep_stop_monitor/2, mep_heartbeat/4
	       ]).

:- use_module('COM/param').
:- use_module('RMV/rmv_ml').
:- use_module('RMV/rmv_mc_nui').
%:- use_module('EPP/epp').
%:- use_module('EPP/eppapi').

% MONITOR EVENT PROCESSING
%

mep_start_monitor(Mid,Status) :-
    rmv_mc_nui:start_monitor(Mid,Status),
    true.

mep_stop_monitor(Mid,Status) :-
    rmv_mc_nui:stop_monitor(Mid,Status),
    true.

mep_heartbeat(Mid,AtomIds,Reportables,Response) :-
    monitor(Mid,_Mod,_Obs,_Reps,Atoms,AtomEval,_Sensor),
    (   ( /* AtomIds == [], */ AtomEval == mep ) % monior specifies mep evaluation
    ->  aT_list_constructor(Atoms,Reportables,TAtomIdList) % ignore AtomIds from MS
    ;   TAtomIdList = AtomIds
    ),
    rmv_mc_nui:heartbeat(Mid,TAtomIdList,Verdict),
    notifications(verdicts,Mid,Reportables,Verdict),
    (   (Verdict == true ; Verdict == inconclusive)
    ->  Response = [acknowledged,verdict=Verdict,recover=false]
    ;   Response = [acknowledged,verdict=Verdict,recover=true],
        notifications(verdicts,Mid,exception,Verdict)
    ),
    true.

% temporary stub:
notifications(verdicts,_,_,_) :- writeln('verdicts notification').

% Note differences of these predicates to those in rmv_ms
%   evaluate atoms for the monitor using values from the mep_heartbeat
%   Monitor configuration will have to make sure that all observables
%   needed for atom evaluation are in the Reportables list
%
aT_list_constructor(As,Vars,ATs) :-
    findall(Ai, (member(Ai:Ap,As), af_evaluator(Ap,Vars)), ATs).

af_evaluator(Ap,Vars) :-
    aformula_instantiate(Ap,Vars,IAp),
    a_eval(IAp).

aformula_instantiate(AF,Vars,IAF) :- atom(AF), memberchk(AF:IAF,Vars), !.
aformula_instantiate(AF,Vars,IAF) :- compound(AF),
    compound_name_arguments(AF,F,Args),
    maplist(arg_instantiate(Vars),Args,IArgs),
    compound_name_arguments(IAF,F,IArgs).

arg_instantiate(ObsVals,A,IA) :- atom(A), memberchk(A=IA,ObsVals), !.
arg_instantiate(_,A,A).

% basic set of atomic expressions
a_eval(true) :- !.
a_eval(not(X)) :- atom(X), !, X \== true.
a_eval(eq(X,Y)) :- number(X), number(Y), !, X=:=Y.
a_eval(eq(X,Y)) :- atom(X), atom(Y), !, X==Y.
a_eval(neq(X,Y)) :- number(X), number(Y), !, X=\=Y.
a_eval(neq(X,Y)) :- atom(X), atom(Y), !, X\==Y.
a_eval(gt(X,Y)) :- number(X), number(Y), !, X>Y.
a_eval(lt(X,Y)) :- number(X), number(Y), !, X<Y.
a_eval(geq(X,Y)) :- number(X), number(Y), !, X>=Y.
a_eval(leq(X,Y)) :- number(X), number(Y), !, X=<Y.
a_eval(_) :- !, fail.
