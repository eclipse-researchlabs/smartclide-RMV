% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_start_monitor/2, mep_stop_monitor/3, mep_heartbeat/5
	       ]).

:- use_module('COM/param').
:- use_module('RMV/rmv_ml').
:- use_module('RMV/rmv_mc_nui').
%:- use_module('EPP/epp').
%:- use_module('EPP/eppapi').

% MONITOR EVENT PROCESSING
%

mep_start_monitor(Mid,Status) :-
    %rmv_mc_nui:start_monitor(Mid,Status), % TODO - currently only returns a status
    rmv_ml:is_monitor(Monitor,Mid,_,_,_,_,_,_),
    initiate_monitor(Monitor,SessId), % TODO - need SessId
	Status = [monitor_started,session(SessId)],
    true.

mep_stop_monitor(Mid,SessId,Status) :-
    %rmv_mc_nui:stop_monitor(Mid,Status), % TODO - currently only returns a status
    rmv_ml:is_monitor(_Monitor,Mid,_,_,_,_,_,_),
    terminate_monitor(SessId), % TODO - don't really need Monitor, just SessId
	Status = [monitor_stopping],
    true.

mep_heartbeat(Mid,Sid,AtomIds,Reportables,Response) :-
    monitor(Mid,_Mod,_Obs,_Reps,Atoms,AtomEval,_Sensor),
    (   ( /* AtomIds == [], */ AtomEval == mep_eval ) % monitor specifies mep evaluation
        % for mep evaluation, property variables must be a subset of reportables
    ->  aT_list_constructor(Atoms,Reportables,TAtomIdList) % ignore AtomIds from MS
    ;   TAtomIdList = AtomIds
    ),
    rmv_mc_nui:heartbeat(Mid,Sid,TAtomIdList,Verdict),
    notifications(report,Mid,Sid,Reportables,Verdict),
    (   (Verdict == true ; Verdict == unknown)
    ->  Response = [acknowledged,verdict(Verdict)]
    ;   (   Verdict == false
        ->  Response = [acknowledged,verdict(Verdict),recovery(true)]
        ;   Response = [exception(Verdict)],
            notifications(report,Mid,Sid,exception,Verdict)
        )
    ),
    true.

% temporary stub for notifications:
notifications(report,Mid,Sid,Reportables,Verdict) :-
    format('monitor report ~q:~a verdict: ~q, vars: ~q~n',[Mid,Sid,Verdict,Reportables]),
    true.

notifications(verdict,Mid,Sid,_,Verdict) :-
    format('monitor report ~q:~a verdict: ~q~n',[Mid,Sid,Verdict]),
    true.

%-------------------------------------------------------
% INITIATE/TERMINATE A RUNTIME MONITOR SESSION
%
initiate_monitor(M,SessId) :- is_monitor(M,MonitorId,ModelId,_,_,_,_,_),
    open_nurv_session(int,SessId),
	format('Monitor ID: ~a; NuRV session: ~a~n',[MonitorId,SessId]), flush_output,
	nurv_session_get_resp(SessId,''),
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
	atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
	nurv_monitor_init(SMVmodelFile,SMVordFile,SessId),
	true.

terminate_monitor(SessId) :-
	quit_nurv_session(SessId),
	writeln('session ended').

initiate_monitor2(M,SessId) :- is_monitor(M,_MonitorId,_ModelId,_,_,_,_,_),
    open_nurv_session(orbit,SessId),
	param:local_nameserver_IOR(IOR),
	atomic_list_concat(['monitor_server -N ',IOR],ServerCmd),
	nurv_session_cmd_resp(SessId,ServerCmd,_Resp),
	true.


:- dynamic test_vars/1.

test_setup :-
    rmv_ml:ms_cv(Mid,CV),
    CV = ms_cv(Mid,_SV,_Ma,_Mv,_Mo,_Mp,_Mr,_Mt,_Mae,SVi),
    %maplist(g,SVi,SVv),
    assert( test_vars(SVi) ),
    retractall( rmv_ms:sus_var(_,_) ),
    forall( member(N=V,SVi), assert( rmv_ms:sus_var(N,V) ) ).

g(sus_var(N,V), (N=V)).


% Note differences of these predicates to those in rmv_ms
%   evaluate atoms for the monitor using values from the mep_heartbeat
%   Monitor configuration will have to make sure that all observables
%   needed for atom evaluation are in the Reportables list
%
aT_list_constructor(As,Vars,ATs) :- % Vars is list of name=value pairs
    findall(Ai, (member(Ai:Ap,As), af_evaluator(Ap,Vars)), ATs).

af_evaluator(Ap,Vars) :-
    aformula_instantiate(Ap,Vars,IAp),
    a_eval(IAp).

aformula_instantiate(AF,Vars,IAF) :- atom(AF), memberchk(AF=IAF,Vars), !.
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
