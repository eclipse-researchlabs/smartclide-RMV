% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_monitor_start/2, mep_monitor_stop/2,
    mep_heartbeat/2, json_var_val/2
	       ]).

:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).
    
:- use_module('COM/param').
:- use_module('COM/sessions').
:- use_module('RMV/rmv_ml').
:- use_module('RMV/rmv_mc_nui').
:- use_module('EPP/epp').
%:- use_module('EPP/eppapi').

% MONITOR EVENT PROCESSING
%
/* TODO - figure out how to get this multifile/discontig to work across modules
% ------------------------------------------------------------------------
% Isolated RMV changes to be moved to RMV-specific module rmv_mf_mep
:- multifile(epp_era:report_event/2).
:- discontiguous(epp_era:report_event/2).
:- multifile(epp_era:report_event/3).
:- discontiguous(epp_era:report_event/3).
:- multifile(epp_era:event_name/1).
:- discontiguous(epp_era:event_name/1).
:- multifile(epp_era:name_event_map/2).
:- discontiguous(epp_era:name_event_map/2).

% :- multifile(report_event/2).
% :- discontiguous(report_event/2).
% :- multifile(report_event/3).
% :- discontiguous(report_event/3).
% :- multifile(event_name/1).
% :- discontiguous(event_name/1).
% :- multifile(name_event_map/2).
% :- discontiguous(name_event_map/2).

event_name(test_ms_event).

ms_event(ms_event(_MSmessage)) :- !.

name_event_map(test_ms_event,  ms_event('MS event data')).

handle_ms_event(PT,Reply) :-
    epp_log_gen(handle_ms_event,PT),

    Reply = normal,
    true.

report_event(Event, Reply) :- ms_event(Event), !,
    Event = ms_event(MSmessage), \+ var(MSmessage), !,
    epp_log_gen(report_ms_event,Event),
    term_to_atom(MSmessage,JTA),
    atom_json_term(JTA,JT,[]),
    handle_ms_event(JT,Reply),
    true.

% ------------------------------------------------------------------------
*/

mep_monitor_start(Mid,Status) :-
    %epp_log_gen(monitor_event_processing, monitor_start),
    %rmv_mc_nui:start_monitor(Mid,Status), % TODO - currently only returns a status
    monitor(Mid,Monitor), !,
    initiate_monitor(Monitor,SessId),
	Status = [monitor_started,session(SessId)],
    true.
mep_monitor_start(Mid,Status) :-
    Status = [monitor_not_found(Mid)].

mep_monitor_stop(SessId,Status) :-
    % (   number(SessId)
    % ->  atom_number(SessIdA,SessId)
    % ;   SessIdA = SessId
    % ),
    % %rmv_mc_nui:stop_monitor(Mid,Status), % TODO - currently only returns a status
    terminate_monitor(SessId), % TODO - don't really need MonitorId, just SessId
	Status = [monitor_stopped,session(SessId)],
    true.

mep_heartbeat(HBterm,Status) :-
    /* HBterm = ms_event(MSmessage),*/
    \+ var(HBterm), !,
    % unpack the HBterm
    term_to_atom(HBterm,JA),
    atom_json_term(JA,JT,[]),
    % handle the heartbeat message in JT
    JT = json([monid=Monid, sessid=Sessid, atoms=ATl, vars=JVAl]),
    is_list(ATl), is_list(JVAl),
    maplist(json_var_val, JVAl, VAl),
    monitor_atoms_eval(Monid,Atoms,Eval),

    mep_heartbeat(Eval,Monid,Sessid,Atoms,ATl,VAl,Status).
    %format(atom(A),'monid=~a, sessid=~a, eval=~a, status=~q', [Monid,Sessid,Eval,Status]),
    %epp_log_gen(monitor_event_processing,mep_heartbeat(A)).


json_var_val( json([Var='@'(true)]), Var=true ) :- !.
json_var_val( json([Var='@'(false)]), Var=false ) :- !.
json_var_val( json([Var=Val]), Var=Val ).


mep_heartbeat(mep_eval,Mid,Sid,Atoms,_AtomIds,Reportables,Status) :- !,
    aT_list_constructor(Atoms,Reportables,AtomIdsMEP), % ignore AtomIds from MS heartbeat
    rmv_mc_nui:heartbeat(Mid,Sid,AtomIdsMEP,Verdict), % call NuRV property monitor
    hb_verdict_status(Verdict,Sid,AtomIdsMEP,Reportables,Status).
    % format(atom(A),'monid=~a, sessid=~a, eval=~a, atoms=~q, vars=~q, status=~q',
    %     [Mid,Sid,mep_eval,AtomIdsMEP,Reportables,Status]),
    % epp_log_gen('mep_heartbeat:',A).

mep_heartbeat(ms_eval,Mid,Sid,_Atoms,AtomIds,Reportables,Status) :- !,
    rmv_mc_nui:heartbeat(Mid,Sid,AtomIds,Verdict), % call NuRV property monitor
    hb_verdict_status(Verdict,Sid,AtomIds,Reportables,Status).

mep_heartbeat(no_eval,_,Sid,_,_,Reportables,Status) :- !,
    hb_verdict_status(no_eval,Sid,[],Reportables,Status).

mep_heartbeat(_,_,Sid,_,_,Reportables,Status) :- !,
    hb_verdict_status(unset_eval,Sid,[],Reportables,Status).


hb_verdict_status(error,Sid,AtomIds,Reportables,Status) :- !,
    Status = [exception,session(Sid),verdict(error),basis(AtomIds,Reportables)].

hb_verdict_status(Verdict,Sid,AtomIds,Reportables,Status) :- !,
    Status = [acknowledged,session(Sid),verdict(Verdict),basis(AtomIds,Reportables)].

/*
mep_heartbeat(Mid,Sid,no_eval,_AtomIds,Reportables,Status) :- !, % no_eval case, reports only
    Status = [acknowledged,session(Sid),basis(no_eval,Reportables)],
    notifications(monitor_report,Mid,Sid,Reportables,no_eval),
    true.

mep_heartbeat(Mid,Sid,Eval,AtomIds,Reportables,Status) :-
    %monitor(Mid,Monitor),
    %Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSid, MScv, MSfile ),
    %monitor(Mid,_Mod,_Obs,_Reps,Atoms,AtomEval,_SensorVers),
    (   Eval == mep_eval % monitor specifies mep evaluation
    ->  aT_list_constructor(Atoms,Reportables,TAtomIdList) % ignore AtomIds from MS heartbeat
        % for mep evaluation, property variables must be a subset of reportables
    ;   TAtomIdList = AtomIds
    ),
    %epp_log_gen('mep_heartbeat/5, true atoms:',TAtomIdList),
    (   Eval == no_eval
    ->  Verdict = no_eval
    ;   rmv_mc_nui:heartbeat(Mid,Sid,TAtomIdList,Verdict) % call NuRV property monitor
    ),
    % basis is temporarily included in the Status for round-trip check - TODO
    (   (Verdict == true ; Verdict == unknown)
    ->  Status = [acknowledged,session(Sid),verdict(Verdict),basis(TAtomIdList,Reportables)],
        notifications(monitor_report,Mid,Sid,Reportables,Verdict)
    ;   (   Verdict == false
        ->  Status = [acknowledged,session(Sid),verdict(Verdict),basis(TAtomIdList,Reportables),recovery(true)],
            notifications(monitor_report,Mid,Sid,Reportables,Verdict)
        ;   Status = [session(Sid),exception(Verdict),basis(TAtomIdList,Reportables)],
            notifications(monitor_report,Mid,Sid,exception,Verdict)
        )
    ),
    true.
*/
% temporary stub for notifications:
notifications(monitor_report,_Mid,Sid,Reportables,Verdict) :-
    epp_log_gen(monitor_event_processing, notification(monitor_report,Sid,Reportables,Verdict)),
    %format('notification: monitor report ~q:~a verdict: ~q, vars: ~q~n',[Mid,Sid,Verdict,Reportables]),
    true.

notifications(monitor_verdict,Mid,Sid,Reportables,Verdict) :-
    notifications(monitor_report,Mid,Sid,Reportables,_),
    epp_log_gen(monitor_event_processing, notification(monitor_verdict,Sid,Verdict)),
    %format('notification: monitor report ~q:~a verdict: ~q~n',[Mid,Sid,Verdict]),
    true.

%-------------------------------------------------------
% INITIATE/TERMINATE A RUNTIME MONITOR SESSION
%
initiate_monitor(M,SessId) :-
    cons_monitor(MonitorId,_SSpecId,ModelId,_,_,_,MScv,_,_,M),
    MScv=ms_cv(MonitorId,_Vdecl,_Vo,_Vm,_Vp,_Vr,_Vt,_Atoms,AEval,_Vinit,_Beh,_Timer,_Host,_Port),

    (   AEval == no_eval
    ->  % this is a RMV-only session, no NuRV session
        % the MonitorId alone is not a sufficient SessId because there can be multiple instances
        % so a unique id is created here
        param:rmv_monitor_id_prefix(MonIdPref), param:rmv_session_id_prefix(SessIdPref),
        atom_concat(MonIdPref,MonIdUniq,MonitorId), % extract unique part of Monitor ID
        uuid(U), % make unique part of Session ID
        atomic_list_concat([SessIdPref,MonIdUniq, '_', U], SessId)
    ;   % the RMV session includes a NuRV session
        open_nurv_session(int,NuRVSessId,MonitorId),
        %format('Monitor ID: ~a; NuRV session: ~a~n',[MonitorId,SessId]), flush_output,
        nurv_session_get_resp(NuRVSessId,''),
        param:monitor_directory_name(MD),
        atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
        atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
        nurv_monitor_init(SMVmodelFile,SMVordFile,NuRVSessId),
        atomic_list_concat([MonitorId, '_', NuRVSessId], SessId)
    ),
    init_session(SessId, monitor_framework),
	true.

terminate_monitor(SessId) :-
    param:rmv_session_id_prefix(SessIdPref), atom_concat(SIP,'_',SessIdPref),
	atomic_list_concat([SIP,_UMid,USid], '_', SessId),
    (   rmv_mc_nui:nurv_session(USid,_,_,_,_)
    ->  quit_nurv_session(USid)
    ;   true
    ),
    ( is_session(SessId, monitor_framework) -> end_session(SessId) ; true ),
	%writeln('session ended'),
    true.

% nameserver version
initiate_monitor2(M,SessId) :-
    cons_monitor(MonitorId,SessId,_,_,_,_,_,_,_,M),
    open_nurv_session(orbit,NuRVSessId,MonitorId),
	param:local_nameserver_IOR(IOR),
	atomic_list_concat(['monitor_server -N ',IOR],ServerCmd),
	nurv_session_cmd_resp(NuRVSessId,ServerCmd,_Resp),
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
% The two implementations may be unifiabe so that there is one module
% included in the MS and in the MEP.
%
aT_list_constructor(As,Vars,ATs) :- % Vars is list of name=value pairs
    epp_log_gen('entered aT_list_constructor/3',''),
    %writeln('mep_eval aT_list_constructor'),
    findall(Ai, (member(Ai:Ap,As), af_evaluate(Ai:Ap,Vars)), ATs).

af_evaluate(_Ai:Ap,Vars) :-
    aformula_instantiate(Ap,Vars,IAp), % a_eval(IAp).
%    format(' atom ~a:~q evaluated ',[Ai,IAp]),
    a_eval(IAp,R),
%    writeln(R),
    (R \== true -> fail; true).

varname(V) :- atom(V), V \== true, V \== false, V \== null, V \== undefined, !.  % could be more specific

aformula_instantiate(true,_,true) :- !.
aformula_instantiate(false,_,false) :- !.
aformula_instantiate(null,_,null) :- !.
aformula_instantiate(undefined,_,undefined) :- !.
aformula_instantiate(AF,Vars,IAF) :-
    varname(AF), !,
    arg_instantiate(Vars,AF,IAF).
aformula_instantiate(AF,Vars,IAF) :- compound(AF),
    compound_name_arguments(AF,F,Args),
    maplist(arg_instantiate(Vars),Args,IArgs),
    compound_name_arguments(IAF,F,IArgs).
aformula_instantiate(AF,AF).

arg_instantiate(VarVals,A,IA) :- varname(A), !,
    (   memberchk(A=IA,VarVals)
    ->  true
    ;   IA = undefined
    ).
arg_instantiate(_,A,A).

% basic set of atomic expressions
/* a_eval(true) :- !.
a_eval(false) :- !, fail.
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
 */
a_eval(true,    true) :- !.
a_eval(false,   false) :- !.
a_eval(not(X),  R) :- atom(X), !, (X \== true -> R=true;R=false).
a_eval(eq(X,Y), R) :- number(X), number(Y), !, (X=:=Y -> R=true; R=false).
a_eval(eq(X,Y), R) :- atom(X), atom(Y), !, (X==Y -> R=true; R=false).
a_eval(ne(X,Y), R) :- number(X), number(Y), !, (X=\=Y -> R=true; R=false).
a_eval(ne(X,Y), R) :- atom(X), atom(Y), !, (X\==Y -> R=true; R=false).
a_eval(neq(X,Y), R) :- number(X), number(Y), !, (X=\=Y -> R=true; R=false).
a_eval(neq(X,Y), R) :- atom(X), atom(Y), !, (X\==Y -> R=true; R=false).
a_eval(gt(X,Y), R) :- number(X), number(Y), !, (X>Y -> R=true; R=false).
a_eval(lt(X,Y), R) :- number(X), number(Y), !, (X<Y -> R=true; R=false).
a_eval(geq(X,Y), R) :- number(X), number(Y), !, (X>=Y -> R=true; R=false).
a_eval(leq(X,Y), R) :- number(X), number(Y), !, (X=<Y -> R=true; R=false).
a_eval(ge(X,Y), R) :- number(X), number(Y), !, (X>=Y -> R=true; R=false).
a_eval(le(X,Y), R) :- number(X), number(Y), !, (X=<Y -> R=true; R=false).
a_eval(_, undefined) :- !.
