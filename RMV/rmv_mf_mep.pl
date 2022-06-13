% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_monitor_start/2, mep_monitor_stop/2,
    mep_heartbeat/3, json_var_val/2
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

% mep_monitor_start(+Mid,-Status)
mep_monitor_start(Mid,Status) :-
    %writeln(user_error,mep_monitor_start(Mid)), flush_output(user_error),
    monitor(Mid,Monitor), !,
    initiate_monitor(Monitor,SessId),
	Status = [monitor_started,session(SessId)],
    epp_log_gen(monitor_event_processing, monitor_start(success,Status)),
    %monid_sessid_muniq_suniq(_,SessId,_,NSid), dump_nu_lines_nsid(NSid),
    true.
mep_monitor_start(Mid,Status) :-
    %writeln(user_error,mep_monitor_start(Mid)),
    Status = [monitor_not_found(Mid)],
    epp_log_gen(monitor_event_processing, monitor_start(failure,Status)),
    % dump_nu_lines(SessId),
    true.


% mep_monitor_stop(+SessId,-Status)
mep_monitor_stop(SessId,Status) :-
    %writeln(user_error,mep_monitor_stop(SessId)), flush_output(user_error),
    terminate_monitor(SessId), !, 
	Status = [monitor_stopped,session(SessId)],
    epp_log_gen(monitor_event_processing, monitor_stop(success,Status)),
    %monid_sessid_muniq_suniq(_,SessId,_,NSid), dump_nu_lines_nsid(NSid),
    true.
mep_monitor_stop(SessId,[unexpected_failure(SessId)]) :-
    epp_log_gen(monitor_event_processing, monitor_stop(failure,unexpected_failure(SessId))),
    %monid_sessid_muniq_suniq(_,SessId,_,NSid), dump_nu_lines_nsid(NSid),
    true.


% mep_heartbeat/3
mep_heartbeat(Sid,HBterm,Status) :-
    %writeln(user_error,mep_heartbeat(HBterm)), flush_output(user_error),
    /* HBterm = ms_event(MSmessage),*/
    \+ var(HBterm), !,
    % unpack the HBterm
    term_to_atom(HBterm,JA),
    atom_json_term(JA,JT,[]),
    % handle the heartbeat message in JT
    JT = json([monid=Monid, sessid=Sessid, atoms=ATl, vars=JVAl]),
    atom(Monid), Sid==Sessid, is_list(ATl), is_list(JVAl),
    maplist(json_var_val, JVAl, VAl),
    monitor_atoms_eval(Monid,Atoms,Eval),

    mep_heartbeat(Eval,Monid,Sessid,Atoms,ATl,VAl,Status),
    epp_log_gen(monitor_event_processing, heartbeat(success,Status)),
    notifications(monitor_report,Sessid,Status),
    %monid_sessid_muniq_suniq(_,Sessid,_,NSid), dump_nu_lines_nsid(NSid),
    true.
mep_heartbeat(Sid,HBterm,Status) :-
    Status = [failure,mep_heartbeat(Sid,HBterm)],
    epp_log_gen(monitor_event_processing, heartbeat(failure,Sid,HBterm)),
    notifications(monitor_report,Sid,Status),
    %monid_sessid_muniq_suniq(_,Sid,_,NSid), dump_nu_lines_nsid(NSid),
    true.


% mep_heartbeat/7
mep_heartbeat(mep_eval,Mid,Sid,Atoms,_AtomIds,Reportables,Status) :- !,
    aT_list_constructor(Atoms,Reportables,AtomIdsMEP), % ignore AtomIds from MS heartbeat
    rmv_mc_nui:heartbeat(Mid,Sid,AtomIdsMEP,Verdict), % call NuRV property monitor
    hb_verdict_status(Verdict,Sid,AtomIdsMEP,Reportables,Status).

mep_heartbeat(ms_eval,Mid,Sid,_Atoms,AtomIds,Reportables,Status) :- !,
    rmv_mc_nui:heartbeat(Mid,Sid,AtomIds,Verdict), % call NuRV property monitor
    hb_verdict_status(Verdict,Sid,AtomIds,Reportables,Status).

mep_heartbeat(no_eval,_,Sid,_,_,Reportables,Status) :- !,
    hb_verdict_status(no_eval,Sid,[],Reportables,Status).

mep_heartbeat(_,_,Sid,_,_,Reportables,Status) :- !,
    hb_verdict_status(unset_eval,Sid,[],Reportables,Status).

%

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
notifications(monitor_report,Sid,Status) :-
    %monid_sessid_muniq_suniq(Mid,Sid,Muniq,Suniq),
    memberchk(basis(_,Reportables), Status), memberchk(verdict(Verdict), Status),
    epp_log_gen(monitor_event_processing, notification(monitor_report,Sid,Reportables,Verdict)),
    %format('notification: monitor report ~q:~a verdict: ~q, vars: ~q~n',[Mid,Sid,Verdict,Reportables]),
    true.

notifications(monitor_verdict,Sid,Reportables,Verdict) :-
    %monid_sessid_muniq_suniq(Mid,Sid,Muniq,Suniq),
    notifications(monitor_report,Sid,Reportables,_),
    epp_log_gen(monitor_event_processing, notification(monitor_verdict,Sid,Verdict)),
    %format('notification: monitor report ~q:~a verdict: ~q~n',[Mid,Sid,Verdict]),
    true.

%-------------------------------------------------------
% INITIATE/TERMINATE A RUNTIME MONITOR SESSION
%   note there are two levels of session: the global unique session with the full session id
%   and the last part of the global session id which is the NuRV session id
%
initiate_monitor(M,SessId) :-
    cons_monitor(MonitorId,_SSpecId,ModelId,_,_,_,MScv,_,_,M),
    MScv=ms_cv(MonitorId,_Vdecl,_Vo,_Vm,_Vp,_Vr,_Vt,_Atoms,AEval,_Vinit,_Beh,_Timer,_Host,_Port),
    % the MonitorId alone is not a sufficient SessId because there can be multiple instances
    % so a unique id is created (either the pid of a NuRV session or other unique id)
    param:rmv_monitor_id_prefix(MonIdPref), param:rmv_session_id_prefix(SessIdPref),
    atom_concat(MonIdPref,MonIdUniq,MonitorId), % extract unique part of Monitor ID

    (   AEval == no_eval % this is a RMV-only session, no NuRV session
    ->  uuid(SessUniq) % make unique part of Session ID
    ;   % the RMV session includes an interactive NuRV session
        param:monitor_directory_name(MD),
        atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
        atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),

        nurv_monitor_init(MonitorId,SMVmodelFile,SMVordFile,SessUniq)
    ),
    atomic_list_concat([SessIdPref,MonIdUniq, '_', SessUniq], SessId),
    init_session(SessId, monitor_framework),
	true.

terminate_monitor(SessId) :-
    monid_sessid_muniq_suniq(_,SessId,_,NSid),
    nurv_monitor_stop(NSid),
	display_session_log_nsid(NSid,clear),
    dump_nu_lines_nsid(NSid),
    ( is_session(SessId, monitor_framework) -> end_session(SessId) ; true ).

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
    %epp_log_gen('mep aT_list_constructor',''),
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
