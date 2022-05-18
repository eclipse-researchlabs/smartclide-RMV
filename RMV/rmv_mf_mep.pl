% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_monitor_start/2, mep_monitor_stop/3,
    mep_heartbeat/2, mep_heartbeat/5, json_var_val/2
	       ]).

:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).
    
:- use_module('COM/param').
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
    %rmv_mc_nui:start_monitor(Mid,Status), % TODO - currently only returns a status
    monitor(Mid,Monitor), !,
    initiate_monitor(Monitor,SessId),
	Status = [monitor_started,session(SessId)],
    true.
mep_monitor_start(Mid,Status) :-
    Status = [monitor_not_found(Mid)].

mep_monitor_stop(Mid,SessId,Status) :- atom(Mid),
    (   number(SessId)
    ->  atom_number(SessIdA,SessId)
    ;   SessIdA = SessId
    ),
    %rmv_mc_nui:stop_monitor(Mid,Status), % TODO - currently only returns a status
    terminate_monitor(SessIdA), % TODO - don't really need MonitorId, just SessId
	Status = [monitor_stopping],
    true.

mep_heartbeat(HBterm,Status) :-
    % unpack the HBterm
    /* HBterm = ms_event(MSmessage),*/ \+ var(HBterm), !,
    term_to_atom(HBterm,JA),
    %writeq(JA),nl,flush_output, %!, fail,
    atom_json_term(JA,JT,[]),
    % handle the heartbeat in JT
    JT = json([monid=Monid, sessid=Sessid, atoms=ATl, vars=JVAl]),
    is_list(ATl), is_list(JVAl),
    maplist(json_var_val, JVAl, VAl),
    format(atom(A),'monid=~a, sessid=~a, atoms=~q, vars=~q', [Monid,Sessid,ATl,VAl]),
    epp_log_gen('mep_heartbeat HBterm:',A),
    mep_heartbeat(Monid,Sessid,ATl,VAl,Status),
    true.

json_var_val( json([Var='@'(true)]), Var=true ) :- !.
json_var_val( json([Var='@'(false)]), Var=false ) :- !.
json_var_val( json([Var=Val]), Var=Val ).

% var_val_json(  ) :- !.
% var_val_json(  ) :- !.
% var_val_json(  ) :- !.

mep_heartbeat(Mid,Sid,AtomIds,Reportables,Status) :-
    %monitor(Mid,Monitor),
    %Monitor = monitor( MonId, SSpecId, ModId, Properties, MSlang, MSid, MScv, MSfile ),
    %monitor(Mid,_Mod,_Obs,_Reps,Atoms,AtomEval,_SensorVers),
    monitor_atoms_eval(Mid,Atoms,AtomEval),
    (   ( /* AtomIds == [], */ AtomEval == mep_eval ) % monitor specifies mep evaluation
        % for mep evaluation, property variables must be a subset of reportables
    ->  aT_list_constructor(Atoms,Reportables,TAtomIdList) % ignore AtomIds from MS
    ;   TAtomIdList = AtomIds
    ),
    epp_log_gen('mep_heartbeat/5, true atoms:',TAtomIdList),

    % fake the NuRV call for testing
    %Verdict=unknown, % fake return
    rmv_mc_nui:heartbeat(Mid,Sid,TAtomIdList,Verdict), % real call

    % basis(,) is temporarily included in the Status for round-trip check - TODO
    (   (Verdict == true ; Verdict == unknown)
    ->  Status = [acknowledged,session(Mid:Sid),verdict(Verdict),basis(TAtomIdList,Reportables,fake_NuRV)]
    ;   (   Verdict == false
        ->  Status = [acknowledged,session(Mid:Sid),verdict(Verdict),basis(TAtomIdList,Reportables),recovery(true)],
            notifications(report,Mid,Sid,Reportables,Verdict)
        ;   Status = [session(Mid:Sid),exception(Verdict),basis(TAtomIdList,Reportables)],
            notifications(report,Mid,Sid,exception,Verdict)
        )
    ),
    true.

% temporary stub for notifications:
notifications(report,Mid,Sid,Reportables,Verdict) :-
    format('notification: monitor report ~q:~a verdict: ~q, vars: ~q~n',
        [Mid,Sid,Verdict,Reportables]),
    true.

notifications(verdict,Mid,Sid,_,Verdict) :-
    format('notification: monitor report ~q:~a verdict: ~q~n',[Mid,Sid,Verdict]),
    true.

%-------------------------------------------------------
% INITIATE/TERMINATE A RUNTIME MONITOR SESSION
%
initiate_monitor(M,SessId) :-
    cons_monitor(MonitorId,_SSpecId,ModelId,_,_,_,_,_,M),
    open_nurv_session(int,SessId,MonitorId),
	%format('Monitor ID: ~a; NuRV session: ~a~n',[MonitorId,SessId]), flush_output,
	nurv_session_get_resp(SessId,''),
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
	atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
	nurv_monitor_init(SMVmodelFile,SMVordFile,SessId),
	true.

terminate_monitor(SessId) :-
	quit_nurv_session(SessId),
	writeln('session ended').

initiate_monitor2(M,SessId) :-
    cons_monitor(MonitorId,SessId,_,_,_,_,_,_,M),
    open_nurv_session(orbit,SessId,MonitorId),
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
