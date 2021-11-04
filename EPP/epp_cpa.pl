% Context-dependent Policy Adaptation (CPA)

:- module(epp_cpa, [condition_context_variable_map/2, read_context_cache/2,
                   context_change_notification/1]).

:- use_module(library(http/http_client)).

%:- use_module(dpl_conditions).
:- use_module(epp_era).
:- use_module(epp).


% ------------------------------------------------------------------------
% epp_cpa initialization - called by module epp
%

:- dynamic cpa_initialized/1.
cpa_initialized(false).

init:- cpa_initialized(true), !. % already initialized
init :-
    init_context_cache(Context_Variables_with_Types),
    % display_context_cache('after initialization'),
    init_context_change_notifications(Context_Variables_with_Types),
    % display_context_cache('after notification registration').
    % ...
    retractall(cpa_initialized(_)), assert(cpa_initialized(true)).

re_init :- un_init, init.

un_init :-
    % ...
    retractall(cpa_initialized(_)), assert(cpa_initialized(false)).


% ------------------------------------------------------------------------
% CONTEXT VARIABLES
%
%   the file context.pl is consulted (loaded) by init_context_cache
%
%   it contains a definition for context_variables/1
%   the empty def below is the default in case the file is empty

:- dynamic context_variables/1.
:- multifile context_variables/1.

context_variables([]).

% ------------------------------------------------------------------------
% MAPPING OF CONDITION VARIABLES TO CONTEXT VARIABLES
%
% condition_context_variable_map(ConditionVar, ContextVar)

:- dynamic condition_context_variable_map/2.
:- multifile condition_context_variable_map/2.

% these declarations should occur in the file context.pl


% ------------------------------------------------------------------------
% CONTEXT VARIABLE CACHE
%    context variables obtained from the context system are cached for
%    mapping to condition variables used in DPL condition predicates
%
%    ctx_cache(ContextVarName, ContextVarType, ContextVarValue)
%    ContextVarName uniquely identifies entry.
%    read_context_cache/3 returns Type and Value
%    update_context_cache/3 Type must be var or match what is stored

:- dynamic ctx_cache/3, ctx_cache_initialized/1.

ctx_cache(last_update,number,0).

ctx_cache_initialized(no).

init_context_cache(CtxVars) :- % TODO this has to change. No individual CV retrieval. Use change notification.
    % return list of needed context variable names
    (   param:context_file(CF), exists_file(CF)
    ->  ensure_loaded(CF)
    ;   true
    ),
    findall(CtxVar, (context_variables(Vars), member(CtxVar,Vars)), CtxVars),
    forall( member(CtxVarName:Type, CtxVars), update_context_cache(CtxVarName,Type,undefined) ),
%    findall(CtxVarName, member(CtxVarName:Type,CtxVars), CtxVarNames),
%    forall( ( member(CtxVar:Type,CtxVars),retrieve_context_variable(CtxVar,CtxVal) ),
%            update_context_cache(CtxVar,Type,CtxVal)
%          ),
%    init_context_change_notifications(CtxVars),
    retractall(ctx_cache_initialized(_)),
    assert(ctx_cache_initialized(yes)),
    true.

update_context_cache(CtxVar,CtxVal) :- update_context_cache(CtxVar,_,CtxVal).
update_context_cache(CtxVar,Type,Value) :- atom(CtxVar), ground(Value),
    context_variables(CVs), memberchk(CtxVar:Type,CVs), !, % update is only for value not type
    retractall(ctx_cache(CtxVar,_,_)), % variable names must be unique
    assert(ctx_cache(CtxVar,Type,Value)),
    % update (time of) last_update entry
    get_time(TS),
    retract(ctx_cache(last_update,UT,_)),
    assert(ctx_cache(last_update,UT,TS)).
update_context_cache(_,_,_). % succeed silently if CtxVar not defined

read_context_cache(CtxVar,CtxVal) :- atom(CtxVar), var(CtxVal), !,
    read_context_cache(CtxVar:_,CtxVal).
read_context_cache(CtxVar:Type,CtxVal) :- % read_context_cache with Type
    ctx_cache_initialized(yes), atom(CtxVar), var(CtxVal), !,
    ctx_cache(CtxVar,Type,CtxVal).
read_context_cache(_:_,undefined).

display_context_cache(M) :-
    findall(N:V, ctx_cache(N,_,V), NVs),
    ui:notify(context_cache,M),
    ui:display_listq(NVs).

% ------------------------------------------------------------------------
% CPA-CME CONTEXT MONITORING AND EXTRACTION INTERFACE
%
%   context_change_notification is invoked by the Context System
%   retrieve_context_variables invokes the Context System

init_context_change_notifications(CtxVarsTypes) :-
    % contact the CME with list of context variables to track
    maplist(context_variable_name,CtxVarsTypes,CVNames),
    param:epp_url(EPP_URL), atom_concat(EPP_URL,'context_notify', NotifyURL),
    param:epp_token(Etoken),
    register_for_context_change_notification(CVNames,NotifyURL,Etoken),
    true.

context_variable_name(Name:_Type,Name).
context_variable_value(_Name:Val,Val).
context_variable_name_value(Name:Val,Name,Val).
context_val_name_nameval(Val,Name,Name:Val).

register_for_context_change_notification(Names,NOTIF_URL,Token) :- param:context_sim(on), !,
    % use simulation of CME:
    ( context_notification_registration_sim(Names,NOTIF_URL,Token) ; true ),
    true.
register_for_context_change_notification(Names,NOTIF_URL,Token) :- param:context_sim(off), !,
    % construct call to the CME API for context change notification registration
    % and call it

    param:context_url(CTX_URL),
    term_to_atom(Names,NA),
    atomic_list_concat([CTX_URL,'context_notification_registration','?context_variables=',NA,
                        '&epp_url=',NOTIF_URL,'&epp_token=',Token],Call),
    % for testing call the sim anyway after showing the call
    format('Perform CME handshake: ~q~n',[Call]),
    http_get(Call,CallResult,[]), % call the CME
    % format('CallResult=~w',[CallResult]),
    (   CallResult == 'success\n'
    ->  format('CME handshake successful~n')
    ;   format('CME handshake unrecognized response~n')
    ),
    % should deal with a failure of the CME call
    % maybe disable further attempted interactions
    flush_output.

% CONTEXT CHANGE NOTIFICATION
%   invoked by the Context System
%   VarsVals is a list of Variable:Value pairs

context_change_notification :- % only for testing
    findall(CtxVar, (context_variables(Vars), member(CtxVar,Vars)), CtxVars),
    findall(Var:Val, (member(Var:_,CtxVars), retrieve_context_variable_sim(Var,Val)), VarsVals),
    update_context_variables(VarsVals),
    report_event(context_change,VarsVals).

context_change_notification_atom(VV) :- atom(VV), !,
    true.

%context_change_notification(VarsVals) :- is_list(VarsVals), !,
%    format('context_change_notification VarsVals: ~q~n',[VarsVals]),
%    ui:notify(context_change_notification,'variable_list_follows:'),
%    ui:display_listq(VarsVals),
    %read_term_from_atom(VarsVals,VVterm,[]),
    %format('VarsVals term: ~q~n',VVterm),
%    true.

context_change_notification(VarsVals) :- is_list(VarsVals), !,
    % format('context_change_notification VarsVals: ~q~n',[VarsVals]),
    epp_log_gen(epp_context_change, VarsVals),
    update_context_variables(VarsVals),
    report_event(context_change,VarsVals). % WHAT IF report_event fails HERE?
context_change_notification(Var:Val) :- atom(Var), ground(Val), !,
    % format('context_change_notification VarVal: ~q:~q~n',[Var,Val]),
    update_context_cache(Var,Val),
    epp_log_gen(epp_context_change, Var:Val),
    report_event(context_change,[Var:Val]).
context_change_notification(_) :- !, fail.

% update multiple variables in the context cache
%

update_context_variables([]) :- !.
update_context_variables([Var:Val|VarsVals]) :-
    update_context_cache(Var,Val),
    update_context_variables(VarsVals).

% ------------------------------------------------------------------------
% CONTEXT MONITORING AND EXTRACTION SIMULATION
%
%   context_variable_change/2 generates a context_change_notification
%
% Actively query the context system for one or more context variables
% to retrieve context variable values from the Context System.
% Not currently supported by the CME module.

context_notification_registration_sim(VarNames,URL,Etoken) :-
    format('Simulated context notification registration:~n  ~q ~q ~q~n',[VarNames,URL,Etoken]),
    % simulate call back
    % need to come up with values -- use simulation
    retrieve_context_variables_sim(VarNames,Vals),
    !,
    maplist(context_variable_name_value,VarsVals,VarNames,Vals),
    % for sim call up to context_change_notification (usually called from eppapi)
    format('Simulating notification response from CME: ~q~n',[VarsVals]),
    context_change_notification(VarsVals),
    true.

retrieve_context_variables_sim([],[]).
retrieve_context_variables_sim([Var|Vars],[Val|Vals]) :-
    retrieve_context_variable_sim(Var,Val),
    retrieve_context_variables_sim(Vars,Vals).

% this is an *individual* context variable retrieval sim, not currently
% supported by the CME module
%
retrieve_context_variable_sim(CtxVar,CtxVal) :-
    sim_context_var(CtxVar,CtxVal).


%
%
sim_context_var(day_of_the_week, DayOfWeek) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    ShortDate = date(Year,Month,Date),
    day_of_the_week(ShortDate,Day),
    nth1(Day,['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'],DayOfWeek).
%sim_context_var(adminLockdown,false) :- !.
sim_context_var(weekday,true) :- !.
sim_context_var(business,true) :- !.
% ...
sim_context_var(_,undefined).

% simulate change of a single context variable
sim_context_variable_change(VarName:Value) :- atom(VarName), ground(Value), !,
    context_change_notification(VarName:Value).

% simulate change of a list of context variables
sim_context_variable_change(VarsVals) :- is_list(VarsVals), !,
    context_change_notification(VarsVals).
