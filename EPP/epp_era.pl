% Event Response Actuator (ERA)

:- module(epp_era, [activate_loaded_erp/1, deactivate_loaded_erp/1,
                    activate_erp/2,deactivate_erp/1,report_event/2,report_event/3]).

:- use_module(erl).
%:- use_module('NGAC/dpl').
:- use_module(epp).
%:- use_module('NGAC/pap').
:- use_module('EPP/epp').
:- use_module('EPP/erl').
:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% ------------------------------------------------------------------------
% epp_era initialization - called by module epp
%

init :-
    init_event_pattern_cache,
    init_event_response_cache,
    init_PAP_link,
    true.

% link to PAP may be by internal or Web socket API
% this initiates the socket communication
%
init_PAP_link :-
    true.


% ------------------------------------------------------------------------
% ACTIVATE AN EVENT-RESPONSE PACKAGE
%   The ERA is commanded to load/unload an ERP

activate_loaded_erp(ERPname) :-
    er_package(ERPname,ERrules), !,
    clear_caches(ERPname),
    activate_er_rules(ERrules,ERPname).

deactivate_loaded_erp(ERPname) :- !,
    er_package(ERPname,_ERrules), !,
    deactivate_erp(ERPname).

% acticate_erp is not currently used but deactivate_erp is used
activate_erp(ERL_package,ERpackageName) :- var(ERpackageName),
        ERL_package = er_package(ERpackageName,ERrules), !,
        activate_er_rules(ERrules,ERpackageName).

activate_er_rules([],_).
activate_er_rules([ERrule|ERrules],ERpackageName) :-
        activate_er_rule(ERrule,ERpackageName),
        activate_er_rules(ERrules,ERpackageName).

activate_er_rule(er(Event_Pattern,Event_Response),ERpackageName) :-
        add_event_response_cache(ResponseID,Event_Response,ERpackageName),
        add_event_pattern_cache(Event_Pattern,ResponseID),
        true.

deactivate_erp(ERpackageName) :-
        findall( ResponseID, eresp_cache(ResponseID,_,ERpackageName), ResponseIDs ),
        retractall( eresp_cache(_,_,ERpackageName) ),
        forall( member(ResponseID, ResponseIDs), retractall( epat_cache(_,ResponseID) ) ),
        true.

% ------------------------------------------------------------------------
:- multifile(report_event/2).
:- discontiguous(report_event/2).
:- multifile(report_event/3).
:- discontiguous(report_event/3).
:- multifile(event_name/1).
:- discontiguous(event_name/1).
:- multifile(name_event_map/2).
:- discontiguous(name_event_map/2).


% EVENT NAME DEFINITION
%   TODO: must be reconciled with event definition below
%
:- multifile(epp_era:event_name/1).
:- discontiguous(epp_era:event_name/1).

event_name(server_startup).
event_name(server_shutdown).
event_name(epp_startup).
event_name(epp_shutdown).
event_name(context_change).
event_name(test_event).
% ...
event_name(test_ms_event).
event_name(ms_event).

% pre-defined event names and corresponding event structure
:- multifile(epp_era:name_event_map/2).
:- discontiguous(epp_era:name_event_map/2).

name_event_map(test_ms_event,  ms_event('MS event data')).
name_event_map(Name,event(Name,x,x,x,x)) :- !, event_name(Name).

name_event_map(server_startup, event(x,x,x,x)).
name_event_map(server_shutdown,event(x,x,x,x)).
name_event_map(epp_startup,    event(x,x,x,x)).
name_event_map(epp_shutdown,   event(x,x,x,x)).
name_event_map(context_change, event(x,x,x,x)).
name_event_map(test_event,     event(x,x,x,x)).
% ...

% ------------------------------------------------------------------------
% EVENT SPECIFICATION
%
% An event can be reported as an event structure or
% as a name mapped to an event structure by name_event_map.

%event(ms_event(_)).
event(event(Name,Uspec,PCspec,OPspec,Ospec)) :- % check whether valid event
    atom(Name), uspec(Uspec), pcspec(PCspec), opspec(OPspec), ospec(Ospec), !.
event(event(_,_,_,_,_)). % allow any event/5 for now

ms_event(ms_event(_MSmessage)) :- !.

uspec(user(UserID)) :- !, atom(UserID). % including 'any'
uspec(user_attribute(UattrID)) :- !, atom(UattrID). % including 'any'
uspec(session(SessID)) :- !, atom(SessID).
uspec(process(ProcID)) :- !, integer(ProcID).

pcspec(policy_class(PCid)) :- !, atom(PCid).

opspec(operation(OPid)) :- !, atom(OPid).
opspec(OPsetID) :- atom(OPsetID), !.

ospec(object(ObjID)) :- !, atom(ObjID).

:- dynamic event_data/1. % currently only context data
event_data([]). % stash last context change list here for context-change event responses

%:- multifile(epp_era:report_event/1).
%:- discontiguous(epp_era:report_event/1).
:- multifile(epp_era:report_event/2).
:- discontiguous(epp_era:report_event/2).
:- multifile(epp_era:report_event/3).
:- discontiguous(epp_era:report_event/3).

/*
report_event(Event, EventData) :-
    % if this is used for more than context change events, will need mutex
    retractall(event_data(_)),
    assert(event_data(EventData)),
    report_event(Event).
*/

report_event(Event, EventData, Reply) :-
    % if this is used for more than context change events, will need mutex
    retractall(event_data(_)),
    assert(event_data(EventData)),
    report_event(Event,Reply).

report_event(Event, Reply) :- event(Event), !,
    epp_log_gen(report_event,Event),
    search_event_pattern_cache(Event,Matches), % !,
    epp_log_gen('matched patterns in report_event',Matches),
    execute_event_responses(Matches), % TODO - should contribute to Reply
    Reply = normal.
report_event(Event, Reply) :- ms_event(Event), !,
    Event = ms_event(MSmessage), \+ var(MSmessage), !,
    epp_log_gen(report_ms_event,Event),
    term_to_atom(MSmessage,JTA),
    atom_json_term(JTA,JT,[]), % TODO - this can fail if not valid JSON - handle?
    handle_ms_event(JT,Reply),
    true.
report_event(EventName,Reply) :- atom(EventName), name_event_map(EventName,Event), !,
    report_event(Event,Reply).

:- discontiguous handle_ms_event/2.

handle_ms_event(PT,Reply) :-
    epp_log_gen(handle_ms_event,PT),
    Reply = normal,
    true.

% following won’t work with multifile/discontiguous b/c must be last clause - just fail
%report_event(X, _Reply) :- % invalid
%    ui:notify('Invalid event reported',X),
%    fail.
% ------------------------------------------------------------------------
% Isolated RMV changes to be moved to RMV-specific module rmv_mf_mep
% :- multifile(epp_era:report_event/2).
% :- discontiguous(epp_era:report_event/2).
% :- multifile(epp_era:report_event/3).
% :- discontiguous(epp_era:report_event/3).
% :- multifile(epp_era:event_name/1).
% :- discontiguous(epp_era:event_name/1).
% :- multifile(epp_era:name_event_map/2).
% :- discontiguous(epp_era:name_event_map/2).

% :- multifile(report_event/2).
% :- discontiguous(report_event/2).
% :- multifile(report_event/3).
% :- discontiguous(report_event/3).
% :- multifile(event_name/1).
% :- discontiguous(event_name/1).
% :- multifile(name_event_map/2).
% :- discontiguous(name_event_map/2).


%name_event_map(test_ms_event,  ms_event('MS event data')).

% ------------------------------------------------------------------------


% ------------------------------------------------------------------------
% Isolated RMV changes to be moved to RMV-specific module rmv_mf_mep

event_name(test_ms_event).

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
% CLEAR CACHES OF A SPECIFIC ER PACKAGE
%

clear_caches(ERpackageName) :- atom(ERpackageName), !,
    findall(ResponseID, retract(eresp_cache(ResponseID,_Actions,ERpackageName)), ResponseIDs),
    forall( member(ResponseID,ResponseIDs), retractall(epat_cache(_,ResponseID)) ).

% ------------------------------------------------------------------------
% EVENT PATTERN CACHE
%

event_pattern_cache(Pattern,ResponseID) :- atom(Pattern), !,
    epat_cache(Pattern,ResponseID).

:- dynamic epat_cache/2.

%epat_cache(nullEvent,nullResponse).

init_event_pattern_cache :-
    true.

add_event_pattern_cache(Pattern,ResponseID) :- ground(Pattern), integer(ResponseID), !,
    assertz(epat_cache(Pattern,ResponseID)).

% ------------------------------------------------------------------------
% EVENT RESPONSE CACHE
%

event_response_cache(ResponseID,Actions,ERpackageName) :-
    eresp_cache(ResponseID,Actions,ERpackageName).

:- dynamic eresp_cache/3, response_id/1.

%eresp_cache(nullResponse,[]).

init_event_response_cache :-
    true.

clear_event_response_cache(ERpackageName,ResponseIDs) :- atom(ERpackageName), !,
    findall(ResponseID, retract(eresp_cache(ResponseID,_Actions,ERpackageName)), ResponseIDs),
    forall( member(ResponseID,ResponseIDs), retractall(epat_cache(_,ResponseID)) ).

add_event_response_cache(ResponseID,Actions,ERpackageName) :-
    var(ResponseID), atom(ERpackageName), !,
    new_response_id(ResponseID),
    assertz(eresp_cache(ResponseID,Actions,ERpackageName)).

update_event_response_cache(ResponseID,Actions,ERpackageName) :-
    atom(ResponseID), atom(ERpackageName), !,
    retractall(eresp_cache(ResponseID,_,ERpackageName)),
    assert(eresp_cache(ResponseID,Actions,ERpackageName)).

response_id(1).

new_response_id(ID) :- var(ID), !,
    response_id(ID),
    NewID is ID + 1,
    retractall(response_id(_)),
    assert(response_id(NewID)).


% ------------------------------------------------------------------------
% EVENT PATTERN SEARCH AND MATCHING
%

search_event_pattern_cache(Event,ERidPCs) :- event(Event), !,
    findall(ERid:PC , (epat_cache(Epat,ERid), event_match(Event,Epat,PC)), ERidPCs).
search_event_pattern_cache(EventName,ERidPCs) :- atom(EventName), name_event_map(EventName,Event), !,
    search_event_pattern_cache(Event, ERidPCs).
search_event_pattern_cache(_,[]).

event_match( event(_, E_U, E_PC, E_Op, E_O), ev_pat(P_U, P_PC, P_Op, P_O), PC ) :- !,
    match_pc(E_PC, P_PC),
    match_user(P_PC, E_U, P_U),
    match_op(P_PC, E_Op, P_Op),
    match_obj(P_PC, E_O, P_O),
    P_PC=policy_class(PC).

match_user(_P_PC, user(Euid), user(Euid)) :- atom(Euid), !.
match_user(_P_PC, user(Euid), user(any)) :- atom(Euid), !.
match_user(_P_PC, user_attribute(UAid), user_attribute(UAid)) :- atom(UAid), !.
match_user(_P_PC, session(Sid), session(Sid)) :- atom(Sid), !.
match_user(_P_PC, process(Pid), process(Pid)) :- integer(Pid), !.

match_pc(policy_class(PCid), policy_class(PCid)) :- atom(PCid), !.
match_pc(policy_class(PCid), policy_class(any)) :- atom(PCid), !.

match_op(_P_PC, operation(OpId), operation(OpId)) :- atom(OpId), !.
match_op(P_PC, operation(OpId), OpSetId) :- atom(OpId), atom(OpSetId), !,
    get_id_operation_set(P_PC,OpSetId,OpSet),
    memberchk(OpId, OpSet).

match_obj(_P_PC, object(Oid), object(Oid)) :- atom(Oid), !.
match_obj(_P_PC, object(Oid), object(any)) :- atom(Oid), !.


% ------------------------------------------------------------------------
% EVENT RESPONSE EXECUTION
%

execute_event_responses([]) :- !.
execute_event_responses([Rid_PC|ResponseID_PCs]) :-
    % the policy class in Rid_PC is the PC from the triggered event pattern
    % which may not be relevant to response execution
    execute_event_response(Rid_PC),
    execute_event_responses(ResponseID_PCs).

execute_event_response(ResponseID:PC) :- number(ResponseID), atom(PC), !,
    eresp_cache(ResponseID,Actions,_),
    execute_response_actions(Actions,PC).

execute_response_actions([],_).
execute_response_actions([Action|Actions],PC) :-
    execute_response_action(Action,PC),
    execute_response_actions(Actions,PC).

execute_response_action(Action,PatPC) :-
    epp_log_gen(ep_command,Action),
    % should authenticate token for command execution (TO DO)
    % the PC from the event pattern may not be relevant to the action execution
    % if not running in the same image as the policy server will have to get
    % the current policy from the server instead of param:current_policy

    param:current_policy(CurrentPol),
    (   CurrentPol \== none
    ->  policy(CurrentPol,CurrentPC)
    ;   CurrentPC = none
    ),
    do_admin_command(Action,CurrentPol,CurrentPC,PatPC),
    true.

% ------------------------------------------------------------------------
% do_admin_command( Command, CP, CPC, PPC )
%
% CP is the current (server) policy
% CPC is the current (server) policy class associated with CP
% PPC is the policy class of the matched pattern by which this execution
% was triggered
%
% some of these should be expanded with additional checks
%
do_admin_command(add(Policy,Element),_CP,_CPC,_PPC) :- !, add_policy_element(Policy,Element).
do_admin_command(addm(Policy,Elements),_CP,_CPC,_PPC) :- !, add_policy_elements(Policy,Elements).
do_admin_command(delete(Policy,Element),_CP,_CPC,_PPC) :- !, delete_policy_element(Policy,Element).
do_admin_command(deletem(Policy,Elements),_CP,_CPC,_PPC) :- !, delete_policy_elements(Policy,Elements).
do_admin_command(add(Element),CP,CPC,_PPC) :- !, add_policy_element(CP,CPC,Element).
do_admin_command(addm(Elements),CP,CPC,_PPC) :- !, add_policy_elements(CP,CPC,Elements).
do_admin_command(delete(Element),CP,CPC,_PPC) :- !, delete_policy_element(CP,CPC,Element).
do_admin_command(deletem(Elements),CP,CPC,_PPC) :- !, delete_policy_elements(CP,CPC,Elements).
do_admin_command(load(PolicyFile),_,_,_) :- !, exists_file(PolicyFile), load_policy(PolicyFile,_).
do_admin_command(loadi(PolicySpec),_,_,_) :- !, load_policy_immediate(PolicySpec,_).
do_admin_command(unload(PolicyName),_,_,_) :- !, dpl:policy(PolicyName,_), unload_policy(PolicyName).
do_admin_command(setpol(PolicyName),_,_,_) :- !, dpl:policy(PolicyName,_), set_current_policy(PolicyName).
do_admin_command(log(M),_,_,_) :- !, epp_log_gen(admin_command_log, M).
do_admin_command(_,_,_,_). % silently ignore unknown admin command


