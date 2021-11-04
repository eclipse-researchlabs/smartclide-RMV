% Audit manager
:- module(audit, [audit_control/1, audit_gen/2,audit_gen/3,
                  audit_auditable_set/1, audit_selection_set/1,
                  audit_select/1, audit_deselect/1]).

:- use_module('COM/param').

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).


% AUDIT EVENTS
%
%   auditable_events is the set of all auditable ngac events
%   audit_selection is the subset of auditable_events currently selected
%     to generate audit records. It can be changed with setparam, but it
%     is the responsibility of the caller of setparam to ensure that
%     only elements of auditable_events are placed in audit_selection.
%

% audit event groups are a shorthand for a group of related audit events
%
% The operations audit_set, audit_select, and audit_deselect must still
% be extended to recognize audit event groups.
%
audit_event_group(policy_query,[pq_grant,pq_deny,pq_obj_info]).
audit_event_group(policy_admin,[pa_add,pa_delete,pa_combine,pa_getpol,pa_setpol,
                                pa_load,pa_unload,pa_init_session,pa_end_session]).
audit_event_group(event_processing,[ep_load,ep_unload,ep_event]).

% This defines the set of possible auditable events
% Normally in a closed system this set would be static
% while the set of events selected from it for auditing
% could be dynamic. In this more general implementation the set of
% auditable events is dynamic. There should be code in the subsystems
% using the audit facilities to generate each auditable event.
% Only reported events that are in the audit selecton will have and
% audit record generated. Even if an event is listed in the current
% audit selection but does not exist in auditable events it will not
% generate an audit record, but will will not be reported as an error.

:- dynamic auditable_events/1.

auditable_events([ngac_start, ngac_shutdown, epp_start, epp_shutdown,
                  pq_grant, pq_deny, % these not currently used
                  pq_objinfo,
                  pa_add, pa_delete, pa_combine, pa_getpol, pa_setpol, pa_load, pa_loadi, pa_unload,
                  pa_init_session, pa_end_session,
                  ep_load, ep_unload, ep_activate, ep_deactivate, ep_event, ep_command,
                  policy_admin, policy_query, gpolicy_query, event_admin, event_processing,
                  error  % do not remove general error
                 ]).

% audit_selection is the set of currently selected events to generate
% audit records. In module param: audit_selection/1 is defined as
% initially []

:- dynamic audit_initialized/1.
audit_initialized(false).

init:- audit_initialized(true), !. % already initialized
init :-
	% ...
        retractall(audit_initialized(_)), assert(audit_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(audit_initialized(_)), assert(audit_initialized(false)).

% specific initialization that sets the audit selection
init(basic) :- !,
    param:setparam(audit_selection,
                   [server_start,
                    pq_grant,pq_deny,
                    pa_combine, pa_setpol, pa_load, pa_loadi,
                    ep_load, ep_unload, ep_event,
                    error
                   ]).
init(full) :- !,
    auditable_events(InitEvents), % turn all events on
    param:setparam(audit_selection,InitEvents).

% ngac audit operations
%   audit_control allows configuration of the audit functions
%     functions include:
%       set_auditable - unconditionally sets the list of auditable events
%       get_auditable - returns the list of all auditable events
%       add_auditable -
%       set_logfile -
%
%   audit_gen generates audit record info for ngac audit
%     it leaves time stamp generation to the system audit function
%     arguments are: event=<auditable event id> data=<event data>
%   audit_set unconditionally sets the parameter
%       auditable_events to the given list, except it will not permit
%       the audit event error is always included even if unspecified.
%   audit_select adds the given list of ngac audit
%       events to the audit_selection audit_deselect removes the given
%       list of ngac audit events from the current audit_selection
%
% ngac audit events
%   There is a generic event: error
%   This is used for ngac errors for otherwise unaudited ngac errors.
%   It is always an auditable event and always selected (cannot be
%   deselsected).
%

audit_control(_).

audit_gen(Event, Data) :- audit_gen(ngac,Event,Data). % backward compatible

audit_gen(Source, Event, Data) :-
    param:audit_selection(AS), auditable_events(Auditable),
    (   ( memberchk(Event, AS), memberchk(Event, Auditable) )
    ->  sys_audit(Source, Event, Data)
    ;   true
    ), !.
audit_gen(_,_,_). % always succeed

audit_auditable_set(Events) :- ground(Events), is_set(Events),
    % unconditionally set the auditable events
    % there is no protection against including events for which there
    % is no code that actually generates the event
    union(Events,[error],NewAuditable), % error always included
    retractall(auditable_events(_)), assert(auditable_events(NewAuditable)), !.
audit_auditable_set(_) :-
    sys_audit(audit, error, 'Failure to set auditable_events.').

audit_selection_set(Events) :- ground(Events), is_set(Events),
    % unconditionally set the selected events
    auditable_events(Auditable),
    subset(Events,Auditable),
    union(Events,[error],NewSelection), % error cannot be removed from ngac audit_selection
    param:setparam(audit_selection,NewSelection), !.
audit_selection_set(_) :-
    sys_audit(audit, error, 'Failure to set audit_selection.').

audit_select(Event) :- atom(Event), !, audit_select([Event]).
audit_select(Events) :-
    ground(Events), is_set(Events),
    auditable_events(Auditable),
    subset(Events,Auditable),
    param:audit_selection(CurrentSelection),
    union(Events,CurrentSelection,NewSelection),
    param:setparam(audit_selection,NewSelection), !.
audit_select(_) :-
    sys_audit(audit, error, 'Failure to add to audit_selection.').

audit_deselect(Event) :- atom(Event), !, audit_deselect([Event]).
audit_deselect(Events) :-
    ground(Events), is_set(Events),
    delete(Events,error,DeSelection), % cannot deselect error
    auditable_events(Auditable),
    subset(DeSelection,Auditable),
    param:audit_selection(CurrentSelection),
    subtract(CurrentSelection,DeSelection,NewSelection),
    param:setparam(audit_selection,NewSelection), !.
audit_deselect(_) :-
    sys_audit(audit, error, 'Failure to delete from audit_selection.').

%
% Customise below for local system audit
%   always succeeds
%

sys_audit(Source, Event, EventData) :- atom(Source), atom(Event), ground(EventData),
    % for general use without a specific system audit service, write to local audit
    % std error is default audit_stream if audit_logging(on)
    % a local file is created for the log if audit_logging(file)
    (	\+ param:audit_logging(off)
    ->	param:audit_stream(Audit),
	gen_time_stamp(TS), % use own time stamp only for own log
        put_audit_record(Audit,[TS,Source,Event,EventData])
    ;	true
    ), % for general use or for testing

    %
    % Call system-specific audit
    %

    % e.g.: local_audit(Source,Event,EventData),

    !.
sys_audit(_,_,_).

gen_time_stamp(TS) :-
	get_time(T),
	stamp_date_time(T,date(YYYY,MM,DD,H,M,S,_,_,_),local),
	format(atom(TS), '~d-~d-~d_~d:~d:~d', [YYYY,MM,DD,H,M,truncate(S)]).

put_audit_record(Audit, VarList) :-
        % VarList =  [TS,Source,Event,EventData]
        param:audit_record(Format),
	format(Audit, Format, VarList),
	flush_output(Audit).




testq :- % temporary test query
    param:admin_token(Token),
    format(atom(AuthHeader),'Authorization: OAuth ~a',Token),
%    ChunkHeader='Transfer-Encoding: chunked',
    ContentHeader='Content-Type: text/plain',
%    ContentHeader='Content-Type: application/x-www-form-urlencoded',
%    http_post('http://httpbin.org:80/post','',Reply,[header(AuthHeader),header(ContentHeader)]),
    http_get('http://httpbin.org:80/get?a=aaa&b=bbb',ReplyG,[]),
    format('Reply:~n~w~n',ReplyG), !,
%    http_get('http://httpbin.org:80/get',ReplyP,[header('Authorization: OAuth eyJ0eXAiOiJKV'),header('Content-Type: text/plain')]),
%    http_post('http://httpbin.org/post','mydata',ReplyP,[header(AuthHeader),header(ContentHeader)]),
    http_post('http://httpbin.org/post?a=aaa&b=bbb','',ReplyP,[header(AuthHeader),header(ContentHeader)]),
    format('Reply:~n~w~n',ReplyP),
    true.
