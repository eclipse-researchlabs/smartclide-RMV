% MONITOR EVENT PROCESSING API
:- module(rmv_mepapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/apiresp').
:- use_module(rmv_mf_mep).
:- use_module('EPP/epp').
%:- use_module('EPP/epp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Monitoring Framework monitor event processing API
%:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(mep), root_apis(mepapi), []).
:- http_handler(root('mep/'), api_unimpl, [prefix]).
:- http_handler(root(mep/monitor_start), mepapi_monitor_start, [prefix]).
:- http_handler(root(mep/monitor_stop), mepapi_monitor_stop, [prefix]).
:- http_handler(root(mep/monitor_heartbeat), mepapi_monitor_heartbeat, [prefix]).

mepapi([monitor_start,monitor_stop,monitor_heartbeat]). % MONITOR EVENT PROCESSING APIs

% monitor_start
mepapi_monitor_start(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
			token(Token,[atom]),
			monitor_id(Mid,[atom])
			]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(	authenticate_rmv(Token)
	->	monitor_start_aux(Mid,_,_,_,_,_) %, !
	;	true
	).
mepapi_monitor_start(_) :- epp_log_gen(monitor_event, monitor_start(failure)).

monitor_start_aux(_,_,_,_,_,_) :-
	std_resp_MS(success,'monitor_start',yes),
	true.

% monitor_stop
mepapi_monitor_stop(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
			token(Token,[atom]),
			monitor_id(Mid,[atom]),
			session_id(Sid,[atom])
			]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(	authenticate_rmv(Token)
	->	monitor_stop_aux(Mid,Sid,_,_,_,_), !
	;	true
	).
mepapi_monitor_stop(_) :- epp_log_gen(monitor_event, monitor_stop(failure)).

monitor_stop_aux(_,_,_,_,_,_) :-
	std_resp_MS(success,'monitor_stop',yes),
	true.

% monitor_heartbeat
mepapi_monitor_heartbeat(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				token(Token,[atom]),
				%monitor_id(Mid,[atom]),
				session_id(Sid,[atom]),
			    heartbeat(HBatom,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(	( authenticate_rmv(Token), authenticate_mep(Sid) )
	->	monitor_heartbeat_aux(Sid,HBatom), !
	;	true
	).
mepapi_monitor_heartbeat(_) :- epp_log_gen(monitor_event, monitor_heartbeat(failure)).

%monitor_heartbeat_aux(_,_) :- !.
monitor_heartbeat_aux(Sid,HBatom) :-
	epp_log_gen('MEP API received heartbeat for session',Sid),
%	read_term_from_atom(AtomsListAtom,AtomsList,[]),
%	read_term_from_atom(OVarsListAtom,OVarsList,[]),
%	mep_heartbeat(Mid,AtomsList,OVarsList,Response),
	read_term_from_atom(HBatom,HBterm,[]),
	epp_log_gen(monitor_heartbeat_aux,args(Sid,HBterm)),
    (   mep_heartbeat(HBterm,Status)
	->  std_resp_BS(success,'monitor heartbeat recorded',Status),
	    epp_log_gen(monitor_event_processing, heartbeat(success,Status))
	;   std_resp_MS(failure,'heartbeat recording',HBterm),
	    epp_log_gen(monitor_event_processing, heartbeat(failure))
	).

authenticate_mep(Session) :-
	(   rmv_mc_nui:nurv_session(Session,_,_,_)
	->  true
	;   std_resp_M(failure,'session authentication error',''),
		audit_gen(mep,'session authentication error'),
		!, fail
	).

authenticate_rmv(Token) :-
(   authenticate_rmv_token(Token)
->  true
;   std_resp_M(failure,'authentication error',''),
	audit_gen(mepapi, 'authentication error'),
	!, fail
).


authenticate_rmv_token(Token) :- atom(Token), param:rmv_token(Token).