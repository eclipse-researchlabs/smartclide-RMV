% MONITOR EVENT PROCESSING API
:- module(rmv_mepapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/apiresp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Monitoring Framework monitor event processing API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(mepapi), root_apis(mepapi), []).
:- http_handler(root('mepapi/'), api_unimpl, [prefix]).
:- http_handler(root(mepapi/monitor_start), mepapi_monitor_start, [prefix]).
:- http_handler(root(mepapi/monitor_stop), mepapi_monitor_stop, [prefix]).
:- http_handler(root(mepapi/monitor_heartbeat), mepapi_monitor_heartbeat, [prefix]).

mepapi([monitor_start,monitor_stop,monitor_heartbeat]). % MONITOR EVENT PROCESSING APIs

% monitor_start
mepapi_monitor_start(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				 monitor_id(Mid,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_start(Mid,_,_,_,_,_),
	!.
mepapi_monitor_start(_) :- audit_gen(monitor_event, monitor_start(failure)).

monitor_start(_Mid,_,_,_,_,_) :-
	true.

% monitor_stop
mepapi_monitor_stop(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				 monitor_id(Mid,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_stop(Mid,_,_,_,_,_),
	!.
mepapi_monitor_stop(_) :- audit_gen(monitor_event, monitor_stop(failure)).

monitor_stop(_Mid,_,_,_,_,_).

% monitor_heartbeat
mepapi_monitor_heartbeat(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				 monitor_id(Mid,[atom]),
				 atoms(AtomsListAtom,[atom]),
				 variables(OVarsListAtom,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_heartbeat(Mid,AtomsListAtom,OVarsListAtom,_,_,_),
	!.
mepapi_monitor_stop(_) :- audit_gen(monitor_event, monitor_heartbeat(failure)).

monitor_heartbeat(_Mid,_AtomsListAtom,_OVarsListAtom,_,_,_).

