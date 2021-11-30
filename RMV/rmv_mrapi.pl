% MONITOR REQUEST API
:- module(rmv_mrapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/apiresp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Monitoring Framework monitoring request and admin API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(mrapi), root_apis(mraapi), []).
:- http_handler(root('mrapi/'), api_unimpl, [prefix]).
:- http_handler(root(mrapi/monitor_list), mrapi_monitor_list, [prefix]).
:- http_handler(root(mrapi/monitor_subscribe), mrapi_monitor_subscribe, [prefix]).
:- http_handler(root(mrapi/monitor_unsubscribe), mrapi_monitor_unsubscribe, [prefix]).

mraapi([monitor_list,monitor_subscribe,monitor_unsubscribe]). % MONITOR REQUEST APIs


% monitor_start
mrapi_monitor_list(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_list(_),
	!.
mrapi_monitor_list(_) :- audit_gen(monitor_request, monitor_list(failure)).

monitor_list(_) :-
	true.

% monitor_subscribe
mrapi_monitor_subscribe(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_subscribe(_),
	!.
mrapi_monitor_stop(_) :- audit_gen(monitor_request, monitor_subscribe(failure)).

monitor_subscribe(_).


% monitor_unsubscribe
mrapi_monitor_unsubscribe(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_unsubscribe(_),
	!.
mrapi_monitor_stop(_) :- audit_gen(monitor_request, monitor_unsubscribe(failure)).

monitor_unsubscribe(_).

