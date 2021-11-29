% MONITOR REQUEST API
:- module(rmv_mrapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/restresp').

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
mrapi_monitor_start(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[user(User,[atom]),
				 ar(AR,[atom]),
				 object(Object,[atom]),
				 purpose(Purpose,[atom,optional(true)]), % DPLP
				 cond(CondAtom,[atom,optional(true)])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	monitor_start(_,User,AR,Object,Purpose,CondAtom),
	!.
mrapi_monitor_start(_) :- audit_gen(monitor_request, monitor_start(failure)).

monitor_start(_,_,_,_,_,_) :-
	true.

% monitor_stop
mrapi_monitor_stop(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[user(User,[atom]),
				 ar(AR,[atom]),
				 object(Object,[atom]),
				 purpose(Purpose,[atom,optional(true)]), % DPLP
				 cond(CondAtom,[atom,optional(true)])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	param:current_policy(Policy),
	monitor_stop(Policy,User,AR,Object,Purpose,CondAtom),
	!.
mrapi_monitor_stop(_) :- audit_gen(monitor_request, monitor_stop(failure)).

monitor_stop(_,_,_,_,_,_).

