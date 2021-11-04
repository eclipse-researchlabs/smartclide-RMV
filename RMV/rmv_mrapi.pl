% MONITOR REQUEST API
:- module(rmv_mrapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/jsonresp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Monitoring Framework monitoring request and admin API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(mrapi), root_apis(mraapi), []).
:- http_handler(root('mrapi/'), api_unimpl, [prefix]).
:- http_handler(root(mrapi/monitor_start), mrapi_monitor_start, [prefix]).
:- http_handler(root(mrapi/monitor_stop), mrapi_monitor_stop, [prefix]).

mraapi([monitor_start,monitor_stop]). % MONITOR REQUEST APIs

%
% Monitor Request API
%
% JSON response structure
% {
%     "respStatus" : "statusType",
%     "respMessage" : "statusDesc",
%     "respBody" : "statusBody"
% }
%
% json_resp(RespStatus,RespMessage,RespBody,JrespTerm,JrespAtom)
%
% assignments to the JSON response structure for each API are given in
% the documentation

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

%
% JSON response structure
% {
%     "respStatus" : "statusType",
%     "respMessage" : "statusDesc",
%     "respBody" : "statusBody"
% }
%
% json_resp(RespStatus,RespMessage,RespBody)
%

std_resp_prefix :-
	(   param:jsonresp(on)
	->  format('Content-type: application/json~n~n')
	;   format('Content-type: text/plain~n~n')
	).

std_resp_MS(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(M), writeln(Status)
	).

std_resp_BS(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(B), writeln(Status)
	).

std_resp_M(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(M)
	).

%
%
%

api_unimpl(_) :-
	std_resp_prefix,
	format('Unimplemented API~n').

root_apis(Kind,_) :- std_resp_prefix, list_apis(Kind), !.
root_apis(_,_).

list_apis(Kind) :-
	format('Valid ~a paths:~n',[Kind]),
	G=..[Kind,APIs], call(G),
	foreach( member(A,APIs), writeln(A)).

use_valid_api(_) :-
	format('Use valid endpoint~n').
