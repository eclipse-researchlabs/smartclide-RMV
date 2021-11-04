% LOGGING and NOTIFICATION Admin API
:- module(rmv_lnapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/jsonresp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Monitor Creation API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(lnapi), root_apis(lnapi), []).
:- http_handler(root('lnapi/'), api_unimpl, [prefix]).
:- http_handler(root(lnapi/subscribe), lnapi_subscribe, [prefix]).
:- http_handler(root(lnapi/create_monitor), lnapi_create_monitor, [prefix]).
:- http_handler(root(lnapi/paramecho), lnapi_paramecho, [prefix]).

lnapi([subscribe]). % MONITOR CREATION API

%
% Logging and Notification Admin API
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

% subscribe
lnapi_subscribe(Request) :-
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
	subscribe(Policy,User,AR,Object,Purpose,CondAtom),
	!.
lnapi_subscribe(_) :- audit_gen(monitor_creation, create(failure)).

subscribe(_,_,_,_,_,_) :-
	true.

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

