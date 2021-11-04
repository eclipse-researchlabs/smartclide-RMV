% MONITOR CREATION API
:- module(rmv_mcapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module([rmv_mc, rmv_mc_cm, rmv_mc_cps]).

:- use_module('COM/jsonresp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Monitor Creation API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(mcapi), root_apis(mcapi), []).
:- http_handler(root('mcapi/'), api_unimpl, [prefix]).
:- http_handler(root(mcapi/load_sspec), mcapi_load_sspec, [prefix]).
:- http_handler(root(mcapi/unload_sspec), mcapi_unload_sspec, [prefix]).
:- http_handler(root(mcapi/create_monitor), mcapi_create_monitor, [prefix]).
:- http_handler(root(mcapi/graph_monitor), mcapi_graph_monitor, [prefix]).

mcapi([load_spec,create_monitor,graph_monitor]). % MONITOR CREATION APIs

% ADD exposed NuRV operations:
%         add_property, show_property, build_monitor, generate_monitor

%
% Monitor Creation API
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

% load_spec
mcapi_load_sspec(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[servicespec(Sspec,[atom]),
				     token(Token,[atom])]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  load_sspec_aux(Sspec), !
	;   true
	).
mcapi_load_sspec(_) :- audit_gen(monitor_creation, load_spec(failure)).

load_sspec_aux(SS) :-
	(   ( ground(SS), load_service_spec_immediate(SS,ServId) )
	->  std_resp_BS(success,'service spec loaded immediate',ServId),
	    audit_gen(monitor_creation, load_spec(ServId,success))
	;   std_resp_MS(failure,'malformed spec or load error','spec elided'),
	    audit_gen(monitor_creation, load_spec('spec elided',failure))
	).

%-------------------------------------
% unload_spec
mcapi_unload_sspec(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[specid(SspecId,[atom]),
				     token(Token,[atom])]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  unload_sspec_aux(SspecId), !
	;   true
	).
mcapi_unload_sspec(_) :- audit_gen(monitor_creation, unload_spec(failure)).

unload_sspec_aux(Sid) :-
	(   ( atom(Sid), unload_service_spec(Sid) )
	->  std_resp_BS(success,'service spec loaded immediate',ServId),
	    audit_gen(monitor_creation, load_spec(ServId,success))
	;   std_resp_MS(failure,'malformed spec or load error','spec elided'),
	    audit_gen(monitor_creation, load_spec('spec elided',failure))
	).

%-------------------------------------
% create_monitor
mcapi_create_monitor(Request) :-
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
	create_monitor_aux(Policy,User,AR,Object,Purpose,CondAtom),
	!.
mcapi_create_monitor(_) :- audit_gen(monitor_creation, create_monitor(failure)).

create_monitor_aux(_,_,_,_,_,_).

% graph_monitor
mcapi_graph_monitor(Request) :-
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
	graph_monitor_aux(Policy,User,AR,Object,Purpose,CondAtom),
	!.
mcapi_graph_monitor(_) :- audit_gen(monitor_creation, graph_monitor(failure)).

graph_monitor_aux(_,_,_,_,_,_).

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

%
%

authenticate(Token) :-
	(   authenticate_token(Token)
	->  true
	;   std_resp_M(failure,'authentication error',''),
	    audit_gen(policy_admin, 'authentication error'),
	    !, fail
	).

authenticate_token(Token) :- atom(Token), param:rmv_token(Token), !.
