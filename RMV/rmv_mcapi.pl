% MONITOR CREATION API
:- module(rmv_mcapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module('COM/apiresp').
:- use_module(rmv_ml).

:- use_module([rmv_mc, rmv_mc_cm, rmv_mc_cps]).

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
	    http_parameters(Request,[
					servicespec(Sspec,[atom]),
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
	;   std_resp_MS(failure,'malformed spec or load error',failure),
	    audit_gen(monitor_creation, load_spec('malformed spec or load error',failure))
	).

%-------------------------------------
% unload_spec
mcapi_unload_sspec(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					specid(SspecId,[atom]),
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
	->  std_resp_BS(success,'service spec unloaded',Sid),
	    audit_gen(monitor_creation, unload_sspec(Sid,success))
	;   std_resp_MS(failure,unload_sspec,Sid),
	    audit_gen(monitor_creation, unload_spec(Sid,failure))
	).

%-------------------------------------
% create_monitor
mcapi_create_monitor(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
				token(Token,[atom]),
				service_spec(SS,[atom])
			]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(	authenticate(Token)
	->	create_monitor_aux(SS)
	;	true
	).
mcapi_create_monitor(_) :- audit_gen(monitor_creation, create_monitor(failure)).

create_monitor_aux(SSA) :- trace,
	(	( read_term_from_atom(SSA,SS,[]), is_service_spec(SS), rmv_mc:service_spec2monitor(SS,Monitor) )
	->	monitor(MonId,Monitor),
		std_resp_BS(success,'monitor created',MonId),
		audit_gen(monitor_creation, create_monito(MonId,success))
	;	std_resp_MS(failure,create_monitor,SS),
		audit_gen(monitor_creation, create_monitor(SS,failure))
	).
	

% graph_monitor
mcapi_graph_monitor(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				token(Token,[atom]),
				monitor_id(Mid,[atom])
				]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(	authenticate(Token)
	->	graph_monitor_aux(Mid), !
	;	true
	).
mmcapi_graph_monitor(_) :- audit_gen(monitor_creation, graph_monitor(failure)).

graph_monitor_aux(_Mid) :-
	std_resp_MS(failure,'graph_monitor',unimplemented).


%
%

authenticate(Token) :-
	(   authenticate_mc_token(Token)
	->  true
	;   std_resp_M(failure,'authentication error',''),
	    audit_gen(monitor_creation, 'authentication error'),
	    !, fail
	).

authenticate_mc_token(Token) :- atom(Token), param:rmv_mc_token(Token), !.
