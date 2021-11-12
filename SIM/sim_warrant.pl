% WARRANT system simulation

:- use_module(library(http/http_client)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

:- use_module('COM/jsonresp').

% WARRANT API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root('warrant'), root_apis('warrant'), []).
:- http_handler(root('warrant/'), api_unimpl, [prefix]).
:- http_handler(root('warrant/get_warrant'), wapi_get_warrant, [prefix]).
:- http_handler(root('warrant/validate_warrant'), wapi_validate_warrant, [prefix]).
:- http_handler(root('warrant/invalidate_warrant'), wapi_invalidate_warrant, [prefix]).

warrant_apis([wapi_get_warrant, wapi_validate_warrant, wapi_invalidate_warrant]). % WARRANT API

warrant_token('warrant_token'). % default warrant token

warrant :- warrant(8003). % start on default warrant port

warrant(Port) :-
	format('WARRANT sim starting~n'),
	http_server(http_dispatch, [port(Port)]),
	format('WARRANT sim listening on port ~d~n',[Port]).

% Simulated Warrant transform
%
%   warrant_data(WarrantBlob,WarrantData).
%
%   Just stores a simple mapping from a random number to the warrant
%   parameters, instead of doing encryption operations. These warrants
%   only last for the duration of the current run of the simulation.
%   Unlike a cryptographic warrant these mappings are forgotten with the
%   termination of this process.

:- dynamic warrant_data/2.

%
% WARRANT APIs
%

% get warrant
wapi_get_warrant(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				 processor(DP,[atom]),
				 operation(DPO,[atom]),
				 purpose(Purpose,[atom]),
				 dataitem(DI,[atom]),
				 warrant_token(Token,[atom])
				]),

	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  get_warrant(DP,DPO,Purpose,DI), !
	;   true
	).
wapi_get_warrant(_) :- audit_gen(warrant, get_warrant(failure)).

get_warrant(DP,DPO,Purpose,DI) :-
	random_between(1000000,9999999,WarrantBlob),
	assert( warrant_data(WarrantBlob,[DP,DPO,Purpose,DI]) ).

% validate_warrant
wapi_validate_warrant(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				 warrant(Warrant,[atom]),
				 warrant_token(Token,[atom])
				]),

	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  validate_warrant(Warrant), !
	;   true
	).
wapi_validate_warrant(_) :- audit_gen(warrant, validate_warrant(failure)).

validate_warrant(WarrantBlob) :-
	(   warrant_data(WarrantBlob,WarrantData)
	->  format( atom(DataAtom), '~q', [WarrantData] ),
	    std_resp_BS(success,'valid warrant',DataAtom)
	;   std_resp_MS(failure,'invalid warrant',WarrantBlob),
	    true
	).

% invalidate_warrant
wapi_invalidate_warrant(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				 warrant(Warrant,[atom]),
				 warrant_token(Token,[atom])
				]),

	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  invalidate_warrant(Warrant), !
	;   true
	).
wapi_invalidate_warrant(_) :- audit_gen(warrant, invalidate_warrant(failure)).

invalidate_warrant(WarrantBlob) :-
	(   warrant_data(WarrantBlob,_)
	->  retractall( warrant_data(WarrantBlob,_) ),
	    std_resp_BS(success,'warrant invalidated',WarrantBlob)
	;   std_resp_MS(failure,'no such warrant',WarrantBlob),
	    true
	).


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

std_resp_prefix :- format('Content-type: text/plain~n~n').

use_valid_api(_) :-
	format('Use valid api~n').


%
authenticate(Token) :-
	(   authenticate_token(Token)
	->  true
	;   std_resp_M(failure,'authentication error',''),
	    audit_gen(policy_admin, 'authentication error'),
	    !, fail
	).

authenticate_token(Token) :- atom(Token), warrant_token(Token), !.

audit_gen(_,_). % NOOP
