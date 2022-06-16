% LOGGING and NOTIFICATION Admin API
:- module(rmv_lnapi, []).

:- use_module('COM/param').
:- use_module('COM/apiresp').
:- use_module(rmv_la).
:- use_module(rmv_na).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Logging and Notification admin API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root(lnapi), root_apis(lnapi), []).
:- http_handler(root('lnapi/'), api_unimpl, [prefix]).
:- http_handler(root(lnapi/subscribe), lnapi_subscribe, [prefix]).

lnapi([subscribe]). % LOGGING AND NOTIFICATION ADMIN API

% subscribe
lnapi_subscribe(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
			token(Token,[atom,optional(true)])
			]),
	    _, ( std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	( nonvar(Token) -> authenticate(rmv_ln,Token) ; true ), % TODO decide whether to have distinct ln token
	subscribe(_,_,_,_,_,_),
	!.
lnapi_subscribe(_) :- audit_gen(logging_notification, subscribe(failure)).

subscribe(_,_,_,_,_,_) :-
	true.

