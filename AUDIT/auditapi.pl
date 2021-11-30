% AUDIT API
:- module(auditapi, []).

:- use_module(audit).
:- use_module('COM/param').
:- use_module('COM/apiresp').
%:- use_module(sessions).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Audit Web API
:- http_handler(root(audit), root_apis(audit), []).
:- http_handler(root('audit/'), api_unimpl, [prefix]).
:- http_handler(root(audit/control), auditapi_control, [prefix]).
:- http_handler(root(audit/select), auditapi_select, [prefix]).
:- http_handler(root(audit/logfile), auditapi_logfile, [prefix]).
:- http_handler(root(audit/gen), auditapi_gen, [prefix]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).

% AUDIT APIs
auditapi([control,select,logfile,gen]).

%
% Audit API
%


% control
auditapi_control(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[audit_op(Operation,[atom]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(audit,Token)
	->  control(Operation), !
	;   true
	).
auditapi_control(_) :- audit_gen(audit, control(failure)).

control(X) :-
	(   true
	->  std_resp_MS(success,'audit operation complete',X),
	    audit_gen(audit, control(x, y, success))
	;   std_resp_MS(failure,'audit operation',X),
	    audit_gen(audit, control(x, y, failure))
	).


% select
auditapi_select(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[events(Events,[atom]),
                                     %delete_events(Events,[atom,optional(true)]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(audit,Token)
	->  select(Events), !
	;   true
	).
auditapi_select(_) :- audit_gen(audit, select(failure)).

select(X) :-
	(   true
	->  std_resp_MS(success,'element added',X),
	    audit_gen(audit, select(x, y, success))
	;   std_resp_MS(failure,'error adding element',X),
	    audit_gen(audit, select(x, y, failure))
	).


% logfile
auditapi_logfile(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[file(File,[atom]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  logfile(File), !
	;   true
	).
auditapi_logfile(_) :- audit_gen(audit, logfile(failure)).

logfile(X) :-
	(   true
	->  std_resp_MS(success,'element added',X),
	    audit_gen(audit, logfile(x, y, success))
	;   std_resp_MS(failure,'error adding element',X),
	    audit_gen(audit, logfile(x, y, failure))
	).


% gen
auditapi_gen(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[source(Source,[atom]),
                                     event(Event,[atom]),
                                     data(Data,[atom]),
				     token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(audit,Token)
	->  gen(Source,Event,Data), !
	;   true
	).
auditapi_gen(_) :- audit_gen(audit, gen(failure)).

gen(_S,X,_Y) :-
	(   true
	->  std_resp_MS(success,'element added',X),
	    audit_gen(audit, gen(x, y, success))
	;   std_resp_MS(failure,'error adding element',X),
	    audit_gen(audit, gen(x, y, failure))
	).



read_term_from_atom_in_list([],[]).
read_term_from_atom_in_list([Elt|Elts],[TElt|TElts]) :-
	read_term_from_atom(Elt,TElt,[]),
	read_term_from_atom_in_list(Elts,TElts).
