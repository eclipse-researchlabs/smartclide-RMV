% ------------------------------------------------------------------------
% EPP WEB APIS
:- module(eppapi, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).
:- use_module(epp).
:- use_module(epp_era).
:- use_module(epp_cpa, [context_change_notification/1]).
:- use_module('AUDIT/audit').
:- use_module('COM/apiresp').

% EPP WEB APIs - note mapping of path names to internal predicate names
%
% management API
:- http_handler(root(epp), root_apis(eppapi), []).
:- http_handler(root('epp/'), api_unimpl, [prefix]).
:- http_handler(root(epp/load_erf), epp_load_er_file, [prefix]).
:- http_handler(root(epp/loadi_erp), epp_load_immediate_er_package, [prefix]).
:- http_handler(root(epp/unload_erp), epp_unload_er_package, [prefix]).
:- http_handler(root(epp/activate_erp), epp_activate_er_package, [prefix]).
:- http_handler(root(epp/deactivate_erp), epp_deactivate_er_package, [prefix]).
:- http_handler(root(epp/current_erp), epp_current_er_package, [prefix]).
%   event reporting API
:- http_handler(root(epp/report_event), epp_report_event, [prefix]).
%   context change notification API
:- http_handler(root(epp/context_notify), epp_context_notify, [prefix]).

eppapi([load_erf, loadi_erp, unload_erp, activate_erp, deactivate_erp, current_erp, report_event, context_notify]).

%
% EPP API
% package manipulation
%   load_erf
%   loadi_erp
%   unload_erp
%   activate_erp
%   deactivate_erp
%   current_erp
%
%   report_event
%   context_notify
%

% load_er_file
epp_load_er_file(Request) :- % load erp file
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erpfile(Efile,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  load_er_file(Efile), !
	;   true
	).
epp_load_er_file(_) :- epp_log_gen(event_processing, load_er_file(failure)).

load_er_file(Efile) :-
	(   ( exists_file(Efile), load_erp(Efile,ERPname) )
	->  std_resp_BS(success,'ER package loaded from file',(Efile,ERPname)),
	    audit_gen(event_admin, load_er_file(Efile,ERPname,success))
	;   std_resp_MS(failure,'ER file or load error',Efile),
	    audit_gen(event_admin, load_er_file(Efile,failure))
	).

% load_immediate_er_package
epp_load_immediate_er_package(Request) :- % load erp immediate
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erp(Erp,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  load_immediate_er_package(Erp), !
	;   true
	).
epp_load_immediate_er_package(_) :- epp_log_gen(event_processing, load_immediate_er_package(failure)).

load_immediate_er_package(Erp) :-
	(   ( ground(Erp), load_erp_immediate(Erp,ErpName) )
	->  std_resp_BS(success,'ER package loaded immediate',ErpName),
	    % policyio:display_policy(ErpName), % temporary for testing
	    epp_log_gen(event_processing, loadi_erp(ErpName,success)),
	    audit_gen(event_admin, loadi_erp(ErpName,success))
	;   std_resp_MS(failure,'malformed ER package or load error',Erp),
	    epp_log_gen(event_processing, loadi_erp(Erp,failure)),
	    audit_gen(event_admin, load_immediate_er_package(failure))
	).

% unload_er_package
epp_unload_er_package(Request) :- % unload erp
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erpname(ERPname,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  unload_er_package(ERPname), !
	;   true
	).
epp_unload_er_package(_) :- epp_log_gen(event_processing, unload_er_package(failure)).

unload_er_package(ERPname) :-
	(   erl:er_package(ERPname,_)
	->  unload_erp(ERPname),
	    std_resp_BS(success,'ER package unloaded',ERPname),
	    epp_log_gen(event_processing, unload_er_package(ERPname,success))
	;   std_resp_MS(failure,'unknown ER package',ERPname),
	    audit_gen(event_admin, unload_er_package(ERPname,failure)),
	    epp_log_gen(event_processing, unload_er_package(ERPname,failure))
	).

% activate_er_package
epp_activate_er_package(Request) :- % activate a loaded erp
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erpname(ERPname,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  activate_er_package(ERPname), !
	;   true
	).
epp_activate_er_package(_) :- epp_log_gen(event_processing, activate_er_package(failure)).

activate_er_package(ERPname) :-
	(   erl:er_package(ERPname,_)
	->  activate_loaded_erp(ERPname),
	    std_resp_BS(success,'ER package activated',ERPname),
	    epp_log_gen(event_processing, activate_er_package(ERPname,success))
	;   std_resp_MS(failure,'unknown ER package',ERPname),
	    audit_gen(event_admin, activate_er_package(ERPname,failure)),
	    epp_log_gen(event_processing, activate_er_package(ERPname,failure))
	).

% deactivate_er_package
epp_deactivate_er_package(Request) :- % deactivate a loaded erp
	std_resp_prefix,
	catch(
	    http_parameters(Request,[erpname(ERPname,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  deactivate_er_package(ERPname), !
	;   true
	).
epp_deactivate_er_package(_) :- epp_log_gen(event_processing, deactivate_er_package(failure)).

deactivate_er_package(ERPname) :-
	(   erl:er_package(ERPname,_)
	->  deactivate_loaded_erp(ERPname),
	    std_resp_BS(success,'ER package deactivated',ERPname),
	    epp_log_gen(event_processing, deactivate_er_package(ERPname,success))
	;   std_resp_MS(failure,'unknown ER package',ERPname),
	    audit_gen(event_admin, deactivate_er_package(ERPname,failure)),
	    epp_log_gen(event_processing, deactivate_er_package(ERPname,failure))
	).

% current_er_package
epp_current_er_package(Request) :- % get current ERP name
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  param:current_erp(Erp),
	    std_resp_BS(success,'current ER package',Erp), !
	;   true
	).
epp_current_er_package(_) :- epp_log_gen(event_processing, current_er_package(failure)).

%current_er_package :- % in-lined above
%	param:current_erp(Erp),
%	std_resp_BS(success,'current ER package',Erp).

% report_event
epp_report_event(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[event(Ename,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  read_term_from_atom(Ename,Eterm,[]),
	    (   report_event(Eterm)
	    ->  std_resp_BS(success,'event reported',Eterm),
		epp_log_gen(event_processing, report_event(Eterm,success))
	    ;   std_resp_BS(failure,'event not reported',Eterm),
		epp_log_gen(event_processing, report_event(Eterm,failure))
	    ), !
	;   true
	).
epp_report_event(_) :- epp_log_gen(event_processing, report_event(failure)).

% context_notify
epp_context_notify(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[context(ContextAtom,[atom]),token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate_epp(Token)
	->  context_notify(ContextAtom), !
	;   true
	).
epp_context_notify(_) :- epp_log_gen(event_processing, context_notify(failure)).

context_notify(ContextAtom) :-
	read_term_from_atom(ContextAtom,Context,[]),
	(   context_change_notification(Context)
	->  std_resp_MS(success,'context change notification accepted',Context),
	    epp_log_gen(event_processing, context_notify(Context,success))
	;   std_resp_BS(failure,'context change notification rejected',Context),
	    epp_log_gen(event_processing, context_notify(failure))
	).


authenticate_epp(Token) :-
	(   authenticate_epp_token(Token)
	->  true
	;   std_resp_M(failure,'authentication error',''),
	    audit_gen(event_admin, 'authentication error'),
	    !, fail
	).

authenticate_epp_token(Token) :- atom(Token), param:epp_token(Token), !.

