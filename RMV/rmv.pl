% Runtime Monitoring and Verification

:- module(rmv, [rmv/0,rmv/1,rmv/4,rmv_server/0]).
:- use_module([
       %rmv_server,
       'AUDIT/audit','AUDIT/auditapi',
       rmv_lnapi,rmv_mrapi,rmv_mcapi,
       rmv_aa,rmv_la,rmv_na,
       rmv_ml,       % rmv_ml_mt,rmv_ml_pst,
       rmv_mc,       % rmv_mc_cm,rmv_mc_cps,
       rmv_mf,       % rmv_mf_epp,
       'COM/param','COM/command','COM/test','COM/procs'%,
       %'EPP/erl','EPP/epp','EPP/epp_cpa','EPP/epp_era','EPP/eppapi'
   ]).
:- use_module('SIM/ext_svcs').

% :- include(rmv_test).

% :- style_check(-singleton).
% :- initialization(rmv).
%

:- set_prolog_flag(verbose, silent).

% These are the main entry points to RMV
% Other special entry points may also be defined here
%
rmv :- % most typical entry
	get_command_args(_Argv),
	rmv(_,_,_,_), !.
rmv :- halt(1).

% can be invoked with directives: (could do to allow a set of directives)
rmv(self_test) :- !, rmv(on,off,on,_).
rmv(regression_test) :- !, rmv(off,on,on,_).
rmv(no_initial) :- !, rmv(off,off,off,_).
rmv(verbose) :- !, rmv(_,_,_,on).

rmv(Selftest,Regression,Init,Verbose) :-
	(   var(Selftest) -> param:self_test(Selftest) ; true ),
	(   var(Regression) -> param:regression_test(Regression) ; true),
	(   var(Init) -> param:initialize(Init) ; true ),
	(   var(Verbose) -> param:verbose(Verbose) ; true ),

	(   Verbose == on
	-> format('self_test=~a regression_test=~a initialize=~a verbose=~a~n',
		  [Selftest,Regression,Init,Verbose])
	; true),

	(   Init == on
	-> initialize_rmv
	; true ),

	(   Selftest == on
	->  self_test_rmv
	;   true ),

	(   Regression == on
	->  regression_test_rmv
	;   true ),

	(   param:guitracer(on)
	->  guitracer
	;   true ),

	param:prompt_string(rmv,Prompt), param:setparam(prompt_string,Prompt),
	command:tl(rmv). % run the top-level command interpreter

rmv_server :-
	get_command_args(Argv),
	initialize_rmv,
	rmv_server_with_args(Argv).

get_command_args(Argv) :-
	current_prolog_flag(argv, Argv),
	% format('Argv: ~q~n',[Argv]),
	true.

% Initialization
%

:- dynamic rmv_initialized/1.
rmv_initialized(false).

% The following initialization (init) is when this module is initialized
% as part of a larger system. The initialization (initialize_all) is
% when rmv IS the system.
%
init:- rmv_initialized(true), !. % already initialized
init :-
	% ...
        retractall(rmv_initialized(_)), assert(rmv_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(rmv_initialized(_)), assert(rmv_initialized(false)).

% initialize_once - do these things only once per run
initialize_once :- param:initialized(true), !.
initialize_once :-
	open_null_stream(Null), param:setparam(null_stream,Null),
	true.

% RMV is the top-level module, so initialize all subsystems and modules
% requiring startup initialization
initialize_rmv :-
	init,
	% ui:notify(initialize,all),
	initialize_once,
	audit:init(full), % basic or full
	% following should properly be conditional on param:rmv_status
	% and how the server and rmv are started
	% ...
	param:setparam(initialized,true).

% Test
%
self_test_rmv :-
	% test:self_test(rmv),
	% writeln('self tests not yet defined for rmv'),
	self_test,
	true.

regression_test_rmv :-
	% test:regression_test(rmv),
	% writeln('regression tests not yet defined for rmv'),
	regression_test,
	true.

%
% links to external services
%

% Retrieve service spec
%
get_service_spec(ServiceCreationContext, ServiceSpec) :-
	ext_get_service_spec(ServiceCreationContext,ServiceSpec).

% Service creation
%
service_spec2service(ServiceSpec, Service) :-
	ext_service_spec2service(ServiceSpec, Service).

% Deployment
%
deploy_service_with_monitor(Service, Monitor, Deployment) :-
	ext_deploy_service_with_monitor(Service, Monitor, Deployment).

