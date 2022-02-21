% Event Processing Point
%   http server startup

:- module(epp, [epp/1,epp/2,epp_with_args/1,epp_with_server/0,load_erp/2,load_erp_immediate/2,
		unload_erp/1,epp_log_gen/2]).

:- use_module('COM/param').
:- use_module(erl).
:- use_module(epp_era).
%:- use_module(epp_cpa).
:- use_module(epp_pcc).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% ------------------------------------------------------------------------
% epp server - called by module ngac or command
%
%
% epp command line options
%
%    --port           -p <integer>
%    --erp --load     -e <filename>
%    --context        -x <filename>
%    --conditions     -c <filename>
%    --selftest       -s
%    --token          -t <admintoken>
%    --verbose        -v
%    --jsonresp       -j
%
epp_opt_spec([
        [opt(eppportnumber), meta('EP'), type(integer), shortflags([p]), longflags(['port']),
         help( 'epp listens for admin and event requests on port EP' )],
        [opt(erpfile), meta('FILE'), type(atom), shortflags([l,e]), longflags(['load','erp']),
         help( 'load event-response package from FILE' )],
        [opt(contextfile), meta('FILE'), type(atom), shortflags([x]), longflags(['context']),
         help( 'context definitions FILE' )],
        [opt(conditionsfile), meta('FILE'), type(atom), shortflags([c]), longflags(['conditions']),
         help( 'condition definitions FILE' )],
        [opt(selftest), type(boolean), default(false), shortflags([s]), longflags(['selftest']),
         help( 'run self tests on startup' )],
        [opt(token), meta('TOKEN'), type(atom), shortflags([t]), longflags(['token']),
         help( 'epp requests must cite TOKEN' )],
        [opt(verbose), type(boolean), default(false), shortflags([v]), longflags(['verbose']),
         help( 'show all messages' )],
        [opt(jsonresp), type(boolean), default(false), shortflags([j]), longflags(['jsonresp']),
         help( 'JSON-encoded responses' )],
        [opt(context), meta('URL'), type(atom), shortflags([]), longflags(['context','crosscpp']),
	 help( 'URL of Context Monitoring and Extraction system' )]
]).

:- dynamic epp_options/1.
epp_options([]).

init :- param:epp_initialized(true), !.
init :-
	create_epp_log,
	epp_cpa:init,
	epp_era:init,
	setparam(epp_initialized,true).

epp(Port) :-
	setparam(epp_status,standalone),
	init, % create_epp_log,
	http_server(http_dispatch, [port(Port)]),
	format('EPP listening on port ~d~n',[Port]),
	epp_log_gen(epp_start, success),
	param:server_sleeptime(S), my_sleep(S).

epp(Port,Token) :-
	param:setparam(epp_token,Token),
	epp(Port).

epp_with_server :-
	epp_with_opts([]).

epp_with_rmv :-
	param:rmv_port(Rport), param:rmv_token(Rtoken),
	param:setparam(eppapi_port,Rport),
	epp_with_opts([eppportnumber(Rport),jsonresp(true),token(Rtoken)]).

epp_with_args(Argv) :-
	% format('EPP Argv: ~q~n',[Argv]),
	% process the arguments
	epp_opt_spec(OptSpec),
	catch(
	    opt_parse(OptSpec,Argv,Opts,_Positionals),
	    E, writeln('error in command line arguments')),
	!,
	(   nonvar(E)
	->  halt(1)
	;   retractall(epp_options(_)), assert(epp_options(Opts))
	    % ,format('Opts: ~q~nPositionals: ~q~n',[Opts,Positionals])
	),
	setparam(epp_status,standalone),
	epp_with_opts(Opts).

epp_with_opts(Opts) :-
	(   memberchk(eppportnumber(EPort),Opts); true ),
	(   var(EPort)
	->  param:pqapi_port(EPort)
	;   param:setparam(pqapi_port,EPort)
	),

	(   memberchk(context(CTX_URL),Opts); true ),
	(   var(CTX_URL)
	->  true % param:context_url(CTX_URL)
	;   param:setparam(context_url,CTX_URL)
	),

	(   memberchk(verbose(true),Opts)
	->  param:setparam(verbose,on) % turns on verbose globally
	;   param:setparam(verbose,off)
	),

	(   memberchk(jsonresp(true),Opts)
	->  param:setparam(jsonresp_epp,on) % turns on JSON responses for EPP
	;   param:setparam(jsonresp_epp,off)
	),

	(   memberchk(selftest(true),Opts) % currently ignored
	->  param:setparam(self_test,on) % turns on self_test globally
	;   param:setparam(self_test,off)
	),

	(   memberchk(token(Token),Opts); true ),
	(   atom(Token)
	->  param:setparam(epp_token,Token)
	;   true
	),
	init, % create_epp_log,
	param:epp_status(EPP_status),
	(   \+ ( EPP_status == policy_server ; EPP_status == rmv_server )
	->  param:setparam(epp_status,standalone),
	    format('EPP server starting on port ~d~n',[EPort]),
	    http_server(http_dispatch, [port(EPort)]),
	    format('Epp server started~n'),
	    epp_log_gen(epp_started, standalone)
	;   format('EPP listening on port ~d~n',[EPort]),
	    epp_log_gen(epp_started, EPP_status)
	),

	% run self-test here if turned on in param or command line
	% ( param:self_test(on) -> true ; true ),

	(   memberchk(erpfile(Efile),Opts) ; true ),
	(   var(Efile)
	->  true % initial erp file not specified
	;   (   ( atom(Efile), exists_file(Efile) )
	    ->	erl:load_er_package(Efile,ERPname),
		param:setparam(current_erp,ERPname),
		format('ERP ~q loaded from ~a~n',[ERPname,Efile]),
		epp_log_gen(event_processing, erploadopt(Efile,ERPname,success))
	    ;   format('ERP file load error: ~a~n',[Efile]),
		epp_log_gen(event_processing, erploadopt(Efile,failure))
	    )
	),

	(   param:epp_status(standalone)
	->  param:server_sleeptime(S), my_sleep(S)
	;   true
	).

epp_internal(_Opts) :- true.

my_sleep(S) :-
	sleep(S),
	periodic_goals,
	my_sleep(S).

periodic_goals :-
	% add periodic EPP goals here
	true.

create_epp_log :- param:epp_logging(file), !,
	audit:gen_time_stamp(TS), % TODO - add leading zeros to single digit quantities
	param:log_directory_name(LogD),
	atomic_list_concat([LogD,'/epp_log','_',TS],LogFile),
	format('EPP log file: ~w~n',LogFile),
	open(LogFile,append,LogStream),
	param:setparam(epp_stream,LogStream).
create_epp_log :- param:epp_logging(on), !,
	format('EPP logging to console~n').
create_epp_log :- !.

epp_log_gen(LogEvent, LogData) :-
	audit:gen_time_stamp(TS),
	(   param:epp_logging(file)
	->  param:epp_stream(Log),
	    format(Log, 'epp_log(~w, ~q, ~q).~n',[TS,LogEvent,LogData]),
	    flush_output(Log)
	;
	    format('epp_log(~w, ~q, ~q).~n',[TS,LogEvent,LogData]),
	    flush_output
	),
	audit_gen(LogEvent, LogData),
	!.


% ------------------------------------------------------------------------
% EPP TOP LEVEL INTERFACES
%   these are called by the Web APIs in module eppapi

load_erp(ERPfile,ERPname) :-
	erl:load_er_package(ERPfile,ERPname),
	activate_loaded_erp(ERPname),!.

load_erp_immediate(ERPspec,ERPname) :-
	erl:load_er_package_immediate(ERPspec,ERPname),
	activate_loaded_erp(ERPname),!.

unload_erp(ERPname) :-
	deactivate_loaded_erp(ERPname),
	erl:unload_er_package(ERPname),!.

% other interfaces called by eppapi are provided by epp_era


