% RMV server process

:- module(rmv_server, [rmv_server_cmd/0,rmv_server_cmd/1,
	rmv_server_cmd/2,rmv_server_cmd/3,rmv_server_with_args/1]).

:- use_module('AUDIT/audit',[audit_gen/3]).
:- use_module('COM/param').
:- use_module('RMV/rmv').
%:- use_module('COM/jsonresp').


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
%:- use_module(library(http/http_wrapper)).
%:- use_module(library(http/http_header)).
%:- use_module(library(http/http_parameters)).


% rmv-server command line options
%
%    --port    --portnumber -p    <integer>
%    --initialfile -i   <filename>   % not currently used, could be a global rmv config file
%    --selftest -s
%    --token   -t    <rmvtoken>
%    --verbose  -v
%    --jsonresp -j
%    --epp      -e
%    --context  -c   <URL of Context System>
%    --nurvsim  -n
%
rmv_server_opt_spec([
        [opt(portnumber), meta('RP'), type(integer), shortflags([p]), longflags(['port','portnumber']),
         help( 'server listens for API calls on port RP' )],
        [opt(selftest), type(boolean), default(false), shortflags([s]), longflags(['selftest']),
         help( 'run self tests on startup' )],
        [opt(token), meta('TOKEN'), type(atom), shortflags([t]), longflags(['token']),
         help( 'requests must cite TOKEN' )],
        [opt(verbose), type(boolean), default(false), shortflags([v]), longflags(['verbose']),
         help( 'show all messages' )],
        [opt(jsonresp), type(boolean), default(true), shortflags([j]), longflags(['jsonresp']),
         help( 'JSON-encoded responses' )],
        [opt(epp), type(boolean), default(true), shortflags([e]), longflags(['epp']),
         help( 'enable Event Processing Point' )],
        [opt(context), meta('URL'), type(atom), shortflags([c]), longflags(['context']),
	 	 help( 'URL of Context system' )],
	% the following are only for testing
		% [opt(awake), type(boolean), default(false), shortflags([a]), longflags(['awake','nosleep']),
		%  help( 'stay awake in top-level loop after starting server' )],
		[opt(nurvsim), type(boolean), default(false), shortflags([n]), longflags(['nurvsim']),
		  help( 'NuRV simulation' )],
		[opt(guitracer), type(boolean), default(false), shortflags([g]), longflags(['guitracer']),
		 help( 'enable GUI tracer' )]
]).

:- dynamic rmv_server_options/1.
rmv_server_options([]).

% rmv_server/0, rmv_server/1, rmv_server/2 and rmv_server/3 are called by command:do
%
% rmv_server_with_args/1 is called by rmv:rmv_server
%
rmv_server_cmd :-
	param:rmv_port(Port), % use same port for all rmv APIs
	rmv_server_cmd(Port).

rmv_server_cmd(nurvsim) :- !,
	param:setparam(rmv_nurv_simulation,true), rmv_server_cmd.

rmv_server_cmd(Port) :-
	%param:build_version(rmv,Vnum), format('rmv-server version ~a starting~n',Vnum),
	%create_server_audit_log,
	(   param:guiserver(on)
	->  trace
	;   true
	),
	(   param:rmv_run_with_http_server(true)
	->  param:rmv_token(Rtoken),
		%param:setparam(epp_status,rmv_server),
		Opts = [portnumber(Port),jsonresp(true),token(Rtoken),epp(true)],
		( param:rmv_nurv_simulation(true) -> Opts1 = [nurvsim(true)|Opts] ; Opts1 = Opts ),
		param:setparam(sleep_after_server_start,off),
		rmv_server_with_opts(Opts1)
	;   true
	).

run_http_rmv_server(Port) :-
	http_server(http_dispatch, [port(Port)]),
	format('rmv-server listening on port ~d~n',[Port]),
	audit_gen(rmv, rmv_start, success),
	param:setparam(epp_status,rmv_server),
	epp:epp_with_rmv,
	(   param:sleep_after_server_start(on)
	->  param:server_sleeptime(S), go_to_sleep(S)
	;   true
	).

rmv_server_cmd(Port,RToken) :-
	param:setparam(rmv_token,RToken),
	rmv_server_cmd(Port).

rmv_server_cmd(Port,RToken,EToken) :-
	param:setparam(rmv_epp_token,EToken),
	rmv_server_cmd(Port,RToken).

rmv_server_with_args(Argv) :-
	% process the arguments
	rmv_server_opt_spec(OptSpec),
	catch(
	    opt_parse(OptSpec,Argv,Opts,_Positionals),
	    E, writeln('error in command line arguments')),
	!,
	(   nonvar(E)
	->  halt(1)
	;   retractall(rmv_server_options(_)), assert(rmv_server_options(Opts))
	),
	rmv_server_with_opts(Opts).

rmv_server_with_opts(_) :- param:rmv_server_is_running(true), !,
	writeln('RMV server is already running').
rmv_server_with_opts(Opts) :-
	process_id(Pid),
	param:build_version(rmv,Vnum), format('rmv-server version ~a starting pid=~d~n',[Vnum,Pid]),
	format('Options=~q~n',[Opts]),
	(   memberchk(portnumber(RPort),Opts); true ),
	(   var(RPort)
	->  param:rmv_port(RPort)
	;   param:setparam(rmv_port,RPort)
	),

	(   memberchk(context(CTX_URL),Opts); true ),
	(   var(CTX_URL)
	->  true % param:context_url(CTX_URL)
	;   param:setparam(context_url,CTX_URL)
	),

	(   memberchk(verbose(true),Opts)
	->  param:setparam(verbose,on)
	;   param:setparam(verbose,off)
	),

	(   memberchk(jsonresp(true),Opts)
	->  param:setparam(jsonresp_server,on), % turns on JSON responses for server
	    param:setparam(jsonresp,on)
	;   param:setparam(jsonresp_server,off),
	    param:setparam(jsonresp,off)
	),

	(   memberchk(nurvsim(true),Opts)
	->  param:setparam(rmv_nurv_simulation,true) % force to true
	;   true % leave whatever is set in param
	),

	(   memberchk(epp(true),Opts)
	->  param:setparam(epp_status,rmv_server) % activate EPP as part of rmv server
	;   true
	),

	(   memberchk(selftest(true),Opts) % currently ignored
	->  param:setparam(self_test,on)
	;   param:setparam(self_test,off)
	),

	(   memberchk(token(Token),Opts); true ),
	(   atom(Token)
	->  param:setparam(rmv_token,Token)
	;   true
	),

	(   memberchk(guitracer(true),Opts)
	->  guitracer
	;   true
	),

	% (   memberchk(awake(true),Opts)
	% ->  param:setparam(sleep_after_server_start,off)
	% ;   true
	% ),

	create_server_audit_log,
	http_server(http_dispatch, [port(RPort)]),
	param:setparam(rmv_server_is_running,true),
	format('rmv-server listening on port ~d~n',[RPort]),
	audit_gen(rmv, rmv_start, success),

	% run self-test here if turned on in param or command line

    (   param:epp_status(rmv_server)
	->  epp:epp_with_rmv
	;   true
	),

	(   param:sleep_after_server_start(on)
	->  param:server_sleeptime(S), go_to_sleep(S)
	;   true
	).

go_to_sleep(S) :-
	sleep(S),
	periodic_goals,
	go_to_sleep(S).

periodic_goals :-
	% add periodic Policy Server goals here
	true.

create_server_audit_log :- param:audit_logging(file), !,
	audit:gen_time_stamp(TS),
	param:log_directory_name(LogD),
	atomic_list_concat([LogD,'/rmv_audit_log','_',TS],LogFile),
	format('Audit log file: ~w~n',LogFile),
	open(LogFile,append,AudStream),
	param:setparam(audit_stream,AudStream).
create_server_audit_log.
