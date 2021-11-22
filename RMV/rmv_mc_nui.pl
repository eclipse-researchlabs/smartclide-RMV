% interface to NuRV (and nuXmvm NuSMV)
% for human and automated interaction

:- module(rmv_mc_nui,[nurv_monitor_init/3,
		      open_nurv_session/1,quit_nurv_session/1,close_nurv_session/1,
		      nurv_session_cmd/2,nurv_session_cmd_resp/2,nurv_session_get_resp/1
		     ]).

:- use_module(['COM/param','COM/ui','COM/sessions',rmv_ml,rmv_mc]).

% Interactive
%
% enter top level loop for live developer interaction with NuRV session
nu_tl(Session) :-
	nurv_session(Session,ToS,FrS),
	nu_tl(Session,ToS,FrS).

% nu_tl/3 ends either by typing quit or an end_of_file
% quit will cause NuRV to terminate
nu_tl(Sid,ToNU,FrNU) :-
	param:local_NuRV_prompt(NuRVp),
	param:raw_read_delay(Delay),
	read_nu_line(Delay,FrNU,NuL,LenL),
	write(NuL), flush_output,
	Pos is LenL - 7,
	(   Pos >= 0, sub_atom(NuL,Pos,7,_,NuRVp) % NuL ends with NuRV prompt
	->  read_line_to_string(user_input,UL),
	    (	UL == end_of_file -> nurv_session_cmd(Sid,quit),fail ; true ),
	    nurv_session_cmd(Sid,UL),
	    (	UL == "quit" -> fail ; true )
	;   true
	),
	nu_tl(Sid,ToNU,FrNU).
nu_tl(_,_,_).

read_nu_line(Slp,S,Lines,Len) :- % read a buffer of the stream from NuRV
	sleep(Slp), % a short delay of less than a second
	with_tty_raw((fill_buffer(S),read_pending_codes(S,Codes,T))), T=[], length(Codes,Len),
	atom_codes(Lines,Codes).

% send a NuRV command to a session
nurv_session_cmd(Sid,Cmd) :-
	nurv_session(Sid,ToS,_),
	writeln(ToS,Cmd), flush_output(ToS).

nurv_session_cmd_resp(Sid,Cmd) :-
	nurv_session(Sid,ToS,_),
	writeln(ToS,Cmd), flush_output(ToS),
	writeln(Cmd), flush_output,
	nurv_session_get_resp(Sid).

nurv_session_get_resp(Sid) :-
	nurv_session(Sid,_,FrS),
	param:local_NuRV_prompt(NuRVp),
	param:raw_read_delay(Delay),
	read_nu_line(Delay,FrS,NuL,LenL),
	write(NuL), flush_output,
	Pos is LenL - 7,
	(   Pos >= 0, sub_atom(NuL,Pos,7,_,NuRVp)
	->  true
	;   nurv_session_get_resp(Sid)
	).

nurv_monitor_init(Infile,Ordfile,Sid) :-
	atomic_list_concat(['set input_file ',Infile],Cmd1),
	atomic_list_concat(['set input_order_file ',Ordfile],Cmd2),
	nurv_session_cmd_resp(Sid,Cmd1),
	nurv_session_cmd_resp(Sid,Cmd2),
	nurv_session_cmd_resp(Sid,go),
	nurv_session_cmd_resp(Sid,'build_monitor -n 0'),
	true.

% session tracking
:- dynamic nurv_session/3.
nurv_session(sid,to_stream,from_stream). % sid is pid as an atom

open_nurv_session(SessionId) :- % open interactive NuRV session
	param:local_NuRV(_,NuRV),
	process_create(path(NuRV),['-quiet', '-int'],
		       [process(NuRVpid),stdin(pipe(ToStream)),stdout(pipe(FromStream))]),
	atom_number(SessionId,NuRVpid),
	init_session(SessionId, monitor_framework),
	( param:verbose(on) -> format('NuRV session ~a~n',SessionId) ; true ),
	assert( nurv_session(SessionId,ToStream,FromStream) ).

quit_nurv_session(SessionId) :- % send quit command, then close
	nurv_session_cmd(SessionId,quit),
	close_nurv_session(SessionId).

close_nurv_session(SessionId) :- % only close the session
	nurv_session(SessionId,ToStream,FromStream),
	close(ToStream), close(FromStream),
	(   is_session(SessionId, monitor_framework) -> end_session(SessionId) ; true ),
	retractall( nurv_session(SessionId,_,_) ),
	atom_number(SessionId,NuRVpid),
	process_wait(NuRVpid,Exit),
	writeln(Exit).

% Batch-style interaction with NuRV
%
% files for batch-style interaction
%   .cmd file - NuRV, NuSMV and nuXmv commands
%   .smv file - system model and LTL property specs
%   .ord file - variable order for BDDs
%   .xml file - system trace
% ...
%



% XML file (trace file) conversion
%
xml_trace(XMLfile,Trace) :-
	load_xml(XMLfile,XMLcontent,[]), struct_trace(XMLcontent,Trace).

struct_trace(XMLstruct,Trace) :-
	XMLstruct = [element(TraceName,_,_)],
	Trace = trace(TraceName,States),
	findall(state(Sid,Vars),
		(xpath(XMLstruct,//(state),S), S = element(state,[id=Sid],_),
		 findall(Var=VAL, (xpath(S,//(value),element(value,[variable=Var],[VAL]))), Vars)
		),
		States).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Deployment Simulation
%
%    Application + Monitor simulation
%
%    app/7
%	app(Aid,AppVars,AppTS,AppInputVars,AppOutputVars,AppCurrentInput,AppCurrentOutput)
%	        Aid - unique application Id
%		AppVars - all App variables including PC
%		AppTS - App transition system
%		AppInputVars - subset of AppVars that can be set by outside agents
%		AppOutputVars - subset of AppVars that can be seen by outside agents
%		AppCurrentInput - assignment of values to AppInputVars
%		AppCurrentOutput - assignment of values to AppOutputVars according to AppTS+AppCurrentInputi
%    mon/7
%	mon(Mid,MonState,MonTS,MonObservables,MonOut,MonCurrentInput,MonCurrentOutput)
%	        Mid - unique monitor Id
%		MonState - monitor state variables
%		MonTS - monitor transition system
%		MonObservables - subset of AppVars that are observable by the monitor
%		MonOut - output of the monitor according to MonTS+MonCurrentInput
%		MonCurrentInput - assignment of values to MonObservables
%		MonCurrentOutput - assignment of value to MonOut
%
%    app_run/2,3,4
%       app_run(Session,Trace)
%
%	app_run(App,Monitor,Trace)
%
%	app_run(App,Monitor,AppTrace,MonTrace)
%

% app_run/3 - use run_trace/2 (console output only)
% app_run/3 -> run_trace/2 -> run_tract_do/3
app_run(App,Monitor,Trace) :-
	is_app(App,_AppId,_AppVars,_AppTS,_AppInputVars,_AppOutputVars,_AppCurrentInput,_AppCurrentOutput),
	is_monitor(Monitor,_MonId,_MonState,_MonTS,_MonObservables,_MonOut,_MonCurrentInput,_MonCurrentOutput),
	is_trace(Trace, _TraceId, States),
	run_trace(Monitor,States). % console output only

% app_run/2 - use run_trace/3  (use NuRV monitor)
% app_run/2 -> run_trace/3 -> run_tract_do/4
app_run(Session, Trace) :-
	Trace = trace(_TraceId, States),
	run_trace(default_monitor,States,Session).

% run_trace/2 - for each state run_trace_do/3
run_trace(_,[]) :- !.
run_trace(Mon,[State|States]) :-
	run_trace_do(Mon,State,NewMon), % console output only
	run_trace(NewMon,States).

% run_trace/3 - for each state run_trace_do/4
run_trace(_,[],_Sid) :-
	% quit the session
	!.
run_trace(Mon,[State|States],Sid) :-
	run_trace_do(Mon,State,NewMon,Sid),
	run_trace(NewMon,States,Sid).

% run_trace_do/3 just output the heartbeat info showing all variables
run_trace_do(Mon,State,NewMon) :-
	State = state(N, Vars),
	format('heartbeat ~w ~q~n',[N,Vars]),
	NewMon = Mon.

% run_trace_do/4 send heartbeat command to NuRV with true variable
run_trace_do(Mon,State,NewMon,Sid) :-
	State = state(_, Vars),
	memberchk(V='TRUE',Vars),
	format(atom(Cmd),'heartbeat -n 0 -c "~w"',[V]),
	nurv_session_cmd_resp(Sid,Cmd),
	%writeln(Cmd),
	NewMon = Mon.

% tests
%
% test - open interactive NuRV session, enter top-level loop relaying comms
%        close the session after user quits the interactive loop
%
% test2 - open and quit interactive NuRV session
%
% test3 - convert XML trace file and simulate run printing heartbeat info
%
% test4 - convert XML trace file & truncate, open NuRV session, create
%	  monitor and run trace sending each state to monitor, close
%	  session
%
test :- open_nurv_session(Sid), format('NuRV session ~a~n',Sid),
	nu_tl(Sid), close_nurv_session(Sid), writeln('session ended'), !.

test2 :- open_nurv_session(Sid), format('NuRV session ~a~n',Sid),
	quit_nurv_session(Sid), writeln('session ended'), !.

test3 :- % test trace conversion and app_run stubs
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/','disjoint_trace.xml'],TraceFile),
	xml_trace(TraceFile,Trace),
	app_run(_App,_Mon,Trace). % this version only outputs heartbeat info

test4 :- % test trace interactively with NuRV monitor
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/','disjoint_trace.xml'],TraceFile),
	xml_trace(TraceFile,Trace),
	truncate_trace(Trace,TTrace),
	open_nurv_session(Sid), % format('NuRV session ~a~n',Sid),
	nurv_session_get_resp(Sid),
	% need to initialize the session with the monitor
	atomic_list_concat([MD,'/','disjoint.smv'],SMVFile),
	atomic_list_concat([MD,'/','disjoint.ord'],OrdFile),
	nurv_monitor_init(SMVFile,OrdFile,Sid),
	app_run(Sid,TTrace),
	%nu_tl(Sid),close_nurv_session(Sid),
	quit_nurv_session(Sid),
	writeln('session ended'),
	!.
