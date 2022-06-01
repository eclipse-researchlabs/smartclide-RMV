% interface to NuRV (and nuXmvm NuSMV)
% for human and automated interaction
%
% includes a simple NuRV simulation for testing

:- module(rmv_mc_nui,[start_monitor/2, stop_monitor/2, heartbeat/4,
		      nurv_monitor_init/3,
			  nurv_session/5, nurv_session_log/2,
			  display_session_log/1, display_session_log/2, clear_session_log/1,
		      open_nurv_session/3, quit_nurv_session/1, close_nurv_session/1,
		      nurv_session_cmd/2, nurv_session_cmd_resp/3, nurv_session_get_resp/2
		     ]).

:- use_module(['COM/param','COM/ui','COM/sessions','RMV/rmv_na','RMV/rmv_ml',rmv_mc]).

nurv_simulation(false). % true/false 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Monitor Server Interactions
%
%   These functions are relayed here by rmv_mf_mep for
%   events received from monitor sensors to be sent to
%   property monitor servers
%

% start a NuRV server instance with instance id derived form monitor id
start_monitor(Mid,Status) :- % TODO
	start_monitor_server(Mid),
	Status = [monitor_started],
	!.
start_monitor(_Mid,[monitor_start_failure]) :-
	true.

% stop the NuRV server intance corresponding to monitor id
stop_monitor(Mid,Status) :-
	stop_monitor_server(Mid),
	Status = [monitor_stopped],
	!.
stop_monitor(_,[monitor_stop_failure]).

% pass the T atoms to the monitor server for a verdict
% send reportables to subscribers
heartbeat(Mid,Sid,AtomIds,Verdict) :-
	heartbeat_monitor_server(Mid,Sid,AtomIds,_Reset,Verdict),
	!.
heartbeat(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Communicate with appropriate monitor server instance
%
%    the full monitor ID FullMid will be used with the nameserver

start_monitor_server(Mid) :- % TODO
	monitor(Mid,_,_,_,_,_,_,_,_),
	%atom_concat('NuRV/Monitor/',Mid,_FullMid),
	true.

stop_monitor_server(Mid) :-
	monitor(Mid,_,_,_,_,_,_,_,_),
	%atom_concat('NuRV/Monitor/',Mid,_FullMid),
	true.

heartbeat_monitor_server(Mid,Sid,AtomIds,Reset,Verdict) :-
	monitor(Mid,_,_,_,_,_,_,_,_),
	%atom_concat('NuRV/Monitor/',Mid,_FullMid),
	% send NuRV heartbeat to session Sid and return verdict
	% dummy values for testing
	(   memberchk(p,AtomIds)
	->  ArgAtom = '-c p'
	;   (   memberchk(q,AtomIds)
        ->  ArgAtom = '-c q'
	    ;   ArgAtom = ''
	    )
	),
	format(atom(Cmd),'heartbeat -n 0 ~w',[ArgAtom]),
	monid_sessid_suniq(Mid,Sid,NuRVSid),
	nurv_session_cmd_resp(NuRVSid,Cmd,Verdict),
	%format('Response from NuRV heartbeat: ~q~n',Verdict),
	Reset = false, % to be determined by model/parameters
	true.

% Interactive
%
% enter top level loop for live developer interaction with NuRV session
nu_tl(Session) :-
	nurv_session(Session,_,_,ToS,FrS),
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
	nurv_session(Sid,_,_,ToS,_),
	writeln(ToS,Cmd), flush_output(ToS),
%	writeln(Cmd), flush_output,
	assertz( nurv_session_log(Sid,Cmd) ),
	true.

nurv_session_cmd_resp(Sid,Cmd,Resp) :-
	nurv_session(Sid,_,_,ToS,_),
	writeln(ToS,Cmd), flush_output(ToS),
%	writeln(Cmd), flush_output,
	assertz( nurv_session_log(Sid,Cmd) ),
	nurv_session_get_resp(Sid,Resp).

nurv_session_get_resp(Sid,Resp) :-
	nurv_session(Sid,_,_,_,FrS),
	param:raw_read_delay(Delay),
	read_nu_line(Delay,FrS,NuL,LenL),
%	write(NuL), flush_output,
	assertz( nurv_session_log(Sid,NuL) ),
	param:local_NuRV_prompt(NuRVp),
	atom_length(NuRVp,NuRVpLen),
	Pos is LenL - NuRVpLen,
	(   (Pos >= 0, sub_atom(NuL,Pos,NuRVpLen,_,NuRVp) )
	->  % NuRV prompt found at end of NuL
		% If NuL is multi-line then the preceding line is the response
		(	sub_atom(NuL,Before,1,After,'\n')
		->	After1 is After+1,
			sub_atom(NuL,0,Before,After1,Resp)
		;	Resp = ''
		)
	;   % keep looking for prompt
		nurv_session_get_resp(Sid,Resp)
	).

nurv_monitor_init(Infile,Ordfile,Sid) :- nurv_simulation(false), !,
	atomic_list_concat(['set input_file ',Infile],Cmd1),
	atomic_list_concat(['set input_order_file ',Ordfile],Cmd2),
	nurv_session_cmd_resp(Sid,Cmd1,_Resp1),
	nurv_session_cmd_resp(Sid,Cmd2,_Resp2),
	nurv_session_cmd_resp(Sid,go,_Resp3),
	nurv_session_cmd_resp(Sid,'build_monitor -n 0',_Resp4),
	true.
nurv_monitor_init(_,_,_) :- nurv_simulation(true), !,
	true.

%%%%%%%%%%%%%%%%%
% NuRV session tracking and logging
%
:- dynamic nurv_session/5, nurv_session_log/2.

nurv_session('11111', int, monid_00001, x, x). % keep for testing
nurv_session(sid,stype,mid,to_stream,from_stream). % sid is pid as an atom

nurv_session_log(sid,response).

%%%

clear_session_log(Sid) :-
	monid_sessid_muniq_suniq(_,Sid,_,NuRVsid),
	retractall( nurv_session_log(NuRVsid,_)).

display_session_log(Sid) :- display_session_log(Sid,noclear).

display_session_log(Sid,Clear) :- writeln('NuRV Log:'),
	monid_sessid_muniq_suniq(_,Sid,_,NuRVsid),
	forall(nurv_session_log(NuRVsid,Msg), format('~a: ~s~n',[Sid,Msg])),
	(Clear==clear -> clear_session_log(Sid) ; true ).

%%%

open_nurv_session(int,NuRVSessionId,MonitorId) :- % open interactive NuRV session
	param:local_NuRV(_,NuRV),
	process_create(path(NuRV),['-quiet', '-int'],
		       [process(NuRVpid),stdin(pipe(ToStream)),stdout(pipe(FromStream))]),
	atom_number(NuRVSessionId,NuRVpid),
%	init_session(SessionId, monitor_framework),
	( param:verbose(on) -> format('NuRV session ~a~n',NuRVSessionId) ; true ),
	assert( nurv_session(NuRVSessionId,int,MonitorId,ToStream,FromStream) ).

open_nurv_session(orbit,NuRVSessionId,MonitorId) :- % open orbit NuRV session
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
	atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
	param:local_NuRV(_,NuRV),
	process_create(path(NuRV),
		       ['-quiet','-int','-i',SMVordFile,SMVmodelFile],
		       [process(NuRVpid),stdin(pipe(ToStream)),stdout(pipe(FromStream))]),
	atom_number(NuRVSessionId,NuRVpid),
%	init_session(SessionId, monitor_framework),
	( param:verbose(on) -> format('NuRV session ~a~n',NuRVSessionId) ; true ),
	assert( nurv_session(NuRVSessionId,orbit,MonitorId,ToStream,FromStream) ),
	nurv_session_cmd_resp(Sid,go,_Resp1),
	nurv_session_cmd_resp(Sid,'build_monitor -n 0',_Resp2).
/*
% NuRV -quiet -int -i disjoint.ord -source t.cmd disjoint.smv

	param:local_NuRV(_,NuRV),
	process_create(path(NuRV),['-quiet', '-int'],
		       [process(NuRVpid),stdin(pipe(ToStream)),stdout(pipe(FromStream))]),
	atom_number(SessionId,NuRVpid),
	init_session(SessionId, monitor_framework),
	( param:verbose(on) -> format('NuRV session ~a~n',SessionId) ; true ),
	assert( nurv_session(SessionId,int,MonitorId,ToStream,FromStream) ).
*/
quit_nurv_session :- % assumes only one active session
	nurv_session(SessionId,_,_,_,_),
	quit_nurv_session(SessionId).

quit_nurv_session(NSessionId) :- % send quit command, then close
	nurv_session_cmd(NSessionId,quit),
	close_nurv_session(NSessionId).

close_nurv_session(NSessionId) :- % only close the session
	nurv_session(NSessionId,_Stype,_MonitorId,ToStream,FromStream),
	close(ToStream), close(FromStream),
%	(   is_session(SessionId, monitor_framework) -> end_session(SessionId) ; true ),
	retractall( nurv_session(NSessionId,_,_,_,_) ),
	atom_number(NSessionId,NuRVpid),
	process_wait(NuRVpid,Exit),
	writeln(Exit). % TODO get rid of this

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
	nurv_session_cmd_resp(Sid,Cmd,_Resp),
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
test :- open_nurv_session(int,Sid,none), format('NuRV session ~a~n',Sid),
	nu_tl(Sid), close_nurv_session(Sid), writeln('session ended'), !.

test2 :- open_nurv_session(int,Sid,none), format('NuRV session ~a~n',Sid),
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
	open_nurv_session(int,Sid,none), % format('NuRV session ~a~n',Sid),
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
