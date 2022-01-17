% RMV external services interface / simulation -
%    Service Creation, Service Deployment, Execution Control
%
% service/monitor execution can be used inside RMV or run as
% a standalone sim process

:- module(ext_svcs, [ext_execute_service/2,ext_get_service_spec/2, exec_sim/0,
	  ext_service_spec2service/2, ext_deploy_service_with_monitor/3]).

:- use_module('COM/param').
:- use_module('RMV/rmv_ml').
:- use_module('RMV/rmv_mc').
:- use_module('RMV/rmv_mc_nui').
:- use_module('SIM/sim_app').

:- use_module(library(http/http_client)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

%-------------------------------------------------------
% EXECUTION SIM API
%   only used in standalone ext_svcs for 'remote' test
%
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root('exec'), root_apis('exec'), []).
:- http_handler(root('exec/'), api_unimpl, [prefix]).
:- http_handler(root('exec/exec_notification_registration'), execapi_notification_reg, [prefix]).
:- http_handler(root('exec/e2e'), execapi_e2e, [prefix]).

execapi([exec_notification_registration, e2e]). % EXEC SIM API

% ------------------------------------------------------------------------
% API IMPLEMENTATION
%
% called from the exec_notification_registration API
execapi_notification_reg(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[exec_variables(VarsAtom,[atom]),
				      epp_url(EPP,[atom]),
				      epp_token(Token,[atom])
				     ]),
	    E,writeln('missing parameter')),	!,
	(   nonvar(E)
	->  writeln(failure)
	;
	    read_term_from_atom(VarsAtom,Vars,[]),
	    %format('Context notification registration:~n  ~q ~q ~q~n',[Vars,EPP,Token]),
	    %flush_output,
	    notification_reg(Vars,EPP,Token),
	    writeln(success)
	).

notification_reg(Vars,URL,Token) :-
	exec_notification_registration_sim(Vars,URL,Token),
	true.

% this is used to enter end-to-end test in simulated environment
execapi_e2e(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[
				     ]),
	    E,writeln('missing parameter')),	!,
	(   nonvar(E)
	->  writeln(failure)
	;   writeln('E2E called'),
	    e2e_api(remote),
	    writeln(success)
	).

e2e_api(Mode) :- % mode can be int, local, remote, orbit
	format('e2e_api ~w test\n',Mode), flush_output,
	e2e(Mode).

%-------------------------------------------------------
% SYSTEM CONTROL SIMULATION DRIVER
%
% End-to-End test of monitor construction and execution
% run within RMV or in a separate process according to the
% argument value: remote or local.
%
% The rmvt command in the RMV tool calls this through e2e_api
%
e2e(RemOrLoc) :- ( RemOrLoc == remote ; RemOrLoc == local ; RemOrLoc == orbit ), !,
	% SCENARIO:
	% service creation calls monitor creation API to create a monitor
	% service and monitor are executed (or simulated execution)
	% execution initialization notifies monitoring framework of monitor execution
	% monitor session is established with NuRV
	% monitor framework notifies execution that MF is ready to receive monitor outputs
	% execution proceeds sending monitor outputs
	% monitor framework sends each monitor output to NuRV which returns its output
	% monitor framework EPP passes each monitor output and NuRV output to EPP
	% MEP takes further response action if an event pattern is matched, incl logging/notification

	% use state sequence from a predefined trace for this test,
	% pass it in with the ServiceCreationContext
	% get specificaiton of the service
	% create the service from the service spec
	% in the real system this is done by SmartCLIDE service creation
	% create the monitor from the service spec
	% deploy and execute the service and monitor

	trc(T), truncate_trace( T, trace(_,States)),
	ServiceCreationContext = [trace=States, ssid=ssid_001],
	ext_get_service_spec(ServiceCreationContext, ServiceSpec), % service spec will have the trace
	ext_service_spec2service(ServiceSpec,Service), % now service has the trace
	rmv_mc:service_spec2monitor(ServiceSpec,Monitor),
	ext_deploy_service_with_monitor(Service,Monitor,Deployment),
	ext_execute_service(RemOrLoc,Deployment), % trace is passed in with deployed service
	!.
e2e(test2) :- !,
	ext_execute_service(test2,_),
	!.
e2e(_) :- writeln('specify remote, local or testN').


% EXEC SIM SERVER entry point and stop server

exec_sim :- exec_sim(8003). % entry point for running as ext server

exec_sim(Port) :-
	format('Exec sim starting~n'),
	http_server(http_dispatch, [port(Port)]),
	format('Exec sim listening on port ~d~n',[Port]),
	param:server_sleeptime(S), go_to_sleep(S).

stop_sim :- stop_sim(8003).

stop_sim(Port) :-
	writeln('Stopping sim server'),
	thread_httpd:http_stop_server(Port,[]),
	writeln('Sim server stopped').

go_to_sleep(S) :-
	sleep(S),
	periodic_goals,
	go_to_sleep(S).

periodic_goals :-
	% add periodic goals here
	true.


%-------------------------------------------------------
% EXTERNAL SERVICES SIMULATION

% Retrieve service spec simulation stub
%
ext_get_service_spec(ServiceCreationContext, ServiceSpec) :-
	AddedItems = [],
	append(ServiceCreationContext,AddedItems,B),
	memberchk(ssid=SSid, ServiceCreationContext),
	is_service_spec(ServiceSpec, SSid, Sbody ),
	is_service_spec_body( Sbody, B ).


%-------------------------------------------------------
% SERVICE CREATION SIMULATION
%

% a service is being created and RMV is called to create a monitor
ext_create_service(SS) :- % not called currently
	is_service_spec(SS),
	ext_service_spec2service(SS,Service),
	create_monitor(SS,Monitor),
	ext_deploy_service_with_monitor(Service,Monitor,Deployment),
	is_deployment(Deployment).

ext_service_spec2service(ServiceSpec, Service) :-
	is_service_spec(ServiceSpec, SSid, SSb),
	is_service_spec_body(SSb,Bitems),
	intersection(Bitems, [trace=_,assigns=_], Behavior),
	memberchk(atom_eval_mode=Aeval,Bitems),
	memberchk(monitor_sensor_lang=MSlang,Bitems),
	atom_concat(ssid_,N,SSid), atom_concat(servid_,N,ServId),
	is_service(Service,ServId,_,_,_,Aeval,MSlang,Behavior).

% end SERVICE CREATION SIMULATION
create_monitor(ServSpec,Monitor) :- % bypass HTTP API
	rmv_mcapi:create_monitor_aux(ServSpec,Monitor).
%-------------------------------------------------------


%-------------------------------------------------------
% SERVICE DEPLOYMENT SIMULATION
% a service and its monitor are ready to be deployed
%

ext_deploy_service_with_monitor(Service, Monitor, Deployment) :-
	is_service(Service), is_monitor(Monitor),
	new_deployment_id(Did),
	cons_deployment(Deployment,Did,Service,Monitor),
	true.

new_deployment_id(Did) :-
	uuid(U), atom_concat('deploy_',U,Did).

% end SERVICE DEPLOYMENT SIMULATION
%-------------------------------------------------------


%-------------------------------------------------------
% EXECUTION CONTROL SIMULATION
% (using state sequence from service)
% There are several distinct scenarios for testing and production.
% First, NuRV can be run by interacting with an interactive NuRV
% session, or the NuRV server version can be run. The latter involves
% interaction through ORBit and is more complicated but more reliable.
%
% Then the monitored service can be either simulated or run as a
% distinct process with a monitor sensor that communicates with the
% monitor event processing. The sim_app module provides application
% simulation in Prolog (see execute_service(remote, ...)).

ext_execute_service( ms_pl, Deployment ) :- !,
	is_deployment(Deployment, _, Service, Monitor),
	is_service(Service,_,_,_,_,_,_,Behavior),
	is_monitor(Monitor,_MonitorId,_,_,_,_,_,_),
	memberchk( assigns=Assigns, Behavior ),
	execute_service(ms_pl,Service,Deployment,_SessionId,Assigns),
	true.

ext_execute_service( ms_c, _Deployment ) :- !.

ext_execute_service( RemOrLoc, Deployment ) :- ( RemOrLoc == remote ; RemOrLoc == local), !,
	is_deployment(Deployment, _, Service, Monitor),
	is_service(Service,_,_,_,_,_,_,Behavior),
	is_monitor(Monitor,_MonitorId,_,_,_,_,_,_),
	memberchk( trace=States, Behavior ),
	initiate_monitor(Monitor,SessionId),
	execute_service(RemOrLoc,Service,Deployment,SessionId,States),
	terminate_monitor(SessionId).

ext_execute_service(orbit,Deployment) :- !,
	is_deployment(Deployment, _, Service, Monitor),
	is_service(Service,_,_,_,_,_,_,States),
	is_monitor(Monitor,MonitorId,_,_,_,_,_,_),
	initiate_monitor(Monitor,SessionId),
	execute_service(orbit,Service,Deployment,SessionId,States),
	param:local_nameserver_IOR(IOR),
	atomic_list_concat(['monitor_server -N ',IOR,' -i ',MonitorId],ServerCmd),
	nurv_session_cmd_resp(SessionId,ServerCmd,_Resp),
	% terminate_monitor
	true.

% end EXECUTION CONTROL SIMULATION
%-------------------------------------------------------

%-------------------------------------------------------
% EXECUTE THE SERVICE
%
execute_service(ms_pl,Service,Deployment,SessionId,Assigns) :- !, is_service(Service),
    % act as the executing (Prolog-implemented) service
    % using the assigns from the behavior argument
    rmv_ms:ms_startup,
	rmv_ms:ms_recovery( recoveryp ), % defined below
    sim_exec_steps(Deployment, SessionId, Assigns), % calls sv_setter
	rmv_ms:ms_shutdown.

execute_service(local,Service,Deployment,SessionId,States) :- !, is_service(Service),
	sim_exec_steps(Deployment, SessionId, States).

% remote service simulation is run in a separate process
execute_service(remote,Service,Deployment,SessionId,States) :- !, is_service(Service),
   % execute the sim_app with sim_sensor
	sim_app:app,
	sim_exec_steps(Deployment, SessionId, States),
	true.

execute_service(orbit,_Service,_Deployment,_SessionId,_States) :-
	true.

recoveryp(R) :- format('Service recovery callback invoked with ~q~n',R).

%-------------------------------------------------------
% STEP - simulated stepping of the service
%
sim_exec_steps(_, _, []) :- !.
sim_exec_steps(Deployment, Sid, [Step|Steps]) :-
	sim_exec_step(Deployment, Sid, Step),
	sim_exec_steps(Deployment, Sid, Steps).

% Step = state(StateNo, Assignments)
% e.g. Step = state('1',[p='TRUE',q='FALSE']),
% Step the service and step the monitor

sim_exec_step(_Deployment, Sid, state(_StateNo, Assignments)) :- !,
	step_monitor(Assignments,Sid).

sim_exec_step(_Deployment, _, Var=Val) :- !,
   rmv_ms:sv_setter(Var,Val).

% end STEP SIMULATION
%-------------------------------------------------------

%-------------------------------------------------------
% monitor controls
%
step_monitor(Assignments,SessId) :-
	assignments2argatom(Assignments,Argatom),
	format(atom(Cmd),'heartbeat -n 0 ~w',[Argatom]),
	nurv_session_cmd_resp(SessId,Cmd,_Resp).


assignments2argatom([], '') :- !.
assignments2argatom(Assignments, Argatom) :-
	(   memberchk(Var='TRUE',Assignments)
	->  format(atom(Argatom),'-c "~w"',[Var])
	;   Argatom = ''
	).
%assignments2argatom([Assignment|Assignments], Argatom) :-
%	assignment2arg(Assignment,Aatom1),
%	atom_concat(Aatom1,Aatom2,Argatom), % FIX
%	assignments2argatom(Assignments, Aatom2).

%assignment2arg(Var='TRUE', Aatom) :-
%	format('heartbeat ~w ~q~n',[N,Var]),
%        true.

terminate_monitor(SessId) :-
	quit_nurv_session(SessId),
	writeln('session ended').

%-------------------------------------------------------
% INITIATE THE MONITOR SESSION
%
/* moved to rmv_mf_mep
initiate_monitor(M,SessId) :- is_monitor(M,MonitorId,ModelId,_,_,_,_,_),
    open_nurv_session(int,SessId),
	format('Monitor ID: ~a; NuRV session: ~a~n',[MonitorId,SessId]), flush_output,
	nurv_session_get_resp(SessId,_Resp),
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
	atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
	nurv_monitor_init(SMVmodelFile,SMVordFile,SessId),
	true.

initiate_monitor2(M,SessId) :- is_monitor(M,_MonitorId,_ModelId,_,_,_,_,_),
    open_nurv_session(orbit,SessId),
	param:local_nameservier_IOR(IOR),
	atomic_list_concat(['monitor_server -N ',IOR],ServerCmd),
	nurv_session_cmd_resp(SessId,ServerCmd,_Resp),
	true.
*/
% ------------------------------------------------------------------------
% SERVICE / MONITOR EXECUTION SIMULATION
%
%   exec_variable_change/2 generates a exec_change_notification
%

% get the values of the variables and send the notification
% with values of all defined variables
exec_notification_registration_sim(VarNames,URL,Etoken) :-
    %format('Simulated exec notification registration:~n  ~q ~q ~q~n',[VarNames,URL,Etoken]),
    %flush_output,
    % need to come up with values -- use simulation
    retrieve_exec_variables_sim(VarNames,Vals),
    !,
    maplist(exec_variable_name_value,VarsValsInclUndef,VarNames,Vals),
    delete(VarsValsInclUndef, _:undefined, VarsVals),
    gen_exec_change_notification(VarsVals,URL,Etoken),

    true.

% construct and execute call to EPP with list of variable:value
gen_exec_change_notification(VarsVals,EPP,Etoken) :-
    %format('Change Notification from EXEC: ~q~n',[VarsVals]),
    term_to_atom(VarsVals,ContextAtom),
    atomic_list_concat([EPP,'?exec=',ContextAtom,'&token=',Etoken],Call),
    % make the call, first show the call
    %format('making EPP call: ~q~n',[Call]),
    http_get(Call,CallResult,[]), % call the EPP
    % should check the call result for "success" but for now accept anything
    ( CallResult == success ; CallResult == 'exec change notification accepted' ; true ),
    % writeln(CallResult), %format('EPP call RESULT: ~q~n',[CallResult]), flush_output,
    true.

exec_variable_name_value(Name:Val,Name,Val).

retrieve_exec_variables_sim([],[]).
retrieve_exec_variables_sim([Var|Vars],[Val|Vals]) :-
    retrieve_exec_variable_sim(Var,Val),
    retrieve_exec_variables_sim(Vars,Vals).

% this is an *individual* exec variable retrieval sim, not currently
% supported by the EXEC module
%
retrieve_exec_variable_sim(CtxVar,CtxVal) :-
    sim_exec_var(CtxVar,CtxVal).


%
%
sim_exec_var(day_of_the_week, DayOfWeek) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    ShortDate = date(Year,Month,Date),
    day_of_the_week(ShortDate,Day),
    nth1(Day,['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'],DayOfWeek).
sim_exec_var(weekday,true) :- !.
sim_exec_var(weekend,false) :- !.
sim_exec_var(business,true) :- !.
sim_exec_var(leisure,false) :- !.
% ...
sim_exec_var(_,undefined) :- !.


% simulate change of a single exec variable
cv_change(VarName:Value) :- atom(VarName), ground(Value), !,
	param:epp_url(EPP_URL), atom_concat(EPP_URL,'exec_notify', NotifyURL),
	param:epp_token(Etoken),
	gen_exec_change_notification([VarName:Value],NotifyURL,Etoken).

% simulate change of a list of exec variables
cv_change(VarsVals) :- is_list(VarsVals), !,
	param:epp_url(EPP_URL), atom_concat(EPP_URL,'exec_notify', NotifyURL),
	param:epp_token(Etoken),
	gen_exec_change_notification(VarsVals,NotifyURL,Etoken).

% TODO - get rid of this?
% utilities

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
