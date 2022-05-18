% RVM service/monitor execution
% can be run inside RMV or as a standalone sim

:- module(sim_exec, []).

:- use_module('RMV/rmv_ml').
:- use_module('RMV/rmv_mc_nui').

:- use_module(library(http/http_client)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% EXEC SIM API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root('exec'), root_apis('exec'), []).
:- http_handler(root('exec/'), api_unimpl, [prefix]).
:- http_handler(root('exec/exec_notification_registration'), notification_reg, [prefix]).

exapi([exec_notification_registration]). % EXEC SIM API

exec_sim :- exec_sim(8003).

exec_sim(Port) :-
	format('Exec sim starting~n'),
	http_server(http_dispatch, [port(Port)]),
	format('Exec sim listening on port ~d~n',[Port]).

% simulation

execute_service( local, Deployment ) :- !,
	is_deployment(Deployment, _, Service, Monitor),
	is_service(Service,_,_,_,_,_,_,States),
	initiate_service(local,Service),
	initiate_monitor(Monitor,SessionId),
	sim_exec_steps(Deployment, SessionId, States),
	terminate_monitor(SessionId).

execute_service( remote, Deployment ) :- !,
	is_deployment(Deployment, _, Service, Monitor),
	is_service(Service,_,_,_,_,_,_,States),
	initiate_service(remote,Service),
	initiate_monitor(Monitor,SessionId),
	sim_exec_steps(Deployment, SessionId, States),
	terminate_monitor(SessionId).


sim_exec_steps(_, _, []).
sim_exec_steps(Deployment, Sid, [Step|Steps]) :-
	sim_exec_step(Deployment, Sid, Step),
	sim_exec_steps(Deployment, Sid, Steps).

sim_exec_step(_Deployment, Sid, Step) :-
	Step = state(_StateNo, Assignments),
	% Step = state(StateNo, Assignments)
	% e.g. Step = state('1',[p='TRUE',q='FALSE']),
	% Step the service and step the monitor
	step_monitor(Assignments,Sid).

initiate_service(local,S) :- !, is_service(S).
initiate_service(remote,S) :- !, is_service(S),
        % execute the sim_app with sim_sensor
	true.

initiate_monitor(M,SessId) :- is_monitor(M,MonitorId,ModelId,_,_,_,_,_),
        open_nurv_session(int,SessId,MonitorId), % format('NuRV session ~a~n',Sid),
	nurv_session_get_resp(SessId,_Resp),
	param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
	nurv_monitor_init(SMVmodelFile,'MONITORS/default.ord',SessId),
	true.

step_monitor(Assignments,SessId) :-
	assignments2argatom(Assignments,Argatom),
	format(atom(Cmd),'heartbeat -n 0 ~w',[Argatom]),
	nurv_session_cmd_resp(SessId,Cmd,_Resp).

terminate_monitor(SessId) :-
	quit_nurv_session(SessId),
	writeln('session ended').

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




%
notification_reg(Request) :-
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
	    notification_reg_response(Vars,EPP,Token),
	    writeln(success)
	).

notification_reg_response(Vars,URL,Token) :-
	exec_notification_registration_sim(Vars,URL,Token),
	true.

% ------------------------------------------------------------------------
% SERVICE / MONITOR EXECUTION SIMULATION
%
%   exec_variable_change/2 generates a exec_change_notification
%

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
    gen_exec_change_notification(VarName:Value).

% simulate change of a list of exec variables
cv_change(VarsVals) :- is_list(VarsVals), !,
    gen_exec_change_notification(VarsVals).

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
