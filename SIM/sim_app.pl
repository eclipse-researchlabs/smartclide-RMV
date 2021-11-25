% monitored app sim
%
:- module(sim_app, []).

%:- use_module(library(http/http_client)).

:- use_module('RMV/rmv_ms').
:- use_module('COM/param').

app :-  param:rmv_port(P),
	app(P). % default RMV monitor port

app(Port) :-
	format('APP sim starting~n'),
	format('talking to RMV at port ~d~n',[Port]),
	reset_state,
	ms_startup,
	app_go,
	ms_shutdown,
	true.

app_go :-
	sim_execution, !,
	post_execution.

% ------------------------------------------------------------------------
% APP EXECUTION SIMULATION
%

:- dynamic current_state/1.
current_state(1).

reset_state :-
	retractall(current_state(_)),
	assert(current_state(1)).

sim_execution :-
	current_state(Old),
	step(Old,_New), % run ends when step or sensor fails
	ms_step,
	sim_execution.
sim_execution.

%
step(C,C) :- C >= 5, !, fail.
step(C,N) :-
	N is C+1,
	retractall(current_state(_)),
	assert(current_state(N)),
	format('state is now = ~d~n',N),
	true.

post_execution :-
	format('execution ended~n'),
	true.

% ------------------------------------------------------------------------
% TRACE FOR APP EXECUTION SIMULATION
%

trc( trace('counter-example',
      [state('1',[p='TRUE',q='FALSE']),state('2',[p='TRUE',q='FALSE']),state('3',[p='TRUE',q='FALSE']),
       state('4',[p='FALSE',q='TRUE']),state('5',[p='FALSE',q='TRUE']),state('6',[p='FALSE',q='TRUE']),
       state('7',[p='FALSE',q='TRUE']),state('8',[p='FALSE',q='TRUE']),state('9',[p='TRUE',q='FALSE']),
       state('10',[p='TRUE',q='FALSE']),state('11',[p='TRUE',q='FALSE']),state('12',[p='FALSE',q='TRUE']),
       state('13',[p='TRUE',q='FALSE']),state('14',[p='TRUE',q='FALSE']),state('15',[p='TRUE',q='FALSE']),
       state('16',[p='TRUE',q='FALSE'])])
   ).

truncate_trace(trace(N,[A,B,C,D,E,F|_]),trace(N,[A,B,C,D,E,F])) :- !. % truncate to 6 steps
truncate_trace(T,T).
