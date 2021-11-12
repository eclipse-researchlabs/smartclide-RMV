% CME standalone sim

:- use_module(library(http/http_client)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% CME API
:- http_handler(root(.), use_valid_api, []).
:- http_handler(root('cross-cpp'), root_apis('cross-cpp'), []).
:- http_handler(root('cross-cpp/'), api_unimpl, [prefix]).
:- http_handler(root('cross-cpp/context_notification_registration'), notification_reg, [prefix]).
%:- http_handler(root('context'), root_apis('context'), []).
%:- http_handler(root('context/'), api_unimpl, [prefix]).
%:- http_handler(root('context/context_notification_registration'), notification_reg, [prefix]).

ccpp([context_notification_registration]). % CME API

cme :- cme(8002).

cme(Port) :-
	format('CME sim starting~n'),
	http_server(http_dispatch, [port(Port)]),
	format('CME sim listening on port ~d~n',[Port]).

%
% Simulated CME API
%
notification_reg(Request) :-
	std_resp_prefix,
	catch(
	     http_parameters(Request,[context_variables(VarsAtom,[atom]),
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
	context_notification_registration_sim(Vars,URL,Token),
	true.

% ------------------------------------------------------------------------
% CONTEXT MONITORING AND EXTRACTION SIMULATION
%
%   context_variable_change/2 generates a context_change_notification
%

context_notification_registration_sim(VarNames,URL,Etoken) :-
    %format('Simulated context notification registration:~n  ~q ~q ~q~n',[VarNames,URL,Etoken]),
    %flush_output,
    % need to come up with values -- use simulation
    retrieve_context_variables_sim(VarNames,Vals),
    !,
    maplist(context_variable_name_value,VarsValsInclUndef,VarNames,Vals),
    delete(VarsValsInclUndef, _:undefined, VarsVals),
    gen_context_change_notification(VarsVals,URL,Etoken),

    true.

gen_context_change_notification(VarsVals,EPP,Etoken) :-
    %format('Change Notification from CME: ~q~n',[VarsVals]),
    term_to_atom(VarsVals,ContextAtom),
    atomic_list_concat([EPP,'?context=',ContextAtom,'&token=',Etoken],Call),
    % make the call, first show the call
    %format('making EPP call: ~q~n',[Call]),
    http_get(Call,CallResult,[]), % call the EPP
    % should check the call result for "success" but for now accept anything
    ( CallResult == success ; CallResult == 'context change notification accepted' ; true ),
    % writeln(CallResult), %format('EPP call RESULT: ~q~n',[CallResult]), flush_output,
    true.

context_variable_name_value(Name:Val,Name,Val).

retrieve_context_variables_sim([],[]).
retrieve_context_variables_sim([Var|Vars],[Val|Vals]) :-
    retrieve_context_variable_sim(Var,Val),
    retrieve_context_variables_sim(Vars,Vals).

% this is an *individual* context variable retrieval sim, not currently
% supported by the CME module
%
retrieve_context_variable_sim(CtxVar,CtxVal) :-
    sim_context_var(CtxVar,CtxVal).


%
%
sim_context_var(day_of_the_week, DayOfWeek) :- !,
    get_time(Stamp),
    stamp_date_time(Stamp,LongDate,local),
    LongDate = date(Year,Month,Date,_Hour,_Min,_Sec,_,_,_),
    ShortDate = date(Year,Month,Date),
    day_of_the_week(ShortDate,Day),
    nth1(Day,['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'],DayOfWeek).
sim_context_var(weekday,true) :- !.
sim_context_var(weekend,false) :- !.
sim_context_var(business,true) :- !.
sim_context_var(leisure,false) :- !.
% ...
sim_context_var(_,undefined) :- !.


% simulate change of a single context variable
cv_change(VarName:Value) :- atom(VarName), ground(Value), !,
    gen_context_change_notification(VarName:Value).

% simulate change of a list of context variables
cv_change(VarsVals) :- is_list(VarsVals), !,
    gen_context_change_notification(VarsVals).

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
