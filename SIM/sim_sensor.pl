% simple MONITOR SENSOR sim

:- module(sim_sensor, [mon_start/0, mon_start/1, sensor/1]).

:- use_module(library(http/http_client)).

rmv_token('rmv_token'). % default RMV token
rmv_url('http://127.0.0.1:8010/rmvapi/').

mon_start :- mon_start(init).

mon_start(Arg) :-
	format('RMV MON SENSOR starting~n'),
	sensor(Arg).

% sensor is called initially and by execution of the monitored app
%
sensor(init) :- !.
%sensor(4) :- !, fail.
sensor(Arg) :-
	gen_heartbeat(Arg).

gen_heartbeat(Arg) :-
	term_to_atom(Arg,ArgAtom),
	rmv_url(RMV_URL), atom_concat(RMV_URL,'heartbeat',HeartbeatURL),
	rmv_token(RMVtoken),
	atomic_list_concat([HeartbeatURL,'?arg=',ArgAtom,'&token=',RMVtoken],RMVcall),
	format('making RMV call: ~q~n',[RMVcall]),
	% http_get(RMVcall,Result,[]), % call the RMV
	( Result == success ; Result == continue ; true ), % accept any result
	format('RMV call RESULT: ~q~n',[Result]), flush_output,
	true.
