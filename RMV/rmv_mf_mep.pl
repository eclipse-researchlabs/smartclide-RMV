% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_start_monitor/2, mep_stop_monitor/1, mep_heartbeat/3
	       ]).

:- use_module('COM/param').
%:- use_module('EPP/epp').
%:- use_module('EPP/eppapi').

% MONITOR EVENT PROCESSING
%

mep_start_monitor(_Mid,Status) :-
    Status = success,
    true.

mep_stop_monitor(_Mid) :-
    true.

mep_heartbeat(_Mid,_AtomIds,_Reportables) :-
    true.
