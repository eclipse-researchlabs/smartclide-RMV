% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_start_monitor/2, mep_stop_monitor/1, mep_heartbeat/4
	       ]).

:- use_module('COM/param').
:- use_module('RMV/rmv_mc_nui').
%:- use_module('EPP/epp').
%:- use_module('EPP/eppapi').

% MONITOR EVENT PROCESSING
%

mep_start_monitor(Mid,Status) :-
    rmv_mc_nui:start_monitor(Mid,Status),
    true.

mep_stop_monitor(Mid) :-
    rmv_mc_nui:stop_monitor(Mid),
    true.

mep_heartbeat(Mid,AtomIds,Reportables,Response) :-
    rmv_mc_nui:heartbeat(Mid,AtomIds,Verdict),
    notifications(Mid,Reportables,Verdict),
    (   (Verdict == true ; Verdict == inconclusive)
    ->  Response = [acknowledged,verdict=Verdict,recover=false]
    ;   Response = [acknowledged,verdict=Verdict,recover=true],
        notifications(Mid,exception,Verdict)
    ),
    true.
