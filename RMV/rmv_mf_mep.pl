% RMV - Monitoring Framework - Monitor Event Processing
% Work in Progress

:- module(rmv_mf_mep,[mep_initiate_monitor/2
	       ]).

:- use_module('COM/param').
%:- use_module('EPP/epp').
%:- use_module('EPP/eppapi').

% MONITOR EVENT PROCESSING
%

mep_initiate_monitor(_Mid,Status) :-
    Status = success,
    true.
