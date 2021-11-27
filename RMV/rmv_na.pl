% RMV - Notification Agent
% Work in Progress

:- module(rmv_na,[notifications/3
	       ]).

:- use_module('COM/param').

:- dynamic na_initialized/1.
na_initialized(false).

init:- na_initialized(true), !. % already initialized
init :-
	% ...
        retractall(na_initialized(_)), assert(na_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(na_initialized(_)), assert(na_initialized(false)).

notifications(_MonitorId,_Reportables,_Verdict) :-
        true.
