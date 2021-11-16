% RMV - Monitor Sensor
% Work in Progress

:- module(rmv_ms,[
	       ]).

:- use_module('COM/param').

:- dynamic ms_initialized/1.
ms_initialized(false).

init:- ms_initialized(true), !. % already initialized
init :-
	% ...
        retractall(ms_initialized(_)), assert(ms_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(ms_initialized(_)), assert(ms_initialized(false)).

