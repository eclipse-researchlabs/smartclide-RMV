% RMV - Logging Agent
% Work in Progress

:- module(rmv_la,[
	       ]).

:- use_module('COM/param').

:- dynamic la_initialized/1.
la_initialized(false).

init:- la_initialized(true), !. % already initialized
init :-
	% ...
        retractall(la_initialized(_)), assert(la_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(la_initialized(_)), assert(la_initialized(false)).

