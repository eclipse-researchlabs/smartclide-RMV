% RMV - Monitoring Framework
% Work in Progress

:- module(rmv_mf,[
	       ]).

:- use_module('COM/param').
:- use_module([rmv_mf_epp]).

:- dynamic mf_initialized/1.
mf_initialized(false).

init:- mf_initialized(true), !. % already initialized
init :-
	% ...
        retractall(mf_initialized(_)), assert(mf_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(mf_initialized(_)), assert(mf_initialized(false)).

