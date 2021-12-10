% RMV - Audit Agent
% Work in Progress

:- module(rmv_aa,[]).

:- use_module('COM/param').
:- use_module('AUDIT/audit').

% Initialization
%

:- dynamic rmv_aa_initialized/1.
rmv_aa_initialized(false).

% The following initialization (init) is when this module is initialized
% as part of a larger system. The initialization (initialize_all) is
% when rmv IS the system.
%
init:- rmv_aa_initialized(true), !. % already initialized
init :-
	% ...
        retractall(rmv_aa_initialized(_)), assert(rmv_aa_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(rmv_aa_initialized(_)), assert(rmv_aa_initialized(false)).

% initialize_once - do these things only once per run
initialize_once :- rmv_aa_initialized(true), !.
initialize_once :-
	true.


