% RMV - Audit Agent
% Work in Progress

:- module(rmv_aa,[]).

:- use_module('COM/param').
:- use_module('AUDIT/audit').

init:- param:initialized(true), !. % already initialized
init :-
	% ...
	param:setparam(initialized,true),
	true.

re_init :- un_init, init.

un_init :-
	% ...
	param:setparam(initialized,false).

