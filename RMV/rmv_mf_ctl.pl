% RMV - Monitoring Framework - Control
% Work in Progress

:- module(rmv_mf_ctl,[
	       ]).

:- use_module('COM/param').

init:- param:initialized(true), !. % already initialized
init :-
	% ...
	param:setparam(initialized,true),
	true.

re_init :- un_init, init.

un_init :-
	% ...
	param:setparam(initialized,false).

