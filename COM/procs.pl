% stored "built-in" procedures

:- module(procs, [proc/2, pmproc/2]).

:- dynamic([proc/2, pmproc/2]).

:- if( exists_file('NGAC/procs_ngac.pl') ).
:- include('NGAC/procs_ngac.pl').
procs_defined(ngac).
:- endif.

:- if( exists_file('PRIV/procs_priv.pl') ).
:- include('PRIV/procs_priv.pl').
procs_defined(priv).
:- endif.

:- if( exists_file('EPP/procs_epp.pl') ).
:- include('EPP/procs_epp.pl').
procs_defined(epp).
:- endif.

:- if( exists_file('RMV/procs_rmv.pl') ).
:- include('RMV/procs_rmv.pl').
procs_defined(rmv).
:- endif.

:- discontiguous proc/2.

defined_procs(ProcSets) :- findall(ProcSet, procs_defined(ProcSet), ProcSets).
% e.g.: defined_procs([ngac,priv,epp,rmv]).
