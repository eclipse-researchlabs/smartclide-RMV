% RMV - Monitor Creation
% Work in Progress

:- module(rmv_mc,[load_service_spec_immediate/2, unload_service_spec/1,
                  service_spec2model/2, service_spec2properties/2, service_spec2script/2, service_spec2monitor/2,
                  create_monitor/4, graph_monitor/1, export_monitor/1
	       ]).

:- use_module('COM/param').
:- use_module(rmv_ml).
:- use_module([rmv_mc_cm,rmv_mc_cps,rmv_mc_nui
	      ]).

:- dynamic mc_initialized/1.
mc_initialized(false).

init:- mc_initialized(true), !. % already initialized
init :-
	% ...
        retractall(mc_initialized(_)), assert(mc_initialized(true)).

re_init :- un_init, init.

un_init :-
	% ...
        retractall(mc_initialized(_)), assert(mc_initialized(false)).


% load_service_spec_immediate
%
load_service_spec_immediate(SS, Sid) :-
        rmv_ml:load_service_specification_immediate(SS,Sid).

% unload_service_spec
%
unload_service_spec(Sid) :-
        rmv_ml:unload_service_specification(Sid).

service_spec2model(SSpecId, SMVmodel) :-
        rmv_ml:service_spec(SSpecId,SSpecBody),
        rmv_mc_cm:create_smv_model( SSpecId, SSpecBody, SMVmodel ).

service_spec2properties(SSpecId, LTLproperties) :-
        rmv_ml:service_spec(SSpecId,SSpecBody),
        rmv_mc_cps:create_ltl_properties( SSpecId, SSpecBody, LTLproperties ).

service_spec2script(SSpecId, NuRVcmds) :-
        rmv_ml:service_spec(SSpecId,_SSpecBody),
        NuRVcmds = [].

service_spec2monitor(SS, Monitor) :-
        is_service_spec(SS,SSid,_SSbody),
        rmv_ml:load_service_specification(SS,SSid),
        service_spec2model(SSid,SMVmodel),
        service_spec2properties(SSid,LTLproperties),
        service_spec2script(SSid,Ncmds), % HERE generate preamble commands
        create_monitor(SMVmodel,/*ModelVars,SharedVars,*/LTLproperties,Ncmds,MonitorId),
        rmv_ml:monitor(MonitorId,A,B,C,D,E,F),
        is_monitor(Monitor,MonitorId,A,B,C,D,E,F).

%
% create_monitor(+Model,+ModelVars,+SharedVars,+Properties,+Commands,-Monitor)
%
%
%   monitor(MonState,MonXS,MonObservables,MonOut,MonCurrentInput,MonCurrentOutput)
%

create_monitor(Model,/*ModelVars,SharedVars,*/Properties,Cmds,MonitorId) :-
        is_model(Model,ModelId),
        forall( member(P,Properties), is_property(P) ),
        forall( member(C,Cmds), is_cmd(C) ),
        modid_monid(ModelId,MonitorId),

        % install the new monitor in the library
        is_monitor(Monitor,MonitorId,ModelId,_,_,_,_,_),
        load_monitor(Monitor),
        true.

graph_monitor(Monitor) :- is_monitor(Monitor).

export_monitor(_Monitor).

