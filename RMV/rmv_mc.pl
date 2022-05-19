% RMV - Monitor Creation
% Work in Progress

:- module(rmv_mc,[load_service_spec_immediate/2, unload_service_spec/1,
                  /* service_spec2properties/2, service_spec2script/2, */ service_spec2monitor/2,
                  create_monitor/5, graph_monitor/1, export_monitor/1
	       ]).

:- use_module('COM/param').
:- use_module(rmv_ml).
:- use_module([rmv_mc_cm, rmv_mc_cps, rmv_mc_nui
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


% load_service_spec_immediate(+SS, -Sid)
%
load_service_spec_immediate(SS, Sid) :-
        rmv_ml:load_service_specification_immediate(SS,Sid).

% unload_service_spec(+Sid)
%
unload_service_spec(Sid) :-
        rmv_ml:unload_service_specification(Sid).

create_script(SS, NuRVcmds) :-
        is_service_spec(SS, _SSpecId, _SSpecBody),
        % rmv_ml:service_spec(SSpecId, SSpecBody),
        NuRVcmds = [].

service_spec2monitor(SS, Monitor) :-
        create_model( SS, Model ),
        %Model = model(ModelId,SMVstruct,SMVmodelFile,SMVordFile)
        %SMVstruct = smv(_SMV, SMVmodeltext, SMVordtext, SMVvars),
        %SMVvars = vars(Vdecls,Vo,Vm,Vp,Vr,Vt),
        create_ltl_properties( SS, Properties ),
        create_script( SS, NuCmds ), % generate preamble NuRV commands

        create_monitor(SS, Model, Properties, NuCmds, Monitor),
        true.

:- dynamic simulated_monitor_creation/0.
simulated_monitor_creation. % comment-out to turn-off simulated

create_monitor(SS, Model, Properties, Cmds, Monitor) :-
        is_service_spec(SS,SSpecId,SSbody),
        is_service_spec_body(SSbody,Bitems),
        rmv_ml:load_service_specification(SS,SSpecId),

        is_model(Model),
        cons_model(ModelId,_SMVstruct,_SMVmodelFile,_SMVordFile,Model),
        modid2monid(ModelId,MonitorId),

        forall( member(C,Cmds), is_cmd(C) ),

        % configure monitor sensor
        %
        % TODO - call NuRV to create and test monitor
        % TODO - Compute Vo, Vm, Vp, Vr, Vt

        % make a unique monitor sensor instance id
        uuid(U), atom_concat(msid_ , U, MSid),

        (       memberchk(monitor_sensor_lang=MSlang,Bitems)
        ->      true
        ;       fail % TODO something else
        ),
        target_lang_monitor_sensor(MSlang,_,MSdir,MSfile,MSEval),
        atomic_list_concat([MSdir,'/',MSfile], MSFullFile),

        (       simulated_monitor_creation
        ->      rmv_ml:ex_cv(2,MScv),
                cons_ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port, MScv),
                (       memberchk(atom_eval_mode=OAEval,Bitems)
                ->      true % override the eval location
                ;       OAEval = AEval
                ),
                (       memberchk(behavior=OBeh,Bitems)
                ->      true % override the behavior
                ;       OBeh = Beh
                ),
                cons_ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, OAEval, Vinit, OBeh, Timer, Host, Port, MScv1)

        ;       
                Properties = properties(Vp, PropAtoms, PropFormulas),
                forall( member(P,PropFormulas), is_property(P) ),
                Vdecl = [],
                Vo = [],
                Vm = [],
                % Vp
                Vr = [],
                Vt = [],
                % PropAtoms
                AEval = MSEval,
                Vinit = [],
                Beh = [],
                Timer = 0,
                param:serverhost_ip(Host),
                param:rmv_port(Port),
                cons_ms_cv(MonitorId,Vdecl,Vo,Vm,Vp,Vr,Vt,PropAtoms,AEval,Vinit,Beh,Timer,Host,Port,MScv1)
        ),

        % construct and install the new monitor in the library
        cons_monitor(MonitorId, SSpecId, ModelId, Properties, MSlang, MSid, MScv1, MSFullFile, Monitor),
        load_monitor(Monitor),
        true.

graph_monitor(Monitor) :- is_monitor(Monitor).

export_monitor(_Monitor).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Items to be used or generated during the multi-stage monitor creation process
%
%   SSpecId, SSpecData, ModelId, SMVmodel, LTLprops, MScv, Monitor
%
%       Monitor = monitor( MonId, SSpecId, ModId, LTLprops, MSlang, MSid, MScv, MSfile )
%       SMVmodel = model( ModId, ModelStruct, ModelText )
%       ModelStruct =
%       LTLprops = [ property(PropId, PropFormulaAtom), … ]
%       MSid = <location/name of file containing monitor sensor>
%       MScv = ms_cv( MonId, Vdecls, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port ) 
%       MSlang = ms_c | ms_pl
%       Vdecls = [ <var name> : <var type> ]
%       Atoms = [ <atom id> : <atom expr>, … ]
%       AEval = ms_eval | mep_eval
%       Vinit = [ <assign>, <assign>, … ]
%       Beh = [ <assign>, <assign>, … ]
%       Timer = <float number>
%       Host = <IP address as an atom>
%       Port = <port number as an integer>
