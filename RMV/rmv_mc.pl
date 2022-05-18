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
        is_service_spec(SS, SSpecId, SSpecBody),
        rmv_ml:service_spec(SSpecId, SSpecBody),
        NuRVcmds = [].

service_spec2monitor(SS, Monitor) :-
        create_smv_model( SS, SMVmodel ),
        create_ltl_properties( SS, Properties ),
        create_script( SS, NuCmds ), % generate preamble NuRV commands

        create_monitor(SS, SMVmodel, Properties, NuCmds, Monitor),
        %Properties = properties(PropAtoms,PropVars,PropFormulas),
        %Monitor = monitor( MonId, SSpecId, ModId, LTLprops, MSlang, MSid, MScv, MSfile ),
        true.

:- dynamic simulated_monitor_creation/0.
simulated_monitor_creation.

create_monitor(SS, Model, Properties, Cmds, Monitor) :-
        is_service_spec(SS,SSpecId,SSbody),
        is_service_spec_body(SSbody,Bitems),
        rmv_ml:load_service_specification(SS,SSpecId),

        is_model(Model),
        Properties = properties(Vp, PropAtoms, PropFormulas),

        forall( member(P,PropFormulas), is_property(P) ),
        forall( member(C,Cmds), is_cmd(C) ),

        % configure monitor sensor
        %
        % TODO - call NuRV to create and test monitor
        % TODO - Compute Vo, Vm, Vp, Vr, Vt
        Vo = [],
        Vm = [],
        Vr = [],
        Vt = [],
        Timer = 0,
        param:rmv_url(mep,MEP_URL),

        memberchk(monitor_sensor_lang=MSlang,Bitems),
        target_lang_monitor_sensor(MSlang,_,MSdir,MSfile,AEval),
        atomic_list_concat([MSdir,'/',MSfile], MSFullFile),

       % make a unique monitor sensor instance id
        uuid(U), atom_concat(msid_ , U, MSid),

        (       simulated_monitor_creation
        ->      rmv_ml:ex_cv(2,MScv),
                cons_ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, MEP_URL, MScv),
                (       memberchk(atom_eval_mode=OAEval,Bitems)
                ->      true % override the eval location
                ;       OAEval = AEval
                ),
                cons_ms_cv(MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, OAEval, Vinit, Beh, Timer, MEP_URL, MScv1)

        ;       cons_ms_cv(MonitorId,_Vdecl,_Vo,_Vm,Vp,_Vr,_Vt,PropAtoms,AEval,_Vinit,_Beh,_Timer,MEP_URL,MScv1)
        ),
        %MScv = ms_cv(MonitorId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port),
        %MScv = ms_cv(MonitorId, _,_,_,_,_,_,_,_,_,_,_,_,_),
        Model = model(ModelId,_SMVstruct,_SMVmodelFile,_SMVordFile),
        %SMVstruct = smv(_SMV, SMVmodeltext, SMVordtext, SMVvars),
        %SMVvars = vars(Vdecls,Vo,Vm,Vp,Vr,Vt),

        modid2monid(ModelId,MonitorId),
        cons_monitor(MonitorId, SSpecId, ModelId, Properties, MSlang, MSid, MScv1, MSFullFile, Monitor),

        % install the new monitor in the library
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
