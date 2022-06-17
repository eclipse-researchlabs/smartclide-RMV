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
:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).

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
        rmv_ml:load_service_specification(SS,SSpecId), % commit this SS to the Monitor Library

        is_model(Model), cons_model(ModelId,_SMVstruct,_SMVmodelFile,_SMVordFile,Model),

        forall( member(C,Cmds), is_cmd(C) ),

        % make a unique monitor sensor instance id
        % modid2monid(ModelId,MonitorId),
        uuid(U), atom_concat(monid_ , U, MonitorId),
        param:monitor_directory_name(RMdir),
        atomic_list_concat([RMdir,'/',MonitorId,'_conf.json'],MScvFullFile),

        % configure monitor sensor
        %
        % TODO - call NuRV to create and test monitor
        % TODO - Compute Vo, Vm, Vp, Vr, Vt

        (       memberchk(monitor_sensor_lang=MSlang,Bitems)
        ->      true
        ;       fail % TODO something else
        ),
        target_lang_monitor_sensor(MSlang,_,MSdir,MSfile,MSEval),
        atomic_list_concat([MSdir,'/',MSfile], MSFullFile),

        (       simulated_monitor_creation
        ->      % take Eval and ServiceMain from SSpec and using MScv make a new ms_cv: MScv1
                rmv_ml:ex_cv(2,MScv), % ignore the monitor ID in the test MScv
                cons_ms_cv(_MonId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, AEval, Vinit, Beh, Timer, Host, Port, MScv),
                (       memberchk(atom_eval_mode=OAEval,Bitems)
                ->      true % override the eval location
                ;       OAEval = AEval
                ),
                (       memberchk(behavior=OBeh,Bitems)
                ->      true % override the behavior
                ;       OBeh = Beh
                ),
                cons_ms_cv(MonitorId, Vdecl, Vo, Vm, Vp, Vr, Vt, Atoms, OAEval, Vinit,  OBeh, Timer, Host, Port, MScv1)
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

        create_cvjson_file(MScv1,MScvFullFile),

        (       MSlang == ms_c
        ->      create_svh_file(MScv1,SVFullFile,MScvFullFile)
        ;       SVFullFile = none
        ),

        % construct and install the new monitor in the library
        %cons_monitor(MonitorId, SSpecId, ModelId, Properties, MSlang, MSfile, MScv1, MSFullFile, SVFullFile, Monitor),
        cons_monitor(MonitorId, SSpecId, ModelId, Properties, MSlang, MSFullFile, MScv1, MScvFullFile, SVFullFile, Monitor),
        load_monitor(Monitor),
        true.

create_cvjson_file(MScv,MScvFullFile) :-
        rmv_ml:ms_cv_to_pjson(MScv,PJCV),
        atom_json_term(JSA,PJCV,[as(atom)]),
        open(MScvFullFile,write,S,[create([default])]),
        write(S,JSA),
        close(S,[force(true)]).

create_svh_file(MScv,SVFullFile,MScvFullFile) :-
        % pull MonitorId and Vdecl from the MScv
        cons_ms_cv(MonitorId,Vdecl,_Vo,_Vm,_Vp,_Vr,_Vt,_PropAtoms,_AEval,_Vinit,_Beh,_Timer,_Host,_Port,MScv),

        monid2monvarshfile(MonitorId,SVfile),
        param:monitor_directory_name(MD),
	atomic_list_concat([MD,'/',SVfile],SVFullFile),

        %current_output(OS),
        open(SVFullFile,write,S,[create([default])]),
        forall( member(L, [
                % file header
                '// This file is created by RMV Monitor Creation for variables',
                '// shared between the SUS and the Monitor Sensor.',
                '// These declarations (must) appear in the same order as in the',
                '// shared variable list in the MS configuration vector generated',
                '// by Monitor Creation. This file is included as monitor_vars.h',
                '// by sensor.h of the C monitor sensor.' ]),
                writeln(S,L)),
        format(S,'// This file is paired with ~s~n',MScvFullFile),
        % variable declarations
        nl(S),
        forall( ( member(Vname:Vtype,Vdecl), type_pl_n_e_c(Vtype,_,_,Ctype) ),
                ( atomic_list_concat([Ctype,' ',Vname,';'],DeclLine), writeln(S,DeclLine)) ),
        % getters and setters for each variable
        nl(S),
        forall( ( member(Vname:Vtype,Vdecl), type_pl_n_e_c(Vtype,_,_,Ctype) ),
                ( atomic_list_concat(['void get_',Vname,'(void*ip){*(',Ctype,'*)ip=',Vname,';}'],GetrLine),
                  atomic_list_concat(['void set_',Vname,'(void*newp, void*oldp){*(',Ctype,'*)oldp = ',
                        Ctype,'_setter_by_addr(&',Vname,',*(',Ctype,'*)newp);}'],
                        SetrLine),
                  writeln(S,GetrLine), writeln(S,SetrLine)
                )
        ),
        % shared_var_attr_t table
        nl(S),
        writeln(S,'shared_var_attr_t shared_var_attrs[] = {'),
        writeln(S,'//  va_name va_type va_addr va_trig va_rep va_prop va_getter va_setter'),

        forall( ( member(Vname:Vtype,Vdecl), type_pl_n_e_c(Vtype,_,Enum,_) ),
                ( atomic_list_concat(['    {"',Vname,'", ',Enum,', &',Vname,
                        ', false, false, false, &get_',Vname,', &set_',Vname,'},'], AtLine),
                  writeln(S,AtLine)
                )
        ),
        writeln(S,'    0\n};'),
        % define N_SHARED_VARS
        nl(S),
        length(Vdecl,Nvars),
        format(S,'#define N_SHARED_VARS ~d~n',Nvars),
        % dump_defined_vars
        nl(S),
        writeln(S,'void dump_defined_vars(){\n    printf("defined vars DIRECT access:\\n");'),
        forall( ( member(Vname:Vtype,Vdecl), member(Vtype,[boolean,integer,float,address]) ),
                  ( nth0(N,[boolean,integer,float,address],Vtype), nth0(N,['%s','%d','%f','%lu'],Ft), !,
                    ( Vtype == boolean -> Ftplus='?"true":"false"' ; Ftplus='' ),
                    atomic_list_concat(['    printf(" ',Vname,'=',Ft,'\\n",',Vname,Ftplus,');'], PLine),
                    writeln(S,PLine)
                  )
        ),
        writeln(S,'    fflush(stdout);\n}'),
        % global monitor id
        nl(S),
        atomic_list_concat(['static const char global_monitor_id[] = "',MonitorId,'";'], MidLine),
        writeln(S,MidLine),
        nl(S),
        close(S),
        %current_output(OS),
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
