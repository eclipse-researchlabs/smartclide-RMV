% RMV-specific command set

:- use_module('RMV/rmv').
:- use_module('RMV/rmv_ml').
:- use_module('SIM/ext_svcs').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of the RMV tool interactive commands syntax
% syntax( Signature, CommandSet ).
%
syntax(rmv,                            basic).
syntax(rmv_server,                                               rmv).
syntax(rmvt,                                                     rmv).
syntax(rmvt(test_id),                                            rmv).
syntax(rmvt(test_id,eval_mode),                                  rmv).
syntax(rmvt(test_id,eval_mode,target_lang),                      rmv).

syntax(rmvtests,                                                 rmv).

syntax(init_ms,                                                  rmv).

syntax(nurv_session,                                             rmv).
syntax(stop_nurv,                                                rmv).
syntax(import_sspec(serv_spec_file,serv_spec_id),                rmv).

syntax(sspec_load(serv_spec_id,smv_model),                       rmv).
syntax(sspec_smv(serv_spec_id,smv_model),                        rmv).
syntax(sspec_ltl(serv_spec_id,ltl_props),                        rmv).
syntax(sspec_nurv(serv_spec_id,nurv_script),                     rmv).

syntax(create_mon,                                               rmv).
syntax(graph_mon,                                                rmv).
syntax(export_mon,                                               rmv).

syntax(monitor_start(monitor_id),                                rmv).
syntax(monitor_stop(monitor_id,session_id),                      rmv).

syntax(nu_add_prop,                                              rmv).
syntax(nu_show_prop,                                             rmv).
syntax(nu_build_mon,                                             rmv).
syntax(nu_gen_mon,                                               rmv).

syntax(check_nameserver,                                         rmv).
syntax(start_nameserver,                                         rmv).
syntax(stop_nameserver,                                          rmv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RMV tool command semantics
% semantics(<signature with formal params>) :- <constraints>.
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
%
semantics(import_sspec(F,V)) :- !, atom(F), var(V).
semantics(monitor_start(M)) :- !, atom(M).
semantics(monitor_stop(M,S)) :- !, atom(M), ( atom(S) ; number(S) ).
semantics(rmvt(T)) :- !, atom(T).
semantics(rmvt(T,E)) :- !, atom(T), atom(E).
semantics(rmvt(T,E,L)) :- !, atom(T), atom(E), atom(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   help(Key,    HelpString)
%
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(rmv_server)"
%
help(rmv,       'Switch to rmv user mode.').
help(rmv_server,'Start the runtime monitoring and verification server.').
help(rmvt,      'run an rmv built-in test. Default is \'e2e\'.').
help(rmvt,      'Arg1 (opt) is a test identifier.').
help(rmvt,      'Arg2 (opt) is evaluation mode (ms_eval, mep_eval, no_eval).').
help(rmvt,      'Arg3 (opt) is monitor sensor language (ms_pl, ms_c).').

help(init_ms,	'Initialize the Prolog Monitor Sensor configuration.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
%
do(stop_nurv) :- !, rmv_mc_nui:quit_nurv_session.

do(monitor_start(M)) :- !, rmv_mf_mep:mep_monitor_start(M,Status),writeln(Status).
do(monitor_stop(M,S)) :- !, rmv_mf_mep:mep_monitor_stop(M,S,Status),writeln(Status).

do(rmv) :- user_mode(rmv), !, writeln('Already in rmv mode').
do(rmv) :- !, user_mode(M), retractall(user_mode(_)), assert(user_mode(rmv)),
	param:prompt_string(rmv,Prompt), param:setparam(prompt_string,Prompt),
	rem_commands(M), add_commands(rmv), banner(rmv).

do(init_ms) :- !, do(rmvt(init_ms_cv)).
do(rmv_server) :- !,
	%do(rmvt(init_ms_cv)),
	rmv_server:rmv_server_cmd.
	%writeln('not yet configured to start rmv_server').

do(rmvt) :- !, rmvt(e2e). % abbrev-change to suit current need
do(rmvt(ms_c)) :- !, rmvt(ms_c).
do(rmvt(ms_pl)) :- !, rmvt(ms_pl).
do(rmvt(e2e)) :- !, rmvt(e2e).
do(rmvt(e2e_c)) :- !, rmvt(e2e_c).
do(rmvt(local)) :- !, ext_svcs:e2e_api(local).
/*
do(rmvt(load_monitor)) :- !,
	cons_monitor(monid, SSpecId, ModelId, Properties, MSlang, MSid, MScv, MSfile, Monitor),
	% install the new monitor in the library
	load_monitor(Monitor).
*/
do(rmvt(init_ms_cv)) :- !, rmv_ms:initialize_ms_configuration.
do(rmvt(atom_eval)) :- !, rmvt(atom_eval).
do(rmvt(reports)) :- !, rmvt(reports).
do(rmvt(atom_eval,Mode)) :- !, rmvt(atom_eval,Mode).
%do(rmvt(T)) :- !, ext_svcs:e2e_api(T).
do(rmvt(T,Emode)) :- !, rmvt(T,Emode).
do(rmvt(T,Emode,MSlang)) :- !, rmvt(T,Emode,MSlang).

do(rmvtests) :- !, load_test_files([]), run_tests. % .plt tests

do(import_sspec(F,Sid)) :- !,
    load_service_specification_from_file(F,Sid).

do(check_nameserver) :- !, rmv:check_nameserver.
do(start_nameserver) :- !, rmv:start_nameserver.
do(stop_nameserver) :- !, rmv:stop_nameserver.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command procedures
%

behavior(1,[p=true,q=false,n=2,p=true,q=false,s=2,p=false,q=true,p=false,q=true]).
behavior(2,[n=5,p=false,o=7,r=3.14159,q=true]).
ssid(1,ssid_00002).
ssid(2,ssid_00003).
pl_goals(1,[false]).
pl_goals(2,[command:service_pl]).
eval_mode(1,ms_eval).
eval_mode(2,no_eval).
ms_lang(1,ms_pl).
ms_lang(2,ms_pl).

service_pl :-
	rmv_ms:ms_startup,
	%rmv_ms:ms_recovery( app_recovery ), % defined below
	rmv_ms:ms_responder,
    rmv_ms:ms_run_behavior,
	rmv_ms:ms_shutdown.

app_recovery(R) :- format('Service recovery callback invoked with ~q~n',R).


rmvt(e2e) :-
	% end-to-end from service/monitor creation through execution
	% uses the disjoint SMV model
	% builds the monitor and runs it as local e2e in ext_svcs
	% must still make real ms heartbeat message and nurv heartbeat in orbit mode
	% fake notifications are temporarily stubbed-out in MEP
	% this test now subsumes rmvt(ms_pl)
%  epp:epp_log_gen(monitor_event_processing, monitor_test(starting)),
	T=2,
	%behavior(T,Assigns),
	ssid(T,SSid), eval_mode(T,EvalMode), ms_lang(T,MSlang), pl_goals(T,Goals),
	ServiceCreationContext = [service_main=Goals,ssid=SSid,atom_eval_mode=EvalMode,monitor_sensor_lang=MSlang],
	ext_get_service_spec(ServiceCreationContext, ServiceSpec), % service spec will have the Main
	ext_service_spec2service(ServiceSpec,Service), % service now has the Main
	rmv_mc:service_spec2monitor(ServiceSpec,Monitor), % monitor configuration vector has the Main
	ext_deploy_service_with_monitor(Service,Monitor,Deployment), % Main is now in the deployment
%  epp:epp_log_gen(monitor_event_processing, monitor_test(ready_to_execute)),
	ext_execute_service(ms_pl,Deployment), % Main, other args are passed in with deployed service/monitor
	true.

rmvt(e2e_c) :-
	behavior(1,Assigns), SSid=ssid_00004,
	ServiceCreationContext = [behavior=Assigns,ssid=SSid,atom_eval_mode=ms_eval,monitor_sensor_lang=ms_c],
	ext_get_service_spec(ServiceCreationContext, ServiceSpec), % service spec will have the assigns
	%ext_service_spec2service(ServiceSpec,Service), % service has the assigns
	rmv_mc:service_spec2monitor(ServiceSpec,Monitor), % monitor configuration vector has the assigns (behavior)
	format('Monitor=~q~n',[Monitor]),
	true.

rmvt(ms_pl) :-
	% take the place of a SUS - just execution of assignments
	% fake the monitor creation (e2e test executes end-to-end flow)
    rmv_ml:ex_cv(2,CV), assert( rmv_ml:monitor(monid_00002,modid_00002,CV,_,_,ms_eval,_) ),
	rmv_ms:ms_startup,
	assigns(1,Assigns),
	forall(member(Var=Val,Assigns), rmv_ms:sv_setter(Var,Val)), % fake service execution
	rmv_ms:ms_shutdown.

rmvt(ms_c) :-
	true.

rmvt(atom_eval) :- param:rmv_atom_eval_mode(no_eval), !.
rmvt(atom_eval) :- !,
	do(rmvt(init_ms_cv)),
	rmv_ms:monitor_atoms(As),
	param:rmv_atom_eval_mode(M),
	(   ( M == ms_eval ; M == ms_cv )
	->  rmv_ms:aT_list_constructor(As,ATl)
	;   rmv_ms:or_list_constructor(ORl),
		rmv_mf_mep:aT_list_constructor(As,ORl,ATl)
	),
	writeq(As), nl, write('   -> '), writeq(ATl), nl,
	true.

rmvt(reports) :-
	rmv_ms:or_list_constructor(ORl),
	writeq(ORl), nl,
	true.

rmvt(atom_eval,Mode) :-
	param:rmv_atom_eval_mode(OldMode),
	param:setparam(rmv_atom_eval_mode,Mode),
	rmvt(atom_eval),
	param:setparam(rmv_atom_eval_mode,OldMode).

rmvt(Test,Emode) :-
	% set the evaluation mode. ServiceCreationContext provides an alternative way.
	param:setparam(rmv_atom_eval_mode,Emode),
	rmvt(Test).

rmvt(Test,Emode,MSlang) :-
	% set the evaluation mode
	param:setparam(rmv_atom_eval_mode,Emode),
	param:setparam(rmv_monitor_sensor_lang,MSlang),
	rmvt(Test).