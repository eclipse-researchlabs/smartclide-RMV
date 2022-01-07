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
help(rmvt,      'Arg2 (opt) is evaluation mode (ms_eval, mep_eval).').
help(rmvt,      'Arg3 (opt) is monitor sensor language (ms_pl, ms_c).').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
%
do(stop_nurv) :- !, rmv_mc_nui:quit_nurv_session.

do(rmv) :- user_mode(rmv), !, writeln('Already in rmv mode').
do(rmv) :- !, user_mode(M), retractall(user_mode(_)), assert(user_mode(rmv)),
	param:prompt_string(rmv,Prompt), param:setparam(prompt_string,Prompt),
	rem_commands(M), add_commands(rmv), banner(rmv).
do(rmv_server) :- !,
	% rmv_server:rmv_server.
	writeln('not yet configured to start rmv_server').

do(rmvt) :- !, rmvt(e2e). % abbrev-change to suit current need
do(rmvt(ms_c)) :- !, rmvt(ms_c).
do(rmvt(ms_pl)) :- !, rmvt(ms_pl).
do(rmvt(e2e)) :- !, rmvt(e2e).
do(rmvt(local)) :- !, ext_svcs:e2e_api(local).
do(rmvt(T)) :- !, ext_svcs:e2e_api(T).
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

assigns(1,[p=true,q=false,n=2,p=true,q=false,s=2,p=false,q=true,p=false,q=true]).
ssid(ssid_00002).

rmvt(e2e) :-
	% end-to-end from service/monitor creation through execution
	% uses the disjoint SMV model
	% builds the monitor and runs it as local e2e in ext_svcs
	% must still make real ms heartbeat message and nurv heartbeat in orbit mode
	% fake notifications are temporarily stubbed-out in MEP
	% this test now subsumes rmvt(ms_pl)
	assigns(1,Assigns), ssid(SSid),
	ServiceCreationContext = [assigns=Assigns,ssid=SSid,atom_eval_mode=ms_eval,monitor_sensor_lang=ms_pl],
	ext_get_service_spec(ServiceCreationContext, ServiceSpec), % service spec will have the assigns
	ext_service_spec2service(ServiceSpec,Service), % now service has the assigns
	rmv_mc:service_spec2monitor(ServiceSpec,Monitor),
	ext_deploy_service_with_monitor(Service,Monitor,Deployment),
	ext_execute_service(ms_pl,Deployment), % assigns, other args are passed in with deployed service/monitor
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

rmvt(Test,Emode) :-
	% set the evaluation mode. ServiceCreationContext provides an alternative way.
	retractall( rmv_ml:atom_eval_mode(_) ), assert( rmv_ml:atom_eval_mode(Emode) ),
	rmvt(Test).

rmvt(Test,Emode,MSlang) :-
	% set the evaluation mode
	retractall( rmv_ml:atom_eval_mode(_) ), assert( rmv_ml:atom_eval_mode(Emode) ),
	% set the monitor sensor language
	retractall( rmv_ml:monitor_sensor_lang(_) ), assert( rmv_ml:monitor_sensor_lang(MSlang) ),
	rmvt(Test).