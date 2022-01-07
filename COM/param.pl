% GLOBAL PARAMETERS OF THE TOG NGAC + EPP + RMV + PRIV
%
% all parameters should be defined here
%
:- module(param, [setparam/2,
                  debug/1, statusprt/1, guitracer/1, guiserver/1,
		  self_test/1, regression_test/1, initialize/1, verbose/1,
		  initialized/1, epp_initialized/1, user_level/1, settable_params/1,
		  self_test_modules/1, regression_test_modules/1,
		  local_pdf_viewer/2, local_dot_render/2,local_open_file/2,local_NuRV/2,local_NuRV_prompt/1,
                  local_nameserver/2, local_nameserver_IOR/1,
                  build_version/2, build_version/3,
                  build_current_version_description/2, build_name/3,
		  name_string/1, name_string/2, prompt_string/1, prompt_string/2, msg_failed_command/1,
		  msg_unimplemented_command/1,
		  msg_script_read/1, msg_running_script/1, msg_script_aborted/1,
		  policy_prefix/1, policy_language_version/1, server_version/1,
		  policy_directory_name/1, log_directory_name/1,
		  test_directory_name/1, graph_directory_name/1, monitor_directory_name/1,
		  files_directory_name/1, prettyprint_tab/1, raw_read_delay/1,
		  host_os/1, local_pdf_viewer/2, default_policy_file/1, graph_tmp_file/1,
		  current_policy/1, current_gpolicy/1, current_cpolicy/1, current_erp/1,
                  pqapi_port/1, paapi_port/1, gpqapi_port/1,
                  admin_token/1, all_composition/1,
                  server_sleeptime/1, audit_token/1, audit_logging/1, audit_stream/1, audit_record/1,
		  audit_selection/1, default_condition_variables/1,
                  conditions_file/1, default_condition_predicates/1,
                  context_file/1, eppapi_port/1, epp_token/1, rmv_token/1, rmv_epp_token/1,
                  epp_logging/1, epp_stream/1, null_stream/1, sleep_after_server_start/1,
                  jsonresp_epp/1, jsonresp_server/1, jsonresp/1, epp_status/1,
                  deny_resp/1, grant_resp/1,
                  localhost_ip/1, serverhost_ip/1, context_url/1, context_sim/1, epp_url/1,
                  context_port/1, exec_sim_port/1, warrant_port/1,
                  rmv_port/1, rmv_mcapi_port/1, rmv_mfapi_port/1, rmv_eppapi_port/1,
                  rmv_auditapi_port/1, rmv_lnapi_port/1, rmv_start_nameserver_on_init/1,
                  rmv_run_with_http_server/1, rmv_monitor_id_prefix/1, rmv_model_id_prefix/1
		 ]).

% Versioning of various things
%
% Past versions ngac_version/2 and current version ngac_version/1:
% When starting a new version create a new ngac_version/1 and
% add a description as a second argument to the preceding version.
%
% When development is actively going on, the current version, given by
% ngac_version/1, is version against which changes are currently being
% actively made and checked-in to the svn repository. It is not fixed.

:- discontiguous build_version/3, build_version/2, build_current_version_description/2.

build_version(ngac,'0.1','initial structure setup').
build_version(ngac,'0.1.1', 'initial development' ).
build_version(ngac,'0.2.1', 'initial demo').
build_version(ngac,'0.2.2', 'initial user trial version').
build_version(ngac,'0.2.3', 'cleanup  of trial version' ).
build_version(ngac,'0.3.1', 'initial server version with http interface' ).
build_version(ngac,'0.3.2', 'enhanced server version with separate PQ and PA APIs' ).
build_version(ngac,'0.3.3', 'added \'all\' composition, new options, policy change' ).
build_version(ngac,'0.3.4', 'added audit module, pqapi audit, other improvements' ).
build_version(ngac,'0.3.4+', 'multi-domain access control policies and access queries' ).
build_version(ngac,'0.3.5', 'paapi audit, local log, factor server, other improvements' ).
build_version(ngac,'0.4.1', 'EPP modules for conditional policy rules and context adaptation').
build_version(ngac,'0.4.2', 'EPP snapshot distribution' ).
build_version(ngac,'0.4.3', 'final EPP features and interfaces; accessm; json option' ).
build_version(ngac,'0.4.3', 'final EPP features and interfaces; accessm; json option' ).
build_version(ngac,'0.4.4', 'enhanced date and time conditions' ).
build_version(ngac,'0.4.5', 'conditional queries').
build_version(ngac,'0.4.6', 'pqapi/users who can access an object; conditional query variable substitutions').
build_version(ngac,'0.4.7', 'resets; early prototype of DPLP' ).

build_version(ngac,'0.4.8' /* ongoing development */ ).
build_current_version_description(ngac,'ongoing development of DPLP and privacy features; runtime monitoring').
%
build_version(priv,'0.1','initial structure setup').
build_version(priv,'0.1.1','initial development' ).

build_version(priv,'0.1.2' /* ongoing development */ ).
build_current_version_description(priv,'ongoing development of privacy tools for smashHit').
%
build_version(rmv,'0.1','initial structure setup').
build_version(rmv,'0.1.1','runtime monitoring initial development' ).

build_version(rmv,'0.1.2', 'monitor sensor, simple NuRV sessions, nameserver session' ).
build_version(rmv,'0.1.3' /* ongoing development */ ).
build_current_version_description(rmv,'NuRV sessions with nameserver,improved sequencing, tests, C monitor sensor').
%
build_version(epp,'0.1','initial structure setup').
build_version(epp,'0.1.1','initial development' ).

build_version(epp,'0.1.2' /* ongoing development */ ).
build_current_version_description(epp,'event processing point for RMV').

% Used by the command interpreter
%
prompt_string('').
prompt_string(ngac,'ngac').
prompt_string(priv,'priv').
prompt_string(rmv,'rmv').
prompt_string(epp,'epp').

build_name(ngac,'TOG-NGAC','TOG-ngac').
build_name(priv,'TOG-PRIV','TOG-priv').
build_name(rmv,'TOG-RMV','TOG-rmv').
build_name(epp,'TOG-EPP','TOG-epp').

name_string('').
name_string(ngac,'Next Generation Access Control - TOG').
name_string(priv,'Privacy specification & support - TOG').
name_string(rmv,'Runtime Monitoring & Verification - TOG').
name_string(epp,'Event Processing Point - TOG').

% SETTABLE PARAMETERS
%
% enter new params both in dynamic directive and settable_params
%
:- dynamic debug/1, statusprt/1, guitracer/1, guiserver/1, self_test/1, current_policy/1, prompt_string/1,
	regression_test/1, initialize/1, initialized/1, epp_initialized/1, verbose/1, name_string/1,
	user_level/1, current_policy/1, pqapi_port/1, paapi_port/1, gpqapi_port/1,
        admin_token/1, audit_token/1, audit_logging/1, audit_stream/1, audit_selection/1, audit_record/1,
        current_gpolicy/1, current_cpolicy/1, current_erp/1, epp_status/1,
        conditions_file/1, context_file/1, eppapi_port/1, epp_token/1, rmv_token/1, rmv_epp_token/1,
        epp_logging/1, epp_stream/1, null_stream/1, sleep_after_server_start/1,
        jsonresp_epp/1, jsonresp_server/1, jsonresp/1,
	serverhost_ip/1, context_url/1, context_sim/1, local_nameserver_IOR/1,
        rmv_run_with_http_server/1.

settable_params([debug,self_test,statusprt,guitracer,guiserver,initialize,initialized,regression_test,verbose,
		 user_level, current_policy, pqapi_port, paapi_port, gpqapi_port, admin_token, prompt_string,
		 audit_token, audit_logging, audit_stream, audit_selection, audit_record, name_string,
                 current_gpolicy, current_cpolicy,
                 current_erp, epp_initialized, epp_status,
                 conditions_file, context_file, eppapi_port, epp_token, rmv_token, rmv_epp_token,
                 epp_logging, epp_stream, null_stream, sleep_after_server_start,
                 jsonresp_epp, jsonresp_server, jsonresp, serverhost_ip, context_url, context_sim,
                 local_nameserver_IOR, rmv_run_with_http_server/1
                ]).

setparam(Param,Value) :- atom(Param), ground(Value),
    settable_params(SP), memberchk(Param,SP), !,
    P1 =.. [Param,_], retractall(P1),
    P2 =.. [Param,Value], assert(P2).
setparam(_,_).

debug(off). % off/on
statusprt(off). % off/on
guitracer(on). % off/on
guiserver(off). % off/on
self_test(off). % off/on
regression_test(off). % off/on
null_stream(x).
initialize(on). % off/on
initialized(false).
verbose(off). % off/on
user_level(developer). % default command user mode: basic/advanced/developer
%  no_sleep(off). % off/on - don't sleep after starting server from ngac
% tool command loop
sleep_after_server_start(on).

current_policy('none').
current_gpolicy('none').
current_cpolicy('none').
current_erp('none').

jsonresp_epp(off).
jsonresp_server(off).
jsonresp(off). % off / on / separate / same

% NGAC service API ports
pqapi_port(8001). % default pqapi server port
paapi_port(8001). % default paapi server port, currently same as pqapi
gpqapi_port(8001). % default gpqapi server port, currently same as pqapi
eppapi_port(8001). % default epp port, currently same as pqapi

% other service API ports
context_port(8002). % local, see context_url below
exec_sim_port(8003).
warrant_port(8004).

% RMV service API ports
rmv_port(8005).
rmv_mcapi_port(8005).
rmv_mfapi_port(8005).
rmv_eppapi_port(8005).
rmv_auditapi_port(8005).
rmv_lnapi_port(8005).

localhost_ip('127.0.0.1').
serverhost_ip('127.0.0.1').
% context_url('http://195.201.23.72:9095/cross-cpp/'). % ATB
context_url('http://127.0.0.1:8002/cross-cpp/'). % old local name
% context_url('http://127.0.0.1:8002/context/'). % new local name
context_sim(off). % off/on
deny_resp(deny).
grant_resp(grant).

all_composition(p_uo). % default qualification condition for 'all' composition
server_sleeptime(32767). % sleep main control repeatedly after server start
                         % periodic tasks will run before sleep is reinitiated

% AUDITING - In addition to (optionally) sending audit records to a
% system audit service, they will be written to a local log if
% audit_logging is not 'off'. The local log is sent to audit_stream
% (user_error by default). If audit_logging is 'file' then a file will
% be opened and audit_stream set to the open file stream.

audit_logging(file). % 'file' or 'on' or 'off'
audit_stream(user_error). % default stream for audit log (standard error)
audit_selection([]). % currently selected set of events for audit generation
audit_record('audit_log(~w, ~q, ~q, ~q).~n'). % format of the audit record [TS,Source,Event,EventData]

epp_url(EPP_URL) :-
    serverhost_ip(IP),
    eppapi_port(EP),
    atomic_list_concat(['http://',IP,':',EP,'/epp/'], EPP_URL).
epp_logging(file). % 'file', or 'on' (to std out), or 'off'
epp_stream(user_error). % default stream for EPP log (standard error)
epp_initialized(false).
epp_status(inactive). % inactive, policy_server, standalone, rmv_server

% AUTHORIZATION TOKENS
admin_token('admin_token'). % default policy admin token
audit_token('audit_token'). % default audit token
epp_token('epp_token'). % default epp token
rmv_token('rmv_token'). % default rmv token
rmv_epp_token('rmv_epp_token'). % default rmv_epp token

% DPL CONDITIONS
%
default_condition_variables([zero:number]). % variables used in condition predicates
default_condition_predicates([]).
% see below conditions and context files

% Modules providing functional tests
%
self_test_modules([pdp]).       % modules that provide self tests
regression_test_modules([pdp,priv]). % modules that provide regression tests

% Misc strings
%
msg_failed_command('command failed').
msg_unimplemented_command('Unimplemented command or option. Enter:<command>. help. or quit.').
msg_script_read('script read:').
msg_running_script('running script ...').
msg_script_aborted('script aborted').

policy_prefix('pol_').
policy_language_version('0.4').
server_version('0.4').


% Files and directories
%
policy_directory_name('RUNTIME/POLICIES').
test_directory_name('TEST').
graph_directory_name('RUNTIME/GRAPHS').
log_directory_name('RUNTIME/LOG').
monitor_directory_name('RUNTIME/MONITORS').
files_directory_name('RUNTIME/FILES').

default_policy_file('policy_tmp').
graph_tmp_file('graph_tmp').

conditions_file('NGAC/conditions.pl').
context_file('NGAC/context.pl').

% Misc values
%
prettyprint_tab(2). % tab indent for pretty printed output

host_os(os_x). % define only one
% host_os(linux). % define only one
% host_os(windows). % define only one
raw_read_delay(0.1). % 0.05 seems fairly reliable; 0.01 is too short to be reliable

% External utilities
%
local_pdf_viewer(os_x,'"/Applications/Adobe Reader 9/Adobe Reader.app/Contents/MacOS/AdobeReader"').
local_dot_render(os_x,'dot').
local_open_file(os_x,'open').
local_nameserver(os_x,'tnameserv').
local_nameserver_IOR('IOR;').
local_NuRV(os_x,'NuRV_orbit').
local_NuRV_prompt('NuRV > ').

% RMV Flags
%
rmv_start_nameserver_on_init(false).
rmv_run_with_http_server(true). % 'false' is used for testing (set to false by test harness)
rmv_monitor_id_prefix('monid_').
rmv_model_id_prefix('modid_').

