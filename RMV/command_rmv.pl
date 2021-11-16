%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of the RMV tool interactive commands syntax
% syntax( Signature, CommandSet ).
%
syntax(rmv,                            basic).
syntax(rmv_server,                                               rmv).
syntax(rmvt,                                                     rmv).

syntax(nurv_session,                                             rmv).
syntax(import_sspec(serv_spec_file,serv_spec_id),                  rmv).

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RMV tool command semantics
% semantics(<signature with formal params>) :- <constraints>.
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
%
semantics(import_sspec(F,V)) :- !, atom(F), var(V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   help(Key,    HelpString)
%
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(rmv_server)"
%
help(rmv,       'Switch to rmv user mode.').
help(rmv_server,'Start the runtime monitoring and verification server.').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
%
do(rmv) :- user_mode(rmv), !, writeln('Already in rmv mode').
do(rmv) :- !, user_mode(M), retractall(user_mode(_)), assert(user_mode(rmv)),
	param:prompt_string(rmv,Prompt), param:setparam(prompt_string,Prompt),
	rem_commands(M), add_commands(rmv), banner(rmv).
do(rmv_server) :- !, writeln('not starting rmv_server'). % rmv_server:rmv_server.
do(rmvt) :- !, ext_svcs:e2e_api(test).
do(import_sspec(F,Sid)) :- !, sspec_load(F,Sid,_SMV_model).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command procedures
%
%
