%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of the EPP tool interactive commands syntax
% syntax( Signature, CommandSet ).
%
syntax(activate_erp(erp_name),                                  epp).
syntax(current_erp,                                             epp).
syntax(deactivate_erp(erp_name),		                epp).
syntax(epp,                            basic).
syntax(epp_server,                                              epp).
syntax(epp_server(port),                                        epp).
syntax(epp_server(port,token),                                  epp).
syntax(load_erf(erp_file),                                      epp).
syntax(unload_erp(erp_name),                                    epp).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EPP tool command semantics
% semantics(<signature with formal params>) :- <constraints>.
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
%
semantics(activate_erp(Erp_name)) :- !, atom(Erp_name).
semantics(deactivate_erp(Erp_name)) :- !, atom(Erp_name).
semantics(epp_server(Port)) :- !, integer(Port).
semantics(epp_server(Port,Token)) :- !, integer(Port), atom(Token).
semantics(load_erf(ERF)) :- atom(ERF).
semantics(unload_erp(ERP)) :- atom(ERP).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   help(Key,    HelpString)
%
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(epp_server)"
%
help(activate_erp, 'Activate an event-response package alreadly loaded.').
help(activate_erp, 'Arg is the name of a loaded ER package.').

help(current_erp, 'Display the name of the current active ER package.').

help(deactivate_erp, 'Deactivate an event-response package alreadly loaded.').
help(deactivate_erp, 'Arg is the name of a loaded ER package.').

help(epp,       'Switch to EPP user mode (with no arguments).').

help(epp_server,'Start the EPP server.').
help(epp_server,'Arg1 is the port number.').
help(epp_server,'Arg2 (optional) is an admin token.').

help(load_erf, '"load" an event-response package from a file').

help(unload_erp, '"unload" an event-response package').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
%
do(activate_erp(ERPname)) :- !, epp_era:activate_loaded_erp(ERPname).

do(current_erp) :- !, param:current_erp(Erp), writeln(Erp).

do(deactivate_erp(ERPname)) :- !, epp_era:deactivate_loaded_erp(ERPname).

do(epp) :- user_mode(epp), !, writeln('Already in epp mode').
do(epp) :- !, user_mode(M), retractall(user_mode(_)), assert(user_mode(epp)),
	param:prompt_string(epp,Prompt), param:setparam(prompt_string,Prompt),
	rem_commands(M), add_commands(epp), banner(epp).

do(epp_server) :- !, writeln('not starting epp_server'). % epp_server:epp_server.
do(epp_server(Port)) :- !, epp:epp(Port).
do(epp_server(Port,Token)) :- !, epp:epp(Port,Token).

do(import(erp(ERLfile))) :- !, do(load_erf(ERLfile)).

do(load_erf(ERLfile)) :- % load an event-response package from file
	epp:load_erp(ERLfile,ERPname),
	ui:notify('Event-Response Package loaded',ERPname).

do(unload_erp(ERPname)) :- % load an event-response package from file
	epp:unload_erp(ERPname),
	ui:notify('Event-Response Package unloaded',ERPname).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command procedures
%


