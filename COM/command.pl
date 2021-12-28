% COMMAND INTERPRETER USER INTERFACE
% and definition of the interactive commands

:- module(command, [ tl/0, tl/1 ]).

:- use_module(param).
:- use_module(ui).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool and RMV tool modes of operation and available commands
%
% Mode of operation and available commands determined by several factors:
%
% Mode - argument given when the command interpreter is invoked
%
% Available commands are based on a default user level defined in param.
%
% CommandSet:
%  'ngac' for commands in ngac user mode
%  'rmv' for commands in rmv user mode
%  'general' for commands that may be used in any user mode
%  'advanced' commands
%  'developer' for commands such as inspect,regtest,reinit
%  'obsolete' for commands that are no longer used and may not work
%
% Each command is declared to be associated with a single command set.
%
% available_commands/1 is a list of the currently available command sets
% See end of this file for manipulation of available_commands.
%
% The command interpreter currently has NGAC and
% RMV modes: NGAC mode: ngac, general, advanced & developer commands RMV
% mode: rmv, general, advanced & developer commands
%
%   general commands are available in every mode to all user levels
%   advanced and developer commands can be added obsolete
%   commands should not appear in available_commands
%
% How available commands are determined:
%   The top level command interpreter tl/1 is invoked with a mode
%   of 'ngac' or 'rmv'. Invoking tl/0 causes tl(ngac) to be invoked.
%
%  UserMode in the command syntax table
%  user_mode/1 dynamic fact - initialized to param:user_level in tl/1
%    values are: ngac, rmv
%  Mode argument passed to tl/1 - ngac or rmv
%  Prompt
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool and RMV tool command declarations
%
% syntax( Signature, CommandSet )
%
% There must be a syntax entry for every command form.
% Do not create a 0-ary command with the name "invalid" (see rd/2)
%
% Commands are listed alphabetically in syntax/2, semantics/1 and do/1
% These declarations may also be made in separate files and included.

:- discontiguous syntax/2, semantics/1, help/2, do/1, commands_defined/1.


:- if( exists_file('NGAC/command_ngac.pl') ).
:- include('NGAC/command_ngac.pl').
commands_defined(ngac).
:- endif.

:- if( exists_file('PRIV/command_priv.pl') ).
:- include('PRIV/command_priv.pl').
commands_defined(priv).
:- endif.

:- if( exists_file('EPP/command_epp.pl') ).
:- include('EPP/command_epp.pl').
commands_defined(epp).
:- endif.

:- if( exists_file('RMV/command_rmv.pl') ).
:- include('RMV/command_rmv.pl').
commands_defined(rmv).
:- endif.

commands_defined(basic).
commands_defined(advanced).
commands_defined(developer).

defined_commands(CmdSets) :- findall(CmdSet, commands_defined(CmdSet), CmdSets).
% e.g.: defined_commands([ngac,priv,epp,rmv,basic,advanced,developer]).

%
syntax(advanced,                                    basic).
syntax(basic,					    basic).
syntax(conditions,			            basic).
syntax(conditions(name),		            basic).
syntax(demo(demo_command),                          basic).
syntax(developer,                                   basic).
syntax(echo(string),                                basic).
syntax(guitracer,						       developer).
syntax(guiserver,			            basic).
syntax(halt,                                        basic).
syntax(help,                                        basic).
syntax(help(command),				    basic).
syntax(inspect,                                                         developer).
syntax(inspect(item),                                                   developer).
syntax(load_cond(cond_file),			              advanced).
syntax(load_cond(cond_name,cond_file),			      advanced).
syntax(make,                                                            developer).
syntax(noop,					    basic).
syntax(nl,                                          basic).
syntax(proc(proc_id),                               basic).
syntax(proc(proc_id,step_or_verbose),		    basic).
syntax(quit,                                        basic).
syntax(regtest,								developer).
syntax(reset,					              advanced).
syntax(reset(domain,name),				      advanced).
syntax(reinit,                                                          developer).
syntax(script(file),                                basic).
syntax(script(file,step_or_verbose),		    basic).
syntax(selftest,					      advanced).
syntax(set,						      advanced).
syntax(set(name),				              advanced).
syntax(set(name,value),				              advanced).
syntax(status,				            basic).
syntax(step,								developer).
syntax(step(number_of_steps),						developer).
syntax(time(command),                                         advanced).
syntax(time(command,repeat),			              advanced).
syntax(traceoff,					                developer).
syntax(traceon,					                        developer).
syntax(traceone,					                developer).
syntax(version,				            basic).
syntax(versions,				    basic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool command semantics
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
semantics(conditions(N)) :- !, atom(N).
semantics(demo(C)) :- !, ground(C).
semantics(echo(S)) :- !, atomic(S).
semantics(help(C)) :- !, ground(C).
semantics(import_model(M)) :- atom(M).
semantics(import_pm(PM)) :- atom(PM).
semantics(inspect(I)) :- nonvar(I).
semantics(load_cond(CondF)) :- atom(CondF).
semantics(load_cond(CondN,CondF)) :- atom(CondN), atom(CondF).
semantics(proc(P)) :- !, atom(P).
semantics(proc(P,Opt)) :- !, atom(P), (Opt==step;Opt==s;Opt==verbose;Opt==v). % other opts can be added
semantics(reset(Dom,Name)) :- !, atom(Dom), atom(Name).
semantics(script(F)) :- !, atom(F).
semantics(script(F,Opt)) :- !, atom(F), (Opt==step;Opt==s;Opt==verbose;Opt==v). % other opts can be added
semantics(set(N)) :- !, atom(N).
semantics(set(N,V)) :- !, atom(N), ground(V).
semantics(step(N)) :- !, (integer(N) ; N == break), !.
semantics(time(C)) :- !, ground(C).
semantics(time(C,N)) :- !, ground(C), integer(N).
semantics(_). % succeed for all other commands

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(assess)"
%
%   help(Key,    HelpString)
help(advanced,  'Switch to advanced user level, enabling all commands.').

help(basic,     'Switch to basic user level, limiting available commands to basic command set.').

help(conditions,'Display current condition variable and condition predicate declarations.').
help(conditions,'Arg1 (optional) the name of the conditions to display (predefined/static/dynamic/...).').

help(demo,	'Run canned demos.'). % command for running canned demos of different features
help(demo,      'Arg is demo identifier.').

help(echo,      'echo a (single-quoted) string argument.').

help(halt,	'Leave NGAC command loop and Prolog.').

help(help,	'"help" with no argument lists the legal command forms.').
help(help,	'With a command name as argument it provides help on that command.').

% help(import_pm, '"import" a policy in PM imperative commands from a file').

help(inspect,	'Inspect values of internal structures or variables based on arg.').
help(inspect,	'arg options: settings, xml, str, prm, current or other structures.').
help(inspect,	'arg: target(<target>,<element>) will show intermediate facts.').

help(load_cond, '"load" condition declarations and definitions from a file').
help(load_cond, 'Arg1 is the conditions file to be loaded.').
help(load_cond, 'Arg2 (opt) is the name to be associated with conditions (default "dynamic").').

help(make,	'Recompile changed source files.').

help(nl,        'Write a newline to the console.').

help(proc,	'Run a stored NGAC command procedure.').
help(proc,	'Arg 1 is a procedure identifier.').
help(proc,      'Arg 2 (optional) is "step" or "verbose".').

help(quit,	'Terminate the NGAC top-level command loop or a command script; stay in Prolog.').

help(regtest,   'Run regression tests.').

help(reinit,	'Re-initialize.').

help(reset,     'Reset policy or condition databases.').
help(reset,	'Arg 1 is the domain (conditions or policy) to be reset.').
help(reset,     'Arg 2 is the name of the group to be reset.').

help(script,	'Run a NGAC command script from a file.').
help(script,	'Arg 1 is the file name.').
help(script,	'Arg 2 (optional) is "step" or "verbose".').

help(selftest,  'Run self tests.').

help(set,	'With no argument displays all settable parameters.').
help(set,	'Arg 1 is name of a paramater. If only one arg, display its value.').
help(set,	'Arg 2, if present, is the value to set.').
help(set,       'Settable: cache, debug, initialize, statusprt, self_test, regression_test, verbose.').

help(status,	'Display NGAC system status.').

help(step,	'"step" with no argument steps engine one cycle.').
help(step,	'With an integer argument it steps the engine that number of cycles.').

help(time,	'Execute command and report time stats.').
help(time,	'With an integer second argument, execute command repeatedly and report total time stats.').

help(traceoff,  'Turn Prolog tracing off.').
help(traceon,   'Turn Prolog tracing on.').
help(traceone,	'Turn Prolog tracing on for one NGAC command.').

help(version,	'Show current version number.').
help(versions,	'Show past versions with descriptions and current version.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% do the command, should be one for every implemented valid command form
% known broken or unimplemented commands should just "fail." straightaway
% interactive_do provides an appropriate message for interactive use of
% a command that is known to be invalid because it fails syntax or
% semantics check, does not have a do entry, or fails in do.
% (Would be better to distinguish between unimplemented and failed do,
% which is what the fail_act in tl is for.)
% As it is now, commands with an entry in do that fail are reported
% in the interactive_do as unimplemented commands.
%
do(advanced) :- !, do(level(advanced)).
do(basic) :- !, do(level(basic)).

do(conditions) :- !, policyio:display_conditions.
do(conditions(N)) :- !, policyio:display_conditions(N).

do(demo(C)) :- !, perform_demo(C).
do(developer) :- !, do(level(developer)).
do(echo(S)) :- !, writeln(S).
do(guitracer) :- !,
	(   param:guitracer(off)
	->  setparam(guitracer,on),
	    guitracer
	;   true).
do(guiserver) :- !,
	(   param:guiserver(off)
	->  do(set(guiserver,on)),
	    do(guitracer),
	    do(set(jsonresp_server,on)), % turns on JSON responses for policy server
	    do(set(jsonresp,on)),
	    do(set(sleep_after_server_start,off)),
	    % do(traceone),
	    do(server(8001)),
	    do(echo(ready))
	;   do(echo('already on'))
	).

do(help) :- !, help_commands.
do(help(C)) :- !, show_help(C).

do(inspect) :- !, writeln('inspect(opt). options: settings').
do(inspect(Item)) :- !, inspect(Item).

do(level(L)) :- !, retractall(user_lev(_)), assert(user_lev(L)),
	user_mode(M), level_commands(L,LCs), union([M],LCs,Cmds), set_avail_commands(Cmds).

do(load_cond(CondFile)) :- !, % load conditions from file
	do( load_cond(CondFile,dynamic) ).

do(load_cond(CondFile,CondName)) :- !, % load conditions from file
	exists_file(CondFile), read_file_to_terms(CondFile,CondTerms,[]),
	format('CondTerms read:~q~n',[CondTerms]),
	pap:dynamic_add_cond_elements(CondName,CondTerms),
	format(atom(M),'Conditions file ~q loaded',[CondFile]),
	ui:notify(CondName,M).

do(make) :- !, make.
do(noop) :- !.
do(nl) :- nl.

do(proc(Pid)) :- !, do(proc(Pid,none)).
do(proc(Pid,Opt)) :- !, procs:proc(Pid,Proc), user_mode(M),
	retractall(interactive(_)), assert(interactive(false)),
	run_commands(M,Proc,Opt),
	retractall(interactive(_)), assert(interactive(true)).
do(quit) :- !.
do(halt) :- !, halt.
do(regtest) :- !, user_mode(M), M:regression_test_all.
do(reinit) :- !, writeln('No top-level reinit currently').
do(reset) :- !, pap:preset(conditions,all).
do(reset(D,N)) :- !, pap:preset(D,N).
do(script(F)) :- !, user_mode(M), run_command_script(M,F,none).
do(script(F,Opt)) :- !, param:prompt_string(P), run_command_script(P,F,Opt).
do(selftest) :- !, user_mode(M), M:self_test_all, /* others ... */ true.
do(set) :- !, param:settable_params(Ps), forall(member(P,Ps),do(set(P))). % display all settable params
do(set(P)) :- param:settable_params(Ps), member(P,Ps), !, % display a settable param
	Q =.. [P,V], call(param:Q), format('~a=~w~n',[P,V]).
do(set(_)) :- !, writeln('Unknown parameter name').
do(set(debug,V)) :- (V == on ; V == off), !, do(debug(V)).
do(set(statusprt,V)) :- (V == on ; V == off), !, do(statusprt(V)).
do(set(self_test,V)) :- (V == on ; V == off), !, do(self_test(V)).
do(set(P,V)) :- param:settable_params(Ps), member(P,Ps), !, param:setparam(P,V).

do(set(initialize,V)) :- (V == on ; V == off), !, param:setparam(initialize,V).
do(set(regression_test,V)) :- (V == on ; V == off), !, param:setparam(regression_test,V).
do(set(verbose,V)) :- (V == on ; V == off), !, param:setparam(verbose,V).
do(set(policy,P)) :- !, do(newpol(P)).
% add cases for other parameter settings here
do(set(P,V)) :- atom(P), ground(V), param:setparam(P,V), !.
do(set(_,_)) :- !,
	writeln('Unknown parameter name or illegal parameter value').
do(status) :- user_mode(M), param:name_string(M,N), user_lev(L),
	write(' '), writeln(N),
	write('   Mode: '), writeln(M),
	write('   Level: '), writeln(L),
	available_commands(Cmds), write('   Command sets: '), writeln(Cmds).
do(time(Command)) :- !, time(do(Command)).
do(time(Command,N)) :- !,
	current_output(S), param:null_stream(Null), set_output(Null),
	time( (foreach(between(1,N,_), do(Command)), set_output(S)) ).
do(traceon) :-	retractall(tracing(_)), assert(tracing(on)), trace.
do(traceone) :-	retractall(tracing(_)), assert(tracing(set)).
do(traceoff) :- retractall(tracing(_)), assert(tracing(off)), notrace.

do(version) :- !, param:prompt_string(Mode),
	param:build_version(Mode,Cur), param:build_current_version_description(Mode,Desc),
	format('Current version: ~a: ~a~n',[Cur,Desc]).
do(versions) :- !, param:prompt_string(Mode),
	forall(param:build_version(Mode,V,D), format('~t~17|~a: ~a~n',[V,D])),
	do(version).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic tracing/1, user_mode/1, user_lev/1, interactive/1, tl_initialized/1.

% tracing
% values: on, off, set, and one
%         set changes to one in mid next iteration
%         one changes to off after the next command is run
tracing(off).

% user_mode
% values: ngac / rmv / epp
user_mode(ngac). % default

% user_lev
% values: basic / advanced / developer
user_lev(basic). % default, will be set to param:user_level

% interactive
% true when reading from user interaction
interactive(true).

% top level initialized
tl_initialized(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-level command loop
%
% tl(Mode)
%

tl :- tl(ngac).

tl(_) :- tl_initialized(true), !, tl_loop.
tl(Mode) :-
	param:user_level(Ulev), % user_level param overides user_mode default
	retractall(user_lev(_)), assert(user_lev(Ulev)),
	add_commands(Ulev), % basic / advanced / developer
	(   Ulev == developer -> add_commands(advanced) ; true),
	retractall(user_mode(_)), assert(user_mode(Mode)),
	add_commands(Mode),
	banner(Mode),
	retractall(tl_initialized(_)), assert(tl_initialized(true)),
	tl_loop.

tl_loop :-
	repeat,
	        user_mode(Mode),
		param:prompt_string(Mode,Prompt),
	        pre_act, rd(Prompt,C), mid_act(C),
		(   interactive_do(Mode,C)
		->  true
		;   fail_act
		),
		post_act,
	(C == quit, ! ; fail).


banner(Mode) :-
	param:build_version(Mode,V), param:name_string(Mode,Name),
	format('~n~a version ~a~n',[Name,V]),
	nl.

pre_act :- % do before reading the command
	true.
mid_act(_) :- % do after reading the command but before do-ing it
	(   tracing(set)
	->  retractall(tracing(_)),
	    assert(tracing(one)),
	    trace
	;   true
	).
post_act :- % do after performing the command or after fail_act
	(   tracing(one)
	->  retractall(tracing(_)),
	    assert(tracing(off)),
	    notrace
	;   true
	),
	(param:statusprt(on) -> do(status);true),
	nl, !.
fail_act :- % do when a command fails
	(   tracing(one)
	->  retractall(tracing(_)),
	    assert(tracing(off)),
	    notrace
	;   true
	),
	param:msg_failed_command(M),
	ui:notify('interactive',M).

interactive_do(_,invalid) :- !, unimplemented_command.
interactive_do(CS,C) :- param:prompt_string(CS), !, do(C).
interactive_do(CS,C) :-	 atom(CS), DO =.. [CS,C], !, call(CS:DO).
interactive_do(_,_) :- unimplemented_command.

unimplemented_command :- param:msg_unimplemented_command(M), writeln(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read and validate a command:
% execute a Prolog goal preceded by :- or ?-
% or check whether a valid NGAC command
% return invalid if not found or fails checks
%
rd(Prompt,C) :-
	atom_concat(Prompt,'> ',FullPrompt),
	read_history(h, '!h', [], FullPrompt, C, _Bindings),
	nonvar(C), % nonvar instead of ground to allow Prolog goals w/vars
	(   (C=..[:-,P];C=..[?-,P]) % command is a Prolog goal
	->  call(P), nl, !, fail    % bypass other goals in tl, repeat
	;   chk_command(C)          % check the command, fail to 'invalid'
	), !.
rd(_,invalid).

%chk_command(CommandSet,C) :-
%	param:prompt_string(Prompt),
%	(   CommandSet == Prompt
%	->  syntax_chk(C),
%	    semantics(C)
%	;   Check =.. [cmd,C,_,_],
%	    clause(CommandSet:Check,true)
%	).
chk_command(C) :-
	available_commands(CommandSets),
	syntax_chk(C,CommandSets),
	semantics(C).

%syntax_chk(C) :-
%	functor(C,F,A), functor(Sig,F,A), user_lev(M),
%	(   M == advanced
%	->  syntax(Sig,_)
%	;   syntax(Sig,M)
%	).
syntax_chk(C,CSets) :-
	functor(C,F,A), functor(Sig,F,A),
	syntax(Sig,CSet),
	memberchk(CSet,CSets).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command scripts
%

run_command_script(Mode,F,Opt) :-
	(   access_file(F,read)
	->  (
	        read_file_to_terms(F,Commands,[]),
	        (   Opt == verbose
	        ->  param:msg_script_read(Mread), writeln(Mread),
		    ui:display_list(Commands,1),
	            param:msg_running_script(Mrun), writeln(Mrun)
	        ;   true
	        ),
	        run_commands(Mode,Commands,Opt)
	    )
	;
	    format('can''t find file "~a"~n', F)
	), !.

run_commands(_,[],_) :- !.
run_commands(Mode,[C|Cs],Opt) :-
	(
	    (	(Opt == step ; Opt == s)
	    ->	format('~n> ~q. ?', C), flush_output, readln(_)
	    ;	(Opt == verbose ; Opt == v)
	    ->	format('> ~q.~n', C)
	    ;	true
	    ),
	    (   (C=..[:-,P] ; C=..[?-,P]) % command is a Prolog goal
	    ->  call(P)
	    ;   % ground(C),
	        chk_command(C),          % check the command, fail to 'invalid'
		(   user_mode(Mode)
		->  do(C)
		;   atom(Mode), DO =.. [Mode,C], call(Mode:DO)
		)
	    )
	    ;   format('~q : ',[C]),
		param:msg_failed_command(CM), writeln(CM),
		param:msg_script_aborted(SM), writeln(SM),
		Abort=true
	),
	((C == quit; Abort == true), ! ; run_commands(Mode,Cs,Opt)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Available commands
%
% Command sets and categories
%  'basic' for commands that may be used in any user mode
%  'advanced' commands
%  'developer' for commands such as inspect,regtest,reinit
%  'obsolete' for commands that are no longer used and may not work
% Modular command sets brought in using ':-include'
%  'ngac' for commands in ngac user mode
%  'priv' for commands in privacy user mode
%  'rmv' for commands in rmv user mode
%  'epp' for commands in the epp user mode
%
% Each command is declared to be associated with a single command set.
%
% available_commands/1 is a list of the currently available command sets
%  This is initialized when the top level loop tl/1 is started.
%
%  Values of available_commands will be, e.g.:
%    [ngac,basic] [ngac,basic,advanced] [ngac,basic,advanced,developer]
%    [rmv,basic]  [rmv,basic,advanced]  [rmv,basic,advanced,developer]
%    [ngac,priv,rmv,epp,basic,advanced,developer]

:- dynamic avail_commands/1.
avail_commands([basic]). % always start with the basic commands

available_commands(AvailCmds) :- avail_commands(AvailCmds).
%	user_lev(L), user_mode(M),
%	level_commands(L,LCs),
%	append([[M],LCs],Cmds),
%	avail_commands(Avail), Avail = AvailCmds.
%	(   subset(Avail,Cmds)
%	->  AvailCmds = Avail
%	;   AvailCmds = Cmds
%	).

level_commands(basic,[basic]).
level_commands(advanced,[basic,advanced]).
level_commands(developer,Dcmds) :- defined_commands(Dcmds).

set_avail_commands(Cs) :- is_list(Cs), defined_commands(Defined),
	subset(Cs,Defined), !,
	retractall(avail_commands(_)), assert(avail_commands(Cs)).
%set_avail_commands(_).

add_commands([]) :- !.
add_commands([C|Cs]) :- !, add_commands(C), add_commands(Cs).
add_commands(ToBeAdded) :- atom(ToBeAdded),
	defined_commands(Defined), memberchk(ToBeAdded, Defined), !,
	avail_commands(Av),
	(   \+ memberchk(ToBeAdded,Av)
	->  set_avail_commands([ToBeAdded|Av])
	;   true
	).
add_commands(_).

rem_commands([]) :- !.
rem_commands([C|Cs]) :- !, rem_commands(C), rem_commands(Cs).
rem_commands(ToBeRemoved) :- atom(ToBeRemoved),
	defined_commands(Defined), memberchk(ToBeRemoved, Defined), !,
	avail_commands(Av),
	(   memberchk(ToBeRemoved,Av)
	->  subtract(Av, [ToBeRemoved], NewAv),
	    set_avail_commands(NewAv)
	;   true
	).
rem_commands(_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command procedures
%   command procedures are support for 'do' entries for a command that
%   permit the 'do' entry to remain relatively short. Many commands
%   invoke detailed procedures in other modules. Command procedures lie
%   in between the 'do' entry for the implementation of the command and
%   the detailed core support from another appropriate module.
%
%   command procedures for miscellaneous commands, in some cases
%   procedures here may be temporary until they are moved to an
%   appropriate module
%

help_commands :-
	available_commands(CSs),
	findall(Sig, (syntax(Sig,CS), memberchk(CS,CSs)), Sigs),
	predsort(comp_name,Sigs,SSigs),
	writeln('<command> ::='),
	forall(member(Sig,SSigs), (write('  '), write(Sig), nl)), !.

comp_name('<',A,B) :- functor(A,Af,_), functor(B,Bf,_), Af @=< Bf.
comp_name('>',A,B) :- functor(A,Af,_), functor(B,Bf,_), Af @> Bf.

show_help(Cname) :-
	syntax(Sig,obsolete), functor(Sig,Cname,_),
	write('  '), write(Sig), writeln(' - OBSOLETE'),
	show_help_strings(Cname), !.
show_help(Cname) :-
	available_commands(CSs),
	findall(Sig, (syntax(Sig,CS), functor(Sig,Cname,_),memberchk(CS,CSs)), Sigs),
	forall(member(Sig,Sigs), (write('  '), writeln(Sig))),
	( Sigs \== [] -> show_help_strings(Cname) ; true).

%show_help(C) :-
%	C =.. [Command|_], % use only the command name, ignore args
%	(   help(Command,_)
%	->  nl, show_help_strings(Command),
%	    (	( syntax(S,obsolete), S =.. [Command|_] )
%	    ->	writeln('  OBSOLETE')
%	    ;	true
%	    )
%	;   format('No help for command "~q"~n', Command)
%	).

show_help_strings(Command) :-
	help(Command,HelpString), format('    ~a~n',HelpString), fail.
show_help_strings(_).

% inspection - for development and test
inspect(settings) :- !, do(set).
% add other inspect clauses here
%% - inspect(graph) :- !, graphmanager:getGraph(G),graphmanager:printGraph(G).
inspect(_) :- writeln('inspect: Unknown parameter name or illegal parameter value').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% demos
%   to show-off implemented portions of functionality
%   insert perform_demo clauses for specific arguments following comment

perform_demo(X) :- unimpl_d(X).

unimpl_d(X) :- format('Unimplemented demo command: ~q~n',[X]).
