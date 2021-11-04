:- module(erl, [er_package/2,load_er_package/2,load_er_package_immediate/2,unload_er_package/1]).

% Event Response Language (ERL)
% consider moving this to epp_era or epp to clean up dependencies

:-dynamic er_package/2.

init.

% ------------------------------------------------------------------------
% EVENT-RESPONSE LANGUAGE (ERL) INPUT
%

load_er_package(ERPfile,ERPname) :- var(ERPname), !,
        policyio:load_term(ERPfile,ERPterm),
        load_er_package_common(ERPterm,ERPname).

load_er_package_immediate(ERPatom,ERPname) :-
	read_term_from_atom(ERPatom,ERPterm,[]),
        load_er_package_common(ERPterm,ERPname).

load_er_package_common(ERPterm,ERPname) :-
        ERPterm = er_package(ERPname,ERrules),
        atom(ERPname), is_list(ERrules), !,
        purge_erp(ERPname),
        assertz(ERPterm),
        % param:setparam(current_erp,ERPname),
        true.

unload_er_package(ERPname) :- atom(ERPname), !,
	purge_erp(ERPname),
	(   param:current_erp(ERPname)
	->  param:setparam(current_erp,none)
	;   true
	).

% following is incomplete
unpack_erp(er_package(ERPname,ERrules)) :-
        purge_erp(ERPname),
        % assertz( erp(ERPname) ),
        unpack_erp_elements(ERPname,ERrules),
        perform_static_erp_checks(ERPname),
        true.

unpack_erp_elements(_,[]).
unpack_erp_elements(_ERPname,[_Rule|_Rules]) :-
        true.


perform_static_erp_checks(_).

% ------------------------------------------------------------------------

%clear_erp :- % clear the ERP database
%        purge_erp(_),
%        param:setparam(current_erp,none).

purge_erp(ERPname) :- atom(ERPname), % clear a specific one
        retractall(er_package(ERPname,_)),
        (   param:current_erp(ERPname)
        ->  param:setparam(current_erp,none)
        ;   true
        ).
purge_erp(ERPname) :- var(ERPname),
        retractall(er_package(ERPname,_)),
        param:setparam(current_erp,none).

% ------------------------------------------------------------------------


