% RMV - Monitor Creation - Create Property Specification
% Work in Progress

:- module(rmv_mc_cps,[create_ltl_properties/2
	       ]).

:- use_module(['COM/param',rmv_ml]).

% in rmv_mc_CM: service_spec2smv(ServiceSpec, SMVmodel).

% in rmv_mc: service_spec2nurv(ServiceSpec, NuRVscript).


create_ltl_properties(SS,Properties) :-
    is_service_spec(SS, SSpecId, _SSpecBody),
    % magic with SSpecBody goes here

    atom_concat(ssid,N,SSpecId), atom_concat(ltlid,N,LTLid),
    PropAtoms = [p:p, q:q],
    PropVars = [p, q],
    PropFormulas = [property(LTLid, 'p U q')],

    Properties = properties( PropVars, PropAtoms, PropFormulas ),

    param:monitor_directory_name(MD),
    atomic_list_concat([MD,'/',LTLid,'.smv'],SMVltlFile),
    open(SMVltlFile,write,Str,[create([default])]),
    write(Str,Properties),
    close(Str).


