% RMV - Monitor Creation - Create Property Specification
% Work in Progress

:- module(rmv_mc_cps,[
	       ]).

:- use_module(['COM/param',rmv_ml]).

% in rmv_mc_CM: service_spec2smv(ServiceSpec, SMVmodel).

% in rmv_mc: service_spec2nurv(ServiceSpec, NuRVscript).


create_ltl_properties(SSpecId,SSpecBody,LTLprops) :- % TODO make proper magic
    service_spec(SSpecId, SSpecBody),
    atom_concat(ssid,N,SSpecId), atom_concat(ltlid,N,LTLid),
    % magic normally goes here
    param:monitor_directory_name(MD),
    atomic_list_concat([MD,'/',LTLid,'.smv'],_SMVltlFile),
    LTLprops = [property(prop001,'p U q')],
    true.

