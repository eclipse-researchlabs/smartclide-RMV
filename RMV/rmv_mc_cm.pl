% RMV - Monitor Creation - Create Model
% Work in Progress

:- module(rmv_mc_cm,[
	       ]).

:- use_module(['COM/param',rmv_ml]).

create_smv_model(SSpecId, SSpecBody, SMVmodel) :-
    % service_spec(SSpecId, SSpecBody),
    ssid2modid(SSpecId,ModelId),

    % magic goes here
    magic(SSpecBody,Model),

    param:monitor_directory_name(MD),
    atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
    is_model(SMVmodel,ModelId),
    model2smv_file(Model,SMVmodelFile),
    atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
    % temporary - must get the variables from the model
    ord2ord_file('p\nq\n',SMVordFile),
    true.

magic(_SS,_M).

model2smv_file(M,SMVfile) :-
    model2text(M,Text),
    open(SMVfile,write,Str,[create([default])]),
    write(Str,Text),
    close(Str).

model2text(_M,Txt) :-
    Txt = 'MODULE main\nVAR\n\tp : boolean;\n\tq : boolean;\nINVAR\n\tp != q\nLTLSPEC\n\tp U q\n'.

ord2ord_file(Ord,OrdFile) :-
    open(OrdFile,write,Str,[create([default])]),
    write(Str,Ord),
    close(Str).

