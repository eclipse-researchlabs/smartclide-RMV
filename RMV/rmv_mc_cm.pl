% RMV - Monitor Creation - Create Model
% Work in Progress

:- module(rmv_mc_cm,[create_smv_model/2
	       ]).

:- use_module(['COM/param',rmv_ml]).

% create_smv_model(+SSpec, -SMVmodel)
%
create_smv_model(SS, Model) :-
    is_service_spec(SS, SSpecId, SSpecBody),
    is_service_spec_body(SSpecBody,Sitems),

    ssid2modid(SSpecId,ModelId),

    magic(Sitems,SMVstruct),

    SMVstruct = smv( _SMV, SMVmodeltext, SMVordtext, _SMVvars),
    %SMVvars = vars(Vdecls,Vo,Vm,Vp,Vr,Vt),

    param:monitor_directory_name(MD),
    atomic_list_concat([MD,'/',ModelId,'.smv'],SMVmodelFile),
    model2smv_file(SMVmodeltext,SMVmodelFile),

    atomic_list_concat([MD,'/',ModelId,'.ord'],SMVordFile),
    ord2ord_file(SMVordtext,SMVordFile),

    cons_model(ModelId,SMVstruct,SMVmodelFile,SMVordFile, Model),
    true.

magic(_,SMVstruct) :- % TODO

    SMVstruct = smv(x, SMVtext, SMVord, SMVvars),
    %SMVvars = vars(Vdecls,Vo,Vm,Vp,Vr,Vt),
    SMVvars = vars([p:boolean,q:boolean],[p,q],[p,q],[p,q],[p,q],[q]),
    SMVtext = 'MODULE main\nVAR\n\tp : boolean;\n\tq : boolean;\nINVAR\n\tp != q\nLTLSPEC\n\tp U q\n',
    SMVord = 'p\nq\n',
    true.

model2smv_file(Modeltext,SMVfile) :-
    %model2text(M,Text),
    open(SMVfile,write,Str,[create([default])]),
    write(Str,Modeltext),
    close(Str).

% model2text(+M, -Txt) % TODO
model2text(_,
	'MODULE main\nVAR\n\tp : boolean;\n\tq : boolean;\nINVAR\n\tp != q\nLTLSPEC\n\tp U q\n').

ord2ord_file(Ord,OrdFile) :- % TODO
    open(OrdFile,write,Str,[create([default])]),
    write(Str,Ord),
    close(Str).

cv(2, ms_cv(
             /* monitor_id */         'monid_00002',
             /* shared_var_decl */    [n:integer,
                                       o:integer,
                                       p:boolean,
                                       q:boolean,
                                       r:float,
                                       s:float],
             /* observable_vars */    [n, o, p, q, r, s],
             /* model_vars */         [n, p, q, s],
             /* property_vars */      [n, p, q],
             /* reportable_vars */    [n, o, s, p, q],
             /* trigger_vars */       [q, s],
             /* monitor_atoms */      [p:p,a1:eq(n,2),a2:lt(n,2),a3:eq(p,q),q:q],
             /* monitor_atom_eval */  ms_eval,
             /* shared_var_init */    [n=1,
                                       o=2,
                                       p=true,
                                       q=false,
                                       r=undefined,
                                       s=1
                                      ],
             /* behavior */           [],
             /* timer */              0,
             /* rmvhost */            '127.0.0.1',
             /* rmvport */            8005
            )
        ).

