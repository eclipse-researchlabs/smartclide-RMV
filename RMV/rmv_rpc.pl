:- module(rmv_rpc,[]).

%
% param:global_orb(CORBA_OBJECT_NIL) (settable)
%
%
%
% Monitor_MonitorService_heartbeat(service, index, state, ev, result)
%
% Monitor_MonitorService_reset(service, index, hard_p, ev)
%
% client_shutdown(Sig)
%   param:global_orb(GO)
%   GO \== CORBA_OBJECT_NIL
%   -> CORBA_ORB_shutdown(GO, false, -Local_ev)
%      etk_abort_if_exception(Local_ev,'caught exception')
%   ; true
%
%
% client_init(Argc, Argv, Orb, Ev, Service, Orb)
%   Name_service = CORBA_OBJECT_NIL
%   ID = ["NuRV","Monitor","Service"]
%   CORBA_exception_init(Ev)
%   % init signal handling (SIGINT, SIGTERM)->client_shutdown
%   CORBA_ORB_init(+Argc,+Argv,orbit_local_orb,+Ev,-Orb)
%   param:setparam(global_orb,Orb)
%   etk_get_name_service(+Orb,+Ev,-Name_service)
%
% client_cleanup(Global_orb, Service, Ev)
%
% etk_abort_if_exception(Ev, Message)
%
%   Ev - a CORBA_Environment (handle)
%
