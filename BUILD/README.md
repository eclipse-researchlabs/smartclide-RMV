# build tog-rtd/RMV software executables
Execute the RMV  build from the project top directory (e.g. BUILD/mkrmv, don't use mkngac).
Execute the resulting executable rmv or rmv-server from the same place so the executable
can find the RUNTIME directory and its subdirectories
    BUILD/rmv           for the interactive command-driven version
    BUILD/rmv-server    to run the server

With no command line arguments the rmv-server listens on the rmv_port defined in param.pl.
This can be changed with a command line option when the server is started.

By default the token needed to call protected APIs is that defined in param.pl (rmv_token).
Separate tokens can be defined for several interfacees, although rmv_token is used for
several RMV APIs.
In the following table the second column is the name of the parameter in param.pl.
For development and testing it is convenient to have well-known default token values.
Defaults for the different interfaces in RMV are in the third column between single quotes.
    RMV APIs                            rmv_token           'rmv_token'
    Monitor Event Processing (mepapi)   rmv_token           'rmv_token'
    Event Processing Point API          epp_token           'epp_token'
    Monitor Creation API (mcapi)        rmv_mc_token        'rmv_mc_token'
    Audit API (audit)                   audi_token          'audit_token'

The relevant definitions in param.pl are:
    audit_token('audit_token'). % default audit token
    epp_token('epp_token'). % default epp token
    rmv_token('rmv_token'). % default rmv token
    rmv_epp_token('rmv_epp_token'). % default rmv_epp token
    rmv_mc_token('rmv_mc_token'). % default rmv_mc token
    rmv_ln_token('rmv_ln_token'). % default rmv_ln token


The rmv-server command line options are described by the opt spec:

rmv_server_opt_spec([
    [opt(portnumber), meta('RP'), type(integer), shortflags([p]), longflags(['port',portnumber']),
         help( 'server listens for API calls on port RP' )],
    [opt(selftest), type(boolean), default(false), shortflags([s]), longflags(['selftest']),
         help( 'run self tests on startup' )],
    [opt(token), meta('TOKEN'), type(atom), shortflags([t]), longflags(['token']),
         help( 'requests must cite TOKEN' )],
    [opt(verbose), type(boolean), default(false), shortflags([v]), longflags(['verbose']),
         help( 'show all messages' )],
    [opt(jsonresp), type(boolean), default(true), shortflags([j]), longflags(['jsonresp']),
         help( 'JSON-encoded responses' )],
    [opt(epp), type(boolean), default(true), shortflags([e]), longflags(['epp']),
         help( 'enable Event Processing Point' )],
    [opt(context), meta('URL'), type(atom), shortflags([c]), longflags(['context']),
	 	 help( 'URL of Context system' )],
	[opt(nurvsim), type(boolean), default(false), shortflags([n]), longflags(['nurvsim']),
		  help( 'NuRV simulation' )],
	[opt(guitracer), type(boolean), default(false), shortflags([g]), longflags(['guitracer'],
		 help( 'enable GUI tracer' )]
]).

