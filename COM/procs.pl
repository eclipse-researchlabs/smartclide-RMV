% stored "built-in" procedures

:- module(procs, [proc/2, pmproc/2]).

:- dynamic([proc/2, pmproc/2]).

%%	NGAC Command Procs
% sequences of commands defined in command module

proc(guiserver, [
         set(guiserver,on),
         guitracer,
         set(jsonresp_server,on),
         set(jsonresp,on),
         set(no_sleep,on),
         traceone,
         server(8001),
         echo(ready)
     ]).

proc(review, [ %
         proc(queryA),
         proc(queryB),
         proc(autoCombined),
	 selftest,
         server(8001)
     ]).

proc(queryA, [ % query the example Policy (a)
	 newpol('Policy (a)'),
         policy_spec,
         echo('access queries, expect G G G D D D D D G D G G G G D D'),
	 access('Policy (a)',(u1,r,o1)),
	 access('Policy (a)',(u1,w,o1)),
	 access('Policy (a)',(u1,r,o2)),
	 access('Policy (a)',(u1,w,o2)),
	 access('Policy (a)',(u1,r,o3)),
	 access('Policy (a)',(u1,w,o3)),
	 access('Policy (a)',(u1,r,o4)),
	 access('Policy (a)',(u1,w,o4)),
	 access('Policy (a)',(u2,r,o1)),
	 access('Policy (a)',(u2,w,o1)),
	 access('Policy (a)',(u2,r,o2)),
	 access('Policy (a)',(u2,w,o2)),
	 access('Policy (a)',(u2,r,o3)),
	 access('Policy (a)',(u2,w,o3)),
	 access('Policy (a)',(u2,r,o4)),
	 access('Policy (a)',(u2,w,o4)),
         echo('Displaying policy graph'),
         policy_graph
     ]).

proc(queryB, [ % query the example Policy (b)
	 newpol('Policy (b)'),
         policy_spec,
         echo('access queries, expect D D G G D D D D D D G G G G G G'),
	 access('Policy (b)',(u1,r,o1)),
	 access('Policy (b)',(u1,w,o1)),
	 access('Policy (b)',(u1,r,o2)),
	 access('Policy (b)',(u1,w,o2)),
	 access('Policy (b)',(u1,r,o3)),
	 access('Policy (b)',(u1,w,o3)),
	 access('Policy (b)',(u1,r,o4)),
	 access('Policy (b)',(u1,w,o4)),
	 access('Policy (b)',(u2,r,o1)),
	 access('Policy (b)',(u2,w,o1)),
	 access('Policy (b)',(u2,r,o2)),
	 access('Policy (b)',(u2,w,o2)),
	 access('Policy (b)',(u2,r,o3)),
	 access('Policy (b)',(u2,w,o3)),
	 access('Policy (b)',(u2,r,o4)),
	 access('Policy (b)',(u2,w,o4)),
         echo('Displaying policy graph'),
         policy_graph
     ]).

proc(autoCombined, [
	 import(policy('EXAMPLES/policy_signals_access.pl')),
         echo('access queries, expect G G D D D D D D'),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals')),
	 import(policy('EXAMPLES/policy_vehicle_ownership.pl')),
         echo('access queries, expect D G D G D D D D'),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals')),
	 combine('Signals Access Policy','Vehicle Ownership Policy','Combined Policy'),
	 newpol('Combined Policy'),
         echo('Using Combined Policy'),
         echo('access queries, expect D G G D D D D D D'),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Trip Signals')),
         policy_graph
     ]).

proc(modelTestA, [
	 newpol('Policy (a)'),
	 los('Policy (a)'),
	 dps('Policy (a)'),
	 aoa(u1),
	 minaoa(u1),
	 userlos('Policy (a)',u1),
	 aoa(u2),
	 minaoa(u2),
	 userlos('Policy (a)',u2)
     ]).

proc(modelTestB, [
	 newpol('Policy (b)'),
	 los('Policy (b)'),
	 dps('Policy (b)'),
	 aoa(u1),
	 minaoa(u1),
	 userlos('Policy (b)',u1),
	 aoa(u2),
	 minaoa(u2),
	 userlos('Policy (b)',u2)
     ]).

proc(demo1, [ %
	 import(policy('EXAMPLES/policy3.pl')),
	 newpol('Policy3'),
	 access('Policy3',(jones,read,mrec1)),
	 access('Policy3',(jones,write,mrec1)),
	 access('Policy3',(smith,read,mrec1)),
	 access('Policy3',(smith,write,mrec1)),
	 access('Policy3',(smith,read,'Medical Records')) % OA not Object
     ]).

proc(demo2, [ % convert a declarative policy file to a PM command file
	 import(policy('EXAMPLES/Policy3.pl')),
	 newpol('Policy3'),
	 decl2imp('EXAMPLES/policy3.pl','EXAMPLES/policy3.pm')
     ]).

proc(demo3, [ %
	 combine('Policy (a)','Policy (b)','Policy (ab)'),
	 dps('Policy (ab)')
     ]).

proc(marketdemo, [ % demo the market policy
         setpol(mpolicy1),
         load_cond('EXAMPLES/market_cond.pl'),
         users('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4',r,
               [devid='95b40cf9-a9fc-4bd8-b695-99773b6f25e4', mchan=2,
                tstart='2020-09-08T08:00:00Z', tstop='2020-09-09T08:00:00Z', tsubmit='2020-09-09T08:03:22.350069Z',
                loMin=3.419216, loMax=3.519216, laMin=40.062069, laMax=40.072069]
              ),
         noop
     ]).

proc(test_echo, [
	 echo('hello world')
     ]).

proc(auto1, [
	 import(policy('EXAMPLES/policy_signals_access.pl')),
         echo('access queries, expect G G D D D D D D'),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

proc(auto2, [
	 import(policy('EXAMPLES/policy_vehicle_ownership.pl')),
         echo('access queries, expect D G D G D D D D'),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

proc(autocomb, [
	 combine('Signals Access Policy','Vehicle Ownership Policy','Combined Policy'),
	 newpol('Combined Policy'),
         echo('access queries, expect D G G D D D D D D'),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')), % <==
	 access('Combined Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

