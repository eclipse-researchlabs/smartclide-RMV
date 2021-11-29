:- module(apiresp,[std_resp_prefix/0,
		    std_resp_MS/3,
		    std_resp_BS/3,
		    std_resp_M/3,
		    api_unimpl/1,root_apis/2,
		    json_resp/3,json_resp/5]).

:- use_module(param).
:- use_module(library(http/json)).

std_resp_prefix :-
	(   param:jsonresp(on)
	->  format('Content-type: application/json~n~n')
	;   format('Content-type: text/plain~n~n')
	).

std_resp_MS(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(M), writeln(Status)
	).

std_resp_BS(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(B), writeln(Status)
	).

std_resp_M(Status, M, B) :-
	(   param:jsonresp(on)
	->  json_resp(Status, M, B)
	;   writeln(M)
	).

%
%
%

api_unimpl(_) :-
	std_resp_prefix,
	format('Unimplemented API~n').

root_apis(Kind,_) :- std_resp_prefix, list_apis(Kind), !.
root_apis(_,_).

list_apis(Kind) :-
	format('Valid ~a paths:~n',[Kind]),
	G=..[Kind,APIs], call(G),
	foreach( member(A,APIs), writeln(A)).

% JSON response structure
% {
%     "respStatus" : "statusType",
%     "respMessage" : "statusDesc",
%     "respBody" : "statusBody"
% }

% statusType: success or failure of any call
statusType([failure,success]).
% statusMessage
statusDesc([missing_parameter,
	    authentication_error,
	    unknown_policy,
	    policy_loaded,
	    error_combining_policies
	   ]).
% statusBody
responseBody([null_status,
	    policy_id,
	    policy_spec,
	    grant,
	    deny
	   ]).

% json_resp(RespStatus,RespMessage,RespBody,JrespTerm,JrespAtom)
%
% assignments to the JSON response structure for each API are given in
% the documentation

% imperative
json_resp(RespStatus,RespMessage,RespBody) :-
	  atomify(RespMessage,MessageAtom,RespBody,BodyAtom),
	  json_resp(RespStatus,MessageAtom,BodyAtom,_RespTerm,RespAtom),
	  writeln(RespAtom),
	  true.

% relation
json_resp(RespStatus,RespMessage,RespBody,JrespTerm,JrespAtom) :-
	  JrespTerm =
	  json([respStatus=RespStatus,respMessage=RespMessage,respBody=RespBody]),
	  atom_json_term(JrespAtom,JrespTerm,[as(atom)]),
	  true.

response(RespStatus,RespMessage,RespBody) :-
        (   param:jsonresp(on)
	->  json_resp(RespStatus, RespMessage, RespBody)
	;   writeln(RespMessage), writeln(RespStatus),
	    (	RespBody \== ''
	    ->	writeln(RespBody)
	    ;	true
	    )
	).

atomify(M,MA,B,BA) :- atomify(M,MA), atomify(B,BA).

atomify(X,XA) :- compound(X), !,
	term_to_atom(X,XA).
atomify(X,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTING
%
jterm( json([name='Bob']) ).
jatom( '{"name":"Bob"}' ).
go :- jatom(JA), jterm(JT),
	format('JSON atom: ~q~n',[JA]),
	format('JSON term: ~q~n',[JT]),
	atom_json_term(JA,T,[]), format('~q~n',[T]),
	atom_json_term(A,JT,[as(atom)]), format('~q~n',[A]),
	true.
go1 :-
	example(S,M,B),
	json_resp(S,M,B,T,A),
	format('term=~q~natom=~n',[T,A]),
	write(A),
	fail.
go1.
go2 :-
	api1(success),
	api1(failure).

% examples for testing: example(Status,Message,Body).
example(success,policy_loaded,policy1). % load, loadi
example(failure,file_error,file1).      % load
example(success,policy_set,policy1).    % setpol

% TEST CASES
api1(SF) :- SF == success, !,
	(   param:jsonresp(on)
	->  json_resp(SF,api1msgsuccess,api1successbody,_,A), writeln(A)
	;   writeln(SF)
	).
api1(SF) :- SF == failure, !,
	(   param:jsonresp(on)
	->  json_resp(SF,api1msgfailure,api1failurebody,_,A), writeln(A)
	;   writeln(SF)
	).


