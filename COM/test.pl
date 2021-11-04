% TESTS - FRAMEWORK FOR SELF-TEST AND REGRESSION TESTS
:- module(test, [regression_test/0, self_test/0, module_test/2]).

regression_test :- % external functionality tests
	param:regression_test_modules(R),
	forall(member(M,R), module_test(M,regression_test)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

self_test :- % internal self-tests
	% report_test(<module>:self_test),
	param:self_test_modules(S),
	(   S \== []
	->  writeln('Self Tests:')
	;   true
	),
	forall(member(M,S), module_test(M,self_test)),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_test(Module,TestCategory) :-
	call(Module:TestCategory),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report_test(G) :- % argument is a test goal
	write(G), write(' ... '),
	(   call(G)
	->  writeln('ok')
	;   writeln('failed')
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

regression_test(M) :-
	param:test_directory_name(D),
	atom_concat([M,'.pl'], '.', Mfilename),
	atomic_list_concat([D, '/', M, '_test.pl'], Tfilename),
	exists_file(Mfilename), exists_file(Tfilename),
	atomic_list_concat([M,':',M,'_regression_tests'],Rfunct),
	Q =.. [Rfunct,Rlist],
	call(Q),
	forall(member(R,Rlist), report_test(M:R)),
	true.

test_case_passed(M,T) :-
	format('~a:~a Passed~n',[M,T]).
