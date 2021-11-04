%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RMV self test
%

rmv_startup_tests([tc01,tc02]).

rmv_regression_tests([]).

self_test :- % rmv:init,
	rmv_startup_tests(Tests),
	forall(member(T,Tests), self_test(T)).

self_test(TC) :- rmv:init, test:report_test(rmv:TC).

regression_test :-  % priv:init,
	rmv_startup_tests(Startup),
	rmv_regression_tests(Regression),
	append(Startup,Regression,AllTests),
	forall(member(T,AllTests), self_test(T)).

% top-level test - monitor creation
%

tc01 :- true.
tc02 :- fail.
