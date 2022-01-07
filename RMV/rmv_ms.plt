:- begin_tests(rmv_ms, [setup(rmv_ms:initialize_ms_configuration),cleanup(rmv_ms:un_init)]).
% relies on the test configuration vector test_cv defined in module rmv_ms
test(initial1) :- rmv_ms:monitor_id('monid_00002').
test(initial2) :- rmv_ms:monitor_atoms([a1:eq(n,2),a2:lt(n,2),a3:eq(p,q)]).
%test(evaluator1) :- rmv_ms:af_evaluator(eq(s,1)).
test(evaluator2) :- rmv_ms:af_evaluator(not(q)).
test(evaluator3) :- rmv_ms:af_evaluator(p).
%test(evaluator5) :- rmv_ms:af_evaluator(eq(r,true)).
test(evaluator6) :- rmv_ms:af_evaluator(neq(n,o)).
%test(evaluator6) :- rmv_ms:af_evaluator(neq(r,s)).
test(evaluator7) :- rmv_ms:af_evaluator(gt(o,n)).
test(evaluator8) :- rmv_ms:af_evaluator(geq(o,n)).
%test(evaluator9) :- rmv_ms:af_evaluator(geq(s,1)).
%test(evaluator10) :- rmv_ms:af_evaluator(leq(s,s)).
%test(evaluator11) :- rmv_ms:af_evaluator(leq(s,o)).
test(atomlist1) :- rmv_ms:monitor_atoms(As), rmv_ms:aT_list_constructor(As,[a2]).
test(atomlist2) :- rmv_ms:aT_list_constructor([a1:p,a2:lt(n,2),a3:lt(o,n)],[a1,a2]).
test(orlist1) :- rmv_ms:or_list_constructor([o=2, s=1, q=false]).

:- end_tests(rmv_ms).
