:- begin_tests(rmv_ms, [setup(rmv_ms:initialize_ms_configuration),cleanup(rmv_ms:un_init)]).
% relies on the test configuration vector test_cv defined in module rmv_ms
test(initial1) :- rmv_ms:monitor_id('Mid_00001').
test(initial2) :- rmv_ms:monitor_atoms([a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)]).
test(evaluator1) :- rmv_ms:af_evaluator(eq(x,1)).
test(evaluator2) :- rmv_ms:af_evaluator(not(v)).
test(evaluator3) :- rmv_ms:af_evaluator(w).
test(evaluator4) :- rmv_ms:af_evaluator(eq(x,1)).
test(evaluator5) :- rmv_ms:af_evaluator(eq(w,true)).
test(evaluator6) :- rmv_ms:af_evaluator(neq(x,y)).
test(evaluator7) :- rmv_ms:af_evaluator(gt(z,x)).
test(evaluator8) :- rmv_ms:af_evaluator(geq(z,y)).
test(evaluator9) :- rmv_ms:af_evaluator(geq(y,y)).
test(evaluator10) :- rmv_ms:af_evaluator(leq(y,y)).
test(evaluator11) :- rmv_ms:af_evaluator(leq(x,y)).
test(atomlist1) :- rmv_ms:monitor_atoms(As), rmv_ms:aT_list_constructor(As,[a2,a4]).
test(atomlist2) :- rmv_ms:aT_list_constructor([a1:w,a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],[a1,a2,a4]).
test(orlist1) :- rmv_ms:or_list_constructor([v=false, w=true, x=1, y=2, z=3]).

:- end_tests(rmv_ms).
