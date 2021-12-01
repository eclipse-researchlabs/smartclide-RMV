:- begin_tests(rmv_mf_mep, [setup(rmv_mf_mep:test_setup),cleanup(true)]).
% relies on test configuration vector test_cv defined in module rmv_ml
%
test(evaluator1) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(eq(x,1),V).

test(evaluator2) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(not(v),V).
test(evaluator3) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(w,V).
test(evaluator4) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(eq(x,1),V).
test(evaluator5) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(eq(w,true),V).
test(evaluator6) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(neq(x,y),V).
test(evaluator7) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(gt(z,x),V).
test(evaluator8) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(geq(z,y),V).
test(evaluator9) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(geq(y,y),V).
test(evaluator10) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(leq(y,y),V).
test(evaluator11) :- rmv_mf_mep:test_vars(V), !, rmv_mf_mep:af_evaluator(leq(x,y),V).
test(atomlist1) :-  rmv_mf_mep:test_vars(V), !, As=[a1:eq(x,2),a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],
    rmv_mf_mep:aT_list_constructor(As,V,[a2,a4]).
test(atomlist2) :- rmv_mf_mep:test_vars(V), !, As=[a1:w,a2:lt(x,2),a3:lt(y,x),a4:leq(x,2)],
    rmv_mf_mep:aT_list_constructor(As,V,[a1,a2,a4]).

:- end_tests(rmv_mf_mep).
