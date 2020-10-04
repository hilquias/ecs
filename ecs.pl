:- module(ecs, []).

:- meta_predicate enqueue_rule(+, +, :, -).

new_rules(rules(P, Q)) :-
    empty_heap(P), empty_heap(Q).

step_rules(rules(P0, Q0), S0, T, S1, R1) :-
    step_rule(rules(Q0, P0), S0, T, S1, R1).

step_rule(R0, S0, T, S1, R1) :-
    dequeue_rule(R0, Pri, M:Rule, R_0),
    (call(M:Rule, R_0, S0, T, S_1, R_1) ->
	 enqueue_rule(R_1, Pri, M:Rule, R_2) ->
	     step_rule(R_2, S_1, T, S1, R1), !
    ; step_rule(R_0, S0, T, S1, R1)), !.
step_rule(R0, S0, _, S0, R0) :-
    \+dequeue_rule(R0, _, _, _), !.

enqueue_rule(rules(P, Q0), Pri, Rule, rules(P, Q1)) :-
    add_to_heap(Q0, Pri, Rule, Q1).

dequeue_rule(rules(P0, Q), Pri, Rule, rules(P1, Q)) :-
    get_from_heap(P0, Pri, Rule, P1).

%
% TESTS
%

:- begin_tests(ecs).

test(new_rules) :-
    new_rules(_).

test(step_rules) :-
    new_rules(R0),
    enqueue_rule(R0, 0, nop, R1),
    step_rules(R1, _, 0, _, _).

test(step_rules_1) :-
    new_rules(R0),
    enqueue_rule(R0, 0, clock_tic, R1),
    enqueue_rule(R1, 1, clock_tac, R2),
    step_rules(R2, [], 0, S1, R3),
    step_rules(R3, S1, 1, S2, _),
    assertion(S2 = [tac, tic, tac, tic]).

test(step_rules_2) :-
    new_rules(R0),
    enqueue_rule(R0, 0, clock_tic, R1),
    enqueue_rule(R1, 1, clock_tac_fail, R2),
    step_rules(R2, [], 0, S1, R3),
    step_rules(R3, S1, 1, S2, _),
    assertion(S2 = [tic, tic]).

test(step_rules_3) :-
    new_rules(R0),
    enqueue_rule(R0, 0, clock_tac_fail, R1),
    step_rules(R1, [], 0, S1, R2),
    step_rules(R2, S1, 1, S2, R3),
    assertion(S2 = []),
    assertion(new_rules(R3)).

test(enqueue_rule) :-
    new_rules(R0),
    enqueue_rule(R0, 0, nop, _).

test(dequeue_rule) :-
    new_rules(R0),
    \+dequeue_rule(R0, _, _, _).

nop(R, S, _, S, R).

clock_tic(R, S, _, [tic|S], R).

clock_tac(R, S, _, [tac|S], R).

clock_tac_fail(R, S, _, [tac|S], R) :- fail.

:- end_tests(ecs).
