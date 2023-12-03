-module(ex2_tests).

-include_lib("eunit/include/eunit.hrl").


maximum_test() ->
	?assertEqual(7, warmup:maximum(3, 7)),
	?assertEqual(5, warmup:maximum(5, 4)).

list_max_test() ->
	?assertEqual(7, warmup:list_max([4, 7, 5, 2])).


sorted_test() ->
	?assertEqual(false, warmup:sorted([4, 7, 5, 2])),
	?assertEqual(true, warmup:sorted([2, 4, 5, 7])).

swap_test() ->
	?assertEqual({100, ok}, warmup:swap({ok, 100})).

find_test() ->
	?assertEqual({ok, 3}, warmup:find(d, [{c, 5}, {z, 7}, {d, 3}, {a, 1}])),
	?assertEqual(error, warmup:find(x, [{c, 5}, {z, 7}, {d, 3}, {a, 1}])).

find_all_test() ->
	?assertEqual([{d, 3}, {c, 5}], warmup:find_all([d, x, c], [{c, 5}, {z, 7}, {d, 3}, {a, 1}])).

positive_test() ->
	?assertEqual([6, 3, 0], warmup:positive([6, -5, 3, 0, -2])).

all_positive_test() ->
	?assertEqual(true, warmup:all_positive([1, 2, 3])),
	?assertEqual(false, warmup:all_positive([1, -2, 3])).

values_test() ->
	?assertEqual([5, 7, 3, 1], warmup:values([{c, 5}, {z, 7}, {d, 3}, {a, 1}])).

minimum_test() ->
	?assertEqual(2, warmup:list_min([7, 2, 9])).

