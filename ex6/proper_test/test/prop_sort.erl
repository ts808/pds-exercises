-module(prop_sort).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([prop_trivial/0, prop_ordered/0, prop_idempotent/0, prop_sort_equiv/0, ordered/1, prop_ordered_negative/0]).




% TODO
ordered([]) -> true;
ordered([_]) -> true;
ordered([A,B|T]) -> A =< B andalso ordered([B|T]).

% trivial property, a generated list of integers should be identical to itself
prop_trivial() ->
  ?FORALL(X, list(integer()), X =:= X).

prop_ordered() ->
  % TODO
   ?FORALL(L, list(integer()), ordered(sort:shell_sort(L))).

prop_idempotent() ->
  % TODO
  ?FORALL(L, list(integer()), sort:shell_sort(L) =:= sort:shell_sort(sort:shell_sort(L))).

prop_sort_equiv() ->
  % TODO
  ?FORALL(L, list(integer()), sort:shell_sort(L) =:= sort:shell_sort(sort:shell_sort(L))).


% TODO custom property
prop_ordered_negative() ->
  ?FORALL(L, list(integer()), ordered(sort:shell_sort(list:map(fun(x) -> -x end, L)))).

