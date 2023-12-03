-module(prop_vc).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    prop_minimal/0, prop_monotonic_merge/0, prop_strict_increment/0, prop_concurrency/0, prop_conv/0
]).

% limit processes to 10
proc() ->
    range(1, 10).

random() ->
    rand:uniform(10).

generate_vc(T) ->
    ?LET(L, list(T), increment_from_list(L, vectorclock:new())).

increment_from_list([], VC) -> VC;
increment_from_list([H | T], VC) -> increment_from_list(T, vectorclock:increment(VC, H)).

prop_minimal() ->
    % TODO
    ?FORALL(VC, generate_vc(proc()), vectorclock:leq(vectorclock:new(), VC)).

prop_monotonic_merge() ->
    % TODO
    ?FORALL(
        {VC1, VC2},
        {generate_vc(integer()), generate_vc(integer())},
        begin
            Merged = vectorclock:merge(VC1, VC2),
            vectorclock:leq(VC1, Merged),
            vectorclock:leq(VC2, Merged)
        end
    ).

prop_strict_increment() ->
    % TODO
    ?FORALL(VC, generate_vc(proc()), vectorclock:leq(VC, vectorclock:increment(VC, random()))).

% Q: Which of those two properties hold for vectorclocks?
% A: If v1 =:= v2 then v1 <= v2 and v2 <= 1,
% because =:= denotes that v1 and v2 are actually equal,
% i.e. have the same number of processes with same values
prop_concurrency() ->
    % TODO
    ?FORALL(
        VC1,
        generate_vc(proc()),
        ?EXISTS(VC2, generate_vc(proc()), begin
            not vectorclock:leq(VC1, VC2),
            not vectorclock:leq(VC2, VC1)
        end)
    ).

prop_conv() ->
    ?FORALL(VC, generate_vc(proc()), vectorclock:from_list(vectorclock:to_list(VC)) =:= VC).
