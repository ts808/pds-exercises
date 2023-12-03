-module(vectorclock).

-export([new/0, increment/2, get/2, leq/2, merge/2, from_list/1]).

new() ->
    dict:new().

increment(VC, P) ->
    case dict:find(P, VC) of
        {ok, Value} -> dict:store(P, Value + 1, VC);
        error -> dict:store(P, 1, VC)
    end.

get(VC, P) ->
    case dict:find(P, VC) of
        {ok, Value} -> Value;
        error -> 0
    end.


leq(VC1, VC2) ->
    dict:fold(
        fun(K1, V1, Acc) ->
            case get(VC2, K1) of
                V2 when V2 >= V1 ->
                    Acc and true;
                V2 when V2 < V1 ->
                    Acc and false;
                0 ->
                    Acc
            end
        end,
        true,
        VC1
    ).

merge(VC1, VC2) ->
    dict:merge(fun(_, V1, V2) -> max(V1, V2) end, VC1, VC2).

from_list(L) ->
    dict:from_list(L).
