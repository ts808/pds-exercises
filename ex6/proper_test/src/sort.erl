-module(sort).

-export([shell_sort/1, swap/3, sort/3, backwards_swap/3]).

-spec shell_sort(L) -> L when L :: [term()].
shell_sort(List) -> sort(List, length(List) div 2, 1).

sort(List, 0, _) ->
    List;
sort(List, Interval, Pos) ->
    case Pos == length(List) - Interval + 1 of
        false ->
            case lists:nth(Pos, List) > lists:nth(Pos + Interval, List) of
                true ->
                    L1 = swap(List, Pos, Pos + Interval),
                    L2 = backwards_swap(L1, Interval, Pos),
                    sort(L2, Interval, Pos + 1);
                false ->
                    sort(List, Interval, Pos + 1)
            end;
        true ->
            sort(List, Interval div 2, 1)
    end.

backwards_swap(List, Interval, Pos) ->
    case Pos - Interval > 0 of
        true ->
            case lists:nth(Pos, List) < lists:nth(Pos - Interval, List) of
                true ->
                    Swapped = swap(List, Pos - Interval, Pos),
                    backwards_swap(Swapped, Interval, Pos - 1);
                false -> List
            end;
        false -> List
    end.

swap([], _, _) ->
    [];
swap(List, Index1, Index2) ->
    lists:sublist(List, Index1 - 1) ++ [lists:nth(Index2, List)] ++
        lists:sublist(List, Index1 + 1, Index2 - Index1 - 1) ++ [lists:nth(Index1, List)] ++
        lists:sublist(List, Index2 + 1, length(List) - Index2).
