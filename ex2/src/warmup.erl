-module(warmup).
-export([
    maximum/2,
    list_max/1,
    sorted/1,
    swap/1,
    find/2,
    find_all/2,
    positive/1,
    all_positive/1,
    values/1,
    list_min/1,
    fun1/2,
	fun2/2,
    fun3/2
]).

%% Numbers, Lists and Tuples
% a)
-spec maximum(number(), number()) -> number().
maximum(A, B) ->
    if
        A >= B -> A;
        true -> B
    end.

% b)
-spec list_max(list(number())) -> number().
list_max([]) -> [];
list_max([Head | Tail]) -> list_max([Head | Tail], Head).
list_max([Head | Tail], Max) ->
    if
        Head >= Max -> list_max(Tail, Head);
        true -> list_max(Tail, Max)
    end;
list_max([], Max) ->
    Max.

% c)
-spec sorted(list(number())) -> boolean().
sorted([]) -> true;
sorted([Head | Tail]) -> sorted(Tail, Head).
sorted([Head | Tail], Prev) ->
    if
        Prev > Head -> false;
        true -> sorted(Tail, Head)
    end;
sorted([], _) ->
    true.

% d)
-spec swap({any(), any()}) -> {any(), any()}.
swap({A, B}) -> {B, A}.

% e)
-spec find(any(), list({any(), any()})) -> {ok, any()} | error.
find(_, []) ->
    error;
find(Target, [{Key, Value} | Tail]) ->
    if
        Target =:= Key -> {ok, Value};
        true -> find(Target, Tail)
    end.

% f)
-spec find_all(list(any()), list({any(), any()})) -> list({any(), any()}).
find_all([], _) ->
    [];
find_all([TargetHead | TargetTail], List) ->
    case find(TargetHead, List) of
        {ok, Value} -> [{TargetHead, Value}] ++ find_all(TargetTail, List);
        error -> find_all(TargetTail, List)
    end.

%% Higher Order Functions
% g)
-spec positive(list(number())) -> list(number()).
positive(List) -> lists:filter(fun(X) -> X >= 0 end, List).

% h)
-spec all_positive(list(number())) -> boolean().
all_positive(List) -> lists:all(fun(X) -> X >= 0 end, List).

% i)
-spec values(list({any(), any()})) -> list(any()).
values([]) -> [];
values(List) -> lists:map(fun({_, Value}) -> Value end, List).

% j)
-spec list_min(list(number())) -> number().
list_min([Head | Tail]) -> lists:foldl(fun min/2, Head, Tail).

%% Typing Erlang
% a)
-spec fun1(boolean(), boolean()) -> number().
fun1(X, Y) ->
    if
        X =:= Y -> 1;
        true -> 0
    end.

% i couldn't come up with anything meaningful here
-spec fun2(list({number, number()}), any()) -> {{notmatched, number()}, {matched, number()}}.   
fun2(_, _) -> {{notmatched, 404}, {matched, 42}}.

-spec fun3(list(any()), atom()) -> list({atom(), any()}).
fun3([], _) -> [];
fun3([Head | Tail], Atom) -> [{Atom, Head} | fun3(Tail, Atom)].