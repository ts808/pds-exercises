-module(vectorclock_server_simple).

-export([start_link/0]).

start_link() ->
    {ok, spawn_link(fun Loop() ->
            receive
                ping -> io:format("pong!~n");
                {From, ping2} -> From ! "pong!";
                {From, new} -> From ! vectorclock:new();
                {From, get, VC, P} -> From ! vectorclock:get(VC, P);
                {From, increment, VC, P} -> From ! vectorclock:increment(VC, P);
                {From, leq, VC1, VC2} -> From ! vectorclock:leq(VC1, VC2);
                {From, merge, VC1, VC2} -> From ! vectorclock:merge(VC1, VC2)
            end,
            Loop()
        end)}.
