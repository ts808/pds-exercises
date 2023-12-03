-module(vc_server_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

start() ->
    vc_server:start_link().

stop(_) ->
    vc_server:stop().

tick_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun (_SetupData) ->
         {inorder,
            [
                ?_assertEqual(0, tick_and_get(a, 0)),
                ?_assertEqual(1, tick_and_get(b, 1)),
                ?_assertEqual(5, tick_and_get(c, 5))]}
     end}.
tick_and_get(P, 0) ->
    vc_server:get(P);
tick_and_get(P, N) ->
    vc_server:tick(P),
    tick_and_get(P, N-1).

advance_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun (_SetupData) ->
         {inorder,
            [
                ?_assertEqual([{a, 5}, {b, 3}], advance_and_get([{a, 5}, {b, 3}])),
                ?_assertEqual([{a, 5}, {b, 3}, {c, 7}], advance_and_get([{c, 7}])),
                ?_assertEqual([{a, 5}, {b, 3}, {c, 7}], advance_and_get([{b, 1}])),
                ?_assertEqual([{a, 5}, {b, 10}, {c, 7}], advance_and_get([{b, 10}])),
                ?_assertEqual([{a, 12}, {b, 13}, {c, 14}], advance_and_get([{a, 12}, {b, 13}, {c,14}]))
            ]}
     end}.

advance_and_get(VC) ->
    vc_server:advance_to(VC),
    vc_server:get_vc().



