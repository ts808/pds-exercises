-module(services_chatty_test).
-include_lib("eunit/include/eunit.hrl").

% Creates a vectorclock from a list of key-value pairs

c() -> global:whereis_name(chatty).

chatty_test_() ->
    {setup,
     % sets up the server once for all test cases
     fun setup_chatty/0,
     fun cleanup_chatty/1,
     [
      fun no_clients/0,
      fun one_message/0,
      fun two_messages/0
     ]
    }.

setup_chatty() ->
	% starting chatty with global name `chatty`
	services_chatty:start_link(),
	ok.

cleanup_chatty(_) ->
	% stopping chatty
	P = c(),
	gen_server:stop(P).

no_clients() ->
	gen_server:call(c(), {message, "hello world"}),
	receive _ -> ?assert(false) after 100 -> ok end.

one_message() ->
	gen_server:call(c(), {action, join}),
	gen_server:call(c(), {message, "hello world"}),
	gen_server:call(c(), {action, leave}),
	receive _ -> ok after 100 -> ?assert(false) end,
	receive _ -> ?assert(false) after 100 -> ok end.

two_messages() ->
	gen_server:call(c(), {action, join}),
	gen_server:call(c(), {message, "hello world"}),
	gen_server:call(c(), {message, "2"}),
	gen_server:call(c(), {action, leave}),
	receive _ -> ok after 1000 -> ?assert(false) end,
	receive _ -> ok after 1000 -> ?assert(false) end,
	receive _ -> ?assert(false) after 100 -> ok end.

three_messages() ->
	gen_server:call(c(), {action, join}),
	gen_server:call(c(), {message, "hello world"}),
	gen_server:call(c(), {message, "2"}),
	gen_server:call(c(), {message, "4"}),
	gen_server:call(c(), {action, leave}),
	receive _ -> ok after 1000 -> ?assert(false) end,
	receive _ -> ok after 1000 -> ?assert(false) end,
	receive _ -> ok after 1000 -> ?assert(false) end,
	receive _ -> ?assert(false) after 100 -> ok end.
