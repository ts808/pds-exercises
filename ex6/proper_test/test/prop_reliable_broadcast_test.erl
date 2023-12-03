-module(prop_reliable_broadcast_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behavior(link_layer_prop_tests).

-export([gen_api_call/0, execute_api_call/2, init/2, check/3, response_tag/0]).

prop_test() ->
    link_layer_prop_tests:check_property(?MODULE).

init(LL, ReplyTo) ->
    reliable_broadcast:start_link(LL, ReplyTo).

response_tag() -> deliver.

check(Broadcasts, Received, Crashed) ->
    conjunction([
        {validity, prop_be_broadcast_test:validity(Broadcasts, Received, Crashed)},
        {no_duplication, prop_be_broadcast_test:no_duplication(Broadcasts, Received)},
        {no_creation, prop_be_broadcast_test:no_creation(Broadcasts, Received)},
        {agreement, agreement(Received, Crashed)}
    ]).

gen_api_call() ->
    {broadcast, gen_message()}.

gen_message() ->
    oneof([msgX, msgY, msgZ]).

execute_api_call(RB, {broadcast, Msg}) ->
    reliable_broadcast:broadcast(RB, Msg).

% Agreement: If a message is delivered by some correct process, it must eventually be delivered by all
agreement(Received, Crashed) ->
  % TODO
  false.

