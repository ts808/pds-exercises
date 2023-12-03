-module('prop_be_broadcast_test').

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behavior(link_layer_prop_tests).

-export([gen_api_call/0, execute_api_call/2, init/2, check/3, response_tag/0, validity/3, no_duplication/2, no_creation/2]).

prop_test() ->
    link_layer_prop_tests:check_property(?MODULE).

init(LL, ReplyTo) ->
    best_effort_broadcast:start_link(LL, ReplyTo).

response_tag() -> deliver.

check(Broadcasts, Received, Crashed) ->
    conjunction([
        {validity, validity(Broadcasts, Received, Crashed)},
        {no_duplication, no_duplication(Broadcasts, Received)},
        {no_creation, no_creation(Broadcasts, Received)}
    ]).

gen_api_call() ->
    {broadcast, gen_message()}.

gen_message() ->
    oneof([msgX, msgY, msgZ]).

execute_api_call(RB, {broadcast, Msg}) ->
    best_effort_broadcast:broadcast(RB, Msg).


% Validity: If a process broadcasts a message, it eventually delivers it
validity(Broadcasts, Received, Crashed) ->
  % TODO
  false.

% No duplication: No message is received more often than it was sent:
no_duplication(Broadcasts, Received) ->
  % TODO
  false.

% no creation: Every received message was sent before
no_creation(Broadcasts, Received) ->
  % TODO
  false.

