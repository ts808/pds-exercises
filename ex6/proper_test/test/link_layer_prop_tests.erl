-module(link_layer_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([check_property/1]).

-callback gen_api_call() -> proper_types:raw_type().
-callback response_tag() -> atom().
-callback execute_api_call(Node :: pid(), Call :: pid()) -> any().
-callback init(LL :: pid(), ReplyTo :: pid()) -> {ok, pid()}.
-callback check(Broadcasts :: [{Node :: any(), Msg :: any()}], Received :: [{Node :: any(), [Msg :: any()]}], Crashed :: sets:set(Node :: any())) -> proper:test().



check_property(TestModule) ->
    OpGen = TestModule:gen_api_call(),
    ?FORALL(Ops, operations(OpGen), check_system(Ops, TestModule)).

test_nodes() -> [nodeA, nodeB, nodeC].

check_system(Ops, TestModule) ->
    process_flag(trap_exit, true),

    % initialize dummy link layer:
    {ok, LL, Nodes} = link_layer_dummy:start_link(test_nodes()),

    % create eager broadcast nodes:
    NodesWithMailbox = lists:map(fun(Node) ->
        M  = mailbox:new(),
        {ok, BC} = TestModule:init(Node, M),
        {BC, M}
      end, Nodes),

    {TestNodes, MailBoxes} = lists:unzip(NodesWithMailbox),

    ?assertEqual(length(test_nodes()), length(TestNodes)),
    ?assertEqual(length(test_nodes()), length(MailBoxes)),

    execute_ops(Ops, TestNodes, TestModule, LL),
    % finally, deliver all pending messages (to check "eventually")
    link_layer_dummy:finish(LL),

    Crashed = sets:from_list([N || {crash, N} <- Ops]),

    ReceivedRaw = [{N, mailbox:read(M)} || {N,M} <- lists:zip(test_nodes(),MailBoxes)],

    lists:foreach(fun
          ({_, Msgs}) ->
            lists:foreach(fun
                  ({Tag, _})->
                    ?assertEqual(TestModule:response_tag(), Tag);
                  (Msg) ->
                    error({invalid_message, Msg})
              end, Msgs);
          (Msg) ->  error({invalid_message2, Msg})
        end, ReceivedRaw),

    Received = [{N, [M || {_Tag, M} <- Msgs]} || {N,Msgs} <- ReceivedRaw],

    Broadcasts = [{Node, Msg} || {api_call, Node, {broadcast, Msg}} <- Ops],

    Error = receive
            {'EXIT',_,normal} -> ok;
            {'EXIT',FromPid,Reason} -> {error, {FromPid, Reason}}
            after 0 -> ok
        end,
    ?assertEqual(ok, Error),

    ?WHENFAIL(
      begin
        io:format("Executed operations:~n"),
        [io:format("  ~p~n",[Op]) || Op <- Ops],
        io:format("Crashed processes:~p~n", [sets:to_list(Crashed)]),
        io:format("Received messages:~n"),
        [io:format("  ~p received ~p~n", [N, Msgs]) || {N, Msgs} <- Received]
      end, TestModule:check(Broadcasts, Received, Crashed)).

execute_ops(Ops, TestNodes, TestModule, LL) ->
    [execute_op(Op, TestNodes, TestModule, LL) || Op <- Ops].

execute_op({api_call, Node, Call}, TestNodes, TestModule, _LL) ->
    B = get_node(Node, TestNodes),
    TestModule:execute_api_call(B, Call);
% all other calls are forwarded to the link layer:
execute_op(LinkLayerOp, _TestNodes, _, LL) ->
    gen_server:call(LL, LinkLayerOp).

get_node(Node, TestNodes) ->
    get_node_h(Node, test_nodes(), TestNodes).

get_node_h(N, [N|_], [R|_]) -> R;
get_node_h(N, [_|Ns], [_|Rs]) -> get_node_h(N, Ns, Rs);
get_node_h(N, L=[_|_], []) -> {error, {not_enough_nodes, N, L}};
get_node_h(N, _, _) -> {error, {could_not_find_node, N}}.




operations(ApiGen) ->
    list(operation(ApiGen)).

operation(ApiGen) ->
    oneof([
        {api_call, gen_node(), ApiGen},
        deliver_all,
        {deliver, gen_node(), gen_node(), pos_integer()},
        {crash, gen_node()}
    ]).


gen_node() ->
    oneof(test_nodes()).
