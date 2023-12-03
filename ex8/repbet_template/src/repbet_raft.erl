-module(repbet_raft).

-export([start/0]).

start() ->
    {ok, spawn_link(fun() ->
        NodeId = {node, node()},
        NodesEnv = os:getenv("R_NODES", ""),
        Nodes = string:tokens(NodesEnv, ","),
        NodeIds = [{node, erlang:list_to_atom(N)} || N <- Nodes],
        Config = #{id => NodeId,
                   uid => erlang:term_to_binary(NodeId),
                   cluster_id => my_cluster,
                   log_module => ra_log_memory,
                   log_init_args => #{},
                   initial_nodes => NodeIds,
                   machine => {module, repbet_machine, #{}}},
        error_logger:info_msg("Config = ~p~n", [Config]),
        ok = ra:start_node(Config),
        ok = ra:trigger_election({node, node()})
    end)}.