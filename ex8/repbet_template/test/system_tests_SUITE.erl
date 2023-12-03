-module(system_tests_SUITE).
%% Run these test cases with `rebar3 ct`

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
  [
    {group, tests}
  ].

all_tests() ->
  [
    user_register1,
    user_register_twice,
    create_challenge
  ].

groups() ->
  [
    {tests, [], all_tests()}
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  application:stop(ra),
  Config.


restart_ra(DataDir) ->
  application:stop(ra),
  _ = application:load(ra),
  ok = application:set_env(ra, data_dir, DataDir),
  ok = application:set_env(ra, segment_max_entries, 128),
  application:ensure_all_started(ra),
  application:ensure_all_started(sasl),
  ok.

init_per_group(_G, Config) ->
  PrivDir = ?config(priv_dir, Config),
  DataDir = filename:join([PrivDir, "data"]),
  ok = restart_ra(DataDir),
  Fun = fun(TestCase) ->
    fun(Name, Nodes, Machine) ->
      UId = atom_to_binary(Name, utf8),
      Dir = filename:join([PrivDir, TestCase,
        ra_lib:to_list(Name)]),
      ok = filelib:ensure_dir(Dir),
      Conf = #{cluster_id => TestCase,
        id => {Name, node()},
        uid => UId,
        log_module => ra_log_file,
        log_init_args => #{uid => UId},
        initial_nodes => Nodes,
        machine => Machine,
        await_condition_timeout => 5000},
      ct:pal("starting ~p", [Name]),
      ra:start_node(Conf)
    end
  end,
  [{start_node_fun, Fun} | Config].

end_per_group(_, Config) ->
  Config.



init_per_testcase(TestCase, Config0) ->
  Fun0 = proplists:get_value(start_node_fun, Config0),
  ct:pal("Fun0 = ~p", [Fun0]),
  Fun = Fun0(TestCase), % "partial application"
  Config = proplists:delete(start_node_fun, Config0),
  [{test_name, ra_lib:to_list(TestCase)}, {start_node_fun, Fun} | Config].

end_per_testcase(_TestCase, Config) ->
  ra_nodes_sup:remove_all(),
  Config.


terminate_cluster(Nodes) ->
  [ra:stop_node(P) || P <- Nodes].


node_name(Config, N) when is_integer(N) ->
  ra_node:name(?config(test_name, Config), erlang:integer_to_list(N)).



user_register1(Config) ->
  NumNodes = 3,
  Machine = {module, repbet_machine, #{}},
  [R1, R2, R3] = ra:start_local_cluster(NumNodes, proplists:get_value(test_name, Config), Machine),

  ok = repbet_machine:create_user(R1, "Hans", "Hans@example.com", "hunter2"),
  ok = repbet_machine:check_password(R2, "Hans", "hunter2"),
  ok = repbet_machine:check_password(R3, "Hans", "hunter2"),
  {error, authentication_failed} = repbet_machine:check_password(R2, "Hans", "hunter"),
  {error, authentication_failed} = repbet_machine:check_password(R2, "Hanss", "hunter2"),

  ok.

user_register_twice(Config) ->
  NumNodes = 2,
  Machine = {module, repbet_machine, #{}},
  [R1, R2] = ra:start_local_cluster(NumNodes, proplists:get_value(test_name, Config), Machine),

  ?assertEqual(ok, repbet_machine:create_user(R1, "Hans", "Hans@example.com", "hunter2")),
  ?assertEqual({error, name_taken}, repbet_machine:create_user(R2, "Hans", "AnotherHans@example.com", "password1234")),

  ok.

create_challenge(Config) ->
  NumNodes = 2,
  Machine = {module, repbet_machine, #{}},
  [R1, R2] = ra:start_local_cluster(NumNodes, proplists:get_value(test_name, Config), Machine),

  {ok, Cid1} = repbet_machine:create_challenge(R1, "It will rain Thursday, 17.05.2018"),
  {ok, Cid2} = repbet_machine:create_challenge(R2, "Kaiserslautern will be back in the 2. Bundesliga in 2019"),
  ?assertEqual(false, Cid1 == Cid2, 'ids should be unique'),

  ok.

