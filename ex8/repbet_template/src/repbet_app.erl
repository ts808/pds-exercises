%%%-------------------------------------------------------------------
%% @doc repbet public API
%% @end
%%%-------------------------------------------------------------------

-module(repbet_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    % Store data for this node in folder ./data./data-{$nodename}
    % This configuration parameter must be set before starting the application:
    ok = application:set_env(ra, data_dir, "./data/data-" ++ atom_to_list(node()), [{persistent, true}]),
    {ok, _} = application:ensure_all_started(ra),
    repbet_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
