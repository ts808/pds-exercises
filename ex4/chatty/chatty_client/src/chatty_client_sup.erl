-module(chatty_client_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 50, period => 10},
    ChildSpecs = [#{id => chatty_client, start => {chatty_client, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.
