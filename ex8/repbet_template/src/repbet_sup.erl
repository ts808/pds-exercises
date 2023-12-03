%%%-------------------------------------------------------------------
%% @doc repbet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(repbet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { #{strategy => one_for_all, intensity => 0, period => 1}, [
        #{id => repbet_raft,
            start => {repbet_raft, start, []},
            restart => transient, % only for starting
            shutdown => 100,
            type => worker,
            modules => [repbet_raft]}
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
