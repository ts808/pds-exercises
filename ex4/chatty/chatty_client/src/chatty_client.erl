-module(chatty_client).

-behaviour(gen_server).

% API
-export([join/0, leave/0, message/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

%% here we are in the caller process
chatty() -> global:whereis_name(chatty).
join() -> joined = gen_server:call(?MODULE, join).
leave() -> left= gen_server:call(?MODULE, leave).
message(Message) -> gen_server:call(chatty(), {message, Message}).

start_link() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% here we are in the gen server process
init([]) -> {ok, no_state}.

handle_call(join, _From, State) -> gen_server:call(chatty(), {action, join}), {reply, joined, State};
handle_call(leave, _From, State) -> gen_server:call(chatty(), {action, leave}), {reply, left, State}.

handle_cast(Request, State) ->
  io:format("Server:~n~p~n", [Request]),
  {noreply, State}.
