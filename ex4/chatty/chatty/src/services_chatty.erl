-module(services_chatty).

-behaviour(gen_server).

% module API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([add_user/2, remove_user/2, send_message/2]).

-define(SERVER, ?MODULE).

-record(state, {users = []}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	% registers this server as a global process
	% you can get the PID for this anywhere by using
	%    Pid = global:whereis_name(chatty).
  gen_server:start_link({global, chatty}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.


handle_call({action, join}, {Pid, _}, State) ->
	{reply, ok, add_user(State, Pid)};

handle_call({action, leave}, {Pid, _}, State) ->
	{reply, ok, remove_user(State, Pid)};

handle_call({message, Message}, _, State) ->
	send_message(State, Message),
	{reply, ok, State}.

handle_cast(_Request, State) ->
  	{noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_user(State, Pid) ->
	Users = State#state.users,
	State#state{users = [Pid | Users]}.

remove_user(State, Pid) ->
	Users = State#state.users,
	State#state{users = lists:delete(Pid, Users)}.

send_message(State, Message) ->
	Users = State#state.users,
	lists:foreach(fun(Pid) -> gen_server:cast(Pid, Message) end, Users).