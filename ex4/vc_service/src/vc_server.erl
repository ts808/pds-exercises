-module(vc_server).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    tick/1,
    get_vc/0,
    stop/0,
    advance_to/1,
    get/1,
    handle_call/3,
    handle_cast/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

tick(P) ->
    gen_server:cast(?MODULE, {tick, P}).

advance_to(VC) ->
    gen_server:cast(?MODULE, {advance_to, VC}).

get(P) ->
    gen_server:call(?MODULE, {get, P}).

get_vc() ->
    gen_server:call(?MODULE, get_vc).

stop() ->
    gen_server:stop(?MODULE).

init(_) ->
    {ok, vectorclock:new()}.

handle_cast({tick, P}, State) ->
    {noreply, vectorclock:increment(State, P)};
handle_cast({advance_to, Other}, VC) ->
    {noreply, vectorclock:merge(VC, vectorclock:from_list(Other))}.

handle_call({get, P}, _From, State) ->
    {reply, vectorclock:get(State, P), State};
handle_call(get_vc, _From, State) ->
    {reply, vectorclock:to_list(State), State}.