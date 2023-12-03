-module(best_effort_broadcast).

-behavior(gen_server).

-export([start_link/2, broadcast/2]).
-export([handle_call/3, init/1, handle_cast/2, handle_info/2]).

-record(state, {link_layer, respond_to}).

start_link(LinkLayer, RespondTo) ->
    gen_server:start_link(?MODULE, [LinkLayer, RespondTo], []).

init([LinkLayer, RespondTo]) ->
    link_layer:register(LinkLayer, self()),
    % {ok, Nodes} = link_layer:other_nodes(LinkLayer),
    % lists:foreach(fun(Node) -> link_layer:register(Node, self()) end, Nodes),
    {ok, #state{link_layer = LinkLayer, respond_to = RespondTo}}.

broadcast(Pid, Message) ->
    gen_server:call(Pid, {be_broadcast, Message}).

handle_call({be_broadcast, Message}, _From, State) ->
    LinkLayer = State#state.link_layer,
    {ok, Nodes} = link_layer:all_nodes(LinkLayer),
    lists:foreach(fun(Node) -> link_layer:send(LinkLayer, {be_receive, Message}, Node) end, Nodes),
    {reply, ok, State}.

handle_info({be_receive, Message}, State) ->
    State#state.respond_to ! {deliver, Message},
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
