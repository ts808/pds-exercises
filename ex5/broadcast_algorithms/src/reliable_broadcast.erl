-module(reliable_broadcast).

-behavior(gen_server).

-export([start_link/2, broadcast/2]).
-export([handle_call/3, init/1, handle_cast/2, handle_info/2]).

-record(state, {respond_to, delivered, best_effort_pid}).

start_link(LinkLayer, RespondTo) ->
    gen_server:start_link(?MODULE, [LinkLayer, RespondTo], []).

init([LinkLayer, RespondTo]) ->
    {ok, Pid} = best_effort_broadcast:start_link(LinkLayer, self()),
    {ok, #state{
        respond_to = RespondTo,
        delivered = [],
        best_effort_pid = Pid
    }}.

broadcast(Pid, Message) ->
    gen_server:call(Pid, {r_broadcast, Message}).


handle_call({r_broadcast, Message}, _From, State) ->
    State#state.respond_to ! {deliver, Message},
    MessageId = erlang:binary_to_list(base64:encode(crypto:strong_rand_bytes(8))),
    Delivered = State#state.delivered,
    best_effort_broadcast:broadcast(State#state.best_effort_pid, {MessageId, Message}),
    {reply, ok, State#state{delivered=[MessageId | Delivered]}}.


handle_info({deliver, {Id, Message}}, State) ->
    Delivered = State#state.delivered,
    case lists:member(Id, Delivered) of
        false -> 
            State#state.respond_to ! {deliver, Message},
            best_effort_broadcast:broadcast(State#state.best_effort_pid, {Id, Message}),
            {noreply, State#state{delivered=[Id | Delivered]}};
        true -> 
            {noreply, State}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.
