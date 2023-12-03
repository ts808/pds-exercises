-module(causal_broadcast).

-behavior(gen_server).

-export([start_link/2, broadcast/2, update_pending/2, iterate_pending/2]).
-export([handle_call/3, init/1, handle_cast/2, handle_info/2]).

-record(state, {this_node, respond_to, reliable_pid, pending, vc}).

start_link(LinkLayer, RespondTo) ->
    gen_server:start_link(?MODULE, [LinkLayer, RespondTo], []).

init([LinkLayer, RespondTo]) ->
    {ok, Pid} = reliable_broadcast:start_link(LinkLayer, self()),
    VC = vectorclock:new(),
    ThisNode = link_layer:this_node(LinkLayer),

    {ok, #state{
        this_node = ThisNode,
        respond_to = RespondTo,
        reliable_pid = Pid,
        pending = [],
        vc = VC
    }}.

broadcast(Pid, Message) ->
    gen_server:call(Pid, {rco_broadcast, Message}).

handle_call({rco_broadcast, Message}, _From, State) ->
    State#state.respond_to ! {deliver, Message},
    reliable_broadcast:broadcast(State#state.reliable_pid, {
        State#state.this_node, State#state.vc, Message
    }),
    {reply, ok, State#state{vc = vectorclock:increment(State#state.vc, State#state.this_node)}}.

handle_info({deliver, {Id, VC, Message}}, State) ->
    case State#state.this_node == Id of
        false ->
            Pending = [{Id, VC, Message} | State#state.pending],
            {UpdatedPending, VC2} = update_pending({Id, VC, Message}, Pending),
            {noreply, State#state{vc = VC2, pending = UpdatedPending}};
        true ->
            {noreply, State}
    end.

update_pending({Id, VC, Message}, Pending) ->
    UpdatedPending = iterate_pending({Id, VC, Message}, Pending),
    case length(Pending) == length(UpdatedPending) of
        true -> {UpdatedPending, VC};
        false -> update_pending({Id, VC, Message}, UpdatedPending)
    end.

iterate_pending([], _) -> [];
iterate_pending([{Id_q, VC_q, Message_q} | Pending], {VC, Message}) ->
     case vectorclock:leq(VC, VC_q) of
            true ->
                Id_q ! {deliver, Message},
                iterate_pending(Pending, {vectorclock:increment(VC, Id_q), Message});
            false ->
                {Id_q, VC_q, Message_q} ++ iterate_pending(Pending, {VC, Message})
    end.


handle_cast(_Request, State) ->
    {noreply, State}.
