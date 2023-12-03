-module(repbet_machine).

-behavior(ra_machine).

-export([
    init/1,
    apply/3,
    leader_effects/1,
    eol_effects/1,
    tick/1,
    overview/1,
    create_user/4,
    check_password/3,
    create_challenge/2,
    bet/5,
    change_balance/3,
    complete_challenge/3,
    list_active_challenges/1,
    list_user_challenges/2,
    get_balance/2
]).

-record(state, {
    users = #{},
    challenges = #{}
}).

-record(user, {
    name,
    mail,
    password,
    balance = 0
}).

-record(challenge, {
    id,
    description,
    bets
}).

%% @doc send a command to the ra node.
%% if the ra node addressed isn't the leader and the leader is known
%% it will automatically redirect the call to the leader node.
%% This function returns after the command has been appended to the leader's
%% raft log.
%%
%% @param ServerRef the ra node id of the node to send the command to.
%% @param Command the command, an arbitrary term that the current state
%% machine can understand.
send(Node, Request) ->
    case ra:send(Node, Request) of
        {ok, Res, _Leader} ->
            Res;
        Other ->
            Other
    end.

%% @doc send a command to the ra node and waits for a result.
%% if the ra node addressed isn't the leader and the leader is known
%% it will automatically redirect the call to the leader node.
%% This function returns after the command has been replicated and applied to
%% the ra state machine. This is a fully synchronous interaction with the
%% ra consensus system.
%% Use this for low throughput actions where simple semantics are needed.
%% if the state machine supports it it may return a result value which will
%% be included in the result tuple.
%%
%% @param ServerRef the ra node id of the node to send the command to.
%% @param Command the command, an arbitrary term that the current state
%% machine can understand.
send_and_await_consensus(Node, Request) ->
    case ra:send_and_await_consensus(Node, Request) of
        {ok, Res, _Leader} ->
            Res;
        Other ->
            Other
    end.

%% @doc query the machine state on any node
%% This allows you to run the QueryFun over the the machine state and
%% return the result. Any ra node can be addressed.
%% This can return infinitely state results.
dirty_query(Node, ReadFunc) ->
    {ok, {_, Res}, _} = ra:dirty_query(Node, ReadFunc),
    Res.

%% @doc Query the state machine
%% This allows a caller to query the state machine by appending the query
%% to the log and returning the result once applied. This guarantees the
%% result is consistent.
consistent_query(Node, ReadFunc) ->
    case ra:consistent_query(Node, ReadFunc) of
        {ok, {_, Res}, _} ->
            Res;
        Other ->
            Other
    end.

%% Creates a new user with the given Name, Mail, and Password.
%% Returns 'ok' when the user was created and {error, name_taken} if the user name is already taken.
create_user(Node, Name, Mail, Password) ->
    send_and_await_consensus(Node, {add_user, Name, Mail, Password}).

%% Checks if there is a user with the given name and password combination
%% Returns ok if password matches and {error, authentication_failed} otherwise
check_password(Node, Username, Password) ->
    dirty_query(Node, fun(State) ->
        Users = State#state.users,
        case lists:keyfind(Username, 1, Users) of
            false ->
                {error, authentication_failed};
            {_, _, Pswd, _} ->
                case Password == Pswd of
                    true -> ok;
                    false -> {error, authentication_failed}
                end
        end
    end).

%% Creates a new challenge with the given description that users can bet on.
%% Each challenge gets assigned a new unique id, which is returned as {ok, ChallengeId}.
create_challenge(Node, Description) ->
    send_and_await_consensus(Node, {add_challenge, Description}).

%% Makes the given user bet on a given challenge.
%% Result is either true or false (true if the user bets, that the challenge turns out to be true, false otherwise)
%% Money is the amount of money the user bets on this challenge. This amount is directly subtracted from the users balance.
%% Returns ok if betting was successful
%% Returns {error, amount_must_be_positive}, if Money is <= 0
%% Returns {error, invalid_user} or {error, invalid_challenge} if user of challenge does not exist
%% Returns {error, insufficient_funds} if the users account balance is less than the Money he wants to bet
bet(Node, Username, ChallengeId, Result, Money) ->
    send_and_await_consensus(Node, {bet, Username, ChallengeId, Result, Money}).

%% Changes the balance of the given user by the given Amount
%% Amount can be positive or negative, but the user account must always have positive value
%% Returns ok on success and {error, insufficient_funds}, if changing the balance would go below 0
change_balance(Node, Username, Amount) ->
    send_and_await_consensus(Node, {change_balance, Username, Amount}).

%% Completes the given challenge by setting a Result
%% Result is either true or false
%% All Money bet on this challenge is distributed among the users that guesses the result correctly,
%% proportionally to the amount they have bet.
complete_challenge(Node, ChallengeId, Result) ->
    send_and_await_consensus(Node, {complete_challenge, ChallengeId, Result}).

%% Get a list of all challenges, that have not yet been completed
%% Returns a list of tuples: {Id, Description, TotalMoney, YesPercentage}
%% Id is the Id of the challenge
%% Description is its description
%% TotalMoney is the overall amount invested in this challenge
%% YesPercentage is the percentage of money that has been set on 'yes' (true)
list_active_challenges(Node) ->
    error(not_implemented).

%% Get a list of all challenges, that the given user has participated in
%% Returns a list of tuples: {Id, Description, Result, UserGuess, UserMoney, TotalMoney, YesPercentage}
%% Id is the Id of the challenge
%% Description is its description
%% Result is either 'true' or 'false' for completed challenges or 'not_completed' otherwise
%% TotalMoney is the overall amount invested in this challenge
%% YesPercentage is the percentage of money that has been set on 'yes' (true)
%% UserGuess is the result guessed by the user in his bet
%% UserMoney is the amount of money invested by the user
list_user_challenges(Node, Username) ->
    error(not_implemented).

%% Get the current balance of a user
%% Returns {ok, Balance} or {error, user_not_found}
get_balance(Node, Username) ->
    dirty_query(Node, fun(State) ->
        Users = State#state.users,
        case lists:keyfind(Username, 1, Users) of
            false -> {error, invalid_user};
            User -> {ok, User#user.balance}
        end
    end).

%% ra_machine callbacks:

%% Init initializes the state machine
%% Returns a pair {InitialState, Effects}
init(_Conf) ->
    {#state{}, []}.

%% Apply get's called when a command is sent to the state machine
%% First argument is the Index of the Command
%% Second argument is the command given in send or send_and_await_consensus
%% Third argument is the current state
%%
%% Returns a tuple {NewState, Effects, Result}
%% Or a tuple {NewState, Effects} if there is no Result
apply(_Index, {add_user, Name, Mail, Password}, State) ->
    Users = State#state.users,
    case lists:keyfind(Name, 1, Users) of
        false -> {State#state{users = [{Name, Mail, Password, 0} | Users]}, [], ok};
        _ -> {error, name_taken}
    end;
apply(_Index, {add_challenge, Description}, State) ->
    Challenges = State#state.challenges,
    case Challenges == [] of
        true ->
            {State#state{challenges = [{1, Description, []}]}, [], {ok, 1}};
        false ->
            ID = lists:foldl(fun(Elem, Cur) -> max(Elem#challenge.id, Cur) end, 1, Challenges) + 1,
            {State#state{challenges = [{ID, Description, []} | Challenges]}, [], {ok, ID}}
    end;
apply(_Index, {bet, Username, ChallengeId, Result, Money}, State) ->
    case Money =< 0 of
        true ->
            {error, amount_must_be_positive};
        false ->
            Users = State#state.users,
            case lists:keyfind(Username, 1, Users) of
                false ->
                    {error, invalid_user};
                User ->
                    Challenges = State#state.challenges,
                    case lists:keyfind(ChallengeId, 1, Challenges) of
                        false ->
                            {error, invalid_challenge};
                        Challenge ->
                            Balance = User#user.balance,
                            case Balance >= Money of
                                false ->
                                    {error, insufficient_funds};
                                true ->
                                    % UserDeducted = User#user{balance = Balance - Money},
                                    Bets = Challenge#challenge.bets,
                                    ChallengedUpdated = Challenge#challenge{
                                        bets = [{Username, Money, Result} | Bets]
                                    },
                                    {
                                        State#state{
                                            % users = [UserDeducted | lists:delete(User, Users)],
                                            challenges = [
                                                ChallengedUpdated
                                                | lists:delete(Challenge, ChallengeId)
                                            ]
                                        },
                                        [],
                                        ok
                                    }
                            end
                    end
            end
    end;
apply(_Index, {change_balance, Username, Amount}, State) ->
    Users = State#state.users,
    case lists:keyfind(Username, 1, Users) of
        false ->
            {error, invalid_user};
        User ->
            Balance = User#user.balance,
            case Balance >= Amount of
                false ->
                    {error, insufficient_funds};
                true ->
                    UserUpdated = User#user{balance = Balance + Amount},
                    {
                        State#state{users = [UserUpdated | lists:delete(User, Users)]},
                        [],
                        ok
                    }
            end
    end;
apply(_Index, {complete_challenge, ChallengeId, Result}, State) ->
    Challenges = State#state.challenges,
    case lists:keyfind(ChallengeId, 1, Challenges) of
        false ->
            {error, invalid_challenge};
        Challenge ->
            Users = State#state.users,
            Bets = Challenge#challenge.bets,
            Prize = lists:foldl(fun({_, M, _}, Acc) -> Acc + M end, 0, Bets) / length(Users),
            WonBets = lists:filter(fun({_, _, R}) -> Result == R end, Bets),
            % yes, the way earnings are assigned is incorrect
            UsersUpdated = lists:map(
                fun({Username, _, _}) ->
                    User = lists:keyfind(Username, 1, Users),
                    User#user{balance = User#user.balance + Prize}
                end,
                WonBets
            ),
            {State#state{users = UsersUpdated}, [], ok}
    end.

leader_effects(_State) -> [].

eol_effects(_State) -> [].

tick(_State) -> [].

overview(_State) -> #{}.
