-module(account_server).
-behaviour(gen_server).
-include("common.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%%
-export([allocate/0]).

-record(state, {
    start = 0, len = 10, l = []
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    Start = start_account(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Start, 1000], []).

%% gen_server.

init([Start, Len]) ->
    L = create_rand_list(Start, Len),
    State = #state{start = Start , len = Len, l = L},
    ?LOG([?MODULE, init, State]),
    {ok, State}.

%% 同步调用
% gen_server:call(account_server, allocate).
handle_call(allocate, _From, State) ->
    [Account|Tail] = State#state.l,
    % ?LOG([handle_call, allocate, From, State]),
    case Tail of
        [] ->
            Start = State#state.start + State#state.len + 1,
            L = create_rand_list(Start, State#state.len),
            State2 = State#state{start = Start, l = L},
            {reply, Account, State2};
        _ ->
        State2 = State#state{l=Tail},
        {reply, Account, State2}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


allocate() ->
    gen_server:call(?MODULE, allocate).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec create_rand_list(Start::integer(), Len::integer()) -> list().
create_rand_list(Start, Len) ->
    L = lists:seq(Start, Start + Len),
    [X||{_, X} <- lists:sort([{rand:uniform(), N} || N <- L])].

-spec start_account() -> integer().
start_account() ->
    Sql = <<"SELECT max(CONVERT(account, UNSIGNED INTEGER)) as max FROM `user`">>,
    case mysql_pool:query(Sql) of
        {ok, _,[[Start]]} ->
            Start;
        _ ->
            50000
    end.

