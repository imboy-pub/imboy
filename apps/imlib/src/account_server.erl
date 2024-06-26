-module(account_server).

-include_lib("imlib/include/log.hrl").

-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([start_account/0]).

%%
-export([allocate/0]).

-record(state, {
          start = 0,
          len = 10,
          l = []
         }).

%% ===================================================================
%% API
%% ===================================================================


allocate() ->
    gen_server:call(?MODULE, allocate).


%% ===================================================================
%%% gen_server callbacks
%% ===================================================================


-spec start_link() -> {ok, pid()}.
start_link() ->
    ?LOG(['account_server/start_link/0', imboy_dt:now()]),
    Start = account_server:start_account(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Start, 1000], []).


init([Start, Len]) ->
    L = create_rand_list(Start, Len),
    State = #state{
              start = Start,
              len = Len,
              l = L
             },
    % ?LOG([?MODULE, init, State]),
    {ok, State}.


%% 同步调用
% gen_server:call(account_server, allocate).
handle_call(allocate, _From, State) ->
    [Account | Tail] = State#state.l,
    % ?LOG([handle_call, allocate, From, State]),
    case Tail of
        [] ->
            Start = State#state.start + State#state.len + 1,
            L = create_rand_list(Start, State#state.len),
            State2 = State#state{start = Start, l = L},
            {reply, Account, State2};
        _ ->
            State2 = State#state{l = Tail},
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


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


-spec create_rand_list(Start :: integer(), Len :: integer()) -> list().
create_rand_list(Start, Len) ->
    L = lists:seq(Start, Start + Len),
    [ X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- L ]) ].


start_account() ->
    ?LOG(['account_server/start_account/0', imboy_dt:now()]),
    % 0.
    % Sql = <<"SELECT max(CAST(account as integer)) as max FROM public.user">>.
    Num = imboy_db:pluck(<<"user">>, <<"max(CAST(account as integer)) as max">>, 50000),
    case Num of
        null ->
            50000;
        _ ->
            Num
    end.
