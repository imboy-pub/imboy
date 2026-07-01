-module(e2ee_cleanup_worker).
%%%===================================================================
%%% @doc E2EE 传输会话过期清理 GenServer
%%%
%%% 定期调用 e2ee_transfer_repo:cleanup_expired_sessions/0
%%% 删除 expires_at < NOW() 的 pending/accepted 会话。
%%%
%%% 配置 (sys.config):
%%% - {e2ee_cleanup_enabled, true}      % 启用清理（默认 true）
%%% - {e2ee_cleanup_interval, 300000}   % 扫描间隔，默认 5 分钟
%%%===================================================================

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0]).
-export([cleanup_now/0]).
-export([get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% 5 minutes
-define(DEFAULT_INTERVAL, 300000).

-record(state, {
    interval :: pos_integer(),
    timer_ref :: reference() | undefined,
    total_cleaned :: non_neg_integer()
}).

%% API

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec cleanup_now() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_now() ->
    gen_server:call(?MODULE, cleanup, 5000).

-spec get_status() -> #{enabled => boolean(), total_cleaned => non_neg_integer()}.
get_status() ->
    gen_server:call(?MODULE, get_status, 5000).

%% gen_server callbacks

init([]) ->
    Enabled = application:get_env(imboy, e2ee_cleanup_enabled, true),
    Interval = application:get_env(imboy, e2ee_cleanup_interval, ?DEFAULT_INTERVAL),
    case Enabled of
        true ->
            TimerRef = erlang:send_after(Interval, self(), cleanup),
            {ok, #state{interval = Interval, timer_ref = TimerRef, total_cleaned = 0}};
        false ->
            ?INFO_LOG([e2ee_cleanup_worker, disabled]),
            {ok, #state{interval = Interval, timer_ref = undefined, total_cleaned = 0}}
    end.

handle_call(cleanup, _From, State) ->
    Result = do_cleanup(),
    {reply, Result, increment_count(State, Result)};
handle_call(get_status, _From, State) ->
    Enabled = application:get_env(imboy, e2ee_cleanup_enabled, true),
    Status = #{enabled => Enabled, total_cleaned => State#state.total_cleaned},
    {reply, Status, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{interval = Interval} = State) ->
    Result = do_cleanup(),
    NewState = increment_count(State, Result),
    TimerRef = erlang:send_after(Interval, self(), cleanup),
    {noreply, NewState#state{timer_ref = TimerRef}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = undefined}) ->
    ok;
terminate(_Reason, #state{timer_ref = Ref}) ->
    _ = erlang:cancel_timer(Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

do_cleanup() ->
    e2ee_transfer_repo:cleanup_expired_sessions().

increment_count(State, {ok, Count}) ->
    State#state{total_cleaned = State#state.total_cleaned + Count};
increment_count(State, _) ->
    State.
