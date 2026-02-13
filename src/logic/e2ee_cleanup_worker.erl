-module(e2ee_cleanup_worker).
%%%===================================================================
%%% @doc E2EE 定时清理工作进程
%%%
%%% 功能：
%%% - 定期清理过期的设备传输会话
%%% - 监控停滞的会话并记录日志
%%%
%%% 配置：
%%% - 清理间隔：默认 1 小时 (3600000ms)
%%% - 可通过环境变量 e2ee_cleanup_interval 配置
%%%===================================================================

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0]).
-export([cleanup_now/0]).
-export([get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_INTERVAL, 3600000). % 1 小时

-record(state, {
    interval :: pos_integer(),
    last_cleanup :: erlang:timestamp() | undefined,
    cleanup_count :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc 启动清理工作进程
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 手动触发清理
-spec cleanup_now() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_now() ->
    gen_server:call(?MODULE, cleanup_now).

%% @doc 获取清理状态
-spec get_status() -> map().
get_status() ->
    gen_server:call(?MODULE, get_status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Interval = application:get_env(imboy, e2ee_cleanup_interval, ?DEFAULT_INTERVAL),
    State = #state{
        interval = Interval,
        last_cleanup = undefined,
        cleanup_count = 0
    },
    % 首次延迟 5 分钟执行，避免启动时压力
    erlang:send_after(300000, self(), do_cleanup),
    ok = ?INFO_LOG([e2ee_cleanup_worker, started, #{interval => Interval}]),
    {ok, State}.

handle_call(cleanup_now, _From, State) ->
    {Result, NewState} = do_cleanup_internal(State),
    {reply, Result, NewState};

handle_call(get_status, _From, State) ->
    Status = #{
        interval => State#state.interval,
        last_cleanup => State#state.last_cleanup,
        cleanup_count => State#state.cleanup_count
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_cleanup, State) ->
    {_Result, NewState} = do_cleanup_internal(State),
    % 调度下一次清理
    erlang:send_after(NewState#state.interval, self(), do_cleanup),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc 执行清理
-spec do_cleanup_internal(#state{}) -> {{ok, non_neg_integer()} | {error, term()}, #state{}}.
do_cleanup_internal(State) ->
    % 1. 清理过期会话
    Result = e2ee_transfer_repo:cleanup_expired_sessions(),

    % 2. 检查停滞会话
    case e2ee_transfer_repo:get_stalled_sessions() of
        {ok, []} ->
            ok;
        {ok, StalledSessions} ->
            ok = ?WARN_LOG([e2ee_cleanup_worker, stalled_sessions, #{
                count => length(StalledSessions),
                sessions => StalledSessions
            }]);
        {error, _Reason} ->
            ok
    end,

    NewState = State#state{
        last_cleanup = erlang:timestamp(),
        cleanup_count = State#state.cleanup_count + 1
    },

    case Result of
        {ok, Count} when Count > 0 ->
            ok = ?INFO_LOG([e2ee_cleanup_worker, cleanup_done, #{deleted => Count}]);
        _ ->
            ok
    end,

    {Result, NewState}.
