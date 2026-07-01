-module(msg_burn_logic).
%%%===================================================================
%%% @doc 消息自毁定时清理 GenServer
%%%
%%% 功能：
%%% - 定期扫描 msg_c2c / msg_c2g 中 expire_at <= NOW() 的消息
%%% - 批量删除过期消息
%%% - 支持手动触发清理
%%%
%%% 配置 (sys.config):
%%% - {msg_burn_enabled, true}         % 启用自毁功能（默认 true）
%%% - {msg_burn_interval, 60000}       % 扫描间隔，默认 60 秒
%%% - {msg_burn_batch_size, 500}       % 每批删除上限
%%%===================================================================

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/0]).
-export([cleanup_now/0]).
-export([get_status/0]).
-export([calc_expire_at/2]).
-export([valid_expire_secs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% 60 秒
-define(DEFAULT_INTERVAL, 60000).
-define(DEFAULT_BATCH_SIZE, 500).
%% 允许的自毁秒数：5s / 30s / 1m / 5m / 1h / 24h
-define(ALLOWED_EXPIRE_SECS, [5, 30, 60, 300, 3600, 86400]).

-record(state, {
    interval :: pos_integer(),
    batch_size :: non_neg_integer(),
    last_cleanup :: erlang:timestamp() | undefined,
    total_cleaned :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 手动触发清理
-spec cleanup_now() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_now() ->
    gen_server:call(?MODULE, cleanup_now, 30000).

%% @doc 获取清理状态
-spec get_status() -> map().
get_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc 根据 expire_secs 计算 expire_at（RFC3339 格式）
%% 返回 null 表示不自毁
-spec calc_expire_at(binary(), integer() | undefined) -> binary() | null.
calc_expire_at(_CreatedAt, undefined) ->
    null;
calc_expire_at(_CreatedAt, 0) ->
    null;
calc_expire_at(CreatedAt, ExpireSecs) when is_integer(ExpireSecs), ExpireSecs > 0 ->
    CreatedAtMs = elib_dt:rfc3339_to(CreatedAt),
    ExpireAtMs = CreatedAtMs + (ExpireSecs * 1000),
    elib_dt:to_rfc3339(ExpireAtMs);
calc_expire_at(_CreatedAt, _) ->
    null.

%% @doc 验证 expire_secs 是否在允许范围内
-spec valid_expire_secs(integer() | undefined) -> boolean().
valid_expire_secs(undefined) ->
    true;
valid_expire_secs(0) ->
    true;
valid_expire_secs(Secs) when is_integer(Secs) ->
    lists:member(Secs, ?ALLOWED_EXPIRE_SECS);
valid_expire_secs(_) ->
    false.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Enabled = application:get_env(imboy, msg_burn_enabled, true),
    case Enabled of
        true ->
            Interval = application:get_env(imboy, msg_burn_interval, ?DEFAULT_INTERVAL),
            BatchSize = application:get_env(imboy, msg_burn_batch_size, ?DEFAULT_BATCH_SIZE),
            State = #state{
                interval = Interval,
                batch_size = BatchSize,
                last_cleanup = undefined,
                total_cleaned = 0
            },
            %% 首次延迟 30 秒执行
            erlang:send_after(30000, self(), do_cleanup),
            ok = ?INFO_LOG([
                msg_burn_logic, started, #{interval => Interval, batch_size => BatchSize}
            ]),
            {ok, State};
        false ->
            ok = ?INFO_LOG([msg_burn_logic, disabled]),
            {ok, #state{interval = 0, batch_size = 0, last_cleanup = undefined, total_cleaned = 0}}
    end.

handle_call(cleanup_now, _From, State) ->
    {Result, NewState} = do_cleanup_internal(State),
    {reply, Result, NewState};
handle_call(get_status, _From, State) ->
    Status = #{
        interval => State#state.interval,
        batch_size => State#state.batch_size,
        last_cleanup => State#state.last_cleanup,
        total_cleaned => State#state.total_cleaned
    },
    {reply, Status, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_cleanup, #state{interval = 0} = State) ->
    %% 功能关闭，不执行
    {noreply, State};
handle_info(do_cleanup, State) ->
    {_Result, NewState} = do_cleanup_internal(State),
    erlang:send_after(State#state.interval, self(), do_cleanup),
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

do_cleanup_internal(State) ->
    try
        BatchSize = State#state.batch_size,
        Now = elib_dt:now(),
        %% 清理 C2C 过期消息
        C2CCount = cleanup_expired_c2c(Now, BatchSize),
        %% 清理 C2G 过期消息
        C2GCount = cleanup_expired_c2g(Now, BatchSize),
        Total = C2CCount + C2GCount,
        case Total > 0 of
            true ->
                ok = ?INFO_LOG([
                    msg_burn_cleanup, #{c2c => C2CCount, c2g => C2GCount, total => Total}
                ]);
            false ->
                ok
        end,
        NewState = State#state{
            last_cleanup = erlang:timestamp(),
            total_cleaned = State#state.total_cleaned + Total
        },
        {{ok, Total}, NewState}
    catch
        _:Error ->
            ok = ?ERROR_LOG([msg_burn_cleanup_error, Error]),
            {{error, Error}, State}
    end.

%% @doc 删除 msg_c2c 中已过期的消息
cleanup_expired_c2c(Now, BatchSize) ->
    msg_c2c_ds:delete_expired(Now, BatchSize).

%% @doc 删除 msg_c2g 中已过期的消息
cleanup_expired_c2g(Now, BatchSize) ->
    msg_c2g_ds:delete_expired(Now, BatchSize).
