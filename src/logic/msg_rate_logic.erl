-module(msg_rate_logic).
%%%===================================================================
%%% @doc
%%% msg_rate_logic - 消息频率异常检测逻辑层
%%%
%%% 功能：
%%% - per-user 每分钟消息数监控
%%% - 超过 30 条/分钟触发警告
%%% - 超过 60 条/分钟自动禁言 5 分钟
%%% - 提供 check_and_record/1, is_muted/1, unmute/1 API
%%%
%%% 使用示例：
%%% ```
%%% ok = msg_rate_logic:check_and_record(Uid),
%%% false = msg_rate_logic:is_muted(Uid),
%%% ok = msg_rate_logic:unmute(Uid)
%%% ```
%%%===================================================================

-include("log.hrl").

%% API 函数
-export([check_and_record/1]).
-export([is_muted/1]).
-export([unmute/1]).

%% 内部导出（用于测试和定时器回调）
-export([init_table/0]).
-export([reset_counters/0]).

%% 阈值默认值（可通过 sys.config 覆盖）
-define(DEFAULT_WARN_THRESHOLD, 30).       % 每分钟超过 30 条触发警告
-define(DEFAULT_MUTE_THRESHOLD, 60).       % 每分钟超过 60 条自动禁言
-define(DEFAULT_MUTE_DURATION_MS, 300000). % 禁言持续 5 分钟 (毫秒)
-define(WINDOW_MS, 60000).                 % 统计窗口 1 分钟 (毫秒)

%% ETS 表名
-define(MSG_RATE_TAB, msg_rate_counter).   % {Uid, Count}  — 原子计数器
-define(MSG_MUTE_TAB, msg_rate_muted).     % {Uid, MuteUntil}

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 检查并记录用户消息发送
%% 返回 ok 表示允许发送，{error, muted} 表示用户被禁言，
%% {warning, Count} 表示接近禁言阈值
%% @param Uid 用户ID (integer)
%% @returns ok | {warning, integer()} | {error, muted}
-spec check_and_record(integer()) -> ok | {warning, integer()} | {error, muted}.
check_and_record(Uid) ->
    ensure_tables(),
    %% 先检查是否被禁言
    case is_muted(Uid) of
        true ->
            {error, muted};
        false ->
            %% 使用 ets:update_counter 原子递增，避免竞态条件
            Count = try
                ets:update_counter(?MSG_RATE_TAB, Uid, {2, 1})
            catch
                error:badarg ->
                    %% Key 不存在，初始化为 1
                    ets:insert_new(?MSG_RATE_TAB, {Uid, 1}),
                    1
            end,
            MuteThreshold = mute_threshold(),
            WarnThreshold = warn_threshold(),
            if
                Count > MuteThreshold ->
                    %% 超过禁言阈值，自动禁言
                    Now = erlang:system_time(millisecond),
                    do_mute(Uid, Now),
                    ?WARN_LOG("User ~p auto-muted: ~p msgs/min exceeded threshold ~p",
                              [Uid, Count, MuteThreshold]),
                    {error, muted};
                Count > WarnThreshold ->
                    %% 超过警告阈值
                    ?WARN_LOG("User ~p msg rate warning: ~p msgs/min exceeded threshold ~p",
                              [Uid, Count, WarnThreshold]),
                    {warning, Count};
                true ->
                    ok
            end
    end.

%% @doc 检查用户是否被禁言
%% @param Uid 用户ID (integer)
%% @returns boolean()
-spec is_muted(integer()) -> boolean().
is_muted(Uid) ->
    ensure_tables(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?MSG_MUTE_TAB, Uid) of
        [{Uid, MuteUntil}] when MuteUntil > Now ->
            true;
        [{Uid, _Expired}] ->
            %% 禁言已过期，清理
            ets:delete(?MSG_MUTE_TAB, Uid),
            false;
        [] ->
            false
    end.

%% @doc 手动解除用户禁言
%% @param Uid 用户ID (integer)
%% @returns ok
-spec unmute(integer()) -> ok.
unmute(Uid) ->
    ensure_tables(),
    ets:delete(?MSG_MUTE_TAB, Uid),
    ok.

%% @doc 初始化 ETS 表（应用启动时调用）
%% MSG_RATE_TAB 存储 {Uid, Count}，每分钟由 reset_counters 定时器清零
-spec init_table() -> ok.
init_table() ->
    create_table_if_not_exists(?MSG_RATE_TAB, [set, public, named_table,
                                                {write_concurrency, true}]),
    create_table_if_not_exists(?MSG_MUTE_TAB, [set, public, named_table,
                                                {read_concurrency, true},
                                                {write_concurrency, true}]),
    %% 每分钟清零计数器（滑动窗口近似为固定窗口，简单且无竞态）
    {ok, _TRef} = timer:apply_interval(?WINDOW_MS, ?MODULE, reset_counters, []),
    ok.

%% @doc 每分钟清零所有用户的消息计数器
-spec reset_counters() -> ok.
reset_counters() ->
    ets:delete_all_objects(?MSG_RATE_TAB),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 执行禁言操作
%% @private
-spec do_mute(integer(), integer()) -> true.
do_mute(Uid, Now) ->
    MuteUntil = Now + mute_duration_ms(),
    ets:insert(?MSG_MUTE_TAB, {Uid, MuteUntil}).

%% @doc 从 sys.config 读取阈值，未配置则用默认值
warn_threshold() ->
    config_ds:env(msg_rate_warn_threshold, ?DEFAULT_WARN_THRESHOLD).
mute_threshold() ->
    config_ds:env(msg_rate_mute_threshold, ?DEFAULT_MUTE_THRESHOLD).
mute_duration_ms() ->
    config_ds:env(msg_rate_mute_duration_ms, ?DEFAULT_MUTE_DURATION_MS).

%% @doc 确保 ETS 表存在
%% @private
-spec ensure_tables() -> ok.
ensure_tables() ->
    create_table_if_not_exists(?MSG_RATE_TAB, [set, public, named_table,
                                                {write_concurrency, true}]),
    create_table_if_not_exists(?MSG_MUTE_TAB, [set, public, named_table,
                                                {read_concurrency, true},
                                                {write_concurrency, true}]),
    ok.

%% @doc 如果 ETS 表不存在则创建
%% @private
-spec create_table_if_not_exists(atom(), list()) -> ok.
create_table_if_not_exists(Name, Options) ->
    case ets:whereis(Name) of
        undefined ->
            try
                ets:new(Name, Options),
                ok
            catch
                error:badarg ->
                    %% 表可能已被其他进程创建（竞态条件）
                    ok
            end;
        _ ->
            ok
    end.
