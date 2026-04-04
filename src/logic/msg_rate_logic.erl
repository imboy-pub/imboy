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

%% 内部导出（用于测试）
-export([init_table/0]).

%% 阈值常量
-define(WARN_THRESHOLD, 30).       % 每分钟超过 30 条触发警告
-define(MUTE_THRESHOLD, 60).       % 每分钟超过 60 条自动禁言
-define(MUTE_DURATION_MS, 300000). % 禁言持续 5 分钟 (毫秒)
-define(WINDOW_MS, 60000).         % 统计窗口 1 分钟 (毫秒)

%% ETS 表名
-define(MSG_RATE_TAB, msg_rate_counter).
-define(MSG_MUTE_TAB, msg_rate_muted).

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
            Now = erlang:system_time(millisecond),
            Count = record_and_count(Uid, Now),
            if
                Count > ?MUTE_THRESHOLD ->
                    %% 超过禁言阈值，自动禁言
                    do_mute(Uid, Now),
                    ?WARN_LOG("User ~p auto-muted: ~p msgs/min exceeded threshold ~p",
                              [Uid, Count, ?MUTE_THRESHOLD]),
                    {error, muted};
                Count > ?WARN_THRESHOLD ->
                    %% 超过警告阈值
                    ?WARN_LOG("User ~p msg rate warning: ~p msgs/min exceeded threshold ~p",
                              [Uid, Count, ?WARN_THRESHOLD]),
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
-spec init_table() -> ok.
init_table() ->
    create_table_if_not_exists(?MSG_RATE_TAB, [set, public, named_table,
                                                {read_concurrency, true},
                                                {write_concurrency, true}]),
    create_table_if_not_exists(?MSG_MUTE_TAB, [set, public, named_table,
                                                {read_concurrency, true},
                                                {write_concurrency, true}]),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 记录消息并返回当前窗口内的消息数
%% @private
-spec record_and_count(integer(), integer()) -> integer().
record_and_count(Uid, Now) ->
    WindowStart = Now - ?WINDOW_MS,
    case ets:lookup(?MSG_RATE_TAB, Uid) of
        [{Uid, Timestamps}] ->
            %% 过滤掉过期的时间戳
            ValidTimestamps = [T || T <- Timestamps, T > WindowStart],
            NewTimestamps = [Now | ValidTimestamps],
            ets:insert(?MSG_RATE_TAB, {Uid, NewTimestamps}),
            length(NewTimestamps);
        [] ->
            ets:insert(?MSG_RATE_TAB, {Uid, [Now]}),
            1
    end.

%% @doc 执行禁言操作
%% @private
-spec do_mute(integer(), integer()) -> true.
do_mute(Uid, Now) ->
    MuteUntil = Now + ?MUTE_DURATION_MS,
    ets:insert(?MSG_MUTE_TAB, {Uid, MuteUntil}).

%% @doc 确保 ETS 表存在
%% @private
-spec ensure_tables() -> ok.
ensure_tables() ->
    create_table_if_not_exists(?MSG_RATE_TAB, [set, public, named_table,
                                                {read_concurrency, true},
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
