-module(msg_store_worker).
%%%-------------------------------------------------------------------
%%% @doc  消息写入批量处理器（gen_statem）
%%%
%%% 从 staging 表批量抢占消息并写入正式表。
%%%
%%% == 与 msg_store_ds 的协作 ==
%%% ```
%%% 发送消息 → msg_store_ds:stage/7 (备份)
%%%         → msg_store_ds:enqueue/3 (触发 Worker)
%%%         → msg_store_worker 批量写入正式表
%%%         → msg_store_ds:unstage/1 (标记已处理)
%%% ```
%%%
%%% == 批量处理策略 ==
%%% - 每批 100 条
%%% - 触发方式：1 秒定时器或 kick 消息
%%% - 使用 FOR UPDATE SKIP LOCKED 抢占（分布式安全）
%%%
%%% == 重试机制 ==
%%% - 指数退避：1s → 2s → 4s → 8s → 16s → 32s → 60s（最大）
%%% - 失败后设置 available_at 延迟重试
%%%
%%% == 状态机 ==
%%% - idle: 等待触发
%%% - draining: 批量处理中
%%% @end
%%%-------------------------------------------------------------------


%% ==================== API ====================

-export([start_link/0]).

%% ==================== Callbacks ====================

-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, draining/3]).

-include("log.hrl").

%% ==================== Macros & Records ====================

-define(SERVER, msg_store_worker).
-define(BATCH_SIZE, 100).      % 每批处理的记录数
-define(BATCH_INTERVAL, 1000).  % 定时触发间隔（毫秒）
-define(LEASE_SECONDS, 30).     % 抢占记录的租约时间（秒）
-define(MAX_BACKOFF_SECONDS, 60). % 最大重试延迟（秒）

-record(state, {
    tick_timer = undefined     % 定时器引用
}).

%% ==================== API Functions ====================

%%-------------------------------------------------------------------
%% @doc  启动批量处理器
%%
%% 启动 gen_statem 进程，初始状态为 idle。
%% @see init/1
%% @end
%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, msg_store_worker, [], []).

%% ==================== Callbacks ====================

%% @private
init([]) ->
    % 确保数据库表存在（自动创建）
    ok = msg_store_repo:ensure_table_exists(),
    ok = ?INFO_LOG("msg_store_worker started successfully"),
    {ok, idle, start_tick(#state{})}.

callback_mode() ->
    state_functions.

idle({cast, kick}, _Content, State) ->
    {next_state, draining, cancel_tick(State), [{next_event, internal, drain}]};
idle(info, tick, State) ->
    {next_state, draining, cancel_tick(State), [{next_event, internal, drain}]};
idle(_EventType, _Event, State) ->
    {keep_state, State}.

draining(internal, drain, State) ->
    case claim_and_process_batch() of
        {ok, 0} ->
            {next_state, idle, start_tick(State)};
        {ok, N} when N >= ?BATCH_SIZE ->
            {keep_state, State, [{next_event, internal, drain}]};
        {ok, _N} ->
            {next_state, idle, start_tick(State)};
        {error, Reason} ->
            ok = ?ERROR_LOG([msg_store_worker, drain_error, Reason]),
            {next_state, idle, start_tick(State)}
    end;
draining({cast, kick}, _Content, State) ->
    {keep_state, State};
draining(info, tick, State) ->
    {keep_state, State};
draining(_EventType, _Event, State) ->
    {keep_state, State}.

terminate(_Reason, _StateName, State) ->
    _ = cancel_tick(State),
    ok = ?INFO_LOG("msg_store_worker terminated"),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ==================== Internal Functions ====================

start_tick(State) ->
    TimerRef = erlang:send_after(?BATCH_INTERVAL, self(), tick),
    State#state{tick_timer = TimerRef}.

cancel_tick(State = #state{tick_timer = undefined}) ->
    State;
cancel_tick(State = #state{tick_timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    State#state{tick_timer = undefined}.

claim_and_process_batch() ->
    case msg_store_repo:claim_pending(?BATCH_SIZE, ?LEASE_SECONDS) of
        {ok, []} ->
            {ok, 0};
        {ok, Rows} ->
            _ = [process_row(Row) || Row <- Rows],
            {ok, length(Rows)};
        {error, Reason} ->
            {error, Reason}
    end.

process_row(Row) ->
    TypeBin = maps:get(<<"type">>, Row),
    MsgId = maps:get(<<"msg_id">>, Row),
    RetryCount = maps:get(<<"retry_count">>, Row, 0),
    TypeAtom = msg_type_atom(TypeBin),
    case do_write(TypeAtom, Row) of
        ok ->
            msg_store_ds:unstage(MsgId),
            ok = ?DEBUG_LOG([msg_store_worker, write_success, TypeAtom, MsgId]);
        {error, Reason} ->
            BackoffSeconds = backoff_seconds(RetryCount),
            ErrorMsg = list_to_binary(io_lib:format("~p", [Reason])),
            _ = msg_store_repo:mark_failed(TypeBin, MsgId, ErrorMsg, BackoffSeconds),
            ok = ?ERROR_LOG([msg_store_worker, write_error, TypeAtom, MsgId, Reason])
    end.

%% v2.0: 使用 staging 表的独立字段，避免重复解析 payload
do_write(c2c, Row) ->
    PayloadBin = maps:get(<<"payload">>, Row),
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row, 0),
    ServerTs = maps:get(<<"server_ts">>, Row, 0),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, <<>>),
    msg_c2c_ds:write_msg(CreatedAt, MsgId, PayloadBin, FromId, ToId, ServerTs, MsgType, E2EE);

do_write(c2g, Row) ->
    PayloadBin = maps:get(<<"payload">>, Row),
    FromId = maps:get(<<"from_id">>, Row),
    ToIdList = maps:get(<<"to_id_list">>, Row, []),
    CreatedAt = maps:get(<<"created_at">>, Row, 0),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, <<>>),
    %% C2G 需要 Gid，从 payload 解析
    PayloadMap = jsone:decode(PayloadBin, [{object_format, map}]),
    GidEnc = maps:get(<<"to">>, PayloadMap),
    Gid = elib_hashids:decode(GidEnc),
    msg_c2g_repo:write_msg(CreatedAt, MsgId, PayloadBin, FromId, ToIdList, Gid, MsgType, E2EE);

do_write(s2c, Row) ->
    PayloadBin = maps:get(<<"payload">>, Row),
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row, elib_dt:now()),
    ServerTs = maps:get(<<"server_ts">>, Row, CreatedAt),
    MsgId = maps:get(<<"msg_id">>, Row),
    Action = maps:get(<<"action">>, Row, <<>>),
    msg_s2c_ds:write_msg(CreatedAt, MsgId, PayloadBin, FromId, ToId, ServerTs, Action, <<>>);

do_write(c2s, Row) ->
    PayloadBin = maps:get(<<"payload">>, Row),
    FromId = maps:get(<<"from_id">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row),
    PayloadMap = jsone:decode(PayloadBin, [{object_format, map}]),
    Status = maps:get(<<"status">>, PayloadMap, 12),
    TopicId = maps:get(<<"topic_id">>, PayloadMap, 0),
    ToIdStr = maps:get(<<"to_id_str">>, PayloadMap, <<>>),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgData = #{
        status => Status,
        topic_id => TopicId,
        from_id => FromId,
        to_id => ToIdStr,
        msg_id => MsgId,
        payload => PayloadBin,
        created_at => CreatedAt
    },
    msg_c2s_ds:write_msg(MsgId, MsgData);

do_write(Unknown, Row) ->
    {error, {unknown_msg_type, Unknown, maps:get(<<"msg_id">>, Row)}}.

msg_type_atom(<<"c2c">>) -> c2c;
msg_type_atom(<<"c2g">>) -> c2g;
msg_type_atom(<<"s2c">>) -> s2c;
msg_type_atom(<<"c2s">>) -> c2s;
msg_type_atom(Bin) when is_binary(Bin) ->
    Bin.

backoff_seconds(RetryCount) when is_integer(RetryCount), RetryCount >= 0 ->
    Pow = case RetryCount > 10 of
        true -> 10;
        false -> RetryCount
    end,
    Seconds0 = 1 bsl Pow,
    case Seconds0 > ?MAX_BACKOFF_SECONDS of
        true -> ?MAX_BACKOFF_SECONDS;
        false -> Seconds0
    end;
backoff_seconds(_Other) ->
    1.
