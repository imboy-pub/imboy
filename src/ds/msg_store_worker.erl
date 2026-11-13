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
%%%         → msg_store_ds:unstage/2 (标记已处理)
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

-ifdef(TEST).
-export([do_write/2, process_row/1, terminal_write_reason/1]).
-export([complete_staged/1, renew_row_lease/1]).
-endif.

-include("log.hrl").

%% ==================== Macros & Records ====================

-define(SERVER, msg_store_worker).
% 每批处理的记录数
-define(BATCH_SIZE, 100).
% 定时触发间隔（毫秒）
-define(BATCH_INTERVAL, 1000).
% 抢占记录的租约时间（秒）
-define(LEASE_SECONDS, 30).
% 最大重试延迟（秒）
-define(MAX_BACKOFF_SECONDS, 60).
% staging 采样指标默认周期（毫秒）；app env msg_store_worker_sample_interval_ms 覆盖
-define(SAMPLE_INTERVAL_MS, 10000).
% 采样查询 statement_timeout 默认（毫秒）；app env msg_store_worker_stats_timeout_ms 覆盖
-define(STATS_TIMEOUT_MS, 2000).

-record(state, {
    % 定时器引用
    tick_timer = undefined,
    % 上次 staging 指标采样的 monotonic 毫秒时间；undefined 表示从未采样
    % （首次 drain 必采，不能用 0 当哨兵：peer/测试 VM 的 monotonic 时钟
    % 可能从接近 0 的小值起步，`Now - 0 < 间隔` 会把首次采样吞掉）
    last_sample_ms = undefined
}).

%% ==================== Types ====================

-type state() :: #state{}.
-type state_name() :: idle | draining.

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
-spec init(term()) -> {ok, state_name(), state()}.
init([]) ->
    % 表结构由 msg_store_ds 在监督树启动时统一初始化，worker 不重复执行 DDL
    _ = ?INFO_LOG("msg_store_worker started successfully"),
    {ok, idle, maybe_start_tick(tick_interval_ms(), #state{})}.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

%% @private
-spec idle(gen_statem:event_type(), term(), state()) ->
    {next_state, state_name(), state()}
    | {keep_state, state()}.
%% gen_statem state_functions 回调的实参形态是 idle(EventType原子, Content,
%% State)——kick 子句头此前误写为 idle({cast, kick}, _Content, State)，第一
%% 参数模式是元组、永远匹配不到原子 cast，kick 100% 落入 catch-all 被静默
%% 吞掉（probe6l 铁证：P ! tick 走 idle(info,tick) 转运成功，cast kick
%% consume 记录 {consume,{cast,kick},idle,idle} 状态不变）。
idle(cast, kick, State) ->
    {next_state, draining, schedule_drain(cancel_tick(State))};
idle(info, tick, State) ->
    {next_state, draining, schedule_drain(cancel_tick(State))};
idle(_EventType, _Event, State) ->
    {keep_state, State}.

%% @private
-spec draining(gen_statem:event_type(), term(), state()) ->
    {next_state, idle, state()}
    | {keep_state, state(), [gen_statem:action()]}
    | {keep_state, state()}.
draining(info, drain, State) ->
    case claim_and_process_batch() of
        {ok, 0} ->
            {next_state, idle, maybe_start_tick(tick_interval_ms(), finish_drain(State))};
        {ok, N} when N >= ?BATCH_SIZE ->
            {keep_state, finish_drain(schedule_drain(State))};
        {ok, _N} ->
            {next_state, idle, maybe_start_tick(tick_interval_ms(), finish_drain(State))};
        {error, Reason} ->
            _ = ?ERROR_LOG([msg_store_worker, drain_error, Reason]),
            {next_state, idle, maybe_start_tick(tick_interval_ms(), finish_drain(State))}
    end;
draining(cast, kick, State) ->
    %% 已有唯一 drain 排在邮箱尾，覆盖此前落库的 kick；批中到达的 kick
    %% 在本批返回后才处理，若已 idle 会重新排空，不能丢失唤醒。
    {keep_state, State};
draining(info, tick, State) ->
    {keep_state, State};
draining(_EventType, _Event, State) ->
    {keep_state, State}.

%% @private
-spec terminate(term(), state_name(), state()) -> ok.
terminate(_Reason, _StateName, State) ->
    _ = cancel_tick(State),
    _ = ?INFO_LOG("msg_store_worker terminated"),
    ok.

%% @private
-spec code_change(term(), state_name(), state(), term()) -> {ok, state_name(), state()}.
%% record 扩字段后的热升级：旧版单字段 {state, Timer} 必须升级到新 tuple，
%% tick_timer 语义原样保留（该 Timer 是既存 send_after 引用，不能丢，否则
%% 旧 timer 泄漏且无法 cancel）。未知形态原样返回（由 gen_statem 层兜底）。
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, upgrade_state(State)}.

upgrade_state(#state{} = State) ->
    State;
upgrade_state({state, Timer}) ->
    #state{tick_timer = Timer, last_sample_ms = undefined};
upgrade_state(State) ->
    State.

%% ==================== Internal Functions ====================

%% 邮箱事件让持续满批也能处理系统消息，并合并排空前的重复唤醒。
schedule_drain(State) ->
    self() ! drain,
    State.

%% tick 间隔可经 app env 覆盖；<=0 或非整数表示禁用周期 tick（纯 kick
%% 驱动）。测试 VM（eunit_runner do_boot）设 0：每秒 tick 的异步 drain
%% 会在其他套件的 meck 窗口内调用 elib_pg 池化版（如 mark_processed 走
%% query/2），污染按全局调用计数断言的白盒用例（adm_message_handler
%% 审计 fails-closed 用例实证：num_calls(elib_pg, query, 2) 期望 0）。
%% 正常发送路径 stage 后 enqueue 必发 kick，kick 驱动足以支撑集成套件
%% 的真实异步转正；生产不设该 env，保持 1s 兜底 tick。
tick_interval_ms() ->
    application:get_env(imboy, msg_store_worker_tick_ms, ?BATCH_INTERVAL).

maybe_start_tick(Ms, State) when is_integer(Ms), Ms > 0 ->
    start_tick(Ms, State);
maybe_start_tick(_Disabled, State) ->
    State.

start_tick(Ms, State) ->
    TimerRef = erlang:send_after(Ms, self(), tick),
    State#state{tick_timer = TimerRef}.

cancel_tick(State = #state{tick_timer = undefined}) ->
    State;
cancel_tick(State = #state{tick_timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    State#state{tick_timer = undefined}.

claim_and_process_batch() ->
    %% 防崩包裹：elib_pg 被测试 meck 的窗口内，worker 的 tick/kick 仍会
    %% 触发 drain，with_tx 返回 meck 桩值 → case_clause 崩溃 → permanent
    %% 子进程每秒重启风暴。包住后仅记 drain_error，窗口过后自然恢复。
    StartedUs = erlang:monotonic_time(microsecond),
    Result =
        try
            T0 = erlang:monotonic_time(microsecond),
            Claim = msg_store_repo:claim_pending(?BATCH_SIZE, ?LEASE_SECONDS),
            T1 = erlang:monotonic_time(microsecond),
            _ = elib_metric:record(
                imboy_msg_store_claim_seconds, elapsed_seconds(T0, T1)
            ),
            case Claim of
                {ok, []} ->
                    {ok, 0};
                {ok, Rows} ->
                    _ = [process_row(Row) || Row <- Rows],
                    {ok, length(Rows)};
                {error, Reason} ->
                    {error, Reason}
            end
        catch
            %% 崩溃原因必须进 drain_error 日志：只记 drain_crash 原子会让
            %% 防崩包裹变成黑洞（自审 R3：观测不到根因的兜底不是兜底）。
            %% 另起命名：Reason 在 try 体内已被上方 case 分支绑定。
            CrashClass:CrashReason ->
                {error, {drain_crash, CrashClass, CrashReason}}
        end,
    _ = elib_metric:record(
        imboy_msg_store_batch_seconds,
        elapsed_seconds(StartedUs, erlang:monotonic_time(microsecond))
    ),
    Result.

%% 每批 drain 收尾：记录本进程 mailbox 长度 gauge，并按周期节流采样
%% staging 积压指标（首次必采；PG 不可达时记 -1，不假装 0）。
finish_drain(State) ->
    _ = record_mailbox_gauge(),
    maybe_sample_staging(State).

record_mailbox_gauge() ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Len} when is_integer(Len) ->
            _ = elib_metric:set_gauge(imboy_msg_store_mailbox, Len),
            ok;
        _ ->
            ok
    end.

sample_interval_ms() ->
    application:get_env(imboy, msg_store_worker_sample_interval_ms, ?SAMPLE_INTERVAL_MS).

maybe_sample_staging(#state{last_sample_ms = undefined} = State) ->
    sample_staging_metrics(),
    State#state{last_sample_ms = erlang:monotonic_time(millisecond)};
maybe_sample_staging(State = #state{last_sample_ms = Last}) when is_integer(Last) ->
    Now = erlang:monotonic_time(millisecond),
    case Now - Last >= sample_interval_ms() of
        true ->
            sample_staging_metrics(),
            State#state{last_sample_ms = Now};
        false ->
            State
    end.

%% 采样在有界 statement_timeout 的事务内执行；任何失败（连接不可达、
%% 查询错误、超时、崩溃）都收敛为 -1 两个 gauge，绝不把失败记成 0。
sample_staging_metrics() ->
    TimeoutMs = application:get_env(
        imboy, msg_store_worker_stats_timeout_ms, ?STATS_TIMEOUT_MS
    ),
    TimeoutBin = integer_to_binary(TimeoutMs),
    Result =
        try
            elib_pg:with_tx(fun(Conn) ->
                case
                    elib_pg:query(
                        Conn,
                        <<"SELECT set_config('statement_timeout', '", TimeoutBin/binary,
                            "', true)">>,
                        []
                    )
                of
                    {ok, _} ->
                        msg_store_repo:get_staging_stats(Conn);
                    Other ->
                        {error, {stats_timeout_setup, Other}}
                end
            end)
        catch
            Class:Reason ->
                {error, {stats_crash, Class, Reason}}
        end,
    {Pending, Oldest} = staging_gauges(Result),
    _ = elib_metric:set_gauge(imboy_msg_staging_pending, Pending),
    _ = elib_metric:set_gauge(imboy_msg_staging_oldest_seconds, Oldest),
    ok.

staging_gauges({ok, [Row | _]}) when is_map(Row) ->
    {
        gauge_int(maps:get(<<"pending">>, Row, null)),
        gauge_int(maps:get(<<"oldest_pending_seconds">>, Row, null))
    };
staging_gauges(_) ->
    {-1, -1}.

gauge_int(I) when is_integer(I) ->
    I;
gauge_int(F) when is_float(F) ->
    trunc(F);
gauge_int(B) when is_binary(B) ->
    try
        binary_to_integer(B)
    catch
        _:_ -> -1
    end;
gauge_int(_) ->
    -1.

elapsed_seconds(FromUs, ToUs) when is_integer(FromUs), is_integer(ToUs) ->
    (ToUs - FromUs) / 1_000_000.

%% ==================== 租约 fencing ====================

%% 处理前逐行 CAS 续租：token 仍是本 claim 写入的 available_at 原始精度
%% 文本才续期；续租成功返回**新 token**（续租已改写 available_at，旧 token
%% 随之失效），后续完成/失败/终态写回必须用新 token。0 行（过期被重领 /
%% 已终态）跳过该行，本 worker 不得再对它做任何写回。无 token 的行
%% （旧调用方 / 降级 claim）走旧路径。
renew_row_lease(#{<<"id">> := Id, <<"lease_token">> := Token} = Row) ->
    case msg_store_repo:renew_lease(Id, Token, ?LEASE_SECONDS) of
        {ok, NewToken} when is_binary(NewToken) ->
            {ok, Row#{<<"lease_token">> => NewToken}};
        {ok, 0} ->
            _ = ?INFO_LOG([msg_store_worker, lease_lost_skip, Id]),
            skip;
        {error, Reason} ->
            %% 续租失败（DB 错误）时归属未知：保守跳过，lease 到期后可被
            %% 重新 claim，不产生滞留；不冒领写回。
            _ = ?ERROR_LOG([msg_store_worker, lease_renew_error, Id, Reason]),
            skip
    end;
renew_row_lease(RowWithoutToken) ->
    {ok, RowWithoutToken}.

process_row(Row) ->
    case renew_row_lease(Row) of
        {ok, RenewedRow} ->
            process_claimed_row(RenewedRow);
        skip ->
            ok
    end.

process_claimed_row(Row) ->
    TypeBin = maps:get(<<"type">>, Row),
    MsgId = maps:get(<<"msg_id">>, Row),
    RetryCount = maps:get(<<"retry_count">>, Row, 0),
    TypeAtom = msg_type_atom(TypeBin),
    WriteResult =
        case write_and_archive(TypeAtom, Row) of
            Written when Written =:= ok; Written =:= {error, conflict_no_insert} ->
                complete_staged(Row);
            Error ->
                Error
        end,
    case WriteResult of
        ok ->
            _ = ?DEBUG_LOG([msg_store_worker, write_success, TypeAtom, MsgId]);
        {error, {stale_lease, TypeBin, MsgId}} ->
            %% mark_processed 被 token CAS 拒绝（0 行）：行已被新 owner 重领，
            %% 本 worker 已做的正式表写入有幂等键兜底；绝不 mark_failed、
            %% 绝不触发 ACK 清理（complete_staged 事务已回滚）。
            _ = ?INFO_LOG([msg_store_worker, write_stale_skip, TypeBin, MsgId]);
        {error, Reason} ->
            ErrorMsg = list_to_binary(io_lib:format("~p", [Reason])),
            case terminal_write_reason(Reason) of
                true ->
                    case mark_terminal_row(Row, TypeBin, MsgId, ErrorMsg) of
                        {ok, 1} ->
                            ok;
                        {ok, 0} ->
                            %% stale owner 的终态写回被 CAS 拒绝：新 owner 接管。
                            _ = ?INFO_LOG([
                                msg_store_worker, mark_terminal_stale, TypeBin, MsgId
                            ]);
                        {error, MarkReason} ->
                            ?ERROR_LOG([msg_mark_terminal_error, TypeBin, MsgId, MarkReason])
                    end,
                    _ = ?ERROR_LOG([
                        msg_store_worker, write_terminal_failure, TypeAtom, MsgId, Reason
                    ]);
                false ->
                    BackoffSeconds = backoff_seconds(RetryCount),
                    case mark_failed_row(Row, TypeBin, MsgId, ErrorMsg, BackoffSeconds) of
                        {ok, 1} ->
                            ok;
                        {ok, 0} ->
                            _ = ?INFO_LOG([
                                msg_store_worker, mark_failed_stale, TypeBin, MsgId
                            ]);
                        {error, MarkReason} ->
                            ?ERROR_LOG([msg_mark_failed_error, TypeBin, MsgId, MarkReason])
                    end,
                    _ = ?ERROR_LOG([msg_store_worker, write_error, TypeAtom, MsgId, Reason])
            end
    end.

mark_terminal_row(#{<<"lease_token">> := Token}, Type, MsgId, ErrorMsg) ->
    msg_store_repo:mark_terminal(Type, MsgId, ErrorMsg, Token);
mark_terminal_row(_, Type, MsgId, ErrorMsg) ->
    normalize_mark(msg_store_repo:mark_terminal(Type, MsgId, ErrorMsg)).

mark_failed_row(#{<<"lease_token">> := Token}, Type, MsgId, ErrorMsg, BackoffSeconds) ->
    msg_store_repo:mark_failed(Type, MsgId, ErrorMsg, BackoffSeconds, Token);
mark_failed_row(_, Type, MsgId, ErrorMsg, BackoffSeconds) ->
    normalize_mark(msg_store_repo:mark_failed(Type, MsgId, ErrorMsg, BackoffSeconds)).

%% 旧 arity 接口（mark_terminal/3、mark_failed/4）返回 {ok, Count} ——
%% 两条 UPDATE 均无 RETURNING，elib_pg:execute 契约下恒为二元 {ok, Count}
%% （三元 {ok, Count, Rows} 仅 RETURNING 语句可达）；token 版走
%% execute_count 同为二元。归一仅透传，便于上层按 0/1 行判 stale。
normalize_mark({ok, Count}) when is_integer(Count) ->
    {ok, Count};
normalize_mark({error, Reason}) ->
    {error, Reason}.

%% 完成与 ACK 清理同事务；失败回滚使 staging 仍可被 worker 重试。
%% T06 fencing：带 token 的行先做 CAS 完成（available_at 必须仍是本
%% claim 写入的值）。0 行 = stale：整事务回滚并返回 {error,{stale_lease,
%% ...}}，绝不能继续 reconcile/删除 delivered（stale worker 的完成不得
%% 触发 ACK 清理）；无 token 行保持旧行为。
complete_staged(Row) ->
    Type = maps:get(<<"type">>, Row),
    MsgId = maps:get(<<"msg_id">>, Row),
    LeaseToken = maps:get(<<"lease_token">>, Row, undefined),
    Result = elib_pg:with_tx(fun(Conn) ->
        MarkResult =
            case LeaseToken of
                undefined ->
                    msg_store_repo:mark_processed(Conn, Type, MsgId);
                Token ->
                    msg_store_repo:mark_processed(Conn, Type, MsgId, Token)
            end,
        case MarkResult of
            {ok, Count} when Count >= 1 ->
                ok;
            {ok, 0} ->
                throw({rollback, {stale_lease, Type, MsgId}});
            {ok, _Unexpected} ->
                ok;
            {error, Reason} ->
                throw({rollback, {mark_processed_failed, Reason}})
        end,
        case Type of
            <<"c2c">> ->
                case c2c_action_receipt_repo:reconcile_consumed_tx(Conn, MsgId) of
                    ok -> ok;
                    {error, Reason2} -> throw({rollback, {action_reconcile_failed, Reason2}})
                end;
            _ ->
                ok
        end,
        case Type of
            K when K =:= <<"c2c">>; K =:= <<"s2c">> ->
                msg_delivery_repo:delete_delivered_batch(
                    Conn,
                    K,
                    [MsgId],
                    maps:get(<<"to_id">>, Row),
                    application:get_env(imboy, msg_delivery_active_days, 30)
                );
            _ ->
                ok
        end
    end),
    case Result of
        {rollback, Reason} -> {error, Reason};
        Other -> Other
    end.

write_and_archive(Type, Row) ->
    case safe_write(Type, Row) of
        ok ->
            maybe_archive(Row);
        {error, conflict_no_insert} ->
            case maybe_archive(Row) of
                ok -> {error, conflict_no_insert};
                ArchiveError -> ArchiveError
            end;
        Error ->
            Error
    end.

safe_write(Type, Row) ->
    try
        do_write(Type, Row)
    catch
        error:{badkey, Key} ->
            {error, {invalid_staging_payload, missing_field, Key}};
        error:{badmap, _} ->
            {error, {invalid_staging_payload, expected_object}};
        %% 保留原始 Reason（此前仅留 Class，写失败只剩 write_exception 无从排障）
        Class:Reason0 ->
            {error, {write_exception, Class, Reason0}}
    end.

-spec terminal_write_reason(term()) -> boolean().
terminal_write_reason({invalid_staging_payload, _, _}) ->
    true;
terminal_write_reason({invalid_staging_payload, _}) ->
    true;
terminal_write_reason(no_recipients) ->
    true;
terminal_write_reason(c2c_sequence_missing) ->
    true;
terminal_write_reason(c2c_sequence_conflict) ->
    true;
terminal_write_reason(msg_id_conflict) ->
    true;
terminal_write_reason({archive_failed, Reason}) ->
    terminal_write_reason(Reason);
terminal_write_reason({error, _, <<"23514">>, check_violation, _, Extra}) ->
    lists:member(proplists:get_value(constraint_name, Extra), [
        <<"c2c_sequence_required">>,
        <<"c2c_order_conflict">>,
        <<"c2c_order_immutable">>,
        <<"c2c_conversation_mismatch">>
    ]);
terminal_write_reason(c2g_conv_seq_missing) ->
    true;
terminal_write_reason(c2g_gid_missing) ->
    true;
terminal_write_reason({unknown_msg_type, _, _}) ->
    true;
terminal_write_reason(_) ->
    false.

-spec to_int_or_null(term()) -> integer() | null.
to_int_or_null(V) when is_integer(V) ->
    V;
to_int_or_null(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I when is_integer(I) -> I
    catch
        _:_ -> null
    end;
to_int_or_null(_) ->
    null.

%%-------------------------------------------------------------------
%% @private 按配置开关决定是否归档到永久存储
%%
%% C2C 归档失败保持 staging 重试，序号与消息不能因归档失败被清理。
%% C2C archive failure retains staging until a retry persists the archive.
%%
%% 配置方式（sys.config）：
%%   {imboy, [{msg_archive_enabled, true}]}
%% @end
%%-------------------------------------------------------------------
maybe_archive(Row) ->
    case application:get_env(imboy, msg_archive_enabled, false) of
        true ->
            case msg_archive_repo:archive(Row) of
                ok ->
                    ok;
                {error, Reason} ->
                    MsgId = maps:get(<<"msg_id">>, Row, <<>>),
                    _ = ?ERROR_LOG([msg_store_worker, archive_error, MsgId, Reason]),
                    case maps:get(<<"type">>, Row) of
                        <<"c2c">> -> {error, {archive_failed, Reason}};
                        <<"c2g">> -> {error, {archive_failed, Reason}};
                        _ -> ok
                    end
            end;
        _ ->
            ok
    end.

%% v2.0: 使用 staging 表的独立字段，避免重复解析 payload
do_write(c2c, Row) ->
    PayloadBin = unwrap_staging_payload(maps:get(<<"payload">>, Row)),
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row, 0),
    ServerTs = maps:get(<<"server_ts">>, Row, 0),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, null),
    %% A2-a：把 staging 行里的发送者设备标识搬进正式表；离线拉取时
    %% PFv3 context binding 第 6 项要用（漏搬 = 静默丢字段，不会报错）
    SenderDid = maps:get(<<"sender_did">>, Row, null),
    %% R03/D04：把首次持久接受时间从 staging 行搬进正式表（旧行缺失为 null）。
    AcceptedAt = maps:get(<<"accepted_at">>, Row, null),
    %% R05-A：自毁绝对期限同样从 staging 行搬运（接受事务内落库）；
    %% 旧行/无自毁缺失为 null（不自毁）。
    ExpireAt = maps:get(<<"expire_at">>, Row, null),
    Res = msg_c2c_ds:write_msg(
        CreatedAt,
        MsgId,
        PayloadBin,
        FromId,
        ToId,
        ServerTs,
        MsgType,
        E2EE,
        SenderDid,
        AcceptedAt,
        ExpireAt
    ),
    Res;
do_write(c2g, Row) ->
    PayloadBin = unwrap_staging_payload(maps:get(<<"payload">>, Row)),
    FromId = maps:get(<<"from_id">>, Row),
    ToIdList =
        case maps:get(<<"to_id_list">>, Row, []) of
            null -> [];
            L when is_list(L) -> L;
            _ -> []
        end,
    CreatedAt = maps:get(<<"created_at">>, Row, 0),
    MsgId = maps:get(<<"msg_id">>, Row),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, null),
    SenderDid = maps:get(<<"sender_did">>, Row, null),
    ConvSeq = maps:get(<<"conv_seq">>, Row, null),
    %% C2G 需要 Gid，缺失或非法形态进入终态失败。
    PayloadMap = jsone:decode(PayloadBin, [{object_format, map}]),
    Gid = to_int_or_null(maps:get(<<"to">>, PayloadMap, null)),
    case {ConvSeq, Gid, ToIdList} of
        {Seq, G, [_ | _]} when is_integer(Seq), Seq >= 1, is_integer(G), G > 0 ->
            msg_c2g_repo:write_accepted_msg(
                CreatedAt,
                MsgId,
                jsone:encode(maps:get(<<"payload">>, PayloadMap), [native_utf8]),
                FromId,
                ToIdList,
                G,
                MsgType,
                E2EE,
                %% R05-A：staging 行的自毁绝对期限（接受事务内落库；缺失 null）。
                maps:get(<<"expire_at">>, Row, null),
                Seq,
                SenderDid,
                %% R03/D04：staging 行的首次接受时间（缺失为 null，旧行兼容）。
                maps:get(<<"accepted_at">>, Row, null)
            );
        {Seq, G, []} when is_integer(Seq), Seq >= 1, is_integer(G), G > 0 ->
            {error, no_recipients};
        {Seq, _, _} when not (is_integer(Seq) andalso Seq >= 1) ->
            {error, c2g_conv_seq_missing};
        _ ->
            {error, c2g_gid_missing}
    end;
do_write(s2c, Row) ->
    PayloadBin = unwrap_staging_payload(maps:get(<<"payload">>, Row)),
    FromId = maps:get(<<"from_id">>, Row),
    ToId = maps:get(<<"to_id">>, Row),
    CreatedAt = maps:get(<<"created_at">>, Row, elib_dt:now()),
    ServerTs = maps:get(<<"server_ts">>, Row, CreatedAt),
    MsgId = maps:get(<<"msg_id">>, Row),
    Action = maps:get(<<"action">>, Row, <<>>),
    MsgType = maps:get(<<"msg_type">>, Row, <<>>),
    E2EE = maps:get(<<"e2ee">>, Row, null),
    case
        msg_s2c_ds:write_msg(
            maps:get(<<"id">>, Row),
            CreatedAt,
            MsgId,
            PayloadBin,
            FromId,
            ToId,
            ServerTs,
            Action,
            MsgType,
            E2EE
        )
    of
        {ok, _} -> ok;
        Other -> Other
    end;
do_write(c2s, Row) ->
    PayloadBin = unwrap_staging_payload(maps:get(<<"payload">>, Row)),
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

msg_type_atom(<<"c2c">>) ->
    c2c;
msg_type_atom(<<"c2g">>) ->
    c2g;
msg_type_atom(<<"s2c">>) ->
    s2c;
msg_type_atom(<<"c2s">>) ->
    c2s;
msg_type_atom(Bin) when is_binary(Bin) ->
    Bin.

backoff_seconds(RetryCount) when is_integer(RetryCount), RetryCount >= 0 ->
    Pow =
        case RetryCount > 10 of
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

%% @private
%% @doc 还原 staging.payload 中可能被 msg_store_repo:msg_store_payload_to_jsonb
%% 包装过的裸 binary（E2EE 密文存为 JSON 字符串 `"\"base64...\""`）。
%% 若为 JSON object（{...}）或 JSON 数组（[...]）等其他形式，原样返回。
%% 客户端依赖 payload 的原始 binary 与 e2ee 元数据中的 nonce 严格匹配，
%% 错误地把 JSON 引号留在前面会导致客户端报
%% "Nonce mismatch between ciphertext and e2ee metadata"。
-spec unwrap_staging_payload(term()) -> binary() | term().
unwrap_staging_payload(<<"\"", _/binary>> = Bin) ->
    try jsone:decode(Bin) of
        Str when is_binary(Str) -> Str;
        _ -> Bin
    catch
        _:_ -> Bin
    end;
unwrap_staging_payload(Other) ->
    Other.
