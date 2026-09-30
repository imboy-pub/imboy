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

-ifdef(TEST).
-export([do_write/2, process_row/1, terminal_write_reason/1]).
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

-record(state, {
    % 定时器引用
    tick_timer = undefined
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
    {next_state, state_name(), state(), [gen_statem:transition_action()]}
    | {keep_state, state()}.
%% gen_statem state_functions 回调的实参形态是 idle(EventType原子, Content,
%% State)——kick 子句头此前误写为 idle({cast, kick}, _Content, State)，第一
%% 参数模式是元组、永远匹配不到原子 cast，kick 100% 落入 catch-all 被静默
%% 吞掉（probe6l 铁证：P ! tick 走 idle(info,tick) 转运成功，cast kick
%% consume 记录 {consume,{cast,kick},idle,idle} 状态不变）。
idle(cast, kick, State) ->
    {next_state, draining, cancel_tick(State), [{next_event, internal, drain}]};
idle(info, tick, State) ->
    {next_state, draining, cancel_tick(State), [{next_event, internal, drain}]};
idle(_EventType, _Event, State) ->
    {keep_state, State}.

%% @private
-spec draining(gen_statem:event_type(), term(), state()) ->
    {next_state, state_name(), state(), [gen_statem:transition_action()]}
    | {keep_state, state()}.
draining(internal, drain, State) ->
    case claim_and_process_batch() of
        {ok, 0} ->
            {next_state, idle, maybe_start_tick(tick_interval_ms(), State)};
        {ok, N} when N >= ?BATCH_SIZE ->
            {keep_state, State, [{next_event, internal, drain}]};
        {ok, _N} ->
            {next_state, idle, maybe_start_tick(tick_interval_ms(), State)};
        {error, Reason} ->
            _ = ?ERROR_LOG([msg_store_worker, drain_error, Reason]),
            {next_state, idle, maybe_start_tick(tick_interval_ms(), State)}
    end;
draining(cast, kick, State) ->
    %% 批处理进行中到达的 kick 重新排队一次 drain：当前 claim 是事务时点
    %% 快照，可能未覆盖 kick 前后才落库的行；若吞掉，该批消息要等下一个
    %% kick/tick（测试模式 tick=0 时会永久滞留）。子句头同 idle 的教训：
    %% 必须 idle/draining(原子 EventType, Content, State) 形态。
    {keep_state, State, [{next_event, internal, drain}]};
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
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ==================== Internal Functions ====================

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
    try
        case msg_store_repo:claim_pending(?BATCH_SIZE, ?LEASE_SECONDS) of
            {ok, []} ->
                {ok, 0};
            {ok, Rows} ->
                _ = [process_row(Row) || Row <- Rows],
                {ok, length(Rows)};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _Class:_Reason ->
            {error, drain_crash}
    end.

process_row(Row) ->
    TypeBin = maps:get(<<"type">>, Row),
    MsgId = maps:get(<<"msg_id">>, Row),
    RetryCount = maps:get(<<"retry_count">>, Row, 0),
    TypeAtom = msg_type_atom(TypeBin),
    WriteResult = do_write(TypeAtom, Row),
    case WriteResult of
        ok ->
            maybe_archive(Row),
            msg_store_ds:unstage(MsgId),
            _ = ?DEBUG_LOG([msg_store_worker, write_success, TypeAtom, MsgId]);
        %% ON CONFLICT DO NOTHING 冲突跳过（msg_c2c_repo:write_msg_with_sender
        %% 改用 execute 后返回的 0 行插入）：消息已存在=幂等成功，unstage 即可，
        %% 但记 WARN 以便排查"冲突但正式表里没有行"的异常场景。
        {error, conflict_no_insert} ->
            maybe_archive(Row),
            msg_store_ds:unstage(MsgId),
            _ = ?WARN_LOG([msg_store_worker, write_conflict_no_insert, TypeAtom, MsgId]);
        {error, Reason} ->
            ErrorMsg = list_to_binary(io_lib:format("~p", [Reason])),
            case terminal_write_reason(Reason) of
                true ->
                    case msg_store_repo:mark_terminal(TypeBin, MsgId, ErrorMsg) of
                        {ok, _} ->
                            ok;
                        {error, MarkReason} ->
                            ?ERROR_LOG([msg_mark_terminal_error, TypeBin, MsgId, MarkReason])
                    end,
                    _ = ?ERROR_LOG([
                        msg_store_worker, write_terminal_failure, TypeAtom, MsgId, Reason
                    ]);
                false ->
                    BackoffSeconds = backoff_seconds(RetryCount),
                    case msg_store_repo:mark_failed(TypeBin, MsgId, ErrorMsg, BackoffSeconds) of
                        {ok, _} ->
                            ok;
                        {error, MarkReason} ->
                            ?ERROR_LOG([msg_mark_failed_error, TypeBin, MsgId, MarkReason])
                    end,
                    _ = ?ERROR_LOG([msg_store_worker, write_error, TypeAtom, MsgId, Reason])
            end
    end.

-spec terminal_write_reason(term()) -> boolean().
terminal_write_reason(no_recipients) ->
    true;
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
        I when is_integer(I) -> I;
        _ -> null
    catch
        _:_ -> null
    end;
to_int_or_null(_) ->
    null.

%%-------------------------------------------------------------------
%% @private 按配置开关决定是否归档到永久存储
%%
%% 归档失败只记日志，不阻塞投递流程（最终一致性）。
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
                    _ = ?ERROR_LOG([msg_store_worker, archive_error, MsgId, Reason])
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
    Res = msg_c2c_ds:write_msg(
        CreatedAt, MsgId, PayloadBin, FromId, ToId, ServerTs, MsgType, E2EE, SenderDid
    ),
    %% 【P0-1】ACK 先于落库竞态：落库后若该消息已被全部活跃设备确认则立即清理
    ok = msg_operation_ds:maybe_clean_delivered(c2c, MsgId, ToId),
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
                PayloadBin,
                FromId,
                ToIdList,
                G,
                MsgType,
                E2EE,
                null,
                Seq,
                SenderDid
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
    Res = msg_s2c_ds:write_msg(CreatedAt, MsgId, PayloadBin, FromId, ToId, ServerTs, Action, <<>>),
    %% 【P0-1】同 c2c：处理 ACK 先于落库的竞态
    ok = msg_operation_ds:maybe_clean_delivered(s2c, MsgId, ToId),
    Res;
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
