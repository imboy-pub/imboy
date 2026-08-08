-module(msg_c2c_logic).
-dialyzer({nowarn_function, [c2c_revoke/3]}).
%%%
%  msg_c2c 业务逻辑模块
%%%

-export([c2c/3]).
-export([c2c/4]).
-export([c2c_client_ack/3]).
-export([c2c_revoke/3]).
-export([c2c_revoke_ack/3]).
-export([c2c_edit/3]).
-export([c2c_edit_ack/3]).
-export([c2c_read/3]).
-export([c2c_read_ack/3]).
-export([c2c_input/3]).
-export([extract_reply_info/1]).

-include("chat.hrl").
-include("log.hrl").
-include("error_code.hrl").

% 2分钟
-define(REVOKE_TIMEOUT_MS, 120000).

%% 抑制 Dialyzer 警告 - 内部辅助函数
-dialyzer(
    {nowarn_function, [
        c2c/3, c2c/4, c2c_send/5, prepare_c2c_data/2, stage_and_send_c2c/11, set_c2c_expire_at/2
    ]}
).

%% ===================================================================
%% API
%% ===================================================================

%% 单聊消息
-spec c2c(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    ToId = ec_cnv:to_integer(To),

    %% 消息级限流（金钱 DoS/刷屏兜底）：超限自动禁言 → 拒发该条，不崩连接。
    %% is_muted/auto-mute/管理员 unmute 语义均由 check_and_record 内聚承载。
    case msg_rate_logic:check_and_record(CurrentUid) of
        {error, muted} ->
            {reply, message_ds:assemble_s2c(MsgId, <<"rate_limited">>, To)};
        _ ->
            c2c_send(MsgId, CurrentUid, To, ToId, Data)
    end.

-spec c2c_send(binary(), integer(), binary(), integer(), map()) -> ok | {reply, map()}.
c2c_send(MsgId, CurrentUid, To, ToId, Data) ->
    % 【优化】使用联合查询函数同时检查好友关系和黑名单状态
    {IsFriend, InDenylist} = friend_ds:check_relationship(ToId, CurrentUid),
    elib_log:info([<<"msg_c2c">>, CurrentUid, ToId, IsFriend, InDenylist]),
    %% AI agent 豁免好友校验：启用中的 agent（ai_agent_ds:is_agent）允许任何用户私聊
    %% 触发回复，无需互为好友；黑名单（InDenylist）依然优先拦截。
    SendIsFriend =
        case ai_agent_ds:is_agent(ToId) of
            {true, _} -> true;
            false -> IsFriend
        end,
    %% T1.2：可发决策退化为外壳调用纯函数 message_policy:send_decision/2
    case message_policy:send_decision(SendIsFriend, InDenylist) of
        allow ->
            {From, PayloadJson, MsgType, Action, E2EE, Timestamps} = prepare_c2c_data(
                CurrentUid, Data
            ),
            Result = stage_and_send_c2c(
                MsgId,
                To,
                ToId,
                From,
                PayloadJson,
                MsgType,
                Action,
                E2EE,
                Timestamps,
                CurrentUid,
                Data
            ),
            %% 【门后旁路】stage_and_send_c2c 返回 {reply, _} 表示该条被拒
            %% （部署级明文门 policy_violation / 引用消息不存在），此时绝不能再触发
            %% 下面两个副作用：agent 旁路会以**明文** C2C 回投用户，等于绕过刚刚生效
            %% 的明文拒收门；billing 也会给一条从未发出的消息计量。
            %% 与 C2G 侧「agent 触发只在 {ok,new} 分支内旁路」同一范式。
            case Result of
                {ok, new} ->
                    %% T1.4：若 To 是 AI agent 账号，旁路异步触发 LLM 回复（agent→human）。
                    %% fire-and-forget，不改变原 C2C 返回值/投递；E2EE 消息在内部被跳过。
                    %% ponytail: maybe_dispatch 对每条非 E2EE 文本 C2C 多做一次 ai_agent
                    %%   主键查，量级可控；真成热点再给 ai_agent_ds:is_agent 加 depcache 缓存。
                    _ = ai_agent_reply:maybe_dispatch(CurrentUid, ToId, Data),
                    %% billing 软计量埋点（金钱相邻）：fire-and-forget 累加 messages_sent，
                    %% 绝不阻塞主返回；无订阅/失败均 no-op（详见 billing_meter）。
                    _ = billing_meter:meter(<<"messages_sent">>, 1),
                    ok;
                {ok, duplicate} ->
                    %% duplicate 只补 SERVER_ACK；不得重复触发 agent 或 billing 副作用。
                    ok;
                _ ->
                    ok
            end,
            case Result of
                {ok, _} -> ok;
                _ -> Result
            end;
        {reject, in_denylist} ->
            Msg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, To),
            {reply, Msg};
        {reject, not_a_friend} ->
            Msg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, To),
            % elib_log:info(Msg),
            {reply, Msg}
    end.

%% @doc 兼容旧入口：保留 To/Payload 形态并复用当前 c2c/3
-spec c2c(binary(), integer(), binary(), map() | binary()) -> ok | {reply, map()}.
c2c(MsgId, CurrentUid, To, Payload) ->
    PayloadMap = compat_payload_map(Payload),
    Data = #{
        <<"to">> => To,
        <<"payload">> => PayloadMap,
        <<"created_at">> => elib_dt:now(),
        <<"msg_type">> => maps:get(<<"msg_type">>, PayloadMap, <<>>),
        <<"action">> => maps:get(<<"action">>, PayloadMap, <<>>),
        <<"e2ee">> => maps:get(<<"e2ee">>, PayloadMap, null)
    },
    c2c(MsgId, CurrentUid, Data).

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec compat_payload_map(map() | binary()) -> map().
compat_payload_map(Payload) when is_map(Payload) ->
    Payload;
compat_payload_map(Payload) when is_binary(Payload) ->
    try jsone:decode(Payload, [{object_format, map}]) of
        Map when is_map(Map) ->
            Map;
        _ ->
            #{<<"body">> => Payload}
    catch
        _:_ ->
            #{<<"body">> => Payload}
    end.

-spec policy_violation_reply(binary(), binary()) -> {reply, map()}.
policy_violation_reply(MsgId, Reason) ->
    {reply, #{
        <<"id">> => MsgId,
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"policy_violation">>,
        <<"payload">> => #{<<"reason">> => Reason},
        <<"server_ts">> => elib_dt:millisecond()
    }}.

%% @doc 准备单聊消息数据
%% @private
-spec prepare_c2c_data(integer(), map()) -> {integer(), binary(), binary(), binary(), map(), map()}.
prepare_c2c_data(CurrentUid, Data) ->
    NowTs = elib_dt:now(),
    NowMS = elib_dt:rfc3339_to(NowTs, millisecond),
    From = CurrentUid,
    Payload = maps:get(<<"payload">>, Data),
    CreatedAt = maps:get(<<"created_at">>, Data),
    CreatedAtRfc = elib_dt:to_rfc3339(CreatedAt),

    % v2.0: 从顶层提取字段
    MsgType = maps:get(<<"msg_type">>, Data, <<>>),
    Action = maps:get(<<"action">>, Data, <<>>),
    % map() | null
    E2EE = maps:get(<<"e2ee">>, Data, null),

    Timestamps = #{
        now_ts => NowTs,
        now_ms => NowMS,
        created_at_rfc => CreatedAtRfc
    },
    {From, Payload, MsgType, Action, E2EE, Timestamps}.

%% @doc 备份并发送单聊消息
%% @private
-spec stage_and_send_c2c(
    binary(),
    binary(),
    integer(),
    binary(),
    binary() | map(),
    binary(),
    binary(),
    map(),
    map(),
    integer(),
    map()
) ->
    {ok, new | duplicate} | {reply, map()}.
stage_and_send_c2c(
    MsgId, To, ToId, From, Payload, MsgType, Action, E2EE, Timestamps, CurrentUid, Data
) ->
    #{now_ts := NowTs, now_ms := NowMS, created_at_rfc := CreatedAtRfc} = Timestamps,

    % 【修复】将 Payload map 编码成 JSON binary
    % T1.2：构建决策退化为外壳调用 message_policy:encode_payload/1
    PayloadJson = message_policy:encode_payload(Payload),

    %% A2-a：服务端验证过的发送者设备标识（websocket_handler 已用
    %% websocket_logic:stamp_sender_device/2 盖进 Data 顶层，客户端不可伪造）。
    %% 实时投递靠下面的 with_sender_device/2 现场带上；**离线路径没有现场**，
    %% 必须在 staging 落库时存下来，否则重连拉取的 v3 消息永久判
    %% context_mismatch_sender_did 不可读。缺失时为 <<>>，落库列保持 NULL。
    SenderDid = maps:get(<<"sender_did">>, Data, <<>>),

    %% 部署级 E2EE fail-closed 门（与 msg_c2g_logic:do_send_c2g 同一入口范式）：
    %% e2ee_mode=required/compliance 或 storage_mode=*_e2ee 的部署拒收明文内容消息。
    %% 明文判定在 imboy_policy:encrypted_message_body/3——顶层 e2ee 为非空 map 且
    %% payload 非空即视为已加密（不看 msg_type），拒收时返回 S2C policy_violation。
    %% 这是最后一道兜底：客户端手动重试（MessageRetry._retryMessage）直接从本地 DB
    %% 原样重发报文、不过客户端策略门，明文重发只有服务端能拦。
    %% 非内容动作（撤回/已读/各类 ack）由 content_bearing_action/1 判 false 直接放行；
    %% 部署未要求加密时 message_encryption_required/0 为 false，整门短路，明文照常放行。
    %% ponytail: 只覆盖 c2c/3(→c2c/4) 这一条用户发送链与 do_c2c_edit；agent 主动消息
    %%   在 ai_agent_proactive:send_text 自带同款门。新增任何直写 msg_store_ds:stage
    %%   的 C2C 路径必须同步补门，否则又是一个绕过口。
    case imboy_policy:validate_message_write(<<"C2C">>, MsgType, Action, E2EE, PayloadJson) of
        ok ->
            % 提取引用回复信息
            {ReplyToMsgId, ReplyToFromId, ReplySnippet} = extract_reply_info(Data),

            % 【关键修复】先备份到 staging 表（同步，确保消息安全）
            % T1.2：暂存路径决策退化为外壳调用 message_policy:reply_mode/1
            StageResult =
                case message_policy:reply_mode({ReplyToMsgId, ReplyToFromId, ReplySnippet}) of
                    none ->
                        % 没有引用信息，使用常规方式
                        msg_store_ds:stage(
                            <<"c2c">>,
                            MsgId,
                            MsgType,
                            Action,
                            E2EE,
                            PayloadJson,
                            CurrentUid,
                            ToId,
                            CreatedAtRfc,
                            NowTs,
                            SenderDid
                        );
                    {reply, _, _, _} ->
                        % 有引用信息，需要先验证被引用的消息是否存在
                        case msg_c2c_ds:find_msg_by_id(ReplyToMsgId) of
                            {ok, _OriginalMsg} ->
                                msg_store_ds:stage(
                                    <<"c2c">>,
                                    MsgId,
                                    MsgType,
                                    Action,
                                    E2EE,
                                    PayloadJson,
                                    CurrentUid,
                                    ToId,
                                    CreatedAtRfc,
                                    NowTs,
                                    SenderDid
                                );
                            {error, not_found} ->
                                % 被引用的消息不存在，直接返回错误 reply（不使用 self() ! 副作用）
                                {reply, message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, To)};
                            {error, Reason} ->
                                % 兼容旧库结构：回复校验查询失败时降级为“跳过严格校验”，避免消息发送流程崩溃
                                ok = ?ERROR_LOG(
                                    "[C2C_REPLY_LOOKUP_FAILED] MsgId=~s, ReplyToMsgId=~s, Reason=~p~n",
                                    [MsgId, ReplyToMsgId, Reason]
                                ),
                                msg_store_ds:stage(
                                    <<"c2c">>,
                                    MsgId,
                                    MsgType,
                                    Action,
                                    E2EE,
                                    PayloadJson,
                                    CurrentUid,
                                    ToId,
                                    CreatedAtRfc,
                                    NowTs,
                                    SenderDid
                                );
                            Other ->
                                ok = ?ERROR_LOG(
                                    "[C2C_REPLY_LOOKUP_UNEXPECTED] MsgId=~s, ReplyToMsgId=~s, Result=~p~n",
                                    [MsgId, ReplyToMsgId, Other]
                                ),
                                msg_store_ds:stage(
                                    <<"c2c">>,
                                    MsgId,
                                    MsgType,
                                    Action,
                                    E2EE,
                                    PayloadJson,
                                    CurrentUid,
                                    ToId,
                                    CreatedAtRfc,
                                    NowTs,
                                    SenderDid
                                )
                        end
                end,

            elib_log:info(["stage_and_send_c2c", StageResult]),
            case StageResult of
                {reply, ErrMsg} ->
                    % 被引用消息不存在等业务错误，直接返回错误响应
                    {reply, ErrMsg};
                {ok, duplicate} ->
                    % 客户端重发（未收到 SERVER_ACK）：只补发 ACK，
                    % 跳过整条投递管道，避免接收端重复推送
                    self() ! {reply, message_policy:build_server_ack(MsgId, NowMS)},
                    {ok, duplicate};
                {ok, new} ->
                    % 立即响应和投递
                    % T1.2：ACK 构建退化为外壳调用 message_policy:build_server_ack/2
                    self() ! {reply, message_policy:build_server_ack(MsgId, NowMS)},

                    % 持久化侧（可安全重放：staging/正式表写入均幂等）
                    elib_async:async_retry(
                        fun() ->
                            % ① 先入队（异步，立即返回）
                            EnqueueData = #{
                                payload => Payload,
                                from_id => CurrentUid,
                                to_id => ToId,
                                created_at => CreatedAtRfc,
                                server_ts => NowTs
                            },
                            msg_store_ds:enqueue(<<"c2c">>, MsgId, EnqueueData),

                            % ② 如果有引用信息，使用 write_msg_with_reply 存储
                            % T1.2：复用纯函数 message_policy:reply_mode/1 判定
                            case
                                message_policy:reply_mode(
                                    {ReplyToMsgId, ReplyToFromId, ReplySnippet}
                                )
                            of
                                none ->
                                    % 没有引用信息，使用常规入队
                                    ok;
                                {reply, _, _, _} ->
                                    % 有引用信息，存储到数据库
                                    msg_c2c_ds:write_msg_with_reply(
                                        NowTs,
                                        MsgId,
                                        PayloadJson,
                                        CurrentUid,
                                        ToId,
                                        CreatedAtRfc,
                                        MsgType,
                                        E2EE,
                                        ReplyToMsgId,
                                        ReplyToFromId,
                                        ReplySnippet
                                    )
                            end,

                            % ③ 消息自毁：设置 expire_at（如果客户端指定了 expire_secs）
                            ExpireSecs = maps:get(<<"expire_secs">>, Data, undefined),
                            case msg_burn_logic:valid_expire_secs(ExpireSecs) of
                                true when is_integer(ExpireSecs), ExpireSecs > 0 ->
                                    ExpireAt = msg_burn_logic:calc_expire_at(
                                        CreatedAtRfc, ExpireSecs
                                    ),
                                    set_c2c_expire_at(MsgId, ExpireAt);
                                _ ->
                                    ok
                            end
                        end,
                        3,
                        1000
                    ),

                    % 投递侧（不纳入重放边界：QoS 有自己的 ACK 重试链，
                    % 与持久化同闭包整体重放会把已成功的实时投递再推一次）
                    elib_async:async(
                        fun() ->
                            Msg0 = message_ds:assemble_msg(
                                <<"C2C">>, From, To, Payload, MsgId, MsgType, Action, E2EE
                            ),
                            % 带上服务端验证过的发送者设备标识（信封顶层）。
                            % 接收侧 PFv3 context binding 第 6 项拿它与受认证的
                            % protected_header.sender_did 硬比对；不带 = 每条
                            % C2C v3 消息都判 context_mismatch_sender_did。
                            Msg = message_ds:with_sender_device(Msg0, Data),
                            imboy_message_helper:encode_and_send(ToId, MsgId, Msg, <<"c2c">>),
                            % 离线推送（异步，不阻塞消息投递）
                            push_notification_logic:maybe_push_for_c2c(
                                CurrentUid, ToId, MsgType, Payload
                            )
                        end
                    ),
                    {ok, new}
                % end case StageResult
            end;
        {error, Reason} ->
            policy_violation_reply(MsgId, Reason)
    end.

%% 客户端确认C2C投递消息
-spec c2c_client_ack(binary(), integer(), binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, DID) ->
    msg_ack_logic:client_ack(<<"c2c">>, MsgId, CurrentUid, DID).

%% 客户端撤回消息 for c2c
-spec c2c_revoke(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c_revoke(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload, <<>>),
    ToId = ec_cnv:to_integer(To),
    FromId = ec_cnv:to_integer(From),
    % ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),

    %% 【权限验证】只能撤销自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            %% 【新增】检查消息是否存在；正式表查不到时兜底查 staging，
            %% 修复"秒撤"竞态：消息仍在异步管道内被误判 msg_not_found
            FindResult =
                case msg_c2c_ds:find_msg_by_id(OriginalMsgId) of
                    {ok, Found} -> {ok, Found};
                    _ -> msg_store_ds:find_staged(OriginalMsgId)
                end,
            case FindResult of
                {ok, MsgData} ->
                    %% 【新增】检查消息撤回时间限制
                    %% 用 CurrentUid（鉴权得到）而非客户端自报的 FromId 做归属比对，
                    %% 消除"影子信任"：即便未来误删了外层等值校验，这里仍然安全
                    case MsgData of
                        #{<<"from_id">> := CurrentUid} ->
                            CreatedAt = maps:get(<<"created_at">>, MsgData),
                            CreatedAtMs = elib_dt:rfc3339_to(CreatedAt, millisecond),
                            NowMs = elib_dt:millisecond(),

                            % 检查是否超过撤回时间限制（2分钟）
                            case
                                is_integer(CreatedAtMs) andalso
                                    NowMs - CreatedAtMs > ?REVOKE_TIMEOUT_MS
                            of
                                true ->
                                    % 超过撤回时间限制
                                    ErrorMsg = #{
                                        <<"id">> => MsgId,
                                        <<"type">> => <<"C2C">>,
                                        <<"from">> => From,
                                        <<"to">> => To,
                                        <<"msg_type">> => <<"custom">>,
                                        <<"action">> => <<"message_revoke_error">>,
                                        <<"payload">> => #{
                                            <<"content">> => <<>>,
                                            <<"original_msg_id">> => OriginalMsgId,
                                            <<"error">> => <<"超过撤回时间限制(2分钟)"/utf8>>,
                                            <<"code">> => ?ERR_REVOKE_TIMEOUT
                                        },
                                        <<"server_ts">> => NowMs
                                    },
                                    {reply, ErrorMsg};
                                false ->
                                    % 未超过时间限制，继续原有逻辑
                                    NowTs = elib_dt:now(),

                                    % 取消原消息在接收方各在线设备上的投递重试定时器，
                                    % 避免撤回后重试窗口内原文仍被再投递一次
                                    _ = [
                                        websocket_logic:cancel_timer(ToId, DID, OriginalMsgId)
                                     || DID <- user_device_logic:online_dids(ToId)
                                    ],

                                    % 构建撤销确认消息（v2.0 格式）
                                    %% msg_type 和 action 在顶层，不在 payload 中
                                    RevokePayload = #{
                                        <<"content">> => <<>>,
                                        <<"original_msg_id">> => OriginalMsgId,
                                        <<"revoked_at">> => NowMs
                                    },

                                    RevokeMsg = #{
                                        <<"id">> => MsgId,
                                        <<"type">> => <<"C2C">>,
                                        <<"from">> => From,
                                        <<"to">> => To,
                                        <<"msg_type">> => <<"custom">>,
                                        <<"action">> => <<"message_revoke_ack">>,
                                        <<"payload">> => RevokePayload,
                                        <<"server_ts">> => NowMs
                                    },

                                    % 判断对方是否在线
                                    case user_logic:is_online(ToId) of
                                        true ->
                                            imboy_message_helper:encode_and_send(
                                                ToId, MsgId, RevokeMsg, <<"c2s">>
                                            ),
                                            ok;
                                        % 对端离线处理
                                        false ->
                                            % v2.0: 使用 revoke_offline_msg/9 显式传递 msg_type 和 action
                                            case
                                                msg_c2c_ds:revoke_offline_msg(
                                                    RevokePayload,
                                                    NowTs,
                                                    MsgId,
                                                    OriginalMsgId,
                                                    CurrentUid,
                                                    ToId,
                                                    <<"custom">>,
                                                    <<"message_revoke_ack">>,
                                                    null
                                                )
                                            of
                                                ok -> ok;
                                                {error, _} -> ok
                                            end
                                    end,
                                    {reply, RevokeMsg}
                            end;
                        #{<<"from_id">> := _OtherId} ->
                            %% 消息不属于当前用户
                            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
                            {reply, ErrorMsg}
                    end;
                _ ->
                    %% 消息不存在或格式错误
                    ErrorMsg = message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, To),
                    {reply, ErrorMsg}
            end;
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.
%% 客户端撤回消息确认 for c2c
-spec c2c_revoke_ack(binary(), integer(), Data :: map()) -> ok.
c2c_revoke_ack(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload, <<>>),
    ok = ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId]),
    AckPayload = Payload#{
        <<"action">> => <<"message_revoke_ack">>,
        <<"ack_msg_id">> => MsgId,
        <<"ack_uid">> => CurrentUid,
        <<"ack_at">> => elib_dt:millisecond()
    },
    persist_action_payload(OriginalMsgId, AckPayload),
    ok.

%% 客户端编辑消息 for c2c
-spec c2c_edit(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c_edit(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload, <<>>),
    FromId = ec_cnv:to_integer(From),

    % 验证权限：只能编辑自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            %% 【新增】编辑时间窗校验（MIRROR c2c_revoke 的查找与时限路径）
            case c2c_edit_window_check(OriginalMsgId, CurrentUid) of
                ok ->
                    do_c2c_edit(MsgId, CurrentUid, Data);
                {expired, NowMs} ->
                    ErrorMsg = #{
                        <<"id">> => MsgId,
                        <<"type">> => <<"C2C">>,
                        <<"from">> => From,
                        <<"to">> => To,
                        <<"msg_type">> => <<"custom">>,
                        <<"action">> => <<"message_edit_error">>,
                        <<"payload">> => #{
                            <<"content">> => <<>>,
                            <<"original_msg_id">> => OriginalMsgId,
                            <<"error">> => <<"超过编辑时间限制"/utf8>>,
                            <<"code">> => ?ERR_REVOKE_TIMEOUT
                        },
                        <<"server_ts">> => NowMs
                    },
                    {reply, ErrorMsg};
                permission_denied ->
                    {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To)};
                not_found ->
                    {reply, message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, To)}
            end;
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.

%% @private c2c_edit 通过权限与时间窗校验后的原编辑逻辑
-spec do_c2c_edit(binary(), integer(), map()) -> ok | {reply, Msg :: map()}.
do_c2c_edit(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload, <<>>),
    NewContent = maps:get(<<"content">>, Payload),
    MsgType = maps:get(<<"msg_type">>, Payload),
    E2EE = maps:get(<<"e2ee">>, Data, null),
    ToId = ec_cnv:to_integer(To),
    ok = ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),
    NowTs = elib_dt:now(),
    NowMS = elib_dt:millisecond(),

    % 构建编辑确认消息（v2.0 格式）
    %% msg_type 和 action 提升到顶层
    EditPayload = #{
        <<"content">> => NewContent,
        <<"original_msg_id">> => OriginalMsgId,
        <<"edited_at">> => NowMS
    },

    EditMsg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => From,
        <<"to">> => To,
        <<"msg_type">> => MsgType,
        <<"action">> => <<"message_edit_ack">>,
        <<"payload">> => EditPayload,
        <<"server_ts">> => NowMS
    },

    EditPayloadJson = imboy_message_helper:encode_json(EditPayload),
    case
        imboy_policy:validate_message_write(
            <<"C2C">>,
            MsgType,
            <<"message_edit">>,
            E2EE,
            EditPayloadJson
        )
    of
        ok ->
            % 判断对方是否在线
            case user_logic:is_online(ToId) of
                true ->
                    imboy_message_helper:encode_and_send(ToId, MsgId, EditMsg, <<"c2s">>),
                    ok;
                % 对端离线处理
                false ->
                    case
                        msg_c2c_ds:edit_offline_msg(
                            EditPayloadJson, NowTs, MsgId, CurrentUid, ToId
                        )
                    of
                        ok ->
                            ok;
                        {error, _Reason} ->
                            ok
                    end
            end,
            {reply, EditMsg};
        {error, Reason} ->
            policy_violation_reply(MsgId, Reason)
    end.

%% @private 编辑时间窗校验：查原消息（staging 兜底），核对归属并检查是否超窗
-spec c2c_edit_window_check(binary(), integer()) ->
    ok | {expired, integer()} | permission_denied | not_found.
c2c_edit_window_check(OriginalMsgId, CurrentUid) ->
    %% 正式表查不到时兜底查 staging（消息仍在异步管道内）
    FindResult =
        case msg_c2c_ds:find_msg_by_id(OriginalMsgId) of
            {ok, Found} -> {ok, Found};
            _ -> msg_store_ds:find_staged(OriginalMsgId)
        end,
    case FindResult of
        {ok, #{<<"from_id">> := CurrentUid} = MsgData} ->
            CreatedAt = maps:get(<<"created_at">>, MsgData),
            CreatedAtMs = elib_dt:rfc3339_to(CreatedAt, millisecond),
            NowMs = elib_dt:millisecond(),
            WindowMs = msg_edit_window_ms(),
            case
                WindowMs > 0 andalso
                    is_integer(CreatedAtMs) andalso
                    NowMs - CreatedAtMs > WindowMs
            of
                true -> {expired, NowMs};
                false -> ok
            end;
        {ok, _} ->
            permission_denied;
        _ ->
            not_found
    end.

%% @private 编辑时间窗（毫秒），env msg_edit_window_seconds，默认 86400 秒；<=0 不限
-spec msg_edit_window_ms() -> integer().
msg_edit_window_ms() ->
    case application:get_env(imboy, msg_edit_window_seconds) of
        {ok, V} when is_integer(V) -> V * 1000;
        _ -> 86400 * 1000
    end.

%% 客户端编辑消息确认 for c2c
-spec c2c_edit_ack(binary(), integer(), Data :: map()) -> ok.
c2c_edit_ack(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload, <<>>),
    NewContent = maps:get(<<"content">>, Payload),
    EditedAt = maps:get(<<"edited_at">>, Payload),
    ok = ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId, NewContent, EditedAt]),
    AckPayload = Payload#{
        <<"action">> => <<"message_edit_ack">>,
        <<"ack_msg_id">> => MsgId,
        <<"ack_uid">> => CurrentUid,
        <<"ack_at">> => elib_dt:millisecond()
    },
    persist_action_payload(OriginalMsgId, AckPayload),
    ok.

%% ===================================================================
%% 消息已读回执功能
%% ===================================================================

%% @doc 客户端发送消息已读回执
%% 接收者阅读消息后，向发送者发送已读通知
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID（接收者）
%% @param Data 消息数据
%% @return ok | {reply, Msg :: map()}
-spec c2c_read(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c_read(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    % 发送者ID
    ToId = ec_cnv:to_integer(To),
    % 接收者ID（自己）
    FromId = ec_cnv:to_integer(From),

    % 检查是否是发给自己的消息（不能对自己发送已读回执）
    case CurrentUid =:= FromId of
        true when CurrentUid =:= ToId ->
            % 发给自己的消息，不需要已读回执
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"invalid_operation">>, To),
            {reply, ErrorMsg};
        true ->
            % 正常的已读回执
            % 检查好友关系
            {IsFriend, InDenylist} = friend_ds:check_relationship(ToId, CurrentUid),
            case {IsFriend, InDenylist} of
                {true, false} ->
                    handle_read_receipt(MsgId, To, ToId, From, FromId, CurrentUid, Data);
                {_, InDenylist2} when
                    InDenylist2 =:= true orelse
                        (is_integer(InDenylist2) andalso InDenylist2 > 0)
                ->
                    ErrorMsg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, To),
                    {reply, ErrorMsg};
                {false, _} ->
                    ErrorMsg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, To),
                    {reply, ErrorMsg}
            end;
        false ->
            % 权限错误：from 字段不匹配当前用户
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.

%% @doc 处理已读回执的内部逻辑
%% @private
-spec handle_read_receipt(binary(), binary(), integer(), binary(), integer(), integer(), map()) ->
    ok | {reply, map()}.
handle_read_receipt(MsgId, To, ToId, From, _FromId, CurrentUid, Data) ->
    NowMs = elib_dt:millisecond(),
    Payload = maps:get(<<"payload">>, Data, #{}),
    ReadAt = maps:get(<<"read_at">>, Payload, NowMs),

    % 从 Data 中获取设备ID（通过 WebSocket 注入的 sender_did）
    ToDid = maps:get(<<"sender_did">>, Payload, <<>>),

    %% 【契约修复】客户端现役形态（buildReadReceiptItem）payload.msg_ids
    %% 是被读消息 id 批量，顶层 id 只是回执自身的新 Xid。此前实现忽略
    %% msg_ids：save_read 存了回执自身 id、转发通知也丢弃 msg_ids，而接收方
    %% _handleReadAction 只认 payload.msg_ids → C2C 已读状态从未生效。
    %% 批量优先；无 msg_ids 的单条形态回落顶层 MsgId（旧契约兼容）。
    MsgIds =
        case maps:get(<<"msg_ids">>, Payload, []) of
            [_ | _] = L -> [ec_cnv:to_binary(I) || I <- L];
            _ -> [MsgId]
        end,

    % 保存已读记录到数据库（逐条；任一失败按失败处理）
    ReadAtRfc = elib_dt:to_rfc3339(ReadAt),
    Results = [msg_read_ds:save_read(Id, ToId, CurrentUid, ToDid, ReadAtRfc) || Id <- MsgIds],
    case [R || R <- Results, R =/= ok] of
        [] ->
            % 【T18/MSG-P1-6】已读状态同步给阅读者自己的其他设备（多端未读数一致）。
            % save 落 msg_s2c：离线设备按 per-device 送达语义（T03）重连后仍可拉到；
            % 阅读设备自身也会收到并 ACK（客户端按已读状态幂等忽略），单设备用户
            % 全端 ACK 后该行随即被清理。新增 S2C action，旧客户端按未知 action 忽略。
            ReadSyncPayload = #{
                % msg_id 保留单值形态兼容旧消费者
                <<"msg_id">> => MsgId,
                <<"msg_ids">> => MsgIds,
                <<"peer">> => To,
                <<"read_at">> => ReadAt
            },
            _ = msg_s2c_ds:send(
                0, [CurrentUid], <<"message_read_sync">>, <<>>, null, ReadSyncPayload, save
            ),
            % 构建已读回执消息（v2.0 格式）；msg_ids 必须回带——
            % 发送方客户端按 payload.msg_ids 标记 seen
            ReadPayload = #{
                <<"read_at">> => ReadAt,
                <<"msg_ids">> => MsgIds
            },

            ReadAckMsg = #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2C">>,
                <<"from">> => From,
                <<"to">> => To,
                <<"msg_type">> => <<"custom">>,
                <<"action">> => <<"message_read_ack">>,
                <<"payload">> => ReadPayload,
                <<"server_ts">> => NowMs
            },

            % 构建发送给发送者的已读通知
            ReadNotifyMsg = #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2C">>,
                % 发送者
                <<"from">> => From,
                % 发送给发送者
                <<"to">> => From,
                <<"msg_type">> => <<"custom">>,
                <<"action">> => <<"message_read">>,
                <<"payload">> => ReadPayload,
                <<"server_ts">> => NowMs
            },

            % 判断发送者是否在线
            case user_logic:is_online(ToId) of
                true ->
                    % 在线：直接发送已读通知
                    imboy_message_helper:encode_and_send(ToId, MsgId, ReadNotifyMsg, <<"c2c">>),
                    {reply, ReadAckMsg};
                false ->
                    % 离线：逐条存储离线已读通知
                    %（原实现只存顶层回执 id；另删除无用的 jsone:encode 死计算）
                    _ = [
                        msg_c2c_ds:read_offline_msg(
                            Id, ToId, CurrentUid, ReadAtRfc, <<"message_read">>
                        )
                     || Id <- MsgIds
                    ],
                    {reply, ReadAckMsg}
            end;
        [Reason | _] ->
            ok = ?ERROR_LOG(
                "[C2C_READ_FAILED] MsgIds=~p, FromUid=~p, ToUid=~p, Reason=~p~n",
                [MsgIds, CurrentUid, ToId, Reason]
            ),
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"internal_error">>, To),
            {reply, ErrorMsg}
    end.

%% @doc 客户端确认已读回执
%% 发送者确认收到已读回执
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID（发送者）
%% @param Data 消息数据
%% @return ok
-spec c2c_read_ack(binary(), integer(), Data :: map()) -> ok.
c2c_read_ack(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    ReadAt = maps:get(<<"read_at">>, Payload),
    ok = ?DEBUG_LOG([<<"c2c_read_ack">>, MsgId, CurrentUid, ReadAt]),
    % 已读状态已在 c2c_read/3 中持久化，这里只做回执送达确认，不重复写库。
    ok.

%% @doc 输入状态（typing）转发：fire-and-forget
%% 好友且在线 → 直发（encode_and_send，不落库不重试不回执）；
%% 离线/非好友 → 静默丢弃（typing 是瞬态信号，无补投语义）。
%% 此前该 action 无路由：客户端 JSON 形态被 route_action 判 unknown、
%% 0x25 二进制形态被 v2 分派回 unsupported_frame_type，typing 从未生效。
-spec c2c_input(binary(), integer(), map()) -> ok.
c2c_input(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data, <<>>),
    ToId = ec_cnv:to_integer(To),
    IsFriend = ToId > 0 andalso friend_ds:is_friend(ToId, CurrentUid),
    case IsFriend andalso user_logic:is_online(ToId) of
        true ->
            Msg = Data#{
                <<"from">> => ec_cnv:to_binary(CurrentUid),
                <<"server_ts">> => elib_dt:millisecond()
            },
            imboy_message_helper:encode_and_send(ToId, MsgId, Msg, <<"c2c">>),
            ok;
        false ->
            ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 从消息数据中提取引用回复信息
%% @param Data 消息数据
%% @return {ReplyToMsgId, ReplyToFromId, ReplySnippet}
-spec extract_reply_info(map()) -> {binary(), integer(), binary()}.
extract_reply_info(Data) ->
    case maps:get(<<"reply_to">>, Data, undefined) of
        undefined ->
            {<<>>, 0, <<>>};
        ReplyTo when is_map(ReplyTo) ->
            ReplyToMsgId = maps:get(<<"msg_id">>, ReplyTo, <<>>),
            ReplyToFromIdBin = maps:get(<<"from_id">>, ReplyTo, <<>>),
            ReplyToFromId = ec_cnv:to_integer(ReplyToFromIdBin),

            % 从被引用的消息中提取摘要
            ReplySnippet =
                case ReplyToMsgId of
                    <<>> ->
                        <<>>;
                    _ ->
                        case msg_c2c_ds:find_msg_by_id(ReplyToMsgId) of
                            {ok, OriginalMsg} ->
                                Payload = maps:get(<<"payload">>, OriginalMsg, <<>>),
                                % 尝试解析 JSON 并提取 content 字段
                                try jsone:decode(Payload) of
                                    PayloadMap when is_map(PayloadMap) ->
                                        Content = maps:get(<<"content">>, PayloadMap, <<>>),
                                        % 截取前50个字符作为摘要
                                        Snippet = binary:part(
                                            Content, {0, min(byte_size(Content), 50)}
                                        ),
                                        case byte_size(Content) > 50 of
                                            true -> <<Snippet/binary, "..."/utf8>>;
                                            false -> Snippet
                                        end;
                                    _ ->
                                        <<>>
                                catch
                                    _:_ ->
                                        % 如果解析失败，截取原始 payload 的前50个字符
                                        Snippet = binary:part(
                                            Payload, {0, min(byte_size(Payload), 50)}
                                        ),
                                        case byte_size(Payload) > 50 of
                                            true -> <<Snippet/binary, "..."/utf8>>;
                                            false -> Snippet
                                        end
                                end;
                            _ ->
                                <<>>
                        end
                end,
            {ReplyToMsgId, ReplyToFromId, ReplySnippet};
        _ ->
            {<<>>, 0, <<>>}
    end.

%% @doc 设置C2C消息的自毁时间
%% @param MsgId 消息ID
%% @param ExpireAt 过期时间（RFC3339 binary）
-spec set_c2c_expire_at(binary(), binary()) -> ok.
set_c2c_expire_at(MsgId, ExpireAt) ->
    msg_c2c_ds:set_expire_at(MsgId, ExpireAt).

%% @doc 持久化 action ack payload 到原消息记录
%% 原消息若已被客户端 ACK 清理，更新影响行数为 0，不视为错误。
-spec persist_action_payload(binary(), map()) -> ok.
persist_action_payload(<<>>, _Payload) ->
    ok;
persist_action_payload(OriginalMsgId, Payload) when is_binary(OriginalMsgId), is_map(Payload) ->
    PayloadJson = imboy_message_helper:encode_json(Payload),
    case msg_c2c_ds:update_payload_by_msg_id(OriginalMsgId, PayloadJson) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            _ = ?WARN_LOG({persist_action_payload_failed, OriginalMsgId, Reason}),
            ok
    end.
