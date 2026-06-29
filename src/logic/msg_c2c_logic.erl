-module(msg_c2c_logic).
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
-export([extract_reply_info/1]).

-include("chat.hrl").
-include("log.hrl").
-include("error_code.hrl").

% 2分钟
-define(REVOKE_TIMEOUT_MS, 120000).

%% 抑制 Dialyzer 警告 - 内部辅助函数
-dialyzer({nowarn_function, [c2c/3, prepare_c2c_data/2, stage_and_send_c2c/11]}).

%% ===================================================================
%% API
%% ===================================================================

%% 单聊消息
-spec c2c(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    ToId = ec_cnv:to_integer(To),

    % 【优化】使用联合查询函数同时检查好友关系和黑名单状态
    {IsFriend, InDenylist} = friend_ds:check_relationship(ToId, CurrentUid),
    elib_log:info([<<"msg_c2c">>, CurrentUid, ToId, IsFriend, InDenylist]),
    %% T1.2：可发决策退化为外壳调用纯函数 message_policy:send_decision/2
    case message_policy:send_decision(IsFriend, InDenylist) of
        allow ->
            {From, PayloadJson, MsgType, Action, E2EE, Timestamps} = prepare_c2c_data(
                CurrentUid, Data
            ),
            stage_and_send_c2c(
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
            );
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
-spec prepare_c2c_data(integer(), map()) -> {binary(), binary(), binary(), binary(), map(), map()}.
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
    ok | {reply, map()}.
stage_and_send_c2c(
    MsgId, To, ToId, From, Payload, MsgType, Action, E2EE, Timestamps, CurrentUid, Data
) ->
    #{now_ts := NowTs, now_ms := NowMS, created_at_rfc := CreatedAtRfc} = Timestamps,

    % 【修复】将 Payload map 编码成 JSON binary
    % T1.2：构建决策退化为外壳调用 message_policy:encode_payload/1
    PayloadJson = message_policy:encode_payload(Payload),

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
                            NowTs
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
                                    NowTs
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
                                    NowTs
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
                                    NowTs
                                )
                        end
                end,

            elib_log:info(["stage_and_send_c2c", StageResult]),
            case StageResult of
                {reply, ErrMsg} ->
                    % 被引用消息不存在等业务错误，直接返回错误响应
                    {reply, ErrMsg};
                ok ->
                    % 立即响应和投递
                    % T1.2：ACK 构建退化为外壳调用 message_policy:build_server_ack/2
                    self() ! {reply, message_policy:build_server_ack(MsgId, NowMS)},

                    % 异步处理：入队 + 投递消息（带重试）
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

                            % ③ 后投递（使用 MsgType/Action/E2EE 参数，不解析 Payload）
                            Msg = message_ds:assemble_msg(
                                <<"C2C">>, From, To, Payload, MsgId, MsgType, Action, E2EE
                            ),
                            imboy_message_helper:encode_and_send(ToId, MsgId, Msg, <<"c2c">>),
                            % ④ 消息自毁：设置 expire_at（如果客户端指定了 expire_secs）
                            ExpireSecs = maps:get(<<"expire_secs">>, Data, undefined),
                            case msg_burn_logic:valid_expire_secs(ExpireSecs) of
                                true when is_integer(ExpireSecs), ExpireSecs > 0 ->
                                    ExpireAt = msg_burn_logic:calc_expire_at(
                                        CreatedAtRfc, ExpireSecs
                                    ),
                                    set_c2c_expire_at(MsgId, ExpireAt);
                                _ ->
                                    ok
                            end,
                            % ⑤ 离线推送（异步，不阻塞消息投递）
                            push_notification_logic:maybe_push_for_c2c(
                                CurrentUid, ToId, MsgType, Payload
                            )
                        end,
                        3,
                        1000
                    ),
                    ok
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
            %% 【新增】检查消息是否存在
            case msg_c2c_ds:find_msg_by_id(OriginalMsgId) of
                {ok, MsgData} ->
                    %% 【新增】检查消息撤回时间限制
                    case MsgData of
                        #{<<"from_id">> := FromId} ->
                            CreatedAt = maps:get(<<"created_at">>, MsgData),
                            CreatedAtMs = elib_dt:rfc3339_to(CreatedAt, millisecond),
                            NowMs = elib_dt:millisecond(),

                            % 检查是否超过撤回时间限制（2分钟）
                            case NowMs - CreatedAtMs > ?REVOKE_TIMEOUT_MS of
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
                                            % v2.0: 使用 revoke_offline_msg/8 显式传递 msg_type 和 action
                                            case
                                                msg_c2c_ds:revoke_offline_msg(
                                                    RevokePayload,
                                                    NowTs,
                                                    MsgId,
                                                    FromId,
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
    NewContent = maps:get(<<"content">>, Payload),
    MsgType = maps:get(<<"msg_type">>, Payload),
    E2EE = maps:get(<<"e2ee">>, Data, null),
    ToId = ec_cnv:to_integer(To),
    FromId = ec_cnv:to_integer(From),
    ok = ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),

    % 验证权限：只能编辑自己发送的消息
    case CurrentUid =:= FromId of
        true ->
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
                                    EditPayloadJson, NowTs, MsgId, FromId, ToId
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
            end;
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
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
    ReadAt = maps:get(<<"read_at">>, maps:get(<<"payload">>, Data, #{}), NowMs),

    % 从 Data 中获取设备ID（通过 WebSocket 注入的 sender_did）
    Payload = maps:get(<<"payload">>, Data, #{}),
    ToDid = maps:get(<<"sender_did">>, Payload, <<>>),

    % 保存已读记录到数据库
    ReadAtRfc = elib_dt:to_rfc3339(ReadAt),
    case msg_read_ds:save_read(MsgId, ToId, CurrentUid, ToDid, ReadAtRfc) of
        ok ->
            % 构建已读回执消息（v2.0 格式）
            ReadPayload = #{
                <<"read_at">> => ReadAt
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
                    % 离线：存储离线已读通知
                    _ = jsone:encode(ReadNotifyMsg, [native_utf8]),
                    case
                        msg_c2c_ds:read_offline_msg(
                            MsgId, ToId, CurrentUid, ReadAtRfc, <<"message_read">>
                        )
                    of
                        ok -> ok;
                        {error, _} -> ok
                    end,
                    {reply, ReadAckMsg}
            end;
        {error, Reason} ->
            ok = ?ERROR_LOG(
                "[C2C_READ_FAILED] MsgId=~s, FromUid=~p, ToUid=~p, Reason=~p~n",
                [MsgId, CurrentUid, ToId, Reason]
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
