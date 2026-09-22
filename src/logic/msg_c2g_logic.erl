-module(msg_c2g_logic).
-dialyzer({nowarn_function, [handle_group_action/6]}).

%%%
%  msg_c2g 业务逻辑模块
%%%
-export([c2g/3]).
%% 群级 E2EE fail-closed 门（导出供 EUnit 直测）
-export([group_e2ee_gate/5]).
-export([c2g_client_ack/3]).
-export([c2g_revoke/3]).
-export([c2g_revoke_ack/3]).
-export([c2g_edit/3]).
-export([c2g_edit_ack/3]).
-export([read_stats/2]).
-export([extract_reply_info/1]).

-include("chat.hrl").
-include("log.hrl").
-include("error_code.hrl").

% 2分钟
-define(REVOKE_TIMEOUT_MS, 120000).

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec policy_violation_reply(binary(), binary()) -> {reply, map()}.
policy_violation_reply(MsgId, Reason) ->
    {reply, #{
        <<"id">> => MsgId,
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"policy_violation">>,
        <<"payload">> => #{<<"reason">> => Reason},
        <<"server_ts">> => elib_dt:millisecond()
    }}.

-spec policy_violation_reply(binary(), binary(), integer()) -> {reply, map()}.
policy_violation_reply(MsgId, Reason, Gid) ->
    {reply, Reply} = policy_violation_reply(MsgId, Reason),
    Payload = maps:get(<<"payload">>, Reply),
    {reply, Reply#{<<"payload">> => Payload#{<<"gid">> => Gid}}}.

%% @private 编辑时间窗（毫秒），env msg_edit_window_seconds，默认 86400 秒；<=0 不限
-spec msg_edit_window_ms() -> integer().
msg_edit_window_ms() ->
    case application:get_env(imboy, msg_edit_window_seconds) of
        {ok, V} when is_integer(V) -> V * 1000;
        _ -> 86400 * 1000
    end.

-spec mentions_from_payload(term()) -> list().
mentions_from_payload(Payload) when is_map(Payload) ->
    maps:get(<<"mentions">>, Payload, []);
mentions_from_payload(_) ->
    [].

%% ===================================================================
%% API
%% ===================================================================

%% 群聊发送消息
-spec c2g(binary(), integer(), map()) -> ok | {reply, map()}.
c2g(MsgId, CurrentUid, Data) ->
    Gid = maps:get(<<"to">>, Data),
    ToGID = ec_cnv:to_integer(Gid),

    %% 消息级限流（金钱 DoS/刷屏兜底）：超限自动禁言 → 拒发该条，不崩连接。
    %% 与群内 check_mute（管理员群禁言）正交：这是 per-user 全局发消息速率闸门。
    case msg_rate_logic:check_and_record(CurrentUid) of
        {error, muted} ->
            _ = ?WARN_LOG("用户 ~p 消息发送频率超限，拒发 C2G", [CurrentUid]),
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_ERROR">>,
                    <<"error">> => <<"Message rate limit exceeded"/utf8>>,
                    <<"code">> => 429
                }},
            ok;
        _ ->
            c2g_send(MsgId, CurrentUid, Gid, ToGID, Data)
    end.

-spec c2g_send(binary(), integer(), binary(), integer(), map()) -> ok | {reply, map()}.
c2g_send(MsgId, CurrentUid, Gid, ToGID, Data) ->
    % 检查是否被禁言
    case group_member_logic:check_mute(ToGID, CurrentUid) of
        true ->
            _ = ?WARN_LOG("用户 ~p 在群组 ~p 中被禁言，无法发送消息", [CurrentUid, ToGID]),
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_ERROR">>,
                    <<"error">> => <<"You are muted in this group"/utf8>>,
                    <<"code">> => 403
                }},
            ok;
        false ->
            % 检查是否是群成员
            case group_ds:is_member(CurrentUid, ToGID) of
                true ->
                    % 解析 mentions 字段
                    Payload = maps:get(<<"payload">>, Data, #{}),
                    Mentions = mentions_from_payload(Payload),
                    HasMentionAll = lists:member(<<"all">>, Mentions),

                    % @所有人需要管理员权限
                    case HasMentionAll of
                        true ->
                            case group_member_ds:check_admin(CurrentUid, ToGID) of
                                true ->
                                    send_c2g_fail_closed(
                                        MsgId, CurrentUid, Data, Gid, ToGID, 3
                                    );
                                false ->
                                    _ = ?WARN_LOG("用户 ~p 尝试使用 @所有人功能但没有管理员权限", [CurrentUid]),
                                    self() !
                                        {reply, #{
                                            <<"id">> => MsgId,
                                            <<"type">> => <<"C2G_ERROR">>,
                                            <<"error">> =>
                                                <<"Permission denied: @all mentions require admin role"/utf8>>,
                                            <<"code">> => 403
                                        }},
                                    ok
                            end;
                        false ->
                            send_c2g_fail_closed(MsgId, CurrentUid, Data, Gid, ToGID, 1)
                    end;
                false ->
                    _ = ?WARN_LOG("用户 ~p 尝试向非成员群组 ~p 发送消息", [CurrentUid, ToGID]),
                    self() !
                        {reply, #{
                            <<"id">> => MsgId,
                            <<"type">> => <<"C2G_ERROR">>,
                            <<"error">> => <<"Not a group member"/utf8>>,
                            <<"code">> => 403
                        }},
                    ok
            end
    end.

%% @private
%% @doc staging 事务内固化收件人，并按消息语义重验发送者最低角色。
-spec send_c2g_fail_closed(binary(), integer(), map(), binary(), integer(), 1 | 3) -> ok.
send_c2g_fail_closed(MsgId, CurrentUid, Data, Gid, ToGID, RequiredRole) ->
    do_send_c2g(MsgId, CurrentUid, Data, Gid, ToGID, RequiredRole).

%% @private
%% @doc 执行群聊消息发送（事务外预检已通过，事务内仍会权威重验）
-spec do_send_c2g(binary(), integer(), map(), binary(), integer(), 1 | 3) ->
    ok | {reply, map()}.
do_send_c2g(MsgId, CurrentUid, Data, Gid, ToGID, RequiredRole) ->
    NowTs = elib_dt:now(),
    NowMS = elib_dt:millisecond(),
    CreatedAt = maps:get(<<"created_at">>, Data),
    CreatedAtRfc = elib_dt:to_rfc3339(CreatedAt),

    % v2.0: 从 Data 提取顶层字段
    MsgType = maps:get(<<"msg_type">>, Data, <<>>),
    Action = maps:get(<<"action">>, Data, <<>>),
    % map() | null
    E2EE = maps:get(<<"e2ee">>, Data, null),

    Payload = maps:get(<<"payload">>, Data),
    %% S0-1: 信封带 ver 字段（出站=当前版本，架构保险）
    MsgBase = #{
        <<"ver">> => ?CUR_MSG_VER,
        <<"id">> => MsgId,
        <<"type">> => <<"C2G">>,
        <<"from">> => CurrentUid,
        <<"to">> => Gid,
        <<"payload">> => Payload,
        <<"created_at">> => CreatedAtRfc,
        <<"server_ts">> => NowMS
    },
    MsgWithType =
        case MsgType of
            <<>> -> MsgBase;
            _ -> MsgBase#{<<"msg_type">> => MsgType}
        end,
    MsgWithAction =
        case Action of
            <<>> -> MsgWithType;
            _ -> MsgWithType#{<<"action">> => Action}
        end,
    MsgFull0 =
        case E2EE of
            null -> MsgWithAction;
            _ -> MsgWithAction#{<<"e2ee">> => E2EE}
        end,
    % 信封重建会丢掉 handler 盖好的 sender_did/sender_dtype；必须在编码前
    % 并回——否则 staging 行与实时投递都不带 sender_did，接收端 PFv3 双层
    % 上下文绑定恒判 context_mismatch_sender_did（与 msg_c2c_logic 同范式）。
    MsgFull = message_ds:with_sender_device(MsgFull0, Data),
    Msg2 = jsone:encode(MsgFull, [native_utf8]),

    ValidateResult =
        case imboy_policy:validate_message_write(<<"C2G">>, MsgType, Action, E2EE, Msg2) of
            ok ->
                %% 群级 fail-closed 门（P0-B B4）：e2ee_mode=1 的群拒收明文内容消息
                group_e2ee_gate(ToGID, MsgType, Action, E2EE, Msg2);
            {error, _} = PolicyErr ->
                PolicyErr
        end,
    case ValidateResult of
        ok ->
            %% agent 群触发在 do_stage_and_send_c2g 的 {ok,new} 分支内旁路（仅真正新消息）
            do_stage_and_send_c2g(
                MsgId,
                CurrentUid,
                Data,
                Gid,
                ToGID,
                RequiredRole,
                MsgType,
                Action,
                E2EE,
                MsgFull,
                Msg2,
                NowTs,
                NowMS,
                CreatedAtRfc
            );
        {error, Reason} ->
            policy_violation_reply(MsgId, Reason)
    end.

%% @doc 群级 E2EE fail-closed 门（P0-B B4）
%% e2ee_mode=1 的群拒收未加密的内容消息；群配置查询失败同样拒发（fail-closed，
%% 修掉项目欠账"E2EE fail-closed 无群级标志"）。非内容动作（撤回/已读等）放行。
%% 热路径读走 group_ds:e2ee_mode 缓存，不逐条裸查 PG。
%% 全局硬闸（storage_mode=disabled）下整门放行：部署已声明不使用 E2EE，此时仍按
%% 群标志要求密文会把"关掉 E2EE 的部署"里的老群变成发不出消息的死群。
%% 硬闸判定放在最前，且不看群标志——群标志是历史存量，关档位后它不再有权威性。
-spec group_e2ee_gate(integer(), binary(), binary(), term(), binary()) ->
    ok | {error, binary()}.
group_e2ee_gate(Gid, MsgType, Action, E2EE, Payload) ->
    case imboy_policy:e2ee_disabled() orelse not imboy_policy:content_bearing_action(Action) of
        true ->
            ok;
        false ->
            case group_ds:e2ee_mode(Gid) of
                {ok, 1} ->
                    case imboy_policy:encrypted_message_body(MsgType, E2EE, Payload) of
                        true -> ok;
                        false -> {error, <<"encrypted_message_required">>}
                    end;
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    _ = ?ERROR_LOG([group_e2ee_gate_query_failed, Gid, Reason]),
                    {error, <<"group_e2ee_check_failed">>}
            end
    end.

-spec do_stage_and_send_c2g(
    binary(),
    integer(),
    map(),
    binary(),
    integer(),
    1 | 3,
    binary(),
    binary(),
    term(),
    map(),
    binary(),
    binary(),
    integer(),
    binary()
) -> ok | {reply, map()}.
do_stage_and_send_c2g(
    MsgId,
    CurrentUid,
    Data,
    Gid,
    ToGID,
    RequiredRole,
    MsgType,
    Action,
    E2EE,
    MsgFull,
    Msg2,
    _NowTs,
    NowMS,
    CreatedAtRfc
) ->
    % 提取引用回复信息
    {ReplyToMsgId, ReplyToFromId, ReplySnippet} = extract_reply_info(Data),
    % PFv3 context binding（ADR 15 §3.3）：离线/worker 投递路径从 staging
    % 行读 sender_did，必须在入栈时落列（与 msg_c2c_logic 的 stage/11 同款）。
    SenderDid = maps:get(<<"sender_did">>, Data, <<>>),

    % 检查是否有引用信息
    StageResult =
        case {ReplyToMsgId, ReplyToFromId, ReplySnippet} of
            {<<>>, 0, <<>>} ->
                % 没有引用信息，使用常规方式
                msg_store_ds:stage(
                    <<"c2g">>,
                    MsgId,
                    MsgType,
                    Action,
                    E2EE,
                    Msg2,
                    CurrentUid,
                    ToGID,
                    CreatedAtRfc,
                    CreatedAtRfc,
                    SenderDid,
                    RequiredRole
                );
            _ ->
                % 有引用信息，需要先验证被引用的消息是否存在
                case msg_c2g_ds:find_msg_by_id(ReplyToMsgId) of
                    {ok, _OriginalMsg} ->
                        msg_store_ds:stage(
                            <<"c2g">>,
                            MsgId,
                            MsgType,
                            Action,
                            E2EE,
                            Msg2,
                            CurrentUid,
                            ToGID,
                            CreatedAtRfc,
                            CreatedAtRfc,
                            SenderDid,
                            RequiredRole
                        );
                    {error, not_found} ->
                        % 被引用的消息不存在，返回错误
                        self() ! {reply, message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, Gid)},
                        error;
                    {error, Reason} ->
                        % 兼容旧库结构：引用消息校验失败时降级处理，避免发送流程崩溃
                        ok = ?ERROR_LOG(
                            "[C2G_REPLY_LOOKUP_FAILED] MsgId=~s, ReplyToMsgId=~s, Reason=~p~n",
                            [MsgId, ReplyToMsgId, Reason]
                        ),
                        msg_store_ds:stage(
                            <<"c2g">>,
                            MsgId,
                            MsgType,
                            Action,
                            E2EE,
                            Msg2,
                            CurrentUid,
                            ToGID,
                            CreatedAtRfc,
                            CreatedAtRfc,
                            SenderDid,
                            RequiredRole
                        )
                end
        end,

    % 【关键修复】先备份到 staging 表（同步，确保消息安全）
    case StageResult of
        {ok, duplicate} ->
            % 客户端重发（未收到 SERVER_ACK）：只补发 ACK，
            % 跳过投递管道，避免全群成员重复推送
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_SERVER_ACK">>,
                    <<"in_reply_to">> => MsgId,
                    <<"server_ts">> => NowMS
                }},
            ok;
        {ok, new, ConvSeq, MemberUids} ->
            % 备份成功，继续处理
            MsLi = elib_retry_config:intervals(<<"c2g">>),
            RealtimeMsg = jsone:encode(
                MsgFull#{<<"conv_seq">> => ConvSeq}, [native_utf8]
            ),
            % 立即响应
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_SERVER_ACK">>,
                    <<"in_reply_to">> => MsgId,
                    <<"server_ts">> => NowMS
                }},

            % ① 先入队（异步，非阻塞）
            msg_store_ds:enqueue(<<"c2g">>, MsgId, #{
                payload => RealtimeMsg,
                from_id => CurrentUid,
                to_id => ToGID,
                to_id_list => MemberUids,
                created_at => CreatedAtRfc,
                server_ts => NowMS
            }),

            % ② 如果有引用信息，存储到数据库
            case {ReplyToMsgId, ReplyToFromId, ReplySnippet} of
                {<<>>, 0, <<>>} ->
                    % 没有引用信息，使用常规处理
                    ok;
                _ ->
                    % 有引用信息，存储到数据库
                    msg_c2g_ds:write_msg_with_reply(
                        CreatedAtRfc,
                        MsgId,
                        RealtimeMsg,
                        CurrentUid,
                        MemberUids,
                        ToGID,
                        MsgType,
                        E2EE,
                        ReplyToMsgId,
                        ReplyToFromId,
                        ReplySnippet
                    )
            end,

            % ③ 后投递消息（仅推送在线成员，离线成员通过 sync 拉取）
            % sender_did 与权威 conv_seq 均在实时信封内；后者来自已提交的
            % staging 事务，客户端不得从密文 payload 自报历史范围。
            OnlineUids = [
                Uid
             || Uid <- MemberUids,
                CurrentUid /= Uid,
                user_logic:is_online(Uid)
            ],
            [message_ds:send_next(Uid, MsgId, RealtimeMsg, MsLi) || Uid <- OnlineUids],

            % ③.5 离线推送（异步，不阻塞消息投递）
            push_notification_logic:maybe_push_for_c2g(CurrentUid, ToGID, MsgType, MemberUids),

            % ③.6 消息自毁：设置 expire_at（如果客户端指定了 expire_secs）
            ExpireSecs = maps:get(<<"expire_secs">>, Data, undefined),
            case msg_burn_logic:valid_expire_secs(ExpireSecs) of
                true when is_integer(ExpireSecs), ExpireSecs > 0 ->
                    ExpireAt = msg_burn_logic:calc_expire_at(CreatedAtRfc, ExpireSecs),
                    set_c2g_expire_at(MsgId, ExpireAt);
                _ ->
                    ok
            end,

            % ④ 创建@提及记录（如果有）
            Payload = maps:get(<<"payload">>, Data, #{}),
            Mentions = mentions_from_payload(Payload),
            _ =
                case Mentions of
                    [] ->
                        ok;
                    _ ->
                        CommittedMentions = committed_mentions(Mentions, MemberUids),
                        _ = mention_logic:create_mentions(
                            MsgId, ToGID, CommittedMentions, CurrentUid
                        )
                end,

            %% Phase 4 T4.2 群触发：仅对**真正新入投递管道**的消息旁路触发 @agent 回复。
            %% 放在 {ok,new} 分支内，避免 {ok,duplicate}(QoS 正常重发) / error(引用不存在未投递)
            %% 场景下误触发 agent（重复 LLM 调用/刷屏）。fire-and-forget。
            _ = ai_agent_group_reply:maybe_dispatch(CurrentUid, ToGID, Data, MemberUids),
            %% BOT-01：群内 @Bot mention 分派（防自环/E2EE fail-closed 在模块内）
            bot_webhook_logic:dispatch_group_mention(
                CurrentUid, ToGID, Data, Payload, MemberUids
            ),

            ok;
        {error, forbidden} ->
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_ERROR">>,
                    <<"error">> => <<"Not an active group member"/utf8>>,
                    <<"code">> => 403
                }},
            ok;
        {error, recipient_limit_exceeded} ->
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_ERROR">>,
                    <<"error">> => <<"Group recipient limit exceeded"/utf8>>,
                    <<"code">> => 409
                }},
            ok;
        {error, msg_id_conflict} ->
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_ERROR">>,
                    <<"error">> => <<"Message id conflicts with another message">>,
                    <<"code">> => 409
                }},
            ok;
        {error, E2EEReason} when
            E2EEReason =:= e2ee_session_unattested;
            E2EEReason =:= e2ee_session_stale;
            E2EEReason =:= e2ee_session_conflict;
            E2EEReason =:= e2ee_session_scope_mismatch;
            E2EEReason =:= e2ee_group_session_invalid;
            E2EEReason =:= e2ee_room_key_invalid;
            E2EEReason =:= e2ee_sender_device_missing;
            E2EEReason =:= e2ee_session_generation_mismatch
        ->
            {reply, ErrorReply} =
                policy_violation_reply(MsgId, atom_to_binary(E2EEReason), ToGID),
            self() ! {reply, ErrorReply},
            ok;
        {error, unavailable} ->
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2G_ERROR">>,
                    <<"error">> => <<"Group message staging failed, please retry"/utf8>>,
                    <<"code">> => 503
                }},
            ok;
        error ->
            % 已经在上面处理了错误响应
            ok
    end.

%% @private 把 @all 展开为 staging 已提交快照，并过滤普通 mention，禁止后加入者
%% 收到旧消息提醒，也禁止已退出/非群成员被旁路触达。
-spec committed_mentions(list(), [integer()]) -> [binary()].
committed_mentions(Mentions, MemberUids) ->
    case lists:member(<<"all">>, Mentions) of
        true ->
            [integer_to_binary(Uid) || Uid <- MemberUids];
        false ->
            lists:usort([
                integer_to_binary(Uid)
             || Mention <- Mentions,
                Uid <- [mention_uid(Mention)],
                lists:member(Uid, MemberUids)
            ])
    end.

-spec mention_uid(term()) -> integer().
mention_uid(Uid) when is_integer(Uid), Uid > 0 ->
    Uid;
mention_uid(Uid) when is_binary(Uid) ->
    try binary_to_integer(Uid) of
        Value when Value > 0 -> Value;
        _ -> 0
    catch
        _:_ -> 0
    end;
mention_uid(_) ->
    0.

%% 客户端确认C2G投递消息
-spec c2g_client_ack(binary(), integer(), binary()) -> ok.
c2g_client_ack(MsgId, Uid, DID) ->
    msg_ack_logic:client_ack(<<"c2g">>, MsgId, Uid, DID).

%% 客户端撤回消息 for c2g
-spec c2g_revoke(binary(), integer(), map()) -> ok | {reply, map()}.
c2g_revoke(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    %% v2.0: msg_type 和 action 提升到顶层
    RevokePayload = #{
        <<"content">> => <<>>,
        <<"original_msg_id">> => OriginalMsgId
    },
    ActionMsgExtra = #{
        <<"msg_type">> => <<"custom">>,
        <<"action">> => <<"message_revoke_ack">>
    },
    handle_group_action(MsgId, CurrentUid, Data, RevokePayload, ActionMsgExtra, revoke).

%% 客户端撤回消息确认 for c2g
-spec c2g_revoke_ack(binary(), integer(), Data :: map()) -> ok.
c2g_revoke_ack(MsgId, CurrentUid, Data) ->
    SenderDid = maps:get(<<"sender_did">>, Data, <<>>),
    msg_ack_logic:client_ack(<<"c2g">>, MsgId, CurrentUid, SenderDid).

%% 客户端编辑消息 for c2g
-spec c2g_edit(binary(), integer(), map()) -> ok | {reply, map()}.
c2g_edit(MsgId, CurrentUid, Data) ->
    case encrypted_edit_target(Data) of
        {ok, OriginalMsgId} ->
            handle_encrypted_group_edit(MsgId, CurrentUid, Data, OriginalMsgId);
        error ->
            Payload = maps:get(<<"payload">>, Data),
            OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
            NewContent = maps:get(<<"content">>, Payload),
            MsgType = maps:get(<<"msg_type">>, Payload),
            %% v2.0: msg_type 和 action 提升到顶层
            EditPayload = #{
                <<"content">> => NewContent,
                <<"original_msg_id">> => OriginalMsgId
            },
            ActionMsgExtra = #{
                <<"msg_type">> => MsgType,
                <<"action">> => <<"message_edit_ack">>
            },
            handle_group_action(MsgId, CurrentUid, Data, EditPayload, ActionMsgExtra, edit)
    end.

%% 客户端编辑消息确认 for c2g
-spec c2g_edit_ack(binary(), integer(), Data :: map()) -> ok.
c2g_edit_ack(MsgId, CurrentUid, Data) ->
    SenderDid = maps:get(<<"sender_did">>, Data, <<>>),
    msg_ack_logic:client_ack(<<"c2g">>, MsgId, CurrentUid, SenderDid).

%% @private E2EE 编辑：服务端只读取 edit_of 做权限/时间窗校验，正文密文原样转发。
-spec handle_encrypted_group_edit(binary(), integer(), map(), binary()) -> {reply, map()}.
handle_encrypted_group_edit(MsgId, CurrentUid, Data, OriginalMsgId) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    ToGID = ec_cnv:to_integer(To),
    FromId = ec_cnv:to_integer(From),
    MsgType = maps:get(<<"msg_type">>, Data, <<"text">>),
    E2EE = maps:get(<<"e2ee">>, Data, null),
    Payload = maps:get(<<"payload">>, Data, <<>>),
    PayloadBin = encrypted_payload_binary(Payload),
    case {CurrentUid =:= FromId, group_ds:is_member(CurrentUid, ToGID)} of
        {true, true} ->
            FindResult =
                case msg_c2g_ds:find_msg_by_id(OriginalMsgId) of
                    {ok, Found} -> {ok, Found};
                    _ -> msg_store_ds:find_staged(<<"c2g">>, OriginalMsgId)
                end,
            case FindResult of
                {ok, #{<<"from_id">> := FromId, <<"to_id">> := ToGID} = MsgData} ->
                    CreatedAt = maps:get(<<"created_at">>, MsgData),
                    CreatedAtMs = elib_dt:rfc3339_to(CreatedAt, millisecond),
                    NowMS = elib_dt:millisecond(),
                    WindowMs = msg_edit_window_ms(),
                    case
                        WindowMs > 0 andalso is_integer(CreatedAtMs) andalso
                            NowMS - CreatedAtMs > WindowMs
                    of
                        true ->
                            {reply, #{
                                <<"id">> => MsgId,
                                <<"type">> => <<"C2G">>,
                                <<"from">> => From,
                                <<"to">> => To,
                                <<"msg_type">> => <<"custom">>,
                                <<"action">> => <<"message_edit_error">>,
                                <<"payload">> => #{
                                    <<"original_msg_id">> => OriginalMsgId,
                                    <<"error">> => <<"超过编辑时间限制"/utf8>>,
                                    <<"code">> => ?ERR_REVOKE_TIMEOUT
                                },
                                <<"server_ts">> => NowMS
                            }};
                        false ->
                            case encrypted_edit_policy(MsgType, E2EE, PayloadBin, ToGID) of
                                ok ->
                                    ActionMsg = #{
                                        <<"id">> => MsgId,
                                        <<"type">> => <<"C2G">>,
                                        <<"from">> => From,
                                        <<"to">> => To,
                                        <<"msg_type">> => MsgType,
                                        %% 与 PFv3 protected_header.action 保持一致。
                                        <<"action">> => <<"message_edit">>,
                                        <<"e2ee">> => E2EE,
                                        <<"payload">> => Payload,
                                        <<"server_ts">> => NowMS
                                    },
                                    stage_and_deliver_group_action(
                                        MsgId,
                                        CurrentUid,
                                        Data,
                                        To,
                                        ToGID,
                                        OriginalMsgId,
                                        MsgType,
                                        <<"message_edit">>,
                                        E2EE,
                                        ActionMsg
                                    );
                                {error, Reason} ->
                                    policy_violation_reply(MsgId, Reason)
                            end
                    end;
                {ok, _} ->
                    {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To)};
                _ ->
                    {reply, message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, To)}
            end;
        {false, _} ->
            {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To)};
        {_, false} ->
            {reply, message_ds:assemble_s2c(MsgId, <<"not_group_member">>, To)}
    end.

-spec encrypted_edit_policy(binary(), term(), binary(), integer()) -> ok | {error, binary()}.
encrypted_edit_policy(MsgType, E2EE, Payload, Gid) ->
    case
        imboy_policy:validate_message_write(
            <<"C2G">>, MsgType, <<"message_edit">>, E2EE, Payload
        )
    of
        ok -> group_e2ee_gate(Gid, MsgType, <<"message_edit">>, E2EE, Payload);
        {error, _} = Error -> Error
    end.

-spec encrypted_edit_target(map()) -> {ok, binary()} | error.
encrypted_edit_target(Data) ->
    case maps:get(<<"e2ee">>, Data, null) of
        #{<<"edit_of">> := OriginalMsgId} when is_binary(OriginalMsgId), OriginalMsgId =/= <<>> ->
            {ok, OriginalMsgId};
        _ ->
            error
    end.

-spec encrypted_payload_binary(term()) -> binary().
encrypted_payload_binary(Payload) when is_binary(Payload) ->
    Payload;
encrypted_payload_binary(Payload) when is_map(Payload) ->
    jsone:encode(Payload, [native_utf8]);
encrypted_payload_binary(_) ->
    <<>>.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 统一的群组消息操作处理（撤回、编辑等）
%% 验证权限、构建消息、发送给群成员
%% @private
%% v2.0: 支持 ActionMsgExtra 参数（包含 msg_type/action）
-spec handle_group_action(binary(), integer(), map(), map(), map(), atom()) -> {reply, map()}.
handle_group_action(MsgId, CurrentUid, Data, ActionPayload, ActionMsgExtra, ActionType) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    ToGID = ec_cnv:to_integer(To),
    FromId = ec_cnv:to_integer(From),
    ok = ?DEBUG_LOG([From, To, ToGID, CurrentUid, Data]),

    % 验证权限：只能操作自己发送的消息，且必须是群成员
    case {CurrentUid =:= FromId, group_ds:is_member(CurrentUid, ToGID)} of
        {true, true} ->
            %% 【新增】检查消息是否存在并验证时间限制
            OriginalMsgId =
                case ActionType of
                    revoke -> maps:get(<<"original_msg_id">>, maps:get(<<"payload">>, Data));
                    edit -> maps:get(<<"original_msg_id">>, maps:get(<<"payload">>, Data))
                end,

            %% 正式表查不到时兜底查 staging（秒撤竞态：消息仍在异步管道内）
            FindResult =
                case msg_c2g_ds:find_msg_by_id(OriginalMsgId) of
                    {ok, Found} -> {ok, Found};
                    _ -> msg_store_ds:find_staged(<<"c2g">>, OriginalMsgId)
                end,
            case FindResult of
                {ok, MsgData} ->
                    %% 检查消息的发送者是否为当前用户
                    case MsgData of
                        #{<<"from_id">> := FromId, <<"to_id">> := ToGID} ->
                            CreatedAt = maps:get(<<"created_at">>, MsgData),
                            CreatedAtMs = elib_dt:rfc3339_to(CreatedAt, millisecond),
                            NowMS = elib_dt:millisecond(),

                            % 撤回受 2 分钟时限约束；编辑受独立编辑时间窗约束（默认 24h，<=0 不限）
                            % ponytail: guard on integer to avoid badarith when CreatedAt is empty/invalid
                            % ceiling: elib_dt:rfc3339_to/1 returns null for empty/unparsable
                            %   input, so the window check is skipped and the revoke/edit is
                            %   allowed through (fail-open on bad timestamps)
                            % upgrade: if the window becomes a compliance/risk-control rule
                            %   rather than a UX guard, switch to fail-closed (reject on null)
                            {WindowMs, ErrAction, ErrText} =
                                case ActionType of
                                    revoke ->
                                        {?REVOKE_TIMEOUT_MS, <<"message_revoke_error">>,
                                            <<"超过撤回时间限制(2分钟)"/utf8>>};
                                    edit ->
                                        {
                                            msg_edit_window_ms(),
                                            <<"message_edit_error">>,
                                            <<"超过编辑时间限制"/utf8>>
                                        }
                                end,
                            case
                                WindowMs > 0 andalso
                                    is_integer(CreatedAtMs) andalso
                                    NowMS - CreatedAtMs > WindowMs
                            of
                                true ->
                                    % 超过操作时间限制
                                    ErrorMsg = #{
                                        <<"id">> => MsgId,
                                        <<"type">> => <<"C2G">>,
                                        <<"from">> => From,
                                        <<"to">> => To,
                                        <<"msg_type">> => <<"custom">>,
                                        <<"action">> => ErrAction,
                                        <<"payload">> => #{
                                            <<"content">> => <<>>,
                                            <<"original_msg_id">> => OriginalMsgId,
                                            <<"error">> => ErrText,
                                            <<"code">> => ?ERR_REVOKE_TIMEOUT
                                        },
                                        <<"server_ts">> => NowMS
                                    },
                                    {reply, ErrorMsg};
                                false ->
                                    MsgType = maps:get(
                                        <<"msg_type">>, ActionMsgExtra, <<"custom">>
                                    ),
                                    Action = maps:get(<<"action">>, ActionMsgExtra, <<>>),
                                    E2EE = maps:get(<<"e2ee">>, ActionMsgExtra, null),
                                    ActionPayloadJson = jsone:encode(ActionPayload, [native_utf8]),
                                    ActionMsg = maps:merge(
                                        #{
                                            <<"id">> => MsgId,
                                            <<"type">> => <<"C2G">>,
                                            <<"from">> => From,
                                            <<"to">> => To,
                                            <<"payload">> => ActionPayload#{
                                                <<"revoked_at">> => NowMS,
                                                <<"edited_at">> => NowMS
                                            },
                                            <<"server_ts">> => NowMS
                                        },
                                        ActionMsgExtra
                                    ),
                                    case
                                        validate_plain_group_action(
                                            ActionType, ToGID, MsgType, Data, ActionPayloadJson
                                        )
                                    of
                                        ok ->
                                            stage_and_deliver_group_action(
                                                MsgId,
                                                CurrentUid,
                                                Data,
                                                To,
                                                ToGID,
                                                OriginalMsgId,
                                                MsgType,
                                                Action,
                                                E2EE,
                                                ActionMsg
                                            );
                                        {error, Reason} ->
                                            policy_violation_reply(MsgId, Reason)
                                    end
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
        {false, _} ->
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg};
        {_, false} ->
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"not_group_member">>, To),
            {reply, ErrorMsg}
    end.

-spec validate_plain_group_action(atom(), integer(), binary(), map(), binary()) ->
    ok | {error, binary()}.
validate_plain_group_action(revoke, _Gid, _MsgType, _Data, _Payload) ->
    ok;
validate_plain_group_action(edit, Gid, MsgType, Data, Payload) ->
    E2EE = maps:get(<<"e2ee">>, Data, null),
    case
        imboy_policy:validate_message_write(<<"C2G">>, MsgType, <<"message_edit">>, E2EE, Payload)
    of
        ok -> group_e2ee_gate(Gid, MsgType, <<"message_edit">>, E2EE, Payload);
        {error, _} = Error -> Error
    end.

-spec stage_and_deliver_group_action(
    binary(), integer(), map(), binary(), integer(), binary(), binary(), binary(), term(), map()
) -> {reply, map()}.
stage_and_deliver_group_action(
    MsgId, CurrentUid, Data, To, ToGID, OriginalMsgId, MsgType, Action, E2EE, ActionMsg
) ->
    TrustedActionMsg = message_ds:with_sender_device(ActionMsg, Data),
    ActionMsgJson = jsone:encode(TrustedActionMsg, [native_utf8]),
    ActionCreatedAt = elib_dt:now(),
    SenderDid = maps:get(<<"sender_did">>, Data, <<>>),
    case
        msg_store_ds:stage_action(
            <<"c2g">>,
            MsgId,
            MsgType,
            Action,
            E2EE,
            ActionMsgJson,
            CurrentUid,
            ToGID,
            ActionCreatedAt,
            ActionCreatedAt,
            SenderDid,
            1,
            OriginalMsgId
        )
    of
        {ok, new, ConvSeq, MemberUids} ->
            RealtimeAction = TrustedActionMsg#{<<"conv_seq">> => ConvSeq},
            RealtimeActionJson = jsone:encode(RealtimeAction, [native_utf8]),
            msg_store_ds:enqueue(<<"c2g">>, MsgId, #{
                payload => RealtimeActionJson,
                from_id => CurrentUid,
                to_id => ToGID,
                to_id_list => MemberUids,
                created_at => ActionCreatedAt,
                server_ts => maps:get(<<"server_ts">>, TrustedActionMsg)
            }),
            maybe_cancel_revoke_timers(Action, OriginalMsgId, CurrentUid, MemberUids),
            MsLi = elib_retry_config:intervals(<<"c2g">>),
            [
                message_ds:send_next(Uid, MsgId, RealtimeActionJson, MsLi)
             || Uid <- MemberUids, Uid =/= CurrentUid
            ],
            {reply, RealtimeAction};
        {ok, duplicate} ->
            {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2G_SERVER_ACK">>,
                <<"in_reply_to">> => MsgId,
                <<"server_ts">> => maps:get(<<"server_ts">>, TrustedActionMsg)
            }};
        {error, msg_id_conflict} ->
            {reply, message_ds:assemble_s2c(MsgId, <<"invalid_msgid">>, To)};
        {error, action_target_forbidden} ->
            {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To)};
        {error, E2EEReason} when
            E2EEReason =:= e2ee_session_unattested;
            E2EEReason =:= e2ee_session_stale;
            E2EEReason =:= e2ee_session_conflict;
            E2EEReason =:= e2ee_session_scope_mismatch;
            E2EEReason =:= e2ee_group_session_invalid;
            E2EEReason =:= e2ee_room_key_invalid;
            E2EEReason =:= e2ee_sender_device_missing;
            E2EEReason =:= e2ee_session_generation_mismatch
        ->
            policy_violation_reply(MsgId, atom_to_binary(E2EEReason), ToGID);
        {error, _} ->
            {reply, message_ds:assemble_s2c(MsgId, <<"service_unavailable">>, To)};
        error ->
            {reply, message_ds:assemble_s2c(MsgId, <<"service_unavailable">>, To)}
    end.

-spec maybe_cancel_revoke_timers(binary(), binary(), integer(), [integer()]) -> ok.
maybe_cancel_revoke_timers(<<"message_revoke_ack">>, OriginalMsgId, CurrentUid, MemberUids) ->
    _ = [
        websocket_logic:cancel_timer(Uid, DID, OriginalMsgId)
     || Uid <- MemberUids,
        Uid =/= CurrentUid,
        DID <- user_device_logic:online_dids(Uid)
    ],
    ok;
maybe_cancel_revoke_timers(_, _, _, _) ->
    ok.

%% @doc 获取群消息已读统计
%% 检查用户是否有权限访问该群消息，并返回已读和总人数
%%
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @return {ok, ReadCount, TotalCount} | {error, Reason}
%% @end
-spec read_stats(binary(), integer()) -> {ok, integer(), integer()} | {error, atom()}.
read_stats(MsgId, CurrentUid) ->
    % 首先从 msg_c2g_timeline 表获取群组ID
    case msg_c2g_ds:timeline_find_by_msg_id(MsgId) of
        {ok, []} ->
            % 消息不存在
            {error, not_found};
        {ok, [#{<<"to_gid">> := Gid} | _]} ->
            % 检查用户是否是群成员
            case group_ds:is_member(CurrentUid, Gid) of
                true ->
                    % 获取群消息总成员数
                    TotalCount = length(group_ds:member_uids(Gid)),

                    % 获取已读人数
                    ReadCount = msg_c2g_ds:count_read(MsgId),

                    {ok, ReadCount, TotalCount};
                false ->
                    % 不是群成员，无权限访问
                    {error, permission_denied}
            end;
        {ok, _} ->
            % timeline 行缺 to_gid 键或格式异常
            {error, not_found};
        {error, Reason} ->
            % 查询错误
            {error, Reason}
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
                        case msg_c2g_ds:find_msg_by_id(ReplyToMsgId) of
                            {ok, OriginalMsg} ->
                                %% RT-P2-05/RT-P3-03（2026-08-27）：E2EE 短路前置——
                                %% 引用的是加密消息时摘要一律占位，不依赖能否解码
                                %% （外壳 JSON 可解码也无明文 content）。
                                %% jsonb 经驱动可能返回文本 binary 或 map，
                                %% 故用「存在且非空」而非结构判定。
                                case maps:get(<<"e2ee">>, OriginalMsg, undefined) of
                                    Undefined when
                                        Undefined =:= undefined;
                                        Undefined =:= null;
                                        Undefined =:= <<>>
                                    ->
                                        extract_snippet_plain(OriginalMsg);
                                    _ ->
                                        <<"[encrypted]"/utf8>>
                                end;
                            _ ->
                                <<>>
                        end
                end,
            {ReplyToMsgId, ReplyToFromId, ReplySnippet};
        _ ->
            {<<>>, 0, <<>>}
    end.

%% @private 非 E2EE 原有摘要提取逻辑（decode 失败退回密文碎片截取仅适用历史行）
extract_snippet_plain(OriginalMsg) ->
    Payload = maps:get(<<"payload">>, OriginalMsg, <<>>),
    try jsone:decode(Payload) of
        PayloadMap when is_map(PayloadMap) ->
            Content = maps:get(<<"content">>, PayloadMap, <<>>),
            Snippet = binary:part(Content, {0, min(byte_size(Content), 50)}),
            case byte_size(Content) > 50 of
                true -> <<Snippet/binary, "..."/utf8>>;
                false -> Snippet
            end;
        _ ->
            <<>>
    catch
        _:_ ->
            Snippet = binary:part(Payload, {0, min(byte_size(Payload), 50)}),
            case byte_size(Payload) > 50 of
                true -> <<Snippet/binary, "..."/utf8>>;
                false -> Snippet
            end
        %% @doc 设置C2G消息的自毁时间
    end.
%% @param MsgId 消息ID
%% @param ExpireAt 过期时间（RFC3339 binary）
-spec set_c2g_expire_at(binary(), binary()) -> ok.
set_c2g_expire_at(MsgId, ExpireAt) ->
    msg_c2g_ds:set_expire_at(MsgId, ExpireAt).
