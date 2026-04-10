-module(msg_c2g_logic).

%%%
%  msg_c2g 业务逻辑模块
%%%
-export([c2g/3]).
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

% 抑制 Dialyzer 类型推断警告 - elib_dt:rfc3339_to 的返回类型复杂
-dialyzer({nowarn_function, [parse_timestamp_or_default/2, ensure_integer/1]}).

-define(REVOKE_TIMEOUT_MS, 120000). % 2分钟

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

%% @private
%% @doc 解析时间戳或返回默认值
%% 使用 try-catch 来处理所有可能的返回类型
-spec parse_timestamp_or_default(binary() | list(), any()) -> integer().
parse_timestamp_or_default(Val, Default) ->
    try
        Result = elib_dt:rfc3339_to(Val, millisecond),
        case Result of
            {error, _} -> ensure_integer(Default);
            Val2 when is_integer(Val2) -> Val2;
            _ -> ensure_integer(Default)
        end
    catch
        _:_ -> ensure_integer(Default)
    end.

%% @private
%% @doc 确保值是整数
%% 使用条件表达式而不是 guard
-spec ensure_integer(any()) -> integer().
ensure_integer(Val) ->
    case is_integer(Val) of
        true -> Val;
        false -> elib_dt:millisecond()
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

    % 检查是否被禁言
    case group_member_logic:check_mute(ToGID, CurrentUid) of
        true ->
            _ = ?WARN_LOG("用户 ~p 在群组 ~p 中被禁言，无法发送消息", [CurrentUid, ToGID]),
            self() ! {reply, #{
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
                                    MemberUids = group_ds:member_uids(ToGID),
                                    do_send_c2g(MsgId, CurrentUid, Data, Gid, ToGID, MemberUids);
                                false ->
                                    _ = ?WARN_LOG("用户 ~p 尝试使用 @所有人功能但没有管理员权限", [CurrentUid]),
                                    self() ! {reply, #{
                                        <<"id">> => MsgId,
                                        <<"type">> => <<"C2G_ERROR">>,
                                        <<"error">> => <<"Permission denied: @all mentions require admin role"/utf8>>,
                                        <<"code">> => 403
                                    }},
                                    ok
                            end;
                        false ->
                            MemberUids = group_ds:member_uids(ToGID),
                            do_send_c2g(MsgId, CurrentUid, Data, Gid, ToGID, MemberUids)
                    end;
                false ->
                    _ = ?WARN_LOG("用户 ~p 尝试向非成员群组 ~p 发送消息", [CurrentUid, ToGID]),
                    self() ! {reply, #{
                        <<"id">> => MsgId,
                        <<"type">> => <<"C2G_ERROR">>,
                        <<"error">> => <<"Not a group member"/utf8>>,
                        <<"code">> => 403
                    }},
                    ok
            end
    end.

%% @private
%% @doc 执行群聊消息发送（已通过权限检查）
-spec do_send_c2g(binary(), integer(), map(), binary(), integer(), [integer()]) -> ok | {reply, map()}.
do_send_c2g(MsgId, CurrentUid, Data, Gid, ToGID, MemberUids) ->
    NowTs = elib_dt:now(),
    NowMS = elib_dt:rfc3339_to(NowTs, millisecond),
    CreatedAt = maps:get(<<"created_at">>, Data),
    CreatedAtRfc = elib_dt:to_rfc3339(CreatedAt),

    % v2.0: 从 Data 提取顶层字段
    MsgType = maps:get(<<"msg_type">>, Data, <<>>),
    Action = maps:get(<<"action">>, Data, <<>>),
    E2EE = maps:get(<<"e2ee">>, Data, null), % map() | null

    Payload = maps:get(<<"payload">>, Data),
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2G">>,
        <<"from">> => CurrentUid,
        <<"to">> => Gid,
        <<"payload">> => Payload,
        <<"created_at">> => CreatedAtRfc,
        <<"server_ts">> => NowMS
    },
    Msg2 = jsone:encode(Msg, [native_utf8]),

    case imboy_policy:validate_message_write(<<"C2G">>, MsgType, Action, E2EE, Msg2) of
        ok ->
            do_stage_and_send_c2g(
                MsgId, CurrentUid, Data, Gid, ToGID, MemberUids,
                MsgType, Action, E2EE, Msg2, NowTs, NowMS, CreatedAtRfc
            );
        {error, Reason} ->
            policy_violation_reply(MsgId, Reason)
    end.

-spec do_stage_and_send_c2g(
    binary(), integer(), map(), binary(), integer(), [integer()],
    binary(), binary(), term(), binary(), term(), integer(), binary()
) -> ok | {reply, map()}.
do_stage_and_send_c2g(
    MsgId, CurrentUid, Data, Gid, ToGID, MemberUids,
    MsgType, Action, E2EE, Msg2, NowTs, NowMS, CreatedAtRfc
) ->

    % 提取引用回复信息
    {ReplyToMsgId, ReplyToFromId, ReplySnippet} = extract_reply_info(Data),

    % 检查是否有引用信息
    StageResult = case {ReplyToMsgId, ReplyToFromId, ReplySnippet} of
        {<<>>, 0, <<>>} ->
            % 没有引用信息，使用常规方式
            msg_store_ds:stage(
                <<"c2g">>, MsgId, MsgType, Action, E2EE, Msg2,
                CurrentUid, MemberUids, CreatedAtRfc, CreatedAtRfc);
        _ ->
            % 有引用信息，需要先验证被引用的消息是否存在
            case msg_c2g_repo:find_msg_by_id(ReplyToMsgId) of
                {ok, _OriginalMsg} ->
                    msg_store_ds:stage(
                        <<"c2g">>, MsgId, MsgType, Action, E2EE, Msg2,
                        CurrentUid, MemberUids, CreatedAtRfc, CreatedAtRfc);
                {error, not_found} ->
                    % 被引用的消息不存在，返回错误
                    self() ! {reply, message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, Gid)},
                    error;
                {error, Reason} ->
                    % 兼容旧库结构：引用消息校验失败时降级处理，避免发送流程崩溃
                    ok = ?ERROR_LOG("[C2G_REPLY_LOOKUP_FAILED] MsgId=~s, ReplyToMsgId=~s, Reason=~p~n",
                               [MsgId, ReplyToMsgId, Reason]),
                    msg_store_ds:stage(
                        <<"c2g">>, MsgId, MsgType, Action, E2EE, Msg2,
                        CurrentUid, MemberUids, CreatedAtRfc, CreatedAtRfc);
                Other ->
                    ok = ?ERROR_LOG("[C2G_REPLY_LOOKUP_UNEXPECTED] MsgId=~s, ReplyToMsgId=~s, Result=~p~n",
                               [MsgId, ReplyToMsgId, Other]),
                    msg_store_ds:stage(
                        <<"c2g">>, MsgId, MsgType, Action, E2EE, Msg2,
                        CurrentUid, MemberUids, CreatedAtRfc, CreatedAtRfc)
            end
    end,

    % 【关键修复】先备份到 staging 表（同步，确保消息安全）
    case StageResult of
        ok ->
            % 备份成功，继续处理
            MsLi = elib_retry_config:intervals(<<"c2g">>),
            % 立即响应
            self() ! {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2G_SERVER_ACK">>,
                <<"server_ts">> => NowMS
            }},

            % ① 先入队（异步，非阻塞）
            msg_store_ds:enqueue(<<"c2g">>, MsgId, #{
                payload => Msg2,
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
                        NowTs, MsgId, Msg2, CurrentUid, MemberUids, ToGID,
                        MsgType, E2EE, ReplyToMsgId, ReplyToFromId, ReplySnippet)
            end,

            % ③ 后投递消息（仅推送在线成员，离线成员通过 sync 拉取）
            OnlineUids = [Uid || Uid <- MemberUids,
                          CurrentUid /= Uid,
                          user_logic:is_online(Uid)],
            [message_ds:send_next(Uid, MsgId, Msg2, MsLi) || Uid <- OnlineUids],

            % ③.5 离线推送（异步，不阻塞消息投递）
            push_notification_logic:maybe_push_for_c2g(CurrentUid, ToGID, MsgType, MemberUids),

            % ③.6 消息自毁：设置 expire_at（如果客户端指定了 expire_secs）
            ExpireSecs = maps:get(<<"expire_secs">>, Data, undefined),
            case msg_burn_logic:valid_expire_secs(ExpireSecs) of
                true when is_integer(ExpireSecs), ExpireSecs > 0 ->
                    ExpireAt = msg_burn_logic:calc_expire_at(CreatedAtRfc, ExpireSecs),
                    set_c2g_expire_at(MsgId, ExpireAt);
                _ -> ok
            end,

            % ④ 创建@提及记录（如果有）
            Payload = maps:get(<<"payload">>, Data, #{}),
            Mentions = mentions_from_payload(Payload),
            case Mentions of
                [] -> ok;
                _ -> mention_logic:create_mentions(MsgId, ToGID, Mentions, CurrentUid)
            end,

            ok;
        error ->
            % 已经在上面处理了错误响应
            ok
    end.

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
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    ok = ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId]),
    AckPayload = Payload#{
        <<"action">> => <<"message_revoke_ack">>,
        <<"ack_msg_id">> => MsgId,
        <<"ack_uid">> => CurrentUid,
        <<"ack_at">> => elib_dt:millisecond()
    },
    persist_action_payload(OriginalMsgId, AckPayload),
    ok.

%% 客户端编辑消息 for c2g
-spec c2g_edit(binary(), integer(), map()) -> ok | {reply, map()}.
c2g_edit(MsgId, CurrentUid, Data) ->
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
    handle_group_action(MsgId, CurrentUid, Data, EditPayload, ActionMsgExtra, edit).

%% 客户端编辑消息确认 for c2g
-spec c2g_edit_ack(binary(), integer(), Data :: map()) -> ok.
c2g_edit_ack(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
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
            OriginalMsgId = case ActionType of
                revoke -> maps:get(<<"original_msg_id">>, maps:get(<<"payload">>, Data));
                edit -> maps:get(<<"original_msg_id">>, maps:get(<<"payload">>, Data))
            end,

            case msg_c2g_ds:find_msg_by_id(OriginalMsgId) of
                {ok, MsgData} ->
                    %% 检查消息的发送者是否为当前用户
                    case MsgData of
                        #{<<"from_id">> := FromId} ->
                            CreatedAt = maps:get(<<"created_at">>, MsgData),
                            CreatedAtMs = elib_dt:rfc3339_to(CreatedAt, millisecond),
                            NowMS = elib_dt:millisecond(),

                            % 编辑沿用权限校验，但不受撤回时限约束
                            case ActionType =:= revoke andalso NowMS - CreatedAtMs > ?REVOKE_TIMEOUT_MS of
                                true ->
                                    % 超过撤回时间限制
                                    ErrorMsg = #{
                                        <<"id">> => MsgId,
                                        <<"type">> => <<"C2G">>,
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
                                        <<"server_ts">> => NowMS
                                    },
                                    {reply, ErrorMsg};
                                false ->
                                    % 未超过时间限制，继续原有逻辑
                                    NowTs = elib_dt:now(),
                                    % v2.0: 存储离线消息时分离 payload、msg_type 和 action
                                    MsgType = maps:get(<<"msg_type">>, ActionMsgExtra, <<"custom">>),
                                    Action = maps:get(<<"action">>, ActionMsgExtra, <<>>),
                                    E2EE = maps:get(<<"e2ee">>, ActionMsgExtra, null), % map() | null
                                    ActionPayloadJson = jsone:encode(ActionPayload, [native_utf8]),

                                    case ActionType of
                                        revoke ->
                                            MemberUids = group_ds:member_uids(ToGID),
                                            % 构建操作消息（v2.0 格式）
                                            %% msg_type 和 action 从 ActionMsgExtra 提取到顶层
                                            ActionMsg = maps:merge(#{
                                                <<"id">> => MsgId,
                                                <<"type">> => <<"C2G">>,
                                                <<"from">> => From,
                                                <<"to">> => To,
                                                <<"payload">> => ActionPayload#{<<"revoked_at">> => NowMS, <<"edited_at">> => NowMS},
                                                <<"server_ts">> => NowMS
                                            }, ActionMsgExtra),
                                            ActionMsgJson = jsone:encode(ActionMsg, [native_utf8]),
                                            MsLi = elib_retry_config:intervals(<<"c2g">>),

                                            % 发送给群组其他成员
                                            [message_ds:send_next(Uid, MsgId, ActionMsgJson, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],

                                            % 根据操作类型调用相应的 v2.0 函数
                                            msg_c2g_ds:revoke_offline_msg(
                                                ActionPayloadJson, NowTs, MsgId, CurrentUid, MemberUids, ToGID, MsgType, Action, E2EE
                                            ),
                                            {reply, ActionMsg};
                                        edit ->
                                            case imboy_policy:validate_message_write(
                                                <<"C2G">>,
                                                MsgType,
                                                <<"message_edit">>,
                                                maps:get(<<"e2ee">>, Data, null),
                                                ActionPayloadJson
                                            ) of
                                                ok ->
                                                    MemberUids = group_ds:member_uids(ToGID),
                                                    ActionMsg = maps:merge(#{
                                                        <<"id">> => MsgId,
                                                        <<"type">> => <<"C2G">>,
                                                        <<"from">> => From,
                                                        <<"to">> => To,
                                                        <<"payload">> => ActionPayload#{<<"revoked_at">> => NowMS, <<"edited_at">> => NowMS},
                                                        <<"server_ts">> => NowMS
                                                    }, ActionMsgExtra),
                                                    ActionMsgJson = jsone:encode(ActionMsg, [native_utf8]),
                                                    MsLi = elib_retry_config:intervals(<<"c2g">>),

                                                    % 发送给群组其他成员
                                                    [message_ds:send_next(Uid, MsgId, ActionMsgJson, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],

                                                    msg_c2g_ds:edit_offline_msg(
                                                        ActionPayloadJson, NowTs, MsgId, CurrentUid, MemberUids, ToGID
                                                    ),
                                                    {reply, ActionMsg};
                                                {error, Reason} ->
                                                    policy_violation_reply(MsgId, Reason)
                                            end
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
    case msg_c2g_timeline_repo:find_by_msg_id(MsgId) of
        {ok, []} ->
            % 消息不存在
            {error, not_found};
        {ok, [#{<<"to_gid">> := Gid}]} ->
            % 检查用户是否是群成员
            case group_ds:is_member(CurrentUid, Gid) of
                true ->
                    % 获取群消息总成员数
                    TotalCount = length(group_ds:member_uids(Gid)),

                    % 获取已读人数
                    ReadCount = msg_c2g_repo:count_read(MsgId),

                    {ok, ReadCount, TotalCount};
                false ->
                    % 不是群成员，无权限访问
                    {error, permission_denied}
            end;
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
            ReplySnippet = case ReplyToMsgId of
                <<>> -> <<>>;
                _ ->
                    case msg_c2g_repo:find_msg_by_id(ReplyToMsgId) of
                        {ok, OriginalMsg} ->
                            Payload = maps:get(<<"payload">>, OriginalMsg, <<>>),
                            % 尝试解析 JSON 并提取 content 字段
                            try jsone:decode(Payload) of
                                PayloadMap when is_map(PayloadMap) ->
                                    Content = maps:get(<<"content">>, PayloadMap, <<>>),
                                    % 截取前50个字符作为摘要
                                    Snippet = binary:part(Content, {0, min(byte_size(Content), 50)}),
                                    case byte_size(Content) > 50 of
                                        true -> <<Snippet/binary, "..."/utf8>>;
                                        false -> Snippet
                                    end;
                                _ ->
                                    <<>>
                            catch
                                _:_ ->
                                    % 如果解析失败，截取原始 payload 的前50个字符
                                    Snippet = binary:part(Payload, {0, min(byte_size(Payload), 50)}),
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

%% @doc 设置C2G消息的自毁时间
%% @param MsgId 消息ID
%% @param ExpireAt 过期时间（RFC3339 binary）
-spec set_c2g_expire_at(binary(), binary()) -> ok.
set_c2g_expire_at(MsgId, ExpireAt) ->
    Tb = msg_c2g_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary,
           " SET expire_at = $1"
           " WHERE msg_id = $2">>,
    case elib_pg:execute(Sql, [ExpireAt, MsgId]) of
        {ok, _} -> ok;
        {error, Reason} ->
            _ = ?WARN_LOG({set_c2g_expire_at_failed, MsgId, Reason}),
            ok
    end.

%% @doc 持久化 action ack payload 到原消息记录
%% 原消息若已被客户端 ACK 清理，更新影响行数为 0，不视为错误。
-spec persist_action_payload(binary(), map()) -> ok.
persist_action_payload(<<>>, _Payload) ->
    ok;
persist_action_payload(OriginalMsgId, Payload) when is_binary(OriginalMsgId), is_map(Payload) ->
    PayloadJson = imboy_message_helper:encode_json(Payload),
    case msg_c2g_repo:update_payload_by_msg_id(OriginalMsgId, PayloadJson) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            _ = ?WARN_LOG({persist_action_payload_failed, OriginalMsgId, Reason}),
            ok
    end.
