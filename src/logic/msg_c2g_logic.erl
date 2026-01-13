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

-include("chat.hrl").
-include("log.hrl").

% 抑制 Dialyzer 类型推断警告 - elib_dt:rfc3339_to 的返回类型复杂
-dialyzer({nowarn_function, [parse_timestamp_or_default/2, ensure_integer/1]}).

%% ===================================================================
%% Internal Functions
%% ===================================================================

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


%% ===================================================================
%% API
%% ===================================================================

%% 群聊发送消息
-spec c2g(binary(), integer(), map()) -> ok | {reply, map()}.
c2g(MsgId, CurrentUid, Data) ->
    Gid = maps:get(<<"to">>, Data),
    ToGID = elib_hashids:decode(Gid),

    % 检查是否是群成员
    case group_ds:is_member(CurrentUid, ToGID) of
        true ->
            MemberUids = group_ds:member_uids(ToGID),
            do_send_c2g(MsgId, CurrentUid, Data, Gid, ToGID, MemberUids);
        false ->
            _ = ?WARN_LOG("用户 ~p 尝试向非成员群组 ~p 发送消息", [CurrentUid, ToGID]),
            self() ! {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2G_ERROR">>,
                <<"error">> => <<"Not a group member"/utf8>>,
                <<"code">> => 403
            }},
            ok
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
    E2EE = maps:get(<<"e2ee">>, Data, <<>>),

    Payload = maps:get(<<"payload">>, Data),
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2G">>,
        <<"from">> => elib_hashids:encode(CurrentUid),
        <<"to">> => Gid,
        <<"payload">> => Payload,
        <<"created_at">> => CreatedAtRfc,
        <<"server_ts">> => NowMS
    },
    Msg2 = jsone:encode(Msg, [native_utf8]),

    % v2.0: 直接传递 MsgType/Action/E2EE 参数
    StageResult = msg_store_ds:stage(
        <<"c2g">>, MsgId, MsgType, Action, E2EE, Msg2,
        CurrentUid, MemberUids, CreatedAtRfc, CreatedAtRfc),
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

            % ② 后投递消息（给每个群成员）
            [message_ds:send_next(Uid, MsgId, Msg2, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],

            ok;
        error ->
            % 备份失败，返回错误
            ok = ?ERROR_LOG("[C2G_STAGE_FAILED] MsgId=~s, FromUid=~p, Gid=~s~n",
                       [MsgId, CurrentUid, Gid]),
            {reply, message_ds:assemble_s2c(MsgId, <<"internal_error">>, Gid)}
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

    % 更新本地消息状态为已撤销
    % 这里可以添加数据库更新逻辑
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

    % 更新本地消息内容
    % 这里可以添加数据库更新逻辑
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
    ToGID = elib_hashids:decode(To),
    FromId = elib_hashids:decode(From),
    ok = ?DEBUG_LOG([From, To, ToGID, CurrentUid, Data]),

    % 验证权限：只能操作自己发送的消息，且必须是群成员
    case {CurrentUid =:= FromId, group_ds:is_member(ToGID, CurrentUid)} of
        {true, true} ->
            NowTs = elib_dt:now(),
            NowMS = elib_dt:millisecond(),
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

            % v2.0: 存储离线消息时分离 payload、msg_type 和 action
            MsgType = maps:get(<<"msg_type">>, ActionMsgExtra, <<"custom">>),
            Action = maps:get(<<"action">>, ActionMsgExtra, <<>>),
            E2EE = maps:get(<<"e2ee">>, ActionMsgExtra, <<>>),
            ActionPayloadJson = jsone:encode(ActionPayload, [native_utf8]),

            % 根据操作类型调用相应的 v2.0 函数
            case ActionType of
                revoke ->
                    msg_c2g_ds:revoke_offline_msg(ActionPayloadJson, NowTs, MsgId, CurrentUid, MemberUids, ToGID, MsgType, Action, E2EE);
                edit ->
                    msg_c2g_ds:edit_offline_msg(ActionPayloadJson, NowTs, MsgId, CurrentUid, MemberUids, ToGID)
            end,

            {reply, ActionMsg};
        {false, _} ->
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg};
        {_, false} ->
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"not_group_member">>, To),
            {reply, ErrorMsg}
    end.
