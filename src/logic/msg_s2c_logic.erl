-module(msg_s2c_logic).

%%%
%  msg_c2c 业务逻辑模块
%%%

-export([s2c/4]).
-export([s2c_client_ack/3]).

-include("chat.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% 系统消息
-spec s2c(binary(), binary(), integer() | binary(), map()) -> ok | {reply, map()}.
s2c(<<"C2C_DEL_EVERYONE">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    To = maps:get(<<"to">>, Data),
    % ToId = ec_cnv:to_integer(To),

    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    ok = ?DEBUG_LOG([CurrentUid, To, Data]),
    NowTs = elib_dt:now(),

    % 删除原有消息 - 使用 DS 层接口
    ok = msg_operation_ds:delete_c2c_msg(OldMsgId, CurrentUid),
    % 数据库会自动删除 相关 msg_c2g_timeline

    % 按策略发送消息
    From = CurrentUid,
    Action = <<"C2C_DEL_EVERYONE">>,
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId, <<>>, Action, null),
    % ?DEBUG_LOG(Msg),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    % 【改进】存储消息到队列（备份表 + shq 队列）
    CreatedAtRfc = elib_dt:to_rfc3339(NowTs),

    % 【修复】将 Payload 转换为 JSON binary
    PayloadJson = jsone:encode(Payload, [native_utf8]),

    % 写入备份表（同步，快速；备份失败不再静默继续投递，与 c2c/c2g/c2s 对齐）
    % v2.0: S2C 消息使用 action 字段
    case
        msg_store_ds:stage(
            <<"s2c">>,
            MsgId,
            <<>>,
            <<"C2C_DEL_EVERYONE">>,
            #{},
            PayloadJson,
            CurrentUid,
            To,
            CreatedAtRfc,
            CreatedAtRfc
        )
    of
        error ->
            _ = ?ERROR_LOG([s2c_stage_failed, <<"C2C_DEL_EVERYONE">>, MsgId]),
            {reply, message_ds:assemble_s2c(MsgId, <<"internal_error">>, To)};
        {ok, _} ->
            % ① 先入队（异步，非阻塞）
            msg_store_ds:enqueue(
                <<"s2c">>,
                MsgId,
                #{
                    payload => PayloadJson,
                    from_id => CurrentUid,
                    to_id => To
                }
            ),

            % ② 后投递
            message_ds:send_next(To, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
            % 给操作者回复消息
            {reply, Msg}
    end;
s2c(<<"C2G_DEL_FOR_ME">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    Gid = maps:get(<<"to">>, Data),
    ToGID = ec_cnv:to_integer(Gid),
    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    % 【权限验证】必须是群成员才能删除自己的时间线记录
    case group_ds:is_member(CurrentUid, ToGID) of
        false ->
            {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, Gid)};
        true ->
            From = CurrentUid,
            % 使用 DS 层接口删除时间线
            ok = msg_operation_ds:delete_c2g_timeline(CurrentUid, OldMsgId),
            % 给操作者回复消息
            Action = <<"C2G_DEL_FOR_ME">>,
            Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId, <<>>, Action, null),
            {reply, Msg}
    end;
s2c(<<"C2G_DEL_EVERYONE">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    Gid = maps:get(<<"to">>, Data),
    ToGID = ec_cnv:to_integer(Gid),
    OldMsgId = maps:get(<<"old_msg_id">>, Payload),

    % 【权限验证】必须是群成员，且只能删除自己发送的消息（对齐 c2g_revoke 的权限模型）
    % 注意：短路求值，非成员时不查询原消息，避免不必要的 DB 访问
    case group_ds:is_member(CurrentUid, ToGID) of
        false ->
            {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, Gid)};
        true ->
            case msg_c2g_ds:find_msg_by_id(OldMsgId) of
                {ok, #{<<"from_id">> := CurrentUid}} ->
                    MemberUids = group_ds:member_uids(ToGID),
                    NowTs = elib_dt:now(),

                    % 删除原有消息 - 使用 DS 层接口（此前误用了单聊删除函数，导致群消息实际未被删除）
                    ok = msg_operation_ds:delete_c2g_msg(
                        <<"C2G_DEL_EVERYONE">>, CurrentUid, OldMsgId
                    ),

                    From = CurrentUid,

                    % 存储s2c消息
                    [
                        s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload)
                     || Uid <- MemberUids, CurrentUid /= Uid
                    ],

                    % 给操作者回复消息
                    Action = <<"C2G_DEL_EVERYONE">>,
                    Msg = message_ds:assemble_msg(
                        <<"S2C">>, From, Gid, Payload, MsgId, <<>>, Action, null
                    ),
                    {reply, Msg};
                {ok, #{<<"from_id">> := _OtherId}} ->
                    {reply, message_ds:assemble_s2c(MsgId, <<"permission_denied">>, Gid)};
                {error, _} ->
                    {reply, message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, Gid)}
            end
    end;
%% ===================================================================
%% E2EE 密钥变更确认
%% ===================================================================

%% @doc E2EE 密钥变更确认
%% 好友确认已收到并更新密钥
s2c(<<"e2ee_key_changed_ack">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    FromUid = ec_cnv:to_integer(maps:get(<<"uid">>, Payload, <<"0">>)),
    KeyId = maps:get(<<"key_id">>, Payload, <<>>),

    % 记录确认日志
    ok = ?INFO_LOG([
        e2ee_key_changed_ack,
        #{
            from_uid => FromUid,
            acknowledged_by => CurrentUid,
            key_id => KeyId
        }
    ]),

    % 返回确认（套用统一消息信封：id/type/action/payload/server_ts 必需顶层字段）
    {reply, #{
        <<"id">> => MsgId,
        <<"type">> => <<"S2C">>,
        <<"action">> => <<"e2ee_key_changed_ack">>,
        <<"payload">> => #{
            <<"status">> => <<"acknowledged">>,
            <<"uid">> => FromUid
        },
        <<"server_ts">> => elib_dt:millisecond()
    }};
%% 兜底：未注册的 S2C action 返回 unknown_action（与 message_router_logic
%% 的 route_action 兜底语义对齐），避免 function_clause 被外层 catch
%% 误报为 invalid_json、把客户端排障引向完全错误的方向
s2c(Action, MsgId, _CurrentUid, _Data) ->
    _ = ?WARN_LOG({unknown_s2c_action, Action, MsgId}),
    {reply, message_ds:assemble_s2c(MsgId, <<"unknown_action">>, <<>>)}.

%% 1 存储s2c消息
%% 2 按策略发送消息
%% @param NowTs 时间戳
%% @param CurrentUid 当前用户ID
%% @param From 发送者（已编码）
%% @param Uid 接收者用户ID（integer）
%% @param Payload 消息内容
%% @private
-spec s2c_for_c2g(binary() | integer(), integer(), integer(), integer(), map()) -> ok.
s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload) ->
    % Uid 已经是 integer（来自 group_ds:member_uids），直接编码即可
    To = Uid,
    % s2c.5ia0V5.Kr3aUs.F
    MsgId = elib_id:gen("s2c"),
    % 按策略发送消息
    Action = <<"C2G_DEL_EVERYONE">>,
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId, <<>>, Action, null),
    MsLi = elib_retry_config:intervals(<<"s2c">>),

    % 【改进】存储消息到队列（备份表 + shq 队列）
    CreatedAtRfc2 = elib_dt:to_rfc3339(NowTs),

    % 【修复】将 Payload 转换为 JSON binary
    PayloadJson = jsone:encode(Payload, [native_utf8]),

    % 写入备份表（同步，快速；失败则跳过该成员的投递并记日志，不静默继续）
    % v2.0: S2C 消息使用 action 字段
    case
        msg_store_ds:stage(
            <<"s2c">>,
            MsgId,
            <<>>,
            <<"C2G_DEL_EVERYONE">>,
            #{},
            PayloadJson,
            CurrentUid,
            Uid,
            CreatedAtRfc2,
            CreatedAtRfc2
        )
    of
        error ->
            _ = ?ERROR_LOG([s2c_stage_failed, <<"C2G_DEL_EVERYONE">>, MsgId, Uid]),
            ok;
        {ok, _} ->
            % ① 先入队（异步，非阻塞）
            msg_store_ds:enqueue(
                <<"s2c">>,
                MsgId,
                #{
                    payload => PayloadJson,
                    from_id => CurrentUid,
                    to_id => Uid
                }
            ),

            % ② 后投递
            message_ds:send_next(Uid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
            ok
    end.

%% 客户端确认S2C投递消息
-spec s2c_client_ack(binary(), integer(), binary()) -> ok.
s2c_client_ack(MsgId, CurrentUid, DID) ->
    msg_ack_logic:client_ack(<<"s2c">>, MsgId, CurrentUid, DID).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
