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
    ToId = imboy_hashids:decode(To),

    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    % CurrentUid = imboy_hashids:decode(From),
    ok = ?DEBUG_LOG([CurrentUid, ToId, Data]),
    NowTs = imboy_dt:now(),

    % 删除原有消息
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1 AND from_id = $2">>,
    _ = msg_c2g_repo:delete_msg(Where, [OldMsgId, CurrentUid]),
    % 数据库会自动删除 相关 msg_c2g_timeline
    % 按策略发送消息
    From = imboy_hashids:encode(CurrentUid),
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId),
    % ?DEBUG_LOG(Msg),
    MsLi = [0, 1500, 1500, 3000, 5000, 7000],

    % 【改进】存储消息到队列（备份表 + shq 队列）
    CreatedAtRfc = imboy_dt:to_rfc3339(NowTs),

    % 【修复】将 Payload 转换为 JSON binary
    PayloadJson = jsone:encode(Payload, [native_utf8]),

    % 写入备份表（同步，快速）
    msg_store_ds:stage(<<"s2c">>,
                          MsgId,
                          PayloadJson,
                          CurrentUid,
                          ToId,
                          CreatedAtRfc,
                          CreatedAtRfc),

    % ① 先入队（异步，非阻塞）
    msg_store_ds:enqueue(s2c,
                            MsgId,
                            #{payload => PayloadJson,
                              from_id => CurrentUid,
                              to_id => ToId}),

    % ② 后投递
    message_ds:send_next(ToId, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    % 给操作者回复消息
    {reply, Msg};
s2c(<<"C2G_DEL_FOR_ME">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    Gid = maps:get(<<"to">>, Data),
    % Note: Gid (encoded) is passed directly to assemble_msg, which handles encoding internally
    From = imboy_hashids:encode(CurrentUid),
    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    _ = msg_c2g_timeline_repo:delete_timeline(CurrentUid, OldMsgId),
    % 给操作者回复消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId),
    {reply, Msg};
s2c(<<"C2G_DEL_EVERYONE">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    Gid = maps:get(<<"to">>, Data),
    ToGID = imboy_hashids:decode(Gid),
    MemberUids = group_ds:member_uids(ToGID),

    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    % CurrentUid = imboy_hashids:decode(From),
    NowTs = imboy_dt:now(),

    % 删除原有消息
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1 AND from_id = $2">>,
    _ = msg_c2c_repo:delete_msg(Where, [OldMsgId, CurrentUid]),

    From = imboy_hashids:encode(CurrentUid),

    % 存储s2c消息
    [s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload)
     || Uid <- MemberUids, CurrentUid /= Uid],

    % 给操作者回复消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId),
    {reply, Msg}.

%% 1 存储s2c消息
%% 2 按策略发送消息
s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload) ->
    To = imboy_hashids:encode(Uid),
    % s2c.5ia0V5.Kr3aUs.F
    MsgId = imboy_func:uid("s2c"),
    % 按策略发送消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId),
    % ?DEBUG_LOG(Msg),
    MsLi = [0, 1500, 1500, 3000, 5000, 7000],

    % 【改进】存储消息到队列（备份表 + shq 队列）
    CreatedAtRfc2 = imboy_dt:to_rfc3339(NowTs),

    % 【修复】将 Payload 转换为 JSON binary
    PayloadJson = jsone:encode(Payload, [native_utf8]),

    % 写入备份表（同步，快速）
    msg_store_ds:stage(<<"s2c">>,
                          MsgId,
                          PayloadJson,
                          CurrentUid,
                          Uid,
                          CreatedAtRfc2,
                          CreatedAtRfc2),

    % ① 先入队（异步，非阻塞）
    msg_store_ds:enqueue(s2c,
                            MsgId,
                            #{payload => PayloadJson,
                              from_id => CurrentUid,
                              to_id => Uid}),

    % ② 后投递
    message_ds:send_next(Uid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    ok.

%% 客户端确认S2C投递消息
-spec s2c_client_ack(binary(), integer(), binary()) -> ok.
s2c_client_ack(MsgId, CurrentUid, DID) ->
    msg_ack_logic:client_ack(<<"s2c">>, MsgId, CurrentUid, DID).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
