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
    % ToId = elib_hashids:decode(To),  % 移除 Logic 层的 HashID 解码

    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    ok = ?DEBUG_LOG([CurrentUid, To, Data]),
    NowTs = elib_dt:now(),

    % 删除原有消息 - 使用 DS 层接口
    ok = msg_operation_ds:delete_c2c_msg(OldMsgId, CurrentUid),
    % 数据库会自动删除 相关 msg_c2g_timeline

    % 按策略发送消息
    From = elib_hashids:encode(CurrentUid),
    Action = <<"C2C_DEL_EVERYONE">>,
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId, <<>>, Action, null),
    % ?DEBUG_LOG(Msg),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    % 【改进】存储消息到队列（备份表 + shq 队列）
    CreatedAtRfc = elib_dt:to_rfc3339(NowTs),

    % 【修复】将 Payload 转换为 JSON binary
    PayloadJson = jsone:encode(Payload, [native_utf8]),

    % 写入备份表（同步，快速）
    % v2.0: S2C 消息使用 action 字段
    msg_store_ds:stage(<<"s2c">>, MsgId, <<>>, <<"C2C_DEL_EVERYONE">>, #{},
                          PayloadJson, CurrentUid, To, CreatedAtRfc, CreatedAtRfc),

    % ① 先入队（异步，非阻塞）
    msg_store_ds:enqueue(<<"s2c">>, MsgId,
                            #{payload => PayloadJson,
                              from_id => CurrentUid,
                              to_id => To}),

    % ② 后投递
    message_ds:send_next(To, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    % 给操作者回复消息
    {reply, Msg};
s2c(<<"C2G_DEL_FOR_ME">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    Gid = maps:get(<<"to">>, Data),
    From = elib_hashids:encode(CurrentUid),
    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    % 使用 DS 层接口删除时间线
    ok = msg_operation_ds:delete_c2g_timeline(CurrentUid, OldMsgId),
    % 给操作者回复消息
    Action = <<"C2G_DEL_FOR_ME">>,
    Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId, <<>>, Action, null),
    {reply, Msg};
s2c(<<"C2G_DEL_EVERYONE">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    Gid = maps:get(<<"to">>, Data),
    ToGID = elib_hashids:decode(Gid),
    MemberUids = group_ds:member_uids(ToGID),

    OldMsgId = maps:get(<<"old_msg_id">>, Payload),
    NowTs = elib_dt:now(),

    % 删除原有消息 - 使用 DS 层接口
    ok = msg_operation_ds:delete_c2c_msg(OldMsgId, CurrentUid),

    From = elib_hashids:encode(CurrentUid),

    % 存储s2c消息
    [s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload)
     || Uid <- MemberUids, CurrentUid /= Uid],

    % 给操作者回复消息
    Action = <<"C2G_DEL_EVERYONE">>,
    Msg = message_ds:assemble_msg(<<"S2C">>, From, Gid, Payload, MsgId, <<>>, Action, null),
    {reply, Msg};

%% ===================================================================
%% E2EE 社交恢复 - 零信任架构
%% ===================================================================

%% @doc E2EE 社交恢复 - 存储分片（代理端）
%% 零信任架构：服务端仅作为传输通道，将分片转发给代理
%% 代理将分片存储在本地安全存储中
s2c(<<"store_shard">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    To = maps:get(<<"to">>, Data),

    % 零信任架构：服务端不存储分片，直接转发给代理
    % 代理将在本地安全存储中保存分片

    From = elib_hashids:encode(CurrentUid),
    Action = <<"store_shard">>,

    % 构造转发消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId, <<>>, Action, null),

    % 获取分片信息用于日志和审计
    ShardId = maps:get(<<"shard_id">>, Payload, <<>>),
    KeyVersion = maps:get(<<"key_version">>, Payload, <<>>),
    Uid = maps:get(<<"uid">>, Payload, 0),
    ProxyUid = elib_hashids:decode(To),

    % 记录分片传输日志（持久化到数据库）
    e2ee_shard_validator:log_shard_transmission(
        shard_sent,
        ShardId,
        #{
            <<"uid">> => Uid,
            <<"proxy_uid">> => ProxyUid,
            <<"key_version">> => KeyVersion
        }
    ),

    % 转发给代理
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(ProxyUid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

    % 给发送者回复确认
    {reply, Msg};

%% @doc E2EE 社交恢复 - 分片已存储确认
%% 代理确认已存储分片
s2c(<<"shard_stored">>, MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    To = maps:get(<<"to">>, Data),

    From = elib_hashids:encode(CurrentUid),
    Action = <<"shard_stored">>,

    % 构造确认消息
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId, <<>>, Action, null),

    % 获取分片信息用于日志和审计
    ShardId = maps:get(<<"shard_id">>, Payload, <<>>),
    KeyVersion = maps:get(<<"key_version">>, Payload, <<>>),
    Uid = maps:get(<<"uid">>, Payload, 0),
    ProxyUid = CurrentUid,

    % 记录分片存储日志（持久化到数据库）
    e2ee_shard_validator:log_shard_transmission(
        shard_stored,
        ShardId,
        #{
            <<"uid">> => Uid,
            <<"proxy_uid">> => ProxyUid,
            <<"key_version">> => KeyVersion
        }
    ),

    % 转发给原始发送者
    ToUid = elib_hashids:decode(To),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(ToUid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

    {reply, Msg}.

%% 1 存储s2c消息
%% 2 按策略发送消息
%% @param NowTs 时间戳
%% @param CurrentUid 当前用户ID
%% @param From 发送者（已编码）
%% @param Uid 接收者用户ID（integer）
%% @param Payload 消息内容
%% @private
-spec s2c_for_c2g(binary() | integer(), integer(), binary(), integer(), map()) -> ok.
s2c_for_c2g(NowTs, CurrentUid, From, Uid, Payload) ->
    % Uid 已经是 integer（来自 group_ds:member_uids），直接编码即可
    To = elib_hashids:encode(Uid),
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

    % 写入备份表（同步，快速）
    % v2.0: S2C 消息使用 action 字段
    msg_store_ds:stage(<<"s2c">>, MsgId, <<>>, <<"C2G_DEL_EVERYONE">>, #{},
                          PayloadJson, CurrentUid, Uid, CreatedAtRfc2, CreatedAtRfc2),

    % ① 先入队（异步，非阻塞）
    msg_store_ds:enqueue(<<"s2c">>, MsgId,
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
