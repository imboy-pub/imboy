-module(msg_c2c_ds).
%%%
% msg_c2c_ds 是 msg_c2c domain service 缩写
%%%

-include_lib("kernel/include/logger.hrl").
-include("chat.hrl").
-include("log.hrl").

-export([write_msg/6]).
-export([write_msg/8]).
-export([write_msg_with_reply/11]).
-export([revoke_offline_msg/5]).
-export([revoke_offline_msg/8]).
-export([edit_offline_msg/5]).
-export([read_offline_msg/5]).
-export([read_msg/2]).
-export([read_msg/3]).
-export([read_msg_for_conversation/3]).
-export([find_msg_by_id/1]).
-export([delete_msg/1]).
-export([update_pinned/3]).
-export([update_payload_by_msg_id/2]).
-export([delete_by_msg_ids_and_to_id/2]).
-export([count_unread_since/2]).
-export([set_expire_at/2]).
-export([delete_expired/2]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 存储点对点消息
%%
%% 将消息存储到数据库中，如果存储的消息数量超过限制，会自动删除旧消息
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（JSON binary）
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳（integer 毫秒或 binary RFC3339）
%% @returns {ok, Count} | {error, Reason} 数据库操作结果
-spec write_msg(
    binary() | integer(), binary(), binary(), integer(), integer(), binary() | integer()
) -> {ok, non_neg_integer()} | {error, term()}.
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_map(Payload); is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    % 统一转换时间戳为 RFC3339 binary 格式（timestamptz 列需要）
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    ServerTS2 = elib_dt:to_rfc3339(ServerTS),

    %% 从 Payload 中提取 msg_type 和 e2ee 字段
    PayloadMap =
        try jsone:decode(Payload) of
            Map when is_map(Map) -> Map;
            _ -> #{}
        catch
            _:_ -> #{}
        end,

    MsgType = maps:get(<<"msg_type">>, PayloadMap, <<>>),
    % map() | null
    E2EE = maps:get(<<"e2ee">>, PayloadMap, null),

    % 检查并清理溢出消息
    ok = check_and_delete_overflow(To),
    msg_c2c_repo:write_msg(CreatedAt2, Id, Payload, From, To, ServerTS2, MsgType, E2EE).

%% @doc 存储点对点消息（v2.0 格式，支持 msg_type 和 e2ee）
%%
%% 将消息存储到数据库中，包含消息类型和端到端加密信息
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（JSON binary）
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳（integer 毫秒或 binary RFC3339）
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @returns {ok, Count} | {error, Reason} 数据库操作结果
-spec write_msg(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    integer(),
    binary() | integer(),
    binary(),
    map() | null
) -> ok | {error, term()}.
write_msg(CreatedAt, Id, Payload, From, To, ServerTS, MsgType, E2EE) when
    is_map(Payload); is_list(Payload)
->
    write_msg(
        CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS, MsgType, E2EE
    );
write_msg(CreatedAt, Id, Payload, From, To, ServerTS, MsgType, E2EE) ->
    % 统一转换时间戳为 RFC3339 binary 格式（timestamptz 列需要）
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    ServerTS2 = elib_dt:to_rfc3339(ServerTS),
    % 检查并清理溢出消息
    ok = check_and_delete_overflow(To),
    msg_c2c_repo:write_msg(CreatedAt2, Id, Payload, From, To, ServerTS2, MsgType, E2EE).

%% @doc 读取点对点消息
%%
%% 从数据库中读取指定用户的消息，默认从最早的消息开始读取
%%
%% @param ToUid 接收方用户ID
%% @param Limit 读取消息数量限制
%% @returns list() 消息列表，每条消息包含完整信息
-spec read_msg(any(), integer()) -> [map()].
read_msg(ToUid, Limit) ->
    read_msg(ToUid, Limit, undefined).

%% @doc 读取点对点消息（带时间戳参数）
%%
%% 从数据库中读取指定用户的未读消息，支持按时间戳过滤
%%
%% @param ToUid 接收方用户ID
%% @param Limit 读取消息数量限制
%% @param Ts 时间戳参数，undefined表示读取所有消息，整数或二进制表示指定时间之后的消息
%% @returns list() 消息列表，每条消息包含完整信息
% msg_c2c_ds:read_msg(1, 10).
-spec read_msg(any(), integer(), undefined | integer() | binary()) -> [map()].
read_msg(ToUid, Limit, undefined) ->
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"to_id = $1">>,
    read_msg_filter(Where, Limit, [ToUid]);
%
read_msg(ToUid, Limit, Ts) ->
    % 使用 elib_dt:to_rfc3339/1 统一转换时间戳为 RFC3339 格式
    FixedTs = elib_dt:to_rfc3339(Ts),
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"to_id = $1 AND created_at >= $2">>,
    read_msg_filter(Where, Limit, [ToUid, FixedTs]).

%% @doc 为会话列表读取消息：同时返回「自己收到」与「自己发出」的 c2c 消息。
%%
%% 与 read_msg/3 的区别：read_msg 只取 to_id = Uid（用于收件箱/离线暂存语义），
%% 而会话列表需要双向聚合，否则一个仅发出过消息、未收到回复的用户的会话不会显示。
%%
%% @param Uid 当前用户 ID
%% @param Limit 数量限制
%% @param Ts undefined 或时间戳（毫秒 / RFC3339 binary）
%% @returns list(map())
-spec read_msg_for_conversation(integer(), integer(), undefined | integer() | binary()) -> [map()].
read_msg_for_conversation(Uid, Limit, undefined) ->
    Where = <<"(to_id = $1 OR from_id = $1)">>,
    read_msg_filter(Where, Limit, [Uid]);
read_msg_for_conversation(Uid, Limit, Ts) ->
    FixedTs = elib_dt:to_rfc3339(Ts),
    Where = <<"(to_id = $1 OR from_id = $1) AND created_at >= $2">>,
    read_msg_filter(Where, Limit, [Uid, FixedTs]).

%% @doc 删除指定的点对点消息
%%
%% 根据消息ID从数据库中删除消息
%%
%% @param Id 消息ID
%% @returns ok 表示操作成功
-spec delete_msg(any()) -> ok.
delete_msg(Id) ->
    case msg_c2c_repo:delete_msg(Id) of
        {ok, _} -> ok;
        {error, Reason} -> ?ERROR_LOG([msg_c2c_delete_failed, Id, Reason])
    end,
    ok.

%% @doc 根据消息ID查找单条消息（用于撤回权限验证）
%% @param MsgId 消息唯一ID
%% @return {ok, MsgMap} | {error, Reason}
-spec find_msg_by_id(binary()) -> {ok, map()} | {error, any()}.
find_msg_by_id(MsgId) ->
    msg_c2c_repo:find_msg_by_id(MsgId).

%% @doc 撤回离线消息
%%
%% 处理消息撤回操作，更新已存储的离线消息内容，并重新发送撤回通知
%%
%% @param Payload 撤回消息的新内容
%% @param NowTs 当前时间戳
%% @param MsgId 原消息ID
%% @param FromId 发送方用户ID
%% @param ToId 接收方用户ID
%% @returns ok | {error, Reason}
-spec revoke_offline_msg(binary(), binary() | integer(), binary(), integer(), integer()) ->
    ok | {error, any()}.
revoke_offline_msg(Payload, NowTs, MsgId, FromId, ToId) ->
    % 存储消息
    _ = msg_c2c_ds:write_msg(NowTs, MsgId, Payload, FromId, ToId, NowTs),
    msg_c2c_repo:update_payload_by_msg_id(MsgId, Payload).

%% @doc 撤回离线消息（v2.0 格式，支持 msg_type 和 action）
%%
%% 处理消息撤回操作，支持显式传递 msg_type 和 action 参数
%%
%% @param Payload 撤回消息的新内容（不包含 msg_type/action）
%% @param NowTs 当前时间戳
%% @param MsgId 原消息ID
%% @param FromId 发送方用户ID
%% @param ToId 接收方用户ID
%% @param MsgType 消息类型（custom, text 等）
%% @param Action 操作类型（message_revoke_ack 等）
%% @param E2EE 端到端加密信息
%% @returns ok | {error, Reason}
-spec revoke_offline_msg(
    map(), binary() | integer(), binary(), integer(), integer(), binary(), binary(), map() | null
) -> ok | {error, any()}.
revoke_offline_msg(Payload, NowTs, MsgId, FromId, ToId, MsgType, Action, E2EE) ->
    % 将 Action 包含在 Payload 中（因为 write_msg/8 不支持单独的 Action 参数）
    PayloadWithAction = Payload#{<<"action">> => Action},
    PayloadBin = imboy_message_helper:encode_json(PayloadWithAction),
    % 存储消息（v2.0: 使用 write_msg/8 显式传递参数）
    _ = msg_c2c_ds:write_msg(NowTs, MsgId, PayloadBin, FromId, ToId, NowTs, MsgType, E2EE),
    msg_c2c_repo:update_payload_by_msg_id(MsgId, PayloadBin).

%% @doc 编辑离线消息
%% @returns ok | {error, Reason}
-spec edit_offline_msg(binary(), binary() | integer(), binary(), integer(), integer()) ->
    ok | {error, any()}.
edit_offline_msg(Payload, _NowTs, MsgId, FromId, ToId) ->
    % 使用 elib_pg:update/4 + {raw, ...} 安全地更新 payload
    case
        elib_pg:update(
            msg_c2c_repo:tablename(),
            #{payload => Payload},
            <<"msg_id = $1 AND from_id = $2 AND to_id = $3">>,
            [MsgId, FromId, ToId]
        )
    of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to edit msg_c2c payload for msg_id ~p: ~p", [MsgId, Reason]),
            {error, Reason}
    end.

%% @doc 存储离线已读回执消息
%%
%% 当发送者离线时，将已读回执存储为离线消息
%%
%% @param MsgId 消息ID
%% @param FromId 发送者用户ID（接收已读通知）
%% @param ToId 接收者用户ID（发送已读回执）
%% @param ReadAt 已读时间（RFC3339 binary）
%% @param Action 操作类型（message_read）
%% @returns ok | {error, Reason}
-spec read_offline_msg(binary(), integer(), integer(), binary(), binary()) -> ok | {error, any()}.
read_offline_msg(MsgId, FromId, ToId, ReadAt, Action) ->
    % 构建已读通知 payload
    % 将 Action 包含在 Payload 中
    Payload = #{
        <<"action">> => Action,
        <<"read_at">> => elib_dt:rfc3339_to(ReadAt, millisecond)
    },
    PayloadBin = jsone:encode(Payload, [native_utf8]),

    % 存储到 msg_c2c 表
    % 使用 write_msg/8 显式传递参数
    NowTs = elib_dt:now(),
    msg_c2c_ds:write_msg(NowTs, MsgId, PayloadBin, FromId, ToId, NowTs, <<"custom">>, null).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 检查并清理溢出消息（公共逻辑，DRY）
%%
%% 检查指定用户的离线消息数量，如果超过限制则异步删除最旧的消息
%%
%% @param ToUid 接收方用户ID
%% @returns ok
-spec check_and_delete_overflow(integer()) -> ok.
check_and_delete_overflow(ToUid) ->
    % ponytail: 1/20 采样，避免每次写消息都 COUNT；溢出最多延迟 ~20 条被清理
    _ =
        case rand:uniform(20) =:= 1 of
            true ->
                Count = msg_c2c_repo:count_by_to_id(ToUid),
                case Count >= ?SAVE_MSG_LIMIT of
                    true ->
                        Limit = Count - ?SAVE_MSG_LIMIT + 1,
                        _ = elib_async:async_retry(
                            fun() -> msg_c2c_repo:delete_overflow_msg(ToUid, Limit) end
                        );
                    false ->
                        ok
                end;
            false ->
                ok
        end,
    ok.

%% @doc 内部函数：根据查询条件过滤和读取消息
%%
%% 执行数据库查询并处理返回的消息数据，包括解码JSON格式的payload
%%
%% @param Where SQL查询条件
%% @param Limit 查询结果数量限制
%% @param Params 查询参数列表
%% @returns list() 处理后的消息列表
-spec read_msg_filter(binary(), integer(), list()) -> [map()].
read_msg_filter(Where, Limit, Params) ->
    Column = <<"id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee">>,
    Res = msg_c2c_repo:read_msg(Where, Column, Limit, Params),
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, Rows} ->
            %% 同时反序列化 payload 与 e2ee 列：二者写入时均为 JSON 字符串，
            %% 离线/历史消息读取时若不 decode，e2ee 元数据会以转义字符串下发，
            %% 导致客户端无法解析密钥信封。json_decode_field 对 null/非 binary 安全。
            [
                elib_response:json_decode_field(
                    elib_response:json_decode_field(Row, <<"payload">>), <<"e2ee">>
                )
             || Row <- Rows
            ];
        _ ->
            []
    end.

%% ===================================================================
%% 引用回复功能
%% ===================================================================

%% @doc 存储带引用回复信息的点对点消息
%%
%% 将消息及其引用信息存储到数据库中
%%
%% @param CreatedAt 消息创建时间戳（integer 毫秒或 binary RFC3339）
%% @param Id 消息ID
%% @param Payload 消息内容（JSON binary）
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳（integer 毫秒或 binary RFC3339）
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON map，可选）
%% @param ReplyToMsgId 被引用回复的消息ID
%% @param ReplyToFromId 被引用消息的发送者ID
%% @param ReplySnippet 被引用消息的摘要
%% @returns ok | {error, Reason} 数据库操作结果
-spec write_msg_with_reply(
    binary() | integer(),
    binary(),
    binary(),
    integer(),
    integer(),
    binary() | integer(),
    binary(),
    map() | null,
    binary(),
    integer(),
    binary()
) ->
    ok | {error, term()}.
write_msg_with_reply(
    CreatedAt,
    Id,
    Payload,
    From,
    To,
    ServerTS,
    MsgType,
    E2EE,
    ReplyToMsgId,
    ReplyToFromId,
    ReplySnippet
) ->
    % 统一转换时间戳为 RFC3339 binary 格式（timestamptz 列需要）
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    ServerTS2 = elib_dt:to_rfc3339(ServerTS),
    % 检查并清理溢出消息
    ok = check_and_delete_overflow(To),
    msg_c2c_repo:write_msg_with_reply(
        CreatedAt2,
        Id,
        Payload,
        From,
        To,
        ServerTS2,
        MsgType,
        E2EE,
        ReplyToMsgId,
        ReplyToFromId,
        ReplySnippet
    ).

%% G3: msg_reaction_logic / msg_pinned_logic 不应直调 msg_c2c_repo
-spec update_pinned(binary(), integer(), boolean()) -> {ok, non_neg_integer()} | {error, term()}.
update_pinned(MsgId, ToUid, Pinned) ->
    msg_c2c_repo:update_pinned(MsgId, ToUid, Pinned).

%% G3: msg_c2c_logic 不应直调 msg_c2c_repo / thin DS wrapper
-spec update_payload_by_msg_id(binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
update_payload_by_msg_id(MsgId, PayloadJson) ->
    msg_c2c_repo:update_payload_by_msg_id(MsgId, PayloadJson).

%% G3: messaging_logic wrapper
-spec delete_by_msg_ids_and_to_id(list(binary()), integer()) -> {ok, integer()} | {error, any()}.
delete_by_msg_ids_and_to_id(MsgIds, Uid) -> msg_c2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid).

-spec count_unread_since(integer(), binary() | undefined) -> non_neg_integer().
count_unread_since(ToId, undefined) ->
    msg_c2c_repo:count_unread_since(ToId);
count_unread_since(ToId, Since) ->
    msg_c2c_repo:count_unread_since(ToId, Since).

-spec set_expire_at(binary(), binary()) -> ok.
set_expire_at(MsgId, ExpireAt) ->
    msg_c2c_repo:set_expire_at(MsgId, ExpireAt).

-spec delete_expired(binary(), pos_integer()) -> non_neg_integer().
delete_expired(_Now, BatchSize) ->
    msg_c2c_repo:delete_expired(BatchSize).
