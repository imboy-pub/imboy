-module(msg_c2c_ds).
%%%
% msg_c2c_ds 是 msg_c2c domain service 缩写
%%%

-include_lib("kernel/include/logger.hrl").
-include("chat.hrl").
-include("log.hrl").

-export([write_msg/6]).
-export([revoke_offline_msg/5]).
-export([edit_offline_msg/5]).
-export([read_msg/2]).
-export([read_msg/3]).
-export([delete_msg/1]).

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
%% @returns any() 数据库操作结果
-spec write_msg(binary() | integer(), binary(), binary(), integer(), integer(), binary() | integer()) -> any().
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_map(Payload); is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    % 统一转换时间戳为 RFC3339 binary 格式（timestamptz 列需要）
    CreatedAt2 = imboy_dt:to_rfc3339(CreatedAt),
    ServerTS2 = imboy_dt:to_rfc3339(ServerTS),
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    Count = msg_c2c_repo:count_by_to_id(To),
    _ = case Count >= ?SAVE_MSG_LIMIT of
        true ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            _ = imboy_async:async_retry(fun() -> msg_c2c_repo:delete_overflow_msg(To, Limit) end);
        false ->
            ok
    end,
    msg_c2c_repo:write_msg(CreatedAt2, Id, Payload, From, To, ServerTS2).


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
    % 使用 imboy_dt:to_rfc3339/1 统一转换时间戳为 RFC3339 格式
    FixedTs = imboy_dt:to_rfc3339(Ts),
    % 使用安全的参数化查询，避免SQL注入
    Where = <<"to_id = $1 AND created_at >= $2">>,
    read_msg_filter(Where, Limit, [ToUid, FixedTs]).


%% @doc 删除指定的点对点消息
%%
%% 根据消息ID从数据库中删除消息
%%
%% @param Id 消息ID
%% @returns ok 表示操作成功
-spec delete_msg(any()) -> ok.
delete_msg(Id) ->
    _ = msg_c2c_repo:delete_msg(Id),
    ok.


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
-spec revoke_offline_msg(binary(), binary() | integer(), binary(), integer(), integer()) -> ok | {error, any()}.
revoke_offline_msg(Payload, NowTs, MsgId, FromId, ToId) ->
    % 存储消息
    _ = msg_c2c_ds:write_msg(NowTs, MsgId, Payload, FromId, ToId, NowTs),
    % 使用 imboy_pg:update/4 + {raw, ...} 安全地更新 payload
    case imboy_pg:update(
        msg_c2c_repo:tablename(),
        #{payload => {raw, imboy_hasher:encoded_val(Payload)}},
        <<"msg_id = $1">>,
        [MsgId]
    ) of
        {ok, _} -> ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to update msg_c2c payload for msg_id ~p: ~p", [MsgId, Reason]),
            {error, Reason}
    end.

%% @doc 编辑离线消息
%% @returns ok | {error, Reason}
-spec edit_offline_msg(binary(), binary() | integer(), binary(), integer(), integer()) -> ok | {error, any()}.
edit_offline_msg(Payload, _NowTs, MsgId, FromId, ToId) ->
    % 使用 imboy_pg:update/4 + {raw, ...} 安全地更新 payload
    case imboy_pg:update(
        msg_c2c_repo:tablename(),
        #{payload => {raw, imboy_hasher:encoded_val(Payload)}},
        <<"msg_id = $1 AND from_id = $2 AND to_id = $3">>,
        [MsgId, FromId, ToId]
    ) of
        {ok, _} -> ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to edit msg_c2c payload for msg_id ~p: ~p", [MsgId, Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id, created_at, server_ts, msg_id">>,
    Res = msg_c2c_repo:read_msg(Where, Column, Limit, Params),
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, Rows} ->
            [imboy_response:json_decode_field(Row, <<"payload">>) || Row <- Rows];
        _ ->
            []
    end.
