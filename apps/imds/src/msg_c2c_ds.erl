-module(msg_c2c_ds).
%%%
% msg_c2c_ds 是 msg_c2c domain service 缩写
%%%

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

-export([write_msg/6]).
-export([revoke_offline_msg/5]).
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
%% @param CreatedAt 消息创建时间戳
%% @param Id 消息ID
%% @param Payload 消息内容，可以是JSON格式的列表或二进制数据
%% @param From 发送方用户ID
%% @param To 接收方用户ID
%% @param ServerTS 服务器时间戳
%% @returns any() 数据库操作结果
-spec write_msg(integer(), binary(), binary() | list(), integer(), integer(), integer()) -> any().
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) when is_list(Payload) ->
    write_msg(CreatedAt, Id, jsone:encode(Payload, [native_utf8]), From, To, ServerTS);
write_msg(CreatedAt, Id, Payload, From, To, ServerTS) ->
    % 检查消息存储数量，如果数量大于limit 删除旧数据、插入新数据
    case msg_c2c_repo:count_by_to_id(To) of
        Count when Count >= ?SAVE_MSG_LIMIT ->
            Limit = Count - ?SAVE_MSG_LIMIT + 1,
            msg_c2c_repo:delete_overflow_msg(To, Limit);
        _ ->
            ok
    end,
    msg_c2c_repo:write_msg(CreatedAt, Id, Payload, From, To, ServerTS).


%% @doc 读取点对点消息
%%
%% 从数据库中读取指定用户的消息，默认从最早的消息开始读取
%%
%% @param ToUid 接收方用户ID
%% @param Limit 读取消息数量限制
%% @returns list() 消息列表，每条消息包含完整信息
-spec read_msg(any(), integer()) -> list().
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
-spec read_msg(any(), integer(), undefined | integer() | binary()) -> list().
read_msg(ToUid, Limit, undefined) ->
    Where = <<"to_id = ", (ec_cnv:to_binary(ToUid))/binary>>,
    read_msg_filter(Where, Limit);

read_msg(ToUid, Limit, Ts) ->
    Where = <<"to_id = " , (ec_cnv:to_binary(ToUid))/binary,
        " AND created_at >= '", (ec_cnv:to_binary(Ts))/binary, "'">>,
    read_msg_filter(Where, Limit).


%% @doc 删除指定的点对点消息
%%
%% 根据消息ID从数据库中删除消息
%%
%% @param Id 消息ID
%% @returns ok 表示操作成功
-spec delete_msg(any()) -> ok.
delete_msg(Id) ->
    msg_c2c_repo:delete_msg(Id),
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
%% @returns ok 表示操作成功
-spec revoke_offline_msg(binary(), integer(), binary(), integer(), integer()) -> ok.
revoke_offline_msg(Payload, NowTs, MsgId, FromId, ToId) ->
    % 存储消息
    msg_c2c_ds:write_msg(NowTs, MsgId, Payload, FromId, ToId, NowTs),
    Tb = msg_c2c_repo:tablename(),
    Payload2 = imboy_hasher:encoded_val(Payload),
    Sql = <<"UPDATE ", Tb/binary,
        " SET payload = ", Payload2/binary,
        " WHERE msg_id = $1">>,
    imboy_db:execute(Sql, [MsgId]),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 内部函数：根据查询条件过滤和读取消息
%%
%% 执行数据库查询并处理返回的消息数据，包括解码JSON格式的payload
%%
%% @param Where SQL查询条件
%% @param Limit 查询结果数量限制
%% @returns list() 处理后的消息列表
-spec read_msg_filter(binary(), integer()) -> list().
read_msg_filter(Where, Limit) ->
    P = imboy_hasher:decoded_payload(),
    Column = <<"id, ", P/binary, ", from_id, to_id, created_at, server_ts, msg_id">>,
    Res = msg_c2c_repo:read_msg(Where, Column, Limit),
    % ?DEBUG_LOG([Res]),
    case Res of
        {ok, Column2, Rows} ->
            Rows2 = [ lists:zipwith(fun(X, Y) -> {X, Y} end, Column2, tuple_to_list(Row)) || Row <- Rows ],
            [imboy_response:json_decode_field(Row, <<"payload">>) || Row <- Rows2];
        _ ->
            []
    end.
