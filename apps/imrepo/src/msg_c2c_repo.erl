-module(msg_c2c_repo).
%%%
% msg_c2c_repo 是 msg_c2c repository 缩写
% 用户到用户离线消息数据仓库层，提供C2C消息的基础数据库操作
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([read_msg/3]).
-export([write_msg/6]).
-export([delete_msg/1]).
-export([delete_msg/2]).
-export([count_by_to_id/1]).
-export([delete_by_to_id/1]).
-export([delete_by_msg_id_and_to_id/2]).
-export([delete_by_msg_ids_and_to_id/2]).
-export([delete_overflow_msg/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取C2C消息表的表名
%% @return 返回C2C消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    imboy_db:public_tablename(<<"msg_c2c">>).


%% @doc 读取C2C离线消息
%% @param Where SQL WHERE子句条件
%% @param Column 要查询的列名
%% @param Limit 查询结果数量限制
%% @return {ok, Columns, Rows} 查询成功返回列和行数据 | {error, Reason} 查询失败
%% @details 使用i_ToId索引，按ID升序排序
-spec read_msg(binary(), binary(), integer()) -> {ok, list(), list()} | {error, any()}.
read_msg(Where, Column, Limit) ->
    Tb = tablename(),
    % use index i_ToId
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE "
        , Where/binary
        , " ORDER BY id ASC LIMIT "
        , (ec_cnv:to_binary(Limit))/binary>>,
    % logger:error("msg_c2c_repo:read_msg/3 ~s~n", [Sql]),
    imboy_db:query(Sql).


%% @doc 写入C2C离线消息
%% @param CreatedAt 消息创建时间
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON格式）
%% @param FromId 发送者用户ID（支持integer或binary）
%% @param ToId 接收者用户ID（支持integer或binary）
%% @param ServerTS 服务器时间戳
%% @return {ok, Result} | {error, Reason}
%% @example msg_c2c_repo:write_msg(imboy_dt:now(), <<"ciik13p2888j8hhi437g">>, <<"{\"msg_type\":\"text\",\"text\":\"ddd的点点滴滴\"},\"created_at\":1688551567306}">>, 1, 2, imboy_dt:now()).
-spec write_msg(binary(), binary(), binary(), integer() | binary(), integer() | binary(), binary()) -> {ok, any()} | {error, any()}.
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, Id, Payload, FromId2, ToId, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(CreatedAt, Id, Payload, FromId, ToId2, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) ->
    % ?DEBUG_LOG([CreatedAt, Id, Payload, FromId, ToId, ServerTS]),
    Tb = tablename(),
    imboy_db:insert_into(Tb, #{
        payload => {raw, imboy_hasher:encoded_val(Payload)},
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTS,
        msg_id => Id
        }).


%% @doc 删除C2C离线消息（根据主键ID或消息ID）
%% @param Id 消息主键ID或消息唯一ID
%% @return {ok, Count} | {error, Reason}
-spec delete_msg(integer() | binary()) -> {ok, any()} | {error, any()}.
delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE id = $1">>,
    delete_msg(Where, [Id]);
delete_msg(Id) ->
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, [Id]).

%% @doc 根据WHERE条件删除C2C离线消息
%% @param Where SQL WHERE子句
%% @param Params 参数列表
%% @return {ok, Count} | {error, Reason}
-spec delete_msg(binary(), list()) -> {ok, any()} | {error, any()}.
delete_msg(Where, Params) when is_list(Params) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG(['delete_msg', Params, Sql]),
    imboy_db:execute(Sql, Params).


%% @doc 统计指定用户的C2C离线消息数量
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
%% @example msg_c2c_repo:count_by_to_id(1).
%% @details 使用i_c2c_ToId索引
-spec count_by_to_id(integer()) -> {ok, integer()} | {error, any()}.
count_by_to_id(ToUid) ->
    ToUid2 = integer_to_binary(ToUid),
    % use index i_c2c_ToId
    imboy_db:pluck(tablename(), <<"to_id = ", ToUid2/binary>>, <<"count(*) as count">>, 0).

%% @doc 删除超出限制数量的C2C离线消息
%% @param ToUid 接收者用户ID
%% @param Limit 保留的消息数量限制
%% @return ok | {error, Reason}
%% @details 删除最旧的消息，保留最新的指定数量消息
-spec delete_overflow_msg(integer(), integer()) -> ok | {error, any()}.
delete_overflow_msg(ToUid, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE to_id = $1 ORDER BY id ASC LIMIT $2">>,
    Sql = <<"SELECT id FROM ", Tb/binary, Where/binary>>,
    case imboy_db:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [ delete_msg(Id) || {Id} <- Rows ],
            ok
    end.

%% @doc 删除用户的所有C2C离线消息
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_to_id(integer()) -> {ok, any()} | {error, any()}.
delete_by_to_id(ToUid) ->
    Where = <<"WHERE to_id = $1">>,
    delete_msg(Where, [ToUid]).

%% @doc 根据消息ID和接收者ID删除特定消息
%% @param MsgId 消息唯一ID
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_id_and_to_id(binary(), integer()) -> {ok, any()} | {error, any()}.
delete_by_msg_id_and_to_id(MsgId, ToUid) ->
    Where = <<"WHERE msg_id = $1 AND to_id = $2">>,
    delete_msg(Where, [MsgId, ToUid]).

%% @doc 批量删除多个消息ID（使用 IN 语句的单个 SQL）
%% @param MsgIds 消息ID列表
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_ids_and_to_id(list(binary()), integer()) -> {ok, integer()} | {error, any()}.
delete_by_msg_ids_and_to_id(MsgIds, ToUid) when is_list(MsgIds), length(MsgIds) > 0 ->
    Tb = tablename(),
    Placeholders = build_placeholders(length(MsgIds)),
    Where = <<"WHERE msg_id IN (", Placeholders/binary, ") AND to_id = $", (integer_to_binary(length(MsgIds) + 1))/binary, " RETURNING id">>,
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    case imboy_db:execute(Sql, MsgIds ++ [ToUid]) of
        {ok, _, Rows} ->
            Count = length(Rows),
            {ok, Count};
        {error, Reason} ->
            {error, Reason}
    end;
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 构建SQL占位符字符串
%% @param Count 占位符数量
%% @return 占位符字符串，如 <<"$1,$2,$3">>
-spec build_placeholders(pos_integer()) -> binary().
build_placeholders(Count) ->
    lists:join(<<",">>, [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, Count)]).
