-module(msg_c2c_repo).
%%%
% msg_c2c_repo 是 msg_c2c repository 缩写
% 用户到用户离线消息数据仓库层，提供C2C消息的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([read_msg/3, read_msg/4]).
-export([find_msg_by_id/1]).
-export([write_msg/8]).
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
    elib_pg_sql:public_tablename(<<"msg_c2c">>).


%% @doc 根据消息ID查找单条消息
%% @param MsgId 消息唯一ID
%% @return {ok, MsgMap} | {error, Reason}
-spec find_msg_by_id(binary()) -> {ok, map()} | {error, any()}.
find_msg_by_id(MsgId) ->
    Tb = tablename(),
    Sql = <<"SELECT from_id, created_at FROM ", Tb/binary, " WHERE msg_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [MsgId]) of
        {ok, [Msg]} -> {ok, Msg};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 读取C2C离线消息
%% @param Where SQL WHERE子句条件
%% @param Column 要查询的列名
%% @param Limit 查询结果数量限制
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @details 使用i_ToId索引，按ID升序排序
-spec read_msg(binary(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
read_msg(Where, Column, Limit) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE ", Where/binary, " ORDER BY id ASC LIMIT $1">>,
    % logger:error("msg_c2c_repo:read_msg/3 ~s~n", [Sql]),
    elib_pg:query(Sql, [Limit]).

%% @doc 读取C2C离线消息（带参数）
%% @param Where SQL WHERE子句条件
%% @param Column 要查询的列名
%% @param Limit 查询结果数量限制
%% @param Params 查询参数列表
%% @return {ok, Rows} 查询成功返回map列表 | {error, Reason} 查询失败
%% @details 使用i_ToId索引，按ID升序排序，支持参数化查询
-spec read_msg(binary(), binary(), integer(), list()) -> {ok, list(map())} | {error, any()}.
read_msg(Where, Column, Limit, Params) ->
    Tb = tablename(),
    % 计算 LIMIT 参数的编号（基于 WHERE 中的参数数量 + 1）
    LimitParamIndex = length(Params) + 1,
    LimitPlaceholder = iolist_to_binary(["$", integer_to_binary(LimitParamIndex)]),

    % 使用安全的参数化查询，避免SQL注入
    % WHERE to_id = $1 ORDER BY id ASC LIMIT $2
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " WHERE ", Where/binary, " ORDER BY id ASC LIMIT ",
            LimitPlaceholder/binary>>,
    % 将参数列表合并：先传递WHERE条件的参数，最后是LIMIT参数
    elib_pg:query(Sql, Params ++ [Limit]).


%% @doc 写入C2C离线消息
%% @param CreatedAt 消息创建时间（RFC3339 binary）
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON binary）
%% @param FromId 发送者用户ID（integer，对应 bigint 列）
%% @param ToId 接收者用户ID（integer，对应 bigint 列）
%% @param ServerTS 服务器时间戳（RFC3339 binary）
%% @param MsgType 消息类型（text, image, audio, video, file 等）
%% @param E2EE 端到端加密信息（JSON binary，可选）
%% @return {ok, Result} | {error, Reason}
%% @example msg_c2c_repo:write_msg(elib_dt:now(microsecond), <<"ciik13p2888j8hhi437g">>, <<"{\"text\":\"hello\"}">>, 1, 2, elib_dt:now(microsecond), <<"text">>, <<"{\"key\":\"...\"}">>).
-spec write_msg(binary(), binary(), binary(), integer(), integer(), binary(), binary(), binary() | null) -> {ok, any()} | {error, any()}.
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, MsgType, E2EE) ->
    %% from_id 和 to_id 是 bigint 类型，必须传入 integer
    Tb = tablename(),
    elib_pg:insert(Tb, #{
        payload => Payload,
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTS,
        msg_id => Id,
        msg_type => MsgType,
        e2ee => case E2EE of
            <<>> -> null;
            _ -> E2EE
        end
    }).


%% @doc 删除C2C离线消息（根据主键ID或消息ID）
%% @param Id 消息主键ID或消息唯一ID
%% @return {ok, Count} | {error, Reason}
-spec delete_msg(integer() | binary()) -> {ok, non_neg_integer()} | {error, any()}.
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
-spec delete_msg(binary(), list()) -> {ok, non_neg_integer()} | {error, any()}.
delete_msg(Where, Params) when is_list(Params) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    % ?DEBUG_LOG(['delete_msg', Params, Sql]),
    case elib_pg:execute(Sql, Params) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.


%% @doc 统计指定用户的C2C离线消息数量
%% @param ToUid 接收者用户ID
%% @return 返回消息数量，错误时返回0
%% @example msg_c2c_repo:count_by_to_id(1).
%% @details 使用i_c2c_ToId索引
-spec count_by_to_id(integer()) -> integer().
count_by_to_id(ToUid) ->
    % use index i_c2c_ToId
    % 使用安全的参数化查询，避免SQL注入
    elib_pg:pluck_value(tablename(), <<"count(*) as count">>, #{to_id => ToUid}, #{}, 0).

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
    case elib_pg:query(Sql, [ToUid, Limit]) of
        {ok, []} ->
            ok;
        {ok, Rows} ->
            _ = [ delete_msg(Id) || #{<<"id">> := Id} <- Rows ],
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

%% @doc 批量删除多个消息ID（使用安全的参数化查询）
%% @param MsgIds 消息ID列表
%% @param ToUid 接收者用户ID
%% @return {ok, Count} | {error, Reason}
-spec delete_by_msg_ids_and_to_id(list(binary()), integer()) -> {ok, integer()} | {error, any()}.
delete_by_msg_ids_and_to_id(MsgIds, ToUid) when is_list(MsgIds), length(MsgIds) > 0 ->
    Tb = tablename(),
    % 手动构建参数化占位符和参数
    Placeholders = iolist_to_binary(lists:join(<<",">>,
        [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(MsgIds))])),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE msg_id IN (", Placeholders/binary, ") AND to_id = $", (integer_to_binary(length(MsgIds) + 1))/binary>>,
    elib_pg:execute(Sql, MsgIds ++ [ToUid]);
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
