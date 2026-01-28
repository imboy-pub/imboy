-module(msg_s2c_repo).
%%%
% msg_s2c_repo 是 msg_s2c repository 缩写
% 系统到用户消息数据仓库层，提供S2C消息的基础数据库操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([read_msg/4]).
-export([write_msg/9]).
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

%% @doc 获取S2C消息表的表名
%% @return 返回S2C消息表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"msg_s2c">>).


-spec read_msg(binary(), list(), binary(), integer()) -> {ok, list(map())} | {error, any()}.
read_msg(Where, Vals, Column, Limit) ->
    Tb = tablename(),
    % use index i_ToId
    % 动态计算 LIMIT 参数索引，并构建完整 SQL
    LimitIndex = length(Vals) + 1,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " ", Where/binary, " ORDER BY id ASC LIMIT $",
            (integer_to_binary(LimitIndex))/binary>>,
    elib_pg:query(Sql, Vals ++ [Limit]).


%% @doc 写入S2C离线消息
%% @param CreatedAt 消息创建时间（RFC3339 binary）
%% @param Id 消息唯一ID
%% @param Payload 消息载荷（JSON binary）
%% @param FromId 发送者用户ID（integer，对应 bigint 列）
%% @param ToId 接收者用户ID（integer，对应 bigint 列）
%% @param ServerTS 服务器时间戳（RFC3339 binary）
%% @param Action S2C消息指令（如 notify, revoke, edit 等）
%% @param MsgType 消息类型（可选，默认为空）
%% @param E2EE 端到端加密信息（JSON map 或 null，S2C 消息通常为 null）
%% @return {ok, Result} | {error, Reason}
-spec write_msg(binary(), binary(), binary(), integer(), integer(), binary(), binary(), binary(), map() | null) -> {ok, any()} | {error, any()}.
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS, Action, MsgType, E2EE) ->
    %% from_id 和 to_id 是 bigint 类型，需要传入 integer
    elib_pg:insert(tablename(), #{
        payload => Payload,
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTS,
        msg_id => Id,
        action => Action,
        msg_type => case MsgType of
            <<>> -> <<>>;
            _ -> MsgType
        end,
        e2ee => case E2EE of
            <<>> -> null;
            null -> null;
            Map when is_map(Map) -> jsone:encode(Map, [native_utf8]);  % map 需要 encode
            Bin when is_binary(Bin) -> Bin;  % 已经是 JSON binary（避免双重编码）
            _ -> null
        end
    }).

-spec delete_msg(integer() | binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE id = $1">>,
    delete_msg(Where, [Id]);
delete_msg(Id) ->
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, [Id]).


-spec delete_msg(binary(), list()) -> {ok, non_neg_integer()} | {error, term()}.
delete_msg(Where, Val) when is_list(Val) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    case elib_pg:execute(Sql, Val) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end;
delete_msg(Where, Val) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    case elib_pg:execute(Sql, [Val]) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.


% msg_s2c_repo:count_by_to_id(1).
-spec count_by_to_id(integer()) -> non_neg_integer().
count_by_to_id(ToUid) ->
    % use index i_ToId
    % to_id 是 bigint 类型，需要传入 integer，不能转换成 binary
    elib_pg:pluck_value(tablename(), <<"count(*)">>, #{to_id => ToUid}, #{}, 0).


-spec delete_overflow_msg(integer(), integer()) -> ok.
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

% 删除用户的所有系统消息
-spec delete_by_to_id(integer() | binary()) -> ok | {ok, non_neg_integer()} | {error, term()}.
delete_by_to_id(ToUid) ->
    Where = <<"WHERE to_id = $1">>,
    delete_msg(Where, [ToUid]).

% 根据消息ID和接收者ID删除特定系统消息
-spec delete_by_msg_id_and_to_id(binary(), integer()) -> ok | {ok, non_neg_integer()} | {error, term()}.
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
    {InClause, InParams} = elib_pg_sql:in(<<"msg_id">>, MsgIds, 2),
    WhereClause = <<"to_id = $1 AND ", InClause/binary>>,
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE ", WhereClause/binary>>,
    elib_pg:execute(Sql, [ToUid | InParams]);
delete_by_msg_ids_and_to_id([], _ToUid) ->
    {ok, 0}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
