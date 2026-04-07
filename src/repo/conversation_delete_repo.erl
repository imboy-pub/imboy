-module(conversation_delete_repo).
%%%
% conversation_delete 相关操作都放到该模块，存储库模块
% conversation delete related operations are put in this module, repository module
% 会话删除数据仓库层，提供会话删除信息的基础数据库操作
%%%

-export([tablename/0]).
-export([mark_deleted/3]).
-export([is_deleted/3]).
-export([list/1]).
-export([restore/3]).
-export([delete_by_user/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取会话删除表的表名
%% @return 返回会话删除表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"conversation_delete">>).

%% @doc 标记会话为已删除
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return {ok, Count} 插入成功返回影响行数 | {error, Reason} 插入失败
-spec mark_deleted(integer(), binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
mark_deleted(Uid, ConversationId, Type) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    GenId = elib_tsid:generate(conversation_delete),
    % 使用 INSERT ... ON CONFLICT DO NOTHING 实现幂等性
    Sql = <<"INSERT INTO ", Tb/binary,
            " (id, user_id, conversation_id, conversation_type, deleted_at, created_at) ",
            "VALUES ($1, $2, $3, $4, $5, $6) ",
            "ON CONFLICT (user_id, conversation_id, conversation_type) ",
            "DO NOTHING">>,
    Params = [GenId, Uid, ConversationId, Type, Now, Now],
    case elib_pg:execute(Sql, Params) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查会话是否已删除
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已删除 | false 未删除
-spec is_deleted(integer(), binary(), binary()) -> boolean().
is_deleted(Uid, ConversationId, Type) ->
    Sql = <<"SELECT count(*) as count FROM ", (tablename())/binary,
            " WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3">>,
    case elib_pg:query(Sql, [Uid, ConversationId, Type]) of
        {ok, [#{<<"count">> := Count}]} when Count > 0 ->
            true;
        _ ->
            false
    end.

%% @doc 获取用户的已删除会话列表（按删除时间倒序）
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回已删除列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, term()}.
list(Uid) ->
    Sql = <<"SELECT conversation_id, conversation_type, deleted_at ",
            "FROM ", (tablename())/binary,
            " WHERE user_id = $1 ",
            "ORDER BY deleted_at DESC">>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 恢复已删除的会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec restore(integer(), binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
restore(Uid, ConversationId, Type) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case elib_pg:execute(Sql, [Uid, ConversationId, Type]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除用户的所有删除记录（用于测试）
%% @param Uid 用户ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec delete_by_user(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_user(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case elib_pg:execute(Sql, [Uid]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
