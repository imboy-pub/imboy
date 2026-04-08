-module(conversation_pin_repo).
%%%
% conversation_pin 相关操作都放到该模块，存储库模块
% conversation pin related operations are put in this module, repository module
% 会话置顶数据仓库层，提供会话置顶信息的基础数据库操作
%%%

-export([tablename/0]).
-export([pin/3]).
-export([unpin/3]).
-export([is_pinned/3]).
-export([list/1]).
-export([delete_by_user/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取会话置顶表的表名
%% @return 返回会话置顶表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"conversation_pin">>).

%% @doc 置顶会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return {ok, Count} 插入成功返回影响行数 | {error, Reason} 插入失败
-spec pin(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
pin(Uid, ConversationId, Type) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Data = #{
        <<"user_id">> => Uid,
        <<"conversation_id">> => ConversationId,
        <<"conversation_type">> => Type,
        <<"pinned_at">> => Now,
        <<"created_at">> => Now
    },
    Id = elib_tsid:generate(conversation_pin),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Sql, Params) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取消置顶会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec unpin(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
unpin(Uid, ConversationId, Type) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case elib_pg:execute(Sql, [Uid, ConversationId, Type]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查会话是否已置顶
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已置顶 | false 未置顶
-spec is_pinned(integer(), integer(), binary()) -> boolean().
is_pinned(Uid, ConversationId, Type) ->
    Sql = <<"SELECT count(*) as count FROM ", (tablename())/binary,
            " WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3">>,
    case elib_pg:query(Sql, [Uid, ConversationId, Type]) of
        {ok, [#{<<"count">> := Count}]} when Count > 0 ->
            true;
        _ ->
            false
    end.

%% @doc 获取用户的置顶会话列表（按置顶时间倒序）
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回置顶列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, term()}.
list(Uid) ->
    Sql = <<"SELECT conversation_id, conversation_type, pinned_at ",
            "FROM ", (tablename())/binary,
            " WHERE user_id = $1 ",
            "ORDER BY pinned_at DESC">>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, Rows};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除用户的所有置顶记录（用于测试）
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
