-module(conversation_delete_repo).
%%%
% conversation_delete 相关操作都放到该模块，存储库模块
% conversation delete related operations are put in this module, repository module
% 会话删除数据仓库层，提供会话删除信息的基础数据库操作
%%%

-export([tablename/0]).
-export([mark_deleted/3]).
-export([mark_deleted/4]).
-export([mark_deleted_tx/5]).
-export([restore_tx/4]).
-export([is_deleted/3]).
-export([is_deleted_tx/4]).
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
-spec mark_deleted(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
mark_deleted(Uid, ConversationId, Type) ->
    %% W09.b：无世代参数的旧入口 → C2C 恒 0；C2G 建议走 mark_deleted/4。
    mark_deleted(Uid, ConversationId, Type, 0).

%% @doc W09.b（§6.7）：删除（隐藏）行绑定世代（C2C=0；C2G=写入时 active 世代）。
%% 已有行（含旧世代行）UPSERT 刷新绑定与时间——重入群重新隐藏即换绑新世代。
-spec mark_deleted(integer(), integer(), binary(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
mark_deleted(Uid, ConversationId, Type, GenerationId) ->
    case
        elib_pg:execute_count(
            mark_deleted_sql(), mark_deleted_params(Uid, ConversationId, Type, GenerationId)
        )
    of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% W09.c（J17.e/D7）：mark_deleted 的同事务变体——返回 {error,_} 令
%% 外层 with_tx 整体回滚。
-spec mark_deleted_tx(pid(), integer(), integer(), binary(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
mark_deleted_tx(Conn, Uid, ConversationId, Type, GenerationId) ->
    case
        elib_pg:execute_count(
            Conn,
            mark_deleted_sql(),
            mark_deleted_params(Uid, ConversationId, Type, GenerationId)
        )
    of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

mark_deleted_sql() ->
    Tb = tablename(),
    <<"INSERT INTO ", Tb/binary,
        " (id, user_id, conversation_id, conversation_type, generation_id, deleted_at, created_at) ",
        "VALUES ($1, $2, $3, $4, $5, $6, $7) ",
        "ON CONFLICT (user_id, conversation_id, conversation_type) DO UPDATE ",
        "SET generation_id = EXCLUDED.generation_id, deleted_at = EXCLUDED.deleted_at">>.

mark_deleted_params(Uid, ConversationId, Type, GenerationId) ->
    Now = elib_dt:now(),
    RowId = elib_tsid:generate(conversation_delete),
    [RowId, Uid, ConversationId, Type, GenerationId, Now, Now].

%% @doc 检查会话是否已删除
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已删除 | false 未删除
-spec is_deleted(integer(), integer(), binary()) -> boolean().
is_deleted(Uid, ConversationId, Type) ->
    %% W09.b：世代过滤——C2G 隐藏态不跨世代延续（重入群重新校验，§6.7）。
    case elib_pg:query(is_deleted_sql(), [Uid, ConversationId, Type]) of
        {ok, [#{<<"count">> := Count}]} when Count > 0 ->
            true;
        _ ->
            false
    end.

%% W09.c：is_deleted 的同事务变体（旧写入口在事务内做幂等判定）。
-spec is_deleted_tx(pid(), integer(), integer(), binary()) -> boolean().
is_deleted_tx(Conn, Uid, ConversationId, Type) ->
    case elib_pg:query(Conn, is_deleted_sql(), [Uid, ConversationId, Type]) of
        {ok, [#{<<"count">> := Count}]} when Count > 0 ->
            true;
        _ ->
            false
    end.

is_deleted_sql() ->
    <<"SELECT count(*) as count FROM ", (tablename())/binary,
        " p"
        " WHERE p.user_id = $1 AND p.conversation_id = $2 AND p.conversation_type = $3"
        " AND (p.conversation_type = 'c2c' OR EXISTS ("
        "  SELECT 1 FROM public.group_member_generation g"
        "  WHERE g.group_id = p.conversation_id AND g.user_id = p.user_id"
        "  AND g.end_seq IS NULL AND g.id = p.generation_id))">>.

%% @doc 获取用户的已删除会话列表（按删除时间倒序）
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回已删除列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, term()}.
list(Uid) ->
    %% W09.b：世代过滤同 is_deleted/3——C2C 行恒适用；C2G 行仅绑定当前
    %% active 世代才适用。
    Sql =
        <<
            "SELECT p.conversation_id, p.conversation_type, p.deleted_at "
            "FROM ",
            (tablename())/binary,
            " p "
            "WHERE p.user_id = $1 AND ("
            "p.conversation_type = 'c2c' OR EXISTS ("
            "  SELECT 1 FROM public.group_member_generation g "
            "  WHERE g.group_id = p.conversation_id AND g.user_id = p.user_id "
            "  AND g.end_seq IS NULL AND g.id = p.generation_id)) "
            "ORDER BY p.deleted_at DESC"
        >>,
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
-spec restore(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
restore(Uid, ConversationId, Type) ->
    case elib_pg:execute_count(restore_sql(), [Uid, ConversationId, Type]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% W09.c（J17.e/D7）：restore 的同事务变体。
-spec restore_tx(pid(), integer(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
restore_tx(Conn, Uid, ConversationId, Type) ->
    case elib_pg:execute_count(Conn, restore_sql(), [Uid, ConversationId, Type]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

restore_sql() ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3">>,
    <<"DELETE FROM ", Tb/binary, Where/binary>>.

%% @doc 删除用户的所有删除记录（用于测试）
%% @param Uid 用户ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec delete_by_user(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_user(Uid) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case elib_pg:execute_count(Sql, [Uid]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
