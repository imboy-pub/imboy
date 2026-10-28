-module(conversation_pin_repo).
%%%
% conversation_pin 相关操作都放到该模块，存储库模块
% conversation pin related operations are put in this module, repository module
% 会话置顶数据仓库层，提供会话置顶信息的基础数据库操作
%%%

-export([tablename/0]).
-export([pin/3]).
-export([pin/4]).
-export([pin_tx/5]).
-export([unpin/3]).
-export([unpin_tx/4]).
-export([is_pinned/3]).
-export([is_pinned_tx/4]).
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
    %% W09.b：无世代参数的旧入口 → C2C 恒 0；C2G 建议走 pin/4。
    pin(Uid, ConversationId, Type, 0).

%% @doc W09.b（§6.7）：置顶行绑定世代（C2C=0；C2G=写入时 active 成员世代）。
%% 已有行（含旧世代行）UPSERT 刷新绑定与时间——重入群重新置顶即换绑新世代。
-spec pin(integer(), integer(), binary(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
pin(Uid, ConversationId, Type, GenerationId) ->
    Sql = pin_sql(),
    Params = pin_params(Uid, ConversationId, Type, GenerationId),
    case elib_pg:execute_count(Sql, Params) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% W09.c（J17.e/D7）：pin 的同事务变体——调用方持有业务事务 Conn 时使用；
%% 返回 {error,_} 令外层 with_tx 整体回滚。
-spec pin_tx(pid(), integer(), integer(), binary(), non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
pin_tx(Conn, Uid, ConversationId, Type, GenerationId) ->
    case
        elib_pg:execute_count(
            Conn, pin_sql(), pin_params(Uid, ConversationId, Type, GenerationId)
        )
    of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

pin_sql() ->
    Tb = tablename(),
    <<"INSERT INTO ", Tb/binary,
        " (id, user_id, conversation_id, conversation_type, "
        "generation_id, pinned_at, created_at) "
        "VALUES ($1, $2, $3, $4, $5, $6, $7) "
        "ON CONFLICT (user_id, conversation_id, conversation_type) DO UPDATE "
        "SET generation_id = EXCLUDED.generation_id, "
        "pinned_at = EXCLUDED.pinned_at">>.

pin_params(Uid, ConversationId, Type, GenerationId) ->
    Now = elib_dt:now(),
    Id = elib_tsid:generate(conversation_pin),
    [Id, Uid, ConversationId, Type, GenerationId, Now, Now].

%% @doc 取消置顶会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
-spec unpin(integer(), integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
unpin(Uid, ConversationId, Type) ->
    case elib_pg:execute_count(unpin_sql(), [Uid, ConversationId, Type]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% W09.c（J17.e/D7）：unpin 的同事务变体。
-spec unpin_tx(pid(), integer(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
unpin_tx(Conn, Uid, ConversationId, Type) ->
    case elib_pg:execute_count(Conn, unpin_sql(), [Uid, ConversationId, Type]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

unpin_sql() ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND conversation_id = $2 AND conversation_type = $3">>,
    <<"DELETE FROM ", Tb/binary, Where/binary>>.

%% @doc 检查会话是否已置顶
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已置顶 | false 未置顶
-spec is_pinned(integer(), integer(), binary()) -> boolean().
is_pinned(Uid, ConversationId, Type) ->
    case elib_pg:query(is_pinned_sql(), [Uid, ConversationId, Type]) of
        {ok, [#{<<"count">> := Count}]} when Count > 0 ->
            true;
        _ ->
            false
    end.

%% W09.c：is_pinned 的同事务变体（旧写入口在事务内做幂等判定）。
-spec is_pinned_tx(pid(), integer(), integer(), binary()) -> boolean().
is_pinned_tx(Conn, Uid, ConversationId, Type) ->
    case elib_pg:query(Conn, is_pinned_sql(), [Uid, ConversationId, Type]) of
        {ok, [#{<<"count">> := Count}]} when Count > 0 ->
            true;
        _ ->
            false
    end.

is_pinned_sql() ->
    <<"SELECT count(*) as count FROM ", (tablename())/binary,
        " p"
        " WHERE p.user_id = $1 AND p.conversation_id = $2 AND p.conversation_type = $3"
        " AND (p.conversation_type = 'c2c' OR EXISTS ("
        "  SELECT 1 FROM public.group_member_generation g"
        "  WHERE g.group_id = p.conversation_id AND g.user_id = p.user_id"
        "  AND g.end_seq IS NULL AND g.id = p.generation_id))">>.

%% @doc 获取用户的置顶会话列表（按置顶时间倒序）
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回置顶列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, term()}.
list(Uid) ->
    %% W09.b 世代过滤：C2C 行恒适用；C2G 行仅当绑定行等于该成员当前
    %% active 世代才适用——旧值不能用于新世代（§6.7/W09.d 固定退出），
    %% 由读取投影过滤承载，不靠客户端筛选。
    Sql =
        <<
            "SELECT p.conversation_id, p.conversation_type, p.pinned_at "
            "FROM ",
            (tablename())/binary,
            " p "
            "WHERE p.user_id = $1 AND ("
            "p.conversation_type = 'c2c' OR EXISTS ("
            "  SELECT 1 FROM public.group_member_generation g "
            "  WHERE g.group_id = p.conversation_id AND g.user_id = p.user_id "
            "  AND g.end_seq IS NULL AND g.id = p.generation_id)) "
            "ORDER BY p.pinned_at DESC"
        >>,
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
