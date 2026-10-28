-module(conversation_delete_ds).
-moduledoc "会话删除数据服务模块 / conversation delete data service。".
%%%
% conversation_delete 数据服务模块
% conversation delete data service module
% 会话删除数据服务层，封装会话删除相关的数据操作
%%%

-export([delete_conversation/3]).
-export([mark_deleted_tx/4]).
-export([restore_conversation/3]).
-export([restore_conversation_tx/4]).
-export([get_deleted_conversations/1]).
-export([is_conversation_deleted/3]).
-export([is_conversation_deleted_tx/4]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% 缓存 TTL 常量（秒）— 会话删除状态缓存 5 分钟
-define(CACHE_TTL_CONV_DELETE, 300).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 删除会话（软删除）
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec delete_conversation(integer(), integer(), binary()) -> ok | {error, term()}.
delete_conversation(Uid, ConversationId, Type) ->
    %% W09.b（§6.7）：C2G 隐藏行绑定写入时 active 成员世代；C2C 恒 0。
    GenerationId =
        case Type of
            <<"c2g">> -> group_member_ds:active_generation_id(ConversationId, Uid);
            _ -> 0
        end,
    case conversation_delete_repo:mark_deleted(Uid, ConversationId, Type, GenerationId) of
        {ok, _Count} ->
            imboy_cache:invalidate({conv_deleted, Uid, ConversationId, Type}),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 恢复已删除的会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功
-spec restore_conversation(integer(), integer(), binary()) -> ok.
restore_conversation(Uid, ConversationId, Type) ->
    case conversation_delete_repo:restore(Uid, ConversationId, Type) of
        {ok, _} -> ok;
        {error, _} -> erlang:error(conversation_restore_failed)
    end,
    imboy_cache:invalidate({conv_deleted, Uid, ConversationId, Type}),
    ok.

%% W09.c（J17.e/D7）：mark_deleted 的同事务变体——世代读取与行写入
%% 同一 Conn 同一快照；缓存失效由调用方在事务提交后执行。
-spec mark_deleted_tx(pid(), integer(), integer(), binary()) ->
    ok | {error, term()}.
mark_deleted_tx(Conn, Uid, ConversationId, Type) ->
    GenerationId =
        case Type of
            <<"c2g">> -> group_member_ds:active_generation_id_tx(Conn, ConversationId, Uid);
            _ -> 0
        end,
    case
        conversation_delete_repo:mark_deleted_tx(
            Conn, Uid, ConversationId, Type, GenerationId
        )
    of
        {ok, _Count} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% W09.c：restore 的同事务变体（返回影响行数；0 行=无变化）。
-spec restore_conversation_tx(pid(), integer(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
restore_conversation_tx(Conn, Uid, ConversationId, Type) ->
    conversation_delete_repo:restore_tx(Conn, Uid, ConversationId, Type).

%% W09.c：is_deleted 的同事务变体（旧写入口在事务内做幂等判定）。
-spec is_conversation_deleted_tx(pid(), integer(), integer(), binary()) -> boolean().
is_conversation_deleted_tx(Conn, Uid, ConversationId, Type) ->
    conversation_delete_repo:is_deleted_tx(Conn, Uid, ConversationId, Type).

%% @doc 获取用户的已删除会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功 | {error, Reason} 查询失败
-spec get_deleted_conversations(integer()) -> {ok, list(map())} | {error, term()}.
get_deleted_conversations(Uid) ->
    conversation_delete_repo:list(Uid).

%% @doc 检查会话是否已删除
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已删除 | false 未删除
-spec is_conversation_deleted(integer(), integer(), binary()) -> boolean().
is_conversation_deleted(Uid, ConversationId, Type) ->
    Key = {conv_deleted, Uid, ConversationId, Type},
    Fun = fun() -> conversation_delete_repo:is_deleted(Uid, ConversationId, Type) end,
    imboy_cache:memo(Fun, Key, ?CACHE_TTL_CONV_DELETE).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
