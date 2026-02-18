-module(conversation_delete_ds).
%%%
% conversation_delete 数据服务模块
% conversation delete data service module
% 会话删除数据服务层，封装会话删除相关的数据操作
%%%

-export([delete_conversation/3]).
-export([restore_conversation/3]).
-export([get_deleted_conversations/1]).
-export([is_conversation_deleted/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 删除会话（软删除）
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec delete_conversation(integer(), binary(), binary()) -> ok | {error, term()}.
delete_conversation(Uid, ConversationId, Type) ->
    case conversation_delete_repo:mark_deleted(Uid, ConversationId, Type) of
        {ok, _Count} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 恢复已删除的会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功
-spec restore_conversation(integer(), binary(), binary()) -> ok.
restore_conversation(Uid, ConversationId, Type) ->
    conversation_delete_repo:restore(Uid, ConversationId, Type),
    ok.

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
-spec is_conversation_deleted(integer(), binary(), binary()) -> boolean().
is_conversation_deleted(Uid, ConversationId, Type) ->
    conversation_delete_repo:is_deleted(Uid, ConversationId, Type).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
