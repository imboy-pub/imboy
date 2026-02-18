-module(conversation_pin_ds).
%%%
% conversation_pin 数据服务模块
% conversation pin data service module
% 会话置顶数据服务层，封装会话置顶相关的数据操作
%%%

-export([pin_conversation/3]).
-export([unpin_conversation/3]).
-export([get_pinned_conversations/1]).
-export([is_conversation_pinned/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 置顶会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec pin_conversation(integer(), binary(), binary()) -> ok | {error, term()}.
pin_conversation(Uid, ConversationId, Type) ->
    case conversation_pin_repo:pin(Uid, ConversationId, Type) of
        {ok, _Count} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 取消置顶会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功
-spec unpin_conversation(integer(), binary(), binary()) -> ok.
unpin_conversation(Uid, ConversationId, Type) ->
    conversation_pin_repo:unpin(Uid, ConversationId, Type),
    ok.

%% @doc 获取用户的置顶会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功 | {error, Reason} 查询失败
-spec get_pinned_conversations(integer()) -> {ok, list(map())} | {error, term()}.
get_pinned_conversations(Uid) ->
    conversation_pin_repo:list(Uid).

%% @doc 检查会话是否已置顶
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已置顶 | false 未置顶
-spec is_conversation_pinned(integer(), binary(), binary()) -> boolean().
is_conversation_pinned(Uid, ConversationId, Type) ->
    conversation_pin_repo:is_pinned(Uid, ConversationId, Type).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
