-module(conversation_mute_ds).
%%%
% conversation_mute 数据服务模块
% conversation mute (DND) data service module
% 会话免打扰数据服务层，封装会话免打扰相关的数据操作
%%%

-export([mute_conversation/3]).
-export([unmute_conversation/3]).
-export([get_muted_conversations/1]).
-export([is_conversation_muted/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 会话开启免打扰
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec mute_conversation(integer(), integer(), binary()) -> ok | {error, term()}.
mute_conversation(Uid, ConversationId, Type) ->
    case conversation_mute_repo:mute(Uid, ConversationId, Type) of
        {ok, _Count} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 会话关闭免打扰
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功
-spec unmute_conversation(integer(), integer(), binary()) -> ok.
unmute_conversation(Uid, ConversationId, Type) ->
    conversation_mute_repo:unmute(Uid, ConversationId, Type),
    ok.

%% @doc 获取用户的免打扰会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功 | {error, Reason} 查询失败
-spec get_muted_conversations(integer()) -> {ok, list(map())} | {error, term()}.
get_muted_conversations(Uid) ->
    conversation_mute_repo:list(Uid).

%% @doc 检查会话是否已免打扰
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已免打扰 | false 未免打扰
-spec is_conversation_muted(integer(), integer(), binary()) -> boolean().
is_conversation_muted(Uid, ConversationId, Type) ->
    conversation_mute_repo:is_muted(Uid, ConversationId, Type).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
