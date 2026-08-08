-module(conversation_pin_logic).

%%%
% conversation_pin 业务逻辑模块
% conversation pin business logic module
% 会话置顶业务逻辑层，处理会话置顶相关的业务逻辑
%%%

-export([pin/3]).
-export([unpin/3]).
-export([list/1]).
-export([is_pinned/3]).

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
-spec pin(integer(), integer(), binary()) -> ok | {error, binary()}.
pin(_Uid, ConversationId, _Type) when not is_integer(ConversationId); ConversationId =< 0 ->
    {error, <<"会话ID无效"/utf8>>};
pin(_Uid, _ConversationId, <<>>) ->
    {error, <<"会话类型不能为空"/utf8>>};
pin(_Uid, _ConversationId, Type) when Type =/= <<"c2c">> andalso Type =/= <<"c2g">> ->
    {error, <<"无效的会话类型"/utf8>>};
pin(Uid, ConversationId, Type) ->
    % 检查是否已经置顶
    case conversation_pin_ds:is_conversation_pinned(Uid, ConversationId, Type) of
        true ->
            % 已经置顶，直接返回成功
            ok;
        false ->
            % 置顶会话
            case conversation_pin_ds:pin_conversation(Uid, ConversationId, Type) of
                ok ->
                    notify_conversation_change(
                        Uid, ConversationId, Type, <<"conversation_pinned">>
                    ),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "置顶会话失败: ~p", [Reason]),
                    {error, <<"置顶会话失败"/utf8>>}
            end
    end.

%% @doc 取消置顶会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec unpin(integer(), integer(), binary()) -> ok | {error, binary()}.
unpin(_Uid, ConversationId, _Type) when not is_integer(ConversationId); ConversationId =< 0 ->
    {error, <<"会话ID无效"/utf8>>};
unpin(Uid, ConversationId, Type) ->
    conversation_pin_ds:unpin_conversation(Uid, ConversationId, Type),
    notify_conversation_change(Uid, ConversationId, Type, <<"conversation_unpinned">>),
    ok.

%% @doc 获取用户的置顶会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回置顶列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, term()}.
list(Uid) ->
    conversation_pin_ds:get_pinned_conversations(Uid).

%% @doc 检查会话是否已置顶
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已置顶 | false 未置顶
-spec is_pinned(integer(), integer(), binary()) -> boolean().
is_pinned(Uid, ConversationId, Type) ->
    conversation_pin_ds:is_conversation_pinned(Uid, ConversationId, Type).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 向用户的所有在线设备推送会话变更通知
%% 使用 no_save 模式：会话状态变更属于轻量级同步，
%% 客户端重连时会拉取最新会话列表作为补偿
%% @private
-spec notify_conversation_change(integer(), integer(), binary(), binary()) -> ok.
notify_conversation_change(Uid, ConversationId, Type, Action) ->
    Payload = #{
        <<"conversation_id">> => ConversationId,
        <<"conversation_type">> => Type
    },
    msg_s2c_ds:send(Uid, [Uid], Action, <<>>, null, Payload, no_save),
    ok.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
