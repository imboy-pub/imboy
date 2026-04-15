-module(conversation_mute_logic).

%%%
% conversation_mute 业务逻辑模块
% conversation mute (DND) business logic module
% 会话免打扰业务逻辑层，处理会话免打扰相关的业务逻辑
%
% 说明：本模块处理的是 **用户级免打扰（do-not-disturb，静音通知）**，
%      与 group_member_logic:mute/4（管理员禁言 mute-to-speak）是两个独立概念。
%%%

-export([mute/3]).
-export([unmute/3]).
-export([list/1]).
-export([is_muted/3]).

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
-spec mute(integer(), integer(), binary()) -> ok | {error, binary()}.
mute(_Uid, ConversationId, _Type) when not is_integer(ConversationId); ConversationId =< 0 ->
    {error, <<"会话ID无效"/utf8>>};
mute(_Uid, _ConversationId, <<>>) ->
    {error, <<"会话类型不能为空"/utf8>>};
mute(_Uid, _ConversationId, Type) when Type =/= <<"c2c">> andalso Type =/= <<"c2g">> ->
    {error, <<"无效的会话类型"/utf8>>};
mute(Uid, ConversationId, Type) ->
    % 检查是否已经免打扰
    case conversation_mute_ds:is_conversation_muted(Uid, ConversationId, Type) of
        true ->
            % 已经免打扰，幂等返回成功
            ok;
        false ->
            case conversation_mute_ds:mute_conversation(Uid, ConversationId, Type) of
                ok ->
                    notify_conversation_change(Uid, ConversationId, Type, <<"conversation_muted">>),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "开启会话免打扰失败: ~p", [Reason]),
                    {error, <<"开启会话免打扰失败"/utf8>>}
            end
    end.

%% @doc 会话关闭免打扰
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功
-spec unmute(integer(), integer(), binary()) -> ok.
unmute(Uid, ConversationId, Type) ->
    conversation_mute_ds:unmute_conversation(Uid, ConversationId, Type),
    notify_conversation_change(Uid, ConversationId, Type, <<"conversation_unmuted">>),
    ok.

%% @doc 获取用户的免打扰会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回免打扰列表 | {error, Reason} 查询失败
-spec list(integer()) -> {ok, list(map())} | {error, term()}.
list(Uid) ->
    conversation_mute_ds:get_muted_conversations(Uid).

%% @doc 检查会话是否已免打扰
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已免打扰 | false 未免打扰
-spec is_muted(integer(), integer(), binary()) -> boolean().
is_muted(Uid, ConversationId, Type) ->
    conversation_mute_ds:is_conversation_muted(Uid, ConversationId, Type).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 向用户的所有在线设备推送会话免打扰状态变更
%% 使用 no_save 模式：免打扰状态属于轻量级同步，
%% 客户端重连时会通过 /v1/conversation/muted 拉取最新列表作为补偿
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
