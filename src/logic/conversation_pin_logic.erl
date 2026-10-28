-module(conversation_pin_logic).

-moduledoc "conversation_pin 业务逻辑模块 / conversation pin business logic module。".
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
    %% W09.c（J17.e/D7）：旧写与版本推进同一数据库事务——置顶行写入
    %% （含 C2G 写入时 active 世代绑定，事务内读取）与 user_setting 会话
    %% 版本推进原子提交；任一步失败整体回滚，不再靠事后补偿冒充原子性。
    case
        elib_pg:with_tx(fun(Conn) ->
            case
                conversation_pin_ds:is_conversation_pinned_tx(
                    Conn, Uid, ConversationId, Type
                )
            of
                true ->
                    %% 已置顶（同目标重试）：返回当前状态，不重复副作用。
                    {ok, noop};
                false ->
                    case
                        conversation_pin_ds:pin_conversation_tx(
                            Conn, Uid, ConversationId, Type
                        )
                    of
                        ok ->
                            Uk3 = user_setting_ds:conversation_uk3(
                                Type, Uid, ConversationId
                            ),
                            case
                                user_setting_ds:bump_conversation_version_tx(
                                    Conn, Uid, Uk3
                                )
                            of
                                {ok, _Entry} ->
                                    {ok, changed};
                                {error, Reason} ->
                                    %% with_tx 契约：返回 {error,_} 不回滚，
                                    %% 必须以 abort_tx 中止整体回滚（W09.c）。
                                    throw({abort_tx, {bump_conversation_version, Reason}})
                            end;
                        {error, Reason} ->
                            throw({abort_tx, {pin_conversation, Reason}})
                    end
            end
        end)
    of
        {ok, noop} ->
            ok;
        {ok, changed} ->
            notify_conversation_change(
                Uid, ConversationId, Type, <<"conversation_pinned">>
            ),
            ok;
        {error, Reason} ->
            ?LOG(error, "置顶会话失败: ~p", [Reason]),
            {error, <<"置顶会话失败"/utf8>>}
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
    %% W09.c（J17.e/D7）：unpin 与版本推进同一事务；0 行（本就未置顶）
    %% 为无变化重试——不推进版本、不推送。
    case
        elib_pg:with_tx(fun(Conn) ->
            case
                conversation_pin_ds:unpin_conversation_tx(
                    Conn, Uid, ConversationId, Type
                )
            of
                {ok, Count} when Count > 0 ->
                    Uk3 = user_setting_ds:conversation_uk3(Type, Uid, ConversationId),
                    case user_setting_ds:bump_conversation_version_tx(Conn, Uid, Uk3) of
                        {ok, _Entry} ->
                            {ok, changed};
                        {error, Reason} ->
                            throw({abort_tx, {bump_conversation_version, Reason}})
                    end;
                {ok, _Zero} ->
                    {ok, noop};
                {error, Reason} ->
                    throw({abort_tx, {unpin_conversation, Reason}})
            end
        end)
    of
        {ok, noop} ->
            ok;
        {ok, changed} ->
            notify_conversation_change(
                Uid, ConversationId, Type, <<"conversation_unpinned">>
            ),
            ok;
        {error, Reason} ->
            ?LOG(error, "取消置顶会话失败: ~p", [Reason]),
            {error, <<"取消置顶会话失败"/utf8>>}
    end.

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
    case msg_s2c_ds:send(Uid, [Uid], Action, <<>>, null, Payload, no_save) of
        ok ->
            ok;
        {error, Reason} ->
            ?WARN_LOG({conversation_pin_notification_failed, Action, Reason}),
            ok
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
