-module(conversation_logic).
%%%
% conversation 业务逻辑模块
% conversation business logic module
% 会话业务逻辑层，处理会话相关的业务逻辑，包括删除、恢复等操作
%%%

-export([delete/3]).
-export([restore/3]).
-export([is_deleted/3]).
-export([get_deleted_list/1]).
-export([filter_deleted_conversations/2]).

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
-spec delete(integer(), binary(), binary()) -> ok | {error, binary()}.
delete(_Uid, <<>>, _Type) ->
    {error, <<"会话ID不能为空"/utf8>>};
delete(_Uid, _ConversationId, <<>>) ->
    {error, <<"会话类型不能为空"/utf8>>};
delete(_Uid, _ConversationId, Type) when Type =/= <<"c2c">> andalso Type =/= <<"c2g">> ->
    {error, <<"无效的会话类型"/utf8>>};
delete(Uid, ConversationId, Type) ->
    % 检查是否已经删除
    case conversation_delete_ds:is_conversation_deleted(Uid, ConversationId, Type) of
        true ->
            % 已经删除，直接返回成功（幂等性）
            ok;
        false ->
            % 标记会话为已删除
            case conversation_delete_ds:delete_conversation(Uid, ConversationId, Type) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG(error, "删除会话失败: ~p", [Reason]),
                    {error, <<"删除会话失败"/utf8>>}
            end
    end.

%% @doc 恢复已删除的会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功
-spec restore(integer(), binary(), binary()) -> ok.
restore(Uid, ConversationId, Type) ->
    conversation_delete_ds:restore_conversation(Uid, ConversationId, Type),
    ok.

%% @doc 检查会话是否已删除
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已删除 | false 未删除
-spec is_deleted(integer(), binary(), binary()) -> boolean().
is_deleted(Uid, ConversationId, Type) ->
    conversation_delete_ds:is_conversation_deleted(Uid, ConversationId, Type).

%% @doc 获取用户的已删除会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回已删除列表 | {error, Reason} 查询失败
-spec get_deleted_list(integer()) -> {ok, list(map())} | {error, term()}.
get_deleted_list(Uid) ->
    conversation_delete_ds:get_deleted_conversations(Uid).

%% @doc 过滤已删除的会话
%% @param Uid 用户ID
%% @param MsgList 消息列表（会话列表）
%% @return 过滤后的消息列表
-spec filter_deleted_conversations(integer(), list(map())) -> list(map()).
filter_deleted_conversations(Uid, MsgList) ->
    % 获取已删除的会话ID列表
    case get_deleted_list(Uid) of
        {ok, DeletedList} ->
            % 构建已删除会话的集合
            DeletedSet = sets:from_list([
                {maps:get(<<"conversation_id">>, Item), maps:get(<<"conversation_type">>, Item)}
                || Item <- DeletedList
            ]),
            % 过滤消息列表
            lists:filter(fun(Msg) ->
                FromId = maps:get(<<"from_id">>, Msg),
                % 将 from_id 转换为 hashids 编码的 conversation_id
                ConversationId = elib_hashids:encode(FromId),
                % 检查是否在已删除列表中
                not sets:is_element({ConversationId, <<"c2c">>}, DeletedSet)
            end, MsgList);
        {error, _Reason} ->
            % 如果查询失败，返回原列表
            MsgList
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% EUnit tests.
%% ===================================================================
