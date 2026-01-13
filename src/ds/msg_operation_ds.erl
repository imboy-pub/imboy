-module(msg_operation_ds).
%%%
% msg_operation_ds 是消息操作数据服务层
% 统一封装消息的删除、撤回、编辑、确认等操作
%%%

-include("log.hrl").

-export([delete_c2c_msg/2]).
-export([delete_c2g_msg/3]).
-export([delete_c2c_timeline/2]).
-export([delete_c2g_timeline/2]).
-export([ack_c2c_msg/2]).
-export([ack_c2g_timeline/2]).
-export([ack_s2c_msg/2]).
-export([ack_c2s_msg/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 删除单聊消息（双方删除）
%% @param MsgId 消息ID
%% @param FromId 发送者用户ID
%% @return ok | {error, Reason}
-spec delete_c2c_msg(binary(), integer()) -> ok | {error, term()}.
delete_c2c_msg(MsgId, FromId) ->
    Where = <<"WHERE msg_id = $1 AND from_id = $2">>,
    case msg_c2c_repo:delete_msg(Where, [MsgId, FromId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除群聊消息
%% @param Action 操作类型：C2G_DEL_FOR_ME 或 C2G_DEL_EVERYONE
%% @param Uid 用户ID
%% @param MsgId 消息ID
%% @return ok | {error, Reason}
-spec delete_c2g_msg(binary(), integer(), binary()) -> ok | {error, term()}.
delete_c2g_msg(<<"C2G_DEL_FOR_ME">>, Uid, MsgId) ->
    % 只删除用户自己的时间线记录
    case msg_c2g_timeline_repo:delete_timeline(Uid, MsgId) of
        {ok, _} -> ok;
        {error, _} -> ok
    end;
delete_c2g_msg(<<"C2G_DEL_EVERYONE">>, _Uid, MsgId) ->
    % 删除群组消息（所有人）
    Where = <<"WHERE msg_id = $1">>,
    case msg_c2g_repo:delete_msg(Where, [MsgId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end;
delete_c2g_msg(_Action, _Uid, _MsgId) ->
    {error, invalid_action}.

%% @doc 删除单聊时间线记录
%% @param Uid 用户ID
%% @param MsgId 消息ID
%% @return ok
-spec delete_c2c_timeline(integer(), binary()) -> ok.
delete_c2c_timeline(_Uid, _MsgId) ->
    % C2C 消息没有单独的时间线表，消息本身已包含双方信息
    ok.

%% @doc 删除群聊时间线记录
%% @param Uid 用户ID
%% @param MsgId 消息ID
%% @return ok
-spec delete_c2g_timeline(integer(), binary()) -> ok.
delete_c2g_timeline(Uid, MsgId) ->
    case msg_c2g_timeline_repo:delete_timeline(Uid, MsgId) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc C2C 消息 ACK 处理
%% 使用原子操作删除消息，避免竞态条件
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_c2c_msg(binary(), integer()) -> ok.
ack_c2c_msg(MsgId, Uid) ->
    case msg_c2c_repo:delete_by_msg_id_and_to_id(MsgId, Uid) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc C2G 消息 ACK 处理
%% 注意：C2G 不删除离线消息，只标记 timeline
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_c2g_timeline(binary(), integer()) -> ok.
ack_c2g_timeline(MsgId, Uid) ->
    case msg_c2g_timeline_repo:client_ack(Uid, MsgId) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc S2C 消息 ACK 处理
%% 使用原子操作删除消息，避免竞态条件
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_s2c_msg(binary(), integer()) -> ok.
ack_s2c_msg(MsgId, Uid) ->
    case msg_s2c_repo:delete_by_msg_id_and_to_id(MsgId, Uid) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% @doc C2S 消息 ACK 处理
%% @param MsgId 消息ID
%% @param Uid 用户ID
%% @return ok
-spec ack_c2s_msg(binary(), integer()) -> ok.
ack_c2s_msg(MsgId, Uid) ->
    Where = <<"msg_id = $1 AND from_id = $2">>,
    case msg_c2s_repo:delete_msg(Where, [MsgId, Uid]) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
