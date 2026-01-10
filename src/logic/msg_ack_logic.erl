-module(msg_ack_logic).
%%%
% msg_ack_logic 是消息确认处理逻辑模块
% 统一处理 C2C、C2G、S2C、C2S 的客户端确认 (CLIENT_ACK)
%%%

-include("log.hrl").

-export([client_ack/4]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 统一的客户端 ACK 处理
%% @param Type 消息类型 (<<"c2c">>, <<"c2g">>, <<"s2c">>, <<"c2s">>)
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @param DID 设备ID
-spec client_ack(binary(), binary(), integer(), binary()) -> ok.
client_ack(Type, MsgId, CurrentUid, _DID) ->
    io:format("📥 [UNIFIED_ACK] Type=~p, MsgId=~s, Uid=~p~n", [Type, MsgId, CurrentUid]),

    % 根据类型执行相应的 ACK 处理
    case Type of
        <<"c2c">> -> handle_c2c_ack(MsgId, CurrentUid);
        <<"c2g">> -> handle_c2g_ack(MsgId, CurrentUid);
        <<"s2c">> -> handle_s2c_ack(MsgId, CurrentUid);
        <<"c2s">> -> handle_c2s_ack(MsgId, CurrentUid);
        _ ->
            ok = ?ERROR_LOG([unknown_msg_type_for_ack, Type])
    end,

    % 统一清理 staging 表
    msg_store_ds:unstage(MsgId),

    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @private
%% @doc C2C 消息 ACK 处理
handle_c2c_ack(MsgId, Uid) ->
    Column = <<"id">>,
    Where = <<"msg_id = $1 AND to_id = $2">>,
    {ok, Rows} = msg_c2c_repo:read_msg(Where, Column, 1, [MsgId, Uid]),
    _ = [msg_c2c_repo:delete_msg(Id) || #{<<"id">> := Id} <- Rows],
    ok.

%% @private
%% @doc C2G 消息 ACK 处理
%% 注意：C2G 不删除离线消息，只标记 timeline
handle_c2g_ack(MsgId, Uid) ->
    _ = msg_c2g_timeline_repo:client_ack(Uid, MsgId),
    ok.

%% @private
%% @doc S2C 消息 ACK 处理
handle_s2c_ack(MsgId, Uid) ->
    Column = <<"id">>,
    Where = <<"msg_id = $1 AND to_id = $2">>,
    {ok, Rows} = msg_s2c_repo:read_msg(Where, Column, 1, [MsgId, Uid]),
    _ = [msg_s2c_repo:delete_msg(Id) || #{<<"id">> := Id} <- Rows],
    ok.

%% @private
%% @doc C2S 消息 ACK 处理
%% 修复 SQL 注入：使用安全的参数化查询
handle_c2s_ack(MsgId, Uid) ->
    Where = <<"msg_id = $1 AND from_id = $2">>,
    msg_c2s_repo:delete_msg(Where, [MsgId, Uid]),
    ok.
