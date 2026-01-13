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
    ok = ?DEBUG_LOG({unified_ack, Type, MsgId, CurrentUid}),

    % 根据类型执行相应的 ACK 处理 - 使用 DS 层接口
    case Type of
        <<"c2c">> -> msg_operation_ds:ack_c2c_msg(MsgId, CurrentUid);
        <<"c2g">> -> msg_operation_ds:ack_c2g_timeline(MsgId, CurrentUid);
        <<"s2c">> -> msg_operation_ds:ack_s2c_msg(MsgId, CurrentUid);
        <<"c2s">> -> msg_operation_ds:ack_c2s_msg(MsgId, CurrentUid);
        _ ->
            ok = ?ERROR_LOG({unknown_msg_type_for_ack, Type})
    end,

    % 统一清理 staging 表
    msg_store_ds:unstage(MsgId),

    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

