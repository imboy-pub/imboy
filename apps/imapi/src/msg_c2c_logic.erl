-module(msg_c2c_logic).
%%%
%  msg_c2c 业务逻辑模块
%%%
-export([check_msg/3]).

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% 检查离线消息
% 单聊离线消息，每个离线用户的消息获取10条（差不多一屏幕多），如果多于10条，再返回消除总数量
%%
check_msg(Uid, Pid, _DID) ->
    % ?LOG(["msg_c2c_logic/check_msg/2", Uid, Pid]),
    case msg_s2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT) of
        [] ->
            ok;
        MsgsS2C ->
            % 发送S2c离线消息
            sent_offline_msg(Pid, <<"S2C">>, MsgsS2C, 0)
    end,
    case msg_c2c_ds:read_msg(Uid, ?SAVE_MSG_LIMIT) of
        [] ->
            ok;
        MsgsC2C ->
            % 发送C2C离线消息
            sent_offline_msg(Pid, <<"C2C">>, MsgsC2C, 0)
    end,
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


sent_offline_msg(_Pid, _Type, [], _Index) ->
    ok;
sent_offline_msg(Pid, Type, [Row | Tail], Index) ->
    {<<"msg_id">>, MsgId} = lists:keyfind(<<"msg_id">>, 1, Row),
    {<<"from_id">>, FromId} = lists:keyfind(<<"from_id">>, 1, Row),
    {<<"to_id">>, ToId} = lists:keyfind(<<"to_id">>, 1, Row),
    {<<"payload">>, Payload} = lists:keyfind(<<"payload">>, 1, Row),
    % ?LOG(["Row", Row, "; Payload: ", Payload]),
    Delay = 100 + Index * 100,
    Msg = [{<<"id">>, MsgId},
           {<<"type">>, Type},
           {<<"from">>, imboy_hashids:uid_encode(FromId)},
           {<<"to">>, imboy_hashids:uid_encode(ToId)},
           {<<"payload">>, jsone:decode(Payload, [{object_format, proplist}])},
           lists:keyfind(<<"created_at">>, 1, Row),
           lists:keyfind(<<"server_ts">>, 1, Row)],
    % ?LOG([Delay, "Msg: ", Msg]),
    erlang:start_timer(Delay, Pid, jsone:encode(Msg, [native_utf8])),
    sent_offline_msg(Pid, Type, Tail, Index + 1).
