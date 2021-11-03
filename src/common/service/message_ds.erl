-module (message_ds).
%%%
% message_ds 是 message domain service 缩写
%%%
-export ([msg/5]).
-export ([system_msg/2]).
-export ([system_msg/4]).

-export ([send/2]).

-include("common.hrl").

-spec send(integer(), list()) -> ok.
send(ToUid, Msg2) ->
    [erlang:start_timer(1, ToPid, Msg2) || {_, ToPid, _Uid, _Type} <- chat_store_repo:lookup(ToUid)],
    ok.

%%% 系统消息 [500 -- 1000) 系统消息

system_msg(786, Content) -> % 在其他地方上线
    system_msg(786, Content, "", "");
system_msg(MsgType, Content) ->
    system_msg(MsgType, Content, "", "").

system_msg(500, State, From, To) -> % 用户在线状态变更
    system_msg(500, State, From, To);
system_msg(MsgType, Content, From, To) ->
    Payload = [
        {<<"msg_type">>, MsgType},
        {<<"content">>, Content}
    ],
    Ts = dt_util:milliseconds(),
    msg(<<"SYSTEM">>, From, To, Payload, Ts).
%%% 系统消息 end

msg(Type, From, To, Payload, Ts) ->
    [
        {<<"type">>, Type},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"payload">>, Payload},
        {<<"server_ts">>, Ts}
    ].

%% Internal.
