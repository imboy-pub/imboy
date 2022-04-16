-module (message_ds).
%%%
% message_ds 是 message domain service 缩写
%%%
-export ([msg/5]).
-export ([s2c/2]).
-export ([s2c/4]).

-export ([send/2]).

-include("common.hrl").

-spec send(integer(), list()) -> ok.
send(ToUid, Msg2) ->
    % start_timer/3 返回的是{timeout, TimerRef, Msg}.
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg} to Dest after Time milliseconds.
    [erlang:start_timer(1, ToPid, Msg2) || {_, ToPid, _Uid, _Type} <- chat_store_repo:lookup(ToUid), is_process_alive(ToPid)],
    ok.

%%% 系统消息 [500 -- 1000) 系统消息

s2c(786, Content) -> % 在其他地方上线
    s2c(786, Content, <<"">>, <<"">>);
s2c(MsgType, Content) ->
    s2c(MsgType, Content, <<"">>, <<"">>).

s2c(1019, Content, From, To) -> % 用户在线状态变更
    s2c(1019, Content, From, To);
s2c(MsgType, Content, From, To) ->
    Payload = [
        {<<"msg_type">>, MsgType},
        {<<"content">>, Content}
    ],
    Ts = dt_util:milliseconds(),
    msg(<<"S2C">>, From, To, Payload, Ts).
%%% 系统消息 end

msg(Type, From, To, Payload, Ts) when is_integer(From) ->
    msg(Type, hashids_translator:uid_encode(From), To, Payload, Ts);
msg(Type, From, To, Payload, Ts) when is_integer(To) ->
    msg(Type, From, hashids_translator:uid_encode(To), Payload, Ts);
msg(Type, From, To, Payload, Ts) ->
    [
        {<<"type">>, Type},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"payload">>, Payload},
        {<<"server_ts">>, Ts}
    ].

%% Internal.
