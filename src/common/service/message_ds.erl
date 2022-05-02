-module (message_ds).
%%%
% message_ds 是 message domain service 缩写
%%%
-export ([msg/5]).
-export ([s2c/2]).
-export ([s2c/4]).

-export ([send/3]).

-include("common.hrl").

-spec send(ToUid::integer(), Msg::list(), Millisecond::integer()) -> list().
send(ToUid, Msg, Millisecond) ->
    % start_timer/3 返回的是 TimerRef erlang:start_timer(1, self(), 1234).
    % #Ref<0.717641544.2272788481.230829>
    % (imboy@127.0.0.1)2> flush().
    % Shell got {timeout,#Ref<8772.717641544.2272788481.230829>,1234}
    % ok
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg} to Dest after Time milliseconds.
    [{DID, erlang:start_timer(Millisecond, ToPid, Msg)} || {_, ToPid, _Uid, DID} <- chat_store_repo:lookup(ToUid), is_process_alive(ToPid)].

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
