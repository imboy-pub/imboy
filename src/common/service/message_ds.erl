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
    % start_timer/3 返回的是{timeout, TimerRef, Msg}.
    % Starts a timer which will send the message {timeout, TimerRef, Msg} to Dest after Time milliseconds.
    [erlang:start_timer(1, ToPid, Msg2) || {_, ToPid, _Uid, _Type} <- chat_store_repo:lookup(ToUid)],
    ok.

%%% 系统消息 [500 -- 1000) 系统消息

system_msg(786, Content) -> % 在其他地方上线
    system_msg(786, Content, <<"">>, <<"">>);
system_msg(MsgType, Content) ->
    system_msg(MsgType, Content, <<"">>, <<"">>).

system_msg(1019, Content, From, To) -> % 用户在线状态变更
    system_msg(1019, Content, From, To);
system_msg(MsgType, Content, From, To) ->
    Payload = [
        {<<"msg_type">>, MsgType},
        {<<"content">>, Content}
    ],
    Ts = dt_util:milliseconds(),
    msg(<<"SYSTEM">>, From, To, Payload, Ts).
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
