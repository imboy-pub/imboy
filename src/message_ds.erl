-module(message_ds).
%%%
% message_ds 是 message domain service 缩写
%%%

-include_lib("imboy/include/log.hrl").

-export([assemble_msg/5]).
-export([assemble_s2c/2]).
-export([assemble_s2c/3]).
-export([assemble_s2c/4]).

-export([send/3]).
-export([send/4]).


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

-spec send(ToUid :: integer(),
           Msg :: list(),
           Millisecond :: integer()) -> list().
send(ToUid, Msg, Millisecond) ->
    % start_timer/3 返回的是 TimerRef erlang:start_timer(1, self(), 1234).
    % #Ref<0.717641544.2272788481.230829>
    % (imboy@127.0.0.1)2> flush().
    % Shell got {timeout,#Ref<8772.717641544.2272788481.230829>,1234}
    % ok
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg}
    % to Dest after Time milliseconds.
    [{DID, erlang:start_timer(Millisecond, ToPid, Msg)} ||
        {_, ToPid, _Uid, _DType, DID} <- chat_online:lookup(ToUid),
        is_process_alive(ToPid)].

-spec send(ToUid :: integer(),
           ToDType :: binary(),
           Msg :: list(),
           Millisecond :: integer()) -> list().
send(ToUid, ToDtype, Msg, Millisecond) ->
    % start_timer/3 返回的是 TimerRef erlang:start_timer(1, self(), 1234).
    % #Ref<0.717641544.2272788481.230829>
    % (imboy@127.0.0.1)2> flush().
    % Shell got {timeout,#Ref<8772.717641544.2272788481.230829>,1234}
    % ok
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg}
    % to Dest after Time milliseconds.
    [{DID, erlang:start_timer(Millisecond, ToPid, Msg)} ||
        {_, ToPid, _Uid, _DType, DID} <- chat_online:lookup_by_dtype(ToUid, ToDtype),
        is_process_alive(ToPid)].


%%% 系统消息 [500 -- 1000) 系统消息

assemble_s2c(<<"logged_another_device">>, UID, DID) ->  % 在其他设备登录了
    Ts = imboy_dt:milliseconds(),
    DName = user_device_repo:device_name(UID, DID),
    Payload = [
        {<<"msg_type">>, <<"logged_another_device">>},
        {<<"did">>, DID},
        {<<"dname">>, DName}
    ],
    assemble_msg(<<"S2C">>, <<"">>, <<"">>, Payload, Ts).

assemble_s2c(MsgType, Content) ->
    assemble_s2c(MsgType, Content, <<"">>, <<"">>).


assemble_s2c(MsgType, Content, From, To) when is_integer(MsgType) ->
    assemble_s2c(integer_to_binary(MsgType), Content, From, To);
assemble_s2c(MsgType, Content, From, To) when is_list(MsgType) ->
    assemble_s2c(list_to_binary(MsgType), Content, From, To);
assemble_s2c(MsgType, Content, From, To) ->
    Payload = [{<<"msg_type">>, MsgType}, {<<"content">>, Content}],
    Ts = imboy_dt:milliseconds(),
    assemble_msg(<<"S2C">>, From, To, Payload, Ts).

%%% 系统消息 end

assemble_msg(Type, From, To, Payload, Ts) when is_integer(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:uid_encode(From), To, Payload, Ts);
assemble_msg(Type, From, To, Payload, Ts) when is_list(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:uid_encode(From), To, Payload, Ts);
assemble_msg(Type, From, To, Payload, Ts) when is_list(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:uid_encode(To), Payload, Ts);
assemble_msg(Type, From, To, Payload, Ts) when is_integer(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:uid_encode(To), Payload, Ts);
assemble_msg(Type, From, To, Payload, Ts) ->
    [{<<"type">>, Type},
     {<<"from">>, From},
     {<<"to">>, To},
     {<<"payload">>, Payload},
     {<<"server_ts">>, Ts}].

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
