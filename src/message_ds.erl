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
-export([send/5]).
-export([send_next/5]).


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
           MsgId :: binary(),
           Msg :: list(),
           Millisecond :: non_neg_integer()) -> ok.
send(ToUid, ToDtype, MsgId, Msg, Millisecond) ->
    % start_timer/3 返回的是 TimerRef erlang:start_timer(1, self(), 1234).
    % #Ref<0.717641544.2272788481.230829>
    % (imboy@127.0.0.1)2> flush().
    % Shell got {timeout,#Ref<8772.717641544.2272788481.230829>,1234}
    % ok
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg}
    % to Dest after Time milliseconds.
    TimerRefList = [{Uid, DID, erlang:start_timer(Millisecond, ToPid, {Millisecond, MsgId, ToUid, Msg})} ||
        {_, ToPid, Uid, _DType, DID} <- chat_online:lookup_by_dtype(ToUid, ToDtype),
        is_process_alive(ToPid)],
    case [Millisecond, TimerRefList] of
        [0, _] ->
            ok;
        [_, TimerRefList] ->
            % 第二次发送的时候，记录到缓存系统；
            % 再 Millisecond 时间内 ack 之后，就撤销 ref 并且清理缓存
            % timeout 的时候判断 Ref 有效才 reply
            [imboy_kv:set({Uid, DID, MsgId}, TimerRef, Millisecond + 10) || {Uid, DID, TimerRef} <- TimerRefList]
    end,
    ok.

-spec send_next(ToUid :: integer(),
           ToDType :: binary(),
           MsgId :: binary(),
           Msg :: list(),
           MillisecondList :: list()) -> ok.
send_next(_ToUid, _ToDtype, _MsgId, _Msg, []) ->
    ok;
send_next(ToUid, ToDtype, MsgId, Msg, [Millisecond | MillisecondList]) ->
    % start_timer/3 返回的是 TimerRef erlang:start_timer(1, self(), 1234).
    % #Ref<0.717641544.2272788481.230829>
    % (imboy@127.0.0.1)2> flush().
    % Shell got {timeout,#Ref<8772.717641544.2272788481.230829>,1234}
    % ok
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg}
    % to Dest after Time milliseconds.
    TimerRefList = [{Uid, DID, erlang:start_timer(
        Millisecond,
        ToPid,
        {MillisecondList, {Uid, DID, MsgId}, Msg}
    )} ||
        {_, ToPid, Uid, _DType, DID} <- chat_online:lookup_by_dtype(ToUid, ToDtype),
        is_process_alive(ToPid)],
    case [Millisecond, TimerRefList] of
        [0, _] ->
            ok;
        [_, TimerRefList] ->
            % 第二次发送的时候，记录到缓存系统；
            % 再 Millisecond 时间内 ack 之后，就撤销 ref 并且清理缓存
            % timeout 的时候判断 Ref 有效才 reply
            [imboy_kv:set({Uid, DID, MsgId}, TimerRef, Millisecond + 1) || {Uid, DID, TimerRef} <- TimerRefList]
    end,
    ok.

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
