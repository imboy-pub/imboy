-module(message_ds).
%%%
% message_ds 是 message domain service 缩写
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").

-export([assemble_s2c/3]).
-export([assemble_msg/5]).

-export([send_next/4, send_next/6]).

%% ===================================================================
%% API
%% ===================================================================


send_next(ToUid, MsgId, Msg, MsLi) ->
    % 给指定用户所有设备发送消息
    send_next(ToUid, MsgId, Msg, MsLi, [], false).


% 如果消息一直没有被客户端确认，
% 那么它将按照 MillisecondList 定义的频率投递 length(MillisecondList) 次，
% 除非投递期间收到客户端确认消息（ CLIENT_ACK,type,msgid,did ）才终止投递；
% 也就是说，消息会按特地平率至少投递一次，至多投递 length(MillisecondList) 次。
-spec send_next(integer(), binary(), list(), list(), list(), boolean()) -> ok.
send_next(_ToUid, _MsgId, _Msg, [], _, _) ->
    ok;
send_next(ToUid, MsgId, Msg, [F | MLTail], DIDLi, IsMember) when is_function(F) ->
    apply(F, []),
    send_next(ToUid, MsgId, Msg, MLTail, DIDLi, IsMember);
send_next(ToUid, MsgId, Msg, [Millisecond | MLTail], DIDLi, IsMember) ->
    % start_timer/3 返回的是 TimerRef erlang:start_timer(1, self(), 1234).
    % #Ref<0.717641544.2272788481.230829>
    % (imboy@127.0.0.1)2> flush().
    % Shell got {timeout,#Ref<8772.717641544.2272788481.230829>,1234}
    % ok
    % 如果有多端设备在线，可以给多端推送
    % Starts a timer which will send the message {timeout, TimerRef, Msg}
    % to Dest after Time milliseconds.
    IsMember2 =
        case DIDLi of
            [] ->
                false;
            _ ->
                IsMember
        end,
    TimerRefList = [ {DID, erlang:start_timer(Millisecond, ToPid, {MLTail, {ToUid, DID, MsgId}, Msg})}
                     || {ToPid, {_Dtype, DID}} <- imboy_syn:list_by_uid(ToUid)
                        % , is_process_alive(ToPid)
                        ,
                        lists:member(DID, DIDLi) == IsMember2 ],
    case TimerRefList of
        [] ->
            ok;
        _TimerRefList ->
            % 第二次发送的时候，记录到缓存系统；
            % 再 Millisecond 时间内 ack 之后，就撤销 ref 并且清理缓存
            % timeout 的时候判断 Ref 有效才 reply
            [ imboy_cache:set({ToUid, DID, MsgId}, TimerRef, Millisecond + 1) || {DID, TimerRef} <- TimerRefList ]
    end,
    ?LOG(['Millisecond', Millisecond, TimerRefList, MsgId, Msg]),
    ok.


%%% 系统消息 [500 -- 1000) 系统消息


-spec assemble_s2c(binary(), binary(), [binary() | integer()]) -> list().
assemble_s2c(MsgId, MsgType, To) ->
    Payload = [{<<"msg_type">>, MsgType}],
    assemble_msg(<<"S2C">>, <<"">>, To, Payload, MsgId).


%%% 系统消息 end


assemble_msg(Type, From, To, Payload, MsgId) when is_integer(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:encode(From), To, Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_list(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:encode(From), To, Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_list(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:encode(To), Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_integer(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:encode(To), Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) ->
    Ts = imboy_dt:now(),
    [{<<"id">>, MsgId},
     {<<"type">>, Type},
     {<<"from">>, From},
     {<<"to">>, To},
     {<<"payload">>, Payload},
     {<<"server_ts">>, Ts}].

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
