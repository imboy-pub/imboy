-module(message_ds).
%%%
% message_ds 是 message domain service 缩写
%%%

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").

-export([assemble_s2c/3]).
-export([assemble_msg/5]).
-export([handle_info/2]).
-export([send_next/4, send_next/6, ack/3, handle_ack_cancel/3]).

%% ===================================================================
%% API
%% ===================================================================

%% send_next/4: 给指定用户所有设备发送消息并支持多次重发
send_next(ToUid, MsgId, Msg, MsLi) ->
    send_next(ToUid, MsgId, Msg, MsLi, [], false).

% 如果消息一直没有被客户端确认，
% 那么它将按照 MillisecondList 定义的频率投递 length(MillisecondList) 次，
% 除非投递期间收到客户端确认消息（ CLIENT_ACK,type,msgid,did ）才终止投递；
% 也就是说，消息会按特地平率至少投递一次，至多投递 length(MillisecondList) 次。
%% 支持按设备DID列表过滤，并指定是否为白名单（IsMember）
%% send_next/6: 支持设备过滤（DIDLi, IsMember 控制是白名单还是黑名单）
-spec send_next(integer(), binary(), any(), list(), list(), boolean()) -> ok.
send_next(_ToUid, _MsgId, _Msg, [], _, _) ->
    ok;
send_next(ToUid, MsgId, Msg, MsLi, DIDLi, IsMember) when is_list(MsLi), MsLi /= [] ->
    % 只允许整数或定时重发间隔组成的列表
    case lists:all(fun(T) -> is_integer(T) andalso T >= 0 end, MsLi) of
        false -> ok; % 非法间隔直接忽略
        true -> send_next_loop(ToUid, MsgId, Msg, MsLi, DIDLi, IsMember)
    end;
send_next(_ToUid, _MsgId, _Msg, _MsLi, _DIDLi, _IsMember) ->
    ok.

%% 实际消息分发和定时重发控制
send_next_loop(_ToUid, _MsgId, _Msg, [], _DIDLi, _IsMember) -> ok;
send_next_loop(ToUid, MsgId, Msg, [Delay|Tail], DIDLi, IsMember) ->
    Members = imboy_syn:list_by_uid(ToUid),
    Filtered = case DIDLi of
        [] -> Members;
        _ when IsMember == true ->
            [ {Pid, {_Dtype, DID}} || {Pid, {_Dtype, DID}} <- Members, lists:member(DID, DIDLi) ];
        _ -> % IsMember == false
            [ {Pid, {_Dtype, DID}} || {Pid, {_Dtype, DID}} <- Members, not lists:member(DID, DIDLi) ]
    end,
    case Filtered of
        [] -> ok;
        _ when Delay =:= 0 ->
            [ imboy_syn:publish(ToUid, Msg, 0) || _ <- [1] ],
            send_next_loop(ToUid, MsgId, Msg, Tail, DIDLi, IsMember);
        _ when is_integer(Delay), Delay > 0 ->
            [
                begin
                    TimerKey = {ToUid, DID, MsgId},
                    Ref = erlang:start_timer(Delay, Pid, {Tail, TimerKey, Msg}),
                    imboy_cache:set(TimerKey, Ref, Delay + 1000) % 超时时间略大于 timer
                end
            || {Pid, {_Dtype, DID}} <- Filtered
            ],
            ok
    end.

%% ===================================================================
%% 分布式 ACK 支持
%% ===================================================================

%% 任意节点收到 ACK 后广播所有节点撤销 timer
-spec ack(integer(), binary(), binary()) -> ok.
ack(ToUid, DID, MsgId) ->
    Nodes = [node() | nodes()],
    % 广播到所有节点（含自己），每台机器都尝试撤销本地 timer
    rpc:multicall(Nodes, ?MODULE, handle_ack_cancel, [ToUid, DID, MsgId]),
    ok.

%% 实际执行 timer 撤销，只在本节点有效
handle_ack_cancel(ToUid, DID, MsgId) ->
    TimerKey = {ToUid, DID, MsgId},
    case imboy_cache:get(TimerKey) of
        undefined -> ok;
        Ref ->
            erlang:cancel_timer(Ref),
            imboy_cache:flush(TimerKey),
            ?LOG([<<"ACK cancel_timer">>, TimerKey, Ref]),
            ok
    end.

%% ===================================================================
%% Timer 超时自动重发逻辑
%% ===================================================================
%% 需放到 websocket_handler 或 gen_server 的 handle_info/2 调用
handle_info({timeout, Ref, {Tail, {ToUid, DID, MsgId}, Msg}}, State) ->
    TimerKey = {ToUid, DID, MsgId},
    case imboy_cache:get(TimerKey) of
        Ref ->
            % 还没被 ACK，本地继续重发
            imboy_syn:publish(ToUid, Msg, 0),
            imboy_cache:flush(TimerKey),
            ?LOG(["timeout resend", TimerKey, Msg]),
            send_next(ToUid, MsgId, Msg, Tail, [DID], true),
            {noreply, State};
        _ ->
            % 已撤销，不再重发
            {noreply, State}
    end.

%%% 系统消息 [500 -- 1000) 系统消息
-spec assemble_s2c(binary(), binary(), [binary() | integer()]) -> list().
assemble_s2c(MsgId, MsgType, To) ->
    Payload = [{<<"msg_type">>, MsgType}],
    assemble_msg(<<"S2C">>, <<"">>, To, Payload, MsgId).

%%% 系统消息 end

%% 组装标准 IM 消息
assemble_msg(Type, From, To, Payload, MsgId) when is_integer(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:encode(From), To, Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_list(From), From > 0 ->
    assemble_msg(Type, imboy_hashids:encode(From), To, Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_list(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:encode(To), Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) when is_integer(To), To > 0 ->
    assemble_msg(Type, From, imboy_hashids:encode(To), Payload, MsgId);
assemble_msg(Type, From, To, Payload, MsgId) ->
    [{<<"id">>, MsgId},
     {<<"type">>, Type},
     {<<"from">>, From},
     {<<"to">>, To},
     {<<"payload">>, Payload},
     {<<"server_ts">>, imboy_dt:millisecond()}].
