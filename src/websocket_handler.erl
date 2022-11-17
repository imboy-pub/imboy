-module(websocket_handler).
-behavior(cowboy_websocket).
%%%
%% websocket API 优先获取 header里面的token
%%%
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("imboy/include/log.hrl").
-include_lib("imboy/include/chat.hrl").


%%websocket 握手
init(Req0, State0) ->
    Env = os:getenv("IMBOYENV"),
    DID = cowboy_req:header(<<"did">>, Req0, <<"">>),
    DType = cowboy_req:header(<<"cos">>, Req0, <<"">>),
    HeaderAuth = cowboy_req:header(<<"authorization">>, Req0),
    QsAuth = cowboy_req:match_qs([{'authorization', [], undefined}], Req0),
    % [<<"sip">>,<<"text">>] = Subprotocols
    Subprotocols = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0),

    ?LOG([Env, DID, DType, HeaderAuth, QsAuth, Subprotocols]),
    Opt0 = #{
        num_acceptors => infinity,
        max_connections => infinity,
        max_frame_size => 1048576,  % 1MB
        idle_timeout => 120000  %  % Cowboy关闭连接空闲120秒 默认值为 60000
    },
    State1 = State0#{
        dtype => DType,
        did => DID
    },
    if
        Env == "local", HeaderAuth =/= undefined ->
            websocket_ds:auth(HeaderAuth, Req0, State1, Opt0);
        Env == "local", QsAuth =/= undefined ->
            Token = maps:get(authorization, QsAuth),
            websocket_ds:auth(Token, Req0, State1, Opt0);
        % 为了安全考虑，非 local 环境
        %   必须要 DID 和 HeaderAuth，
        %   必须 check subprotocols
        bit_size(DID) > 0, HeaderAuth =/= undefined ->
            case websocket_ds:check_subprotocols(Subprotocols, Req0) of
                {ok, Req1, State1} ->
                    {ok, Req1, State1};
                {cowboy_websocket, Req1, State1} ->
                    websocket_ds:auth(HeaderAuth, Req1, State1, Opt0)
            end;
        true ->
            ?LOG([Req0, State0]),
            % token无效 (包含缺失token情况) 或者设备ID不存在
            {cowboy_websocket, Req0, State0#{error => 706}}
    end.


%%连接初始 onopen
websocket_init(State) ->
    CurrentPid = self(),
    case maps:find(error, State) of
        {ok, Code} ->
            Msg = [{<<"type">>, <<"error">>},
                   {<<"code">>, Code},
                   {<<"server_ts">>, imboy_dt:millisecond()}],
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        error ->
            CurrentUid = maps:get(current_uid, State),
            % 用户上线
            DID = maps:get(did, State, <<"">>),
            DType = maps:get(dtype, State, <<"">>),
            user_logic:online(CurrentUid, CurrentPid, DType, DID),
            {ok, State, hibernate}
    end.


%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    ?LOG([ping, cowboy_clock:rfc1123(), State]),
    case maps:find(error, State) of
        {ok, _Code} ->
            {stop, State};
        error ->
            {reply, pong, State, hibernate}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    % ?LOG([<<"ping">>, cowboy_clock:rfc1123(), State]),
    case maps:find(error, State) of
        {ok, _Code} ->
            {stop, State};
        error ->
            {reply, {text, <<"pong2">>}, State, hibernate}
    end;
websocket_handle({text, <<"logout">>}, State) ->
    ?LOG([<<"logout">>, cowboy_clock:rfc1123(), State]),
    {stop, State};
% 客户端确认消息
websocket_handle({text, <<"CLIENT_ACK,", Tail/binary>>}, State) ->
    ?LOG(["CLIENT_ACK", Tail, State]),
    CurrentUid = maps:get(current_uid, State),
    try
         binary:split(Tail, <<",">>, [global])
    of
        [Type, MsgId, DID] ->
            case imboy_kv:get({CurrentUid, DID, MsgId}) of
                undefined ->
                    ok;
                {ok, TimerRef} ->
                    erlang:cancel_timer(TimerRef)
            end,
            case Type of
                <<"C2C">> ->
                    websocket_logic:c2c_client_ack(MsgId, CurrentUid, DID),
                    {ok, State, hibernate};
                <<"S2C">> ->
                    websocket_logic:s2c_client_ack(MsgId, CurrentUid, DID),
                    {ok, State, hibernate}
            end
    catch
        Class:Reason:Stacktrace ->
            ?LOG(["websocket_handle try catch: Class:", Class,
                  "Reason:", Reason,
                  "Stacktrace:", Stacktrace,
                  erlang:trace(all, true, [call])]),
            {ok, State, hibernate}
    end;

websocket_handle({text, Msg}, State) ->
    % ?LOG([State, Msg]),
    % ?LOG(State),
    try
        DType = maps:get(dtype, State, <<"">>),
        CurrentUid = maps:get(current_uid, State),
        Data = jsone:decode(Msg, [{object_format, proplist}]),
        Id = proplists:get_value(<<"id">>, Data),
        Type = proplists:get_value(<<"type">>, Data),
        % ?LOG([Id, Type, Data]),
        % 逻辑层负责IM系统各项功能的核心逻辑实现
        % Type 包括单聊（c2c）、推送(s2c)、群聊(c2g)
        case cowboy_bstr:to_lower(Type) of
            <<"c2c">> ->  % 单聊消息
                websocket_logic:c2c(Id, CurrentUid, DType, Data);
            <<"c2c_revoke">> ->  % 客户端撤回消息
                websocket_logic:c2c_revoke(Id, Data, Type);
            <<"c2c_revoke_ack">> ->  % 客户端撤回消息ACK
                websocket_logic:c2c_revoke(Id, Data, Type);
            <<"c2g">> ->  % 群聊消息
                websocket_logic:c2g(Id, CurrentUid, Data);
            <<"webrtc_", _Event/binary>> ->
                % Room = webrtc_ws_logic:room_name(
                %     imboy_hashids:uid_encode(CurrentUid,
                %     To),
                To = proplists:get_value(<<"to">>, Data),
                ToUid = imboy_hashids:uid_decode(To),
                webrtc_ws_logic:event(ToUid, DType, Id, Msg);
            _ ->
                ok
        end
    of
        ok ->
            {ok, State, hibernate};
        {reply, Msg2} ->
            {reply, {text, jsone:encode(Msg2, [native_utf8])},
                    State,
                    hibernate};
        {reply, Msg2, State2} ->
            {reply, {text, jsone:encode(Msg2, [native_utf8])},
                    State2,
                    hibernate}
    catch
        Class:Reason:Stacktrace ->
            ?LOG(["websocket_handle try catch: Class:", Class,
                  "Reason:", Reason,
                  "Stacktrace:", Stacktrace,
                  erlang:trace(all, true, [call])]),
            {ok, State, hibernate}
    end;
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.


%% 处理erlang 发送的消息
% 客户端如果没有确认消息，没隔 3 5 7 秒各投递1次
% start 0 3 5 7 s
websocket_info({timeout, _Ref, {Ms=0, MsgId, ToId, Msg}}, State) ->
    ?LOG([timeout, _Ref, Ms, MsgId, ToId, Msg, State, cowboy_clock:rfc1123()]),
    DType = maps:get(dtype, State, <<"">>),
    message_ds:send(ToId, DType, MsgId, Msg, 3000),
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {Ms=3000, MsgId, ToId, Msg}}, State) ->
    ?LOG([timeout, _Ref, Ms, MsgId, ToId, Msg, State, cowboy_clock:rfc1123()]),
    DType = maps:get(dtype, State, <<"">>),
    message_ds:send(ToId, DType, MsgId, Msg, 5000),
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {Ms=5000, MsgId, ToId, Msg}}, State) ->
    ?LOG([timeout, _Ref, Ms, MsgId, ToId, Msg, State, cowboy_clock:rfc1123()]),
    DType = maps:get(dtype, State, <<"">>),
    message_ds:send(ToId, DType, MsgId, Msg, 7000),
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {Ms, MsgId, ToId, Msg}}, State) ->
    ?LOG([timeout, _Ref, Ms, MsgId, ToId, Msg, State, cowboy_clock:rfc1123()]),
    {reply, {text, Msg}, State, hibernate};
% end 0 3 5 7 s

% 客户端如果没有确认消息，每隔Ms毫秒投递1次消息，总共投递len(Tail)+2次
websocket_info({timeout, _Ref, {MsLi, {Uid, DID, MsgId}, Msg}}, State) ->
    ?LOG([timeout, _Ref, {Uid, DID, MsgId}, Msg, MsLi, State, cowboy_clock:rfc1123()]),
    DType = maps:get(dtype, State, <<"">>),
    message_ds:send_next(Uid, DType, MsgId, Msg, MsLi),
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {[], {Uid, DID, MsgId}, Msg}}, State) ->
    ?LOG([timeout, _Ref, {Uid, DID, MsgId}, Msg, State, cowboy_clock:rfc1123()]),
    {reply, {text, Msg}, State, hibernate};

websocket_info({timeout, _Ref, Msg}, State) ->
    ?LOG([timeout, cowboy_clock:rfc1123(), _Ref, Msg, State]),
    {reply, {text, Msg}, State, hibernate};
websocket_info(stop, State) ->
    ?LOG([stop, State]),
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State}.


%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(Reason, _Req, State) ->
    ?LOG([terminate, cowboy_clock:rfc1123(), State, Reason]),
    case maps:get(current_uid, State) of
        Uid when is_integer(Uid)  ->
            DID = maps:get(did, State, <<"">>),
            user_logic:offline(Uid, self(), DID);
        false ->
            chat_online:dirty_delete(self())
    end,
    ok.
