-module(websocket_handler).
-behavior(cowboy_websocket).
%%%
%% websocket API 获取 header里面的token
%%%
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/chat.hrl").


%%websocket 握手
init(Req0, State0) ->
    % Env = os:getenv("IMBOYENV"),
    % DID device id
    DID = cowboy_req:header(<<"did">>, Req0, undefined),
    DType = cowboy_req:header(<<"cos">>, Req0, undefined),
    Auth = cowboy_req:header(<<"authorization">>, Req0, undefined),
    % [<<"sip">>,<<"text">>] = Subprotocols
    SubPt = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0),

    % ?LOG([Env, DID, DType, Auth, SubPt]),
    Opt0 = #{num_acceptors => infinity,
             max_connections => infinity,
             max_frame_size => 1048576,  % 1MB
             % Cowboy关闭连接空闲120秒 默认值为 60000
             idle_timeout => 120000},
    State1 = State0#{dtype => DType, did => DID},
    case throttle:check(throttle_ws, DID) of
        {limit_exceeded, _, _} ->
            imboy_log:warning("DeviceID ~p exceeded api limit", [DID]),
            % 429 Too Many Requests
            Req = cowboy_req:reply(429, Req0),
            {ok, Req, State0};
        _ ->
            % ?LOG([SubPt]),
            case websocket_ds:check_subprotocols(SubPt, Req0) of
                {ok, Req1} ->
                    {ok, Req1, State1};
                {cowboy_websocket, Req1} ->
                    % ?LOG([State1]),
                    websocket_ds:auth(Auth, Req1, State1, Opt0)
            end
    end.


%%连接初始 onopen
websocket_init(State) ->
    case maps:find(error, State) of
        {ok, Code} ->
            Msg = [{<<"type">>, <<"error">>}, {<<"code">>, Code}, {<<"server_ts">>, imboy_dt:millisecond()}],
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        error ->
            CurrentUid = maps:get(current_uid, State),
            % 用户上线
            DID = maps:get(did, State, <<"">>),
            DType = maps:get(dtype, State, <<"">>),
            user_logic:online(CurrentUid, DType, self(), DID),
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
websocket_handle({text, <<"check_offline_msg">>}, State) ->
    CurrentUid = maps:get(current_uid, State),
    Pid = self(),
    DID = maps:get(did, State, <<"">>),
    % 检查离线消息
    msg_c2c_logic:check_msg(CurrentUid, Pid, DID),
    % 检查群聊离线消息
    msg_c2g_logic:check_msg(CurrentUid, Pid, DID),
    {ok, State, hibernate};
websocket_handle({text, <<"logout">>}, State) ->
    ?LOG([<<"logout">>, cowboy_clock:rfc1123(), State]),
    {stop, State};

% 客户端确认消息
% CLIENT_ACK,type,msgid,did
websocket_handle({text, <<"CLIENT_ACK,", Tail/binary>>}, State) ->
    ?LOG(["CLIENT_ACK", Tail, State]),
    CurrentUid = maps:get(current_uid, State),
    try
        binary:split(Tail, <<",">>, [global])
    of
        [Type, MsgId, DID] ->
            ?LOG(["CLIENT_ACK", {CurrentUid, DID, MsgId}]),
            % 缓存在 message_ds:send_next/5 中设置
            case imboy_cache:get({CurrentUid, DID, MsgId}) of
                undefined ->
                    ok;
                {ok, TimerRef} ->
                    ?LOG(["CLIENT_ACK", {CurrentUid, DID, MsgId}, TimerRef]),
                    erlang:cancel_timer(TimerRef)
            end,
            case Type of
                <<"C2C">> ->
                    websocket_logic:c2c_client_ack(MsgId, CurrentUid, DID),
                    {ok, State, hibernate};
                <<"S2C">> ->
                    websocket_logic:s2c_client_ack(MsgId, CurrentUid, DID),
                    {ok, State, hibernate};
                <<"C2G">> ->
                    websocket_logic:c2g_client_ack(MsgId, CurrentUid, DID),
                    {ok, State, hibernate};
                _ ->
                    {ok, State, hibernate}
            end
    catch
        Class:Reason:Stacktrace ->
            ?LOG(["websocket_handle try catch: Class:",
                  Class,
                  "Reason:",
                  Reason,
                  "Stacktrace:",
                  Stacktrace,
                  erlang:trace(all, true, [call])]),
            {ok, State, hibernate}
    end;

websocket_handle({text, Msg}, State) ->
    % ?LOG([State, Msg]),
    try
        CurrentUid = maps:get(current_uid, State),
        Data = jsone:decode(Msg, [{object_format, proplist}]),
        MsgId = proplists:get_value(<<"id">>, Data),
        Type = proplists:get_value(<<"type">>, Data),
        % ?LOG([MsgId, Type, Data]),
        % 逻辑层负责IM系统各项功能的核心逻辑实现
        % Type 包括单聊（c2c）、推送(s2c)、群聊(c2g)
        case cowboy_bstr:to_lower(Type) of
            <<"c2c">> ->  % 单聊消息
                websocket_logic:c2c(MsgId, CurrentUid, Data);
            <<"c2c_revoke">> ->  % 客户端撤回消息
                websocket_logic:c2c_revoke(MsgId, Data, Type);
            <<"c2c_revoke_ack">> ->  % 客户端撤回消息ACK
                websocket_logic:c2c_revoke(MsgId, Data, Type);
            <<"c2g">> ->  % 群聊消息
                websocket_logic:c2g(MsgId, CurrentUid, Data);
            <<"webrtc_", _Event/binary>> ->
                % Room = webrtc_ws_logic:room_name(
                %     imboy_hashids:uid_encode(CurrentUid,
                %     To),
                To = proplists:get_value(<<"to">>, Data),
                ToUid = imboy_hashids:uid_decode(To),
                webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg);
            _ ->
                ok
        end
    of
        ok ->
            {ok, State, hibernate};
        {reply, Msg2} ->
            ?LOG([reply, 2, Msg2, State]),
            {reply, {text, jsone:encode(Msg2, [native_utf8])}, State, hibernate};
        {reply, Msg3, State3} ->
            {reply, {text, jsone:encode(Msg3, [native_utf8])}, State3, hibernate}
    catch
        Class:Reason:Stacktrace ->
            ?LOG(["websocket_handle try catch: Class:",
                  Class,
                  "Reason:",
                  Reason,
                  "Stacktrace:",
                  Stacktrace,
                  erlang:trace(all, true, [call])]),
            {ok, State, hibernate}
    end;

websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.


%% 处理erlang 发送的消息
websocket_info({reply, Msg}, State) ->
    ?LOG([reply, State]),
    {reply, {text, jsone:encode(Msg, [native_utf8])}, State, hibernate};

websocket_info({timeout, _Ref, {[], _, Msg}}, State) ->
    {reply, {text, Msg}, State, hibernate};
websocket_info({timeout, _Ref, {MsLi, {Uid, DID, MsgId}, Msg}}, State) ->
    ?LOG([timeout, _Ref, {Uid, DID, MsgId}, MsLi, State, Msg, cowboy_clock:rfc1123()]),
    message_ds:send_next(Uid, MsgId, Msg, MsLi, [DID], true),
    {reply, {text, Msg}, State, hibernate};

websocket_info({timeout, _Ref, Msg}, State) ->
    ?LOG([timeout, cowboy_clock:rfc1123(), _Ref, Msg, State]),
    {reply, {text, Msg}, State, hibernate};
websocket_info({close, CloseCode, Reason}, State) ->
    ?LOG([close, CloseCode, Reason, State]),
    {reply, {close, CloseCode, Reason}, State};
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
    case maps:find(current_uid, State) of
        {ok, Uid} when is_integer(Uid) ->
            DID = maps:get(did, State, <<>>),
            user_logic:offline(Uid, self(), DID),
            ok;
        error ->
            ok
    end.
