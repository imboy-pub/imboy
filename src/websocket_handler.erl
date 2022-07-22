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

    State1 = [{'dtype', DType} | State0],
    State2 = [{'did', DID} | State1],
    if
        Env == "local", HeaderAuth =/= undefined ->
            websocket_ds:auth(HeaderAuth, Req0, State2, Opt0);
        Env == "local", QsAuth =/= undefined ->
            Token = maps:get(authorization, QsAuth),
            websocket_ds:auth(Token, Req0, State2, Opt0);
        % 为了安全考虑，非 local 环境
        %   必须要 DID 和 HeaderAuth，
        %   必须 check subprotocols
        bit_size(DID) > 0, HeaderAuth =/= undefined ->
            case websocket_ds:check_subprotocols(Subprotocols, Req0) of
                {ok, Req1, State2} ->
                    {ok, Req1, State2};
                {cowboy_websocket, Req1, State2} ->
                    websocket_ds:auth(HeaderAuth, Req1, State2, Opt0)
            end;
        true ->
            ?LOG([Req0, State0]),
            % token无效 (包含缺失token情况) 或者设备ID不存在
            {cowboy_websocket, Req0, [{error, 706} | State0]}
    end.


%%连接初始 onopen
websocket_init(State) ->
    CurrentPid = self(),
    ?LOG([websocket_init, lists:keyfind(error, 1, State), State]),
    case lists:keyfind(error, 1, State) of
        {error, Code} ->
            Msg = [{<<"type">>, <<"error">>},
                   {<<"code">>, Code},
                   {<<"server_ts">>, imboy_dt:milliseconds()}],
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        false ->
            CurrentUid = proplists:get_value(current_uid, State),
            % 用户上线
            DType = proplists:get_value('dtype', State, <<"">>),
            DID = proplists:get_value('did', State, <<"">>),
            user_logic:online(CurrentUid, CurrentPid, DType, DID),
            {ok, proplists:delete('dtype', State), hibernate}
    end.


%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    ?LOG([ping, cowboy_clock:rfc1123(), State]),
    case lists:keyfind(error, 1, State) of
        {error, _Code} ->
            {stop, State};
        false ->
            {reply, pong, State, hibernate}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    ?LOG([<<"ping">>, cowboy_clock:rfc1123(), State]),
    case lists:keyfind(error, 1, State) of
        {error, _Code} ->
            {stop, State};
        false ->
            {reply, {text, <<"pong2">>}, State, hibernate}
    end;
websocket_handle({text, <<"logout">>}, State) ->
    ?LOG([<<"logout">>, cowboy_clock:rfc1123(), State]),
    {stop, State};
% 客户端确认消息
websocket_handle({text, <<"CLIENT_ACK,", Tail/binary>>}, State) ->
    ?LOG(["CLIENT_ACK", Tail, State]),
    CurrentUid = proplists:get_value(current_uid, State),
    try
         binary:split(Tail, <<",">>, [global])
    of
        [Type, MsgId, DID] ->
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
websocket_handle({text, <<"C_ACK", MsgId:20/binary, ",DID", DID/binary>>}, State) ->
    % 该方法兼容之前发布的客户端，2022-06-26 作废，3个月后可用删除之
    ?LOG(["C_ACK", MsgId, DID, State]),
    CurrentUid = proplists:get_value(current_uid, State),
    websocket_logic:c2c_client_ack(MsgId, CurrentUid, DID),
    {ok, State, hibernate};
websocket_handle({text, <<"REGISTER sip:", _Tail/binary>> = Msg}, State) ->
    ?LOG([State, Msg]),
    % L1 = binary:split(Msg, <<"\r\n">>, [global, trim]),
    % L2 = [binary:split(I, <<$:>>, [global, trim]) || I <- L1],
    {reply, "", State, hibernate};
% websocket_handle({text, <<"INVITE", TO/binary>>}, State) ->
%     % INVITE 邀请会话
%     {reply, "", State, hibernate};
websocket_handle({text, Msg}, State) ->
    ?LOG([State, Msg]),
    % ?LOG(State),
    try
        CurrentUid = proplists:get_value(current_uid, State),
        Data = jsone:decode(Msg, [{object_format, proplist}]),
        Id = proplists:get_value(<<"id">>, Data),
        Type = proplists:get_value(<<"type">>, Data),
        ?LOG([Id, Type, Data]),
        % 逻辑层负责IM系统各项功能的核心逻辑实现
        % Type 包括单聊（c2c）、推送(s2c)、群聊(c2g)
        Result0 = case cowboy_bstr:to_upper(Type) of
            <<"C2C">> ->  % 单聊消息
                websocket_logic:c2c(Id, CurrentUid, Data);
            <<"C2C_REVOKE">> ->  % 客户端撤回消息
                websocket_logic:c2c_revoke(Id, Data, Type);
            <<"C2C_REVOKE_ACK">> ->  % 客户端撤回消息ACK
                websocket_logic:c2c_revoke(Id, Data, Type);
            <<"C2G">> ->  % 群聊消息
                websocket_logic:c2g(Id, CurrentUid, Data)
        end,
        case Result0 of
            ok ->
                ok;
            {reply, Msg2} ->
                {reply, Msg2}
        end
    of
        ok ->
            {ok, State, hibernate};
        {reply, Msg4} ->
            {reply, {text, jsone:encode(Msg4, [native_utf8])},
                    State,
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

    DID = proplists:get_value('did', State, <<"">>),
    case lists:keyfind(current_uid, 1, State) of
        {current_uid, Uid} ->
            user_logic:offline(Uid, self(), DID);
        false ->
            chat_online:dirty_delete(self())
    end,
    ok.
