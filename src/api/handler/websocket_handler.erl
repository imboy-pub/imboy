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

-include("common.hrl").

%%websocket 握手
init(Req0, State0) ->
    Type = cowboy_req:header(<<"device-type">>, Req0, <<"web">>),
    SystemState = [{'device-type', Type}|State0],
    case websocket_ds:check_subprotocols(Req0, State0) of
        {ok, Req1, State1} ->
            ?LOG(['check_subprotocols']),
            {ok, Req1, State1};
        {cowboy_websocket, Req1, State1, Opt} ->
            case cowboy_req:header(<<"authorization">>, Req0, undefined) of
                undefined ->
                    % HTTP 412 - 先决条件失败
                    % Req2 = cowboy_req:reply(412, Req1),
                    % {ok, Req2, State1};
                    case cowboy_req:match_qs([{'authorization', [], undefined}], Req1) of
                        #{'authorization' := undefined} ->
                            % HTTP 412 - 先决条件失败
                            Req2 = cowboy_req:reply(412, Req1),
                            {ok, Req2, State1};
                        #{'authorization' := Token} ->
                            ?LOG(Token),
                            case catch token_ds:decrypt_token(Token) of
                                {ok, Uid, _ExpireAt, _Type} ->
                                    Timeout = user_logic:idle_timeout(Uid),
                                    {cowboy_websocket, Req1, [{current_uid, Uid}|SystemState], Opt#{idle_timeout := Timeout}};
                                {error, 705, Msg, _Li} ->
                                    Req3 = resp_json_dto:error(Req1, Msg),
                                    {ok, Req3, State0};
                                {error, Code, _Msg, _Li} ->
                                    {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
                            end
                    end;
                Authorization ->
                    case catch token_ds:decrypt_token(Authorization) of
                        {ok, Uid, _ExpireAt, _Type} ->
                            Timeout = user_logic:idle_timeout(Uid),
                            {cowboy_websocket, Req1, [{current_uid, Uid}|SystemState], Opt#{idle_timeout := Timeout}};
                        {error, 705, Msg, _Li} ->
                            Req3 = resp_json_dto:error(Req1, Msg),
                            {ok, Req3, State0};
                        {error, Code, _Msg, _Li} ->
                            {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
                    end
            end
    end.

%%连接初始 onopen
websocket_init(State) ->
    CurrentPid = self(),
    ?LOG([websocket_init, lists:keyfind(error, 1, State), State]),
    case lists:keyfind(error, 1, State) of
        {error, Code} ->
            Msg = [
                {<<"type">>, <<"error">>},
                {<<"code">>, Code},
                {<<"timestamp">>, dt_util:milliseconds()}
            ],
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        false ->
            CurrentUid = proplists:get_value(current_uid, State),
            % 用户上线
            DeviceType = proplists:get_value('cos', State, <<"web">>),
            user_logic:online(CurrentUid, CurrentPid, DeviceType),
            {ok, State, hibernate}
    end.

%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    % ?LOG([ping, cowboy_clock:rfc1123(), State]),
    case lists:keyfind(error, 1, State) of
        {error, _Code} ->
            {stop, State};
        false ->
            {reply, pong, State, hibernate}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    % ?LOG([<<"ping">>, cowboy_clock:rfc1123(), State]),
    case lists:keyfind(error, 1, State) of
        {error, _Code} ->
            {stop, State};
        false ->
            {reply, {text, <<"pong2">>}, State, hibernate}
    end;
websocket_handle({text, <<"logout">>}, State) ->
    ?LOG([<<"logout">>, cowboy_clock:rfc1123(), State]),
    {stop, State};
websocket_handle({text, Msg}, State) ->
    % ?LOG([State, Msg]),
    % ?LOG(State),
    try
        case lists:keyfind(error, 1, State) of
            {error, Code} ->
                ErrMsg = [
                    {<<"type">>, <<"error">>},
                    {<<"code">>, Code},
                    {<<"timestamp">>, dt_util:milliseconds()}
                ],
                {reply, ErrMsg};
            false ->
                CurrentUid = proplists:get_value(current_uid, State),
                Data = jsone:decode(Msg, [{object_format, proplist}]),
                Id = proplists:get_value(<<"id">>, Data),
                Type = proplists:get_value(<<"type">>, Data),
                ?LOG([Id, Type, Data]),
                % 逻辑层负责IM系统各项功能的核心逻辑实现
                % 包括单聊（c2c）、上报(c2s)、推送(s2c)、群聊(c2g)
                case cowboy_bstr:to_upper(Type) of
                    <<"C2C">> -> % 单聊消息
                        websocket_logic:c2c(Id, CurrentUid, Data);
                    <<"C2C_CLIENT_ACK">> -> % 客户端确认投递消息
                        websocket_logic:c2c_client_ack(Id, CurrentUid);
                    <<"C2C_REVOKE">> -> % 客户端撤回消息
                        websocket_logic:c2c_revoke(Id, Data, Type);
                    <<"C2C_REVOKE_ACK">> -> % 客户端撤回消息ACK
                        websocket_logic:c2c_revoke(Id, Data, Type);
                    <<"C2G">> -> % 群聊消息
                        websocket_logic:c2g(Id, CurrentUid, Data)
                end
        end
    of
        Res ->
            ?LOG(Res),
            case Res of
                ok ->
                    {ok, State, hibernate};
                {reply, Msg2} ->
                    {reply, {text, jsone:encode(Msg2)}, State, hibernate}
            end
    catch
        Class:Reason ->
            ?LOG(["websocket_handle try catch: Class:", Class, "Reason:", Reason, "trace:", erlang:trace(all, true, [call])]),
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
    case lists:keyfind(current_uid, 1, State) of
        {current_uid, Uid} ->
            user_logic:offline(Uid, self());
        false ->
            chat_store_repo:dirty_delete(self())
    end,
    ok.
