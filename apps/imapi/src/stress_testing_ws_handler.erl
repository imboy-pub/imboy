-module(stress_testing_ws_handler).

-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("imlib/include/log.hrl").


%%websocket 握手
init(Req0, State0) ->
    % ?DEBUG_LOG(cowboy_req:match_qs([{token, [], undefined}], Req0)),
    case cowboy_req:match_qs([{token, [], undefined}], Req0) of
        #{token := undefined} ->
            % HTTP 412 - 先决条件失败
            Req1 = cowboy_req:reply(412, Req0),
            {ok, Req1, State0};
        #{token := Token} ->
            Opt = #{
                    num_acceptors => infinity,
                    max_connections => infinity,
                    max_frame_size => 1048576,  % 1MB
                    idle_timeout => 86400000  %  % Cowboy关闭连接空闲60秒 默认值为 60000
                   },
            case catch token_ds:decrypt_token(Token) of
                {ok, Uid, _ExpireDAt, _Type} ->
                    {cowboy_websocket, Req0, State0#{current_uid => Uid}, Opt};
                {error, Code, _Msg, _Map} ->
                    {ok, Req0, State0#{error => Code}, Opt}
            end
    end.


%%连接初始 onopen
websocket_init(State) ->
    CurrentPid = self(),
    case maps:find(error, State) of
        {ok, Code} ->
            Msg = [{<<"type">>, <<"error">>}, {<<"code">>, Code}, {<<"timestamp">>, imboy_dt:now()}],
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        error ->
            CurrentUid = maps:get(current_uid, State),
            % 用户上线
            user_logic:online(CurrentUid, CurrentPid, <<"web">>),
            {ok, State, hibernate}
    end.


%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    % ?DEBUG_LOG(State),
    {reply, pong, State, hibernate};
websocket_handle({text, <<"ping">>}, State) ->
    % ?DEBUG_LOG(State),
    {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle({text, <<"{\"action\":\"confirmMessage", _/binary>>}, State) ->
    % 匹配前端确认消息，不做任何处理
    {ok, State, hibernate};
websocket_handle({text, Msg}, State) ->
    % ?DEBUG_LOG(Msg),
    try case lists:keyfind(error, 1, State) of
            {error, Code} ->
                ErrMsg = [{<<"type">>, <<"error">>}, {<<"code">>, Code}, {<<"timestamp">>, imboy_dt:now()}],
                {reply, ErrMsg};
            false ->
                CurrentUid = maps:get(current_uid, State),
                Data = jsone:decode(Msg, [{object_format, proplist}]),
                % C2C/SYSTEM/GROUP
                Type = proplists:get_value(<<"conversation_type">>, Data),
                % ?DEBUG_LOG(Type),
                case cowboy_bstr:to_upper(Type) of
                    <<"C2C">> ->
                        websocket_logic:dialog(CurrentUid, Data);

                    <<"GROUP">> ->
                        websocket_logic:group_dialog(CurrentUid, Data);

                    <<"S2C">> ->
                        websocket_logic:s2c(CurrentUid, Data)
                end
        end of
        Res ->
            % ?DEBUG_LOG(Res),
            case Res of
                ok ->
                    {ok, State, hibernate};
                {reply, Msg2} ->
                    {reply, {text, jsone:encode(Msg2, [native_utf8])}, State, hibernate}
            end
    catch
        ErrCode:ErrorMsg ->
            ?DEBUG_LOG(["websocket_handle try catch: ", ErrCode, ErrorMsg, Msg]),
            {ok, State, hibernate}
    end;
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.


%% 处理erlang 发送的消息
websocket_info({timeout, _Ref, Msg}, State) ->
    % ?DEBUG_LOG(Msg),
    {reply, {text, Msg}, State, hibernate};
websocket_info(stop, State) ->
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State}.


%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(Reason, _Req, State) ->
    ?DEBUG_LOG([terminate, cowboy_clock:rfc1123(), State, Reason]),
    case maps:find(current_uid, State) of
        {ok, Uid} when is_integer(Uid) ->
            DID = maps:get(did, State, <<"">>),
            user_logic:offline(Uid, self(), DID),
            ok;
        error ->
            ok
    end.
