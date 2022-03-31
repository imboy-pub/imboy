-module(stress_testing_ws_handler).

-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("common.hrl").

%%websocket 握手
init(Req0, State0) ->
    % ?LOG(cowboy_req:match_qs([{token, [], undefined}], Req0)),
    case cowboy_req:match_qs([{token, [], undefined}], Req0) of
        #{token := undefined} ->
            % HTTP 412 - 先决条件失败
            Req1 = cowboy_req:reply(412, Req0),
            {ok, Req1, State0};
        #{token := Token} ->
            Opt = #{
                num_acceptors => infinity,
                max_connections => infinity,
                max_frame_size => 1048576, % 1MB
                idle_timeout => 86400000 %  % Cowboy关闭连接空闲60秒 默认值为 60000
            },
            case catch token_ds:decrypt_token(Token) of
                {ok, Uid, _ExpireAt, _Type} ->
                    {cowboy_websocket, Req0, [{current_uid, Uid}|State0], Opt};
                {error, Code, _Msg, _Li} ->
                    {cowboy_websocket, Req0, [{error, Code} | State0], Opt}
            end
    end.

%%连接初始 onopen
websocket_init(State) ->
    CurrentPid = self(),
    % ?LOG([websocket_init, lists:keyfind(error, 1, State), State]),
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
            user_logic:online(CurrentUid, CurrentPid, <<"web">>),
            {ok, State, hibernate}
    end.

%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    % ?LOG(State),
    {reply, pong, State, hibernate};
websocket_handle({text, <<"ping">>}, State) ->
    % ?LOG(State),
    {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle({text, <<"{\"action\":\"confirmMessage", _/binary>>}, State) ->
    % 匹配前端确认消息，不做任何处理
    {ok, State, hibernate};
websocket_handle({text, Msg}, State) ->
    % ?LOG(Msg),
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
                % C2C/SYSTEM/GROUP
                Type = proplists:get_value(<<"conversation_type">>, Data),
                % ?LOG(Type),
                case cowboy_bstr:to_upper(Type) of
                    <<"C2C">> ->
                        websocket_logic:dialog(CurrentUid, Data);

                    <<"GROUP">> ->
                        websocket_logic:group_dialog(CurrentUid, Data);

                    <<"S2C">> ->
                        websocket_logic:system(CurrentUid, Data)
                end
        end
    of
        Res ->
            % ?LOG(Res),
            case Res of
                ok ->
                    {ok, State, hibernate};
                {reply, Msg2} ->
                    {reply, {text, jsone:encode(Msg2)}, State, hibernate}
            end
    catch
        ErrCode:ErrorMsg ->
            % lager:error("websocket_handle try catch: ~p", [ErrCode,ErrorMsg, Msg]),
            ?LOG(["websocket_handle try catch: ", ErrCode, ErrorMsg, Msg]),
            {ok, State, hibernate}
    end;
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.

%% 处理erlang 发送的消息
websocket_info({timeout, _Ref, Msg}, State) ->
    % ?LOG(Msg),
    {reply, {text, Msg}, State, hibernate};
websocket_info(stop, State) ->
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(_Reason, _Req, State) ->
    case lists:keyfind(current_uid, 1, State) of
        {current_uid, Uid} ->
            user_logic:offline(Uid, self());
        false ->
            chat_store_repo:dirty_delete(self())
    end,
    ok.
