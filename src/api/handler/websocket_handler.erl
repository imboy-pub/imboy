-module(websocket_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("imboy.hrl").

init(Req0, State0) ->
    case websocket_ds:check_subprotocols(Req0, State0) of
        {ok, Req1, State1} ->
            {ok, Req1, State1};
        {cowboy_websocket, Req1, State1, Opt} ->
            case cowboy_req:match_qs([{token, [], undefined}], Req1) of
                #{token := undefined} ->
                    % HTTP 412 - 先决条件失败
                    Req2 = cowboy_req:reply(412, Req1),
                    {ok, Req2, State1};
                #{token := Token} ->
                    case catch token_ds:decrypt_token(Token) of
                        {ok, Id, _ExpireAt} ->
                            Uid = list_to_integer(binary_to_list(Id)),
                            {cowboy_websocket, Req1, [{current_uid, Uid}|State1], Opt};
                        {error, Code, _Msg, _Li} ->
                            {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
                    end
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
                {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            {reply, {text, jsx:encode(Msg)}, State, hibernate};
        false ->
            {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
            % 用户上线
            user_as:online(CurrentUid, CurrentPid, web),

            % 检查离线消息
            dialog_msg_as:check_msg(CurrentUid, CurrentPid),

            % 检查群聊离线消息
            group_msg_as:check_msg(CurrentUid, CurrentPid),

            {ok, State, hibernate}
    end.

%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    % ?LOG(State),
    {reply, pong, State, hibernate};
websocket_handle({text, <<"ping">>}, State) ->
    % ?LOG(State),
    {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle({text, Msg}, State) ->
    % ?LOG([State, Msg]),
    try
        {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
        Data = jsx:decode(Msg),
        case lists:keyfind(<<"type">>, 1, Data) of
            {<<"type">>,<<"dialog">>} ->
                websocket_as:dialog(CurrentUid, Data);
            {<<"type">>,<<"friend">>} ->
                websocket_as:dialog(CurrentUid, Data);
            {<<"type">>,<<"group">>} ->
                websocket_as:group_dialog(CurrentUid, Data);
            {<<"type">>,<<"pong">>} ->
                ok
        end
    of
        Res ->
            case Res of
                ok ->
                    {ok, State, hibernate};
                {reply, Msg2} ->
                    {reply, {text, jsx:encode(Msg2)}, State, hibernate}
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
            user_as:offline(Uid);
        false ->
            chat_store_repo:dirty_delete(self())
    end,
    ok.
