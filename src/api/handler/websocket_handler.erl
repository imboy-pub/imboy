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
                            websocket_ds:check_subprotocols(Req0, [{current_uid, Uid}|State1]);
                        {error, Code, _Msg, _Li} ->
                            {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
                    end
            end
    end.

%%连接初始 onopen
websocket_init(State) ->
    SPid = self(),
    ?LOG([websocket_init, lists:keyfind(error, 1, State), State]),
    case lists:keyfind(error, 1, State) of
        {error, Code} ->
            Msg = [
                {<<"type">>,<<"error">>},
                {<<"code">>, Code},
                {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            erlang:start_timer(1, SPid, jsx:encode(Msg)),
            {ok, State, hibernate};
        false ->
            {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
            %%插入数据
            websocket_store_repo:dirty_insert(CurrentUid, SPid, web),
            case chat_message_ds:read_msg(CurrentUid, 1000) of
                {error, _Msg} ->
                    % "暂无离线消息";
                    ok;
                Msgs ->
                    % 发送离线消息
                    chat_message_ds:sent_offline_msg(SPid, Msgs, 0)
            end,
            {ok, State, hibernate}
    end.

%%处理客户端发送投递的消息 onmessage
websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle({text, Msg}, State) ->
    try
        case lists:keyfind(current_uid, 1, State) of
           {current_uid, CurrentUid} ->
                Data = jsx:decode(Msg),
                case lists:keyfind(<<"type">>, 1, Data) of
                    {<<"type">>,<<"dialog">>} ->
                        websocket_as:dialog(CurrentUid, Data);
                    {<<"type">>,<<"pong">>} ->
                        ok
                end;
            false ->
                ok
        end
    of
        _ -> ok
    catch
        ErrCode:ErrorMsg ->
            ?LOG(["websocket_handle try catch: ", ErrCode, ErrorMsg, Msg])
    end,
    {ok, State, hibernate};
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.

%% 处理erlang 发送的消息
websocket_info({timeout, Ref, Msg}, State) ->
    ?LOG(["websocket_info timeout ", Ref, Msg, State]),
    {reply, {text, Msg}, State, hibernate};
websocket_info(Info, State) ->
    ?LOG(["websocket_info ", Info, State]),
    {ok, State}.

%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(_Reason, _Req, State) ->
    SPid = self(),
    case lists:keyfind(current_uid, 1, State) of
        {current_uid, Uid} ->
            websocket_store_repo:dirty_delete(Uid);
        false ->
            ok
    end,
    % ?LOG([dirty_delete, Uid, SPid]),
    websocket_store_repo:dirty_delete(SPid),
    ok.
