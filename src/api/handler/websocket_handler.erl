-module(websocket_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

-include("imboy.hrl").

init(Req0, State0) ->
    Cookies = cowboy_req:parse_cookies(Req0),
    {Req1, State1} = case lists:keyfind(<<"imboy-token">>, 1, Cookies) of
        false ->
            {resp_json_dto:error(Req0, "Token不存在", 706), State0};
        {_, Token}->
            case token_ds:decrypt_token(Token) of
                {ok, Id, _ExpireAt} ->
                    Uid = list_to_integer(binary_to_list(Id)),
                    {Req0, [{current_uid, Uid}|State0]};
                {error, 707, _Msg, [Id, _ExpireAt]} ->
                    % 自动刷新 imboy-token
                    Token2 = token_ds:encrypt_token(Id),
                    Req2 = cowboy_req:set_resp_cookie(<<"imboy-token">>,
                        Token2, Req0, #{path => <<"/">>}),
                    Uid = list_to_integer(binary_to_list(Id)),
                    {Req2, [{current_uid, Uid}|State0]};
                {error, 706, Msg, _Li} ->
                    {resp_json_dto:error(Req0, Msg, 706), State0}
            end
    end,
    % ?LOG([Req1, State1]),
    % Cowboy关闭连接空闲60秒 默认值为 60000
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req1) of
        undefined ->
            {cowboy_websocket, Req1, State1, #{idle_timeout => 100}};
        Subprotocols ->
            % ?LOG([self(), State1, Subprotocols]),
            IsText = lists:member(<<"text">>, Subprotocols),
            if
                IsText == true ->
                    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"text">>, Req1),
                    {cowboy_websocket, Req, State1, #{idle_timeout => 60000}};
                true ->
                    Req = cowboy_req:reply(400, Req1),
                    {ok, Req, State1}
            end
    end.

%%连接初始 onopen
websocket_init(State) ->
    SPid = self(),
    ?LOG([websocket_init, State]),
    case lists:keyfind(current_uid, 1, State) of
        {current_uid, CurrentUid} ->
            ?LOG([dirty_insert, CurrentUid, SPid]),
            %%插入数据
            websocket_store_repo:dirty_insert(CurrentUid, SPid, web),
            Msg = [ {<<"type">>,<<"websocket_init ping">>},
              {<<"CurrentUid">>, CurrentUid},
              {<<"pid">>, list_to_binary(pid_to_list(SPid))},
              {<<"datetime">>, calendar:local_time()},
              {<<"timestamp">>, imboy_func:milliseconds()}
            ],
            {reply, {text, jsx:encode(Msg)}, State};
            % erlang:start_timer(10, SPid, jsx:encode(Msg)),
            % io:format("websocket_init Pid: ~p, State~p~n", [SPid, State]);
        false ->
            {ok, State}
    end.

%%处理客户端发送投递的消息 onmessage
websocket_handle({text, <<"ping">>}, State) ->
    {[{text, <<"pong">>}], State};
websocket_handle({text, Msg}, State) ->
    try
        {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
        Data = jsx:decode(Msg),
        case lists:keyfind(<<"type">>, 1, Data) of
            {<<"type">>,<<"dialog">>} ->
                websocket_as:dialog(CurrentUid, Data);
            {<<"type">>,<<"pong">>} ->
                ok
        end
    catch
        ErrCode:ErrorMsg ->
            ?LOG(["websocket_handle try catch", ErrCode, ErrorMsg, Msg])
    end,
    {[], State, hibernate};
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.

%% 处理erlang 发送的消息
websocket_info({timeout, Ref, Msg}, State) ->
    try
        {current_uid, CurrentUid} = lists:keyfind(current_uid, 1, State),
        SPid = self(),
        Msg2 = [
            {<<"type">>,<<"websocket_info timeout">>},
            {<<"CurrentUid">>, CurrentUid},
            {<<"pid">>, list_to_binary(pid_to_list(SPid))},
            {<<"timestamp">>, imboy_func:milliseconds()}
        ],
        ?LOG(["websocket_info timeout", Ref, Msg, Msg2, State])
    of
        _ -> ok
    catch
        ErrCode:ErrorMsg ->
            ?LOG(["websocket_info websocket_info try catch", ErrCode, ErrorMsg])
    end,
    {[{text, Msg}], State};
websocket_info(Info, State) ->
    ?LOG(["websocket_info ", Info, State]),
    {ok, State}.

%%断开socket onclose
websocket_terminate(_Reason, _Req, State) ->
    SPid = self(),
    {current_uid, Uid} = lists:keyfind(current_uid, 1, State),
    ?LOG([dirty_delete, Uid, SPid]),
    websocket_store_repo:dirty_delete(SPid),
    websocket_store_repo:dirty_delete(Uid),
    io:format("websocket_terminate Pid: ~p, State:~p ~n", [SPid, State]),
    ok.
