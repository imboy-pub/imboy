-module(websocket_ds).
%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export([check_subprotocols/3]).
-export([auth/4]).

-include_lib("imboy/include/log.hrl").


-spec check_subprotocols(list(), any(), any()) ->
          {ok, any(), any()} | {cowboy_websocket, any(), any(), any()}.

check_subprotocols(undefined, Req0, State0) ->
    % HTTP 400 - 请求无效
    Req = cowboy_req:reply(400, Req0),
    {ok, Req, State0};
check_subprotocols(Subprotocols, Req0, State0) ->
    ImOpts = #{num_acceptors => infinity,
               max_connections => infinity,
               max_frame_size => 1048576,  % 1MB
               idle_timeout => 120000  %  % Cowboy关闭连接空闲120秒 默认值为 60000
        },
    % ?LOG([self(), State0, Subprotocols]),
    IsText = lists:member(<<"text">>, Subprotocols),
    case IsText == true of
        true ->
            Req =
                cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                                           <<"text">>,
                                           Req0),
            {cowboy_websocket, Req, State0, ImOpts};
        _ ->
            % HTTP 406 - 无法接受
            Req1 = cowboy_req:reply(406, Req0),
            {ok, Req1, State0}
    end.


-spec auth(Token :: binary(),
           Req1 :: any(),
           State1 :: list(),
           Opt :: any()) -> any().
auth(Token, Req1, State1, Opt) when is_binary(Token) ->
    ?LOG(["token", Token, token_ds:decrypt_token(Token)]),
    case token_ds:decrypt_token(Token) of
        {ok, Uid, _ExpireAt, _Type} ->
            Timeout = user_logic:idle_timeout(Uid),
            {cowboy_websocket, Req1,
                               [{current_uid, Uid} | State1],
                               Opt#{idle_timeout := Timeout}};
        {error, 705, Msg, _Li} ->
            % token无效、刷新token
            Req3 = imboy_response:error(Req1, Msg),
            {ok, Req3, State1};
        {error, Code, _Msg, _Li} ->
            {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
    end;
auth(Auth, Req0, State0, _Opt) ->
    ?LOG(["Auth", Auth]),
    % HTTP 412 - 先决条件失败 缺少token参数
    Req1 = cowboy_req:reply(412, Req0),
    {ok, Req1, State0}.
