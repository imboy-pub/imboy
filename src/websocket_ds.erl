-module(websocket_ds).
%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export([check_subprotocols/2]).
-export([auth/4]).

-include_lib("imboy/include/log.hrl").


-spec check_subprotocols(list(), any()) ->
          {ok, any()} | {cowboy_websocket, any()}.

check_subprotocols(undefined, Req0) ->
    % HTTP 400 - 请求无效
    Req = cowboy_req:reply(400, Req0),
    {ok, Req};
check_subprotocols([], Req0) ->
    % HTTP 406 - 无法接受
    Req = cowboy_req:reply(406, Req0),
    {ok, Req};
check_subprotocols([H|_Tail], Req0) ->
    % [<<"sip">>,<<"text">>] = Subprotocols
    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, H, Req0),
    {cowboy_websocket, Req}.

-spec auth(Token :: binary(),
           Req1 :: any(),
           State1 :: map(),
           Opt :: any()) -> any().
auth(Token, Req1, State1, Opt) when is_binary(Token) ->
    % ?LOG(["token", Token, token_ds:decrypt_token(Token)]),
    case token_ds:decrypt_token(Token) of
        {ok, Uid, _ExpireAt, _Type} ->
            Timeout = user_logic:idle_timeout(Uid),
            {
                cowboy_websocket,
                Req1,
                State1#{current_uid => Uid},
                Opt#{idle_timeout := Timeout}
            };
        {error, Code, Msg, _Map} ->
            {ok, Req1, State1#{error => Code, msg => Msg}}
    end;
auth(Auth, Req0, State0, _Opt) ->
    ?LOG(["Auth", Auth]),
    % HTTP 412 - 先决条件失败 缺少token参数
    Req1 = cowboy_req:reply(412, Req0),
    {ok, Req1, State0}.
