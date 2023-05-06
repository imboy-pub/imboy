-module(websocket_ds).
%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export([check_subprotocols/2]).
-export([auth/4]).

-include_lib("imboy/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec check_subprotocols(list(), any()) -> {ok, any()} | {cowboy_websocket, any()}.
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

-spec auth(binary(), any(), map(), any()) -> any().
auth(Token, Req, State, Opt) when is_binary(Token) ->
    % ?LOG(["token", Token, token_ds:decrypt_token(Token)]),
    case token_ds:decrypt_token(Token) of
        {ok, Uid, _ExpireAt, _Type} ->
            DID = maps:get(did, State, <<"">>),
            DIDIsOnline = imboy_session:is_online(Uid, {did, DID}),
            auth_after(DIDIsOnline, Uid, Req, State, Opt);
        {error, Code, Msg, _Map} ->
            {ok, Req, State#{error => Code, msg => Msg}}
    end;
auth(Auth, Req0, State0, _Opt) ->
    ?LOG(["Auth", Auth]),
    % HTTP 412 - 先决条件失败 缺少token参数
    Req1 = cowboy_req:reply(412, Req0),
    {ok, Req1, State0}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec auth_after(boolean(), integer(), any(), map(), map()) ->
    {ok, any(), map()} | {cowboy_websocket, any(), map(), map()}.
auth_after(true, _Uid, Req0, State0, _Opt) ->
    % lager:warning("DeviceID ~p is online", [State0]),
    % 429 Too Many Requests
    Req = cowboy_req:reply(429, Req0),
    {ok, Req, State0};
auth_after(false, Uid, Req, State, Opt) ->
    Timeout = idle_timeout(Uid),
    {
        cowboy_websocket,
        Req,
        State#{current_uid => Uid},
        Opt#{idle_timeout := Timeout}
    }.

% 设置用户websocket超时时间，默认60秒
idle_timeout(_Uid) ->
    60000.
