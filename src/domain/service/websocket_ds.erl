-module (websocket_ds).
%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export ([check_subprotocols/2]).

-include("imboy.hrl").

check_subprotocols(Req0, State1) ->
    % ?LOG([Req0, State1]),
    % Cowboy关闭连接空闲60秒 默认值为 60000
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            % HTTP 400 - 请求无效
            Req = cowboy_req:reply(400, Req0),
            {ok, Req, State1};
        Subprotocols ->
            % ?LOG([self(), State1, Subprotocols]),
            IsText = lists:member(<<"text">>, Subprotocols),
            if
                IsText == true ->
                    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"text">>, Req0),
                    {cowboy_websocket, Req, State1, #{idle_timeout => 60000}};
                true ->
                    % HTTP 406 - 无法接受
                    Req1 = cowboy_req:reply(406, Req0),
                    {ok, Req1, State1}
            end
    end.
