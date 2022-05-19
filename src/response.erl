-module(response).
%%%
% API响应json数据传输器
% The API responds to the JSON data transfer
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).


success(Req) ->
    reply_json(0, "success", [], Req).


success(Req, Payload) ->
    reply_json(0, "success", Payload, Req).


success(Req, Payload, Msg) ->
    reply_json(0, Msg, Payload, Req).


success(Req, Payload, Msg, Options) ->
    reply_json(0, Msg, Payload, Req, Options).


error(Req) ->
    reply_json(1, "error", [], Req).


error(Req, Msg) ->
    reply_json(1, Msg, [], Req).


error(Req, Msg, Code) ->
    reply_json(Code, Msg, [], Req).


error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, [], Req, Options).


%% Internal.
reply_json(Code, Msg, Payload, Req) ->
    reply_json(Code, Msg, Payload, Req, []).


reply_json(Code, Msg, Payload, Req, Options) ->
    LPayload = [{<<"code">>, Code},
                {<<"msg">>, unicode:characters_to_binary(Msg)},
                {<<"payload">>, Payload}
        % ,{<<"data">>, Payload}
        ],
    Body = jsone:encode(LPayload ++ Options, [native_utf8]),
    cowboy_req:reply(200,
                     #{<<"content-type">> =>
                           <<"application/json; charset=utf-8">>},
                     Body,
                     Req).
