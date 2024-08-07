-module(imboy_response).
%%%
% API响应json数据传输器
% The API responds to the JSON data transfer
%%%
-export([success/4, success/1, success/2, success/3]).
-export([error/4, error/1, error/2, error/3]).
-export([page_payload/4]).

%% ===================================================================
%% API
%% ===================================================================


-spec page_payload(integer(), integer(), integer(), list()) -> list().
page_payload(Total, Page, Size, List) ->
    [{<<"total">>, Total}, {<<"page">>, Page}, {<<"size">>, Size}, {<<"list">>, List}].


success(Req) ->
    reply_json(0, "success", #{}, Req).


success(Req, Payload) ->
    reply_json(0, "success", Payload, Req).


success(Req, Payload, Msg) ->
    reply_json(0, Msg, Payload, Req).


success(Req, Payload, Msg, Options) ->
    reply_json(0, Msg, Payload, Req, Options).


error(Req) ->
    reply_json(1, "error", #{}, Req).


error(Req, Msg) ->
    reply_json(1, Msg, #{}, Req).


error(Req, Msg, Code) ->
    reply_json(Code, Msg, #{}, Req).


error(Req, Msg, Code, Options) ->
    reply_json(Code, Msg, #{}, Req, Options).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


reply_json(Code, Msg, Payload, Req) ->
    reply_json(Code, Msg, Payload, Req, []).


reply_json(Code, Msg, Payload, Req, Options) ->
    Msg2 = case io_lib:printable_unicode_list(Msg) of
        true ->
            unicode:characters_to_binary(Msg);
        false ->
            ec_cnv:to_binary(Msg)
    end,
    LPayload = [{<<"code">>, Code}, {<<"msg">>, Msg2}, {<<"payload">>, Payload}],
    Body = jsone:encode(LPayload ++ Options, [native_utf8]),
    cowboy_req:reply(200
        , #{<<"content-type">> => <<"application/json; charset=utf-8">>}
        , Body
        , Req).
