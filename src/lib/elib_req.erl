-module(elib_req).

%%% @doc HTTP 请求处理模块
%%% 提供 GET/POST 请求、参数解析、IP 获取等功能

-export([peer_ip/1]).
-export([get_client_ip/1]).
-export([cookie/2]).
-export([get/1, get/2]).
-export([post/2, post/3]).
-export([body/2]).
-export([post_params/1]).
-export([parse_urlencoded_body/1, parse_key_value_pairs/1, parse_json_body/1, add_value/3]).

-include("log.hrl").

-define(ReqHeaders, [
    {"content-type", "application/json"},
    {"client", "imboy-req"}
]).

-spec get(binary() | list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
get(Url) ->
    req(get, Url, #{}, ?ReqHeaders).

-spec get(binary() | list(), list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
get(Url, Headers) ->
    req(get, Url, #{}, Headers).

-spec post(binary() | list(), map() | list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
post(Url, Params) ->
    req(post, Url, Params, ?ReqHeaders).

-spec post(binary() | list(), map() | list(), list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
post(Url, Params, Headers) ->
    req(post, Url, Params, Headers).

-spec body(cowboy_req:req(), list()) -> {ok, map(), cowboy_req:req()}.
body(Req0, _Opts) ->
    case read_full_body(Req0, <<>>) of
        {ok, <<>>, Req1} ->
            {ok, #{}, Req1};
        {ok, BodyBin, Req1} ->
            ContentType = cowboy_req:header(<<"content-type">>, Req1, <<>>),
            case parse_body_by_content_type(BodyBin, ContentType) of
                {ok, Params} when is_map(Params) ->
                    {ok, Params, Req1};
                _ ->
                    {ok, #{}, Req1}
            end
    end.

-spec post_params(cowboy_req:req()) -> map().
post_params(Req0) ->
    case body(Req0, []) of
        {ok, Params, _Req1} ->
            Params
    end.

-spec read_full_body(cowboy_req:req(), binary()) -> {ok, binary(), cowboy_req:req()}.
read_full_body(Req0, Acc0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req1} ->
            {ok, <<Acc0/binary, (ensure_binary(Data))/binary>>, Req1};
        {more, Data, Req1} ->
            read_full_body(Req1, <<Acc0/binary, (ensure_binary(Data))/binary>>)
    end.

-spec parse_body_by_content_type(binary(), binary()) -> {ok, map()} | {error, atom()}.
parse_body_by_content_type(Body, ContentType) ->
    case {has_content_type(ContentType, <<"application/x-www-form-urlencoded">>),
          has_content_type(ContentType, <<"multipart/form-data">>)} of
        {true, _} ->
            parse_urlencoded_body(Body);
        {false, true} ->
            {ok, #{}};
        {false, false} ->
            case parse_json_body(Body) of
                {ok, Map} when is_map(Map) ->
                    {ok, Map};
                {ok, _Other} ->
                    {ok, #{}};
                {error, _} ->
                    parse_key_value_pairs(Body)
            end
    end.

-spec parse_urlencoded_body(binary()) -> {ok, map()} | {error, atom()}.
parse_urlencoded_body(<<>>) ->
    {ok, #{}};
parse_urlencoded_body(Body) when is_binary(Body) ->
    parse_query_pairs(binary:split(Body, <<"&">>, [global])).

-spec parse_key_value_pairs(binary()) -> {ok, map()} | {error, atom()}.
parse_key_value_pairs(<<>>) ->
    {ok, #{}};
parse_key_value_pairs(Body) when is_binary(Body) ->
    parse_query_pairs(binary:split(Body, <<"&">>, [global])).

-spec parse_json_body(binary()) -> {ok, term()} | {error, atom()}.
parse_json_body(<<>>) ->
    {error, empty_body};
parse_json_body(Body) when is_binary(Body) ->
    try
        {ok, jsone:decode(Body, [{object_format, map}])}
    catch
        _:_ ->
            {error, invalid_json}
    end.

-spec parse_query_pairs([binary()]) -> {ok, map()} | {error, atom()}.
parse_query_pairs(Pairs) ->
    try
        Params = lists:foldl(fun parse_query_pair/2, #{}, Pairs),
        {ok, Params}
    catch
        _:_ ->
            {error, parse_failed}
    end.

parse_query_pair(<<>>, Acc) ->
    Acc;
parse_query_pair(Pair, Acc) ->
    {RawKey, RawValue} = split_pair(Pair),
    Key = decode_query_component(RawKey),
    Value = decode_query_component(RawValue),
    add_value(Key, Value, Acc).

-spec split_pair(binary()) -> {binary(), binary()}.
split_pair(Pair) ->
    case binary:match(Pair, <<"=">>) of
        {Pos, 1} ->
            <<Key:Pos/binary, $=, Value/binary>> = Pair,
            {Key, Value};
        nomatch ->
            {Pair, <<>>}
    end.

-spec decode_query_component(binary()) -> binary().
decode_query_component(Value) when is_binary(Value) ->
    Value1 = binary:replace(Value, <<"+">>, <<" ">>, [global]),
    try
        unicode:characters_to_binary(uri_string:unquote(binary_to_list(Value1)))
    catch
        _:_ ->
            Value1
    end.

-spec add_value(binary(), binary(), map()) -> map().
add_value(Key, Value, Acc) ->
    case maps:get(Key, Acc, undefined) of
        undefined ->
            maps:put(Key, Value, Acc);
        Existing when is_list(Existing) ->
            maps:put(Key, Existing ++ [Value], Acc);
        Existing ->
            maps:put(Key, [Existing, Value], Acc)
    end.

-spec cookie(binary(), cowboy_req:req()) -> binary() | false.
cookie(Key, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    maps:get(Key, maps:from_list(Cookies), false).

-spec peer_ip(map()) -> binary().
peer_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    list_to_binary(inet:ntoa(IP)).

-spec get_client_ip(cowboy_req:req()) -> binary().
get_client_ip(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req, undefined) of
        undefined ->
            case cowboy_req:peer(Req) of
                {Ip, _Port} when is_tuple(Ip) ->
                    ec_cnv:to_binary(inet:ntoa(Ip))
            end;
        XForwardedFor when is_binary(XForwardedFor) ->
            case binary:split(XForwardedFor, <<",">>) of
                [FirstIp | _] ->
                    ec_cnv:to_binary(string:trim(FirstIp));
                _ ->
                    XForwardedFor
            end
    end.

-spec req(atom(), binary() | list(), map() | list(), list()) ->
          {ok, map()} | {error, any()} | {error, integer(), map()}.
req(Method, Url, Params, Headers) ->
    _ = application:ensure_started(ssl),
    _ = application:ensure_started(inets),
    ContentType = maps:get("content-type", maps:from_list(Headers), "application/json"),
    Request =
        case Method of
            post ->
                Bin = jsone:encode(Params, [native_utf8]),
                {Url, Headers, ContentType, Bin};
            get ->
                {Url, Headers}
        end,
    Response = httpc:request(Method, Request, [], []),
    ok = ?DEBUG_LOG([response, Response]),
    case Response of
        {ok, {{_, 200, _}, _RespHeaders, Body}} ->
            {ok, decode_response_body(Body)};
        {ok, {{_, StatusCode, _}, _RespHeaders, Body}} ->
            {error, StatusCode, decode_response_body(Body)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec has_content_type(binary(), binary()) -> boolean().
has_content_type(ContentType, Expected) when is_binary(ContentType) ->
    binary:match(ContentType, Expected) =/= nomatch.

-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    iolist_to_binary(Value);
ensure_binary(Value) ->
    ec_cnv:to_binary(Value).

-spec decode_response_body(term()) -> map().
decode_response_body(Body) ->
    case parse_json_body(ensure_binary(Body)) of
        {ok, Payload} when is_map(Payload) ->
            Payload;
        {ok, Payload} ->
            #{<<"data">> => Payload};
        {error, _} ->
            #{}
    end.
