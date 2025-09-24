-module(imboy_req).


-export([peer_ip/1]).
-export([get_client_ip/1]).
-export([cookie/2]).
-export([get/1, get/2]).
-export([post/2, post/3]).

-include_lib("imlib/include/log.hrl").

-define(ReqHeaders, [
    {"content-type", "application/json"}
    , {"client", "imboy-req"}
]).

%% ===================================================================
%% API
%% ===================================================================

get(Url) ->
    req(get, Url, #{}, ?ReqHeaders).


get(Url, Headers) ->
    req(get, Url, #{}, Headers).


post(Url, Params) ->
    req(post, Url, Params, ?ReqHeaders).


post(Url, Params, Headers) ->
    req(post, Url, Params, Headers).


cookie(Key, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(Key, 1, Cookies) of
        {_, Val} ->
            Val;
        false ->
            false
    end.

peer_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    % io:format("Client IP: ~p, Port: ~p~n", [IP, Port]),
    % 将IP转换为可读格式
    IPString = inet:ntoa(IP),
    IPString.

%% 获取客户端IP地址
%% 支持代理和负载均衡器场景下的真实IP获取
get_client_ip(Req) ->
    % 首先检查 X-Forwarded-For 头部
    case cowboy_req:header(<<"x-forwarded-for">>, Req, undefined) of
        undefined ->
            % 如果没有 X-Forwarded-For，使用直接连接的IP
            case cowboy_req:peer(Req) of
                {Ip, _Port} when is_tuple(Ip) ->
                    ec_cnv:to_binary(inet:ntoa(Ip));
                _ ->
                    <<"unknown">>
            end;
        XForwardedFor when is_binary(XForwardedFor) ->
            % 如果有 X-Forwarded-For，取第一个IP
            case binary:split(XForwardedFor, <<",">>) of
                [FirstIp | _] ->
                    ec_cnv:to_binary(string:trim(FirstIp, trailing, "\s"));
                _ ->
                    XForwardedFor
            end;
        _ ->
            <<"unknown">>
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% https://stackoverflow.com/questions/19103694/simple-example-using-erlang-for-https-post
% imboy_req:post("http://127.0.0.1:9800/test/req_post", #{type => 1, b => 2}).
% imboy_req:post("http://127.0.0.1:9800/test/req_post", [1,2,3]).
% imboy_req:get("http://127.0.0.1:9800/test/req_get").
-spec req(atom(), list() | binary(), list() | map(), list()) -> {ok, map()} | {error, any()}.
req(Method, Url, Params, Headers) ->
    application:ensure_started(ssl),
    application:ensure_started(inets),
    % 检查 content-type
    ContentType = proplists:get_value("content-type", Headers, "application/json"),
    Request =
        case Method of
            post ->
                Bin = jsone:encode(Params, [native_utf8]),
                {Url, Headers, ContentType, Bin};
            get ->
                {Url, Headers};
            _ ->
                {Url, Headers}
        end,
    Response = httpc:request(Method, Request, [], []),
    ?DEBUG_LOG([response, Response]),
    case Response of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, jsone:decode(list_to_binary(Body))};
        % {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, StatusCode, jsone:decode(list_to_binary(Body))};
        {error, Reason} ->
            {error, Reason}
    end.
