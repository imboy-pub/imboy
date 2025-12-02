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

%% @doc 发送GET请求，使用默认请求头
%% @param Url 请求URL
%% @returns {ok, map()} | {error, any()}
-spec get(binary() | list()) -> {ok, map()} | {error, any()}.
get(Url) ->
    req(get, Url, #{}, ?ReqHeaders).


%% @doc 发送GET请求，使用自定义请求头
%% @param Url 请求URL
%% @param Headers 请求头列表
%% @returns {ok, map()} | {error, any()}
-spec get(binary() | list(), list()) -> {ok, map()} | {error, any()}.
get(Url, Headers) ->
    req(get, Url, #{}, Headers).


%% @doc 发送POST请求，使用默认请求头
%% @param Url 请求URL
%% @param Params 请求参数
%% @returns {ok, map()} | {error, any()}
-spec post(binary() | list(), map() | list()) -> {ok, map()} | {error, any()}.
post(Url, Params) ->
    req(post, Url, Params, ?ReqHeaders).


%% @doc 发送POST请求，使用自定义请求头
%% @param Url 请求URL
%% @param Params 请求参数
%% @param Headers 请求头列表
%% @returns {ok, map()} | {error, any()}
-spec post(binary() | list(), map() | list(), list()) -> {ok, map()} | {error, any()}.
post(Url, Params, Headers) ->
    req(post, Url, Params, Headers).


%% @doc 从请求中获取指定名称的Cookie值
%% @param Key Cookie名称
%% @param Req cowboy请求对象
%% @returns Cookie值或false
-spec cookie(binary(), cowboy_req:req()) -> binary() | false.
cookie(Key, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(Key, 1, Cookies) of
        {_, Val} ->
            Val;
        false ->
            false
    end.

%% @doc 获取客户端IP地址（直接连接的IP）
%% @param Req cowboy请求对象
%% @returns IP地址字符串
-spec peer_ip(cowboy_req:req()) -> binary().
peer_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    % io:format("Client IP: ~p, Port: ~p~n", [IP, Port]),
    % 将IP转换为可读格式
    IPString = inet:ntoa(IP),
    IPString.

%% @doc 获取客户端真实IP地址
%% 支持代理和负载均衡器场景下的真实IP获取
%% 优先检查 X-Forwarded-For 头部，如果没有则使用直接连接的IP
%% @param Req cowboy请求对象
%% @returns 客户端IP地址字符串
-spec get_client_ip(cowboy_req:req()) -> binary().
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


%% @doc 内部HTTP请求处理函数
%% 支持 GET 和 POST 请求，自动处理JSON编码和解码
%% @param Method HTTP方法（get 或 post）
%% @param Url 请求URL
%% @param Params 请求参数
%% @param Headers 请求头列表
%% @returns {ok, map()} | {error, any()}
%% 示例:
%%   imboy_req:post("http://127.0.0.1:9800/test/req_post", #{type => 1, b => 2}).
%%   imboy_req:post("http://127.0.0.1:9800/test/req_post", [1,2,3]).
%%   imboy_req:get("http://127.0.0.1:9800/test/req_get").
-spec req(atom(), binary() | list(), map() | list(), list()) -> {ok, map()} | {error, any()}.
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
