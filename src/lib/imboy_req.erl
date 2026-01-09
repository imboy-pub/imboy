-module(imboy_req).


-export([peer_ip/1]).
-export([get_client_ip/1]).
-export([cookie/2]).
-export([get/1, get/2]).
-export([post/2, post/3]).
-export([post_params/1]).

-include("log.hrl").

-define(ReqHeaders, [
    {"content-type", "application/json"}
    , {"client", "imboy-req"}
]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 发送GET请求，使用默认请求头
%% @param Url 请求URL
%% @returns {ok, map()} | {error, any()} | {error, integer(), map()}
-spec get(binary() | list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
get(Url) ->
    req(get, Url, #{}, ?ReqHeaders).


%% @doc 发送GET请求，使用自定义请求头
%% @param Url 请求URL
%% @param Headers 请求头列表
%% @returns {ok, map()} | {error, any()} | {error, integer(), map()}
-spec get(binary() | list(), list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
get(Url, Headers) ->
    req(get, Url, #{}, Headers).


%% @doc 发送POST请求，使用默认请求头
%% @param Url 请求URL
%% @param Params 请求参数
%% @returns {ok, map()} | {error, any()} | {error, integer(), map()}
-spec post(binary() | list(), map() | list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
post(Url, Params) ->
    req(post, Url, Params, ?ReqHeaders).


%% @doc 发送POST请求，使用自定义请求头
%% @param Url 请求URL
%% @param Params 请求参数
%% @param Headers 请求头列表
%% @returns {ok, map()} | {error, any()} | {error, integer(), map()}
-spec post(binary() | list(), map() | list(), list()) -> {ok, map()} | {error, any()} | {error, integer(), map()}.
post(Url, Params, Headers) ->
    req(post, Url, Params, Headers).


%% @doc 从Cowboy请求中解析POST参数
%% 支持application/x-www-form-urlencoded和multipart/form-data格式
%% @param Req cowboy请求对象
%% @returns POST参数映射
-spec post_params(cowboy_req:req()) -> map().

post_params(Req) ->
    % 读取请求体
    case cowboy_req:read_body(Req) of
        {ok, Body, _Req2} when Body =/= <<>> ->
            % 获取Content-Type头部
            ContentType = cowboy_req:header(<<"content-type">>, Req, <<>>),
            % 解析不同类型的POST数据
            case parse_body_by_content_type(Body, ContentType) of
                {ok, Params} ->
                    Params;
                {error, _Reason} ->
                    % 解析失败时返回空列表
                    #{}
            end;
        {ok, <<>>, _Req2} ->
            % 空请求体
            #{}
    end.

%% @doc 根据Content-Type解析请求体
%% @param Body 请求体二进制数据
%% @param ContentType Content-Type头部
%% @returns {ok, Params} | {error, Reason}
-spec parse_body_by_content_type(binary(), binary()) -> {ok, map()} | {error, atom()}.

parse_body_by_content_type(Body, ContentType) ->
    case binary:match(ContentType, <<"application/x-www-form-urlencoded">>) of
        nomatch ->
            case binary:match(ContentType, <<"multipart/form-data">>) of
                nomatch ->
                    % 尝试解析JSON格式
                    try jsone:decode(Body, [{object_format, map}, native_utf8]) of
                        Map when is_map(Map) ->
                            {ok, Map};
                        _ ->
                            % 如果JSON解析失败，尝试简单的键值对解析
                            parse_key_value_pairs(Body)
                    catch
                        _:_ ->
                            % 如果JSON解析失败，尝试简单的键值对解析
                            parse_key_value_pairs(Body)
                    end;
                _ ->
                    % multipart/form-data暂时不支持
                    {ok, #{}}
            end;
        _ ->
            % application/x-www-form-urlencoded格式
            parse_urlencoded_body(Body)
    end.

%% @doc 解析URL编码的请求体
%% @param Body URL编码的请求体
%% @returns {ok, Params} | {error, Reason}
-spec parse_urlencoded_body(binary()) -> {ok, map()} | {error, atom()}.

parse_urlencoded_body(Body) ->
    try
        % 使用uri_string解析URL编码数据
        Decoded = uri_string:unquote(binary_to_list(Body)),
        % 解码后按&分割参数
        Pairs = string:tokens(Decoded, "&"),
        Params = lists:foldl(fun(Pair, Acc) ->
            case string:tokens(Pair, "=") of
                [Key, Value] ->
                    add_value(ec_cnv:to_binary(Key), ec_cnv:to_binary(Value), Acc);
                [Key] ->
                    add_value(ec_cnv:to_binary(Key), <<>>, Acc);
                _ ->
                    Acc
            end
        end, #{}, Pairs),
        {ok, Params}
    catch
        error:_ ->
            {error, parse_failed}
    end.

%% @doc 解析简单键值对格式
%% @param Body 键值对格式的请求体
%% 解析 URL-encoded form body，支持同 key 多值
%% 示例：
%%  <<"a=1&b=2&a=3">>
%% 返回：
%%  {ok, #{<<"a">> => [<<"1">>, <<"3">>], <<"b">> => <<"2">>}}
%%
-spec parse_key_value_pairs(binary()) -> {ok, map()} | {error, atom()}.
parse_key_value_pairs(Body) ->
    try
        %% 先按 & 切分出 key=value 对
        Pairs = binary:split(Body, <<"&">>, [global]),
        %% 遍历每一对参数，累加到 Map
        Map = lists:foldl(
            fun(Pair, Acc) ->
            %% 再按 = 分成 Key / Value
            case binary:split(Pair, <<"=">>) of
                %% 标准 key=value
                [Key, Value] ->
                    add_value(Key, Value, Acc);
                %% 只有 key（等价 key=）
                [Key] ->
                    add_value(Key, <<>>, Acc);
                %% 其他异常情况忽略
                _ ->
                    Acc
                end
            end,
            #{},      %% 初始为空 map
            Pairs
        ),
        {ok, Map}
    catch
        _:_ ->
            {error, parse_failed}
    end.


%% add_value/3
%% 功能：
%%  - 如果 key 第一次出现：直接放值
%%  - 如果 key 已存在且之前已经是 list：直接 append
%%  - 如果 key 已存在但之前是单值：转为 list 再加入
%%
add_value(Key, Value, Acc) ->
    case maps:get(Key, Acc, undefined) of
        %% 第一次出现该 Key
        undefined ->
            maps:put(Key, Value, Acc);
        %% 已经是 list，继续累加
        Existing when is_list(Existing) ->
            maps:put(Key, Existing ++ [Value], Acc);
        %% 第二次出现，把原来单值转成 list
        Existing ->
            maps:put(Key, [Existing, Value], Acc)
    end.


%% @doc 从请求中获取指定名称的Cookie值
%% @param Key Cookie名称
%% @param Req cowboy请求对象
%% @returns Cookie值或false
-spec cookie(binary(), cowboy_req:req()) -> binary() | false.
cookie(Key, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    maps:get(Key, maps:from_list(Cookies), false).

%% @doc 获取客户端IP地址（直接连接的IP）
%% @param Req cowboy请求对象
%% @returns IP地址字符串
-spec peer_ip(map()) -> binary().
peer_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    % io:format("Client IP: ~p, Port: ~p~n", [IP, Port]),
    % 将IP转换为可读格式
    IPString = inet:ntoa(IP),
    list_to_binary(IPString).

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
                    ec_cnv:to_binary(inet:ntoa(Ip))
            end;
        XForwardedFor when is_binary(XForwardedFor) ->
            % 如果有 X-Forwarded-For，取第一个IP
            case binary:split(XForwardedFor, <<",">>) of
                [FirstIp | _] ->
                    ec_cnv:to_binary(string:trim(FirstIp, trailing, "\s"));
                _ ->
                    XForwardedFor
            end
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
-spec req(atom(), binary() | list(), map() | list(), list()) ->
          {ok, map()} | {error, any()} | {error, integer(), map()}.
req(Method, Url, Params, Headers) ->
    _ = application:ensure_started(ssl),
    _ = application:ensure_started(inets),
    % 检查 content-type
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
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, jsone:decode(list_to_binary(Body), [{object_format, map}])};
        % {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, StatusCode, jsone:decode(list_to_binary(Body), [{object_format, map}])};
        {error, Reason} ->
            {error, Reason}
    end.
