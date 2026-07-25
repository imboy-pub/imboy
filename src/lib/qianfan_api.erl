-module(qianfan_api).

%%% @doc 百度千帆大模型 API 调用模块
%%% 提供对话创建、访问令牌获取、签名生成等功能
%%% @author leeyi
%%% @since 2024-02-28

-export([create_chat/3]).

-export([access_token/0]).

% -export([signature/3]).
-export([canonical_uri/1]).
-export([generate/1]).

-export([canonical_header/1]).

-include("log.hrl").

-define(SignedHeaders, [
    {"content-type", "application/json"},
    {"client", "imboy-req"}
]).

%% @doc 创建千帆对话
%% @param Uid 用户ID
%% @param Content 对话内容
%% @param History 历史消息列表
%% @returns 响应映射
%% @example
%% qianfan_api:create_chat(1, <<"介绍一下你自己"/utf8>>, []).
%% @see https://cloud.baidu.com/doc/WENXINWORKSHOP/s/clntwmv7t
-spec create_chat(integer(), binary(), list()) -> map().
create_chat(Uid, Content, History) ->
    Tk = access_token(),
    URL =
        <<"https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/completions_pro?access_token=",
            Tk/binary>>,
    Headers = [
        {"Content-Type", "application/json"},
        {"x-bce-date", ec_date:format_iso8601(calendar:local_time())}
    ],
    Method = "POST",
    Authorization = elib_cnv:implode("/", signature(Method, URL, Headers)),
    Data = #{
        % integer
        <<"user_id">> => Uid,
        <<"messages">> => History ++
            [
                #{
                    <<"content">> => Content,
                    <<"role">> => <<"user">>
                }
            ]
    },
    % ?DEBUG_LOG([Data]),
    {ok, RespMap} = elib_req:post(URL, Data, [{"Authorization", Authorization} | Headers]),
    ok = ?DEBUG_LOG([RespMap]),
    RespMap.

%% @doc 获取访问令牌（带缓存）
%% @returns Access Token（有效期30天，缓存2591997秒）
%% @example
%% qianfan_api:access_token().
-spec access_token() -> binary().
access_token() ->
    Key = qianfan_access_token,
    Fun = fun() ->
        % curl 'https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials&client_id=[API Key]&client_secret=[Secret Key]'
        Conf = config_ds:env(qianfan),
        %
        AppId = maps:get(api_key, Conf),
        %
        AppKey = maps:get(secret_key, Conf),
        URL = <<"https://aip.baidubce.com/oauth/2.0/token">>,
        Query = elib_cnv:implode("", [
            "grant_type=client_credentials&",
            "client_id=",
            AppId,
            "&"
            "client_secret=",
            AppKey,
            "&"
        ]),
        Headers = [
            {"Content-Type", "application/json"}
        ],
        {ok, RespMap} = elib_req:post(<<URL/binary, "?", Query/binary>>, [], Headers),
        maps:get(<<"access_token">>, RespMap)
    end,
    % 有效期30天 - 3秒
    imboy_cache:memo(Fun, Key, 2591997).

%% @doc 生成百度云 API 签名
%% @param Method HTTP 方法（GET、POST 等）
%% @param URL 请求 URL
%% @param Headers 请求头列表
%% @returns [AuthStringPrefix, SignedHeaders, Signature]
%% @example
%% qianfan_api:signature("POST", URL, Headers).
%% @see https://cloud.baidu.com/doc/Reference/s/hjwvz1y4f
-spec signature(string(), binary(), list()) -> [binary()].
signature(Method, URL, Headers) ->
    Conf = config_ds:env(qianfan),
    % accessKeyId
    Ak = maps:get(auth_access_key, Conf),
    %
    Sk = maps:get(auth_secret_key, Conf),

    {CanonicalURI, CanonicalQueryString} = canonical_uri(URL),
    {CanonicalHeaders, SignedHeaders} = canonical_header(Headers),

    ok = ?DEBUG_LOG([CanonicalHeaders]),
    % authStringPrefix代表认证字符串的前缀部分，即： bce-auth-v2/{accessKeyId}/{date}/{region}/{service}
    Date = ec_date:format("Ymd"),
    % ?
    Service = "bot",
    AuthStringPrefix = elib_cnv:implode("/", [
        "bce-auth-v2",
        Ak,
        Date,
        "sz",
        Service
    ]),
    % SigningKey = HMAC-SHA256-HEX(sk, AuthStringPrefix)
    SigningKey = elib_hasher:hmac_sha512(Sk, AuthStringPrefix),
    % Signature = HMAC-SHA256-HEX(SigningKey, CanonicalRequest)
    % CanonicalRequest的计算公式为： CanonicalRequest = HTTP Method + "\n" + CanonicalURI + "\n" + CanonicalQueryString + "\n" + CanonicalHeaders
    CanonicalRequest = elib_cnv:implode("\n", [
        Method,
        CanonicalURI,
        CanonicalQueryString,
        CanonicalHeaders
    ]),
    Signature = elib_hasher:hmac_sha512(SigningKey, CanonicalRequest),
    [AuthStringPrefix, SignedHeaders, Signature].

%% @doc 解析 URL 为规范 URI 和查询字符串
%% @param URL 完整 URL
%% @returns {CanonicalURI, CanonicalQueryString}
%% @example
%% qianfan_api:canonical_uri(<<"https://bos.cn-n1.baidubce.com/example?text=test">>).
-spec canonical_uri(binary()) -> {binary(), binary()}.
canonical_uri(URL) ->
    % URL = <<"https://bos.cn-n1.baidubce.com/example/测试"/utf8>>,
    EncodedURI = cow_uri:urlencode(URL),
    I2 = elib_str:replace(EncodedURI, "(?i)%2f", "/"),
    I3 = elib_str:replace(I2, "(?i)%3f", "?"),
    R1 =
        case uri_string:parse(I3) of
            #{} = Map -> Map;
            %% uri_string:parse/1 出错返回三元组 {error, Atom, Term}
            {error, _, _} -> #{path => <<"/">>, query => <<>>}
        end,
    CanonicalURI = maps:get(path, R1, <<"/">>),
    Query = maps:get(query, R1, <<>>),
    % Result = uri_string:parse(elib_str:replace(<<"https://bos.cn-n1.baidubce.com/example/hhh?s=1">>, "%2F", "/")).
    {CanonicalURI, cow_uri:urldecode(generate(Query))}.

%% @doc 生成规范查询字符串（CanonicalQueryString）
%% 对查询参数进行排序、编码和拼接
%% @param QueryString 查询字符串
%% @returns 规范化的查询字符串
-spec generate(binary()) -> binary().
generate(QueryString) when is_binary(QueryString) ->
    % 拆分查询字符串为键值对
    Pairs = split_query_string(QueryString),
    % 过滤并编码键值对
    EncodedPairs = [encode_pair(Key, Value) || {Key, Value} <- Pairs, Key /= <<"authorization">>],
    % 按字典顺序排序
    SortedPairs = lists:sort(EncodedPairs),
    % 连接排序后的键值对
    elib_cnv:implode("&", SortedPairs).
% list_to_binary(string:join(SortedPairs, "&")).

% 拆分查询字符串为键值对列表
-spec split_query_string(binary()) -> [{binary(), binary()}].
split_query_string(QueryString) ->
    Pairs = binary:split(QueryString, <<"&">>, [global]),
    [split_pair(Pair) || Pair <- Pairs].

% 拆分单个键值对
-spec split_pair(binary()) -> {binary(), binary()}.
split_pair(Pair) ->
    case binary:split(Pair, <<"=">>) of
        % 只有键的情况
        [Key] -> {Key, <<>>};
        % 键值对的情况
        [Key, Value] -> {Key, Value}
    end.

%% @doc 编码键值对（URL 编码）
%% @param Key 键
%% @param Value 值
%% @returns 编码后的 key=value 字符串
-spec encode_pair(binary(), binary()) -> binary().
encode_pair(Key, Value) ->
    EncodedKey = cow_uri:urlencode(Key),
    EncodedValue = cow_uri:urlencode(Value),
    <<EncodedKey/binary, "=", EncodedValue/binary>>.

%% @doc 生成规范请求头（CanonicalHeaders）
%% 对请求头进行筛选、修剪、编码、排序和拼接
%% @param Headers 原始请求头列表
%% @returns {CanonicalHeaders, SignedHeaders}
%% @example
%% Headers = [{"Host","bj.bcebos.com"}, {"x-bce-date","2015-04-27T08:23:49Z"}],
%% qianfan_api:canonical_header(Headers).
-spec canonical_header(list()) -> {string(), string()}.
canonical_header(Headers) ->
    % Step 1: Select and convert headers to lowercase
    SelectedHeaders = lists:filtermap(
        fun({Name, Value}) ->
            LowerName = string:to_lower(Name),
            case
                lists:member(LowerName, [
                    "host", "content-length", "content-type", "content-md5", "x-bce-date"
                ])
            of
                true ->
                    {true, {LowerName, Value}};
                false ->
                    case string:prefix(LowerName, "x-bce-") of
                        nomatch -> false;
                        _ -> {true, {LowerName, Value}}
                    end
            end
        end,
        Headers
    ),

    % Step 2: Trim whitespace and ignore empty headers
    TrimmedHeaders = [{Name, string:trim(Value)} || {Name, Value} <- SelectedHeaders, Value /= ""],

    % Step 3: URI Encode header values（对 Value 做 percent-encoding，保留 / 不编码）
    EncodedHeaders = [
        {Name, elib_str:replace(cow_uri:urlencode(ec_cnv:to_binary(Value)), "(?i)%2f", "/")}
     || {Name, Value} <- TrimmedHeaders
    ],

    % Step 4: Sort headers lexicographically
    SortedHeaders = lists:sort(EncodedHeaders),

    % Step 5: Join headers with newline character
    CanonicalHeaders = lists:join("\n", [
        Name ++ ":" ++ binary_to_list(Value)
     || {Name, Value} <- SortedHeaders
    ]),

    % Optionally, generate the signedHeaders string
    SignedHeaders = string:join(lists:sort([Name || {Name, _} <- SelectedHeaders]), ";"),

    % Return both CanonicalHeaders and SignedHeaders (if needed)
    {CanonicalHeaders, SignedHeaders}.

% %% 创建chat
% %% https://cloud.baidu.com/doc/WENXINWORKSHOP/s/clntwmv7t
% create_chat() ->
%     Url = <<"https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/completions_pro">>,
%     Headers = [
%         {"content-type", "application/json"}
%         , {"x-bce-date", "imboy-req"}
%         , {"Authorization", elib_dt:now()}
%     ],
%     elib_req:post(Url, #{}, Headers),
