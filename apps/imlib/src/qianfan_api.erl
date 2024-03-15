-module(qianfan_api).

%%%
%%   百度千帆大模型API调用erlang模块
%%   leeyi add 2024-02-28
%%%%

-export([create_chat/3]).

-export([access_token/0]).

% -export([signature/3]).
-export([canonical_uri/1]).
-export([generate/1]).

-export([canonical_header/1]).

-include_lib("imlib/include/log.hrl").

-define(SignedHeaders, [
    {"content-type", "application/json"}
    , {"client", "imboy-req"}
]).

% qianfan_api:create_chat(1, <<"介绍一下你自己"/utf8>, []).
% https://cloud.baidu.com/doc/WENXINWORKSHOP/s/clntwmv7t#body%E5%8F%82%E6%95%B0
-spec create_chat(integer(), binary(), list()) -> map().
create_chat(Uid, Content, History) ->
    Tk = access_token(),
    URL = <<"https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/completions_pro?access_token=", Tk/binary>>,
    Headers = [
        {"Content-Type","application/json"}
        , {"x-bce-date", ec_date:format_iso8601(calendar:local_time())}
    ],
    Method = "POST",
    Authorization = imboy_cnv:implode("/", signature(Method, URL, Headers)),
    Data = #{
        <<"user_id">> => imboy_hashids:encode(Uid), % string
        <<"messages">> => History ++ [
            #{
                <<"content">> => Content,
                <<"role">> => <<"user">>
            }
        ]
    },
    % ?LOG([Data]),
    {ok, RespMap} = imboy_req:post(URL, Data, [{"Authorization", Authorization} | Headers]),
    ?LOG([RespMap]),
    RespMap.


% qianfan_api:access_token().
access_token() ->
    Key = qianfan_access_token,
    Fun = fun() ->
        % curl 'https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials&client_id=[API Key]&client_secret=[Secret Key]'
        Conf = config_ds:env(qianfan),
        AppId = maps:get(api_key, Conf), %
        AppKey = maps:get(secret_key, Conf), %
        URL = <<"https://aip.baidubce.com/oauth/2.0/token">>,
        Query = imboy_cnv:implode("", [
            "grant_type=client_credentials&",
            "client_id=", AppId, "&"
            "client_secret=", AppKey, "&"
            ]),
        Headers = [
            {"Content-Type","application/json"}
        ],
        {ok, RespMap} = imboy_req:post(<<URL/binary, "?", Query/binary>>, [], Headers),
        maps:get(<<"access_token">>, RespMap)
    end,
    % 有效期30天 - 3秒
    imboy_cache:memo(Fun, Key, 2591997).


%% https://cloud.baidu.com/doc/Reference/s/hjwvz1y4f
% qianfan_api:signature("POST", <<"https://bos.cn-n1.baidubce.com/example?text&text1=测试&text10=test"/utf8>>, Headers)
signature(Method, URL, Headers) ->
    Conf = config_ds:env(qianfan),
    Ak = maps:get(auth_access_key, Conf), % accessKeyId
    Sk = maps:get(auth_secret_key, Conf), %

    {CanonicalURI, CanonicalQueryString} = canonical_uri(URL),
    {CanonicalHeaders, SignedHeaders} = canonical_header(Headers),

    ?LOG([CanonicalHeaders]),
    % authStringPrefix代表认证字符串的前缀部分，即： bce-auth-v2/{accessKeyId}/{date}/{region}/{service}
    Date = ec_date:format("Ymd"),
    Service = "bot", % ?
    AuthStringPrefix = imboy_cnv:implode("/", [
        "bce-auth-v2",
        Ak,
        Date,
        "sz",
        Service
    ]),
    % SigningKey = HMAC-SHA256-HEX(sk, AuthStringPrefix)
    SigningKey = imboy_hasher:hmac_sha512(Sk, AuthStringPrefix),
    % Signature = HMAC-SHA256-HEX(SigningKey, CanonicalRequest)
    % CanonicalRequest的计算公式为： CanonicalRequest = HTTP Method + "\n" + CanonicalURI + "\n" + CanonicalQueryString + "\n" + CanonicalHeaders
    CanonicalRequest = imboy_cnv:implode("\n", [
        Method,
        CanonicalURI,
        CanonicalQueryString,
        CanonicalHeaders
    ]),
    Signature = imboy_hasher:hmac_sha512(SigningKey, CanonicalRequest),
    [AuthStringPrefix, SignedHeaders, Signature].

% qianfan_api:canonical_uri(<<"https://bos.cn-n1.baidubce.com/example?text&text1=测试&text10=test"/utf8>>).
canonical_uri(URL) ->
    % URL = <<"https://bos.cn-n1.baidubce.com/example/测试"/utf8>>,
    EncodedURI = cow_uri:urlencode(URL),
    I2 = imboy_str:replace(EncodedURI, "(?i)%2f", "/"),
    I3 = imboy_str:replace(I2, "(?i)%3f", "?"),
    R1 = uri_string:parse(I3),
    CanonicalURI = maps:get(path, R1, <<"/">>),
    Query = maps:get(query, R1, <<>>),
    % Result = uri_string:parse(imboy_str:replace(<<"https://bos.cn-n1.baidubce.com/example/hhh?s=1">>, "%2F", "/")).
    {CanonicalURI, cow_uri:urldecode(generate(Query))}.

% qianfan_api:generate()
% 生成CanonicalQueryString
generate(QueryString) when is_binary(QueryString) ->
    % 拆分查询字符串为键值对
    Pairs = split_query_string(QueryString),
    % 过滤并编码键值对
    EncodedPairs = [encode_pair(Key, Value) || {Key, Value} <- Pairs, Key /= <<"authorization">>],
    % 按字典顺序排序
    SortedPairs = lists:sort(EncodedPairs),
    % 连接排序后的键值对
    imboy_cnv:implode("&", SortedPairs).
    % list_to_binary(string:join(SortedPairs, "&")).

% 拆分查询字符串为键值对列表
split_query_string(QueryString) ->
    Pairs = binary:split(QueryString, <<"&">>, [global]),
    [split_pair(Pair) || Pair <- Pairs].

% 拆分单个键值对
split_pair(Pair) ->
    case binary:split(Pair, <<"=">>) of
        [Key] -> {Key, <<>>}; % 只有键的情况
        [Key, Value] -> {Key, Value} % 键值对的情况
    end.

% 编码键值对
encode_pair(Key, Value) ->
    EncodedKey = cow_uri:urlencode(Key),
    EncodedValue = cow_uri:urlencode(Value),
    <<EncodedKey/binary, "=", EncodedValue/binary>>.


% Headers = [{"Host","bj.bcebos.com"}, {"Date","Mon, 27 Apr 2015 16:23:49 +0800"}, {"Content-Type","text/plain"}, {"Content-Length","8"}, {"Content-Md5","NFzcPqhviddjRNnSOGo4rw=="}, {"x-bce-date","2015-04-27T08:23:49Z"}].
% qianfan_api:canonical_header(Headers).
canonical_header(Headers) ->
    % Step 1: Select and convert headers to lowercase
    SelectedHeaders = lists:filtermap(
        fun({Name, Value}) ->
            LowerName = string:to_lower(Name),
            case lists:member(LowerName, ["host", "content-length", "content-type", "content-md5", "x-bce-date"]) of
                true -> {true, {LowerName, Value}};
                false ->
                    case string:prefix(LowerName, "x-bce-") of
                        nomatch -> false;
                        _ -> {true, {LowerName, Value}}
                    end
            end
        end, Headers),

    % Step 2: Trim whitespace and ignore empty headers
    TrimmedHeaders = [{Name, string:trim(Value)} || {Name, Value} <- SelectedHeaders, Value /= ""],

    % Step 3: URI Encode headers (stubbed)
    EncodedHeaders = [{Name, imboy_str:replace(cow_uri:urlencode(ec_cnv:to_binary(Value)), "(?i)%2f", "/")} || {Name, Value} <- TrimmedHeaders],

    % Step 4: Sort headers lexicographically
    SortedHeaders = lists:sort(EncodedHeaders),

    % Step 5: Join headers with newline character
    CanonicalHeaders = lists:join("\n", [Name ++ ":" ++ binary_to_list(Value) || {Name, Value} <- SortedHeaders]),

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
%         , {"Authorization", imboy_dt:now()}
%     ],
%     imboy_req:post(Url, #{}, Headers),
