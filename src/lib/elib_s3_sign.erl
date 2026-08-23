-module(elib_s3_sign).
%%%
% AWS Signature Version 4 实现（纯 OTP，无第三方依赖）
% 用于 Garage S3 兼容存储的请求签名和 Presigned URL 生成
%
% Garage 使用 path-style URL：endpoint/bucket/key
% 不是 AWS 的 virtual-hosted-style (bucket.endpoint/key)
%%%

-export([presign_put/5, presign_get/4, presign_delete/4, authorization_header/7]).
-export([format_date/1, format_amz_date/1]).
-export([uri_encode_path/1]).

%% @doc 生成 presigned PUT URL（Flutter 直传用）
%% Expires 单位：秒，最大 604800（7天）
-spec presign_put(binary(), binary(), binary(), binary(), pos_integer()) -> binary().
presign_put(Endpoint, Bucket, ObjectKey, MimeType, Expires) ->
    presign_url(<<"PUT">>, Endpoint, Bucket, ObjectKey, MimeType, Expires).

%% @doc presigned URL 生成核心，按 HTTP 方法签名（PUT 上传 / GET 下载）
-spec presign_url(binary(), binary(), binary(), binary(), binary(), pos_integer()) -> binary().
presign_url(Method, Endpoint, Bucket, ObjectKey, MimeType, Expires) ->
    Now = calendar:universal_time(),
    DateStr = format_date(Now),
    AmzDate = format_amz_date(Now),
    Cfg = garage_config(),
    Region = maps:get(region, Cfg, <<"garage">>),
    AccessKey = maps:get(access_key, Cfg, <<>>),
    SecretKey = maps:get(secret_key, Cfg, <<>>),

    Credential = <<AccessKey/binary, "/", DateStr/binary, "/", Region/binary, "/s3/aws4_request">>,
    SignedHeaders = <<"host">>,
    Host = host_from_endpoint(Endpoint),

    RawParts = [
        <<"X-Amz-Algorithm=AWS4-HMAC-SHA256">>,
        <<"X-Amz-Credential=", (uri_encode(Credential))/binary>>,
        <<"X-Amz-Date=", AmzDate/binary>>,
        <<"X-Amz-Expires=", (integer_to_binary(Expires))/binary>>,
        <<"X-Amz-SignedHeaders=", SignedHeaders/binary>>
        | case MimeType of
            <<>> -> [];
            _ -> [<<"Content-Type=", (uri_encode(MimeType))/binary>>]
        end
    ],
    %% SigV4 规范：Canonical Query String 必须按字典序排列
    QueryParts = lists:sort(RawParts),
    QueryStr = lists:join(<<"&">>, QueryParts),
    QueryBin = iolist_to_binary(QueryStr),

    %% SigV4 规范：Canonical URI 的路径段必须 URI 编码（保留 '/' 分隔符）。
    %% 否则含空格/中文等字符的文件名签名与实际请求路径不一致 → 403 SignatureDoesNotMatch。
    EncObjectKey = uri_encode_path(ObjectKey),
    CanonicalUri = <<"/", Bucket/binary, "/", EncObjectKey/binary>>,
    CanonicalHeaders = <<"host:", Host/binary, "\n">>,

    CanonicalRequest = iolist_to_binary([
        Method,
        "\n",
        CanonicalUri,
        "\n",
        QueryBin,
        "\n",
        CanonicalHeaders,
        "\n",
        SignedHeaders,
        "\n",
        "UNSIGNED-PAYLOAD"
    ]),

    StringToSign = string_to_sign(AmzDate, DateStr, Region, CanonicalRequest),
    SignKey = signing_key(SecretKey, DateStr, Region, <<"s3">>),
    Sig = binary_to_hex(hmac_sha256(SignKey, StringToSign)),

    <<Endpoint/binary, "/", Bucket/binary, "/", EncObjectKey/binary, "?", QueryBin/binary,
        "&X-Amz-Signature=", Sig/binary>>.

%% @doc 生成 presigned GET URL（附件查看用）
-spec presign_get(binary(), binary(), binary(), pos_integer()) -> binary().
presign_get(Endpoint, Bucket, ObjectKey, Expires) ->
    presign_url(<<"GET">>, Endpoint, Bucket, ObjectKey, <<>>, Expires).

%% @doc 生成 presigned DELETE URL（服务端删除对象用）
%% query 签名仅签 host+方法，规避经 nginx 反代后 Garage 对 header 鉴权
%% "Invalid signature" 的坑（与 head_object 改 presigned GET 同源）。
-spec presign_delete(binary(), binary(), binary(), pos_integer()) -> binary().
presign_delete(Endpoint, Bucket, ObjectKey, Expires) ->
    presign_url(<<"DELETE">>, Endpoint, Bucket, ObjectKey, <<>>, Expires).

%% @doc 生成 Authorization Header（服务端 PUT/DELETE 用）
-spec authorization_header(binary(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    binary().
authorization_header(Method, Bucket, ObjectKey, ContentType, AmzDate, AccessKey, SecretKey) ->
    Cfg = garage_config(),
    Region = maps:get(region, Cfg, <<"garage">>),
    DateStr = binary:part(AmzDate, 0, 8),

    %% x-amz-content-sha256 必须在 SignedHeaders 中（Garage 要求）
    %% Canonical headers 按字典序：content-type < host < x-amz-content-sha256 < x-amz-date
    SignedHeaders =
        case ContentType of
            <<>> -> <<"host;x-amz-content-sha256;x-amz-date">>;
            _ -> <<"content-type;host;x-amz-content-sha256;x-amz-date">>
        end,

    Endpoint = maps:get(endpoint, Cfg, <<"http://127.0.0.1:3900">>),
    Host = host_from_endpoint(Endpoint),

    CanonicalUri = <<"/", Bucket/binary, "/", (uri_encode_path(ObjectKey))/binary>>,
    CanonicalHeaders =
        case ContentType of
            <<>> ->
                <<"host:", Host/binary, "\nx-amz-content-sha256:UNSIGNED-PAYLOAD\nx-amz-date:",
                    AmzDate/binary, "\n">>;
            _ ->
                <<"content-type:", ContentType/binary, "\nhost:", Host/binary,
                    "\nx-amz-content-sha256:UNSIGNED-PAYLOAD\nx-amz-date:", AmzDate/binary, "\n">>
        end,

    CanonicalRequest = iolist_to_binary([
        Method,
        "\n",
        CanonicalUri,
        "\n",
        "\n",
        CanonicalHeaders,
        "\n",
        SignedHeaders,
        "\n",
        "UNSIGNED-PAYLOAD"
    ]),

    StringToSign = string_to_sign(AmzDate, DateStr, Region, CanonicalRequest),
    SignKey = signing_key(SecretKey, DateStr, Region, <<"s3">>),
    Sig = binary_to_hex(hmac_sha256(SignKey, StringToSign)),

    Credential = <<AccessKey/binary, "/", DateStr/binary, "/", Region/binary, "/s3/aws4_request">>,
    <<"AWS4-HMAC-SHA256 Credential=", Credential/binary, ", SignedHeaders=", SignedHeaders/binary,
        ", Signature=", Sig/binary>>.

%% ===== 内部函数 =====

-spec hmac_sha256(binary(), binary()) -> binary().
hmac_sha256(Key, Data) ->
    crypto:mac(hmac, sha256, Key, Data).

-spec signing_key(binary(), binary(), binary(), binary()) -> binary().
signing_key(SecretKey, Date, Region, Service) ->
    K1 = hmac_sha256(<<"AWS4", SecretKey/binary>>, Date),
    K2 = hmac_sha256(K1, Region),
    K3 = hmac_sha256(K2, Service),
    hmac_sha256(K3, <<"aws4_request">>).

-spec string_to_sign(binary(), binary(), binary(), binary()) -> binary().
string_to_sign(AmzDate, DateStr, Region, CanonicalRequest) ->
    Scope = <<DateStr/binary, "/", Region/binary, "/s3/aws4_request">>,
    Hash = binary_to_hex(crypto:hash(sha256, CanonicalRequest)),
    <<"AWS4-HMAC-SHA256\n", AmzDate/binary, "\n", Scope/binary, "\n", Hash/binary>>.

-spec garage_config() -> map().
garage_config() ->
    application:get_env(imboy, garage, #{}).

%% @doc 从 endpoint 提取 SigV4 签名用 host（保留显式端口，剥离路径前缀）。
%% 支持反代形态的 public endpoint（如 https://api.example.com/s3 →
%% api.example.com）：路径前缀不属于 Host canonical header；反代（nginx
%% location /s3/ → proxy_pass …/ 剥前缀）转发后的 Host 必须与签名 host
%% 一致，Garage 重算签名才能通过。
-spec host_from_endpoint(binary()) -> binary().
host_from_endpoint(Endpoint) ->
    NoScheme =
        case binary:split(Endpoint, <<"://">>) of
            [_, Rest] -> Rest;
            _ -> Endpoint
        end,
    case binary:split(NoScheme, <<"/">>) of
        [Host | _] -> Host;
        [] -> NoScheme
    end.

-spec format_date(calendar:datetime()) -> binary().
format_date({{Y, M, D}, _}) ->
    iolist_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Y, M, D])).

-spec format_amz_date(calendar:datetime()) -> binary().
format_amz_date({{Y, M, D}, {H, Min, S}}) ->
    iolist_to_binary(
        io_lib:format(
            "~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0BZ", [Y, M, D, H, Min, S]
        )
    ).

-spec binary_to_hex(binary()) -> binary().
binary_to_hex(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).

-spec uri_encode(binary()) -> binary().
uri_encode(Bin) ->
    iolist_to_binary([encode_char(C) || <<C>> <= Bin]).

%% @doc 对象 Key 路径编码：逐字节百分号编码，但保留 '/' 分隔符。
%% 用于 SigV4 Canonical URI 与实际请求 URL 的对象路径。
%% ASCII 安全字符（字母/数字/-_.~ 与 '/'）保持原样，对纯 ASCII Key 无任何变化。
-spec uri_encode_path(binary()) -> binary().
uri_encode_path(Bin) ->
    iolist_to_binary([encode_path_char(C) || <<C>> <= Bin]).

encode_path_char($/) -> $/;
encode_path_char(C) -> encode_char(C).

encode_char(C) when C >= $A, C =< $Z -> C;
encode_char(C) when C >= $a, C =< $z -> C;
encode_char(C) when C >= $0, C =< $9 -> C;
encode_char($-) -> $-;
encode_char($_) -> $_;
encode_char($.) -> $.;
encode_char($~) -> $~;
encode_char(C) -> io_lib:format("%~2.16.0B", [C]).
