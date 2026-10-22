-module(push_provider_fcm_token).
-moduledoc "FCM service account → OAuth2 access token 刷新（G4/Q09）。".
%%%
% FCM access token 刷新 provider（Google OAuth2 JWT-bearer 流）
%
% G4 缺口修复（Q09，2026-10-07）：FCM 出口此前仅支持静态
% {fcm_access_token, ...} 配置——OAuth2 access token 约 1 小时过期，
% 无刷新机制即无法长期运行。本模块实现标准 service account 刷新链路：
%
%   service account JSON (fcm_service_account_json 内联 / _path 文件)
%     → RS256 JWT（iss=client_email, aud=token_uri,
%        scope=firebase.messaging, exp=iat+1h）
%     → POST token_uri (grant_type=jwt-bearer, gun HTTP/2 TLS)
%     → access_token 缓存 50 分钟（depcache，同 APNs JWT 口径）
%
% 失败分类（fail-closed，绝不打真实凭据）：
%   {error, not_configured}        无 service account 配置（静态 token
%                                  路径由 push_notification_ds:get_fcm_config
%                                  自行回退，不经本模块）
%   {error, {config_error, R}}     JSON/字段/私钥解析失败——配置类错误，
%                                  调用方不应重试
%   {error, {fcm_token_error, S}}  token endpoint 明确拒绝（4xx/5xx）——
%                                  不缓存、不建议逐消息重试（下一轮缓存
%                                  过期再试）
%   {error, Reason}                网络类错误原样透传——调用方可重试
%
% 隐私红线：全程不落请求/响应体日志；service account 私钥只在签名
% 瞬间解引用，不入任何返回值。
%%%

-include("log.hrl").
-include_lib("public_key/include/public_key.hrl").

-compile([nowarn_deprecated_catch]).

-export([get_access_token/0]).

%% 缓存键独立于旧静态 token 配置位（互不污染）
-define(CACHE_KEY, {fcm_access_token_dyn}).
%% 缓存 50 分钟（Google access_token 有效期 1h，同 APNs JWT 的 3000s 口径）
-define(CACHE_TTL, 3000).
-define(FIREBASE_MESSAGING_SCOPE,
    <<"https://www.googleapis.com/auth/firebase.messaging">>
).
-define(JWT_GRANT,
    <<"grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer">>
).

%% @doc 取 FCM access token：缓存命中直返；否则走完整 JWT-bearer 刷新。
-spec get_access_token() -> {ok, binary()} | {error, term()}.
get_access_token() ->
    case imboy_cache:get(?CACHE_KEY) of
        {ok, Token} when is_binary(Token) ->
            {ok, Token};
        _ ->
            refresh()
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec refresh() -> {ok, binary()} | {error, term()}.
refresh() ->
    case load_service_account() of
        {ok, #{client_email := Email, token_uri := TokenUri, private_key := Key}} ->
            case build_assertion(Email, TokenUri, Key) of
                {ok, Jwt} ->
                    request_token(TokenUri, Jwt);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% 读取 service account：内联 JSON 优先，其次文件路径
-spec load_service_account() ->
    {ok, #{client_email := binary(), token_uri := binary(), private_key := term()}}
    | {error, term()}.
load_service_account() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            InlineBin =
                case proplists:get_value(fcm_service_account_json, PushConfig) of
                    undefined -> undefined;
                    Inline -> {ok, elib_cnv:safe_to_binary(Inline)}
                end,
            JsonBin =
                case InlineBin of
                    {ok, B} ->
                        {ok, B};
                    undefined ->
                        case proplists:get_value(fcm_service_account_path, PushConfig) of
                            undefined ->
                                undefined;
                            Path ->
                                case file:read_file(elib_cnv:safe_to_binary(Path)) of
                                    {ok, B} -> {ok, B};
                                    {error, R} -> {error, {config_error, {read_failed, R}}}
                                end
                        end
                end,
            case JsonBin of
                undefined ->
                    {error, not_configured};
                {error, _} = Err ->
                    Err;
                {ok, Bin} ->
                    parse_service_account(Bin)
            end;
        undefined ->
            {error, not_configured}
    end.

-spec parse_service_account(binary()) ->
    {ok, #{client_email := binary(), token_uri := binary(), private_key := term()}}
    | {error, term()}.
parse_service_account(JsonBin) ->
    try
        #{<<"client_email">> := Email, <<"token_uri">> := TokenUri, <<"private_key">> := Pem} =
            jsone:decode(JsonBin),
        try
            [Entry] = public_key:pem_decode(elib_cnv:safe_to_binary(Pem)),
            Key = public_key:pem_entry_decode(Entry),
            Parsed =
                case Key of
                    %% PKCS#8（Google service account 标准形态）
                    #'PrivateKeyInfo'{} ->
                        public_key:der_decode('RSAPrivateKey', Key#'PrivateKeyInfo'.privateKey);
                    %% PKCS#1 兼容
                    _ ->
                        Key
                end,
            {ok, #{
                client_email => Email,
                token_uri => TokenUri,
                private_key => Parsed
            }}
        catch
            _:PemErr ->
                {error, {config_error, {private_key_parse_failed, PemErr}}}
        end
    catch
        _:{badmatch, _} ->
            {error, {config_error, missing_required_field}};
        _:JsonErr ->
            {error, {config_error, {json_decode_failed, JsonErr}}}
    end.

%% 构造 RS256 JWT assertion（exp = iat + 1h，Google 上限）
-spec build_assertion(binary(), binary(), term()) -> {ok, binary()} | {error, term()}.
build_assertion(Email, TokenUri, Key) ->
    Now = erlang:system_time(second),
    try
        Header = b64url(jsone:encode(#{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>})),
        Claims = b64url(
            jsone:encode(#{
                <<"iss">> => Email,
                <<"scope">> => ?FIREBASE_MESSAGING_SCOPE,
                <<"aud">> => TokenUri,
                <<"iat">> => Now,
                <<"exp">> => Now + 3600
            })
        ),
        SignInput = <<Header/binary, ".", Claims/binary>>,
        Sig = b64url(public_key:sign(SignInput, sha256, Key)),
        {ok, <<SignInput/binary, ".", Sig/binary>>}
    catch
        _:SignErr ->
            {error, {config_error, {sign_failed, SignErr}}}
    end.

%% POST token_uri（gun HTTP/2 TLS；连接不复用——刷新 1 次/50min，无必要）
-spec request_token(binary(), binary()) -> {ok, binary()} | {error, term()}.
request_token(TokenUri, Jwt) ->
    try
        #{host := Host0, path := Path0} = uri_string:parse(TokenUri),
        Host = unicode:characters_to_binary(Host0),
        Path =
            case unicode:characters_to_binary(Path0) of
                B when is_binary(B) -> B;
                {incomplete, B, _} -> B;
                _ -> <<"/">>
            end,
        Body = <<?JWT_GRANT/binary, "&assertion=", Jwt/binary>>,
        Headers = [
            {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
        ],
        HttpTimeout = config_ds:env(push_http_timeout, 15000),
        case
            gun:open(binary_to_list(Host), 443, #{
                transport => tls,
                protocols => [http2],
                tls_opts => [
                    {verify, verify_peer},
                    {customize_hostname_check, [
                        {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                    ]}
                ]
            })
        of
            {ok, ConnPid} ->
                case gun:await_up(ConnPid, 5000) of
                    {ok, http2} ->
                        StreamRef = gun:post(ConnPid, Path, Headers, Body),
                        Result =
                            case gun:await(ConnPid, StreamRef, HttpTimeout) of
                                {response, nofin, 200, _} ->
                                    {ok, RespBody} = gun:await_body(ConnPid, StreamRef, 8000),
                                    decode_token_response(RespBody);
                                {response, fin, 200, _} ->
                                    {error, {fcm_token_error, empty_body}};
                                {response, _, Status, _} ->
                                    {error, {fcm_token_error, Status}};
                                {error, Reason} ->
                                    {error, Reason}
                            end,
                        try
                            gun:close(ConnPid)
                        catch
                            _:_ -> ok
                        end,
                        cache_on_success(Result);
                    {error, AwaitReason} ->
                        try
                            gun:close(ConnPid)
                        catch
                            _:_ -> ok
                        end,
                        {error, AwaitReason}
                end;
            {error, OpenReason} ->
                {error, OpenReason}
        end
    catch
        _:Error ->
            {error, Error}
    end.

-spec decode_token_response(binary()) -> {ok, binary()} | {error, term()}.
decode_token_response(RespBody) ->
    try
        #{<<"access_token">> := Token} = jsone:decode(RespBody),
        {ok, elib_cnv:safe_to_binary(Token)}
    catch
        _:_ ->
            {error, {fcm_token_error, malformed_response}}
    end.

cache_on_success({ok, Token} = Ok) ->
    _ = imboy_cache:set(?CACHE_KEY, Token, ?CACHE_TTL),
    Ok;
cache_on_success(Err) ->
    Err.

%% @doc base64url 编码（无填充）
-spec b64url(binary()) -> binary().
b64url(Data) ->
    B64 = base64:encode(Data),
    <<
        <<
            (case C of
                $+ -> $-;
                $/ -> $_;
                _ -> C
            end)
        >>
     || <<C>> <= B64, C =/= $=
    >>.
