%%% @doc TextGuard 检测器 HTTP 适配（T15，AC-060/061 的 imboy 侧薄客户端）。
%%%
%%% 线上契约（与 Rust TextGuard 服务 /v1 面对齐，真闭合在 T19）：
%%%
%%%   POST {http_base_url}/v1/check
%%%     头：authorization: Bearer <token>、content-type/accept: application/json
%%%     体：{"request_id":"…","scene":"channel_message",
%%%          "fields":[{"field_id":"…","text":"…"}]}
%%%     200 体（**严格**校验：缺键/多余键/非 UTF-8/截断一律 protocol_error，
%%%     绝不折算成 allow）：
%%%          {"decision":"allow|block|review","scan_complete":true|false}
%%%
%%%   GET {http_base_url}/v1/info
%%%     200 体（严格）：{"version":"…","ready":true|false}
%%%
%%% 非 200 错误信封（可选）：
%%%   {"error":{"code":"OVERLOADED|TEXT_TOO_LARGE|UNAUTHENTICATED|
%%%                     PERMISSION_DENIED|NOT_READY","message":"…"}}
%%%   映射优先级：**可解析信封的已知 code > 信封未知 code（回落 internal）>
%%%   无信封按状态码表**。状态码表（无信封时）：
%%%     401→unauthenticated  403→permission_denied  413→text_too_large
%%%     429→overloaded       503→overloaded         504→timeout
%%%     其它→internal
%%%
%%% 总预算（AC-061）：单一 deadline 覆盖连接+请求+收体，逐步扣减剩余预算；
%%% 默认 150ms（配置 `timeout_ms` 或 Ctx `deadline_ms` 覆盖）。
%%% **失败立即上抛，零自动重试**——熔断/重试由调用方（T19 装配）负责。
%%%
%%% 传输与连接复用：`gun`（仓内既有出站 HTTP 惯例，同
%%% push_provider_jpush_http）。HTTP/1.1 keep-alive：连接按
%%% {scheme, host, port} 进程字典缓存复用（本仓既有惯例；适配器运行在
%%% 调用方进程内，进程结束连接随之释放，无请求级进程泄漏）；对端关闭
%%% 或出错即弃置缓存连接，下次重建。https 走 TLS verify_peer。
%%%
%%% 配置见 `moderation_detector_transport` 模块头；`http_base_url` 与
%%% `token` 缺失/非法 → `{error, not_ready}`（fail-closed）。
%%% 响应体超过 `max_response_bytes`（默认 65536）→ protocol_error，
%%% 且该上限在形状校验**之前**生效（超大的「形状合法」体同样拒绝）。
-module(moderation_detector_http).

-behaviour(moderation_detector_transport).

-export([check/2, info/1]).

-define(DEFAULT_TIMEOUT_MS, 150).
-define(DEFAULT_MAX_RESPONSE_BYTES, 65536).
-define(CHECK_PATH, <<"/v1/check">>).
-define(INFO_PATH, <<"/v1/info">>).

%% ===================================================================
%% behaviour 实现
%% ===================================================================

%% @doc 一次文本检测：POST /v1/check + 严格响应校验。
-spec check(
    moderation_detector_transport:detection(),
    moderation_detector_transport:ctx()
) ->
    {ok, moderation_detector_transport:check_result()}
    | {error, moderation_detector_transport:error_reason()}.
check(Detection, Ctx) ->
    with_config(
        fun(BaseUrl, Token, Conf) ->
            Deadline = deadline_ms(Ctx, Conf),
            case build_check_request(Detection) of
                {ok, Body} ->
                    request(
                        <<"POST">>,
                        ?CHECK_PATH,
                        Body,
                        BaseUrl,
                        Token,
                        Deadline,
                        Conf,
                        fun validate_check_body/1
                    );
                {error, Reason} ->
                    {error, Reason}
            end
        end
    ).

%% @doc 服务元信息：GET /v1/info + 严格响应校验。
-spec info(moderation_detector_transport:ctx()) ->
    {ok, moderation_detector_transport:info_result()}
    | {error, moderation_detector_transport:error_reason()}.
info(Ctx) ->
    with_config(
        fun(BaseUrl, Token, Conf) ->
            Deadline = deadline_ms(Ctx, Conf),
            request(
                <<"GET">>,
                ?INFO_PATH,
                <<>>,
                BaseUrl,
                Token,
                Deadline,
                Conf,
                fun validate_info_body/1
            )
        end
    ).

%% ===================================================================
%% 配置装载（fail-closed：缺配置 = not_ready）
%% ===================================================================

-spec with_config(fun((binary(), binary(), proplists:proplist()) -> R)) -> R.
with_config(Fun) ->
    Conf = config_ds:env(moderation_detector, []),
    case
        {
            proplists:get_value(http_base_url, Conf),
            proplists:get_value(token, Conf)
        }
    of
        {BaseUrl, Token} when
            is_binary(BaseUrl), BaseUrl =/= <<>>, is_binary(Token), Token =/= <<>>
        ->
            Fun(BaseUrl, Token, Conf);
        _ ->
            %% 缺失/非法配置不细分：一律 not_ready，绝不静默放行
            {error, not_ready}
    end.

%% 总 deadline：Ctx 显式值 > 配置 timeout_ms > 默认 150。
-spec deadline_ms(moderation_detector_transport:ctx(), proplists:proplist()) ->
    pos_integer().
deadline_ms(Ctx, Conf) ->
    Raw = ctx_deadline(Ctx, proplists:get_value(timeout_ms, Conf, ?DEFAULT_TIMEOUT_MS)),
    case Raw of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_TIMEOUT_MS
    end.

-spec ctx_deadline(moderation_detector_transport:ctx(), term()) -> term().
ctx_deadline(#{deadline_ms := D} = _Ctx, _ConfDefault) when is_integer(D), D > 0 ->
    D;
ctx_deadline(_Ctx, ConfDefault) ->
    ConfDefault.

%% ===================================================================
%% 请求构造（形状不符 = 本侧协议违约 → protocol_error）
%% ===================================================================

-spec build_check_request(moderation_detector_transport:detection()) ->
    {ok, binary()} | {error, protocol_error}.
build_check_request(Detection) when is_map(Detection) ->
    ReqId = maps:get(request_id, Detection, undefined),
    Scene = maps:get(scene, Detection, undefined),
    Fields = maps:get(fields, Detection, undefined),
    case valid_request_shape(ReqId, Scene, Fields) of
        ok ->
            {ok, SceneBin} = moderation_detector_transport:scene_to_binary(Scene),
            {ok, FieldMaps} = normalize_fields(Fields),
            {ok,
                jsone:encode(
                    #{
                        <<"request_id">> => ReqId,
                        <<"scene">> => SceneBin,
                        <<"fields">> => FieldMaps
                    },
                    [native_utf8]
                )};
        error ->
            {error, protocol_error}
    end;
build_check_request(_NotAMap) ->
    {error, protocol_error}.

-spec valid_request_shape(term(), term(), term()) -> ok | error.
valid_request_shape(ReqId, Scene, Fields) when
    is_binary(ReqId),
    is_atom(Scene),
    is_list(Fields)
->
    case moderation_detector_transport:scene_to_binary(Scene) of
        {ok, _} -> ok;
        error -> error
    end;
valid_request_shape(_, _, _) ->
    error.

%% {FieldId, Text} 全二进制；空表放行（裁决权在服务端）。
-spec normalize_fields([{binary(), binary()}]) -> {ok, [map()]} | error.
normalize_fields(Fields) ->
    normalize_fields(Fields, []).

normalize_fields([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_fields([{FieldId, Text} | Rest], Acc) when
    is_binary(FieldId), is_binary(Text)
->
    normalize_fields(Rest, [#{<<"field_id">> => FieldId, <<"text">> => Text} | Acc]);
normalize_fields(_, _Acc) ->
    error.

%% ===================================================================
%% 请求执行（单一总预算，逐步扣减；零重试）
%% ===================================================================

-spec request(
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    pos_integer(),
    proplists:proplist(),
    fun((binary()) -> R)
) -> R.
request(Method, Path, Body, BaseUrl, Token, Deadline, Conf, Validator) ->
    case parse_base_url(BaseUrl) of
        {ok, #{scheme := Scheme, host := Host, port := Port, base_path := BasePath}} ->
            T0 = erlang:monotonic_time(millisecond),
            case get_or_open_conn(Scheme, Host, Port, remaining(Deadline, T0)) of
                {ok, ConnPid} ->
                    Headers = request_headers(Method, Token),
                    FullPath = join_path(BasePath, Path),
                    StreamRef = gun:request(ConnPid, Method, FullPath, Headers, Body),
                    await_response(ConnPid, StreamRef, Deadline, T0, Validator, Conf);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_ready} = NotReady ->
            NotReady
    end.

-spec request_headers(binary(), binary()) -> [{binary(), binary()}].
request_headers(<<"POST">>, Token) ->
    [
        {<<"authorization">>, bearer(Token)},
        {<<"content-type">>, <<"application/json">>},
        {<<"accept">>, <<"application/json">>}
    ];
request_headers(_Method, Token) ->
    [
        {<<"authorization">>, bearer(Token)},
        {<<"accept">>, <<"application/json">>}
    ].

-spec bearer(binary()) -> binary().
bearer(Token) ->
    <<"Bearer ", Token/binary>>.

%% 剩余预算（毫秒）；耗尽按 1ms 交给 gun 使其立即超时。
-spec remaining(pos_integer(), integer()) -> pos_integer().
remaining(Deadline, T0) ->
    erlang:max(Deadline - (erlang:monotonic_time(millisecond) - T0), 1).

-spec await_response(
    pid(),
    gun:stream_ref(),
    pos_integer(),
    integer(),
    fun((binary()) -> R),
    proplists:proplist()
) -> R.
await_response(ConnPid, StreamRef, Deadline, T0, Validator, Conf) ->
    case gun:await(ConnPid, StreamRef, remaining(Deadline, T0)) of
        {response, fin, Status, _RespHeaders} ->
            handle_response(Status, <<>>, Validator, Conf);
        {response, nofin, Status, _RespHeaders} ->
            case gun:await_body(ConnPid, StreamRef, remaining(Deadline, T0)) of
                {ok, Body} ->
                    handle_response(Status, Body, Validator, Conf);
                {error, timeout} ->
                    drop_conn(ConnPid),
                    {error, timeout};
                {error, _Other} ->
                    drop_conn(ConnPid),
                    {error, connection_failed}
            end;
        {error, timeout} ->
            %% 挂死/慢对端：弃置连接，错误立即上抛（零重试）
            drop_conn(ConnPid),
            {error, timeout};
        {error, _Other} ->
            drop_conn(ConnPid),
            {error, connection_failed}
    end.

%% ===================================================================
%% 响应裁决（严格校验 + 错误闭集映射）
%% ===================================================================

-spec handle_response(
    integer(), binary(), fun((binary()) -> R), proplists:proplist()
) -> R.
handle_response(200, Body, Validator, Conf) ->
    MaxBytes = max_response_bytes(Conf),
    case byte_size(Body) of
        N when N > MaxBytes ->
            %% 上限先于形状校验：超大响应无论内容一律拒绝
            {error, protocol_error};
        _ ->
            Validator(Body)
    end;
handle_response(Status, Body, _Validator, Conf) ->
    %% 非 200：信封 code 优先，无信封走状态码表
    case envelope_error_code(Body, max_response_bytes(Conf)) of
        {ok, Code} -> {error, error_code_to_reason(Code)};
        envelope_absent -> {error, status_to_reason(Status)}
    end.

%% 错误信封 code 白名单 → 闭集原子；未知串回落 internal（绝不动态成原子）。
-spec error_code_to_reason(binary()) ->
    overloaded
    | text_too_large
    | unauthenticated
    | permission_denied
    | not_ready
    | internal.
error_code_to_reason(<<"OVERLOADED">>) -> overloaded;
error_code_to_reason(<<"TEXT_TOO_LARGE">>) -> text_too_large;
error_code_to_reason(<<"UNAUTHENTICATED">>) -> unauthenticated;
error_code_to_reason(<<"PERMISSION_DENIED">>) -> permission_denied;
error_code_to_reason(<<"NOT_READY">>) -> not_ready;
error_code_to_reason(_UnknownCode) -> internal.

%% 无信封时的状态码表。
-spec status_to_reason(integer()) ->
    unauthenticated
    | permission_denied
    | text_too_large
    | overloaded
    | timeout
    | internal.
status_to_reason(401) -> unauthenticated;
status_to_reason(403) -> permission_denied;
status_to_reason(413) -> text_too_large;
status_to_reason(429) -> overloaded;
status_to_reason(503) -> overloaded;
status_to_reason(504) -> timeout;
status_to_reason(_Other) -> internal.

%% 解析 {"error":{"code":"…"}}；形状不符/解码失败/超限 = 无信封。
-spec envelope_error_code(binary(), pos_integer()) -> {ok, binary()} | envelope_absent.
envelope_error_code(Body, MaxBytes) ->
    case byte_size(Body) =< MaxBytes of
        true ->
            try jsone:decode(Body) of
                #{<<"error">> := #{<<"code">> := Code}} when is_binary(Code) ->
                    {ok, Code};
                _ ->
                    envelope_absent
            catch
                _:_ -> envelope_absent
            end;
        false ->
            envelope_absent
    end.

%% 200/check 严格校验：键恰为 decision+scan_complete（缺键/多余键拒绝）。
%% 显式 UTF-8 门禁：jsone 不校验 UTF-8（原始字节直通），非 UTF-8 体必须
%% 在解码前拦截为 protocol_error（AC-060）。
-spec validate_check_body(binary()) ->
    {ok, moderation_detector_transport:check_result()}
    | {error, protocol_error}.
validate_check_body(Body) ->
    case is_utf8(Body) of
        false ->
            {error, protocol_error};
        true ->
            try jsone:decode(Body) of
                #{<<"decision">> := DecBin, <<"scan_complete">> := SC} = M when
                    is_binary(DecBin), is_boolean(SC)
                ->
                    case extra_keys(M, [<<"decision">>, <<"scan_complete">>]) of
                        [] -> check_decision(DecBin, SC);
                        _ -> {error, protocol_error}
                    end;
                _ ->
                    {error, protocol_error}
            catch
                _:_ -> {error, protocol_error}
            end
    end.

%% 白名单查表（单一真源在 behaviour 模块）；未知枚举 → protocol_error。
-spec check_decision(binary(), boolean()) ->
    {ok, moderation_detector_transport:check_result()}
    | {error, protocol_error}.
check_decision(DecBin, SC) ->
    case moderation_detector_transport:decision_to_atom(DecBin) of
        {ok, Decision} ->
            {ok, #{decision => Decision, scan_complete => SC}};
        error ->
            {error, protocol_error}
    end.

%% 200/info 严格校验：键恰为 version+ready（同款 UTF-8 门禁）。
-spec validate_info_body(binary()) ->
    {ok, moderation_detector_transport:info_result()}
    | {error, protocol_error}.
validate_info_body(Body) ->
    case is_utf8(Body) of
        false ->
            {error, protocol_error};
        true ->
            try jsone:decode(Body) of
                #{<<"version">> := Version, <<"ready">> := Ready} = M when
                    is_binary(Version), is_boolean(Ready)
                ->
                    case extra_keys(M, [<<"version">>, <<"ready">>]) of
                        [] -> {ok, #{version => Version, ready => Ready}};
                        _ -> {error, protocol_error}
                    end;
                _ ->
                    {error, protocol_error}
            catch
                _:_ -> {error, protocol_error}
            end
    end.

-spec extra_keys(map(), [binary()]) -> [binary()].
extra_keys(M, Allowed) ->
    [K || {K, _} <- maps:to_list(M), not lists:member(K, Allowed)].

%% UTF-8 门禁（guard BIF 形态）：非 UTF-8 二进制 → false。
-spec is_utf8(binary()) -> boolean().
is_utf8(Bin) when is_binary(Bin) ->
    case unicode:characters_to_list(Bin, utf8) of
        Chars when is_list(Chars) ->
            true;
        {error, _, _} ->
            false;
        {incomplete, _, _} ->
            false
    end;
is_utf8(_) ->
    false.

-spec max_response_bytes(proplists:proplist()) -> pos_integer().
max_response_bytes(Conf) ->
    case proplists:get_value(max_response_bytes, Conf, ?DEFAULT_MAX_RESPONSE_BYTES) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_MAX_RESPONSE_BYTES
    end.

%% ===================================================================
%% 连接管理（gun；按 {scheme, host, port} 进程字典缓存，本仓惯例）
%% ===================================================================

-spec parse_base_url(binary()) ->
    {ok, #{
        scheme := binary(),
        host := binary(),
        port := inet:port_number(),
        base_path := binary()
    }}
    | {error, not_ready}.
parse_base_url(BaseUrl) ->
    try uri_string:parse(BaseUrl) of
        #{host := Host0, scheme := Scheme0} = Parsed ->
            Scheme = unicode_to_binary(Scheme0),
            Host = unicode_to_binary(Host0),
            case is_http_scheme(Scheme) andalso Host =/= <<>> of
                true ->
                    Port = maps:get(port, Parsed, default_port(Scheme)),
                    BasePath = unicode_to_binary(maps:get(path, Parsed, <<>>)),
                    {ok, #{
                        scheme => Scheme,
                        host => Host,
                        port => Port,
                        base_path => BasePath
                    }};
                false ->
                    {error, not_ready}
            end;
        _ ->
            {error, not_ready}
    catch
        _:_ -> {error, not_ready}
    end.

-spec is_http_scheme(binary()) -> boolean().
is_http_scheme(<<"http">>) -> true;
is_http_scheme(<<"https">>) -> true;
is_http_scheme(_) -> false.

-spec default_port(binary()) -> inet:port_number().
default_port(<<"https">>) -> 443;
default_port(<<"http">>) -> 80.

-spec unicode_to_binary(binary() | string()) -> binary().
unicode_to_binary(B) when is_binary(B) -> B;
unicode_to_binary(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end;
unicode_to_binary(_) ->
    <<>>.

-spec join_path(binary(), binary()) -> binary().
join_path(<<>>, Path) ->
    Path;
join_path(BasePath, Path) when is_binary(BasePath), is_binary(Path), Path =/= <<>> ->
    case binary:last(BasePath) of
        $/ -> <<BasePath/binary, (binary:part(Path, 1, byte_size(Path) - 1))/binary>>;
        _ -> <<BasePath/binary, Path/binary>>
    end.

-spec get_or_open_conn(binary(), binary(), inet:port_number(), pos_integer()) ->
    {ok, pid()} | {error, timeout | connection_failed}.
get_or_open_conn(Scheme, Host, Port, BudgetMs) ->
    Key = conn_key(Scheme, Host, Port),
    case get(Key) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    erase(Key),
                    open_and_cache_conn(Key, Scheme, Host, Port, BudgetMs)
            end;
        _ ->
            open_and_cache_conn(Key, Scheme, Host, Port, BudgetMs)
    end.

-spec open_and_cache_conn(term(), binary(), binary(), inet:port_number(), pos_integer()) ->
    {ok, pid()} | {error, timeout | connection_failed}.
open_and_cache_conn(Key, Scheme, Host, Port, BudgetMs) ->
    case gun:open(binary_to_list(Host), Port, gun_opts(Scheme)) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, BudgetMs) of
                {ok, _Protocol} ->
                    put(Key, ConnPid),
                    {ok, ConnPid};
                {error, timeout} ->
                    drop_conn(ConnPid),
                    {error, timeout};
                {error, _Other} ->
                    drop_conn(ConnPid),
                    {error, connection_failed}
            end;
        {error, _OpenReason} ->
            {error, connection_failed}
    end.

-spec gun_opts(binary()) -> map().
gun_opts(<<"https">>) ->
    #{
        transport => tls,
        %% retry=1：连接只尝试一次——总预算语义下绝不自动重连/重试
        retry => 1,
        protocols => [http2, http],
        tls_opts => [
            {verify, verify_peer},
            {customize_hostname_check, [
                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
            ]}
        ]
    };
gun_opts(<<"http">>) ->
    #{transport => tcp, retry => 1, protocols => [http]}.

%% 弃置缓存连接（对端关闭/超时/错误后由下次调用重建）。
-spec drop_conn(pid()) -> ok.
drop_conn(ConnPid) when is_pid(ConnPid) ->
    try
        gun:close(ConnPid)
    catch
        _:_ -> ok
    end,
    ok.

-spec conn_key(binary(), binary(), inet:port_number()) -> term().
conn_key(Scheme, Host, Port) ->
    {gun_conn, ?MODULE, Scheme, Host, Port}.
