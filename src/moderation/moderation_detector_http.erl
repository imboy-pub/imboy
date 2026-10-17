%%% @doc TextGuard HTTP 薄适配：冻结 V1 schema，连接复用、总 deadline、请求内零重试。
-module(moderation_detector_http).

-behaviour(moderation_detector_transport).

-export([check/2, info/1]).
%% T16 发布面（管理端点；不属于 behaviour 检测契约）
-export([get_active_ruleset/1, publish_package/2]).

-define(DEFAULT_TIMEOUT_MS, 150).
-define(DEFAULT_MAX_RESPONSE_BYTES, 65536).
-define(CHECK_PATH, <<"/v1/check">>).
-define(INFO_PATH, <<"/v1/info">>).
-define(ACTIVE_RULESET_PATH, <<"/internal/v1/rulesets/active">>).

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
                        fun(ResponseBody) -> validate_check_body(ResponseBody, Detection) end,
                        #{request_id => maps:get(request_id, Detection)}
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
                fun validate_info_body/1,
                request_options(Ctx)
            )
        end
    ).

%% ===================================================================
%% T16 发布面（管理端点；复用检测面请求管线，fail-closed 同款）
%% ===================================================================

-spec get_active_ruleset(moderation_detector_transport:ctx()) -> {ok, map()} | {error, atom()}.
get_active_ruleset(Ctx) ->
    with_publish_config(fun(BaseUrl, Token, Conf) ->
        request(
            <<"GET">>,
            ?ACTIVE_RULESET_PATH,
            <<>>,
            BaseUrl,
            Token,
            deadline_ms(Ctx, Conf),
            Conf,
            fun(Body) -> validate_publication_body(Body, get) end,
            request_options(Ctx)
        )
    end).

-spec publish_package(binary(), map()) -> {ok, map()} | {error, atom()}.
publish_package(Package, #{if_match := ETag} = Opts) when is_binary(Package) ->
    case moderation_detector_transport:valid_digest(ETag) of
        true ->
            with_publish_config(fun(BaseUrl, Token, Conf) ->
                Ctx = maps:get(ctx, Opts, #{}),
                RequestOpts = request_options(Ctx),
                Confirm =
                    case maps:get(confirm_empty, Opts, false) of
                        true -> [{<<"x-textguard-confirm-empty">>, <<"true">>}];
                        _ -> []
                    end,
                Headers =
                    [{<<"if-match">>, ETag} | Confirm] ++ maps:get(extra_headers, RequestOpts),
                request(
                    <<"PUT">>,
                    ?ACTIVE_RULESET_PATH,
                    Package,
                    BaseUrl,
                    Token,
                    deadline_ms(Ctx, Conf),
                    Conf,
                    fun(Body) -> validate_publish_body(Body, Package) end,
                    RequestOpts#{extra_headers => Headers}
                )
            end);
        false ->
            {error, protocol_error}
    end;
publish_package(_, _) ->
    {error, protocol_error}.

request_options(Ctx) ->
    case maps:get(request_id, Ctx, undefined) of
        undefined ->
            #{extra_headers => []};
        RequestId ->
            #{
                request_id => RequestId,
                extra_headers => [{<<"x-textguard-request-id">>, RequestId}]
            }
    end.

%% 发布面配置：http_base_url + publish_token（独立于检测 token）。
-spec with_publish_config(fun((binary(), binary(), proplists:proplist()) -> R)) -> R.
with_publish_config(Fun) ->
    Conf = config_ds:env(moderation_detector, []),
    case
        {
            proplists:get_value(http_base_url, Conf),
            proplists:get_value(publish_token, Conf)
        }
    of
        {BaseUrl, Token} when
            is_binary(BaseUrl), BaseUrl =/= <<>>, is_binary(Token), Token =/= <<>>
        ->
            Fun(BaseUrl, Token, Conf);
        _ ->
            {error, not_ready}
    end.

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
    fun((binary()) -> R),
    map()
) -> R.
request(Method, Path, Body, BaseUrl, Token, Deadline, Conf, Validator, Opts) ->
    case parse_base_url(BaseUrl) of
        {ok, #{scheme := Scheme, host := Host, port := Port, base_path := BasePath}} ->
            T0 = erlang:monotonic_time(millisecond),
            case get_or_open_conn(Scheme, Host, Port, remaining(Deadline, T0)) of
                {ok, ConnPid} ->
                    Headers = request_headers(Method, Token, maps:get(extra_headers, Opts, [])),
                    FullPath = join_path(BasePath, Path),
                    StreamRef = gun:request(ConnPid, Method, FullPath, Headers, Body),
                    await_response(
                        ConnPid,
                        StreamRef,
                        Deadline,
                        T0,
                        Validator,
                        Conf,
                        Opts#{endpoint => {Method, Path}}
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_ready} = NotReady ->
            NotReady
    end.

-spec request_headers(binary(), binary(), [{binary(), binary()}]) -> [{binary(), binary()}].
request_headers(Method, Token, ExtraHeaders) ->
    Base =
        case has_body(Method) of
            true ->
                [
                    {<<"authorization">>, bearer(Token)},
                    {<<"content-type">>, <<"application/json">>},
                    {<<"accept">>, <<"application/json">>}
                ];
            false ->
                [
                    {<<"authorization">>, bearer(Token)},
                    {<<"accept">>, <<"application/json">>}
                ]
        end,
    Base ++ ExtraHeaders.

-spec has_body(binary()) -> boolean().
has_body(<<"POST">>) -> true;
has_body(<<"PUT">>) -> true;
has_body(_) -> false.

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
    proplists:proplist(),
    map()
) -> R.
await_response(ConnPid, StreamRef, Deadline, T0, Validator, Conf, Opts) ->
    case gun:await(ConnPid, StreamRef, remaining(Deadline, T0)) of
        {response, fin, Status, _RespHeaders} ->
            handle_response(Status, <<>>, Validator, Conf, Opts);
        {response, nofin, Status, _RespHeaders} ->
            case gun:await_body(ConnPid, StreamRef, remaining(Deadline, T0)) of
                {ok, Body} ->
                    handle_response(Status, Body, Validator, Conf, Opts);
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
    integer(), binary(), fun((binary()) -> R), proplists:proplist(), map()
) -> R.
handle_response(200, Body, Validator, Conf, _Opts) ->
    MaxBytes = max_response_bytes(Conf),
    case byte_size(Body) of
        N when N > MaxBytes ->
            %% 上限先于形状校验：超大响应无论内容一律拒绝
            {error, protocol_error};
        _ ->
            Validator(Body)
    end;
handle_response(Status, Body, _Validator, Conf, Opts) ->
    case byte_size(Body) =< max_response_bytes(Conf) of
        true -> validate_error_body(Status, Body, Opts);
        false -> {error, protocol_error}
    end.

validate_error_body(Status, Body, Opts) ->
    try
        {ok, #{<<"error">> := Error} = Envelope} = moderation_detector_transport:decode_json(Body),
        1 = map_size(Envelope),
        #{<<"code">> := Code, <<"request_id">> := RequestId, <<"retryable">> := Retryable} = Error,
        3 = map_size(Error),
        true = is_binary(RequestId) andalso byte_size(RequestId) =< 64,
        true = is_boolean(Retryable),
        ExpectedRequestId = maps:get(request_id, Opts, undefined),
        true =
            RequestId =:= <<>> orelse ExpectedRequestId =:= undefined orelse
                RequestId =:= ExpectedRequestId,
        true = lists:member({Status, Code}, http_error_pairs(maps:get(endpoint, Opts))),
        {error, moderation_detector_transport:error_code_to_reason(Code)}
    catch
        _:_ -> {error, protocol_error}
    end.

http_error_pairs(Endpoint) ->
    Common = [
        {401, <<"UNAUTHENTICATED">>},
        {403, <<"PERMISSION_DENIED">>},
        {500, <<"INTERNAL_ERROR">>},
        {502, <<"INTERNAL_ERROR">>}
    ],
    Common ++
        case Endpoint of
            {<<"POST">>, ?CHECK_PATH} ->
                [
                    {400, <<"INVALID_ARGUMENT">>},
                    {400, <<"INVALID_UTF8">>},
                    {500, <<"PROTOCOL_ERROR">>},
                    {502, <<"PROTOCOL_ERROR">>},
                    {413, <<"TEXT_TOO_LARGE">>},
                    {422, <<"NORMALIZATION_LIMIT">>},
                    {422, <<"MATCH_BUDGET_EXCEEDED">>},
                    {429, <<"OVERLOADED">>},
                    {503, <<"NOT_READY">>},
                    {504, <<"CHECK_DEADLINE">>},
                    {503, <<"CONNECTION_FAILED">>},
                    {500, <<"NORMALIZER_INCOMPATIBLE">>},
                    {502, <<"NORMALIZER_INCOMPATIBLE">>},
                    {503, <<"UNSUPPORTED_RULESET">>}
                ];
            {<<"GET">>, ?INFO_PATH} ->
                [];
            {<<"GET">>, ?ACTIVE_RULESET_PATH} ->
                [{500, <<"PUBLISH_STATE_UNKNOWN">>}];
            {<<"PUT">>, ?ACTIVE_RULESET_PATH} ->
                [
                    {400, <<"INVALID_ARGUMENT">>},
                    {400, <<"INVALID_UTF8">>},
                    {500, <<"PROTOCOL_ERROR">>},
                    {502, <<"PROTOCOL_ERROR">>},
                    {400, <<"INVALID_RULESET">>},
                    {503, <<"UNSUPPORTED_RULESET">>},
                    {413, <<"TEXT_TOO_LARGE">>},
                    {503, <<"NOT_READY">>},
                    {409, <<"PUBLISH_BUSY">>},
                    {409, <<"RETIRED_SNAPSHOT_BUSY">>},
                    {412, <<"CAS_CONFLICT">>},
                    {504, <<"PUBLISH_DEADLINE">>},
                    {500, <<"PUBLISH_STATE_UNKNOWN">>},
                    {500, <<"NORMALIZER_INCOMPATIBLE">>},
                    {502, <<"NORMALIZER_INCOMPATIBLE">>}
                ]
        end.

validate_check_body(Body, Detection) ->
    case moderation_detector_transport:decode_json(Body) of
        {ok, Response} -> moderation_detector_transport:validate_check(Response, Detection, http);
        Error -> Error
    end.

validate_info_body(Body) ->
    case moderation_detector_transport:decode_json(Body) of
        {ok, Response} -> moderation_detector_transport:validate_info(Response, http);
        Error -> Error
    end.

validate_publication_body(Body, Method) ->
    case moderation_detector_transport:decode_json(Body) of
        {ok, Response} -> moderation_detector_transport:validate_publication(Response, Method);
        Error -> Error
    end.

validate_publish_body(Body, Package) ->
    case validate_publication_body(Body, put) of
        {ok, #{ruleset_revision := Revision} = State} ->
            case binary:encode_hex(crypto:hash(sha256, Package), lowercase) of
                Revision -> {ok, State};
                _ -> {error, protocol_error}
            end;
        Error ->
            Error
    end.

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
