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
%%%
%%% == 管理面：规则包发布（T16 扩展，真闭合在 T19）==
%%%
%%% 在同一适配器上追加了 TextGuard **发布**端点（管理面，与检测面
%%% `/v1/*` 分离；线上契约与 Rust 侧 `/internal` 面对齐，T19 闭合）：
%%%
%%%   GET {http_base_url}/internal/v1/rulesets/active
%%%     头：authorization: Bearer <publish_token>
%%%     200 体（严格键集 revision/generation/digest）：
%%%          {"revision":"…","generation":N,"digest":"<包字节 sha256 hex>"}
%%%     404 → `{error, not_found}`（当前无活跃规则包）
%%%
%%%   PUT {http_base_url}/internal/v1/rulesets/active
%%%     头：authorization: Bearer <publish_token>、content-type: application/json
%%%         、可选 if-match: <revision>（无活跃包时省略）
%%%     体：**逐字节精确**的规则包（调用方产出的确定性序列化字节，
%%%         适配器不重编码/不改写字节）
%%%     200 体（严格键集）：{"revision":"…","generation":N}
%%%     409 → `{error, cas_conflict}`（If-Match 的 revision 已被他人覆盖）
%%%
%%% 错误闭集在 T15 检测面闭集之上，发布面额外两个固定原子：
%%% `cas_conflict | not_found`（同样绝不动态成原子）。发布令牌独立于检测
%%% 令牌：配置键 `{publish_token, binary()}`（占位符部署，检测 `token`
%%% 与发布 `publish_token` 分开轮换）。发布缺 `publish_token` →
%%% `{error, not_ready}`（fail-closed）。复用检测面的总预算/连接复用/
%%% 响应上限机制；`textguard_publisher` 通过 Ctx `deadline_ms` 给发布
%%% 更大的预算（检测 150ms 不适合整包上传）。
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
%% T16 发布面（管理端点；复用检测面请求管线，fail-closed 同款）
%% ===================================================================

%% @doc 读当前活跃规则包元信息（CAS 前置 GET）：无活跃包 → not_found。
-spec get_active_ruleset(moderation_detector_transport:ctx()) ->
    {ok, #{revision := binary(), generation := integer(), digest := binary()}}
    | {error, not_found | moderation_detector_transport:error_reason()}.
get_active_ruleset(Ctx) ->
    with_publish_config(
        fun(BaseUrl, Token, Conf) ->
            Deadline = deadline_ms(Ctx, Conf),
            request(
                <<"GET">>,
                ?ACTIVE_RULESET_PATH,
                <<>>,
                BaseUrl,
                Token,
                Deadline,
                Conf,
                fun validate_active_ruleset_body/1,
                #{non200 => #{404 => not_found}}
            )
        end
    ).

%% @doc 发布规则包（PUT，体 = 调用方给出的精确包字节，不重编码）。
%%%
%%% Opts：
%%%   `if_match` → 二进制 revision（CAS 前置 GET 的结果）或 `none`
%%%   （无活跃包时省略 If-Match 头）；缺省 none；
%%%   `ctx` → 检测面同款 Ctx（`deadline_ms` 给发布更大预算）。
%%%
%%% 200 → `{ok, #{revision, generation}}`；409 → `{error, cas_conflict}`；
%%% 其余错误与检测面闭集一致（信封 code 优先，CAS_CONFLICT 信封同样映射）。
-spec publish_package(binary(), map()) ->
    {ok, #{revision := binary(), generation := integer()}}
    | {error, cas_conflict | moderation_detector_transport:error_reason()}.
publish_package(Package, Opts) when is_binary(Package), is_map(Opts) ->
    with_publish_config(
        fun(BaseUrl, Token, Conf) ->
            Ctx = maps:get(ctx, Opts, #{}),
            Deadline = deadline_ms(Ctx, Conf),
            Extra =
                case maps:get(if_match, Opts, none) of
                    Rev when is_binary(Rev), Rev =/= <<>> ->
                        [{<<"if-match">>, Rev}];
                    _ ->
                        []
                end,
            request(
                <<"PUT">>,
                ?ACTIVE_RULESET_PATH,
                Package,
                BaseUrl,
                Token,
                Deadline,
                Conf,
                fun validate_publish_body/1,
                #{extra_headers => Extra, non200 => #{409 => cas_conflict}}
            )
        end
    ).

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
    fun((binary()) -> R)
) -> R.
request(Method, Path, Body, BaseUrl, Token, Deadline, Conf, Validator) ->
    request(Method, Path, Body, BaseUrl, Token, Deadline, Conf, Validator, #{}).

%% T16 泛化形态：Opts 支持
%%   `extra_headers` → 追加请求头（发布面 if-match）；
%%   `non200` → 非 200 状态码直映射表（发布面 404→not_found、409→cas_conflict，
%%     仅在无信封/信封未命中时兜底，不影响检测面行为）。
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
                    await_response(ConnPid, StreamRef, Deadline, T0, Validator, Conf, Opts);
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
    %% 非 200：信封 code 优先；信封未命中走 non200 直映射（发布面）；
    %% 最后回落状态码表（检测面语义不变）
    case envelope_error_code(Body, max_response_bytes(Conf)) of
        {ok, Code} ->
            {error, error_code_to_reason(Code)};
        envelope_absent ->
            Non200 = maps:get(non200, Opts, #{}),
            case maps:find(Status, Non200) of
                {ok, Reason} -> {error, Reason};
                error -> {error, status_to_reason(Status)}
            end
    end.

%% 错误信封 code 白名单 → 闭集原子；未知串回落 internal（绝不动态成原子）。
-spec error_code_to_reason(binary()) ->
    overloaded
    | text_too_large
    | unauthenticated
    | permission_denied
    | not_ready
    | cas_conflict
    | not_found
    | internal.
error_code_to_reason(<<"OVERLOADED">>) -> overloaded;
error_code_to_reason(<<"TEXT_TOO_LARGE">>) -> text_too_large;
error_code_to_reason(<<"UNAUTHENTICATED">>) -> unauthenticated;
error_code_to_reason(<<"PERMISSION_DENIED">>) -> permission_denied;
error_code_to_reason(<<"NOT_READY">>) -> not_ready;
error_code_to_reason(<<"CAS_CONFLICT">>) -> cas_conflict;
error_code_to_reason(<<"NOT_FOUND">>) -> not_found;
error_code_to_reason(_UnknownCode) -> internal.

%% 无信封时的状态码表（检测面语义原样；发布面特例走 non200 直映射）。
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

%% T16：GET active 200 严格校验：键恰为 revision+generation+digest
%%（generation 整数；revision/digest 非空二进制；UTF-8 门禁同款）。
-spec validate_active_ruleset_body(binary()) ->
    {ok, #{revision := binary(), generation := integer(), digest := binary()}}
    | {error, protocol_error}.
validate_active_ruleset_body(Body) ->
    case strict_decode(Body, [<<"revision">>, <<"generation">>, <<"digest">>]) of
        {ok, #{<<"revision">> := Rev, <<"generation">> := Gen, <<"digest">> := Digest}} when
            is_binary(Rev), Rev =/= <<>>, is_integer(Gen), is_binary(Digest), Digest =/= <<>>
        ->
            {ok, #{revision => Rev, generation => Gen, digest => Digest}};
        _ ->
            {error, protocol_error}
    end.

%% T16：PUT publish 200 严格校验：键恰为 revision+generation。
-spec validate_publish_body(binary()) ->
    {ok, #{revision := binary(), generation := integer()}}
    | {error, protocol_error}.
validate_publish_body(Body) ->
    case strict_decode(Body, [<<"revision">>, <<"generation">>]) of
        {ok, #{<<"revision">> := Rev, <<"generation">> := Gen}} when
            is_binary(Rev), Rev =/= <<>>, is_integer(Gen)
        ->
            {ok, #{revision => Rev, generation => Gen}};
        _ ->
            {error, protocol_error}
    end.

%% 严格解码共享：UTF-8 门禁 + 精确键集（缺键/多余键 = protocol_error）。
-spec strict_decode(binary(), [binary()]) -> {ok, map()} | {error, protocol_error}.
strict_decode(Body, Allowed) ->
    case is_utf8(Body) of
        false ->
            {error, protocol_error};
        true ->
            try jsone:decode(Body) of
                M when is_map(M) ->
                    case extra_keys(M, Allowed) of
                        [] -> {ok, M};
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
