%%% @doc 内容安全检测器（TextGuard）传输适配 behaviour（T15）。
%%%
%%% imboy 侧对 Rust TextGuard 服务的**薄传输客户端**契约：
%%%
%%%   * `check/2` —— 一次文本检测（归一化请求 → 远端裁决）；
%%%   * `info/1`  —— 服务元信息/就绪探测（GET /v1/info、GetInfo）。
%%%
%%% 实现模块：
%%%   * `moderation_detector_http` —— HTTP 适配（POST /v1/check、GET /v1/info）；
%%%   * `moderation_detector_grpc` —— gRPC 适配（CheckText/GetInfo，传输函数
%%%     经配置注入，见该模块头注释的装配说明）；
%%%   * 本模块自带 `legacy` 实现（`legacy_check/2` / `legacy_info/1`）：
%%%     返回 `{ok, legacy}` 标记——**不发起任何远程调用**，调用方据此走
%%%     既有敏感词链路，绝不把 legacy 误当 allow/block 裁决。
%%%
%%% 选择器 `select/0` 读取 `{imboy, moderation_detector}` 配置的
%%% `{transport, legacy | http | grpc}`；缺省/未知值一律回落 legacy
%%% （未配置 = 不出网）。
%%%
%%% 错误理由由 error_code_to_reason/1 固定白名单映射，不创建远程输入原子。
%%%
%%% 总预算（AC-061）：每次调用强制一个「调用方提供或配置」的总 deadline
%%% （默认 150ms，连接+请求共用），失败立即上抛——**不做任何自动重试**；
%%% 熔断/重试策略由调用方配置（T19 装配），本层不「好心」重试。
%%%
%%% 配置（application env `{imboy, moderation_detector}`，proplists）：
%%%
%%%   `{transport, legacy | http | grpc}`        传输选择，默认 legacy
%%%   `{timeout_ms, pos_integer()}`              总 deadline 默认 150ms
%%%   `{token, binary()}`                        Bearer 令牌（占位符部署）
%%%   `{http_base_url, binary()}`                如 <<"http://127.0.0.1:7463">>
%%%   `{grpc_host, binary()}` / `{grpc_port, ...}` gRPC 端点
%%%   `{grpc_transport, fun/4 | {M, F}}`         gRPC 传输函数注入（T19）
%%%   `{max_response_bytes, pos_integer()}`      响应体上限，默认 65536
%%%
%%% 配置缺失时适配器返回 `{error, not_ready}`（fail-closed，绝不静默放行）。
-module(moderation_detector_transport).

%% 选择器 + legacy/noop 实现（本模块即 legacy 实现）。
-export([select/0, legacy_check/2, legacy_info/1]).
%% 适配器共享的白名单查表（单一真源，两个适配模块复用）。
-export([scene_to_binary/1, decision_to_atom/1]).
-export([
    decode_json/1,
    validate_check/3,
    validate_info/2,
    validate_publication/2,
    error_code_to_reason/1,
    valid_request_id/1,
    valid_digest/1
]).

-export_type([
    scene/0,
    decision/0,
    detection/0,
    ctx/0,
    check_result/0,
    info_result/0,
    error_reason/0
]).

%% ===================================================================
%% 类型（T15 冻结；T19 装配两侧共用）
%% ===================================================================

%% 检测场景：群/频道消息、时刻帖子、资料字段。
-type scene() :: channel_message | moment_post | profile_field.

%% 远端裁决值（HTTP/gRPC 白名单查表映射后的原子）。
%% review = 送人工复审队列（对齐既有 moderation review 域）。
-type decision() :: allow | block | review.

%% 归一化检测请求：fields 为 {FieldId, Text} 显式文本对。
-type detection() :: #{
    request_id := binary(),
    scene := scene(),
    fields := [{FieldId :: binary(), Text :: binary()}]
}.

%% 调用上下文：deadline_ms 覆盖配置的总预算（毫秒）。
-type ctx() :: #{deadline_ms => pos_integer(), request_id => binary()}.

%% 检测结果：scan_complete 缺失的远端响应一律 protocol_error（AC-060）。
-type check_result() :: #{
    decision := decision(),
    scan_complete := true,
    request_id := binary(),
    ruleset_revision := binary(),
    generation := binary(),
    hits := [map()],
    hits_truncated := boolean(),
    fields_checked := pos_integer(),
    normalization_id := binary(),
    t2s_table_id := binary(),
    semantics_id := binary(),
    base_rules_digest := binary(),
    hit_semantics := binary(),
    build_id := binary()
}.

%% 服务元信息。
-type info_result() :: #{
    instance_id := binary(),
    build_id := binary(),
    ready := boolean(),
    supported_semantics := [binary()],
    atom() => term()
}.

%% 错误闭集（见模块头）：固定原子，无二进制成员。
-type error_reason() ::
    overloaded
    | text_too_large
    | unauthenticated
    | permission_denied
    | not_ready
    | protocol_error
    | timeout
    | connection_failed
    | invalid_argument
    | invalid_utf8
    | normalization_limit
    | match_budget_exceeded
    | normalizer_incompatible
    | unsupported_ruleset
    | invalid_ruleset
    | cas_conflict
    | publish_deadline
    | publish_state_unknown
    | publish_busy
    | retired_snapshot_busy
    | internal.

%% ===================================================================
%% behaviour 回调
%% ===================================================================

-callback check(Detection :: detection(), Ctx :: ctx()) ->
    {ok, check_result() | legacy} | {error, error_reason()}.

-callback info(Ctx :: ctx()) ->
    {ok, info_result() | legacy} | {error, error_reason()}.

%% ===================================================================
%% 选择器
%% ===================================================================

%% @doc 按 `{transport, ...}` 配置选择实现模块。
%% 缺省/未知值回落本模块（legacy，零远程调用）。
-spec select() -> module().
select() ->
    Conf = config_ds:env(moderation_detector, []),
    case proplists:get_value(transport, Conf, legacy) of
        http -> moderation_detector_http;
        grpc -> moderation_detector_grpc;
        _ -> ?MODULE
    end.

%% ===================================================================
%% legacy / noop 实现：返回标记，绝不出网
%% ===================================================================

%% @doc legacy 标记：调用方走既有敏感词链路，不得当 TextGuard 裁决用。
-spec legacy_check(detection(), ctx()) -> {ok, legacy}.
legacy_check(_Detection, _Ctx) ->
    {ok, legacy}.

%% @doc legacy 标记（info 面同理，不出网）。
-spec legacy_info(ctx()) -> {ok, legacy}.
legacy_info(_Ctx) ->
    {ok, legacy}.

%% ===================================================================
%% 适配器共享白名单（闭集查表；远端二进制永不动态成原子）
%% ===================================================================

%% @doc 场景原子 → 线上枚举串（白名单，未知场景 error）。
-spec scene_to_binary(scene()) -> {ok, binary()} | error.
scene_to_binary(channel_message) -> {ok, <<"channel_message">>};
scene_to_binary(moment_post) -> {ok, <<"moment_post">>};
scene_to_binary(profile_field) -> {ok, <<"profile_field">>};
scene_to_binary(_) -> error.

%% @doc 裁决串 → 原子（白名单，未知值 error——绝不 allow 兜底）。
-spec decision_to_atom(binary()) -> {ok, decision()} | error.
decision_to_atom(<<"allow">>) -> {ok, allow};
decision_to_atom(<<"block">>) -> {ok, block};
decision_to_atom(<<"review">>) -> {ok, review};
decision_to_atom(_) -> error.

decode_json(Body) ->
    try
        true = is_binary(Body),
        true = is_list(unicode:characters_to_list(Body, utf8)),
        Map = json_value(jsone:decode(Body, [{object_format, tuple}])),
        true = is_map(Map),
        {ok, Map}
    catch
        _:_ -> {error, protocol_error}
    end.

json_value({Pairs}) ->
    true = length(Pairs) =:= map_size(maps:from_list(Pairs)),
    maps:from_list([{Key, json_value(Value)} || {Key, Value} <- Pairs]);
json_value(Values) when is_list(Values) -> [json_value(Value) || Value <- Values];
json_value(Value) ->
    Value.

validate_check(Response, #{request_id := RequestId, fields := Fields}, Protocol) ->
    try
        Keys = [
            request_id,
            ruleset_revision,
            generation,
            normalization_id,
            t2s_table_id,
            semantics_id,
            base_rules_digest,
            decision,
            scan_complete,
            hits_truncated,
            hits,
            fields_checked,
            hit_semantics,
            build_id
        ],
        Result = response_fields(Response, Keys, Protocol),
        RequestId = maps:get(request_id, Result),
        true = valid_request_id(RequestId),
        true = valid_digest(maps:get(ruleset_revision, Result)),
        true = valid_generation(maps:get(generation, Result)),
        true = valid_digest(maps:get(base_rules_digest, Result)),
        ok = validate_versions(Result),
        true = valid_string(maps:get(build_id, Result), 1, 64),
        <<"literal-all-regex-first-v1">> = maps:get(hit_semantics, Result),
        true = maps:get(scan_complete, Result),
        Truncated = maps:get(hits_truncated, Result),
        true = is_boolean(Truncated),
        Count = maps:get(fields_checked, Result),
        true = is_integer(Count) andalso Count >= 1 andalso Count =< 8,
        true = Count =:= length(Fields),
        Decision = enum_value(maps:get(decision, Result), decision, Protocol),
        Hits = [validate_hit(Hit, Fields, Protocol) || Hit <- maps:get(hits, Result, [])],
        true = length(Hits) =< 100,
        true = not Truncated orelse length(Hits) =:= 100,
        true = Protocol =:= grpc orelse maps:is_key(hits, Result),
        ok = validate_hit_order(Hits, Fields),
        MaxAction = lists:foldl(
            fun(Hit, Acc) ->
                max(Acc, action_rank(maps:get(effective_action, Hit)))
            end,
            0,
            Hits
        ),
        true = action_rank(Decision) =:= MaxAction,
        {ok, DecisionAtom} = decision_to_atom(Decision),
        {ok, Result#{decision => DecisionAtom, hits => Hits}}
    catch
        _:_ -> {error, protocol_error}
    end;
validate_check(_, _, _) ->
    {error, protocol_error}.

validate_hit(Response, Fields, Protocol) ->
    Keys = [
        field_id,
        rule_id,
        variant_id,
        variant_type,
        word,
        category,
        severity,
        effective_action,
        start_byte,
        end_byte,
        match_methods
    ],
    Hit = response_fields(Response, Keys, Protocol),
    true = lists:all(
        fun(Key) -> valid_string(maps:get(Key, Hit), 1, 64) end,
        [field_id, rule_id, variant_id, word, category]
    ),
    {_, Text} = lists:keyfind(maps:get(field_id, Hit), 1, Fields),
    Start = maps:get(start_byte, Hit),
    End = maps:get(end_byte, Hit),
    true = is_integer(Start) andalso is_integer(End),
    true = Start >= 0 andalso End > Start andalso End =< min(byte_size(Text), 16384),
    true = is_list(unicode:characters_to_list(binary:part(Text, 0, Start), utf8)),
    true = is_list(unicode:characters_to_list(binary:part(Text, Start, End - Start), utf8)),
    Variant = enum_value(maps:get(variant_type, Hit), variant_type, Protocol),
    Severity = enum_value(maps:get(severity, Hit), severity, Protocol),
    Action = enum_value(maps:get(effective_action, Hit), decision, Protocol),
    true = lists:member(Action, [<<"review">>, <<"block">>]),
    Methods = [
        enum_value(Method, match_method, Protocol)
     || Method <- maps:get(match_methods, Hit)
    ],
    true = length(Methods) >= 1 andalso length(Methods) =< 3,
    true = length(Methods) =:= length(lists:usort(Methods)),
    ok = validate_variant(Variant, maps:get(variant_id, Hit), Methods),
    Hit#{
        variant_type => Variant,
        severity => Severity,
        effective_action => Action,
        match_methods => Methods
    }.

validate_hit_order(Hits, Fields) ->
    FieldOrder = maps:from_list(lists:zip([Id || {Id, _} <- Fields], lists:seq(1, length(Fields)))),
    SortKeys = [
        {
            -action_rank(maps:get(effective_action, Hit)),
            maps:get(maps:get(field_id, Hit), FieldOrder),
            maps:get(start_byte, Hit),
            -maps:get(end_byte, Hit),
            maps:get(rule_id, Hit),
            maps:get(variant_id, Hit),
            maps:get(match_methods, Hit)
        }
     || Hit <- Hits
    ],
    SortKeys = lists:sort(SortKeys),
    Identities = [
        {
            maps:get(field_id, Hit),
            maps:get(rule_id, Hit),
            maps:get(variant_id, Hit),
            maps:get(start_byte, Hit),
            maps:get(end_byte, Hit)
        }
     || Hit <- Hits
    ],
    true = length(Identities) =:= length(lists:usort(Identities)),
    ok.

validate_variant(<<"canonical">>, <<"canonical">>, Methods) ->
    true = lists:member(Methods, [
        [<<"literal">>],
        [<<"literal">>, <<"t2s">>],
        [<<"literal">>, <<"gap">>],
        [<<"literal">>, <<"t2s">>, <<"gap">>]
    ]),
    ok;
validate_variant(<<"regex_rule">>, <<"regex">>, [<<"regex">>]) ->
    ok;
validate_variant(Variant, VariantId, Methods) ->
    true = lists:member(Variant, [<<"pinyin">>, <<"homophone">>, <<"lookalike">>, <<"decomposed">>]),
    true = VariantId =/= <<"canonical">> andalso VariantId =/= <<"regex">>,
    true = lists:member(Methods, [[<<"alias">>], [<<"t2s">>, <<"alias">>]]),
    ok.

validate_info(Response, Protocol) ->
    try
        Keys = [
            instance_id,
            ready,
            build_id,
            ruleset_revision,
            generation,
            normalization_id,
            t2s_table_id,
            base_rules_digest,
            canonical_count,
            alias_count,
            regex_count,
            supported_semantics
        ],
        Info = response_fields(Response, Keys, Protocol),
        true = valid_string(maps:get(instance_id, Info), 1, 64),
        true = valid_string(maps:get(build_id, Info), 1, 64),
        Ready = maps:get(ready, Info),
        true = is_boolean(Ready),
        Semantics = maps:get(supported_semantics, Info),
        true = is_list(Semantics) andalso Semantics =/= [],
        true = length(Semantics) =:= length(lists:usort(Semantics)),
        true = lists:all(fun valid_semantics/1, Semantics),
        SnapshotKeys = [
            ruleset_revision,
            generation,
            normalization_id,
            t2s_table_id,
            base_rules_digest,
            canonical_count,
            alias_count,
            regex_count
        ],
        true = not Ready orelse lists:all(fun(Key) -> maps:is_key(Key, Info) end, SnapshotKeys),
        ok = validate_info_snapshot(Info),
        {ok, Info}
    catch
        _:_ -> {error, protocol_error}
    end.

validate_info_snapshot(Info) ->
    Checks =
        [
            {ruleset_revision, fun valid_digest/1},
            {generation, fun valid_generation/1},
            {base_rules_digest, fun valid_digest/1},
            {normalization_id, fun(Value) -> Value =:= <<"imboy-base-v1">> end},
            {t2s_table_id, fun(Value) -> valid_string(Value, 1, 64) end}
        ] ++
            [
                {Key, fun(Value) -> is_integer(Value) andalso Value >= 0 andalso Value =< Max end}
             || {Key, Max} <- [{canonical_count, 50000}, {alias_count, 100000}, {regex_count, 32}]
            ],
    lists:foreach(
        fun({Key, Check}) ->
            case maps:find(Key, Info) of
                {ok, Value} -> true = Check(Value);
                error -> ok
            end
        end,
        Checks
    ),
    ok.

validate_versions(Result) ->
    <<"imboy-base-v1">> = maps:get(normalization_id, Result),
    true = valid_string(maps:get(t2s_table_id, Result), 1, 64),
    true = valid_semantics(maps:get(semantics_id, Result)),
    ok.

valid_semantics(Value) -> lists:member(Value, [<<"compat-v1">>, <<"enhanced-v1">>]).

validate_publication(Response, Method) ->
    try
        State = response_fields(
            Response,
            [
                instance_id,
                ready,
                generation,
                ruleset_revision,
                state_etag,
                status,
                scene_counts
            ],
            http
        ),
        Instance = maps:get(instance_id, State),
        true = valid_string(Instance, 1, 64),
        Generation = maps:get(generation, State),
        true = valid_generation(Generation),
        Revision = maps:get(ruleset_revision, State),
        ETag = maps:get(state_etag, State),
        true = valid_digest(ETag),
        ETag = binary:encode_hex(
            crypto:hash(
                sha256,
                <<Instance/binary, "\n", Generation/binary, "\n", Revision/binary>>
            ),
            lowercase
        ),
        Counts = response_fields(
            maps:get(scene_counts, State),
            [channel_message, moment_post, profile_field],
            http
        ),
        true = map_size(Counts) =:= 3,
        true = lists:all(
            fun(Value) -> is_integer(Value) andalso Value >= 0 end, maps:values(Counts)
        ),
        Status = maps:get(status, State),
        case {Method, Status, maps:get(ready, State), Generation, Revision} of
            {get, <<"uninitialized">>, false, <<"0">>, <<>>} ->
                true = lists:all(fun(Value) -> Value =:= 0 end, maps:values(Counts));
            {get, <<"active">>, true, _, _} ->
                true = valid_digest(Revision) andalso Generation =/= <<"0">>;
            {put, PublishStatus, true, _, _} when
                PublishStatus =:= <<"committed">>; PublishStatus =:= <<"already_active">>
            ->
                true = valid_digest(Revision) andalso Generation =/= <<"0">>
        end,
        {ok, State#{scene_counts => Counts}}
    catch
        _:_ -> {error, protocol_error}
    end.

response_fields(Response, Keys, http) when is_map(Response) ->
    Pairs = [
        {Key, maps:get(atom_to_binary(Key, utf8), Response)}
     || Key <- Keys,
        maps:is_key(atom_to_binary(Key, utf8), Response)
    ],
    true = length(Pairs) =:= map_size(Response),
    maps:from_list(Pairs);
response_fields(Response, Keys, grpc) when is_map(Response) ->
    maps:filter(fun(_, Value) -> Value =/= undefined end, maps:with(Keys, Response)).

enum_value(Value, Type, Protocol) ->
    Values = enum_values(Type),
    case
        [
            Binary
         || {Number, Atom, Binary} <- Values,
            Value =:= Binary orelse
                (Protocol =:= grpc andalso
                    (Value =:= Number orelse Value =:= Atom))
        ]
    of
        [Binary] -> Binary
    end.

enum_values(decision) ->
    [{1, 'ALLOW', <<"allow">>}, {2, 'REVIEW', <<"review">>}, {3, 'BLOCK', <<"block">>}];
enum_values(severity) ->
    [{1, 'LOW', <<"low">>}, {2, 'MEDIUM', <<"medium">>}, {3, 'HIGH', <<"high">>}];
enum_values(match_method) ->
    [
        {1, 'LITERAL', <<"literal">>},
        {2, 'T2S', <<"t2s">>},
        {3, 'GAP', <<"gap">>},
        {4, 'ALIAS', <<"alias">>},
        {5, 'REGEX', <<"regex">>}
    ];
enum_values(variant_type) ->
    [
        {1, 'CANONICAL', <<"canonical">>},
        {2, 'PINYIN', <<"pinyin">>},
        {3, 'HOMOPHONE', <<"homophone">>},
        {4, 'LOOKALIKE', <<"lookalike">>},
        {5, 'DECOMPOSED', <<"decomposed">>},
        {6, 'REGEX_RULE', <<"regex_rule">>}
    ].

action_rank(<<"allow">>) -> 0;
action_rank(<<"review">>) -> 1;
action_rank(<<"block">>) -> 2.

valid_request_id(Value) -> matches(Value, <<"[A-Za-z0-9._-]{1,64}">>).
valid_digest(Value) -> matches(Value, <<"[0-9a-f]{64}">>).
valid_generation(Value) ->
    matches(Value, <<"(0|[1-9][0-9]{0,19})">>) andalso
        binary_to_integer(Value) =< 18446744073709551615.

matches(Value, Pattern) when is_binary(Value) ->
    re:run(Value, <<"\\A(?:", Pattern/binary, ")\\z">>, [{capture, none}]) =:= match;
matches(_, _) ->
    false.

valid_string(Value, Min, Max) when is_binary(Value) ->
    case unicode:characters_to_list(Value, utf8) of
        Chars when is_list(Chars) -> length(Chars) >= Min andalso length(Chars) =< Max;
        _ -> false
    end;
valid_string(_, _, _) ->
    false.

error_code_to_reason(<<"OVERLOADED">>) -> overloaded;
error_code_to_reason(<<"TEXT_TOO_LARGE">>) -> text_too_large;
error_code_to_reason(<<"NORMALIZATION_LIMIT">>) -> normalization_limit;
error_code_to_reason(<<"MATCH_BUDGET_EXCEEDED">>) -> match_budget_exceeded;
error_code_to_reason(<<"UNAUTHENTICATED">>) -> unauthenticated;
error_code_to_reason(<<"PERMISSION_DENIED">>) -> permission_denied;
error_code_to_reason(<<"NOT_READY">>) -> not_ready;
error_code_to_reason(<<"CONNECTION_FAILED">>) -> connection_failed;
error_code_to_reason(<<"CHECK_DEADLINE">>) -> timeout;
error_code_to_reason(<<"INVALID_ARGUMENT">>) -> invalid_argument;
error_code_to_reason(<<"INVALID_UTF8">>) -> invalid_utf8;
error_code_to_reason(<<"PROTOCOL_ERROR">>) -> protocol_error;
error_code_to_reason(<<"NORMALIZER_INCOMPATIBLE">>) -> normalizer_incompatible;
error_code_to_reason(<<"INTERNAL_ERROR">>) -> internal;
error_code_to_reason(<<"UNSUPPORTED_RULESET">>) -> unsupported_ruleset;
error_code_to_reason(<<"CAS_CONFLICT">>) -> cas_conflict;
error_code_to_reason(<<"INVALID_RULESET">>) -> invalid_ruleset;
error_code_to_reason(<<"PUBLISH_DEADLINE">>) -> publish_deadline;
error_code_to_reason(<<"PUBLISH_STATE_UNKNOWN">>) -> publish_state_unknown;
error_code_to_reason(<<"PUBLISH_BUSY">>) -> publish_busy;
error_code_to_reason(<<"RETIRED_SNAPSHOT_BUSY">>) -> retired_snapshot_busy;
error_code_to_reason(_) -> protocol_error.
