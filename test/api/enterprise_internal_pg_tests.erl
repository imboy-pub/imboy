%% enterprise_internal_pg_tests
%% EPGZ-02 — internal credential 认证链 / scope / 幂等 / 运维命令 真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 EPGZ02_INTTEST，直连
%% imboy_pg18:4323）。业务用例每条 BEGIN ... ROLLBACK，不留数据。
%%
%% 覆盖（plan-gz §4.1/§4.2、§6；manifest auth_contexts/INV-4/INV-7/INV-9）：
%%   ① 固定 scope 枚举：10 个、无 wildcard、无隐含包含（INV-4 负例）
%%   ② 错误信封：13 个 stable 码 → HTTP 状态映射 + 信封形态
%%   ③ credential 解析：合法/畸形各形态
%%   ④ 认证链负例矩阵：unknown prefix / 错 secret / revoked / expired /
%%      application disabled / organization archived / 成功 context 形态
%%   ⑤ 中间件 decide：路由匹配、scope、rate（含 fail-closed）、
%%      mutation 缺 Idempotency-Key 拒绝（INV-7）、INT-14 豁免
%%   ⑥ 幂等三态：同 key 同 body 重放 / 同 key 异 body 409 / claim 单次
%%   ⑦ 运维命令：create application / issue credential（明文只出现一次）/
%%      rotate（新生效旧吊销）/ revoke / status（无 digest 泄露）
%%   ⑧ redaction：认证失败日志与错误信封不含 secret / Authorization 值
-module(enterprise_internal_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% logger handler 回调（redaction 捕获用；agent_grant_tests 同款）
-export([log/2]).

%% ---- 夹具（988 段独立 ID，与 A1 987 段互不冲突） ----

-define(OWNER_A, 988001).
-define(OWNER_B, 988002).
-define(ORG_A, 988101).
-define(ORG_B, 988102).

-define(READ_SCOPES, [<<"application:read">>, <<"identities:read">>]).
%% 刻意不含 application:read：供 middleware_route_and_flow 的
%% insufficient_scope 负例使用（GET /application 需要 application:read）。
-define(WRITE_SCOPES, [<<"identities:read">>, <<"identities:write">>, <<"groups:write">>]).

-define(RATE_CFG, #{internal_read => 1000, internal_write => 1000, internal_sso => 1000}).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(throttle),
    application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG),
    inttest_marker_db:provision(#{
        env_prefix => <<"EPGZ02_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    application:unset_env(imboy, enterprise_internal_rate_limits),
    inttest_marker_db:release(State),
    ok.

%% 每条业务用例 BEGIN ... ROLLBACK，不留数据。
with_tx(C, TestFun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            TestFun(C),
            ok
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

%% 负例包装（A1 同款）：预期 SQL 错误会 abort 事务，在 SAVEPOINT 内执行。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT epgz02_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT epgz02_sp">>),
        exec(C, <<"RELEASE SAVEPOINT epgz02_sp">>)
    end.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

seed_org(C, OrgId, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(OwnerUid),
        ", 'x', 't988_owner_",
        integer_to_binary(OwnerUid),
        <<"', '127.0.0.1', 'x')">>
    ]),
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", 't988_org', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

%% 建 org + application + 凭证（固定 secret 便于断言），返回
%% #{org, app_id, prefix, secret, credential}。
seed_app_fixture(C) ->
    seed_org(C, ?ORG_A, ?OWNER_A),
    {ok, App} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"epgz02-oa">>, <<"epgz02 test oa"/utf8>>, ?WRITE_SCOPES
    ),
    AppId = maps:get(<<"id">>, App),
    Secret = <<"epgz02_high_entropy_secret_0123456789">>,
    {ok, #{credential_id := CredId, credential := Full}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppId, Secret, undefined),
    Prefix = prefix_of(Full),
    #{
        org => ?ORG_A,
        app_id => AppId,
        credential_id => CredId,
        prefix => Prefix,
        secret => Secret,
        credential => Full
    }.

%% 从完整凭证（ib_int_<locator>.<secret>）解析定位前缀。
%% locator 数字与行 id 独立（repo create_tx 内部生成行 id，prefix 由调用方
%% 传入——repo API 缺口已记录 EPGZ-02 checkpoint），故必须从返回值解析。
prefix_of(Full) ->
    [Prefix, _Secret] = binary:split(Full, <<".">>),
    Prefix.

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_internal_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                %% ① 固定 scope（纯）
                {"scope_fixed_enum_no_wildcard", scope_fixed_enum_test()},
                {"scope_no_implicit_inclusion", scope_no_implicit_inclusion_test()},
                %% ② 错误信封（纯）
                {"error_envelope_status_map", error_envelope_status_test()},
                {"error_envelope_body_shape", error_envelope_body_shape_test()},
                %% ③ credential 解析（纯）
                {"credential_parse_forms", credential_parse_forms_test()},
                %% ④ 认证链（真库）
                {"auth_chain_success_context", with_tx(C, fun auth_chain_success/1)},
                {"auth_chain_unknown_prefix", with_tx(C, fun auth_chain_unknown_prefix/1)},
                {"auth_chain_wrong_secret", with_tx(C, fun auth_chain_wrong_secret/1)},
                {"auth_chain_revoked", with_tx(C, fun auth_chain_revoked/1)},
                {"auth_chain_expired", with_tx(C, fun auth_chain_expired/1)},
                {"auth_chain_application_disabled",
                    with_tx(C, fun auth_chain_application_disabled/1)},
                {"auth_chain_organization_archived",
                    with_tx(C, fun auth_chain_organization_archived/1)},
                {"auth_chain_touches_last_used", with_tx(C, fun auth_chain_touches_last_used/1)},
                %% ⑤ 中间件 decide（真库认证 + 内存路由/限流/幂等键）
                {"middleware_route_and_flow", with_tx(C, fun middleware_route_and_flow/1)},
                {"middleware_rate_fail_closed", middleware_rate_fail_closed_test()},
                {"middleware_rate_limited", middleware_rate_limited_test()},
                {"middleware_idempotency_key_required",
                    with_tx(C, fun middleware_idem_key_required/1)},
                {"middleware_int14_exempt_idempotency", middleware_int14_exempt_test()},
                {"middleware_credential_forbidden_prefixes",
                    with_tx(C, fun middleware_forbidden_prefixes/1)},
                %% ⑥ 幂等三态（真库）
                {"idempotency_three_states", with_tx(C, fun idempotency_three_states/1)},
                {"idempotency_claim_single", with_tx(C, fun idempotency_claim_single/1)},
                %% ⑦ 运维命令（真库）
                {"ops_application_lifecycle", with_tx(C, fun ops_application_lifecycle/1)},
                {"ops_credential_plaintext_once", with_tx(C, fun ops_credential_plaintext_once/1)},
                {"ops_rotate_new_works_old_revoked", with_tx(C, fun ops_rotate/1)},
                {"ops_revoke", with_tx(C, fun ops_revoke/1)},
                {"ops_status_no_digest_leak", with_tx(C, fun ops_status_no_digest_leak/1)},
                %% ⑧ redaction
                {"redaction_log_and_envelope", with_tx(C, fun redaction_log_and_envelope/1)}
            ]
        end}}.

%%%===================================================================
%%% ① 固定 scope 枚举（plan-gz §4.2，INV-4）
%%%===================================================================

scope_fixed_enum_test() ->
    ?_test(begin
        %% all() 返回 plan-gz §4.2 的固定顺序（不做 sort：顺序也是契约）
        ?assertEqual(
            [
                <<"application:read">>,
                <<"identities:read">>,
                <<"identities:write">>,
                <<"groups:write">>,
                <<"files:write">>,
                <<"messages:send">>,
                <<"messages:send_as_human">>,
                <<"friend_requests:create">>,
                <<"webhooks:manage">>,
                <<"sso:exchange">>
            ],
            enterprise_internal_scope:all()
        ),
        %% 显式授予才可用；未授予即拒
        ?assertEqual(
            ok,
            enterprise_internal_scope:authorize(
                <<"groups:write">>, [<<"application:read">>, <<"groups:write">>]
            )
        ),
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_internal_scope:authorize(<<"groups:write">>, [<<"application:read">>])
        ),
        %% wildcard 不存在也不被接受
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_internal_scope:authorize(<<"groups:write">>, [<<"*">>])
        ),
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_internal_scope:authorize(<<"*">>, [<<"*">>])
        ),
        %% 非法 scope 字符串不能被 authorize 接受为 required
        ?assertEqual(
            {error, invalid_scope},
            enterprise_internal_scope:authorize(<<"messages:send:*">>, [<<"messages:send:*">>])
        )
    end).

scope_no_implicit_inclusion_test() ->
    ?_test(begin
        %% INV-4：三个高危 scope 不被 messages:send 隐含包含
        Granted = [<<"application:read">>, <<"messages:send">>],
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_internal_scope:authorize(<<"messages:send_as_human">>, Granted)
        ),
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_internal_scope:authorize(<<"friend_requests:create">>, Granted)
        ),
        ?assertEqual(
            {error, insufficient_scope},
            enterprise_internal_scope:authorize(<<"webhooks:manage">>, Granted)
        ),
        %% 显式授予后可用（对照组）
        ?assertEqual(
            ok,
            enterprise_internal_scope:authorize(
                <<"messages:send_as_human">>, [<<"messages:send_as_human">>]
            )
        )
    end).

%%%===================================================================
%%% ② 错误信封（manifest stable_error_codes）
%%%===================================================================

error_envelope_status_test() ->
    ?_test(begin
        Expected = #{
            <<"invalid_credential">> => 401,
            <<"credential_expired">> => 401,
            <<"application_disabled">> => 403,
            <<"organization_disabled">> => 403,
            <<"insufficient_scope">> => 403,
            <<"resource_not_found">> => 404,
            <<"identity_not_mapped">> => 422,
            <<"organization_boundary_violation">> => 403,
            <<"idempotency_conflict">> => 409,
            <<"rate_limited">> => 429,
            <<"security_gate_closed">> => 503,
            <<"invalid_request">> => 400,
            <<"internal_error">> => 500
        },
        ?assertEqual(
            lists:sort(maps:keys(Expected)), lists:sort(enterprise_internal_error:codes())
        ),
        maps:foreach(
            fun(Code, Status) ->
                ?assertEqual(Status, enterprise_internal_error:http_status(Code), {Code, status})
            end,
            Expected
        ),
        %% 未知码 fail-safe 落 internal_error 语义
        ?assertEqual(500, enterprise_internal_error:http_status(<<"boom_unknown">>))
    end).

error_envelope_body_shape_test() ->
    ?_test(begin
        Body = enterprise_internal_error:error_body(<<"invalid_credential">>),
        ?assertMatch(
            #{<<"error">> := #{<<"code">> := <<"invalid_credential">>, <<"message">> := _}},
            jsone:decode(Body)
        ),
        %% 信封不含任何凭证/正文载体字段
        ?assertEqual(false, binary:match(Body, <<"secret">>) =/= nomatch),
        ?assertEqual(false, binary:match(Body, <<"authorization">>) =/= nomatch)
    end).

%%%===================================================================
%%% ③ credential 解析（格式 ib_int_<credential_id>.<secret>）
%%%===================================================================

credential_parse_forms_test() ->
    ?_test(begin
        ?assertEqual(
            {ok, <<"ib_int_1234567890">>, <<"s3cr3t-value">>},
            enterprise_internal_auth:parse_bearer(<<"Bearer ib_int_1234567890.s3cr3t-value">>)
        ),
        ?assertEqual({error, credential_missing}, enterprise_internal_auth:parse_bearer(<<>>)),
        ?assertEqual(
            {error, credential_missing}, enterprise_internal_auth:parse_bearer(undefined)
        ),
        ?assertEqual(
            {error, invalid_credential},
            enterprise_internal_auth:parse_bearer(<<"ib_int_1234567890.s3cr3t-value">>)
        ),
        ?assertEqual(
            {error, invalid_credential},
            enterprise_internal_auth:parse_bearer(<<"Bearer ib_ext_1234567890.s3cr3t-value">>)
        ),
        ?assertEqual(
            {error, invalid_credential},
            enterprise_internal_auth:parse_bearer(<<"Bearer 1234567890.s3cr3t-value">>)
        ),
        %% 无点分隔 / 空 secret / 非数字 credential_id
        ?assertEqual(
            {error, invalid_credential},
            enterprise_internal_auth:parse_bearer(<<"Bearer ib_int_1234567890">>)
        ),
        ?assertEqual(
            {error, invalid_credential},
            enterprise_internal_auth:parse_bearer(<<"Bearer ib_int_1234567890.">>)
        ),
        ?assertEqual(
            {error, invalid_credential},
            enterprise_internal_auth:parse_bearer(<<"Bearer ib_int_notdigits.s3cr3t-value">>)
        )
    end).

%%%===================================================================
%%% ④ 认证链（真库）
%%%===================================================================

auth_chain_success(C) ->
    F = seed_app_fixture(C),
    ?assertMatch(
        {ok, #{
            organization_id := ?ORG_A,
            application_id := _,
            credential_id := _,
            granted_scopes := _
        }},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ),
    {ok, Ctx} = enterprise_internal_auth:authenticate_tx(
        C, maps:get(prefix, F), maps:get(secret, F)
    ),
    ?assertEqual(lists:sort(?WRITE_SCOPES), lists:sort(maps:get(granted_scopes, Ctx))),
    ?assertEqual(maps:get(app_id, F), maps:get(application_id, Ctx)),
    ?assertEqual(maps:get(credential_id, F), maps:get(credential_id, Ctx)).

auth_chain_unknown_prefix(C) ->
    _F = seed_app_fixture(C),
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:authenticate_tx(C, <<"ib_int_111111111">>, <<"whatever-secret">>)
    ).

auth_chain_wrong_secret(C) ->
    F = seed_app_fixture(C),
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), <<"wrong-secret">>)
    ).

auth_chain_revoked(C) ->
    F = seed_app_fixture(C),
    ok = enterprise_internal_ops:revoke_credential_tx(C, ?ORG_A, maps:get(credential_id, F)),
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ).

auth_chain_expired(C) ->
    seed_org(C, ?ORG_A, ?OWNER_A),
    {ok, App} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"epgz02-oa-exp">>, <<"expired cred app"/utf8>>, ?READ_SCOPES
    ),
    AppId = maps:get(<<"id">>, App),
    Past = <<"2020-01-01T00:00:00+00:00">>,
    {ok, #{credential := ExpiredFull}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppId, <<"secret-expired">>, Past),
    ?assertEqual(
        {error, credential_expired},
        enterprise_internal_auth:authenticate_tx(C, prefix_of(ExpiredFull), <<"secret-expired">>)
    ).

auth_chain_application_disabled(C) ->
    F = seed_app_fixture(C),
    ok = enterprise_internal_ops:set_application_status_tx(
        C, ?ORG_A, maps:get(app_id, F), <<"disabled">>
    ),
    ?assertEqual(
        {error, application_disabled},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ).

auth_chain_organization_archived(C) ->
    F = seed_app_fixture(C),
    exec(C, [
        <<"UPDATE organization SET status = 'archived' WHERE id = ">>,
        integer_to_binary(?ORG_A)
    ]),
    ?assertEqual(
        {error, organization_disabled},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ).

auth_chain_touches_last_used(C) ->
    F = seed_app_fixture(C),
    {ok, _} = enterprise_internal_auth:authenticate_tx(
        C, maps:get(prefix, F), maps:get(secret, F)
    ),
    {ok, [Row | _]} = elib_pg:query(
        C,
        [
            <<"SELECT last_used_at IS NOT NULL AS used FROM enterprise_application_credential">>,
            <<" WHERE id = ">>,
            integer_to_binary(maps:get(credential_id, F))
        ],
        []
    ),
    ?assertEqual(true, maps:get(<<"used">>, Row)).

%%%===================================================================
%%% ⑤ 中间件 decide（认证链之上的路由/scope/rate/幂等键编排）
%%%===================================================================

auth_fun(C, F) ->
    fun() ->
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    end.

headers(F) ->
    Full = <<(maps:get(prefix, F))/binary, ".", (maps:get(secret, F))/binary>>,
    #{<<"authorization">> => <<"Bearer ", Full/binary>>}.

middleware_route_and_flow(C) ->
    F = seed_app_fixture(C),
    %% INT-01：scope application:read 未授予（fixture 只给 write scopes）→ 403 码
    ?assertEqual(
        {error, insufficient_scope},
        enterprise_internal_auth:decide(
            <<"GET">>, <<"/api/internal/v1/application">>, headers(F), auth_fun(C, F)
        )
    ),
    %% 授予后通过，context 含 route 元数据
    ok = enterprise_internal_ops:update_scopes_tx(
        C, ?ORG_A, maps:get(app_id, F), [<<"application:read">> | ?WRITE_SCOPES]
    ),
    ?assertMatch(
        {ok, #{route_id := <<"INT-01">>, rate_bucket := internal_read}},
        enterprise_internal_auth:decide(
            <<"GET">>, <<"/api/internal/v1/application">>, headers(F), auth_fun(C, F)
        )
    ),
    %% INT-09/INT-10 动态 scope：中间件放行（handler 按 sender_mode 裁决），
    %% context 标注 dynamic_scope（mutation 带 Idempotency-Key）
    HIdem = maps:put(<<"idempotency-key">>, <<"epgz02-key-dyn">>, headers(F)),
    ?assertMatch(
        {ok, #{dynamic_scope := messages_send}},
        enterprise_internal_auth:decide(
            <<"POST">>, <<"/api/internal/v1/messages/direct">>, HIdem, auth_fun(C, F)
        )
    ),
    %% 未知路由 → resource_not_found（fail-closed，不落人类 API）
    ?assertEqual(
        {error, resource_not_found},
        enterprise_internal_auth:decide(
            <<"GET">>, <<"/api/internal/v1/nope">>, headers(F), auth_fun(C, F)
        )
    ),
    %% 方法不匹配 → resource_not_found
    ?assertEqual(
        {error, resource_not_found},
        enterprise_internal_auth:decide(
            <<"DELETE">>, <<"/api/internal/v1/application">>, headers(F), auth_fun(C, F)
        )
    ),
    %% 认证失败透传 stable 码（AuthFun 认证头里的真实凭证：未知 prefix）
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:decide(
            <<"GET">>,
            <<"/api/internal/v1/application">>,
            #{<<"authorization">> => <<"Bearer ib_int_1.bad">>},
            fun() ->
                enterprise_internal_auth:authenticate_tx(C, <<"ib_int_1">>, <<"bad">>)
            end
        )
    ).

middleware_rate_fail_closed_test() ->
    ?_test(begin
        application:unset_env(imboy, enterprise_internal_rate_limits),
        try
            AuthFun =
                fun() ->
                    {ok, #{application_id => 988777001, granted_scopes => [<<"application:read">>]}}
                end,
            ?assertEqual(
                {error, security_gate_closed},
                enterprise_internal_auth:decide(
                    <<"GET">>,
                    <<"/api/internal/v1/application">>,
                    #{<<"authorization">> => <<"Bearer ib_int_1.x">>},
                    AuthFun
                ),
                "限流配置缺失必须 fail-closed（INV-9），不得 fail-open"
            )
        after
            ok = application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG)
        end
    end).

middleware_rate_limited_test() ->
    ?_test(begin
        AppId = 988777002,
        application:set_env(
            imboy,
            enterprise_internal_rate_limits,
            #{internal_read => 2, internal_write => 2, internal_sso => 2}
        ),
        try
            H = #{<<"authorization">> => <<"Bearer ib_int_1.x">>},
            AuthFun = fun() ->
                {ok, #{application_id => AppId, granted_scopes => [<<"application:read">>]}}
            end,
            ?assertMatch(
                {ok, _},
                enterprise_internal_auth:decide(
                    <<"GET">>, <<"/api/internal/v1/application">>, H, AuthFun
                )
            ),
            ?assertMatch(
                {ok, _},
                enterprise_internal_auth:decide(
                    <<"GET">>, <<"/api/internal/v1/application">>, H, AuthFun
                )
            ),
            ?assertEqual(
                {error, rate_limited},
                enterprise_internal_auth:decide(
                    <<"GET">>, <<"/api/internal/v1/application">>, H, AuthFun
                )
            )
        after
            ok = application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG)
        end
    end).

middleware_idem_key_required(C) ->
    F = seed_app_fixture(C),
    ok = enterprise_internal_ops:update_scopes_tx(
        C, ?ORG_A, maps:get(app_id, F), [<<"identities:write">>, <<"groups:write">> | ?WRITE_SCOPES]
    ),
    %% INT-02 mutation：无 Idempotency-Key → invalid_request（INV-7）
    ?assertEqual(
        {error, invalid_request},
        enterprise_internal_auth:decide(
            <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, headers(F), auth_fun(C, F)
        )
    ),
    %% 带非空 key 通过编排层
    H2 = maps:put(<<"idempotency-key">>, <<"epgz02-key-1">>, headers(F)),
    ?assertMatch(
        {ok, #{route_id := <<"INT-02">>}},
        enterprise_internal_auth:decide(
            <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, H2, auth_fun(C, F)
        )
    ),
    %% 空串 key 同样拒绝
    H3 = maps:put(<<"idempotency-key">>, <<>>, headers(F)),
    ?assertEqual(
        {error, invalid_request},
        enterprise_internal_auth:decide(
            <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, H3, auth_fun(C, F)
        )
    ).

middleware_int14_exempt_test() ->
    ?_test(begin
        %% INT-14 幂等豁免（single_use_code）：无 Idempotency-Key 不因 INV-7 拒绝
        AppId = 988777003,
        try
            application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG),
            H = #{<<"authorization">> => <<"Bearer ib_int_1.x">>},
            AuthFun =
                fun() ->
                    {ok, #{
                        application_id => AppId,
                        organization_id => ?ORG_A,
                        credential_id => 1,
                        granted_scopes => [<<"sso:exchange">>, <<"application:read">>]
                    }}
                end,
            ?assertMatch(
                {ok, #{route_id := <<"INT-14">>, rate_bucket := internal_sso}},
                enterprise_internal_auth:decide(
                    <<"POST">>, <<"/api/internal/v1/oa/sso/exchange">>, H, AuthFun
                )
            )
        after
            ok = application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG)
        end
    end).

middleware_forbidden_prefixes(C) ->
    %% INV-2：credential 只能进 /api/internal/v1/*；对非 internal 前缀 decide
    %% 一律 resource_not_found（不误放行、不落人类/admin 面）
    F = seed_app_fixture(C),
    lists:foreach(
        fun({M, P}) ->
            ?assertEqual(
                {error, resource_not_found},
                enterprise_internal_auth:decide(M, P, headers(F), auth_fun(C, F)),
                {credential_must_not_route, M, P}
            )
        end,
        [
            {<<"GET">>, <<"/api/v1/user">>},
            {<<"GET">>, <<"/api/adm/user">>},
            {<<"GET">>, <<"/api/open/v1/anything">>},
            {<<"POST">>, <<"/api/v1/oa/sso/code">>}
        ]
    ).

%%%===================================================================
%%% ⑥ 幂等三态（INV-7；repo：enterprise_internal_idempotency_repo）
%%%===================================================================

idempotency_three_states(C) ->
    F = seed_app_fixture(C),
    Ctx = #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_id, F),
        credential_id => maps:get(credential_id, F)
    },
    Key = <<"epgz02-idem-key-1">>,
    D1 = enterprise_internal_idempotency:request_digest(
        <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, <<"{\"external_user_id\":\"e1\"}">>
    ),
    %% 态一：首插 → inserted（执行业务）
    ?assertEqual(
        {ok, inserted},
        enterprise_internal_idempotency:begin_tx(C, Ctx, <<"identity_mapping">>, Key, D1)
    ),
    %% 业务执行成功后回填结果
    ok = enterprise_internal_idempotency:complete_tx(
        C, Ctx, <<"identity_mapping">>, Key, 4242, 200
    ),
    %% 态二：同 key 同 body → replay（回读原结果；回读 map 只含这两个键）
    ?assertEqual(
        {ok, replay, #{response_code => 200, resource_id => 4242}},
        enterprise_internal_idempotency:begin_tx(C, Ctx, <<"identity_mapping">>, Key, D1)
    ),
    %% 态三：同 key 异 body → digest_conflict（上层 409 idempotency_conflict）
    D2 = enterprise_internal_idempotency:request_digest(
        <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, <<"{\"external_user_id\":\"e2\"}">>
    ),
    ?assertEqual(
        {error, digest_conflict},
        enterprise_internal_idempotency:begin_tx(C, Ctx, <<"identity_mapping">>, Key, D2)
    ),
    %% 409 映射：digest_conflict → idempotency_conflict
    ?assertEqual(
        <<"idempotency_conflict">>,
        enterprise_internal_idempotency:conflict_code()
    ).

idempotency_claim_single(C) ->
    F = seed_app_fixture(C),
    Ctx = #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_id, F),
        credential_id => maps:get(credential_id, F)
    },
    Key = <<"epgz02-idem-key-claim">>,
    D = enterprise_internal_idempotency:request_digest(
        <<"POST">>, <<"/api/internal/v1/groups">>, <<"{}">>
    ),
    {ok, inserted} = enterprise_internal_idempotency:begin_tx(
        C, Ctx, <<"group">>, Key, D
    ),
    ?assertEqual(
        ok,
        enterprise_internal_idempotency:complete_tx(C, Ctx, <<"group">>, Key, 555000111, 201)
    ),
    ?assertEqual(
        {error, already_claimed},
        enterprise_internal_idempotency:complete_tx(C, Ctx, <<"group">>, Key, 555000112, 201),
        "并发第二个执行者不得重复回填"
    ),
    %% 回填后同 key 同 body 重放读回原结果
    ?assertEqual(
        {ok, replay, #{response_code => 201, resource_id => 555000111}},
        enterprise_internal_idempotency:begin_tx(C, Ctx, <<"group">>, Key, D)
    ).

%%%===================================================================
%%% ⑦ 运维命令（最小运维面，非 Admin UI）
%%%===================================================================

ops_application_lifecycle(C) ->
    seed_org(C, ?ORG_A, ?OWNER_A),
    {ok, App} = enterprise_internal_ops:create_application_tx(
        C, ?ORG_A, <<"epgz02-ops-app">>, <<"ops app"/utf8>>, [<<"application:read">>]
    ),
    AppId = maps:get(<<"id">>, App),
    ?assertEqual(?ORG_A, maps:get(<<"organization_id">>, App)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, App)),
    %% 同 org 同 key 唯一（SAVEPOINT：23505 会 abort 事务）
    ?assertEqual(
        {error, key_conflict},
        in_savepoint(C, fun() ->
            enterprise_internal_ops:create_application_tx(
                C, ?ORG_A, <<"epgz02-ops-app">>, <<"dup"/utf8>>, []
            )
        end)
    ),
    %% 非法 scope 拒绝
    ?assertEqual(
        {error, invalid_scope},
        enterprise_internal_ops:create_application_tx(
            C, ?ORG_A, <<"epgz02-ops-bad">>, <<"bad"/utf8>>, [<<"not:a:scope">>]
        )
    ),
    %% scopes 更新 / 状态切换
    ok = enterprise_internal_ops:update_scopes_tx(C, ?ORG_A, AppId, [<<"sso:exchange">>]),
    {ok, St1} = enterprise_internal_ops:application_status_tx(C, ?ORG_A, AppId),
    ?assertEqual([<<"sso:exchange">>], maps:get(granted_scopes, St1)),
    ok = enterprise_internal_ops:set_application_status_tx(C, ?ORG_A, AppId, <<"disabled">>),
    {ok, St2} = enterprise_internal_ops:application_status_tx(C, ?ORG_A, AppId),
    ?assertEqual(<<"disabled">>, maps:get(status, maps:get(application, St2))),
    ok = enterprise_internal_ops:set_application_status_tx(C, ?ORG_A, AppId, <<"active">>).

ops_credential_plaintext_once(C) ->
    F = seed_app_fixture(C),
    Full = maps:get(credential, F),
    %% 明文形态 ib_int_<id>.<secret> 且只在此处出现：库里只有 digest
    ?assertEqual(
        <<(maps:get(prefix, F))/binary, ".", (maps:get(secret, F))/binary>>, Full
    ),
    {ok, [Row | _]} = elib_pg:query(
        C,
        [
            <<"SELECT secret_digest FROM enterprise_application_credential WHERE id = ">>,
            integer_to_binary(maps:get(credential_id, F))
        ],
        []
    ),
    Digest = maps:get(<<"secret_digest">>, Row),
    ?assertEqual(64, byte_size(Digest)),
    ?assertEqual(false, binary:match(Digest, maps:get(secret, F)) =/= nomatch),
    %% 凭证可用（端到端）
    ?assertMatch(
        {ok, _},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ).

ops_rotate(C) ->
    F = seed_app_fixture(C),
    {ok, #{credential := NewFull}} =
        enterprise_internal_ops:rotate_credential_tx(C, ?ORG_A, maps:get(credential_id, F)),
    NewPrefix = prefix_of(NewFull),
    <<"ib_int_", _/binary>> = NewPrefix,
    %% 新凭证可用
    NewSecret = binary:part(
        NewFull, byte_size(NewPrefix) + 1, byte_size(NewFull) - byte_size(NewPrefix) - 1
    ),
    ?assertMatch({ok, _}, enterprise_internal_auth:authenticate_tx(C, NewPrefix, NewSecret)),
    %% 旧凭证立即失效
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ).

ops_revoke(C) ->
    F = seed_app_fixture(C),
    ok = enterprise_internal_ops:revoke_credential_tx(C, ?ORG_A, maps:get(credential_id, F)),
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), maps:get(secret, F))
    ),
    %% 幂等吊销
    ?assertEqual(
        {error, not_active},
        enterprise_internal_ops:revoke_credential_tx(C, ?ORG_A, maps:get(credential_id, F))
    ).

ops_status_no_digest_leak(C) ->
    F = seed_app_fixture(C),
    {ok, Status} = enterprise_internal_ops:application_status_tx(C, ?ORG_A, maps:get(app_id, F)),
    ?assertMatch(#{application := #{}, credentials := [_ | _]}, Status),
    lists:foreach(
        fun(Cred) ->
            ?assertEqual(false, maps:is_key(<<"secret_digest">>, Cred), "status 不得泄露 digest"),
            ?assertEqual(false, maps:is_key(secret_digest, Cred)),
            ?assertEqual(false, maps:is_key(credential, Cred), "status 不得泄露明文凭证")
        end,
        maps:get(credentials, Status)
    ).

%%%===================================================================
%%% ⑧ redaction：日志与错误信封不含 secret / Authorization 值
%%%===================================================================

redaction_log_and_envelope(C) ->
    F = seed_app_fixture(C),
    WrongSecret = <<"epgz02_WRONG_secret_VALUE_xyz">>,
    FullAuth = <<(maps:get(prefix, F))/binary, ".", WrongSecret/binary>>,
    Events = capture_events(fun() ->
        %% 错 secret 的认证失败（会打日志）
        {error, invalid_credential} =
            enterprise_internal_auth:authenticate_tx(C, maps:get(prefix, F), WrongSecret)
    end),
    %% 所有捕获的日志事件（含格式化后的串）都不得包含 secret / 完整 Authorization 值
    lists:foreach(
        fun(Ev) ->
            Text = event_text(Ev),
            ?assertEqual(
                false,
                binary:match(Text, WrongSecret) =/= nomatch,
                {secret_leaked_in_log, Text}
            ),
            ?assertEqual(
                false,
                binary:match(Text, FullAuth) =/= nomatch,
                {authorization_leaked_in_log, Text}
            )
        end,
        Events
    ),
    %% 错误信封同理
    Body = enterprise_internal_error:error_body(<<"invalid_credential">>),
    ?assertEqual(false, binary:match(Body, WrongSecret) =/= nomatch).

%% ---- logger 捕获（agent_grant_tests 同款） ----

capture_events(F) ->
    Parent = self(),
    HandlerId = epgz02_log_capture,
    ok = clear_handler(HandlerId),
    ok = logger:add_handler(HandlerId, ?MODULE, #{forward => Parent}),
    try
        F(),
        timer:sleep(100),
        collect_events([])
    after
        _ = logger:remove_handler(HandlerId)
    end.

clear_handler(Id) ->
    _ = logger:remove_handler(Id),
    ok.

collect_events(Acc) ->
    receive
        {epgz02_log, Ev} -> collect_events([Ev | Acc])
    after 300 ->
        lists:reverse(Acc)
    end.

log(Event, Config) ->
    case maps:get(forward, Config, undefined) of
        Pid when is_pid(Pid) -> Pid ! {epgz02_log, Event};
        _ -> ok
    end.

event_text(#{msg := Msg}) ->
    iolist_to_binary(io_lib:format("~0p", [Msg]));
event_text(_) ->
    <<>>.
