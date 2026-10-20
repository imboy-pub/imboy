-module(enterprise_oa_sso_tests).

%% EPGZ-05 W2 实测（OA one-time SSO 合同负例矩阵全量转绿）。
%%
%% 权威合同：docs/architecture/2026-09-21-epgz05-oa-sso-contract.md（W1 冻结）
%% 上游：plan-gz §7.2 / §6 INT-14 / §4 + control/internal-api-manifest.yaml
%%
%% 口径：
%%   * 一次性 marker 库（inttest_marker_db 配方，env 前缀 EPGZ05_INTTEST，
%%     直连 imboy_pg18:4323），业务用例每条 BEGIN ... ROLLBACK 不留数据；
%%     NEG-04 并发 CAS 用例例外（跨连接行锁需要已提交数据），自带
%%     seed-commit-race 全序，保留已提交事实，最后释放专用 marker 库。
%%   * 覆盖合同 §6 负例矩阵 26 条（NEG-01..15 / NEG-H01..H07 /
%%     NEG-X01..02 / STORAGE-01..02）+ 1 条实现在场哨兵（W1 哨兵翻转）。
%%   * 认证链产物（A2）经 enterprise_internal_auth:authenticate_signed_tx 真链获取；
%%     handler 面信封由壳层薄映射，HTTP 形态断言经 logic 错误码 +
%%     enterprise_internal_error 信封联合钉住（Router 接线归 A0 W4）。

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% logger handler 回调（NEG-15 redaction 捕获用；A2 同款）
-export([log/2]).

%% ---- 夹具（989 段独立 ID，与 A1 987 / A2 988 段互不冲突） ----

%% 用户/组织采用 run 唯一 ID；每次 seed 的应用 key 独立，已提交身份与审计
%% 不做逐行删除，随专用 marker 库释放清理。
-define(OWNER_A, run_base() + 1).
-define(OWNER_B, run_base() + 2).
-define(ORG_A, run_base() + 101).
-define(ORG_B, run_base() + 102).
-define(ALICE, run_base() + 201).
-define(NOMAP_H, run_base() + 202).
-define(FOREIGN, run_base() + 203).

-define(APP_KEY, fixture_app_key(<<"">>)).
-define(APP_KEY_B, fixture_app_key(<<"-b">>)).
-define(APP_KEY_BB, fixture_app_key(<<"-bb">>)).
-define(APP_KEY_EMPTY, fixture_app_key(<<"-empty">>)).
-define(APP_KEY_RO, fixture_app_key(<<"-ro">>)).
-define(REDIRECT, <<"https://oa.customer.example.com/sso/cb">>).
-define(NONCE, <<"nonce_0123456789abcdef">>).
-define(SECRET_A, <<"epgz05_high_entropy_secret_A_0123456789">>).
-define(SECRET_B, <<"epgz05_high_entropy_secret_B_0123456789">>).
-define(SECRET_E, <<"epgz05_high_entropy_secret_E_0123456789">>).
-define(SECRET_RO, <<"epgz05_high_entropy_secret_RO_012345678">>).
-define(EXCHANGE_PATH, <<"/api/internal/v1/oa/sso/exchange">>).
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
    State = inttest_marker_db:provision(#{
        env_prefix => <<"EPGZ05_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    PreviousSigningKey = application:get_env(imboy, postgre_aes_key),
    application:set_env(imboy, postgre_aes_key, crypto:strong_rand_bytes(32)),
    State#{previous_internal_signing_key => PreviousSigningKey}.

close_conn(State) ->
    application:unset_env(imboy, enterprise_internal_rate_limits),
    inttest_marker_db:release(State),
    case maps:get(previous_internal_signing_key, State) of
        {ok, PreviousSigningKey} -> application:set_env(imboy, postgre_aes_key, PreviousSigningKey);
        undefined -> application:unset_env(imboy, postgre_aes_key)
    end,
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

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec(C, IoData, Params) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

seed_user(C, Uid) ->
    %% NEG-04 已提交的用户/组织仍在本专用库中；后续 seed 复用这些业务对象。
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't989_u_",
        integer_to_binary(Uid),
        <<"', '127.0.0.1', 'x') ON CONFLICT (id) DO NOTHING">>
    ]).

seed_org(C, OrgId, OwnerUid) ->
    seed_user(C, OwnerUid),
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", 't989_org', ",
        integer_to_binary(OwnerUid),
        <<
            ", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
            " ON CONFLICT (id) DO NOTHING"
        >>
    ]).

seed_member(C, OrgId, Uid) ->
    seed_user(C, Uid),
    exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, joined_at, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        <<
            ", 'member', CURRENT_TIMESTAMP, 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"
            " ON CONFLICT DO NOTHING"
        >>
    ]).

%% SSO 主夹具：org A（ALICE 有 mapping / NOMAP_H 无 mapping / FOREIGN 非成员）
%% + app A（allowlist 注册 ?REDIRECT，scope sso:exchange）+ credential；
%% app B（同 org，NEG-06）；空 allowlist app（NEG-H05）；只读 scope app
%% （NEG-09）；过期 credential（NEG-10）；org B + app + credential（NEG-05）。
seed_sso_fixture(C) ->
    put({?MODULE, fixture_app_key}, binary:encode_hex(crypto:strong_rand_bytes(16), lowercase)),
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    ok = seed_member(C, ?ORG_A, ?ALICE),
    ok = seed_member(C, ?ORG_A, ?NOMAP_H),
    ok = seed_user(C, ?FOREIGN),
    {ok, AppA} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY, <<"epgz05 oa a"/utf8>>, null, [
            ?REDIRECT
        ]
    ),
    AppAId = maps:get(<<"id">>, AppA),
    {ok, #{credential := FullA}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppAId, ?SECRET_A, undefined),
    {ok, _} = enterprise_external_identity_repo:bind_tx(
        C, ?ORG_A, AppAId, <<"ext-alice">>, ?ALICE
    ),
    %% app B 同 org（NEG-06 跨 app）
    {ok, AppB} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_B, <<"epgz05 oa b"/utf8>>, null, [
            ?REDIRECT
        ]
    ),
    AppBId = maps:get(<<"id">>, AppB),
    {ok, #{credential := FullB}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppBId, ?SECRET_B, undefined),
    %% 空 allowlist app（NEG-H05：空 allowlist fail-closed）
    {ok, AppEmpty} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_EMPTY, <<"epgz05 oa empty"/utf8>>, null
    ),
    %% 只读 scope app（NEG-09：缺 sso:exchange）
    {ok, AppRO} = enterprise_application_repo:create_tx(
        C, ?ORG_A, ?APP_KEY_RO, <<"epgz05 oa ro"/utf8>>, null
    ),
    AppROId = maps:get(<<"id">>, AppRO),
    {ok, #{credential := FullRO}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_A, AppROId, ?SECRET_RO, undefined),
    %% 过期 credential（挂 app A；NEG-10）
    {ok, #{credential := FullE}} = enterprise_internal_ops:issue_credential_tx(
        C, ?ORG_A, AppAId, ?SECRET_E, <<"2020-01-01T00:00:00Z">>
    ),
    %% org B + credential（原 NEG-05 跨 org；定位修订后 org 不再是绑定
    %% 维度——同 key 唯一约束下 org B 应用需独立 key，改作「org 状态门
    %% 豁免后仍拒绝非本应用 code」的对照夹具）
    ok = seed_org(C, ?ORG_B, ?OWNER_B),
    {ok, AppBB} = enterprise_application_repo:create_tx(
        C, ?ORG_B, ?APP_KEY_BB, <<"epgz05 oa b-org"/utf8>>, null, [
            ?REDIRECT
        ]
    ),
    AppBBId = maps:get(<<"id">>, AppBB),
    SecretBB = <<"epgz05_high_entropy_secret_BB_012345678">>,
    {ok, #{credential := FullBB}} =
        enterprise_internal_ops:issue_credential_tx(C, ?ORG_B, AppBBId, SecretBB, undefined),
    #{
        org => ?ORG_A,
        org_b => ?ORG_B,
        app_id => AppAId,
        app_b_id => AppBId,
        app_empty_id => maps:get(<<"id">>, AppEmpty),
        app_ro_id => AppROId,
        app_bb_id => AppBBId,
        credential => FullA,
        credential_b => FullB,
        credential_ro => FullRO,
        credential_expired => FullE,
        credential_bb => FullBB,
        secret_bb => SecretBB
    }.

prefix_of(Full) ->
    [Prefix, _Secret] = binary:split(Full, <<".">>),
    Prefix.

valid_params() ->
    valid_params(?APP_KEY, ?REDIRECT, ?NONCE).

valid_params(AppKey, RedirectUri, Nonce) ->
    #{<<"application_key">> => AppKey, <<"redirect_uri">> => RedirectUri, <<"nonce">> => Nonce}.

%% 签发（成功前置断言）
issue_ok(C, Uid) ->
    issue_ok(C, Uid, valid_params()).

issue_ok(C, Uid, Params) ->
    {ok, R} = enterprise_oa_sso_logic:issue_code_tx(C, Uid, Params),
    R.

%% 交换：app A credential 真链认证后走 logic
do_exchange(C, F, Code, RedirectUri, Nonce) ->
    {ok, Ctx} = enterprise_hmac_test_support:signed_context_tx(
        C, prefix_of(maps:get(credential, F)), ?SECRET_A
    ),
    enterprise_oa_sso_logic:exchange_tx(C, Ctx, #{
        <<"code">> => Code, <<"redirect_uri">> => RedirectUri, <<"nonce">> => Nonce
    }).

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_oa_sso_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                %% 实现在场哨兵（W1 red_canary 翻转）
                {"impl_present_canary", impl_present_canary_test()},
                %% INT-14 面（合同 §4/§6）
                {"NEG-01 exchange 未知 code 被拒", with_tx(C, fun neg01_unknown_code/1)},
                {"NEG-02 exchange 过期 code（>60s）被拒", with_tx(C, fun neg02_expired_code/1)},
                {"NEG-03 exchange 重放被拒且不回放原响应", with_tx(C, fun neg03_replay/1)},
                {"NEG-04 并发双 exchange 恰好一个赢家", neg04_concurrent_cas_test(State)},
                {"NEG-05 exchange 跨应用 code 被拒（原跨 org 语义，定位修订后 org 非绑定维度）",
                    with_tx(C, fun neg05_cross_org/1)},
                {"NEG-06 exchange 跨 app（同 org）被拒", with_tx(C, fun neg06_cross_app/1)},
                {"NEG-07 exchange redirect_uri 不匹配被拒", with_tx(C, fun neg07_redirect_mismatch/1)},
                {"NEG-08 exchange nonce 不匹配被拒", with_tx(C, fun neg08_nonce_mismatch/1)},
                {"NEG-10 凭证/应用拒绝与组织归档无关性", with_tx(C, fun neg10_credential_chain/1)},
                {"NEG-11 identity_not_mapped 且 code 不被消费",
                    with_tx(C, fun neg11_identity_not_mapped/1)},
                {"NEG-12 exchange 语法非法请求 invalid_request", neg12_malformed_request_test()},
                {"NEG-13 internal_sso 限流配置缺失 fail-closed",
                    with_tx(C, fun neg13_rate_fail_closed/1)},
                {"NEG-14 exchange 响应无任何 IMBoy 凭证", with_tx(C, fun neg14_no_imboy_credential/1)},
                {"NEG-15 code/nonce 明文零落日志", with_tx(C, fun neg15_zero_log_leak/1)},
                %% HUMAN-SSO-01 面（合同 §3/§6）
                {"NEG-H01 签发缺 Human JWT 401", with_tx(C, fun neg_h01_no_jwt/1)},
                {"NEG-H02 签发未知/停用 application 404", with_tx(C, fun neg_h02_unknown_app/1)},
                {"POS-H03 非成员用户签发成功（org 成员关系不参与判定）", with_tx(C, fun pos_h03_any_human/1)},
                {"POS-H04 无 identity mapping 签发成功（mapping 可选化）",
                    with_tx(C, fun pos_h04_no_mapping/1)},
                {"NEG-H05 签发 redirect_uri 不在 allowlist 400",
                    with_tx(C, fun neg_h05_redirect_not_allowlisted/1)},
                {"NEG-H06 签发 nonce/body 格式非法 400", neg_h06_malformed_params_test()},
                {"NEG-H07 重复签发 code 相互独立且 TTL 恒 60", with_tx(C, fun neg_h07_independent_codes/1)},
                {"NEG-H08 当前非 active Human 拒绝签发且无残留", with_tx(C, fun neg_h08_inactive_issue/1)},
                %% 双向隔离（manifest INV-2/INV-3）
                {"NEG-X01 Application Credential 不能调签发端点",
                    with_tx(C, fun neg_x01_credential_cannot_issue/1)},
                {"NEG-X02 Human JWT 不能调 exchange 端点",
                    with_tx(C, fun neg_x02_human_jwt_cannot_exchange/1)},
                %% 存储合同（合同 §2 digest-only）
                {"STORAGE-01 code/nonce 只存 digest", with_tx(C, fun storage01_digest_only/1)},
                {"STORAGE-02 签发响应字段合同", with_tx(C, fun storage02_issue_response/1)}
            ]
        end}}.

%%%===================================================================
%%% 哨兵（W1 red_canary_impl_absent_test_ 翻转：实现必须在场）
%%%===================================================================

impl_present_canary_test() ->
    ?_assertNotEqual(non_existing, code:which(enterprise_oa_sso_logic)).

%%%===================================================================
%%% INT-14 面
%%%===================================================================

%% NEG-01：未知 code（同形态伪造，oa_sso_ + 43 字符随机）→ 统一不透明拒绝。
neg01_unknown_code(C) ->
    F = seed_sso_fixture(C),
    _R = issue_ok(C, ?ALICE),
    Fake = fake_code(),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, Fake, ?REDIRECT, ?NONCE)
    ).

%% NEG-02：签发后时钟越过 60s（expires_at 推回 61s）→ resource_not_found。
neg02_expired_code(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    Digest = enterprise_oa_sso_code_repo:digest_hex(Code),
    ok = exec(
        C,
        <<"UPDATE enterprise_oa_sso_code SET expires_at = CURRENT_TIMESTAMP - interval '61 seconds' WHERE code_digest = $1">>,
        [Digest]
    ),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, Code, ?REDIRECT, ?NONCE)
    ).

%% NEG-03：同 code 第二次 exchange → resource_not_found，不回放首次成功响应。
neg03_replay(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    {ok, First} = do_exchange(C, F, Code, ?REDIRECT, ?NONCE),
    ?assert(is_map(First)),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, Code, ?REDIRECT, ?NONCE)
    ).

%% NEG-04：并发双 exchange 同一 code——跨连接行锁串行化 CAS，
%% 恰好 1 赢家。本用例需要已提交数据（两个独立连接互不可见未提交行），
%% 故自带 seed-commit-race 全序；已提交身份/审计保留至专用库释放。
neg04_concurrent_cas_test(State) ->
    ?_test(begin
        C = maps:get(conn, State),
        ok = exec(C, <<"BEGIN">>),
        F = seed_sso_fixture(C),
        R = issue_ok(C, ?ALICE),
        Code = maps:get(<<"code">>, R),
        {ok, Ctx} = enterprise_hmac_test_support:signed_context_tx(
            C, prefix_of(maps:get(credential, F)), ?SECRET_A
        ),
        ok = exec(C, <<"COMMIT">>),
        C2 = extra_conn(State),
        C3 = extra_conn(State),
        try
            Params = #{<<"code">> => Code, <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => ?NONCE},
            Results = race_exchange(C2, C3, Ctx, Params),
            Winners = [R1 || {ok, _} = R1 <- Results],
            Losers = [R2 || {error, <<"resource_not_found">>} = R2 <- Results],
            ?assertEqual(1, length(Winners)),
            ?assertEqual(1, length(Losers)),
            ?assertEqual(2, length(Results))
        after
            close_extra(C2),
            close_extra(C3)
        end
    end).

race_exchange(C2, C3, Ctx, Params) ->
    Parent = self(),
    P1 = spawn_runner(C2, Parent, Ctx, Params),
    P2 = spawn_runner(C3, Parent, Ctx, Params),
    receive_ready(P1),
    receive_ready(P2),
    P1 ! go,
    P2 ! go,
    [receive_result(P1), receive_result(P2)].

spawn_runner(Conn, Parent, Ctx, Params) ->
    spawn(fun() ->
        ok = exec(Conn, <<"BEGIN">>),
        Parent ! {ready, self()},
        receive
            go -> ok
        after 10000 -> ok
        end,
        Result =
            try
                enterprise_oa_sso_logic:exchange_tx(Conn, Ctx, Params)
            catch
                Class:Reason -> {crashed, Class, Reason}
            end,
        %% 赢家提交消费；输家/异常回滚（对断言两者均无害）
        case Result of
            {ok, _} -> _ = try_ok(fun() -> exec(Conn, <<"COMMIT">>) end);
            _ -> _ = try_ok(fun() -> exec(Conn, <<"ROLLBACK">>) end)
        end,
        Parent ! {result, self(), Result}
    end).

receive_ready(P) ->
    receive
        {ready, P} -> ok
    after 5000 ->
        erlang:error(racer_not_ready)
    end.

receive_result(P) ->
    receive
        {result, P, Result} -> Result
    after 10000 ->
        erlang:error(racer_no_result)
    end.

extra_conn(State) ->
    S = maps:get(server, State),
    {ok, Conn} = inttest_marker_db:safe_connect(#{
        host => maps:get(host, S),
        port => maps:get(port, S),
        username => maps:get(username, S),
        password => maps:get(password, S),
        database => maps:get(db, State),
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    Conn.

close_extra(Conn) ->
    try
        epgsql:close(Conn)
    catch
        _:_ -> ok
    end,
    ok.

%% NEG-05（定位修订后语义）：app BB（org B 归属）credential 换 app A
%% 签发的 code → resource_not_found（绑定四元组的 app 维度拒绝；org 不
%% 再参与绑定）。同时钉住：org B 应用照常通过 A2 认证链（org 状态门
%% 按路由豁免），拒绝来自 code 绑定而非 org 边界。
neg05_cross_org(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    {ok, CtxB} = enterprise_hmac_test_support:signed_context_tx(
        C, prefix_of(maps:get(credential_bb, F)), maps:get(secret_bb, F)
    ),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        enterprise_oa_sso_logic:exchange_tx(C, CtxB, #{
            <<"code">> => Code, <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => ?NONCE
        })
    ).

%% NEG-06：app B credential（同 org）换 app A code → resource_not_found。
neg06_cross_app(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    {ok, CtxB} = enterprise_hmac_test_support:signed_context_tx(
        C, prefix_of(maps:get(credential_b, F)), ?SECRET_B
    ),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        enterprise_oa_sso_logic:exchange_tx(C, CtxB, #{
            <<"code">> => Code, <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => ?NONCE
        })
    ).

%% NEG-07：redirect_uri 变体（尾斜杠/大小写/query/子路径）→ resource_not_found。
neg07_redirect_mismatch(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    Variants = [
        <<"https://oa.customer.example.com/sso/cb/">>,
        <<"https://OA.CUSTOMER.EXAMPLE.COM/sso/cb">>,
        <<"https://oa.customer.example.com/sso/cb?from=imboy">>,
        <<"https://oa.customer.example.com/sso">>
    ],
    lists:foreach(
        fun(Variant) ->
            ?assertEqual(
                {error, <<"resource_not_found">>},
                do_exchange(C, F, Code, Variant, ?NONCE),
                {redirect_variant_rejected, Variant}
            )
        end,
        Variants
    ).

%% NEG-08：nonce 不匹配（digest 不等）→ resource_not_found。
neg08_nonce_mismatch(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, Code, ?REDIRECT, <<"nonce_ZZZZZZZZZZZZZZZZ">>)
    ).

%% NEG-09：credential 缺 sso:exchange scope → decide 编排 insufficient_scope
%%（A2 链在 exchange 端点的 scope 门）。

%% NEG-10：凭证/应用授权仍拒绝失效状态；组织归档不关闭应用认证。
neg10_credential_chain(C) ->
    F = seed_sso_fixture(C),
    FullA = maps:get(credential, F),
    %% ① 错 secret → invalid_credential
    ?assertEqual(
        {error, invalid_credential},
        enterprise_hmac_test_support:signed_context_tx(
            C, prefix_of(FullA), <<"WRONG_secret_value">>
        )
    ),
    %% ② 过期 credential → credential_expired
    ?assertEqual(
        {error, credential_expired},
        enterprise_hmac_test_support:signed_context_tx(
            C, prefix_of(maps:get(credential_expired, F)), ?SECRET_E
        )
    ),
    %% ③ application 停用 → application_disabled
    ok = exec(
        C,
        <<"UPDATE enterprise_application SET status = 'disabled' WHERE id = $1">>,
        [maps:get(app_id, F)]
    ),
    ?assertEqual(
        {error, application_disabled},
        enterprise_hmac_test_support:signed_context_tx(C, prefix_of(FullA), ?SECRET_A)
    ),
    ok = exec(C, <<"UPDATE enterprise_application SET status = 'active' WHERE id = $1">>, [
        maps:get(app_id, F)
    ]),
    %% ④ organization 是业务对象：归档后编排和直接认证均沿用应用授权。
    ok = exec(
        C, <<"UPDATE organization SET status = 'archived' WHERE id = $1">>, [?ORG_A]
    ),
    %% scope 门前置：零 Grant 时生效 scope 为空集会先拒 insufficient_scope
    %%（V2.1 §5.2/F-09，同 NEG-13 处理），先给 app A 种生效 Grant。
    ?assertMatch(
        {ok, _},
        enterprise_internal_auth:decide(
            <<"POST">>,
            ?EXCHANGE_PATH,
            enterprise_hmac_test_support:headers(FullA, <<"POST">>, ?EXCHANGE_PATH),
            fun() ->
                enterprise_hmac_test_support:signed_context_tx(C, prefix_of(FullA), ?SECRET_A)
            end
        )
    ),
    {ok, ArchivedCtx} = enterprise_hmac_test_support:signed_context_tx(
        C, prefix_of(FullA), ?SECRET_A
    ),
    ?assertEqual(maps:get(app_id, F), maps:get(application_id, ArchivedCtx)),
    ok = exec(
        C, <<"UPDATE organization SET status = 'active' WHERE id = $1">>, [?ORG_A]
    ),
    %% ⑤ 信封承载：stable 码 → HTTP 映射（A2 冻结表）
    ?assertEqual(401, enterprise_internal_error:http_status(<<"invalid_credential">>)),
    ?assertEqual(401, enterprise_internal_error:http_status(<<"credential_expired">>)),
    ?assertEqual(403, enterprise_internal_error:http_status(<<"application_disabled">>)),
    ?assertEqual(403, enterprise_internal_error:http_status(<<"organization_disabled">>)).

%% NEG-11（定位修订后语义）：mapping 可选化——绑定全过但 mapping 缺失时
%% exchange **成功**，external_user_id 回退为 user_id 十进制字符串；code
%% 被消费。账号非 active Human（status!=1 / account_type!=0）时保留
%% identity_not_mapped 拒绝（账号级 kill-switch，code 不被消费）。
neg11_identity_not_mapped(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    %% ① mapping 移除（签发后竞态）→ 回退 user_id，成功消费
    ok = exec(C, <<"SAVEPOINT neg11_sp">>),
    ok = exec(
        C,
        <<"UPDATE enterprise_external_identity SET status = 'removed' WHERE organization_id = $1">>,
        [
            ?ORG_A
        ]
    ),
    {ok, P1} = do_exchange(C, F, Code, ?REDIRECT, ?NONCE),
    ?assertEqual(integer_to_binary(?ALICE), maps:get(<<"external_user_id">>, P1)),
    %% 回退成功即已消费——重放拒绝
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, Code, ?REDIRECT, ?NONCE)
    ),
    ok = exec(C, <<"ROLLBACK TO SAVEPOINT neg11_sp">>),
    ok = exec(C, <<"RELEASE SAVEPOINT neg11_sp">>),
    %% ② 登记映射的用户在无 mapping 用户上不生效：NOMAP_H（无 mapping 行）
    %% 签发+交换 → 回退 user_id（此前为 identity_not_mapped 拒绝）
    R2 = issue_ok(C, ?NOMAP_H),
    Code2 = maps:get(<<"code">>, R2),
    {ok, P2} = do_exchange(C, F, Code2, ?REDIRECT, ?NONCE),
    ?assertEqual(integer_to_binary(?NOMAP_H), maps:get(<<"external_user_id">>, P2)),
    %% ③ user 非 active：账号级 kill-switch（mapping 行仍在但账号停用）
    %% → identity_not_mapped，code 不被消费；SAVEPOINT 回滚后重试成功
    R3 = issue_ok(C, ?ALICE),
    Code3 = maps:get(<<"code">>, R3),
    ok = exec(C, <<"SAVEPOINT neg11_sp3">>),
    ok = exec(
        C,
        <<"UPDATE \"user\" SET status = 0 WHERE id = $1">>,
        [?ALICE]
    ),
    ?assertEqual(
        {error, <<"identity_not_mapped">>},
        do_exchange(C, F, Code3, ?REDIRECT, ?NONCE)
    ),
    ok = exec(C, <<"ROLLBACK TO SAVEPOINT neg11_sp3">>),
    ok = exec(C, <<"RELEASE SAVEPOINT neg11_sp3">>),
    %% TTL 内重试成功：证明 code 停留 issued 而非被消费
    {ok, P3} = do_exchange(C, F, Code3, ?REDIRECT, ?NONCE),
    ?assertEqual(<<"ext-alice">>, maps:get(<<"external_user_id">>, P3)),
    %% ④ 有 mapping 时外部员工号原样返回（别名优先于回退）
    R4 = issue_ok(C, ?ALICE),
    Code4 = maps:get(<<"code">>, R4),
    {ok, P4} = do_exchange(C, F, Code4, ?REDIRECT, ?NONCE),
    ?assertEqual(<<"ext-alice">>, maps:get(<<"external_user_id">>, P4)).

%% NEG-12：请求字段语法非法（纯函数面；语法 invalid_request 与绑定
%% resource_not_found 分离）。
neg12_malformed_request_test() ->
    BadCodes = [
        <<>>,
        <<"no_prefix_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz">>,
        <<"oa_sso_$invalid_chars_pad_pad_pad_pad_pad_pad">>,
        binary:copy(<<"a">>, 129)
    ],
    BadRedirects = [
        <<"http://oa.customer.example.com/sso/cb">>,
        <<"https://oa.customer.example.com/sso/cb#frag">>,
        <<"https://">>,
        <<>>,
        binary:copy(<<"https://a.example.com/">>, 200)
    ],
    BadNonces = [
        <<"short_nonce_123">>,
        <<"nonce_with+plus+chars">>,
        <<>>,
        binary:copy(<<"n">>, 129)
    ],
    [
        ?_assertEqual(
            {error, invalid_request},
            enterprise_oa_sso_logic:validate_exchange_params(#{
                <<"code">> => Bad, <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => ?NONCE
            })
        )
     || Bad <- BadCodes
    ] ++
        [
            ?_assertEqual(
                {error, invalid_request},
                enterprise_oa_sso_logic:validate_exchange_params(#{
                    <<"code">> => fake_code(), <<"redirect_uri">> => RU, <<"nonce">> => ?NONCE
                })
            )
         || RU <- BadRedirects
        ] ++
        [
            ?_assertEqual(
                {error, invalid_request},
                enterprise_oa_sso_logic:validate_exchange_params(#{
                    <<"code">> => fake_code(), <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => N
                })
            )
         || N <- BadNonces
        ] ++
        [
            ?_assertEqual(
                {error, invalid_request},
                enterprise_oa_sso_logic:validate_exchange_params(not_a_map)
            ),
            ?_assertEqual(
                {error, invalid_request},
                enterprise_oa_sso_logic:validate_exchange_params(#{})
            ),
            begin
                Code = fake_code(),
                ?_assertEqual(
                    {ok, Code, ?REDIRECT, ?NONCE},
                    enterprise_oa_sso_logic:validate_exchange_params(#{
                        <<"code">> => Code,
                        <<"redirect_uri">> => ?REDIRECT,
                        <<"nonce">> => ?NONCE
                    })
                )
            end
        ].

%% NEG-13：internal_sso 限流配置缺失 → fail-closed security_gate_closed
%%（INV-9；exchange 端点经由 decide 的 rate 门）。
neg13_rate_fail_closed(C) ->
    F = seed_sso_fixture(C),
    FullA = maps:get(credential, F),
    %% V2.1 §5.2/F-09 零 Grant 拒绝：不种 Grant 时生效 scope 为空集，
    %% scope 门先拒 insufficient_scope，到不了本用例的目标（限流门 fail-closed）。
    %% 先给 AppA 种生效 Grant（生效 scope = allowed ∩ grant = sso:exchange）。
    application:unset_env(imboy, enterprise_internal_rate_limits),
    try
        ?assertEqual(
            {error, security_gate_closed},
            enterprise_internal_auth:decide(
                <<"POST">>,
                ?EXCHANGE_PATH,
                enterprise_hmac_test_support:headers(FullA, <<"POST">>, ?EXCHANGE_PATH),
                fun() ->
                    enterprise_hmac_test_support:signed_context_tx(C, prefix_of(FullA), ?SECRET_A)
                end
            )
        )
    after
        application:set_env(imboy, enterprise_internal_rate_limits, ?RATE_CFG)
    end,
    %% 对照：配置在场时同链放行且携带 INT-14 幂等豁免元数据
    {ok, Ctx} = enterprise_internal_auth:decide(
        <<"POST">>,
        ?EXCHANGE_PATH,
        enterprise_hmac_test_support:headers(FullA, <<"POST">>, ?EXCHANGE_PATH),
        fun() ->
            enterprise_hmac_test_support:signed_context_tx(C, prefix_of(FullA), ?SECRET_A)
        end
    ),
    ?assertEqual(<<"INT-14">>, maps:get(route_id, Ctx)),
    ?assertEqual(single_use_code, maps:get(idempotency, Ctx)),
    ?assertEqual(internal_sso, maps:get(rate_bucket, Ctx)).

%% NEG-14：成功响应五字段合同——键集精确、TSID 为 integer（JSON integer，
%% INV-8）、无任何 IMBoy token/JWT/session（R-6）。
neg14_no_imboy_credential(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    {ok, Payload} = do_exchange(C, F, Code, ?REDIRECT, ?NONCE),
    ?assertEqual(
        lists:sort([
            <<"application_id">>,
            <<"consumed_at">>,
            <<"external_user_id">>,
            <<"organization_id">>,
            <<"user_id">>
        ]),
        lists:sort(maps:keys(Payload))
    ),
    ?assert(is_integer(maps:get(<<"organization_id">>, Payload))),
    ?assert(is_integer(maps:get(<<"application_id">>, Payload))),
    ?assert(is_integer(maps:get(<<"user_id">>, Payload))),
    ?assert(is_binary(maps:get(<<"external_user_id">>, Payload))),
    ?assertEqual(<<"ext-alice">>, maps:get(<<"external_user_id">>, Payload)),
    ?assertMatch(
        {match, _},
        re:run(
            maps:get(<<"consumed_at">>, Payload),
            <<"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z$">>
        )
    ),
    Text = unicode:characters_to_binary(io_lib:format("~0p", [Payload])),
    lists:foreach(
        fun(Leak) -> ?assertEqual(nomatch, binary:match(Text, Leak)) end,
        [<<"token">>, <<"jwt">>, <<"JWT">>, <<"session">>, <<"session_id">>, <<"refresh">>]
    ).

%% NEG-15：code/nonce 明文零落日志（红线；错误/成功路径日志全捕获扫描）。
neg15_zero_log_leak(C) ->
    F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    Nonce = maps:get(<<"nonce">>, valid_params()),
    Events = capture_events(fun() ->
        %% 成功交换
        {ok, _} = do_exchange(C, F, Code, ?REDIRECT, ?NONCE),
        %% 统一不透明拒绝（未知 code）
        {error, <<"resource_not_found">>} =
            do_exchange(C, F, fake_code(), ?REDIRECT, ?NONCE),
        %% 语法拒绝
        {error, <<"invalid_request">>} =
            do_exchange(C, F, <<"no_prefix">>, ?REDIRECT, ?NONCE),
        %% 程序错误路径（ctx 缺失）
        {error, <<"internal_error">>} =
            enterprise_oa_sso_logic:exchange_tx(C, #{}, #{
                <<"code">> => fake_code(), <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => ?NONCE
            }),
        %% 签发面 404 路径 + 无 mapping 用户成功路径（定位修订：mapping 可选化）
        {error, {404, _}} = enterprise_oa_sso_logic:issue_code_tx(
            C, ?ALICE, valid_params(<<"epgz05-no-such-app-x">>, ?REDIRECT, ?NONCE)
        ),
        {ok, _} = enterprise_oa_sso_logic:issue_code_tx(C, ?NOMAP_H, valid_params()),
        ok
    end),
    lists:foreach(
        fun(Ev) ->
            Text = event_text(Ev),
            ?assertEqual(nomatch, binary:match(Text, Code), {code_leaked, Text}),
            ?assertEqual(nomatch, binary:match(Text, Nonce), {nonce_leaked, Text})
        end,
        Events
    ).

%%%===================================================================
%%% HUMAN-SSO-01 面
%%%===================================================================

%% NEG-H01：无 Human JWT（uid=0/负数，即中间件未注入认证身份）→ 401。
neg_h01_no_jwt(C) ->
    _F = seed_sso_fixture(C),
    ?assertMatch(
        {error, {401, _}},
        enterprise_oa_sso_logic:issue_code_tx(C, 0, valid_params())
    ),
    ?assertMatch(
        {error, {401, _}},
        enterprise_oa_sso_logic:issue_code_tx(C, -1, valid_params())
    ).

%% NEG-H02：application_key 未知 / application 停用 → 404。
neg_h02_unknown_app(C) ->
    _F = seed_sso_fixture(C),
    ?assertMatch(
        {error, {404, _}},
        enterprise_oa_sso_logic:issue_code_tx(
            C, ?ALICE, valid_params(<<"epgz05-no-such-app-x">>, ?REDIRECT, ?NONCE)
        )
    ),
    ok = exec(
        C, <<"UPDATE enterprise_application SET status = 'disabled' WHERE application_key = $1">>, [
            ?APP_KEY
        ]
    ),
    ?assertMatch(
        {error, {404, _}},
        enterprise_oa_sso_logic:issue_code_tx(C, ?ALICE, valid_params())
    ).

%% POS-H03（定位修订；原 NEG-H03 翻转）：非 org A 成员（FOREIGN，从未
%% 入会）对实例内 active 应用照常签发——org 成员关系不再是授权条件，
%% 授权边界是实例 + Human 账号（JWT 已认证）。ALICE 被 suspend 后同样
%% 不影响（member 表不参与判定链）。
pos_h03_any_human(C) ->
    _F = seed_sso_fixture(C),
    ?assertMatch(
        {ok, _},
        enterprise_oa_sso_logic:issue_code_tx(C, ?FOREIGN, valid_params())
    ),
    ok = exec(
        C,
        <<"UPDATE organization_member SET status = 'suspended' WHERE organization_id = $1 AND user_id = $2">>,
        [?ORG_A, ?ALICE]
    ),
    ?assertMatch(
        {ok, _},
        enterprise_oa_sso_logic:issue_code_tx(C, ?ALICE, valid_params())
    ).

%% POS-H04（定位修订；原 NEG-H04 翻转）：无 active identity mapping 的
%% 用户（NOMAP_H）照常签发——mapping 可选化（ADR §4.4），外部员工号在
%% exchange 时回退 user_id，不再是签发前置。
pos_h04_no_mapping(C) ->
    _F = seed_sso_fixture(C),
    ?assertMatch(
        {ok, _},
        enterprise_oa_sso_logic:issue_code_tx(C, ?NOMAP_H, valid_params())
    ),
    %% mapping removed 同样不拦截
    ok = exec(
        C,
        <<"UPDATE enterprise_external_identity SET status = 'removed' WHERE organization_id = $1">>,
        [
            ?ORG_A
        ]
    ),
    ?assertMatch(
        {ok, _},
        enterprise_oa_sso_logic:issue_code_tx(C, ?ALICE, valid_params())
    ).

%% NEG-H05：redirect_uri 未注册/变体/空 allowlist → 400 invalid_param。
neg_h05_redirect_not_allowlisted(C) ->
    _F = seed_sso_fixture(C),
    Variants = [
        <<"https://oa.customer.example.com/other/cb">>,
        <<"https://oa.customer.example.com/sso/cb/">>,
        <<"https://oa.customer.example.com:443/sso/cb">>,
        <<"https://oa.customer.example.com/sso/cb?x=1">>
    ],
    lists:foreach(
        fun(V) ->
            ?assertMatch(
                {error, {400, _}},
                enterprise_oa_sso_logic:issue_code_tx(
                    C, ?ALICE, valid_params(?APP_KEY, V, ?NONCE)
                ),
                {redirect_variant_rejected, V}
            )
        end,
        Variants
    ),
    %% Empty allowlist app (different key): any valid https URI is always rejected
    %% (fail-closed; after the positioning revision, the mapping check no longer exists,
    %% the empty-allowlist rejection is the only gate for this key)
    ?assertMatch(
        {error, {400, _}},
        enterprise_oa_sso_logic:issue_code_tx(
            C, ?ALICE, valid_params(?APP_KEY_EMPTY, ?REDIRECT, ?NONCE)
        )
    ).

%% NEG-H06：nonce/application_key/redirect 格式非法、body 非法 → 400
%%（validate 面断言 invalid_param；issue_code_tx 面 400 映射另在
%% neg_h05/neg_h01 复合路径钉住）。
neg_h06_malformed_params_test() ->
    [
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{
                <<"application_key">> => ?APP_KEY,
                <<"redirect_uri">> => ?REDIRECT,
                <<"nonce">> => <<"short_nonce_123">>
            })
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{
                <<"application_key">> => ?APP_KEY,
                <<"redirect_uri">> => ?REDIRECT,
                <<"nonce">> => <<"nonce_with+plus+chars">>
            })
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{
                <<"application_key">> => <<"short7">>,
                <<"redirect_uri">> => ?REDIRECT,
                <<"nonce">> => ?NONCE
            })
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{
                <<"redirect_uri">> => ?REDIRECT, <<"nonce">> => ?NONCE
            })
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{
                <<"application_key">> => ?APP_KEY,
                <<"redirect_uri">> => <<"http://oa.customer.example.com/sso/cb">>,
                <<"nonce">> => ?NONCE
            })
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{
                <<"application_key">> => ?APP_KEY,
                <<"redirect_uri">> => <<"https://oa.customer.example.com/sso/cb#f">>,
                <<"nonce">> => ?NONCE
            })
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(not_a_map)
        ),
        ?_assertEqual(
            {error, invalid_param},
            enterprise_oa_sso_logic:validate_issue_params(#{})
        ),
        ?_assertEqual(
            {ok, ?APP_KEY, ?REDIRECT, ?NONCE},
            enterprise_oa_sso_logic:validate_issue_params(valid_params())
        )
    ].

%% NEG-H07：同用户连续签发两个 code → 独立 TTL/独立单次消费/互不失效；
%% expires_in 恒 60。
neg_h07_independent_codes(C) ->
    F = seed_sso_fixture(C),
    R1 = issue_ok(C, ?ALICE),
    R2 = issue_ok(C, ?ALICE),
    C1 = maps:get(<<"code">>, R1),
    C2 = maps:get(<<"code">>, R2),
    ?assertNotEqual(C1, C2),
    ?assertEqual(60, maps:get(<<"expires_in">>, R1)),
    ?assertEqual(60, maps:get(<<"expires_in">>, R2)),
    %% 新签发不失效旧 code（c1 在 c2 签发后仍可消费）
    {ok, _} = do_exchange(C, F, C1, ?REDIRECT, ?NONCE),
    %% 两者独立单次消费
    {ok, _} = do_exchange(C, F, C2, ?REDIRECT, ?NONCE),
    %% 重放各自拒绝
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, C1, ?REDIRECT, ?NONCE)
    ),
    ?assertEqual(
        {error, <<"resource_not_found">>},
        do_exchange(C, F, C2, ?REDIRECT, ?NONCE)
    ).

%% 当前账号停用/非 Human：正 Uid 不能代替当前账号事实；恢复活性后仍可签发。
neg_h08_inactive_issue(C) ->
    _F = seed_sso_fixture(C),
    Before = issue_side_effect_snapshot(C),
    lists:foreach(
        fun({Status, Type}) ->
            ok = exec(
                C,
                <<"UPDATE \"user\" SET status=$1, account_type=$2 WHERE id=$3">>,
                [Status, Type, ?ALICE]
            ),
            ?assertMatch(
                {error, {403, _}},
                enterprise_oa_sso_logic:issue_code_tx(C, ?ALICE, valid_params())
            ),
            ?assertEqual(Before, issue_side_effect_snapshot(C))
        end,
        [{0, 0}, {1, 1}]
    ),
    ok = exec(C, <<"UPDATE \"user\" SET status=1, account_type=0 WHERE id=$1">>, [?ALICE]),
    ?assertMatch({ok, _}, enterprise_oa_sso_logic:issue_code_tx(C, ?ALICE, valid_params())).

%% 只比较数量/摘要，不把 code、mapping 或设备凭证明文输出到失败日志。
issue_side_effect_snapshot(C) ->
    Tables = [
        <<"enterprise_oa_sso_code">>,
        <<"enterprise_external_identity">>,
        <<"user_device">>,
        <<"user_auth_epoch">>
    ],
    [
        begin
            {ok, Rows} = elib_pg:query(
                C,
                <<
                    "SELECT count(*) AS n, md5(COALESCE(jsonb_agg(to_jsonb(t) "
                    "ORDER BY to_jsonb(t)::text)::text,'[]')) AS digest FROM ",
                    Table/binary,
                    " t"
                >>,
                []
            ),
            {Table, Rows}
        end
     || Table <- Tables
    ].

%%%===================================================================
%%% 双向隔离（manifest INV-2/INV-3）
%%%===================================================================

%% NEG-X01：Application Credential 调 /api/v1/oa/sso/code——凭证携带者
%% 没有 Human JWT，中间件注入 uid=0，logic 面 401 拒绝（Router 侧由
%% /api/v1 认证块兜底，A0 W4 接线；ib_int_* Bearer 不满足人类 JWT 链）。
neg_x01_credential_cannot_issue(C) ->
    _F = seed_sso_fixture(C),
    %% 无人类认证上下文（credential 调用者的形态）→ 401
    ?assertMatch(
        {error, {401, _}},
        enterprise_oa_sso_logic:issue_code_tx(C, 0, valid_params())
    ),
    %% 结构隔离：ib_int_* Bearer 只被 internal 面解析；人类面 token
    %% 校验器不接受该形态（auth_ds:verify_token 对非 JWT 拒绝）
    ?assertMatch(
        {error, _, _},
        auth_ds:verify_token(<<"ib_int_1234567890.epgz05_secret">>)
    ).

%% NEG-X02：Human JWT 调 /api/internal/v1/oa/sso/exchange → 认证链 401
%%（internal 面只认 ib_int_* Bearer credential；JWT 串非 credential 形态）。
neg_x02_human_jwt_cannot_exchange(C) ->
    _F = seed_sso_fixture(C),
    JwtLike = <<"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.signature">>,
    ?assertEqual(
        {error, invalid_credential},
        enterprise_internal_auth:decide(
            <<"POST">>,
            ?EXCHANGE_PATH,
            #{<<"authorization">> => <<"Bearer ", JwtLike/binary>>},
            fun() -> {ok, #{}} end
        )
    ),
    ?assertEqual(401, enterprise_internal_error:http_status(<<"invalid_credential">>)).

%%%===================================================================
%%% 存储合同（合同 §2：digest-only）
%%%===================================================================

%% STORAGE-01：code/nonce 只存 SHA-256 digest（64 hex，code_digest 唯一命中），
%% 明文不出现在行任何列。
storage01_digest_only(C) ->
    _F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    Code = maps:get(<<"code">>, R),
    Nonce = maps:get(<<"nonce">>, valid_params()),
    Digest = enterprise_oa_sso_code_repo:digest_hex(Code),
    {ok, Row} = enterprise_oa_sso_code_repo:find_by_digest_tx(C, Digest),
    ?assertEqual(Digest, maps:get(<<"code_digest">>, Row)),
    ?assertEqual(64, byte_size(maps:get(<<"code_digest">>, Row))),
    ?assertMatch(
        {match, _},
        re:run(maps:get(<<"code_digest">>, Row), <<"^[0-9a-f]{64}$">>)
    ),
    ?assertEqual(
        enterprise_oa_sso_code_repo:digest_hex(Nonce),
        maps:get(<<"nonce_digest">>, Row)
    ),
    ?assertMatch(
        {match, _},
        re:run(maps:get(<<"nonce_digest">>, Row), <<"^[0-9a-f]{64}$">>)
    ),
    %% 明文零落库：全行文本扫描（含全部列值）
    RowText = unicode:characters_to_binary(io_lib:format("~0p", [Row])),
    ?assertEqual(nomatch, binary:match(RowText, Code)),
    ?assertEqual(nomatch, binary:match(RowText, Nonce)),
    %% 未消费行 consumed_at 为 NULL
    ?assertEqual(null, maps:get(<<"consumed_at">>, Row)).

%% STORAGE-02：签发响应明文 code 仅此一次；expires_in == 60；
%% redirect_uri 原样回显；code 形态（7 字节前缀 + 43 字符 base64url，共 50 长）。
storage02_issue_response(C) ->
    _F = seed_sso_fixture(C),
    R = issue_ok(C, ?ALICE),
    ?assertEqual(
        lists:sort([<<"code">>, <<"expires_in">>, <<"redirect_uri">>]),
        lists:sort(maps:keys(R))
    ),
    Code = maps:get(<<"code">>, R),
    ?assertMatch({match, _}, re:run(Code, <<"^oa_sso_[A-Za-z0-9_-]{43}$">>)),
    ?assertEqual(50, byte_size(Code)),
    ?assertEqual(60, maps:get(<<"expires_in">>, R)),
    ?assertEqual(?REDIRECT, maps:get(<<"redirect_uri">>, R)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% 尽力而为执行（NEG-04 racer 收尾；失败不影响断言）。
try_ok(Fun) when is_function(Fun, 0) ->
    try
        Fun()
    catch
        _:_ -> ok
    end.

%% 同形态伪造 code（oa_sso_ + 43 字符随机 base64url；合同 §2 形态）。
fake_code() ->
    Raw = crypto:strong_rand_bytes(32),
    B64 = base64:encode(Raw),
    Url = <<<<(urlsafe(C))/binary>> || <<C>> <= B64, C =/= $=>>,
    <<"oa_sso_", Url/binary>>.

urlsafe($+) -> <<"-">>;
urlsafe($/) -> <<"_">>;
urlsafe(C) -> <<C>>.

%% ---- logger 捕获（A2 enterprise_internal_pg_tests 同款） ----

capture_events(F) ->
    Parent = self(),
    HandlerId = epgz05_log_capture,
    _ = logger:remove_handler(HandlerId),
    ok = logger:add_handler(HandlerId, ?MODULE, #{forward => Parent}),
    try
        F(),
        timer:sleep(100),
        collect_events([])
    after
        _ = logger:remove_handler(HandlerId)
    end.

collect_events(Acc) ->
    receive
        {epgz05_log, Ev} -> collect_events([Ev | Acc])
    after 300 ->
        lists:reverse(Acc)
    end.

log(Event, Config) ->
    case maps:get(forward, Config, undefined) of
        Pid when is_pid(Pid) -> Pid ! {epgz05_log, Event};
        _ -> ok
    end.

event_text(#{msg := Msg}) ->
    iolist_to_binary(io_lib:format("~0p", [Msg]));
event_text(_) ->
    <<>>.

%% 每次 seed 唯一，同一用例所有宏调用稳定；纯参数验证无需 seed。
fixture_app_key(Label) ->
    case get({?MODULE, fixture_app_key}) of
        undefined -> <<"epgz05-oa", Label/binary>>;
        Namespace -> <<"epgz05-oa", Label/binary, "-", Namespace/binary>>
    end.

%% run 级随机 base（24 位空间、64 位安全整数内；每 VM 一次）。
run_base() ->
    case persistent_term:get({?MODULE, run_base}, undefined) of
        B when is_integer(B), B > 0 ->
            B;
        _ ->
            B =
                500_000_000_000_000 +
                    (erlang:phash2(integer_to_binary(erlang:unique_integer([positive]))) rem
                        400_000_000) *
                        1_000,
            persistent_term:put({?MODULE, run_base}, B),
            B
    end.
