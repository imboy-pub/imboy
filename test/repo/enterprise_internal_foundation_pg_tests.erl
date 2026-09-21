%% enterprise_internal_foundation_pg_tests
%% EPGZ-01 — Enterprise Internal API 基础五表（迁移 00000136）真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 EPGZ01_INTTEST）：
%%   空库全量迁移 up（erlang_migrate strict，含 00000136）→ 业务 oracle
%%   → down 00000136 → 再次 up 00000136。
%% 业务用例每条 BEGIN ... ROLLBACK，不留数据；down/up 循环用独立连接，
%% 排在最后（其后表已重建但为空）。
%%
%% oracle 覆盖（plan-gz §5 约束逐条 + EPGZ-01R 修订）：
%%   ⓪ 空库全量 up 至版本 136
%%   ① enterprise_application：CRUD、(org,application_key) 唯一拒绝、跨 Org 同 key 放行
%%   ①b redirect URI allowlist（EPGZ-01R / EPGZ-05 硬需求）：写读往返、整体
%%      替换/清空、默认空、非法元素（http/非 https scheme/无 host/空串/
%%      fragment/重复/超 20 个）触发器 23514 拒绝、exact match 语义
%%      （= ANY 逐字节：尾斜杠/查询参数变体不相等）
%%   ② credential：digest/prefix 落库、按 prefix 查找、全局 prefix 唯一拒绝、
%%      复合 org 约束（跨 Org application 引用 23503 拒绝）、revoke 幂等
%%   ③ identity mapping：bind/resolve/unbind、双向唯一拒绝、
%%      非 member / 非 Human / suspended member 的 bind 一律 23514 拒绝
%%   ④ idempotency：record 首插/重放/同键异 body、claim 单次、过期标记
%%   ⑤ sso code：issue/consume 单次原子、重放 already_consumed、过期 expired、
%%      redirect_uri 不匹配拒绝、非 https redirect CHECK 拒绝
%%   ⑤b push_token platform 值域（EPGZ-01R / EPGZ-07 硬需求）：'jpush' 可插入、
%%      fcm/apns/web_push 回归放行、非法值（含大小写变体）仍 23514 拒绝
%%   ⑥ migration 循环：down 136 五表+两守卫函数全消、push platform 值域恢复
%%      原定义（jpush 拒/fcm 放行）→ up 136 全部重建 → 版本 136→135→136
%%      → 重建后 oracle 复验（redirect 守卫 + jpush 放行）
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% EPGZ-01R exact match 语义说明：text[] 元素级"逐字节相等"由 = ANY 谓词在
%% DB 层即成立（本测试 ①b 直接断言）；"尾斜杠/查询参数不算相等"无需应用
%% 层参与——变体字符串不是同一数组元素，= ANY 必为 false。SSO 签发/交换
%% 侧的 fail-closed 调用顺序由 A5（EPGZ-05 W2）负责。

-module(enterprise_internal_foundation_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% ---- 夹具（987 段独立 ID，与 moya 97/98 段互不冲突；每次运行独立 marker 库） ----

-define(OWNER_A, 987001).
-define(OWNER_B, 987002).
-define(HUMAN_A, 987011).
-define(HUMAN_B, 987012).
%% account_type=1 的平台 AI（member 但非 Human）
-define(AGENT_USER, 987013).
-define(SUSPENDED_USER, 987014).
%% 非 member
-define(OUTSIDER, 987015).

-define(ORG_A, 987101).
-define(ORG_B, 987102).

-define(TABLES, [
    <<"enterprise_application">>,
    <<"enterprise_application_credential">>,
    <<"enterprise_external_identity">>,
    <<"enterprise_internal_idempotency">>,
    <<"enterprise_oa_sso_code">>
]).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    %% 直连模式未起 imboy 应用：本测试仅需 default TSID 生成器
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    %% 一次性 marker 库（inttest_marker_db 配方）：env 覆盖（<= imboy.pg_conf
    %% 回退）→ 建库 → 12 扩展 → erlang_migrate:up 全链（空库全量 up 即用例 ⓪）；
    %% 任一失败显式 error。
    inttest_marker_db:provision(#{
        env_prefix => <<"EPGZ01_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

%% down/up 循环用独立连接（erlang_migrate 需要事务自治连接）。
connect_marker(State) ->
    #{host := Host, port := Port, username := User, password := Pass} = maps:get(server, State),
    {ok, Conn} =
        epgsql:connect(#{
            host => Host,
            port => Port,
            username => User,
            password => Pass,
            database => maps:get(db, State),
            timeout => 10000,
            codecs => [{epgsql_codec_rfc3339_bin, []}]
        }),
    Conn.

%% 每条业务 oracle 用例 BEGIN ... ROLLBACK，不留数据。
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

%% 负例包装：预期 SQL 错误（23505/23503/23514 等）会 abort 事务，后续语句将
%% 25P02；在 SAVEPOINT 内执行并回退到保存点，使同一事务内可连续多个负例。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT epgz01_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT epgz01_sp">>),
        exec(C, <<"RELEASE SAVEPOINT epgz01_sp">>)
    end.

foundation_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            [
                {"empty_db_full_up_reaches_136", empty_db_full_up_test(C)},
                {"application_crud_and_org_scoped_key_unique",
                    with_tx(C, fun application_oracle/1)},
                {"application_redirect_uri_allowlist_domain",
                    with_tx(C, fun application_redirect_oracle/1)},
                {"credential_digest_prefix_and_org_boundary", with_tx(C, fun credential_oracle/1)},
                {"identity_mapping_bind_resolve_guards", with_tx(C, fun identity_oracle/1)},
                {"idempotency_record_claim_expiry", with_tx(C, fun idempotency_oracle/1)},
                {"sso_code_issue_consume_single_use", with_tx(C, fun sso_code_oracle/1)},
                {"push_token_platform_jpush_domain", with_tx(C, fun push_token_platform_oracle/1)},
                {"migration_136_down_then_up_cycle", {timeout, 300, migration_cycle_test(State)}}
            ]
        end}}.

%%%===================================================================
%%% ⓪ 空库全量迁移 up
%%%===================================================================

empty_db_full_up_test(C) ->
    ?_test(begin
        {ok, Version, Dirty} = erlang_migrate:version(#{conn => C, dir => "priv/migrations"}),
        ?assertEqual(false, Dirty),
        ?assertEqual(136, Version),
        %% 五表全部就位
        lists:foreach(
            fun(T) -> ?assertNot(table_missing(C, T), {table_missing, T}) end,
            ?TABLES
        )
    end).

%%%===================================================================
%%% ① enterprise_application
%%%===================================================================

application_oracle(C) ->
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    ok = seed_org(C, ?ORG_B, ?OWNER_B),
    %% 创建 + 查询
    {ok, AppA} = enterprise_application_repo:create_tx(
        C,
        ?ORG_A,
        <<"oa-gz-a">>,
        <<"gz_customer_oa"/utf8>>,
        {?OWNER_A, [<<"application:read">>, <<"messages:send">>]}
    ),
    AppAId = maps:get(<<"id">>, AppA),
    ?assertEqual(?ORG_A, maps:get(<<"organization_id">>, AppA)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, AppA)),
    ?assertEqual(?OWNER_A, maps:get(<<"principal_user_id">>, AppA)),
    ?assertEqual(
        [<<"application:read">>, <<"messages:send">>],
        jsone:decode(maps:get(<<"allowed_scopes">>, AppA))
    ),
    {ok, Found} = enterprise_application_repo:find_tx(C, ?ORG_A, AppAId),
    ?assertEqual(<<"gz_customer_oa"/utf8>>, maps:get(<<"name">>, Found)),
    %% Org 边界在 SQL 内强制：跨 Org 查询同一 id → not_found
    ?assertEqual({error, not_found}, enterprise_application_repo:find_tx(C, ?ORG_B, AppAId)),
    %% 按 key 查找
    {ok, _} = enterprise_application_repo:find_by_key_tx(C, ?ORG_A, <<"oa-gz-a">>),
    ?assertEqual(
        {error, not_found}, enterprise_application_repo:find_by_key_tx(C, ?ORG_B, <<"oa-gz-a">>)
    ),
    %% 同 Org 同 key 唯一拒绝
    ?assertEqual(
        {error, key_conflict},
        in_savepoint(C, fun() ->
            enterprise_application_repo:create_tx(C, ?ORG_A, <<"oa-gz-a">>, <<"dup_key"/utf8>>)
        end)
    ),
    %% 跨 Org 同 key 放行（唯一约束是 (org, key)）
    {ok, AppB} = enterprise_application_repo:create_tx(
        C, ?ORG_B, <<"oa-gz-a">>, <<"org_b_same_key"/utf8>>
    ),
    ?assertEqual(?ORG_B, maps:get(<<"organization_id">>, AppB)),
    %% 更新：status / name / scopes
    ok = enterprise_application_repo:update_status_tx(C, ?ORG_A, AppAId, <<"disabled">>),
    {ok, Disabled} = enterprise_application_repo:find_tx(C, ?ORG_A, AppAId),
    ?assertEqual(<<"disabled">>, maps:get(<<"status">>, Disabled)),
    ok = enterprise_application_repo:update_status_tx(C, ?ORG_A, AppAId, <<"active">>),
    ok = enterprise_application_repo:update_name_tx(C, ?ORG_A, AppAId, <<"renamed"/utf8>>),
    ok = enterprise_application_repo:update_scopes_tx(
        C, ?ORG_A, AppAId, [<<"application:read">>, <<"sso:exchange">>]
    ),
    {ok, Updated} = enterprise_application_repo:find_tx(C, ?ORG_A, AppAId),
    ?assertEqual(
        [<<"application:read">>, <<"sso:exchange">>],
        jsone:decode(maps:get(<<"allowed_scopes">>, Updated))
    ),
    ?assertEqual(<<"renamed"/utf8>>, maps:get(<<"name">>, Updated)),
    %% 非法 scopes（非数组 jsonb）被 DB CHECK 拒绝
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        in_savepoint(C, fun() ->
            enterprise_application_repo:update_scopes_tx(
                C, ?ORG_A, AppAId, <<"{\"not\":\"array\"}">>
            )
        end)
    ).

%%%===================================================================
%%% ①b redirect URI allowlist（EPGZ-01R / EPGZ-05 硬需求）
%%%===================================================================

application_redirect_oracle(C) ->
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    Uris = [
        <<"https://oa.customer.example.com/sso/cb">>,
        <<"https://oa2.customer.example.com/callback">>
    ],
    %% 创建带 allowlist + 写读往返（RETURNING / find / find_by_key 三口径一致）
    {ok, App} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"oa-gz-redir">>, <<"redir_app"/utf8>>, {?OWNER_A, []}, Uris
    ),
    AppId = maps:get(<<"id">>, App),
    ?assertEqual(Uris, maps:get(<<"allowed_redirect_uris">>, App)),
    {ok, Found} = enterprise_application_repo:find_tx(C, ?ORG_A, AppId),
    ?assertEqual(Uris, maps:get(<<"allowed_redirect_uris">>, Found)),
    {ok, FoundKey} = enterprise_application_repo:find_by_key_tx(C, ?ORG_A, <<"oa-gz-redir">>),
    ?assertEqual(Uris, maps:get(<<"allowed_redirect_uris">>, FoundKey)),
    %% create_tx/4 默认空 allowlist（空表 = SSO 一律拒绝 fail-closed）
    {ok, Plain} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"oa-gz-plain">>, <<"plain"/utf8>>
    ),
    ?assertEqual([], maps:get(<<"allowed_redirect_uris">>, Plain)),
    %% 整体替换 + 清空
    Uris2 = [<<"https://new.customer.example.com/cb">>],
    ok = enterprise_application_repo:update_redirect_uris_tx(C, ?ORG_A, AppId, Uris2),
    {ok, Updated} = enterprise_application_repo:find_tx(C, ?ORG_A, AppId),
    ?assertEqual(Uris2, maps:get(<<"allowed_redirect_uris">>, Updated)),
    ok = enterprise_application_repo:update_redirect_uris_tx(C, ?ORG_A, AppId, []),
    {ok, Cleared} = enterprise_application_repo:find_tx(C, ?ORG_A, AppId),
    ?assertEqual([], maps:get(<<"allowed_redirect_uris">>, Cleared)),
    %% 不存在的 application
    ?assertEqual(
        {error, not_found},
        enterprise_application_repo:update_redirect_uris_tx(
            C, ?ORG_A, AppId + 999999, Uris2
        )
    ),
    %% ---- 存储层元素校验：非法元素一律触发器 23514 拒绝 ----
    BadUriCases = [
        {<<"http://oa.customer.example.com/cb">>, <<"http_scheme">>},
        {<<"ftp://oa.customer.example.com/cb">>, <<"non_http_scheme">>},
        {<<"https://">>, <<"no_host">>},
        {<<"https:///cb">>, <<"empty_host">>},
        {<<>>, <<"empty_string">>},
        {<<"https://oa.customer.example.com/cb#frag">>, <<"fragment">>},
        {<<"https://oa.customer.example.com/cb?x=1#f">>, <<"query_then_fragment">>}
    ],
    lists:foreach(
        fun({BadUri, Label}) ->
            ?assertMatch(
                {error, #error{code = <<"23514">>}},
                in_savepoint(C, fun() ->
                    enterprise_application_repo:create_tx(
                        C,
                        ?ORG_A,
                        <<"oa-gz-bad-", Label/binary>>,
                        <<"bad"/utf8>>,
                        undefined,
                        [BadUri]
                    )
                end)
            ),
            %% update 路径同样被守卫
            ?assertMatch(
                {error, #error{code = <<"23514">>}},
                in_savepoint(C, fun() ->
                    enterprise_application_repo:update_redirect_uris_tx(C, ?ORG_A, AppId, [BadUri])
                end)
            )
        end,
        BadUriCases
    ),
    %% 合法带查询参数的 URI 可注册（exact match 消费时逐字节比对，见下）
    ok = enterprise_application_repo:update_redirect_uris_tx(
        C, ?ORG_A, AppId, [<<"https://oa.customer.example.com/cb?tenant=gz">>]
    ),
    %% 重复元素拒绝
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        in_savepoint(C, fun() ->
            enterprise_application_repo:update_redirect_uris_tx(
                C,
                ?ORG_A,
                AppId,
                [<<"https://oa.customer.example.com/cb">>, <<"https://oa.customer.example.com/cb">>]
            )
        end)
    ),
    %% 超 20 个元素拒绝
    TooMany = [
        <<"https://oa.customer.example.com/cb/", (integer_to_binary(N))/binary>>
     || N <- lists:seq(1, 21)
    ],
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        in_savepoint(C, fun() ->
            enterprise_application_repo:update_redirect_uris_tx(C, ?ORG_A, AppId, TooMany)
        end)
    ),
    %% 恰好 20 个放行（边界）
    Exactly20 = lists:sublist(TooMany, 20),
    ok = enterprise_application_repo:update_redirect_uris_tx(C, ?ORG_A, AppId, Exactly20),
    %% ---- exact match 语义（DB 层 = ANY 逐字节比较，A5 消费谓词同型）----
    %% 尾斜杠变体 / 查询参数变体都不是同一元素：不匹配（不算相等）
    ?assert(bool(C, allow_match_sql(<<"https://oa.customer.example.com/cb/1">>))),
    ?assertNot(bool(C, allow_match_sql(<<"https://oa.customer.example.com/cb/1/">>))),
    ?assertNot(bool(C, allow_match_sql(<<"https://oa.customer.example.com/cb/1?x=1">>))),
    ?assertNot(bool(C, allow_match_sql(<<"https://oa.customer.example.com:443/cb/1">>))),
    %% allowlist 未注册的 URI 不匹配（跨 app/org 隔离由行唯一性天然保证）
    ?assertNot(bool(C, allow_match_sql(<<"https://evil.example.com/cb/1">>))),
    ok.

%%%===================================================================
%%% ② enterprise_application_credential
%%%===================================================================

credential_oracle(C) ->
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    ok = seed_org(C, ?ORG_B, ?OWNER_B),
    {ok, AppA} = enterprise_application_repo:create_tx(C, ?ORG_A, <<"oa-gz-a">>, <<"a_org"/utf8>>),
    {ok, AppB} = enterprise_application_repo:create_tx(C, ?ORG_B, <<"oa-b">>, <<"b_org"/utf8>>),
    AppAId = maps:get(<<"id">>, AppA),
    Secret = <<"high_entropy_secret_0123456789abcdef">>,
    Prefix = <<"ib_int_cred_a1">>,
    %% 创建：digest 落库且非明文
    {ok, Cred} = enterprise_application_credential_repo:create_tx(
        C, ?ORG_A, AppAId, Prefix, Secret
    ),
    CredId = maps:get(<<"id">>, Cred),
    ?assertEqual(64, byte_size(maps:get(<<"secret_digest">>, Cred))),
    ?assertEqual(
        enterprise_application_credential_repo:digest_hex(Secret),
        maps:get(<<"secret_digest">>, Cred)
    ),
    ?assertNotEqual(Secret, maps:get(<<"secret_digest">>, Cred)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Cred)),
    %% 按 prefix 查找（全局定位键；marker 库直连模式只用 _tx 变体）
    {ok, Found} = enterprise_application_credential_repo:find_by_prefix_tx(C, Prefix),
    ?assertEqual(CredId, maps:get(<<"id">>, Found)),
    {ok, ActiveFound} = enterprise_application_credential_repo:find_active_by_prefix_tx(C, Prefix),
    ?assertEqual(false, maps:get(<<"expired">>, ActiveFound)),
    %% prefix 全局唯一拒绝
    ?assertEqual(
        {error, prefix_conflict},
        in_savepoint(C, fun() ->
            enterprise_application_credential_repo:create_tx(
                C, ?ORG_A, AppAId, Prefix, <<"another_secret">>
            )
        end)
    ),
    %% 复合 org 约束：ORG_B + AppA（A 机构的 application）→ 23503 拒绝
    ?assertMatch(
        {error, #error{code = <<"23503">>}},
        in_savepoint(C, fun() ->
            enterprise_application_credential_repo:create_tx(
                C, ?ORG_B, AppAId, <<"ib_int_cred_cross">>, Secret
            )
        end)
    ),
    %% 空 secret 前置拒绝（不产生 DB 写入）
    ?assertEqual(
        {error, invalid_secret},
        enterprise_application_credential_repo:create_tx(
            C, ?ORG_A, AppAId, <<"ib_int_empty">>, <<>>
        )
    ),
    %% 带 expires_at 的创建 + revoked 幂等
    Future = <<"2035-01-01T00:00:00+00:00">>,
    {ok, Cred2} = enterprise_application_credential_repo:create_tx(
        C, ?ORG_A, AppAId, <<"ib_int_cred_a2">>, Secret, Future
    ),
    ok = enterprise_application_credential_repo:revoke_tx(C, ?ORG_A, maps:get(<<"id">>, Cred2)),
    ?assertEqual(
        {error, not_active},
        enterprise_application_credential_repo:revoke_tx(C, ?ORG_A, maps:get(<<"id">>, Cred2))
    ),
    %% 吊销后 active 查找不再命中
    ?assertEqual(
        {error, not_found},
        enterprise_application_credential_repo:find_active_by_prefix_tx(C, <<"ib_int_cred_a2">>)
    ),
    %% 已过期的 active 凭证：active 查找命中但 expired = true
    Past = <<"2020-01-01T00:00:00+00:00">>,
    {ok, _Cred3} = enterprise_application_credential_repo:create_tx(
        C, ?ORG_A, AppAId, <<"ib_int_cred_a3">>, Secret, Past
    ),
    {ok, ExpiredFound} = enterprise_application_credential_repo:find_active_by_prefix_tx(
        C, <<"ib_int_cred_a3">>
    ),
    ?assertEqual(true, maps:get(<<"expired">>, ExpiredFound)),
    %% last_used 记录（active 凭证命中；已吊销凭证 not_found）
    ok = enterprise_application_credential_repo:touch_last_used_tx(C, CredId),
    ?assertEqual(
        {error, not_found},
        enterprise_application_credential_repo:touch_last_used_tx(C, maps:get(<<"id">>, Cred2))
    ),
    %% 不存在的 application 归属（AppB 属于 ORG_B，ORG_A 引用它）→ 23503
    ?assertMatch(
        {error, #error{code = <<"23503">>}},
        in_savepoint(C, fun() ->
            enterprise_application_credential_repo:create_tx(
                C, ?ORG_A, maps:get(<<"id">>, AppB), <<"ib_int_cred_orphan">>, Secret
            )
        end)
    ).

%%%===================================================================
%%% ③ enterprise_external_identity
%%%===================================================================

identity_oracle(C) ->
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    ok = seed_org(C, ?ORG_B, ?OWNER_B),
    ok = seed_org_members(C, ?ORG_A, [?HUMAN_A, ?HUMAN_B, ?AGENT_USER, ?SUSPENDED_USER]),
    %% AGENT_USER 是 account_type=1 的平台 AI 用户（active member 但非 Human）
    exec(C, [<<"UPDATE \"user\" SET account_type = 1 WHERE id = ">>, integer_to_binary(?AGENT_USER)]),
    %% SUSPENDED_USER 置为 suspended（114 扩展态：member 存在但非 active）
    exec(C, [
        <<"UPDATE organization_member SET status = 'suspended' WHERE organization_id = ">>,
        integer_to_binary(?ORG_A),
        <<" AND user_id = ">>,
        integer_to_binary(?SUSPENDED_USER)
    ]),
    {ok, AppA} = enterprise_application_repo:create_tx(C, ?ORG_A, <<"oa-gz-a">>, <<"a_org"/utf8>>),
    AppAId = maps:get(<<"id">>, AppA),
    %% bind 成功（active Human member）
    {ok, M1} = enterprise_external_identity_repo:bind_tx(
        C, ?ORG_A, AppAId, <<"oa-emp-001">>, ?HUMAN_A
    ),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, M1)),
    ?assertEqual(?HUMAN_A, maps:get(<<"user_id">>, M1)),
    %% 非 member（OUTSIDER）→ 拒绝（触发器 23514）
    ?assertEqual(
        {error, invalid_member},
        in_savepoint(C, fun() ->
            enterprise_external_identity_repo:bind_tx(C, ?ORG_A, AppAId, <<"oa-emp-x">>, ?OUTSIDER)
        end)
    ),
    %% 非 Human（AGENT_USER，active member，account_type=1）→ 拒绝
    ?assertEqual(
        {error, invalid_member},
        in_savepoint(C, fun() ->
            enterprise_external_identity_repo:bind_tx(
                C, ?ORG_A, AppAId, <<"oa-emp-agent">>, ?AGENT_USER
            )
        end)
    ),
    %% suspended member → 拒绝（仅 active 成员可绑定）
    ?assertEqual(
        {error, invalid_member},
        in_savepoint(C, fun() ->
            enterprise_external_identity_repo:bind_tx(
                C, ?ORG_A, AppAId, <<"oa-emp-susp">>, ?SUSPENDED_USER
            )
        end)
    ),
    %% 已停用 user（status=-1，正常 member）→ 拒绝
    exec(C, [<<"UPDATE \"user\" SET status = -1 WHERE id = ">>, integer_to_binary(?HUMAN_B)]),
    ?assertEqual(
        {error, invalid_member},
        in_savepoint(C, fun() ->
            enterprise_external_identity_repo:bind_tx(C, ?ORG_A, AppAId, <<"oa-emp-b">>, ?HUMAN_B)
        end)
    ),
    exec(C, [<<"UPDATE \"user\" SET status = 1 WHERE id = ">>, integer_to_binary(?HUMAN_B)]),
    %% 复合 org 约束：ORG_B member（触发器通过）+ AppA（ORG_A 的 application）
    %% → 复合 FK (org, app) 23503 拒绝
    ?assertMatch(
        {error, #error{code = <<"23503">>}},
        in_savepoint(C, fun() ->
            enterprise_external_identity_repo:bind_tx(
                C, ?ORG_B, AppAId, <<"oa-emp-cross">>, ?OWNER_B
            )
        end)
    ),
    %% 正常绑定第二个映射
    {ok, M2} = enterprise_external_identity_repo:bind_tx(
        C, ?ORG_A, AppAId, <<"oa-emp-002">>, ?HUMAN_B
    ),
    ?assertEqual(?HUMAN_B, maps:get(<<"user_id">>, M2)),
    %% 反向唯一：同一 user 再绑另一个 external_user_id → 23505 拒绝
    ?assertEqual(
        {error, user_already_mapped},
        in_savepoint(C, fun() ->
            enterprise_external_identity_repo:bind_tx(
                C, ?ORG_A, AppAId, <<"oa-emp-002b">>, ?HUMAN_B
            )
        end)
    ),
    %% resolve 批量：只返回 active + 入参集合内（不存在的不出现）
    {ok, Rows} = enterprise_external_identity_repo:resolve_tx(
        C, ?ORG_A, AppAId, [<<"oa-emp-001">>, <<"oa-emp-002">>, <<"oa-emp-none">>]
    ),
    ?assertEqual(2, length(Rows)),
    %% unbind 后 resolve 不再命中；重复 unbind → not_active
    ok = enterprise_external_identity_repo:unbind_tx(C, ?ORG_A, AppAId, <<"oa-emp-002">>),
    ?assertEqual(
        {error, not_active},
        enterprise_external_identity_repo:unbind_tx(C, ?ORG_A, AppAId, <<"oa-emp-002">>)
    ),
    {ok, Rows2} = enterprise_external_identity_repo:resolve_tx(
        C, ?ORG_A, AppAId, [<<"oa-emp-001">>, <<"oa-emp-002">>]
    ),
    ?assertEqual(1, length(Rows2)),
    %% 解绑后重绑（removed 行被 upsert 复活，行 id 不变）
    {ok, M3} = enterprise_external_identity_repo:bind_tx(
        C, ?ORG_A, AppAId, <<"oa-emp-002">>, ?HUMAN_B
    ),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, M3)),
    ?assertEqual(maps:get(<<"id">>, M2), maps:get(<<"id">>, M3)),
    %% 按 user 解绑（离场路径）
    {ok, 1} = enterprise_external_identity_repo:unbind_user_tx(C, ?ORG_A, AppAId, ?HUMAN_A),
    {ok, 0} = enterprise_external_identity_repo:unbind_user_tx(C, ?ORG_A, AppAId, ?HUMAN_A),
    %% 不存在的映射
    ?assertEqual(
        {error, not_found},
        enterprise_external_identity_repo:find_by_external_tx(C, ?ORG_A, AppAId, <<"oa-emp-nope">>)
    ).

%%%===================================================================
%%% ④ enterprise_internal_idempotency
%%%===================================================================

idempotency_oracle(C) ->
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    {ok, AppA} = enterprise_application_repo:create_tx(C, ?ORG_A, <<"oa-gz-a">>, <<"a_org"/utf8>>),
    AppAId = maps:get(<<"id">>, AppA),
    Digest = sha256_hex(<<"request body">>),
    Future = <<"2035-01-01T00:00:00+00:00">>,
    %% 首插
    {ok, inserted, Row1} = enterprise_internal_idempotency_repo:record_tx(
        C, ?ORG_A, AppAId, <<"idem-001">>, <<"message.direct">>, Digest, Future
    ),
    ?assertEqual(null, maps:get(<<"resource_id">>, Row1)),
    ?assertEqual(null, maps:get(<<"response_code">>, Row1)),
    %% 同键同 body → existing（重放窗口命中）
    {ok, existing, Row2} = enterprise_internal_idempotency_repo:record_tx(
        C, ?ORG_A, AppAId, <<"idem-001">>, <<"message.direct">>, Digest, Future
    ),
    ?assertEqual(
        maps:get(<<"idempotency_key">>, Row1),
        maps:get(<<"idempotency_key">>, Row2)
    ),
    %% 同键异 body → digest_conflict（上层 409）
    ?assertEqual(
        {error, digest_conflict},
        enterprise_internal_idempotency_repo:record_tx(
            C,
            ?ORG_A,
            AppAId,
            <<"idem-001">>,
            <<"message.direct">>,
            sha256_hex(<<"different body">>),
            Future
        )
    ),
    %% claim 回填一次；第二次 already_claimed
    ok = enterprise_internal_idempotency_repo:claim_tx(
        C, ?ORG_A, AppAId, <<"idem-001">>, 987999, 200
    ),
    ?assertEqual(
        {error, already_claimed},
        enterprise_internal_idempotency_repo:claim_tx(
            C, ?ORG_A, AppAId, <<"idem-001">>, 987998, 200
        )
    ),
    {ok, Claimed} = enterprise_internal_idempotency_repo:find_tx(C, ?ORG_A, AppAId, <<"idem-001">>),
    ?assertEqual(987999, maps:get(<<"resource_id">>, Claimed)),
    ?assertEqual(200, maps:get(<<"response_code">>, Claimed)),
    ?assertEqual(false, maps:get(<<"expired">>, Claimed)),
    %% 过期标记：过去时间签发 → expired = true
    Past = <<"2020-01-01T00:00:00+00:00">>,
    {ok, inserted, _} = enterprise_internal_idempotency_repo:record_tx(
        C, ?ORG_A, AppAId, <<"idem-old">>, <<"message.direct">>, Digest, Past
    ),
    {ok, Old} = enterprise_internal_idempotency_repo:find_tx(C, ?ORG_A, AppAId, <<"idem-old">>),
    ?assertEqual(true, maps:get(<<"expired">>, Old)),
    %% 不存在的键
    ?assertEqual(
        {error, not_found},
        enterprise_internal_idempotency_repo:find_tx(C, ?ORG_A, AppAId, <<"idem-missing">>)
    ).

%%%===================================================================
%%% ⑤ enterprise_oa_sso_code
%%%===================================================================

sso_code_oracle(C) ->
    ok = seed_org(C, ?ORG_A, ?OWNER_A),
    ok = seed_org_members(C, ?ORG_A, [?HUMAN_A]),
    {ok, AppA} = enterprise_application_repo:create_tx(C, ?ORG_A, <<"oa-gz-a">>, <<"a_org"/utf8>>),
    AppAId = maps:get(<<"id">>, AppA),
    Code = <<"opaque-one-time-code-987">>,
    CodeDigest = enterprise_oa_sso_code_repo:digest_hex(Code),
    Redirect = <<"https://oa.customer.example.com/sso/cb">>,
    Future = <<"2035-01-01T00:00:00+00:00">>,
    %% issue：只存 digest
    {ok, Issued} = enterprise_oa_sso_code_repo:issue_tx(
        C,
        ?ORG_A,
        AppAId,
        ?HUMAN_A,
        CodeDigest,
        Redirect,
        enterprise_oa_sso_code_repo:digest_hex(<<"state-nonce">>),
        Future
    ),
    ?assertEqual(CodeDigest, maps:get(<<"code_digest">>, Issued)),
    ?assertNotEqual(Code, maps:get(<<"code_digest">>, Issued)),
    ?assertEqual(null, maps:get(<<"consumed_at">>, Issued)),
    {ok, Found} = enterprise_oa_sso_code_repo:find_by_digest_tx(C, CodeDigest),
    ?assertEqual(false, maps:get(<<"expired">>, Found)),
    %% 原子单次消费
    {ok, Consumed} = enterprise_oa_sso_code_repo:consume_tx(C, CodeDigest),
    ?assert(is_binary(maps:get(<<"consumed_at">>, Consumed))),
    %% 重放 → already_consumed（拒绝，不是重放响应）
    ?assertEqual(
        {error, already_consumed},
        enterprise_oa_sso_code_repo:consume_tx(C, CodeDigest)
    ),
    %% redirect 不匹配 → redirect_mismatch
    Digest2 = enterprise_oa_sso_code_repo:digest_hex(<<"another-code-987">>),
    {ok, _} = enterprise_oa_sso_code_repo:issue_tx(
        C,
        ?ORG_A,
        AppAId,
        ?HUMAN_A,
        Digest2,
        Redirect,
        enterprise_oa_sso_code_repo:digest_hex(<<"n2">>),
        Future
    ),
    ?assertEqual(
        {error, redirect_mismatch},
        enterprise_oa_sso_code_repo:consume_tx(C, Digest2, <<"https://evil.example.com/cb">>)
    ),
    %% 已过期 code → expired
    Digest3 = enterprise_oa_sso_code_repo:digest_hex(<<"expired-code-987">>),
    Past = <<"2020-01-01T00:00:00+00:00">>,
    {ok, _} = enterprise_oa_sso_code_repo:issue_tx(
        C,
        ?ORG_A,
        AppAId,
        ?HUMAN_A,
        Digest3,
        Redirect,
        enterprise_oa_sso_code_repo:digest_hex(<<"n3">>),
        Past
    ),
    ?assertEqual(
        {error, expired},
        enterprise_oa_sso_code_repo:consume_tx(C, Digest3, Redirect)
    ),
    %% 不存在的 code
    ?assertEqual(
        {error, not_found},
        enterprise_oa_sso_code_repo:consume_tx(
            C, enterprise_oa_sso_code_repo:digest_hex(<<"no-such-code">>)
        )
    ),
    %% 非 https redirect 被 CHECK 拒绝
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        in_savepoint(C, fun() ->
            enterprise_oa_sso_code_repo:issue_tx(
                C,
                ?ORG_A,
                AppAId,
                ?HUMAN_A,
                enterprise_oa_sso_code_repo:digest_hex(<<"http-code">>),
                <<"http://oa.customer.example.com/cb">>,
                enterprise_oa_sso_code_repo:digest_hex(<<"n4">>),
                Future
            )
        end)
    ).

%%%===================================================================
%%% ⑤b push_token platform 值域（EPGZ-01R / EPGZ-07 硬需求）
%%%===================================================================

push_token_platform_oracle(C) ->
    %% 'jpush' 可插入（136 扩展值域；EPGZ-07 目标合同 device_type=android + platform=jpush）
    ?assertMatch(
        {ok, _},
        insert_push_token(C, 987501, <<"android">>, <<"jpush">>)
    ),
    %% 既有值域回归放行：fcm / apns / web_push
    lists:foreach(
        fun({Id, Dt, Platform}) ->
            ?assertMatch(
                {ok, _},
                insert_push_token(C, Id, Dt, Platform)
            )
        end,
        [
            {987502, <<"android">>, <<"fcm">>},
            {987503, <<"ios">>, <<"apns">>},
            {987504, <<"web">>, <<"web_push">>}
        ]
    ),
    %% 非法值仍拒（CHECK 23514）：未知 provider / 大小写变体 / 空串
    BadPlatforms = [<<"xxx">>, <<"JPush">>, <<"FCM">>, <<"">>],
    lists:foreach(
        fun(Platform) ->
            ?assertMatch(
                {error, #error{code = <<"23514">>}},
                in_savepoint(C, fun() ->
                    insert_push_token(C, 987505, <<"android">>, Platform)
                end)
            )
        end,
        BadPlatforms
    ),
    %% device_type 值域未受本次修订影响：非法 device_type 仍拒
    ?assertMatch(
        {error, #error{code = <<"23514">>}},
        in_savepoint(C, fun() ->
            insert_push_token(C, 987506, <<"watchos">>, <<"jpush">>)
        end)
    ).

%%%===================================================================
%%% ⑥ migration down/up 循环（独立连接；排在最后）
%%%===================================================================

migration_cycle_test(State) ->
    ?_test(migration_cycle(State)).

migration_cycle(State) ->
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    Conn = connect_marker(State),
    try
        MigConfig = #{conn => Conn, dir => "priv/migrations", strict => true},
        %% down 00000136：五表 + 两个守卫函数全部消失，push platform 值域恢复，
        %% 版本回到 135
        ok = erlang_migrate:down(MigConfig, 1),
        {ok, VerAfterDown, false} = erlang_migrate:version(MigConfig),
        ?assertEqual(135, VerAfterDown),
        lists:foreach(
            fun(T) -> ?assert(table_missing(Conn, T), {table_should_be_gone, T}) end,
            ?TABLES
        ),
        ?assert(function_missing(Conn, <<"fn_enterprise_external_identity_member_guard">>)),
        ?assert(function_missing(Conn, <<"fn_enterprise_application_redirect_guard">>)),
        %% down 后 push_token platform 值域精确恢复原定义（00000001）：
        %% 'jpush' 被拒、'fcm' 放行
        ?assertMatch(
            {error, #error{code = <<"23514">>}},
            insert_push_token(Conn, 987601, <<"android">>, <<"jpush">>)
        ),
        ?assertMatch(
            {ok, _},
            insert_push_token(Conn, 987602, <<"android">>, <<"fcm">>)
        ),
        %% 再次 up 00000136：五表全部重建，版本回到 136
        ok = erlang_migrate:up(MigConfig, 1),
        {ok, 136, false} = erlang_migrate:version(MigConfig),
        lists:foreach(
            fun(T) -> ?assertNot(table_missing(Conn, T), {table_should_exist, T}) end,
            ?TABLES
        ),
        %% up 后 push platform 值域重新扩展：'jpush' 放行
        ?assertMatch(
            {ok, _},
            insert_push_token(Conn, 987603, <<"android">>, <<"jpush">>)
        ),
        %% 重建后 oracle 复验：五表可写且约束/守卫仍在
        ok = exec(Conn, <<"BEGIN">>),
        try
            ok = seed_org(Conn, ?ORG_A, ?OWNER_A),
            {ok, AppA} = enterprise_application_repo:create_tx(
                Conn, ?ORG_A, <<"oa-gz-a">>, <<"rebuilt"/utf8>>
            ),
            {ok, _} = enterprise_application_credential_repo:create_tx(
                Conn, ?ORG_A, maps:get(<<"id">>, AppA), <<"ib_int_rebuilt">>, <<"secret-987">>
            ),
            %% 触发器守卫同样重建：非 member 绑定仍被拒绝
            ?assertEqual(
                {error, invalid_member},
                in_savepoint(Conn, fun() ->
                    enterprise_external_identity_repo:bind_tx(
                        Conn, ?ORG_A, maps:get(<<"id">>, AppA), <<"oa-emp-x">>, ?OUTSIDER
                    )
                end)
            ),
            %% redirect 守卫同样重建：非 https 元素仍被 23514 拒绝
            ?assertMatch(
                {error, #error{code = <<"23514">>}},
                in_savepoint(Conn, fun() ->
                    enterprise_application_repo:create_tx(
                        Conn,
                        ?ORG_A,
                        <<"oa-gz-redir-rebuilt">>,
                        <<"rb"/utf8>>,
                        undefined,
                        [<<"http://oa.customer.example.com/cb">>]
                    )
                end)
            ),
            %% 合法 allowlist 重建后可写
            {ok, Rb} = enterprise_application_repo:create_tx(
                Conn,
                ?ORG_A,
                <<"oa-gz-redir-rebuilt2">>,
                <<"rb2"/utf8>>,
                undefined,
                [<<"https://oa.customer.example.com/sso/cb">>]
            ),
            ?assertEqual(
                [<<"https://oa.customer.example.com/sso/cb">>],
                maps:get(<<"allowed_redirect_uris">>, Rb)
            ),
            ok
        after
            exec(Conn, <<"ROLLBACK">>)
        end
    after
        try
            epgsql:close(Conn)
        catch
            _:_ -> ok
        end
    end.

%%%===================================================================
%%% Seed helpers
%%%===================================================================

%% 建机构（owner 用户先落行满足 FK；组织触发器自动补 owner member 行）。
seed_org(C, OrgId, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(OwnerUid),
        ", 'x', 't987_owner_",
        integer_to_binary(OwnerUid),
        <<"', '127.0.0.1', 'x')">>
    ]),
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", 't987_org', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

%% 建普通 Human 用户 + active org member 行。
seed_org_members(C, OrgId, Uids) ->
    lists:foreach(
        fun(Uid) ->
            exec(C, [
                <<"INSERT INTO \"user\" (id, password, account, reg_ip, reg_cosv) VALUES (">>,
                integer_to_binary(Uid),
                ", 'x', 't987_u_",
                integer_to_binary(Uid),
                <<"', '127.0.0.1', 'x')">>
            ]),
            exec(C, [
                <<"INSERT INTO organization_member (organization_id, user_id, role, joined_at, status, created_at, updated_at) VALUES (">>,
                integer_to_binary(OrgId),
                ", ",
                integer_to_binary(Uid),
                <<", 'member', CURRENT_TIMESTAMP, 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
            ])
        end,
        Uids
    ).

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

%% 插入 push_token 行（无 FK，user_id 仅占位）；device_id/token 按 Id 派生
%% 避开既有部分唯一索引 uk_push_token_user_device (user_id, device_id)
%% WHERE status=1；返回 {ok,_}|{error,#error{}} 以便负例直接断言 CHECK 违约。
insert_push_token(C, Id, DeviceType, Platform) ->
    Sql = iolist_to_binary([
        <<"INSERT INTO push_token (id, user_id, device_id, device_type, platform, token, status, created_at, updated_at) VALUES (">>,
        integer_to_binary(Id),
        ", ",
        integer_to_binary(?HUMAN_A),
        ", 'dev-",
        integer_to_binary(Id),
        <<"', '">>,
        DeviceType,
        <<"', '">>,
        Platform,
        <<"', 'tok-">>,
        integer_to_binary(Id),
        <<"', 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]),
    elib_pg:query(C, Sql, []).

%% 与 A5 消费谓词同型的 exact match 断言 SQL：
%% <uri> = ANY(allowed_redirect_uris)（逐字节比较，结果别名 missing 复用 bool/1；
%% 行限定本事务内创建的 oa-gz-redir application，其 allowlist 为 cb/1..cb/20）。
allow_match_sql(Uri) ->
    Escaped = binary:replace(Uri, <<"'">>, <<"''">>, [global]),
    <<"SELECT ('", Escaped/binary, "' = ANY(allowed_redirect_uris)) AS missing",
        " FROM enterprise_application", " WHERE organization_id = ",
        (integer_to_binary(?ORG_A))/binary, " AND application_key = 'oa-gz-redir' LIMIT 1">>.

table_missing(C, Table) ->
    bool(C, <<"SELECT to_regclass('public.", Table/binary, "') IS NULL AS missing">>).

function_missing(C, Function) ->
    bool(C, <<"SELECT to_regprocedure('public.", Function/binary, "()') IS NULL AS missing">>).

bool(C, Sql) ->
    case elib_pg:query(C, Sql, []) of
        {ok, [#{<<"missing">> := True}]} -> True;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.

sha256_hex(Value) when is_binary(Value) ->
    binary:encode_hex(crypto:hash(sha256, Value), lowercase).
