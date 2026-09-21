%%% @doc CSB-01 真库套件：Widget 安装 / 身份密钥 / bootstrap 令牌 / JTI nonce
%%% 的持久化与安全不变量（POST-V4.1 run §12.7 CSB-01-A01..A05）。
%%%
%%% ============================================================================
%%% 【复用/新建决策（CSB-01 §12.4「能复用 visit token 时不复制」）】
%%%
%%% bootstrap 令牌**复用** customer_service_visit_token，不新建独立 bootstrap 表：
%%%   * visit_token 已具备 bootstrap 所需的全部承载列——(organization_id,
%%%     contact_id) 复合 FK（fk_csvt_contact，与 125 同口径）、token_digest
%%%     （uq_csvt_org_digest 唯一）、expires_at / revoked_at；
%%%   * 缺口只有三个可空列：widget_installation_id（复合 FK 指回 installation）、
%%%     anonymous_subject_hmac、last_seen_at（00000132 迁移只做追加）；
%%%   * 追加列全部 NULL 即非 Widget 令牌——既有运营侧 visit token 行零语义
%%%     变化，digest / 过期 / 吊销不变量未放宽，满足「不放宽既有不变量即可复用」；
%%%   * 新建独立表反而是复制：同一套 digest 唯一性 + expiry/revoke 语义要维护
%%%     两份，且 session.visit_token_id 的审计链路无法覆盖 Widget 令牌。
%%% installation / identity_key / nonce 三表无既有载体，为新建（00000132）。
%%% ============================================================================
%%%
%%% 判定对应：
%%%   * A01：00000132 up/down 往返无残留；00000114..00000125 文件列表逐字冻结、
%%%     内容零修改（读目录断言）；
%%%   * A02：public_widget_id 可公开（非 secret），但解析同语句带 Org——错 Org
%%%     查询 not_found，换不来跨 Org 权限；
%%%   * A03：明文 secret / signing key / JTI 不落库（列级 + 行级 json 断言），
%%%     store 返回行也不外泄 digest；
%%%   * A04：同 Org 复合 FK 负例（23503）、过期 CHECK（23514）、吊销语义、
%%%     nonce 重放唯一性（串行 + 8 进程并发恰一胜）；
%%%   * A05：错误归一化只含 SQLSTATE/约束名；实现模块零日志调用（静态断言），
%%%     不存在明文进日志/错误的通道。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/1` 随机 TSID scope；无真实数据。
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
-module(cs_pg_widget_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(FIX, cs_pg_test_fixture).
-define(MIG_DIR, "priv/migrations").
-define(LEGACY_MIGRATIONS_114_125, [
    "00000114_enterprise_business_identity.up.sql",
    "00000114_enterprise_business_identity.down.sql",
    "00000115_enterprise_contact.up.sql",
    "00000115_enterprise_contact.down.sql",
    "00000116_enterprise_conversation_message.up.sql",
    "00000116_enterprise_conversation_message.down.sql",
    "00000117_enterprise_retention.up.sql",
    "00000117_enterprise_retention.down.sql",
    "00000118_enterprise_asset.up.sql",
    "00000118_enterprise_asset.down.sql",
    "00000119_enterprise_audit_event.up.sql",
    "00000119_enterprise_audit_event.down.sql",
    "00000120_enterprise_offboarding.up.sql",
    "00000120_enterprise_offboarding.down.sql",
    "00000121_enterprise_retention_hold_scope_active.up.sql",
    "00000121_enterprise_retention_hold_scope_active.down.sql",
    "00000122_enterprise_conversation_consent_evidence_kind.up.sql",
    "00000122_enterprise_conversation_consent_evidence_kind.down.sql",
    "00000123_enterprise_message_delivery_null_device_idempotency.up.sql",
    "00000123_enterprise_message_delivery_null_device_idempotency.down.sql",
    "00000124_enterprise_consent_evidence_kind_not_null_guard.up.sql",
    "00000124_enterprise_consent_evidence_kind_not_null_guard.down.sql",
    "00000125_customer_service_foundation.up.sql",
    "00000125_customer_service_foundation.down.sql"
]).

cs_pg_widget_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_legacy_migrations_untouched/0},
        {timeout, 120, fun a01_widget_migration_roundtrip_no_residue/0},
        {timeout, 60, fun tenant_keys_carry_org_in_every_statement/0},
        {timeout, 60, fun a02_public_widget_id_cannot_cross_org/0},
        {timeout, 60, fun csd_be01_global_public_id_lookup/0},
        {timeout, 60, fun csd_be01s_global_token_digest_lookup/0},
        {timeout, 60, fun a02_allowed_origins_jsonb_roundtrip/0},
        {timeout, 60, fun a03_only_digest_columns_and_rows/0},
        {timeout, 60, fun a03_returned_rows_carry_no_plaintext/0},
        {timeout, 60, fun a04_same_org_fk_negatives/0},
        {timeout, 60, fun a04_expiry_and_revocation_semantics/0},
        {timeout, 60, fun a04_nonce_replay_unique_conflict/0},
        {timeout, 120, fun a04_nonce_concurrent_uniqueness/0},
        {timeout, 60, fun a05_errors_and_source_have_no_secret_channel/0}
    ];
cases({error, Reason}) ->
    erlang:error({csb01_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% A01：旧迁移零修改 + 00000132 up/down 往返无残留
%% ===================================================================

a01_legacy_migrations_untouched() ->
    {ok, Files} = file:list_dir(?MIG_DIR),
    %% 114..125 的 24 个文件逐字在册（未被删除/改名）
    lists:foreach(fun(Name) -> ?assert(lists:member(Name, Files)) end, ?LEGACY_MIGRATIONS_114_125),
    %% 该版本段没有多出来的文件（未追加/未覆盖其他编号）
    Foreign = [
        F
     || F <- Files,
        migration_version_in_range(F, 114, 125),
        not lists:member(F, ?LEGACY_MIGRATIONS_114_125)
    ],
    ?assertEqual([], Foreign),
    %% 内容零修改的证据性断言：旧迁移正文不含 widget 字样（widget 只属于 132）
    lists:foreach(
        fun(Name) ->
            {ok, Bin} = file:read_file(filename:join(?MIG_DIR, Name)),
            ?assertMatch(nomatch, re:run(Bin, <<"widget">>, [caseless]))
        end,
        ?LEGACY_MIGRATIONS_114_125
    ),
    %% 本卡迁移落定为 00000132：分支期分配的是 131，但三计划合并序列把
    %% 131 给了 organization_invitation_platform_invite（ORG-V1），widget
    %% foundation 顺延为 132（agent_run_foundation 居 134 为 head）。
    ?assert(lists:member("00000132_customer_service_widget_foundation.up.sql", Files)),
    ?assert(lists:member("00000132_customer_service_widget_foundation.down.sql", Files)).

a01_widget_migration_roundtrip_no_residue() ->
    %% 前置：app 启动 migrate 已把 132 应用到 scratch 库
    ?assert(table_exists(<<"customer_service_widget_installation">>)),
    ok = with_migrate_conn(fun(Conn) ->
        %% table 显式留默认（erlang_migrate 自有的 schema_migrations，含 dirty 列）：
        %% app 侧 schema_migrations_history 无 dirty 列、非同 schema，不可混用；
        %% scratch 库由本 run 独占，roundtrip 前置断言已锁定基线在位。
        Config = #{conn => Conn, dir => imboy_migrate:get_scripts_path(), strict => true},
        %% down 恰 4 步 = 从 head（135=customer_service_seat_sse；134=agent_run
        %% foundation、133=agent_grant_foundation 同批在册）回滚 135/134/133/132，
        %% 其中 132 即本卡 widget foundation；131 及更早保持不动
        %% （CSD-BE-01 修正：迁移 135 入库（BE-S01）后 head 不再是 134，
        %% 原 down 3 步在 head=135 的库上留 132 在表 → 断言恒红）。
        ok = erlang_migrate:down(Config, 4),
        ?assertNot(table_exists(<<"customer_service_widget_installation">>)),
        ?assertNot(table_exists(<<"customer_service_widget_identity_key">>)),
        ?assertNot(table_exists(<<"customer_service_widget_nonce">>)),
        ?assertNot(column_exists(<<"customer_service_visit_token">>, <<"widget_installation_id">>)),
        ?assertNot(column_exists(<<"customer_service_visit_token">>, <<"anonymous_subject_hmac">>)),
        ?assertNot(column_exists(<<"customer_service_visit_token">>, <<"last_seen_at">>)),
        %% up 1 步 = 重新应用 132（widget 回归）；再跑一次 up 全量
        %% （补回 133/134，幂等：IF NOT EXISTS 全绿）
        ok = erlang_migrate:up(Config, 1),
        ?assert(table_exists(<<"customer_service_widget_installation">>)),
        ok = erlang_migrate:up(Config)
    end),
    ?assert(column_exists(<<"customer_service_visit_token">>, <<"widget_installation_id">>)).

%% 铁律 6 机械断言：本模块冻结语句每条同语句带 organization_id——
%% INSERT 语句以列承载、其余（读取/UPDATE）必须是谓词 organization_id = $1。
tenant_keys_carry_org_in_every_statement() ->
    Statements = cs_pg_widget:sql_statements(),
    lists:foreach(
        fun(Sql) -> ?assertMatch({match, _}, re:run(Sql, <<"organization_id">>)) end,
        Statements
    ),
    lists:foreach(
        fun(Sql) ->
            ?assertMatch({match, _}, re:run(Sql, <<"organization_id\\s*=\\s*\\$1">>))
        end,
        [S || S <- Statements, not is_insert_statement(S)]
    ).

is_insert_statement(Sql) ->
    match =:= re:run(Sql, <<"^\\s*INSERT\\s+INTO">>, [{capture, none}]).

%% ===================================================================
%% A02：public_widget_id 可公开但不能换取跨 Org 权限
%% ===================================================================

a02_public_widget_id_cannot_cross_org() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    PublicId = public_widget_id(),
    try
        {ok, Inst} = cs_pg_widget:insert_widget_installation(Org, #{
            id => ?FIX:id(),
            organization_id => Org,
            public_widget_id => PublicId,
            display_name => <<"csb01-widget">>,
            allowed_origins => [<<"https://shop.example.com">>],
            branding => #{<<"primary">> => <<"#0a84ff">>},
            consent_version => <<"csb01-consent-v1">>,
            created_by_user_id => maps:get(owner_user_id, Scope)
        }),
        InstallationId = maps:get(id, Inst),
        %% 本 Org 解析成功（公开 ID 本身不是 secret，可公开分发）
        {ok, ByPublic} = cs_pg_widget:fetch_widget_installation_by_public_id(Org, PublicId),
        ?assertEqual(InstallationId, maps:get(id, ByPublic)),
        %% 错 Org 的查询拿到 not_found：公开 ID 换不来跨 Org 权限
        {error, not_found} = cs_pg_widget:fetch_widget_installation_by_public_id(
            OtherOrg, PublicId
        ),
        {error, not_found} = cs_pg_widget:fetch_widget_installation(OtherOrg, InstallationId),
        {error, not_found} = cs_pg_widget:revoke_widget_installation(
            OtherOrg, InstallationId, erlang:system_time(second)
        )
    after
        cleanup_widget(Scope)
    end.

%% CSD-BE-01（hosted-widget-contract S3，A06 oracle）：public_widget_id
%% **全局**反查的 PG 证明——无 Org 输入命中唯一行，行的 organization_id 即
%% 权威派生租户（/w/ 面的租户归属真源）；不存在 → not_found（application
%% 归一 installation_unavailable，无枚举）。零 DDL：走既有行与列。
csd_be01_global_public_id_lookup() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    PublicId = public_widget_id(),
    try
        {ok, Inst} = cs_pg_widget:insert_widget_installation(Org, #{
            id => ?FIX:id(),
            organization_id => Org,
            public_widget_id => PublicId,
            display_name => <<"csdb01-global-lookup">>,
            allowed_origins => [<<"https://shop.example.com">>],
            branding => #{<<"primary">> => <<"#0a84ff">>},
            consent_version => <<"csb01-consent-v1">>,
            created_by_user_id => maps:get(owner_user_id, Scope)
        }),
        InstallationId = maps:get(id, Inst),
        %% 全局反查：无 Org 输入；命中行派生 (id, organization_id)。
        {ok, Row} = cs_pg_widget:fetch_widget_installation_by_public_id_global(PublicId),
        ?assertEqual(InstallationId, maps:get(id, Row)),
        ?assertEqual(Org, maps:get(organization_id, Row)),
        %% 不存在 → not_found（与命中失败的唯一形状，三态归一在上层）。
        {error, not_found} = cs_pg_widget:fetch_widget_installation_by_public_id_global(
            <<"wgt_pub_absent_csdb01">>
        ),
        %% 吊销后反查仍命中行（行保留以审计）；active 门由 application 裁决
        %% （installation_active + installation_unavailable 归一）。status 列
        %% 契约：`active` → atom，其余 fail-closed 保留 binary。
        At = erlang:system_time(second),
        ok = cs_pg_widget:revoke_widget_installation(Org, InstallationId, At),
        {ok, RevRow} = cs_pg_widget:fetch_widget_installation_by_public_id_global(PublicId),
        ?assertEqual(<<"revoked">>, maps:get(status, RevRow))
    after
        cleanup_widget(Scope)
    end.

%% CSD-BE-01S（hosted-widget-contract S3 v1.1，GAP-3 oracle）：bootstrap
%% token digest **全局**命中的 PG 证明——无 Org 输入，(installation_id, digest)
%% 命中行派生 organization_id（持 token 动作面的租户真源）；digest 未命中/
%% 跨 installation → not_found（digest = sha256(secret)，无存在性枚举）。
%% SQL 形状机械断言：谓词零 Org、恰两个占位符。
csd_be01s_global_token_digest_lookup() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    try
        {ok, Inst} = cs_pg_widget:insert_widget_installation(Org, #{
            id => ?FIX:id(),
            organization_id => Org,
            public_widget_id => public_widget_id(),
            display_name => <<"csbe01s-global-digest">>,
            allowed_origins => [<<"https://shop.example.com">>],
            branding => #{},
            consent_version => <<"csb01-consent-v1">>
        }),
        InstallationId = maps:get(id, Inst),
        Digest = cs_access_app:default_digest(<<"csbe01s-global-token">>),
        {ok, Token} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
            id => ?FIX:id(),
            contact_id => maps:get(contact_id, Scope),
            token_digest => Digest,
            expires_at => erlang:system_time(second) + 600,
            widget_installation_id => InstallationId
        }),
        %% 全局命中：无 Org 输入，行的 organization_id 即派生租户。
        {ok, Row} = cs_pg_widget:fetch_widget_bootstrap_token_by_digest_global(
            InstallationId, Digest
        ),
        ?assertEqual(maps:get(id, Token), maps:get(id, Row)),
        ?assertEqual(Org, maps:get(organization_id, Row)),
        ?assertEqual(InstallationId, maps:get(widget_installation_id, Row)),
        %% digest 不匹配 → not_found；跨 installation → not_found（不可枚举）。
        {error, not_found} = cs_pg_widget:fetch_widget_bootstrap_token_by_digest_global(
            InstallationId, cs_access_app:default_digest(<<"csbe01s-other-token">>)
        ),
        {error, not_found} = cs_pg_widget:fetch_widget_bootstrap_token_by_digest_global(
            ?FIX:id(), Digest
        ),
        %% 与既有 (Org, installation) 同语句口径一致：行保留、可复核。
        {ok, _} = cs_pg_widget:fetch_widget_bootstrap_token_by_digest(
            Org, InstallationId, Digest
        ),
        _ = OtherOrg,
        %% SQL 形状机械断言（同 by_public_id_global 先例）：谓词零 Org
        %% （organization_id 只在 SELECT 投影）、占位符恰为 $1/$2。
        Chunk = global_digest_sql_chunk(),
        ?assertMatch(
            {match, _},
            re:run(Chunk, <<"WHERE\\s+widget_installation_id = \\$1 AND token_digest = \\$2">>)
        ),
        ?assertMatch(nomatch, re:run(Chunk, <<"organization_id\\s*=">>)),
        ?assertMatch({match, _}, re:run(Chunk, <<"SELECT id, organization_id,">>)),
        ok
    after
        cleanup_widget(Scope)
    end.

%% 全局 digest 语句是模块级宏（不进 sql_statements/0——「同语句带 Org」的
%% 机械断言集语义上不适用）；形状以模块源码冻结（宏原文切片）。
global_digest_sql_chunk() ->
    {ok, Bin} = file:read_file(
        filename:join([
            "src", "features", "customer_service", "infrastructure", "cs_pg_widget.erl"
        ])
    ),
    case binary:split(Bin, <<"-define(SQL_FETCH_BOOTSTRAP_BY_DIGEST_GLOBAL, <<">>) of
        [_Only] ->
            erlang:error(global_digest_sql_missing);
        [_Head, Rest] ->
            case binary:split(Rest, <<">>).">>) of
                [Chunk, _Tail] -> Chunk;
                _ -> erlang:error(global_digest_sql_unterminated)
            end
    end.

%% ===================================================================
%% A02b：allowed_origins 必须以 JSON 数组落库（jsonb/1 列表编码回归）
%%
%% 背景：W4 真实 HTTP 验证（2026-09-17）抓出 cs_pg_common:jsonb/1 的
%% catch-all 把列表吞成 <<"{}">> 字符串——create 落库的 installation
%% allowed_origins 恒为 JSON string "{}"，origin 白名单永不命中。
%% 断言用 jsonb_typeof（codec 无关）：eunit 池未配 json codec 时回读
%% 是 binary，不能以回读类型判定写侧正确性。
%% ===================================================================

a02_allowed_origins_jsonb_roundtrip() ->
    %% 编码层：list → JSON 数组（decode 比较，规避 jsone 的 \/ 转义形式）；
    %% map → 对象；binary 透传；其他保持 fail 值
    ?assertEqual(
        [<<"https://shop.example.com">>],
        jsone:decode(cs_pg_common:jsonb([<<"https://shop.example.com">>]))
    ),
    ?assertEqual([], jsone:decode(cs_pg_common:jsonb([]))),
    ?assertMatch(<<"{", _/binary>>, cs_pg_common:jsonb(#{<<"a">> => 1})),
    ?assertEqual(<<"plain">>, cs_pg_common:jsonb(<<"plain">>)),
    %% 落库层：insert 后列类型必须是 jsonb array（而非 string）
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, Inst} = cs_pg_widget:insert_widget_installation(Org, #{
            id => ?FIX:id(),
            organization_id => Org,
            public_widget_id => public_widget_id(),
            display_name => <<"csb01-origins-roundtrip">>,
            allowed_origins => [<<"https://shop.example.com">>],
            branding => #{},
            consent_version => <<"csb01-consent-v1">>
        }),
        InstallationId = maps:get(id, Inst),
        ?assertEqual(
            <<"array">>,
            ?FIX:scalar(
                <<
                    "SELECT jsonb_typeof(allowed_origins) FROM customer_service_widget_installation"
                    " WHERE id = $1"
                >>,
                [InstallationId],
                <<>>
            )
        ),
        %% 回读：读侧 jsonb_read 归一后必须是等价列表（origin 校验只认 list）
        {ok, Back} = cs_pg_widget:fetch_widget_installation(Org, InstallationId),
        ?assertEqual([<<"https://shop.example.com">>], maps:get(allowed_origins, Back)),
        ?assertEqual(#{}, maps:get(branding, Back))
    after
        cleanup_widget(Scope)
    end.

%% ===================================================================
%% A03：明文 secret / token / JTI 只存 digest（列级 + 行级）
%% ===================================================================

a03_only_digest_columns_and_rows() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, Inst} = install(Scope),
        InstallationId = maps:get(id, Inst),
        Now = erlang:system_time(second),
        %% 列级：密钥/nonce/令牌/安装表不存在任何明文承载列
        lists:foreach(
            fun(Table) -> ?assertEqual([], plaintext_columns(Table)) end,
            [
                <<"customer_service_widget_installation">>,
                <<"customer_service_widget_identity_key">>,
                <<"customer_service_widget_nonce">>,
                <<"customer_service_visit_token">>
            ]
        ),
        %% 行级：signing key 明文不落库（identity_key 只存 key_digest）
        KeyPlain = <<"csb01-PLAINTEXT-signing-key-do-not-store">>,
        {ok, _} = cs_pg_widget:insert_widget_identity_key(Org, InstallationId, #{
            id => ?FIX:id(),
            key_digest => cs_access_app:default_digest(KeyPlain),
            key_version => 1,
            display_hint => <<"last4-abcd">>,
            expires_at => Now + 3600
        }),
        ?assertNot(row_text_contains(<<"customer_service_widget_identity_key">>, Org, KeyPlain)),
        %% 行级：bootstrap 令牌明文不落库（visit_token 只存 token_digest）
        TokenPlain = <<"csb01-PLAINTEXT-bootstrap-token-do-not-store">>,
        {ok, _} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
            id => ?FIX:id(),
            contact_id => maps:get(contact_id, Scope),
            token_digest => cs_access_app:default_digest(TokenPlain),
            expires_at => Now + 600,
            widget_installation_id => InstallationId,
            anonymous_subject_hmac => <<"csb01-anon-subject-hmac">>
        }),
        ?assertNot(row_text_contains(<<"customer_service_visit_token">>, Org, TokenPlain)),
        %% 行级：JTI 明文不落库（nonce 只存 jti_digest）
        JtiPlain = <<"csb01-PLAINTEXT-jti-do-not-store">>,
        ok = cs_pg_widget:record_widget_nonce(
            Org, InstallationId, cs_access_app:default_digest(JtiPlain), Now + 60
        ),
        ?assertNot(row_text_contains(<<"customer_service_widget_nonce">>, Org, JtiPlain))
    after
        cleanup_widget(Scope)
    end.

%% store 返回行：不携带明文、不外泄 bootstrap digest（投影唯一出口在 application）。
a03_returned_rows_carry_no_plaintext() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, Inst} = install(Scope),
        InstallationId = maps:get(id, Inst),
        TokenPlain = <<"csb01-PLAINTEXT-bootstrap-2">>,
        {ok, Token} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
            id => ?FIX:id(),
            contact_id => maps:get(contact_id, Scope),
            token_digest => cs_access_app:default_digest(TokenPlain),
            expires_at => erlang:system_time(second) + 600,
            widget_installation_id => InstallationId,
            anonymous_subject_hmac => <<"csb01-anon-hmac-2">>
        }),
        %% 返回行任何字段都不等于明文，且 digest 不在行里
        ?assertNot(is_map_key(token_digest, Token)),
        lists:foreach(
            fun({_K, V}) -> ?assertNotEqual(TokenPlain, V) end,
            maps:to_list(Token)
        ),
        {ok, Key} = cs_pg_widget:insert_widget_identity_key(Org, InstallationId, #{
            id => ?FIX:id(),
            key_digest => cs_access_app:default_digest(<<"csb01-PLAINTEXT-key-2">>),
            key_version => 1,
            expires_at => erlang:system_time(second) + 3600
        }),
        %% key_digest 是行内唯一密钥材料且不等于明文
        ?assert(is_map_key(key_digest, Key)),
        ?assertNotEqual(<<"csb01-PLAINTEXT-key-2">>, maps:get(key_digest, Key))
    after
        cleanup_widget(Scope)
    end.

%% ===================================================================
%% A04：同 Org FK 负例 / 过期与吊销语义 / nonce 重放与并发唯一性
%% ===================================================================

a04_same_org_fk_negatives() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    try
        {ok, _} = install(Scope),
        %% 跨 Org 的 installation
        {ok, ForeignInst} = cs_pg_widget:insert_widget_installation(OtherOrg, #{
            id => ?FIX:id(),
            organization_id => OtherOrg,
            public_widget_id => public_widget_id(),
            display_name => <<"csb01-foreign">>,
            consent_version => <<"csb01-consent-v1">>
        }),
        ForeignId = maps:get(id, ForeignInst),
        Now = erlang:system_time(second),
        %% identity_key：installation_id 属于别的 Org ⇒ 复合 FK 23503 拒绝
        {error, ErrKey} = cs_pg_widget:insert_widget_identity_key(Org, ForeignId, #{
            id => ?FIX:id(),
            key_digest => cs_access_app:default_digest(<<"csb01-fk-key">>),
            key_version => 1,
            expires_at => Now + 3600
        }),
        %% 归一化错误 = {sql, SQLSTATE, 约束名}（注意：cs_pg_common:error_constraint
        %% 接收 extra 列表，不接收归一化元组——这里直接整元组断言）
        ?assertEqual({sql, <<"23503">>, <<"fk_cswk_installation">>}, ErrKey),
        %% bootstrap 令牌：widget_installation_id 属于别的 Org ⇒ 复合 FK 23503 拒绝
        {error, ErrToken} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
            id => ?FIX:id(),
            contact_id => maps:get(contact_id, Scope),
            token_digest => cs_access_app:default_digest(<<"csb01-fk-token">>),
            expires_at => Now + 600,
            widget_installation_id => ForeignId
        }),
        ?assertEqual({sql, <<"23503">>, <<"fk_csvt_widget_installation">>}, ErrToken),
        %% nonce：installation_id 属于别的 Org ⇒ 复合 FK 23503 拒绝（绕过应用直插）
        {error, ErrNonce} = ?FIX:exec(
            <<
                "INSERT INTO customer_service_widget_nonce"
                " (id, organization_id, installation_id, jti_digest, expires_at)"
                " VALUES ($1, $2, $3, 'csb01-fk-jti', to_timestamp($4))"
            >>,
            [?FIX:id(), Org, ForeignId, Now + 60]
        ),
        ?assertEqual(<<"23503">>, raw_error_code(ErrNonce)),
        ?assertEqual(<<"fk_cswn_installation">>, raw_error_constraint(ErrNonce)),
        %% 本 Org 不存在任何跨 Org 行（全部被 DB 拒绝）
        ?assertEqual(0, widget_row_count(Org, <<"customer_service_widget_identity_key">>)),
        ?assertEqual(0, widget_row_count(Org, <<"customer_service_widget_nonce">>)),
        ?assertEqual(0, widget_token_count(Org))
    after
        cleanup_widget(Scope)
    end.

a04_expiry_and_revocation_semantics() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, Inst} = install(Scope),
        InstallationId = maps:get(id, Inst),
        Now = erlang:system_time(second),
        %% 过期负例：identity_key expires_at <= created_at ⇒ ck_cswk_expiry 23514
        {error, ErrExpiry} = cs_pg_widget:insert_widget_identity_key(Org, InstallationId, #{
            id => ?FIX:id(),
            key_digest => cs_access_app:default_digest(<<"csb01-expired-key">>),
            key_version => 1,
            expires_at => Now - 3600
        }),
        ?assertEqual({sql, <<"23514">>, <<"ck_cswk_expiry">>}, ErrExpiry),
        %% 有效 key（expires_at > created_at）
        {ok, Key} = cs_pg_widget:insert_widget_identity_key(Org, InstallationId, #{
            id => ?FIX:id(),
            key_digest => cs_access_app:default_digest(<<"csb01-live-key">>),
            key_version => 2,
            expires_at => Now + 3600
        }),
        ?assertEqual(active, maps:get(status, Key)),
        %% 吊销后：status=revoked + revoked_at 非空；重复吊销 not_found
        ok = cs_pg_widget:revoke_widget_identity_key(Org, InstallationId, 2, Now + 1),
        {ok, RevokedKey} = cs_pg_widget:fetch_widget_identity_key(Org, InstallationId, 2),
        ?assertEqual(<<"revoked">>, maps:get(status, RevokedKey)),
        ?assert(is_integer(maps:get(revoked_at, RevokedKey))),
        {error, not_found} = cs_pg_widget:revoke_widget_identity_key(
            Org, InstallationId, 2, Now + 2
        ),
        %% bootstrap 令牌：吊销后行保留（application 判 revoked_at）；
        %% touch / 再吊销均 not_found
        TokenDigest = cs_access_app:default_digest(<<"csb01-revoke-token">>),
        {ok, Token} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
            id => ?FIX:id(),
            contact_id => maps:get(contact_id, Scope),
            token_digest => TokenDigest,
            expires_at => Now + 600,
            widget_installation_id => InstallationId
        }),
        TokenId = maps:get(id, Token),
        ok = cs_pg_widget:touch_widget_bootstrap_token(Org, InstallationId, TokenId, Now + 1),
        {ok, Touched} = cs_pg_widget:fetch_widget_bootstrap_token_by_digest(
            Org, InstallationId, TokenDigest
        ),
        ?assert(is_integer(maps:get(last_seen_at, Touched))),
        ok = cs_pg_widget:revoke_widget_bootstrap_token(Org, InstallationId, TokenId, Now + 2),
        {ok, RevokedToken} = cs_pg_widget:fetch_widget_bootstrap_token_by_digest(
            Org, InstallationId, TokenDigest
        ),
        ?assert(is_integer(maps:get(revoked_at, RevokedToken))),
        {error, not_found} = cs_pg_widget:touch_widget_bootstrap_token(
            Org, InstallationId, TokenId, Now + 3
        ),
        {error, not_found} = cs_pg_widget:revoke_widget_bootstrap_token(
            Org, InstallationId, TokenId, Now + 4
        ),
        %% installation 吊销：行保留 + 状态翻转；重复吊销 not_found
        ok = cs_pg_widget:revoke_widget_installation(Org, InstallationId, Now + 5),
        {ok, RevokedInst} = cs_pg_widget:fetch_widget_installation(Org, InstallationId),
        ?assertEqual(<<"revoked">>, maps:get(status, RevokedInst)),
        ?assert(is_integer(maps:get(revoked_at, RevokedInst))),
        {error, not_found} = cs_pg_widget:revoke_widget_installation(
            Org, InstallationId, Now + 6
        )
    after
        cleanup_widget(Scope)
    end.

a04_nonce_replay_unique_conflict() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, Inst} = install(Scope),
        InstallationId = maps:get(id, Inst),
        Now = erlang:system_time(second),
        Jti = cs_access_app:default_digest(<<"csb01-jti-once">>),
        ok = cs_pg_widget:record_widget_nonce(Org, InstallationId, Jti, Now + 60),
        %% 串行重放：唯一约束裁决为 replay
        {error, replay} = cs_pg_widget:record_widget_nonce(Org, InstallationId, Jti, Now + 60),
        %% 过期负例：expires_at <= created_at ⇒ ck_cswn_expiry 23514
        {error, ErrExpiry} = ?FIX:exec(
            <<
                "INSERT INTO customer_service_widget_nonce"
                " (id, organization_id, installation_id, jti_digest, expires_at)"
                " VALUES ($1, $2, $3, 'csb01-expired-jti', to_timestamp($4))"
            >>,
            [?FIX:id(), Org, InstallationId, Now - 60]
        ),
        ?assertEqual(<<"23514">>, raw_error_code(ErrExpiry)),
        ?assertEqual(<<"ck_cswn_expiry">>, raw_error_constraint(ErrExpiry))
    after
        cleanup_widget(Scope)
    end.

a04_nonce_concurrent_uniqueness() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, Inst} = install(Scope),
        InstallationId = maps:get(id, Inst),
        Jti = cs_access_app:default_digest(<<"csb01-jti-race">>),
        ExpiresAt = erlang:system_time(second) + 60,
        Results = parallel(8, fun() ->
            cs_pg_widget:record_widget_nonce(Org, InstallationId, Jti, ExpiresAt)
        end),
        {Oks, Errors} = lists:partition(
            fun
                (ok) -> true;
                (_) -> false
            end,
            Results
        ),
        %% 并发/重放下恰好一个成功，其余全部是唯一约束裁决的 replay
        ?assertEqual(1, length(Oks)),
        ?assertEqual(7, length(Errors)),
        ?assert(lists:all(fun(E) -> E =:= {error, replay} end, Errors)),
        %% 落库恰一行
        ?assertEqual(1, widget_row_count(Org, <<"customer_service_widget_nonce">>))
    after
        cleanup_widget(Scope)
    end.

%% ===================================================================
%% A05：错误与日志无明文 secret 通道
%% ===================================================================

a05_errors_and_source_have_no_secret_channel() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        {ok, _} = install(Scope),
        %% 错误归一化只含 SQLSTATE / 约束名：撞唯一约束也不回显 digest/明文
        Digest = cs_access_app:default_digest(<<"csb01-secret-echo-probe">>),
        {ok, _} = cs_pg_widget:insert_widget_installation(Org, #{
            id => ?FIX:id(),
            organization_id => Org,
            public_widget_id => Digest,
            display_name => <<"csb01-echo">>,
            consent_version => <<"csb01-consent-v1">>
        }),
        {error, ErrDup} = cs_pg_widget:insert_widget_installation(Org, #{
            id => ?FIX:id(),
            organization_id => Org,
            public_widget_id => Digest,
            display_name => <<"csb01-echo-2">>,
            consent_version => <<"csb01-consent-v1">>
        }),
        ?assertEqual({sql, <<"23505">>, <<"uq_cswi_public_widget_id">>}, ErrDup),
        ?assertNot(error_contains(ErrDup, Digest)),
        %% 静态断言：实现模块零日志调用——不存在明文进日志的通道
        {ok, Source} = file:read_file(
            "src/features/customer_service/infrastructure/cs_pg_widget.erl"
        ),
        ?assertMatch(nomatch, re:run(Source, <<"lager:">>)),
        ?assertMatch(nomatch, re:run(Source, <<"logger:">>))
    after
        cleanup_widget(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).

%% 建一个最小 installation（本 Org）。
install(Scope) ->
    Org = org(Scope),
    cs_pg_widget:insert_widget_installation(Org, #{
        id => ?FIX:id(),
        organization_id => Org,
        public_widget_id => public_widget_id(),
        display_name => <<"csb01-widget">>,
        allowed_origins => [<<"https://shop.example.com">>],
        branding => #{},
        consent_version => <<"csb01-consent-v1">>
    }).

public_widget_id() ->
    <<"wgt_pub_csb01_", (integer_to_binary(?FIX:id()))/binary>>.

%% ---- DB 探针 ----

table_exists(Table) ->
    1 =:=
        ?FIX:scalar(
            <<
                "SELECT count(*) AS n FROM information_schema.tables"
                " WHERE table_name = $1"
            >>,
            [Table],
            -1
        ).

column_exists(Table, Column) ->
    1 =:=
        ?FIX:scalar(
            <<
                "SELECT count(*) AS n FROM information_schema.columns"
                " WHERE table_name = $1 AND column_name = $2"
            >>,
            [Table, Column],
            -1
        ).

%% 列级断言：表内不存在任何明文承载列（返回违规列名列表，空 = 通过）。
plaintext_columns(Table) ->
    Column =
        ?FIX:scalar(
            <<
                "SELECT column_name FROM information_schema.columns"
                " WHERE table_name = $1 AND (column_name LIKE '%secret%'"
                "   OR column_name LIKE '%plain%' OR column_name LIKE '%raw%'"
                "   OR column_name = 'key_material')"
                " LIMIT 1"
            >>,
            [Table],
            none
        ),
    case Column of
        none -> [];
        Name when is_binary(Name) -> [Name];
        _ -> [{probe_failed, Table}]
    end.

%% 行级明文断言：整行转 JSON 文本后搜明文（任意列都藏不住）。
row_text_contains(Table, Org, PlainText) ->
    RowJson =
        ?FIX:scalar(
            <<
                "SELECT coalesce(string_agg(t::text, ' | '), '') AS n FROM ("
                "  SELECT row_to_json(x) AS t FROM ",
                Table/binary,
                " x WHERE organization_id = $1"
                ") s"
            >>,
            [Org],
            <<>>
        ),
    case RowJson of
        <<>> -> false;
        Bin when is_binary(Bin) -> binary:match(Bin, PlainText) =/= nomatch;
        _ -> true
    end.

widget_row_count(Org, Table) ->
    ?FIX:scalar(
        <<"SELECT count(*) AS n FROM ", Table/binary, " WHERE organization_id = $1">>,
        [Org],
        -1
    ).

widget_token_count(Org) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) AS n FROM customer_service_visit_token"
            " WHERE organization_id = $1 AND widget_installation_id IS NOT NULL"
        >>,
        [Org],
        -1
    ).

%% 错误元组文本是否含明文（A05 断言用）。
error_contains(Err, PlainText) ->
    Chars = io_lib:format("~p", [Err]),
    binary:match(iolist_to_binary(Chars), PlainText) =/= nomatch.

migration_version_in_range(FileName, Min, Max) ->
    case re:run(FileName, <<"^([0-9]{8})_.*\\.(up|down)\\.sql$">>, [{capture, [1], binary}]) of
        {match, [VersionBin]} ->
            Version = binary_to_integer(VersionBin),
            Version >= Min andalso Version =< Max;
        nomatch ->
            false
    end.

%% 迁移往返：独立连接（不占池连接的 advisory 锁）。
with_migrate_conn(Fun) ->
    {ok, Conn} = epgsql:connect(config_ds:env(super_account)),
    try
        Fun(Conn)
    after
        _ = epgsql:close(Conn)
    end.

%% Widget 域清场：先删 visit_token 上的 widget 令牌（RESTRICT 指向
%% installation），再删 nonce → identity_key → installation，最后走夹具
%% 标准清场（其 purge 列表不含 widget 表）。
cleanup_widget(Scope) ->
    Orgs = [maps:get(org_id, Scope), maps:get(other_org_id, Scope, undefined)],
    lists:foreach(
        fun(Org) when is_integer(Org) ->
            _ = ?FIX:exec(
                <<
                    "DELETE FROM customer_service_visit_token"
                    " WHERE organization_id = $1 AND widget_installation_id IS NOT NULL"
                >>,
                [Org]
            ),
            lists:foreach(
                fun(Table) ->
                    _ = ?FIX:exec(
                        <<"DELETE FROM ", Table/binary, " WHERE organization_id = $1">>, [Org]
                    )
                end,
                [
                    <<"customer_service_widget_nonce">>,
                    <<"customer_service_widget_identity_key">>,
                    <<"customer_service_widget_installation">>
                ]
            )
        end,
        Orgs
    ),
    ?FIX:cleanup(Scope).

%% epgsql #error{} 的取值辅助（?FIX:exec 返回原始错误；形状同 cs_pg_tests）。
raw_error_code({error, _Severity, Code, _Codename, _Message, _Extra}) -> Code;
raw_error_code(_Other) -> undefined.

raw_error_constraint({error, _Severity, _Code, _Codename, _Message, Extra}) when
    is_list(Extra)
->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> undefined
    end;
raw_error_constraint(_Other) ->
    undefined.

%% 真并发：全部进程就绪后同时放行（同款手法见 cs_pg_tests:parallel/2）。
parallel(N, Fun) ->
    Parent = self(),
    Go = make_ref(),
    Pids = [
        spawn(fun() ->
            receive
                Go -> ok
            after 30000 -> ok
            end,
            Parent ! {self(), run(Fun)}
        end)
     || _ <- lists:seq(1, N)
    ],
    [Pid ! Go || Pid <- Pids],
    [
        receive
            {Pid, Result} -> Result
        after 60000 -> timeout
        end
     || Pid <- Pids
    ].

run(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {crashed, Class, Reason}
    end.
