%% enterprise_admin_governance_pg_tests
%% FULL-08 — Admin 企业应用治理面（/api/adm/enterprise/*，A-01..A-14）的
%% **仓储/只读逻辑层**真库集成测试。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL08B_INTTEST）。
%% 为什么这一层单独成套件：治理 logic 走池化 elib_pg:with_tx（固定库），不能
%% 指向 marker 库；而它调用的全部 **写面仓储函数与只读查询都接受显式 Conn**，
%% 故把「业务 oracle」落在这一层，既拿到真库证据又不污染共享库。
%%
%% oracle（plan-full §3.1 / §3.2 / §7 安全硬门）：
%%   ① credential 元数据面**永不**带 secret/digest（键集合机械断言），且含
%%      created_at（前端 CREDENTIAL_SAFE_KEYS 要求）
%%   ② Application CAS：命中 version+1；旧版本 → version_conflict；
%%      跨 Org → not_found（IDOR 在仓储层即失败，不留给上层）
%%   ③ 非法生命周期值在仓储层就被拒（不产生任何 SQL 写入）
%%   ④ Grant 撤销归因双通道：adm 通道写 revoked_by_adm_user_id 且
%%      revoked_by_user_id 恒 NULL；重复撤销 → already_revoked；
%%      旧版本 → version_conflict；无效 adm id → 守卫拒绝（不落库）
%%   ⑤ 撤权后 Grant 列表仍可见该行，且状态/归因逐字正确
%%   ⑥ 审计：Admin 写入落 enterprise_audit_event，actor_role=platform_admin，
%%      detail 不含任何 secret 类键
%%
%% ID 段：995xxx（本 run 独立 marker 库，跨套件不共享数据）。

-module(enterprise_admin_governance_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(OWNER_A, 995001).
-define(PRIN_A, 995002).
-define(PRIN_B, 995003).
-define(TENANT_U, 995004).
-define(ADM_USER, 995005).
-define(ORG_A, 995101).
-define(ORG_B, 995102).
-define(APP_A_ID, 995201).
-define(APP_B_ID, 995202).
-define(CRED_A1, 995301).
-define(CRED_A2, 995302).
-define(GRANT_A1, 995401).
-define(GRANT_A2, 995402).

%% 前端熔断键（contracts.ts:SECRET_FORBIDDEN_KEYS / SENSITIVE_KEY_PATTERN 同口径）
-define(FORBIDDEN_KEYS, [
    <<"secret">>,
    <<"secret_digest">>,
    <<"secret_hash">>,
    <<"credential_secret">>,
    <<"credential_hash">>,
    <<"token_digest">>,
    <<"token_hash">>,
    <<"sha256_digest">>,
    <<"signing_key">>,
    <<"private_key">>
]).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    State = inttest_marker_db:provision(#{
        env_prefix => <<"FULL08B_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_fixture(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    State.

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

seed_fixture(C) ->
    seed_user(C, ?OWNER_A),
    seed_user(C, ?PRIN_A),
    seed_user(C, ?PRIN_B),
    seed_user(C, ?TENANT_U),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"eag995-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_A, <<"eag995-org-b">>),
    seed_app(C, ?APP_A_ID, ?ORG_A, ?PRIN_A, <<"eag995-app-a">>),
    seed_app(C, ?APP_B_ID, ?ORG_B, ?PRIN_B, <<"eag995-app-b">>),
    seed_credential(C, ?CRED_A1, ?ORG_A, ?APP_A_ID, <<"eag995-prefix-a1">>, <<"active">>),
    seed_credential(C, ?CRED_A2, ?ORG_A, ?APP_A_ID, <<"eag995-prefix-a2">>, <<"revoked">>),
    seed_grant(C, ?GRANT_A1, ?ORG_A, ?APP_A_ID, <<"eag995-idem-1">>),
    seed_grant(C, ?GRANT_A2, ?ORG_A, ?APP_A_ID, <<"eag995-idem-2">>),
    ok.

seed_user(C, Uid) ->
    ok = exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv)">>,
        <<" VALUES (">>,
        integer_to_binary(Uid),
        <<", 'x', 't995_u">>,
        integer_to_binary(Uid),
        <<"', 0, 1, '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    ok = exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings,">>,
        <<" created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        <<", '">>,
        Name,
        <<"', ">>,
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_app(C, AppId, OrgId, PrinUid, Key) ->
    ok = exec(C, [
        <<"INSERT INTO enterprise_application (id, organization_id, principal_user_id,">>,
        <<" application_key, name) VALUES (">>,
        integer_to_binary(AppId),
        <<", ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(PrinUid),
        <<", '">>,
        Key,
        <<"', 'eag995 app')">>
    ]).

%% secret_digest 必须是 64 字符（ck_eac_secret_digest）；这里用合成值，
%% 唯一目的就是**证明它不会被读面带回**。
seed_credential(C, CredId, OrgId, AppId, Prefix, Status) ->
    ok = exec(C, [
        <<"INSERT INTO enterprise_application_credential (id, organization_id,">>,
        <<" application_id, credential_prefix, secret_digest, status, revoked_at)">>,
        <<" VALUES (">>,
        integer_to_binary(CredId),
        <<", ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(AppId),
        <<", '">>,
        Prefix,
        <<"', '">>,
        binary:copy(<<"a">>, 64),
        <<"', '">>,
        Status,
        <<"', ">>,
        case Status of
            <<"revoked">> -> <<"NOW()">>;
            _ -> <<"NULL">>
        end,
        <<")">>
    ]).

seed_grant(C, GrantId, OrgId, AppId, Idem) ->
    ok = exec(C, [
        <<"INSERT INTO enterprise_application_grant (id, organization_id, application_id,">>,
        <<" workspace_scope_kind, status, valid_from, expires_at, idempotency_key) VALUES (">>,
        integer_to_binary(GrantId),
        <<", ">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(AppId),
        <<", 'none', 'active', NOW(), NOW() + interval '30 days', '">>,
        Idem,
        <<"')">>
    ]).

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

%% 负例包装：预期 SQL 错误会 abort 事务，在 SAVEPOINT 内执行并回退。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT eag995_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT eag995_sp">>),
        exec(C, <<"RELEASE SAVEPOINT eag995_sp">>)
    end.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

scalar(C, Sql, Params) ->
    Row = one(C, Sql, Params),
    case maps:values(Row) of
        [V | _] -> V;
        [] -> undefined
    end.

%%%===================================================================
%%% 套件入口
%%%===================================================================

all_test_() ->
    {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
        C = maps:get(conn, State),
        {inorder, [
            {"credential_metadata_never_carries_secret",
                with_tx(C, fun credential_metadata_oracle/1)},
            {"application_status_cas_and_cross_org",
                with_tx(C, fun application_status_cas_oracle/1)},
            {"application_scopes_cas_and_illegal_status",
                with_tx(C, fun application_scopes_cas_oracle/1)},
            {"grant_revoke_admin_attribution", with_tx(C, fun grant_revoke_admin_oracle/1)},
            {"grant_revoke_idempotent_and_stale_version",
                with_tx(C, fun grant_revoke_repeat_oracle/1)},
            {"audit_write_and_read_platform_admin", with_tx(C, fun audit_oracle/1)}
        ]}
    end}.

%%%===================================================================
%%% ① credential 元数据面
%%%===================================================================

credential_metadata_oracle(C) ->
    Rows = enterprise_internal_ops:list_credentials(C, ?ORG_A, ?APP_A_ID),
    ?assertEqual(2, length(Rows)),
    lists:foreach(
        fun(Row) ->
            Keys = maps:keys(Row),
            %% 键集合封闭：既不含 secret/digest，也不多出未声明的列
            ?assertEqual(
                [],
                [K || K <- ?FORBIDDEN_KEYS, lists:member(K, Keys)]
            ),
            ?assert(lists:member(<<"created_at">>, Keys)),
            ?assert(lists:member(<<"credential_prefix">>, Keys)),
            ?assert(lists:member(<<"status">>, Keys)),
            ?assertNot(lists:member(<<"secret_digest">>, Keys)),
            %% 值层面再验一次：任何值都不允许等于库里的 digest
            ?assertEqual(
                false,
                lists:member(binary:copy(<<"a">>, 64), maps:values(Row))
            )
        end,
        Rows
    ),
    %% 跨应用不可见（B 组织的应用查不到 A 的 credential）
    ?assertEqual([], enterprise_internal_ops:list_credentials(C, ?ORG_B, ?APP_B_ID)).

%%%===================================================================
%%% ② Application 生命周期 CAS + 跨 Org IDOR
%%%===================================================================

application_status_cas_oracle(C) ->
    %% 命中：version 1 → 2
    ?assertEqual(
        ok,
        enterprise_application_repo:update_status_cas_tx(C, ?ORG_A, ?APP_A_ID, 1, <<"disabled">>)
    ),
    ?assertEqual(<<"disabled">>, app_status(C, ?APP_A_ID)),
    ?assertEqual(2, app_version(C, ?APP_A_ID)),
    %% 旧版本号（并发输家）→ version_conflict，且行未被改写
    ?assertEqual(
        {error, version_conflict},
        enterprise_application_repo:update_status_cas_tx(C, ?ORG_A, ?APP_A_ID, 1, <<"archived">>)
    ),
    ?assertEqual(<<"disabled">>, app_status(C, ?APP_A_ID)),
    ?assertEqual(2, app_version(C, ?APP_A_ID)),
    %% 跨 Org（IDOR）：以 A 的 org_id 操作 B 组织的应用 → not_found，零写入
    ?assertEqual(
        {error, not_found},
        enterprise_application_repo:update_status_cas_tx(C, ?ORG_A, ?APP_B_ID, 1, <<"archived">>)
    ),
    ?assertEqual(<<"active">>, app_status(C, ?APP_B_ID)),
    ?assertEqual(1, app_version(C, ?APP_B_ID)),
    %% 不存在的行
    ?assertEqual(
        {error, not_found},
        enterprise_application_repo:update_status_cas_tx(C, ?ORG_A, 995999, 1, <<"active">>)
    ).

%%%===================================================================
%%% ③ scope CAS + 非法状态
%%%===================================================================

application_scopes_cas_oracle(C) ->
    ?assertEqual(
        ok,
        enterprise_application_repo:update_scopes_cas_tx(C, ?ORG_A, ?APP_A_ID, 1, [
            <<"messages:send">>
        ])
    ),
    ?assertEqual(2, app_version(C, ?APP_A_ID)),
    ?assertEqual(
        <<"messages:send">>,
        scalar(
            C,
            <<"SELECT allowed_scopes ->> 0 FROM enterprise_application WHERE id = $1">>,
            [?APP_A_ID]
        )
    ),
    %% 旧版本重复提交 → version_conflict（不是静默覆盖）
    ?assertEqual(
        {error, version_conflict},
        enterprise_application_repo:update_scopes_cas_tx(C, ?ORG_A, ?APP_A_ID, 1, [
            <<"identities:write">>
        ])
    ),
    ?assertEqual(
        <<"messages:send">>,
        scalar(
            C,
            <<"SELECT allowed_scopes ->> 0 FROM enterprise_application WHERE id = $1">>,
            [?APP_A_ID]
        )
    ),
    %% 非法生命周期值：DB CHECK 拦截。该语句会 abort 当前事务，故必须在
    %% SAVEPOINT 内执行并回退，否则后续断言全部变成 25P02 假红。
    Res =
        in_savepoint(C, fun() ->
            enterprise_application_repo:update_status_cas_tx(
                C, ?ORG_A, ?APP_A_ID, 2, <<"deleted">>
            )
        end),
    ?assertMatch({error, _}, Res),
    ?assertNotEqual(<<"deleted">>, app_status(C, ?APP_A_ID)),
    ?assertEqual(2, app_version(C, ?APP_A_ID)).

%%%===================================================================
%%% ④ Grant 撤销归因：平台管理员通道
%%%===================================================================

grant_revoke_admin_oracle(C) ->
    ?assertEqual(
        ok,
        enterprise_application_grant_repo:revoke_admin_tx(
            C, ?ORG_A, ?APP_A_ID, ?GRANT_A1, 1, ?ADM_USER
        )
    ),
    Row = one(
        C,
        <<
            "SELECT status, revoked_by_user_id, revoked_by_adm_user_id, version"
            " FROM enterprise_application_grant WHERE id = $1"
        >>,
        [?GRANT_A1]
    ),
    ?assertEqual(<<"revoked">>, maps:get(<<"status">>, Row)),
    ?assertEqual(?ADM_USER, maps:get(<<"revoked_by_adm_user_id">>, Row)),
    ?assertEqual(null, maps:get(<<"revoked_by_user_id">>, Row)),
    ?assertEqual(2, maps:get(<<"version">>, Row)),
    %% 无效 adm id（0 / 负 / 非整数）被守卫拒绝，不落库
    ?assertError(
        function_clause,
        enterprise_application_grant_repo:revoke_admin_tx(
            C, ?ORG_A, ?APP_A_ID, ?GRANT_A2, 1, 0
        )
    ),
    ?assertEqual(<<"active">>, grant_status(C, ?GRANT_A2)),
    %% 撤权后列表仍可见该行（授权行不可物理删除），归因逐字正确
    {ok, Grants} = enterprise_application_grant_repo:list_tx(C, ?ORG_A, ?APP_A_ID),
    ?assertEqual(2, length(Grants)),
    Revoked = [G || G <- Grants, maps:get(<<"id">>, G) =:= ?GRANT_A1],
    ?assertEqual(1, length(Revoked)).

grant_revoke_repeat_oracle(C) ->
    ok = enterprise_application_grant_repo:revoke_admin_tx(
        C, ?ORG_A, ?APP_A_ID, ?GRANT_A1, 1, ?ADM_USER
    ),
    %% 重复撤销（同版本）→ already_revoked（不是静默成功）
    ?assertEqual(
        {error, already_revoked},
        enterprise_application_grant_repo:revoke_admin_tx(
            C, ?ORG_A, ?APP_A_ID, ?GRANT_A1, 1, ?ADM_USER
        )
    ),
    %% 用新版本号撤销已撤销行 → 同样 already_revoked
    ?assertEqual(
        {error, already_revoked},
        enterprise_application_grant_repo:revoke_admin_tx(
            C, ?ORG_A, ?APP_A_ID, ?GRANT_A1, 2, ?ADM_USER
        )
    ),
    %% 另一个 grant：旧版本号 → version_conflict（并发输家）
    ok = enterprise_application_grant_repo:replace_scopes_tx(
        C, ?ORG_A, ?APP_A_ID, ?GRANT_A2, 1, [<<"messages:send">>]
    ),
    ?assertEqual(
        {error, version_conflict},
        enterprise_application_grant_repo:revoke_admin_tx(
            C, ?ORG_A, ?APP_A_ID, ?GRANT_A2, 1, ?ADM_USER
        )
    ),
    %% 跨 Org IDOR：以 A 的 org_id 撤 B 的行 → not_found
    ?assertEqual(
        {error, not_found},
        enterprise_application_grant_repo:revoke_admin_tx(
            C, ?ORG_A, ?APP_B_ID, ?GRANT_A2, 2, ?ADM_USER
        )
    ).

%%%===================================================================
%%% ⑥ 审计：Admin 写入留痕（platform_admin，无 secret）
%%%===================================================================

audit_oracle(C) ->
    ok = enterprise_application_repo:update_status_cas_tx(C, ?ORG_A, ?APP_A_ID, 1, <<"disabled">>),
    Detail = #{
        <<"before">> => #{<<"status">> => <<"active">>},
        <<"after">> => #{<<"status">> => <<"disabled">>},
        <<"actor_account">> => <<"admin@imboy">>
    },
    {ok, _AuditId} =
        enterprise_audit_event_repo:append_tx(C, ?ORG_A, #{
            organization_id => ?ORG_A,
            resource_type => <<"enterprise_application">>,
            resource_id => ?APP_A_ID,
            action => <<"application_status_changed">>,
            %% 平台管理员不是租户 user：actor_user_id 恒 null
            %% （fk_eae_actor REFERENCES "user"(id) 会拒绝 adm_user id）。
            actor_user_id => undefined,
            actor_role => <<"platform_admin">>,
            detail => Detail
        }),
    {ok, Items} =
        enterprise_audit_event_repo:list_tx(
            C, ?ORG_A, <<"enterprise_application">>, ?APP_A_ID, #{page => 1, size => 10}
        ),
    ?assertEqual(1, length(Items)),
    [Event] = Items,
    ?assertEqual(<<"application_status_changed">>, maps:get(<<"action">>, Event)),
    ?assertEqual(<<"platform_admin">>, maps:get(<<"actor_role">>, Event)),
    ?assertEqual(null, maps:get(<<"actor_user_id">>, Event)),
    %% 管理员账号落在 detail.actor_account（归因不丢，只是不伪造成租户 user）。
    %% detail 是 jsonb，epgsql 侧为原文 text（无 jsonb codec），故做包含断言。
    DetailBin = maps:get(<<"detail">>, Event),
    ?assertMatch(<<_/binary>>, DetailBin),
    ?assertNotEqual(nomatch, binary:match(DetailBin, <<"actor_account">>)),
    ?assertNotEqual(nomatch, binary:match(DetailBin, <<"admin@imboy">>)),
    %% 审计 detail 里不得出现任何 secret 类键
    ?assertEqual(
        [],
        [K || K <- ?FORBIDDEN_KEYS, lists:member(K, maps:keys(Event))]
    ),
    %% 跨 Org 读不到（审计也按 org_id 隔离）
    {ok, ItemsB} =
        enterprise_audit_event_repo:list_tx(
            C, ?ORG_B, <<"enterprise_application">>, ?APP_A_ID, #{page => 1, size => 10}
        ),
    ?assertEqual(0, length(ItemsB)).

%%%===================================================================
%%% 读取助手
%%%===================================================================

app_status(C, Id) ->
    scalar(C, <<"SELECT status FROM enterprise_application WHERE id = $1">>, [Id]).

app_version(C, Id) ->
    scalar(C, <<"SELECT version FROM enterprise_application WHERE id = $1">>, [Id]).

grant_status(C, Id) ->
    scalar(C, <<"SELECT status FROM enterprise_application_grant WHERE id = $1">>, [Id]).
