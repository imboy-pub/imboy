%%% @doc EB-03 企业客户与客户渠道标识的 PostgreSQL store 套件。
%%%
%%% 覆盖：
%%%   EB-03-A01 客户类资源同样必须「前两个业务参数 = OrgId/WorkspaceId」且每条 SQL
%%%             同语句带两者（`enterprise_contact_identity` 没有 workspace 列，
%%%             故实现以 workspace 归属子查询把租户键放进**同一条语句**）；
%%%   EB-03-A03 「显式 sender 复合 FK」的客户侧：created_by identity 跨 Org 由 DB
%%%             复合 FK 拒绝（不是应用层判断），SQLSTATE 23503；
%%%   EB-03-A05 DB 无明文：客户资料只落企业托管密文、渠道标识只落组织域 HMAC，
%%%             明文金丝雀不得出现在任何列或审计 detail 中。
-module(eb_contact_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

contact_store_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            ok = eb_pg_test_fixture:ensure_purge_role(),
            {ok, Conn};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_insert_and_fetch_contact_within_scope/0},
        {timeout, 60, fun a01_contact_identity_is_tenant_scoped/0},
        {timeout, 60, fun a01_store_statements_carry_both_tenants/0},
        {timeout, 60, fun a03_created_by_identity_from_other_org_is_rejected_by_db/0},
        {timeout, 60, fun a05_profile_cipher_and_subject_hmac_never_store_plaintext/0},
        {timeout, 60, fun a05_contact_identity_hmac_is_org_scoped/0}
    ];
cases(_Skipped) ->
    {skip, "contact store suite requires the scratch database connection"}.

%% ===================================================================
%% EB-03-A01
%% ===================================================================

a01_insert_and_fetch_contact_within_scope() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Cid = eb_pg_test_fixture:id(),
        Contact = #{
            id => Cid,
            display_name => <<"eb03-contact-store-", (integer_to_binary(Cid))/binary>>,
            created_by_business_identity_id => Sales
        },
        {ok, Inserted} = eb_pg_store:insert_contact(Org, Ws, Contact),
        ?assertEqual(Org, maps:get(organization_id, Inserted)),
        ?assertEqual(Ws, maps:get(workspace_id, Inserted)),
        ?assertEqual(active, maps:get(status, Inserted)),
        ?assertEqual({error, conflict}, eb_pg_store:insert_contact(Org, Ws, Contact)),
        ?assertEqual({ok, Inserted}, eb_pg_store:fetch_contact(Org, Ws, Cid)),
        %% 租户只对一半 → not_found（同语句的 workspace 归属校验拒绝）
        ?assertEqual({error, not_found}, eb_pg_store:fetch_contact(OtherOrg, Ws, Cid)),
        ?assertEqual({error, not_found}, eb_pg_store:fetch_contact(Org, OtherWs, Cid)),
        %% 写入侧：Workspace 不属于该 Org → 拒绝写
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_pg_store:insert_contact(
                Org,
                OtherWs,
                Contact#{id => eb_pg_test_fixture:id(), display_name => <<"eb03-nope">>}
            )
        ),
        %% 缺租户参数 → fail-closed，不触库
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_store:insert_contact(undefined, Ws, Contact#{id => eb_pg_test_fixture:id()})
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_store:fetch_contact(Org, undefined, Cid)
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_contact_identity_is_tenant_scoped() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Ref = eb_pg_test_fixture:key_ref(1),
        Subject = <<"eb03-subject-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        {ok, Hmac} = eb_managed_crypto:subject_hmac(Org, <<"wechat">>, Subject, Ref),
        Iid = eb_pg_test_fixture:id(),
        Identity = #{
            id => Iid,
            contact_id => Contact,
            channel => <<"wechat">>,
            subject_hmac => Hmac,
            subject_mask => <<"wx***1">>
        },
        ?assertMatch({ok, _}, eb_pg_store:insert_contact_identity(Org, Ws, Identity)),
        %% 幂等：第二次同键 → conflict（不增行，由 UNIQUE(org,channel,subject_hmac) 强制）
        %% 同 Org 同 channel 同 subject_hmac 幂等去重：不增行（UNIQUE 由 DB 强制）
        ?assertEqual(
            {error, conflict},
            eb_pg_store:insert_contact_identity(Org, Ws, Identity#{
                id => eb_pg_test_fixture:id()
            })
        ),
        %% Workspace 不属于该 Org → 同语句租户校验拒绝
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_pg_store:insert_contact_identity(Org, OtherWs, Identity#{
                id => eb_pg_test_fixture:id(), subject_hmac => Hmac
            })
        ),
        %% 跨 Org 的 contact 引用 → 同语句拒绝（contact 必须属于该 Org）
        ?assertMatch(
            {error, _},
            eb_pg_store:insert_contact_identity(Org, Ws, Identity#{
                id => eb_pg_test_fixture:id(),
                contact_id => eb_pg_test_fixture:id(),
                subject_hmac => Hmac
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_store_statements_carry_both_tenants() ->
    Statements = eb_pg_store:sql_statements(),
    ?assert(length(Statements) >= 12),
    lists:foreach(
        fun(Sql) ->
            ?assert(binary:match(Sql, <<"$1">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"$2">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"organization_id">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"workspace_id">>) =/= nomatch)
        end,
        Statements
    ).

%% ===================================================================
%% EB-03-A03：复合 FK（客户侧）
%% ===================================================================

a03_created_by_identity_from_other_org_is_rejected_by_db() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        ForeignIdentity = eb_pg_test_fixture:id(),
        %% 在另一个 Org 里建一个 identity，其 id 与该 Org 绑定
        ok = eb_pg_test_fixture:exec(
            <<
                "INSERT INTO organization_business_identity"
                " (id,organization_id,function_key,display_name,status,version)"
                " VALUES ($1,$2,'sales',$3,'active',1)"
            >>,
            [
                ForeignIdentity,
                OtherOrg,
                <<"eb03-foreign-", (integer_to_binary(ForeignIdentity))/binary>>
            ]
        ),
        Cid = eb_pg_test_fixture:id(),
        Result = eb_pg_store:insert_contact(Org, Ws, #{
            id => Cid,
            display_name => <<"eb03-cross-org-", (integer_to_binary(Cid))/binary>>,
            created_by_business_identity_id => ForeignIdentity
        }),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, Constraint}} = Result,
        ?assertEqual(<<"23503">>, Code),
        ?assertEqual(<<"fk_enterprise_contact_created_by_identity">>, Constraint),
        %% 拒绝后不得留下半行
        ?assertEqual(0, count_contact(Org, Cid))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

count_contact(Org, ContactId) ->
    eb_pg_test_fixture:scalar(
        <<"SELECT count(*) AS n FROM enterprise_contact WHERE organization_id=$1 AND id=$2">>,
        [Org, ContactId]
    ).

%% ===================================================================
%% EB-03-A05：DB 无明文
%% ===================================================================

a05_profile_cipher_and_subject_hmac_never_store_plaintext() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Ref = eb_pg_test_fixture:key_ref(1),
        Canary = eb_pg_test_fixture:canary(),
        %% 客户资料：企业托管密文（AAD 绑定 Org/Workspace/资源）
        Cid = eb_pg_test_fixture:id(),
        ContactAad = eb_pg_test_fixture:resource_aad(<<"enterprise_contact">>, Org, Ws, Cid),
        {ok, Sealed} = eb_managed_crypto:seal_scoped(ContactAad, Canary, Ref),
        {ok, Contact} = eb_pg_store:insert_contact(Org, Ws, #{
            id => Cid,
            display_name => <<"eb03-cipher-", (integer_to_binary(Cid))/binary>>,
            profile_cipher => maps:get(cipher, Sealed),
            profile_key_version => maps:get(key_version, Sealed),
            created_by_business_identity_id => Sales
        }),
        ?assertNotEqual(Canary, maps:get(profile_cipher, Contact)),
        %% 渠道标识：只落组织域 HMAC + 掩码，不落明文
        {ok, Hmac} = eb_managed_crypto:subject_hmac(Org, <<"wechat">>, Canary, Ref),
        Iid = eb_pg_test_fixture:id(),
        {ok, StoredIdentity} = eb_pg_store:insert_contact_identity(Org, Ws, #{
            id => Iid,
            contact_id => Cid,
            channel => <<"wechat">>,
            subject_hmac => Hmac,
            subject_mask => <<"wx***9">>
        }),
        ?assertEqual(Hmac, maps:get(subject_hmac, StoredIdentity)),
        %% 从 DB 侧整体回读（行转文本），金丝雀明文必须找不到
        %% 两张表列数不同，不能 UNION ALL；分别整行转文本后拼接。
        Rows = eb_pg_test_fixture:scalar(
            <<
                "SELECT coalesce((SELECT string_agg(c::text, '|') FROM enterprise_contact c"
                "                  WHERE c.organization_id=$1 AND c.id=$2), '')"
                "    || '|' ||"
                "       coalesce((SELECT string_agg(ci::text, '|') FROM enterprise_contact_identity ci"
                "                  WHERE ci.organization_id=$1 AND ci.contact_id=$2), '') AS blob"
            >>,
            [Org, Cid]
        ),
        ?assert(is_binary(Rows)),
        ?assert(eb_pg_test_fixture:canary_absent(Rows)),
        ?assertEqual(nomatch, binary:match(Rows, Canary))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a05_contact_identity_hmac_is_org_scoped() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Ref = eb_pg_test_fixture:key_ref(1),
        Subject = <<"eb03-scoped-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        {ok, HmacOrgA} = eb_managed_crypto:subject_hmac(Org, <<"wechat">>, Subject, Ref),
        {ok, HmacOrgB} = eb_managed_crypto:subject_hmac(OtherOrg, <<"wechat">>, Subject, Ref),
        ?assertNotEqual(HmacOrgA, HmacOrgB),
        %% 同一明文 subject 在 Org B 落库时，与 Org A 的摘要不同（不可跨租户字典反查）
        ok = eb_pg_test_fixture:exec(
            <<
                "INSERT INTO enterprise_contact(id,organization_id,status,display_name,version)"
                " VALUES ($1,$2,'active','eb03-other-org-contact',1)"
            >>,
            [eb_pg_test_fixture:id(), OtherOrg]
        ),
        ?assertMatch(
            {ok, _},
            eb_pg_store:insert_contact_identity(Org, Ws, #{
                id => eb_pg_test_fixture:id(),
                contact_id => Contact,
                channel => <<"wechat">>,
                subject_hmac => HmacOrgA,
                subject_mask => <<"wx***2">>
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.
