%%% @doc EB-05 企业客户（enterprise contact）的 application 用例套件（真库）。
%%%
%%% 覆盖作业书 §5 的 A02 / A03 / A04（contact 侧）：
%%%   * A02（计划点名红线）：**企业好友不产生个人 `user_friend` 行**。
%%%     两端都断：① 企业侧关系确实建立（`enterprise_contact` +
%%%     `enterprise_contact_identity` 真落库）；② `user_friend` / `msg_c2c` /
%%%     `attachment` 行数增量 **0**（不是只看「没调用 user_friend*」的静态证据）。
%%%   * A03：actor 变化不改变 Org owner（客户行 organization_id 恒为 Org，
%%%     actor 只落在审计字段）。
%%%   * A04：跨 Org / 跨 Workspace / 非法 channel / 缺密钥负例零副作用。
%%%
%%% 隔离与命名同 `eb_identity_app_tests`（随机 TSID 合成租户、`eb05-` 前缀）。
-module(eb_contact_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).

contact_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a02_enterprise_friend_creates_no_personal_friend_rows/0},
        {timeout, 60, fun a02_duplicate_subject_is_idempotent_and_leaves_no_orphan/0},
        {timeout, 60, fun a02_profile_plaintext_never_reaches_db/0},
        {timeout, 60, fun a03_actor_changes_do_not_change_contact_owner/0},
        {timeout, 60, fun a04_cross_org_and_invalid_inputs_have_zero_side_effects/0},
        {timeout, 60, fun a07_get_contact_is_tenant_scoped/0},
        {timeout, 60, fun a07_list_contacts_returns_own_org_only_and_cross_org_is_empty/0},
        {timeout, 60, fun a07_list_contacts_keyset_is_not_offset/0},
        {timeout, 60, fun a08_update_contact_persists_and_is_readable_back/0},
        {timeout, 60, fun a08_update_contact_cross_org_fails_with_zero_side_effects/0},
        {timeout, 60, fun a09_assign_contact_inserts_row_and_reads_back/0},
        {timeout, 60, fun a09_assign_contact_cross_org_fails_with_zero_side_effects/0}
    ];
cases({error, Reason}) ->
    erlang:error({eb05_contact_suite_db_unavailable, Reason}).

%% ===================================================================
%% EB-05-A02：企业好友不产生个人 friend 行
%% ===================================================================

a02_enterprise_friend_creates_no_personal_friend_rows() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        Subject = subject(<<"a02-im/">>),
        %% 个人关系世界基线（企业动作前）
        PersonalBefore = personal_table_counts(),
        FriendRowsBefore = personal_friend_rows(Peer),
        %% 「添加企业好友」：把 IMBoy 账号 Peer 作为企业客户加入（含渠道标识）
        {ok, Result} = eb_contact_app:create_contact(Org, #{
            workspace_id => Ws,
            channel => <<"imboy">>,
            subject => Subject,
            display_name => <<"eb05-contact-a02">>,
            imboy_user_id => Peer,
            created_by_business_identity_id => Sales,
            actor_user_id => Actor,
            key_ref => ?FIX:key_ref(1)
        }),
        Contact = maps:get(contact, Result),
        Identity = maps:get(contact_identity, Result),
        %% ① 企业侧关系确实建立
        ?assertEqual(Org, maps:get(organization_id, Contact)),
        ?assertEqual(Ws, maps:get(workspace_id, Contact)),
        ?assertEqual(active, maps:get(status, Contact)),
        ?assertEqual(Peer, maps:get(imboy_user_id, Contact)),
        ?assertEqual(Org, maps:get(organization_id, Identity)),
        ?assertEqual(maps:get(id, Contact), maps:get(contact_id, Identity)),
        ?assertEqual(<<"imboy">>, maps:get(channel, Identity)),
        ?assertEqual(64, byte_size(maps:get(subject_hmac, Identity))),
        ?assertEqual(1, contact_rows(Org, maps:get(id, Contact))),
        ?assertEqual(1, contact_identity_rows(Org, maps:get(id, Identity))),
        %% ② 个人 friend / c2c / 附件行数增量必须为 0（两端都断）
        ?assertEqual(0, maps:get(personal_friend_rows, Result)),
        ?assertEqual(FriendRowsBefore, personal_friend_rows(Peer)),
        ?assertEqual(PersonalBefore, personal_table_counts()),
        %% ③ 企业客户的渠道标识里不留明文 subject（只留组织域 HMAC）
        Blob = contact_blob(Org, maps:get(id, Contact), maps:get(id, Identity)),
        ?assertEqual(nomatch, binary:match(Blob, Subject))
    after
        ?FIX:cleanup(Scope)
    end.

a02_duplicate_subject_is_idempotent_and_leaves_no_orphan() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        Params = #{
            workspace_id => Ws,
            channel => <<"wechat">>,
            subject => subject(<<"a02-dup/">>),
            display_name => <<"eb05-contact-dup">>,
            created_by_business_identity_id => Sales,
            key_ref => ?FIX:key_ref(1)
        },
        {ok, First} = eb_contact_app:create_contact(Org, Params),
        FirstContactId = maps:get(id, maps:get(contact, First)),
        ContactsBefore = contact_count(Org),
        IdentitiesBefore = contact_identity_rows_org(Org),
        PersonalBefore = personal_table_counts(),
        FriendRowsBefore = personal_friend_rows(Peer),
        %% 同一 Org + channel + subject 重复添加 → 幂等拒绝，且不新增任何行（无孤儿 contact）
        ?assertEqual(
            {error, {contact_exists, FirstContactId}},
            eb_contact_app:create_contact(Org, Params)
        ),
        ?assertEqual(ContactsBefore, contact_count(Org)),
        ?assertEqual(IdentitiesBefore, contact_identity_rows_org(Org)),
        ?assertEqual(PersonalBefore, personal_table_counts()),
        ?assertEqual(FriendRowsBefore, personal_friend_rows(Peer))
    after
        ?FIX:cleanup(Scope)
    end.

a02_profile_plaintext_never_reaches_db() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Canary = canary(),
        {ok, Result} = eb_contact_app:create_contact(Org, #{
            workspace_id => Ws,
            channel => <<"phone">>,
            subject => subject(<<"a02-profile/">>),
            display_name => <<"eb05-contact-profile">>,
            created_by_business_identity_id => Sales,
            profile_plaintext => Canary,
            key_ref => ?FIX:key_ref(1)
        }),
        Contact = maps:get(contact, Result),
        ?assertNotEqual(Canary, maps:get(profile_cipher, Contact)),
        ?assertEqual(1, maps:get(profile_key_version, Contact)),
        Blob = contact_blob(
            Org, maps:get(id, Contact), maps:get(id, maps:get(contact_identity, Result))
        ),
        ?assertEqual(nomatch, binary:match(Blob, Canary)),
        %% 缺主密钥 → fail-closed（不降级为明文、不写行）。
        %% 显式隔离 env keyring：EUNIT_CONFIG 可能注入 eb_enterprise_keyring
        %% （F6 合成密钥），此时缺省 key_ref 合法解析成功——那是另一条产品
        %% 路径。本场景验证的是「env 无 keyring ⇒ fail-closed」，故 mock 掉
        %% env 解析面返回 undefined，不依赖「全局环境恰好无 keyring」的假设。
        catch meck:unload(eb_env_keyring),
        ok = meck:new(eb_env_keyring, [passthrough, no_link]),
        meck:expect(eb_env_keyring, resolve_key_ref, fun(_Any) -> undefined end),
        Before = contact_count(Org),
        ?assertEqual(
            {error, missing_key},
            eb_contact_app:create_contact(Org, #{
                workspace_id => Ws,
                channel => <<"email">>,
                subject => subject(<<"a02-nokey/">>),
                display_name => <<"eb05-contact-nokey">>,
                profile_plaintext => Canary
            })
        ),
        ?assertEqual(Before, contact_count(Org))
    after
        catch meck:unload(eb_env_keyring),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A03：actor 变化不改变 Org owner（客户侧）
%% ===================================================================

a03_actor_changes_do_not_change_contact_owner() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        {ok, ByOwner} = eb_contact_app:create_contact(Org, #{
            workspace_id => Ws,
            channel => <<"wechat">>,
            subject => subject(<<"a03-owner/">>),
            display_name => <<"eb05-contact-by-owner">>,
            created_by_business_identity_id => Sales,
            actor_user_id => Owner,
            key_ref => ?FIX:key_ref(1)
        }),
        {ok, ByActor} = eb_contact_app:create_contact(Org, #{
            workspace_id => Ws,
            channel => <<"wechat">>,
            subject => subject(<<"a03-actor/">>),
            display_name => <<"eb05-contact-by-actor">>,
            created_by_business_identity_id => Sales,
            actor_user_id => Actor,
            key_ref => ?FIX:key_ref(1)
        }),
        ContactOwner = maps:get(contact, ByOwner),
        ContactActor = maps:get(contact, ByActor),
        IdentityOwner = maps:get(contact_identity, ByOwner),
        IdentityActor = maps:get(contact_identity, ByActor),
        %% owner 恒为 Organization；actor 不同不改变归属
        ?assertEqual(Org, maps:get(organization_id, ContactOwner)),
        ?assertEqual(Org, maps:get(organization_id, ContactActor)),
        ?assertEqual(Org, maps:get(organization_id, IdentityOwner)),
        ?assertEqual(Org, maps:get(organization_id, IdentityActor)),
        ?assertEqual(Sales, maps:get(created_by_business_identity_id, ContactOwner)),
        ?assertEqual(Sales, maps:get(created_by_business_identity_id, ContactActor)),
        %% 审计如实记录 actor（两名不同 user，同一 Org）
        ?assertEqual(1, audit_actor_rows(Org, maps:get(id, ContactOwner), Owner)),
        ?assertEqual(1, audit_actor_rows(Org, maps:get(id, ContactActor), Actor)),
        %% 两行客户都在该 Org 下
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_contact"
                    " WHERE organization_id=$1 AND id = ANY($2::bigint[])"
                >>,
                [Org, [maps:get(id, ContactOwner), maps:get(id, ContactActor)]],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A04：跨 Org / 非法输入零副作用
%% ===================================================================

a04_cross_org_and_invalid_inputs_have_zero_side_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        KeyRef = ?FIX:key_ref(1),
        Before = {contact_count(Org), contact_identity_rows_org(Org)},
        %% Workspace 不属于该 Org → store 同语句租户自检拒绝，零写入
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_contact_app:create_contact(Org, #{
                workspace_id => OtherWs,
                channel => <<"wechat">>,
                subject => subject(<<"a04-cross-ws/">>),
                display_name => <<"eb05-cross-ws">>,
                key_ref => KeyRef
            })
        ),
        %% 非法 channel（DB CHECK 白名单之外的取值）→ 零写入
        ?assertMatch(
            {error, {unknown_channel, _}},
            eb_contact_app:create_contact(Org, #{
                workspace_id => Ws,
                channel => <<"telegram">>,
                subject => subject(<<"a04-channel/">>),
                display_name => <<"eb05-bad-channel">>,
                key_ref => KeyRef
            })
        ),
        %% 空 subject → 零写入
        ?assertMatch(
            {error, {invalid_subject, _}},
            eb_contact_app:create_contact(Org, #{
                workspace_id => Ws,
                channel => <<"wechat">>,
                subject => <<>>,
                display_name => <<"eb05-empty-subject">>,
                key_ref => KeyRef
            })
        ),
        %% 缺租户参数 → fail-closed，不触库
        ?assertMatch(
            {error, {invalid_workspace_id, _}},
            eb_contact_app:create_contact(Org, #{
                channel => <<"wechat">>,
                subject => subject(<<"a04-no-ws/">>),
                display_name => <<"eb05-no-ws">>,
                key_ref => KeyRef
            })
        ),
        ?assertEqual(Before, {contact_count(Org), contact_identity_rows_org(Org)})
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A07：GET /contacts/{id} + GET /contacts（键集分页）
%% ===================================================================

a07_get_contact_is_tenant_scoped() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        {ok, Row} = eb_contact_app:get_contact(Org, #{workspace_id => Ws, contact_id => Contact}),
        ?assertEqual(Contact, maps:get(id, Row)),
        ?assertEqual(Org, maps:get(organization_id, Row)),
        ?assertEqual(Ws, maps:get(workspace_id, Row)),
        ?assertEqual(active, maps:get(status, Row)),
        %% 跨 Org / 跨 Workspace / 不存在 —— 一律 contact_not_found（不区分，避免枚举）
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:get_contact(OtherOrg, #{workspace_id => OtherWs, contact_id => Contact})
        ),
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:get_contact(Org, #{workspace_id => OtherWs, contact_id => Contact})
        ),
        ?assertEqual(
            {error, {contact_not_found, 999999999999}},
            eb_contact_app:get_contact(Org, #{workspace_id => Ws, contact_id => 999999999999})
        ),
        %% 非法入参 fail-closed，不触库
        ?assertMatch(
            {error, {invalid_contact_id, _}},
            eb_contact_app:get_contact(Org, #{workspace_id => Ws})
        )
    after
        ?FIX:cleanup(Scope)
    end.

a07_list_contacts_returns_own_org_only_and_cross_org_is_empty() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        %% 跨 Org / 配错 Workspace ⇒ 空列表（不是内部错误）
        ?assertEqual({ok, []}, eb_contact_app:list_contacts(OtherOrg, #{workspace_id => OtherWs})),
        ?assertEqual({ok, []}, eb_contact_app:list_contacts(Org, #{workspace_id => OtherWs})),
        %% 本 Org ⇒ 恰好夹具那一条
        {ok, Rows} = eb_contact_app:list_contacts(Org, #{workspace_id => Ws}),
        ?assertEqual([Contact], [maps:get(id, Row) || Row <- Rows]),
        %% 反例：他 Org 的客户混进本 Org 列表即红
        Foreign = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO enterprise_contact"
                " (id,organization_id,status,display_name,version)"
                " VALUES ($1,$2,'active',$3,1)"
            >>,
            [Foreign, OtherOrg, <<"eb05-foreign-", (integer_to_binary(Foreign))/binary>>]
        ),
        {ok, After} = eb_contact_app:list_contacts(Org, #{workspace_id => Ws}),
        Ids = [maps:get(id, Row) || Row <- After],
        ?assertNot(lists:member(Foreign, Ids)),
        lists:foreach(
            fun(Row) -> ?assertEqual(Org, maps:get(organization_id, Row)) end,
            After
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% A07 的**分页口径**判据：键集（非 offset）。
%% 反例构造：分页之间插入一条 **id 小于游标** 的行。
%%   * 键集（`id > 游标`）⇒ 结果逐字不变；
%%   * 若实现成 `OFFSET 2` ⇒ 窗口整体后移，结果首元素 ≤ 游标 ⇒ 断言必红。
a07_list_contacts_keyset_is_not_offset() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        lists:foreach(
            fun(N) ->
                {ok, _} = eb_contact_app:create_contact(Org, #{
                    workspace_id => Ws,
                    channel => <<"wechat">>,
                    subject => subject(<<"a07-page/">>, N),
                    display_name => <<"eb05-a07-page-", (integer_to_binary(N))/binary>>,
                    key_ref => ?FIX:key_ref(1)
                })
            end,
            lists:seq(1, 3)
        ),
        {ok, All} = eb_contact_app:list_contacts(Org, #{workspace_id => Ws}),
        AllIds = [maps:get(id, Row) || Row <- All],
        ?assertEqual(4, length(AllIds)),
        %% 契约：按键升序（键集分页的前提）
        ?assertEqual(lists:sort(AllIds), AllIds),
        [First, Second | _] = AllIds,
        {ok, Page1} = eb_contact_app:list_contacts(Org, #{workspace_id => Ws, limit => 2}),
        ?assertEqual([First, Second], [maps:get(id, Row) || Row <- Page1]),
        %% 插入一条 id < 游标 的客户
        SmallId = First - 1,
        ok = ?FIX:exec(
            <<
                "INSERT INTO enterprise_contact"
                " (id,organization_id,status,display_name,version)"
                " VALUES ($1,$2,'active',$3,1)"
            >>,
            [SmallId, Org, <<"eb05-a07-small-", (integer_to_binary(SmallId))/binary>>]
        ),
        {ok, Page2} = eb_contact_app:list_contacts(Org, #{
            workspace_id => Ws, after_id => Second
        }),
        Page2Ids = [maps:get(id, Row) || Row <- Page2],
        ?assertEqual([Id || Id <- AllIds, Id > Second], Page2Ids),
        ?assert(lists:all(fun(Id) -> Id > Second end, Page2Ids)),
        ?assertNot(lists:member(SmallId, Page2Ids)),
        %% 游标之后无行 ⇒ 空（offset 语义会在这里返回尾部）
        {ok, Tail} = eb_contact_app:list_contacts(Org, #{
            workspace_id => Ws, after_id => lists:last(AllIds)
        }),
        ?assertEqual([], [maps:get(id, Row) || Row <- Tail])
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A08：PATCH /contacts/{id}
%% ===================================================================

a08_update_contact_persists_and_is_readable_back() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Contact = maps:get(contact_id, Scope),
        Canary = <<"EB05-A08-CANARY-PLAINTEXT">>,
        VersionBefore = contact_version(Org, Contact),
        {ok, Updated} = eb_contact_app:update_contact(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            display_name => <<"eb05-a08-renamed">>,
            profile_plaintext => Canary,
            key_ref => ?FIX:key_ref(1)
        }),
        ?assertEqual(Contact, maps:get(id, Updated)),
        ?assertEqual(Org, maps:get(organization_id, Updated)),
        ?assertEqual(<<"eb05-a08-renamed">>, maps:get(display_name, Updated)),
        ?assertEqual(1, maps:get(profile_key_version, Updated)),
        ?assertNotEqual(Canary, maps:get(profile_cipher, Updated)),
        ?assertEqual(VersionBefore + 1, maps:get(version, Updated)),
        %% 真库回读（不采信返回值自述）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_contact"
                    " WHERE organization_id=$1 AND id=$2 AND display_name=$3"
                    "   AND profile_key_version=1 AND version=$4"
                >>,
                [Org, Contact, <<"eb05-a08-renamed">>, VersionBefore + 1],
                0
            )
        ),
        %% 明文不入库
        ?assertEqual(nomatch, binary:match(contact_blob_row(Org, Contact), Canary)),
        %% 白名单外的字段（status / imboy_user_id / organization_id / version）不可被改
        {ok, Same} = eb_contact_app:update_contact(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            display_name => <<"eb05-a08-again">>,
            status => <<"archived">>,
            imboy_user_id => ?FIX:id(),
            organization_id => maps:get(other_org_id, Scope),
            version => 999
        }),
        ?assertEqual(active, maps:get(status, Same)),
        ?assertEqual(Org, maps:get(organization_id, Same)),
        ?assertEqual(undefined, maps:get(imboy_user_id, Same)),
        %% 空 patch（没有任何可改字段）⇒ fail-closed，零写入
        VersionAfter = contact_version(Org, Contact),
        ?assertEqual(
            {error, empty_patch},
            eb_contact_app:update_contact(Org, #{workspace_id => Ws, contact_id => Contact})
        ),
        ?assertEqual(VersionAfter, contact_version(Org, Contact))
    after
        ?FIX:cleanup(Scope)
    end.

a08_update_contact_cross_org_fails_with_zero_side_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        DigestBefore = contact_digest(Org),
        VersionBefore = contact_version(Org, Contact),
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:update_contact(OtherOrg, #{
                workspace_id => OtherWs,
                contact_id => Contact,
                display_name => <<"eb05-a08-hijack">>
            })
        ),
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:update_contact(Org, #{
                workspace_id => OtherWs,
                contact_id => Contact,
                display_name => <<"eb05-a08-hijack">>
            })
        ),
        ?assertEqual(
            {error, {contact_not_found, 999999999999}},
            eb_contact_app:update_contact(Org, #{
                workspace_id => Ws,
                contact_id => 999999999999,
                display_name => <<"eb05-a08-hijack">>
            })
        ),
        %% 零副作用：整行摘要 + version 逐字不变
        ?assertEqual(DigestBefore, contact_digest(Org)),
        ?assertEqual(VersionBefore, contact_version(Org, Contact)),
        ?assertEqual(
            0,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_contact"
                    " WHERE organization_id=$1 AND display_name='eb05-a08-hijack'"
                >>,
                [Org],
                -1
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A09：POST /contacts/{id}/assignment
%% ===================================================================

a09_assign_contact_inserts_row_and_reads_back() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Before = contact_assignment_count(Org),
        {ok, Assigned} = eb_contact_app:assign_contact(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            business_identity_id => Sales,
            role => <<"primary">>,
            actor_user_id => Actor
        }),
        ?assertEqual(Org, maps:get(organization_id, Assigned)),
        ?assertEqual(Contact, maps:get(contact_id, Assigned)),
        ?assertEqual(Sales, maps:get(business_identity_id, Assigned)),
        ?assertEqual(<<"primary">>, maps:get(role, Assigned)),
        ?assertEqual(active, maps:get(status, Assigned)),
        ?assert(is_integer(maps:get(id, Assigned))),
        ?assertEqual(Before + 1, contact_assignment_count(Org)),
        %% 真库回读
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_contact_assignment"
                    " WHERE organization_id=$1 AND contact_id=$2 AND business_identity_id=$3"
                    "   AND role='primary' AND status='active' AND ended_at IS NULL"
                >>,
                [Org, Contact, Sales],
                0
            )
        ),
        %% 同一 contact 的第二个 active primary → 唯一索引裁决 ⇒ 必败且零新增
        CountBefore2 = contact_assignment_count(Org),
        ?assertEqual(
            {error, {contact_assignment_conflict, Contact}},
            eb_contact_app:assign_contact(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                business_identity_id => Service,
                role => <<"primary">>
            })
        ),
        ?assertEqual(CountBefore2, contact_assignment_count(Org)),
        %% collaborator 角色允许并存（证明上一条不是笼统拒绝）
        {ok, Collab} = eb_contact_app:assign_contact(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            business_identity_id => Service,
            role => <<"collaborator">>
        }),
        ?assertEqual(<<"collaborator">>, maps:get(role, Collab)),
        %% 非法角色 fail-closed
        ?assertMatch(
            {error, {invalid_role, _}},
            eb_contact_app:assign_contact(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                business_identity_id => Service,
                role => <<"owner">>
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.

a09_assign_contact_cross_org_fails_with_zero_side_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        CountBefore = contact_assignment_count(Org),
        %% 他 Org + 本 Org 客户 ⇒ 必败（客户不在该租户内）
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:assign_contact(OtherOrg, #{
                workspace_id => OtherWs,
                contact_id => Contact,
                business_identity_id => Sales
            })
        ),
        %% 本 Org 客户 + 他 Org 的业务身份 ⇒ 必败
        Foreign = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity"
                " (id,organization_id,function_key,display_name,status,version)"
                " VALUES ($1,$2,'sales',$3,'active',1)"
            >>,
            [Foreign, OtherOrg, <<"eb05-a09-foreign-", (integer_to_binary(Foreign))/binary>>]
        ),
        ?assertEqual(
            {error, {identity_not_found, Foreign}},
            eb_contact_app:assign_contact(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                business_identity_id => Foreign
            })
        ),
        %% 配错 Workspace ⇒ 租户键不成立，仍以 contact_not_found 拒绝
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:assign_contact(Org, #{
                workspace_id => OtherWs,
                contact_id => Contact,
                business_identity_id => Sales
            })
        ),
        %% 零副作用
        ?assertEqual(CountBefore, contact_assignment_count(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).

subject(Prefix) ->
    <<Prefix/binary, (integer_to_binary(?FIX:id()))/binary>>.

subject(Prefix, N) ->
    <<Prefix/binary, (integer_to_binary(N))/binary>>.

canary() ->
    <<"EB05-CANARY-PLAINTEXT-", (integer_to_binary(?FIX:id()))/binary>>.

contact_version(Org, ContactId) ->
    ?FIX:scalar(
        <<"SELECT version FROM enterprise_contact WHERE organization_id=$1 AND id=$2">>,
        [Org, ContactId],
        -1
    ).

contact_assignment_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_contact_assignment WHERE organization_id=$1">>,
        [Org],
        -1
    ).

%% 客户整行转文本（用于「明文不入库」的机械判据）。
contact_blob_row(Org, ContactId) ->
    ?FIX:scalar(
        <<
            "SELECT coalesce(string_agg(c::text, '|'), '') FROM enterprise_contact c"
            " WHERE c.organization_id=$1 AND c.id=$2"
        >>,
        [Org, ContactId],
        <<>>
    ).

%% 客户关键字段摘要：拒绝路径必须逐字不变。
contact_digest(Org) ->
    Blob = ?FIX:scalar(
        <<
            "SELECT coalesce(string_agg("
            "  id::text || ':' || status || ':' || coalesce(display_name,'-')"
            "  || ':' || coalesce(profile_key_version::text,'-') || ':' || version::text"
            "  || ':' || coalesce(imboy_user_id::text,'-'), '|' ORDER BY id), '')"
            "  FROM enterprise_contact WHERE organization_id=$1"
        >>,
        [Org],
        <<>>
    ),
    crypto:hash(sha256, term_to_binary(Blob)).

contact_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_contact WHERE organization_id=$1">>,
        [Org],
        -1
    ).

contact_identity_rows_org(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_contact_identity WHERE organization_id=$1">>,
        [Org],
        -1
    ).

contact_rows(Org, ContactId) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_contact WHERE organization_id=$1 AND id=$2">>,
        [Org, ContactId],
        -1
    ).

contact_identity_rows(Org, IdentityId) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_contact_identity WHERE organization_id=$1 AND id=$2">>,
        [Org, IdentityId],
        -1
    ).

audit_actor_rows(Org, ResourceId, ActorUserId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_audit_event"
            " WHERE organization_id=$1 AND resource_id=$2 AND actor_user_id=$3"
            "   AND action='enterprise_contact.create'"
        >>,
        [Org, ResourceId, ActorUserId],
        0
    ).

%% 两张客户表整行转文本（列数不同，不能 UNION ALL）。
contact_blob(Org, ContactId, IdentityId) ->
    ?FIX:scalar(
        <<
            "SELECT coalesce((SELECT string_agg(c::text, '|') FROM enterprise_contact c"
            "                  WHERE c.organization_id=$1 AND c.id=$2), '')"
            "    || '|' ||"
            "       coalesce((SELECT string_agg(ci::text, '|') FROM enterprise_contact_identity ci"
            "                  WHERE ci.organization_id=$1 AND ci.id=$3), '')"
        >>,
        [Org, ContactId, IdentityId],
        <<>>
    ).

%% 个人关系世界的行数（企业动作前后必须逐字相等 = 增量 0）。
personal_table_counts() ->
    lists:foldl(
        fun(Table, Acc) ->
            Acc#{Table => ?FIX:scalar(<<"SELECT count(*) FROM ", Table/binary>>, [], -1)}
        end,
        #{},
        [<<"user_friend">>, <<"msg_c2c">>, <<"attachment">>]
    ).

personal_friend_rows(UserId) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM user_friend WHERE from_user_id=$1 OR to_user_id=$1">>,
        [UserId],
        -1
    ).
