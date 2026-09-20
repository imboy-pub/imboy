%%% @doc Widget 与 Seat application 合同套件（CSB-02；plan §12.4/§12.7）。
%%%
%%% 混合夹具（有意为之，见各用例注释）：
%%%   * cs 侧存储 = `cs_fake_store`（widget installation / identity key /
%%%     bootstrap token / nonce / session 全在 fake，快速且确定性）；
%%%   * enterprise 真源 = 真 PG（`eb_pg_test_fixture` 随机 TSID 合成租户）——
%%%     A01 的「重放不重复 contact」锚是 enterprise contact 的**确定性资源键
%%%     唯一约束**，fake 只会镜像自己的假设、没有牙齿，所以 contact /
%%%     conversation / message 的幂等断言全部数真库行。
%%%
%%% 覆盖对应（acceptance-matrix CSB-02）：
%%%   * A01 → a01_*（同 installation+subject 重放不重复 contact/session）；
%%%   * A02 → a02_*（申报 org/contact/conversation/identity 一律被忽略）；
%%%   * A03 → a03_*（jti 重放/过期/错 aud/widget/sub/跨 Org 全拒）；
%%%   * A04 → a04_*（queue 键集分页 + seat detail + claim/close/rate 状态机）；
%%%   * A05 → a05_*（消息/附件只写 enterprise 真源：真库行数 + 无副本表 +
%%%     cs_widget_app 静态引用扫描）；
%%%   * A06 → a06_*（widget 全生命周期冒烟；既有不变量回归由门里的
%%%     cs_application/cs_session/cs_dispatch/cs_pg_widget 套件承担）。
%%%
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
-module(cs_widget_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FAKE, cs_fake_store).
-define(EBFIX, eb_pg_test_fixture).
-define(T0, 1700000000).
-define(SUBJECT_KEY, <<"csb02-test-subject-key">>).
%% 合成主密钥（test-only，恰 32 字节）：contact 幂等锚（组织域 subject HMAC）
%% 跨调用必须稳定，所以用固定合成材料显式注入 key_ref（F6：显式注入优先于
%% env 装配）。
-define(TEST_MASTER_KEY, <<"csb02-synthetic-master-key-v1!!!">>).

widget_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            {ok, Conn};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_bootstrap_replay_reuses_contact_and_no_duplicates/0},
        {timeout, 60, fun a02_declared_values_are_ignored_server_derived_wins/0},
        {timeout, 60, fun a02_origin_negatives_sibling_and_revoked/0},
        {timeout, 60, fun a03_identity_exchange_replay_expired_mismatch_all_rejected/0},
        {timeout, 60, fun a04_queue_keyset_seat_detail_and_rate_state_machine/0},
        {timeout, 60, fun a05_messages_only_via_enterprise_source/0},
        {timeout, 60, fun a06_widget_lifecycle_smoke/0},
        {timeout, 60, fun csb02s_d2_bootstrap_token_ttl_two_directions/0},
        {timeout, 30, fun origin_normalization_edges/0}
    ];
cases({error, Reason}) ->
    erlang:error({csb02_widget_suite_db_unavailable, Reason}).

%% ===================================================================
%% A01：同 installation + anonymous subject 重放不重复 contact / session
%% ===================================================================

a01_bootstrap_replay_reuses_contact_and_no_duplicates() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a01">>),
    try
        Org = org(Scope),
        Contacts0 = contacts(Org),
        %% 首次 bootstrap：真库 contact 恰 +1；bootstrap 不开任何 session。
        {ok, V1} = bootstrap_for(Scope, PublicId, <<"subj-1">>),
        Contact1 = maps:get(contact_id, V1),
        ?assertEqual(Contacts0 + 1, contacts(Org)),
        ?assertEqual(false, maps:get(reused, V1)),
        ?assertEqual(0, session_count(Scope, Contact1)),
        %% 品牌白名单：installation.branding 里白名单外的键不出响应。
        ?assertEqual(#{<<"primary_color">> => <<"#0066cc">>}, maps:get(branding, V1)),
        ?assertEqual(<<"consent-v1">>, maps:get(consent_version, V1)),
        %% 重放（同 subject + 同 secret）：复用原令牌与原 contact，零新增行。
        {ok, V2} =
            cs_widget_app:bootstrap(
                Org,
                wp(Scope, #{
                    public_widget_id => PublicId,
                    origin => origin(),
                    subject_id => <<"subj-1">>,
                    secret => maps:get(secret, V1)
                })
            ),
        ?assertEqual(true, maps:get(reused, V2)),
        ?assertEqual(Contact1, maps:get(contact_id, V2)),
        ?assertEqual(maps:get(expires_at, V1), maps:get(expires_at, V2)),
        ?assertEqual(Contacts0 + 1, contacts(Org)),
        ?assertEqual(0, session_count(Scope, Contact1)),
        %% 丢令牌后同 subject 重新引导：新 token、同一 contact（幂等锚复用）。
        {ok, V3} = bootstrap_for(Scope, PublicId, <<"subj-1">>),
        ?assertEqual(false, maps:get(reused, V3)),
        ?assertNotEqual(maps:get(secret, V1), maps:get(secret, V3)),
        ?assertEqual(Contact1, maps:get(contact_id, V3)),
        ?assertEqual(Contacts0 + 1, contacts(Org)),
        %% 不同 subject → 不同 contact（安装级主体隔离）。
        {ok, V4} = bootstrap_for(Scope, PublicId, <<"subj-2">>),
        ?assertNotEqual(Contact1, maps:get(contact_id, V4)),
        ?assertEqual(Contacts0 + 2, contacts(Org)),
        _ = InstId,
        ok
    after
        teardown(Scope)
    end.

%% ===================================================================
%% A02：浏览器申报值一律被忽略，服务端派生值为准
%% ===================================================================

a02_declared_values_are_ignored_server_derived_wins() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a02">>),
    try
        Org = org(Scope),
        {ok, V1} = bootstrap_for(Scope, PublicId, <<"subj-1">>),
        Contact1 = maps:get(contact_id, V1),
        %% 申报 contact/conversation/identity/org 全是垃圾值——必须被忽略。
        {ok, Created} =
            cs_widget_session_app:create_session(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, V1),
                    contact_id => 42,
                    conversation_id => 99999,
                    business_identity_id => maps:get(sales_identity_id, Scope),
                    organization_id => maps:get(other_org_id, Scope)
                })
            ),
        ?assertEqual(Contact1, maps:get(contact_id, Created)),
        Conv = maps:get(conversation_id, Created),
        ?assert(is_integer(Conv)),
        ?assertNotEqual(99999, Conv),
        %% conversation 真源行属于 token contact（不是申报值）。
        ?assertEqual(Contact1, conversation_contact(Org, Conv)),
        %% session 行同样以派生值为准；queued 会话无经办坐席。
        {ok, Session} = cs_session_app:fetch_session(
            Org,
            wp(Scope, #{
                workspace_id => workspace(Scope), session_id => maps:get(session_id, Created)
            })
        ),
        ?assertEqual(Contact1, maps:get(contact_id, Session)),
        ?assertEqual(Conv, maps:get(conversation_id, Session)),
        ?assertEqual(undefined, maps:get(business_identity_id, Session)),
        %% 同 contact 再开会话 → session_already_open（A01 的会话侧幂等）。
        ?assertMatch(
            {error, {session_already_open, _}},
            cs_widget_session_app:create_session(
                Org,
                wp(Scope, #{
                    installation_id => InstId, secret => maps:get(secret, V1)
                })
            )
        ),
        %% 访客消息：申报坐席身份被剥离，sender 恒为 token contact。
        ok = cs_fake_canonical_tx:reset(),
        {ok, _} =
            cs_widget_session_app:visitor_message(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, V1),
                    session_id => maps:get(session_id, Created),
                    client_msg_id => <<"cmsg-a02-1">>,
                    body => <<"hello">>,
                    business_identity_id => maps:get(service_identity_id, Scope),
                    canonical_tx => cs_fake_canonical_tx
                })
            ),
        [{_, _, TxParams}] = cs_fake_canonical_tx:calls(),
        ?assertEqual(contact, maps:get(sender_type, TxParams)),
        ?assertEqual(Contact1, maps:get(contact_id, TxParams)),
        %% 申报的坐席身份没有变成发送者（identity_id 键由 eb 归一化统一携带，
        %% contact 发送路径下恒为无值）。
        ?assertEqual(undefined, maps:get(identity_id, TxParams, undefined)),
        %% 他人会话不可见：s1 令牌给 s2 的会话评分 → 拒（归属裁决）。
        {ok, V2} = bootstrap_for(Scope, PublicId, <<"subj-2">>),
        {ok, Created2} =
            cs_widget_session_app:create_session(
                Org,
                wp(Scope, #{
                    installation_id => InstId, secret => maps:get(secret, V2)
                })
            ),
        ?assertMatch(
            {error, {not_session_contact, _, _}},
            cs_widget_session_app:rate(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, V1),
                    session_id => maps:get(session_id, Created2),
                    rating => 5,
                    expected_version => 1
                })
            )
        )
    after
        teardown(Scope)
    end.

a02_origin_negatives_sibling_and_revoked() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a02b">>),
    try
        Org = org(Scope),
        %% 未授权 origin 全拒。
        ?assertEqual(
            {error, origin_not_allowed},
            cs_widget_app:bootstrap(
                Org,
                wp(Scope, #{
                    public_widget_id => PublicId,
                    origin => <<"https://evil.example.com">>,
                    subject_id => <<"s">>
                })
            )
        ),
        %% 形状非法（带 path / 无 scheme / 端口越界）→ invalid_origin。
        ?assertMatch(
            {error, {invalid_origin, _}},
            cs_widget_app:bootstrap(
                Org,
                wp(Scope, #{
                    public_widget_id => PublicId,
                    origin => <<"https://shop.example.com/path">>,
                    subject_id => <<"s">>
                })
            )
        ),
        ?assertMatch(
            {error, {invalid_origin, _}},
            cs_widget_app:bootstrap(
                Org,
                wp(Scope, #{
                    public_widget_id => PublicId,
                    origin => <<"shop.example.com">>,
                    subject_id => <<"s">>
                })
            )
        ),
        %% 同源 sibling（缺省端口折叠 + host 大小写折叠）→ 放行。
        {ok, _} =
            cs_widget_app:bootstrap(
                Org,
                wp(Scope, #{
                    public_widget_id => PublicId,
                    origin => <<"https://SHOP.example.com:443">>,
                    subject_id => <<"subj-sibling">>
                })
            ),
        %% 空 allowlist 的安装 → 全拒（fail-closed）。
        EmptyInst = cs_fake_id:new_id(cs_session),
        {ok, _} =
            ?FAKE:insert_widget_installation(Org, #{
                id => EmptyInst,
                public_widget_id => <<"wgt_pub_a02b_empty">>,
                display_name => <<"no origins">>,
                allowed_origins => [],
                branding => #{},
                consent_version => <<"consent-v1">>
            }),
        ?assertEqual(
            {error, origin_not_allowed},
            cs_widget_app:bootstrap(
                Org,
                wp(Scope, #{
                    public_widget_id => <<"wgt_pub_a02b_empty">>,
                    origin => origin(),
                    subject_id => <<"s2">>
                })
            )
        ),
        %% CSD-BE-01R（hosted-widget-contract S3 零申报面）：OrgId 形参是
        %% 占位——application 用 public_widget_id 全局反查的命中行权威派生
        %% 租户；申报任意/错误 org 值被忽略，contact 仍落在命中行的 Org
        %% （旧断言「跨 Org 命中不了行」随零申报面语义一并废止）。
        ContactsX = contacts(Org),
        {ok, VCross} =
            cs_widget_app:bootstrap(
                maps:get(other_org_id, Scope),
                wp(Scope, #{
                    public_widget_id => PublicId, origin => origin(), subject_id => <<"subj-x">>
                })
            ),
        ?assertEqual(ContactsX + 1, contacts(Org)),
        ?assertEqual(false, maps:get(reused, VCross)),
        %% 吊销安装 → 新 bootstrap 与新会话全拒（kill switch 语义；
        %% CSD-BE-01R：bootstrap 反查面三态归一 installation_unavailable）。
        {ok, V1} = bootstrap_for(Scope, PublicId, <<"subj-rev">>),
        ok = ?FAKE:revoke_widget_installation(Org, InstId, ?T0 + 1),
        ?assertEqual(
            {error, installation_unavailable},
            bootstrap_for(Scope, PublicId, <<"subj-rev2">>)
        ),
        ?assertEqual(
            {error, installation_revoked},
            cs_widget_session_app:create_session(
                Org,
                wp(Scope, #{
                    installation_id => InstId, secret => maps:get(secret, V1)
                })
            )
        )
    after
        teardown(Scope)
    end.

%% ===================================================================
%% A03：JTI 重放 / 过期 / 键不对 / 错 aud / 错 widget / 跨 Org 全拒
%% ===================================================================

a03_identity_exchange_replay_expired_mismatch_all_rejected() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a03">>),
    try
        Org = org(Scope),
        KeyMat = <<"csb02-signing-key-material">>,
        WrongKeyMat = <<"csb02-wrong-key-material">>,
        {ok, V1} = bootstrap_for(Scope, PublicId, <<"subj-1">>),
        Claims1 = claims_for(PublicId, #{jti => <<"jti-1">>}),
        %% 无 identity_key 的 installation：4xx 语义错误（identity_key_not_configured）。
        ?assertEqual(
            {error, identity_key_not_configured},
            cs_widget_app:identity_exchange(
                Org, exchange_params(Scope, InstId, V1, Claims1, KeyMat)
            )
        ),
        %% 登记签名密钥（只登记 digest；真钥材料在注入验证器侧）。
        {ok, _} =
            ?FAKE:insert_widget_identity_key(Org, InstId, #{
                id => cs_fake_id:new_id(cs_shop_key),
                key_digest => sha256hex(KeyMat),
                key_version => 1,
                display_hint => <<"k1">>,
                expires_at => ?T0 + 3600
            }),
        Contacts0 = contacts(Org),
        %% 正向：断言验证通过 → 可信 contact 幂等绑定（新 contact，真库 +1）。
        {ok, Ex1} =
            cs_widget_app:identity_exchange(
                Org, exchange_params(Scope, InstId, V1, Claims1, KeyMat)
            ),
        Trusted1 = maps:get(contact_id, Ex1),
        ?assertEqual(false, maps:get(contact_reused, Ex1)),
        ?assertNotEqual(maps:get(anonymous_contact_id, Ex1), Trusted1),
        ?assertEqual(Contacts0 + 1, contacts(Org)),
        %% 幂等：同 sub 新 jti → 复用同一可信 contact，零新增行。
        Sub1 = maps:get(sub, Claims1),
        {ok, Ex2} =
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-2">>, sub => Sub1}),
                    KeyMat
                )
            ),
        ?assertEqual(true, maps:get(contact_reused, Ex2)),
        ?assertEqual(Trusted1, maps:get(contact_id, Ex2)),
        ?assertEqual(Contacts0 + 1, contacts(Org)),
        %% jti 重放 → replay（DB 唯一裁决的 fake 镜像）。
        ?assertEqual(
            {error, replay},
            cs_widget_app:identity_exchange(
                Org, exchange_params(Scope, InstId, V1, Claims1, KeyMat)
            )
        ),
        %% 过期断言。
        ?assertEqual(
            {error, assertion_expired},
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-3">>, exp => ?T0 - 1}),
                    KeyMat
                )
            )
        ),
        %% iat 在未来。
        ?assertEqual(
            {error, assertion_iat_in_future},
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-4">>, iat => ?T0 + 10}),
                    KeyMat
                )
            )
        ),
        %% 错 aud / 错 widget_id / 空 sub。
        ?assertEqual(
            {error, assertion_aud_mismatch},
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-5">>, aud => <<"wgt_pub_other">>}),
                    KeyMat
                )
            )
        ),
        ?assertEqual(
            {error, assertion_widget_mismatch},
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-6">>, widget_id => <<"wgt_pub_other">>}),
                    KeyMat
                )
            )
        ),
        ?assertEqual(
            {error, {invalid_claim, sub}},
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-7">>, sub => <<>>}),
                    KeyMat
                )
            )
        ),
        %% 钥材料不符：注入验证器按 digest 显式拒绝。
        ?assertEqual(
            {error, unknown_key},
            cs_widget_app:identity_exchange(
                Org,
                exchange_params(
                    Scope,
                    InstId,
                    V1,
                    claims_for(PublicId, #{jti => <<"jti-8">>}),
                    WrongKeyMat
                )
            )
        ),
        %% 跨 Org：令牌在错 Org 的 (installation, digest) 查找命中不了行。
        ?assertEqual(
            {error, not_found},
            cs_widget_app:identity_exchange(
                maps:get(other_org_id, Scope),
                exchange_params(
                    Scope, InstId, V1, claims_for(PublicId, #{jti => <<"jti-9">>}), KeyMat
                )
            )
        )
    after
        teardown(Scope)
    end.

%% ===================================================================
%% A04：queue 键集分页 + seat detail + claim/close/rate 状态机
%% ===================================================================

a04_queue_keyset_seat_detail_and_rate_state_machine() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a04">>),
    try
        Org = org(Scope),
        Service = maps:get(service_identity_id, Scope),
        %% 三个访客各开一个 queued session（真实 conversation 各一）。
        S1 = make_visitor_session(Scope, InstId, PublicId, <<"q-1">>),
        S2 = make_visitor_session(Scope, InstId, PublicId, <<"q-2">>),
        S3 = make_visitor_session(Scope, InstId, PublicId, <<"q-3">>),
        %% queue 键集分页（DESC，limit=2）：两页取尽，游标不重不漏。
        {ok, P1} =
            cs_session_app:list_sessions(Org, wp(Scope, #{status => <<"queued">>, limit => 2})),
        Rows1 = maps:get(sessions, P1),
        Cursor = maps:get(next_after_id, P1),
        ?assertEqual(2, length(Rows1)),
        ?assert(Cursor =/= undefined),
        {ok, P2} =
            cs_session_app:list_sessions(
                Org, wp(Scope, #{status => <<"queued">>, limit => 2, after_id => Cursor})
            ),
        Rows2 = maps:get(sessions, P2),
        ?assertEqual(1, length(Rows2)),
        ?assertEqual(undefined, maps:get(next_after_id, P2)),
        Ids = [maps:get(id, S) || S <- Rows1 ++ Rows2],
        ?assertEqual(3, length(lists:usort(Ids))),
        lists:foreach(
            fun(S) -> ?assert(lists:member(maps:get(session_id, S), Ids)) end,
            [S1, S2, S3]
        ),
        %% seat detail：须为本 Org enabled 坐席（应用侧业务前提）。
        ok = ?FAKE:seed_identity_function(Org, Service, <<"customer_service">>),
        {ok, _} =
            cs_seat_app:create_seat(Org, wp(Scope, #{business_identity_id => Service})),
        {ok, Detail} =
            cs_seat_app:session_detail(
                Org,
                wp(Scope, #{
                    business_identity_id => Service, session_id => maps:get(session_id, S1)
                })
            ),
        ?assertEqual(maps:get(session_id, S1), maps:get(id, Detail)),
        ?assertMatch(
            {error, {seat_not_found, _}},
            cs_seat_app:session_detail(
                Org,
                wp(Scope, #{
                    business_identity_id => maps:get(sales_identity_id, Scope),
                    session_id => maps:get(session_id, S1)
                })
            )
        ),
        {ok, _} =
            cs_seat_app:suspend_seat(
                Org,
                wp(Scope, #{
                    business_identity_id => Service, at => ?T0 + 1
                })
            ),
        ?assertEqual(
            {error, seat_disabled},
            cs_seat_app:session_detail(
                Org,
                wp(Scope, #{
                    business_identity_id => Service, session_id => maps:get(session_id, S1)
                })
            )
        ),
        {ok, _} =
            cs_seat_app:resume_seat(
                Org, wp(Scope, #{business_identity_id => Service, at => ?T0 + 2})
            ),
        %% claim CAS：并发第二人 cas_mismatch；close → widget rating 状态机。
        {ok, Active1} =
            cs_session_app:claim(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S1),
                    business_identity_id => Service,
                    expected_version => 1,
                    at => ?T0 + 3
                })
            ),
        ?assertMatch(
            {error, {cas_mismatch, _}},
            cs_session_app:claim(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S1),
                    business_identity_id => Service,
                    expected_version => 1,
                    at => ?T0 + 4
                })
            )
        ),
        {ok, Closed} =
            cs_session_app:close(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S1),
                    expected_version => maps:get(version, Active1),
                    at => ?T0 + 5
                })
            ),
        {ok, Rated} =
            cs_widget_session_app:rate(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S1),
                    session_id => maps:get(session_id, S1),
                    rating => 5,
                    expected_version => maps:get(version, Closed),
                    at => ?T0 + 6
                })
            ),
        ?assertEqual(5, maps:get(rating, Rated)),
        ?assertEqual(
            {error, already_rated},
            cs_widget_session_app:rate(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S1),
                    session_id => maps:get(session_id, S1),
                    rating => 4,
                    expected_version => maps:get(version, Rated),
                    at => ?T0 + 7
                })
            )
        ),
        ?assertMatch(
            {error, {invalid_rating, 0}},
            cs_widget_session_app:rate(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S1),
                    session_id => maps:get(session_id, S1),
                    rating => 0,
                    expected_version => maps:get(version, Rated),
                    at => ?T0 + 8
                })
            )
        ),
        _ = S3,
        ok
    after
        teardown(Scope)
    end.

%% ===================================================================
%% A05：visitor/seat 消息与附件只写 enterprise 真源
%% ===================================================================

a05_messages_only_via_enterprise_source() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a05">>),
    try
        Org = org(Scope),
        Ws = workspace(Scope),
        Service = maps:get(service_identity_id, Scope),
        S = make_visitor_session(Scope, InstId, PublicId, <<"m-1">>),
        Msgs0 = messages(Org, Ws),
        KeyRef = ?EBFIX:key_ref(1),
        %% 访客入站：真库 enterprise_message +1，sender = token contact。
        {ok, _} =
            cs_widget_session_app:visitor_message(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S),
                    session_id => maps:get(session_id, S),
                    client_msg_id => <<"cmsg-a05-1">>,
                    body => <<"visitor hello">>,
                    key_ref => KeyRef
                })
            ),
        ?assertEqual(Msgs0 + 1, messages(Org, Ws)),
        ?assertEqual(
            maps:get(contact_id, S),
            ?EBFIX:scalar(
                <<
                    "SELECT sender_contact_id AS v FROM enterprise_message"
                    " WHERE organization_id=$1 AND client_msg_id=$2"
                >>,
                [Org, <<"cmsg-a05-1">>]
            )
        ),
        %% client_msg_id 幂等：同 id 重放不增行（enterprise 冻结口径）。
        {ok, _} =
            cs_widget_session_app:visitor_message(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S),
                    session_id => maps:get(session_id, S),
                    client_msg_id => <<"cmsg-a05-1">>,
                    body => <<"visitor hello">>,
                    key_ref => KeyRef
                })
            ),
        ?assertEqual(Msgs0 + 1, messages(Org, Ws)),
        %% 坐席出站：同经 enterprise 真源（claim 后以当前经办身份发送）。
        ok = ensure_seat(Scope),
        {ok, Active} =
            cs_session_app:claim(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S),
                    business_identity_id => Service,
                    expected_version => 1,
                    at => ?T0 + 1
                })
            ),
        {ok, _} =
            cs_session_app:append_session_message(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S),
                    business_identity_id => Service,
                    actor_user_id => maps:get(actor_user_id, Scope),
                    client_msg_id => <<"cmsg-a05-2">>,
                    body => <<"seat reply">>,
                    key_ref => KeyRef,
                    accepted_at => ?T0 + 2
                })
            ),
        ?assertEqual(Msgs0 + 2, messages(Org, Ws)),
        ?assertEqual(
            Service,
            ?EBFIX:scalar(
                <<
                    "SELECT sender_business_identity_id AS v FROM enterprise_message"
                    " WHERE organization_id=$1 AND client_msg_id=$2"
                >>,
                [Org, <<"cmsg-a05-2">>]
            )
        ),
        _ = Active,
        %% 客服域零副本表：库中不存在 customer_service%message/attachment%。
        ?assertEqual(0, copy_table_count()),
        %% cs 侧 session 行不带任何消息体键（副本写路径不存在）。
        {ok, Row} = ?FAKE:fetch_session(Org, Ws, maps:get(session_id, S)),
        ?assertEqual(false, maps:is_key(body, Row)),
        ?assertEqual(false, maps:is_key(body_cipher, Row)),
        %% 静态纪律：cs_widget_app 唯一跨单元引用是 enterprise facade；
        %% 全 feature 源码零副本表名。
        ok = assert_widget_source_discipline()
    after
        teardown(Scope)
    end.

%% ===================================================================
%% A06：widget 全生命周期冒烟（状态机与审计链回归）
%% ===================================================================

a06_widget_lifecycle_smoke() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_a06">>),
    try
        Org = org(Scope),
        Service = maps:get(service_identity_id, Scope),
        S = make_visitor_session(Scope, InstId, PublicId, <<"smoke-1">>),
        ok = ensure_seat(Scope),
        ok = cs_fake_canonical_tx:reset(),
        {ok, _} =
            cs_widget_session_app:visitor_message(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S),
                    session_id => maps:get(session_id, S),
                    client_msg_id => <<"cmsg-6a">>,
                    body => <<"in">>,
                    canonical_tx => cs_fake_canonical_tx
                })
            ),
        %% 入站即时断言（fake 覆盖语义：reset 后仅存本次调用）。
        [{_, _, In}] = cs_fake_canonical_tx:calls(),
        ?assertEqual(contact, maps:get(sender_type, In)),
        ?assertEqual(<<"cmsg-6a">>, maps:get(client_msg_id, In)),
        {ok, Active} =
            cs_session_app:claim(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S),
                    business_identity_id => Service,
                    expected_version => 1,
                    at => ?T0 + 1
                })
            ),
        %% fake canonical tx 是「只留最后一次调用」的捕获器（既有约定：每次
        %% 捕获前 reset），出站前清一次以便逐条断言。
        ok = cs_fake_canonical_tx:reset(),
        {ok, _} =
            cs_session_app:append_session_message(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S),
                    business_identity_id => Service,
                    actor_user_id => maps:get(actor_user_id, Scope),
                    client_msg_id => <<"cmsg-6b">>,
                    body => <<"out">>,
                    key_ref => #{key => <<"k">>, key_version => 1},
                    accepted_at => ?T0 + 2,
                    canonical_tx => cs_fake_canonical_tx
                })
            ),
        {ok, Closed} =
            cs_session_app:close(
                Org,
                wp(Scope, #{
                    session_id => maps:get(session_id, S),
                    expected_version => maps:get(version, Active),
                    at => ?T0 + 3
                })
            ),
        {ok, _} =
            cs_widget_session_app:rate(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => maps:get(secret, S),
                    session_id => maps:get(session_id, S),
                    rating => 4,
                    expected_version => maps:get(version, Closed),
                    at => ?T0 + 4
                })
            ),
        %% 两条消息都经 enterprise canonical tx（真源唯一入口），sender 各归其位。
        %% fake 是「只留最后一次调用」的捕获器（set 表覆盖语义），入站已在
        %% reset 后即时断言，此处断言出站这条。
        [{_, _, Out}] = cs_fake_canonical_tx:calls(),
        ?assertEqual(business_identity, maps:get(sender_type, Out)),
        ?assertEqual(<<"cmsg-6b">>, maps:get(client_msg_id, Out)),
        %% fake 审计链完整：bootstrap → open → claim → close → rate。
        ?assertMatch([_ | _], ?FAKE:events_with_action(<<"widget.bootstrapped">>)),
        ?assertMatch([_ | _], ?FAKE:events_with_action(<<"widget.session_created">>)),
        ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.opened">>)),
        ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.claimed">>)),
        ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.closed">>)),
        ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.rated">>))
    after
        teardown(Scope)
    end.

%% ===================================================================
%% domain 纯函数：Origin 归一化边界（同源 sibling 的机械口径）
%% ===================================================================

origin_normalization_edges() ->
    ?assertEqual({ok, <<"https://a.com">>}, cs_widget:normalize_origin(<<"https://a.com">>)),
    %% 缺省端口折叠（同源 sibling）。
    ?assertEqual({ok, <<"https://a.com">>}, cs_widget:normalize_origin(<<"https://a.com:443">>)),
    ?assertEqual({ok, <<"http://a.com">>}, cs_widget:normalize_origin(<<"http://a.com:80">>)),
    %% 非缺省端口保留；scheme/host 大小写折叠。
    ?assertEqual(
        {ok, <<"http://a.com:8080">>}, cs_widget:normalize_origin(<<"HTTP://A.COM:8080">>)
    ),
    ?assertEqual(
        {ok, <<"https://a.com:8443">>}, cs_widget:normalize_origin(<<"https://a.com:8443">>)
    ),
    %% IPv6 字面量。
    ?assertEqual(
        {ok, <<"http://[::1]:8080">>}, cs_widget:normalize_origin(<<"http://[::1]:8080">>)
    ),
    %% 非法形状全拒。
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"https://a.com/x">>)),
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"https://">>)),
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"https://a.com:0">>)),
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"a.com">>)),
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(<<"https://u@a.com">>)),
    %% allowlist 精确匹配：归一后相等才放行。
    ?assertEqual(
        ok, cs_widget:origin_allowed(<<"https://a.com:443">>, [<<"https://a.com">>])
    ),
    ?assertEqual(
        {error, origin_not_allowed},
        cs_widget:origin_allowed(<<"https://b.com">>, [<<"https://a.com">>])
    ),
    %% allowlist 里的非法条目 fail-closed（配置错误显式暴露）。
    ?assertMatch(
        {error, {invalid_origin, _}},
        cs_widget:origin_allowed(<<"https://a.com">>, [<<"not-an-origin">>])
    ),
    ok.

%% ===================================================================
%% CSB-02S D2：bootstrap 令牌 TTL 双向（时间基准 = Unix 秒，TTL 缺省 3600s）
%% ===================================================================

%% @doc 回归 D2：handler 曾以毫秒注入 at、TTL 按秒比较 ⇒ 有效期 3.6s。
%% 修复后基准统一为秒：
%%   * 有效方向 —— 签发后 TTL 内（T+3599）令牌仍可用（能开会话）；
%%   * 过期方向 —— TTL 后（T+3601）同一 secret 开会话被拒（token_expired），
%%     而 bootstrap 重放语义照常重签（不被本用例覆盖，见 A01）。
csb02s_d2_bootstrap_token_ttl_two_directions() ->
    {Scope, InstId, PublicId} = fresh_world(<<"wgt_pub_d2_ttl">>),
    try
        Org = org(Scope),
        {ok, V} = bootstrap_for(Scope, PublicId, <<"d2-subj">>),
        Secret = maps:get(secret, V),
        %% 有效方向：TTL 内（+3599s）token 仍可用。
        {ok, Created} =
            cs_widget_session_app:create_session(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => Secret,
                    at => ?T0 + 3599
                })
            ),
        ?assert(is_integer(maps:get(session_id, Created))),
        %% 过期方向：TTL 后（+3601s）同一 secret 开会话 = token_expired。
        %% （开新 contact 的 subject，避开「已有未关闭会话」的 409 分支。）
        {ok, V2} = bootstrap_for(Scope, PublicId, <<"d2-subj-2">>),
        Secret2 = maps:get(secret, V2),
        ?assertEqual(
            {error, token_expired},
            cs_widget_session_app:create_session(
                Org,
                wp(Scope, #{
                    installation_id => InstId,
                    secret => Secret2,
                    at => ?T0 + 3601
                })
            )
        )
    after
        teardown(Scope)
    end.

%% ===================================================================
%% 夹具与构造辅助
%% ===================================================================

fresh_world(PublicId) ->
    Scope = ?EBFIX:new_scope(),
    ok = ?FAKE:init(),
    cs_fake_id:reset(),
    Org = org(Scope),
    InstId = cs_fake_id:new_id(cs_session),
    {ok, _} =
        ?FAKE:insert_widget_installation(Org, #{
            id => InstId,
            public_widget_id => PublicId,
            display_name => <<"CSB02 Widget">>,
            allowed_origins => [<<"https://shop.example.com">>, <<"http://localhost:3000">>],
            branding => #{
                <<"primary_color">> => <<"#0066cc">>, <<"internal_note">> => <<"do-not-leak">>
            },
            consent_version => <<"consent-v1">>
        }),
    {Scope, InstId, PublicId}.

teardown(Scope) ->
    ?EBFIX:cleanup(Scope),
    ?FAKE:destroy(),
    cs_fake_id:reset(),
    ok.

wp(Scope, Extra) ->
    maps:merge(
        #{
            store => ?FAKE,
            id => cs_fake_id,
            at => ?T0,
            %% workspace_id 仅供测试**直调** cs_session_app/cs_seat_app 时的
            %% 租户门使用；cs_widget_app 自身只认 default_workspace 注入事实
            %% （maps:with 收敛后该键不进 widget 用例），A02 的申报值负例照常。
            workspace_id => workspace(Scope),
            subject_key => ?SUBJECT_KEY,
            key_ref => #{key => ?TEST_MASTER_KEY, key_version => 1},
            default_workspace => fun(_Org) -> {ok, workspace(Scope)} end,
            intake_business_identity_id => maps:get(service_identity_id, Scope)
        },
        Extra
    ).

bootstrap_for(Scope, PublicId, Subject) ->
    cs_widget_app:bootstrap(
        org(Scope),
        wp(Scope, #{public_widget_id => PublicId, origin => origin(), subject_id => Subject})
    ).

make_visitor_session(Scope, InstId, PublicId, Subject) ->
    Org = org(Scope),
    {ok, V} = bootstrap_for(Scope, PublicId, Subject),
    {ok, Created} =
        cs_widget_session_app:create_session(
            Org,
            wp(Scope, #{
                installation_id => InstId, secret => maps:get(secret, V)
            })
        ),
    #{
        secret => maps:get(secret, V),
        contact_id => maps:get(contact_id, Created),
        session_id => maps:get(session_id, Created),
        conversation_id => maps:get(conversation_id, Created)
    }.

ensure_seat(Scope) ->
    Org = org(Scope),
    Service = maps:get(service_identity_id, Scope),
    ok = ?FAKE:seed_identity_function(Org, Service, <<"customer_service">>),
    case cs_seat_app:create_seat(Org, wp(Scope, #{business_identity_id => Service})) of
        {ok, _} -> ok;
        {error, conflict} -> ok
    end.

exchange_params(Scope, InstId, V, Claims, KeyMat) ->
    wp(Scope, #{
        installation_id => InstId,
        secret => maps:get(secret, V),
        assertion => #{
            key_version => 1,
            claims => Claims,
            %% 测试签名：HMAC(注入钥材料, "jti:" + jti)——与 verifier 同口径。
            sig => sign(Claims, KeyMat)
        },
        assertion_verifier => verifier(KeyMat)
    }).

%% 注入验证器（生产里由持有真钥材料的一侧装配）：核对 store 里的 key_digest
%% 与本侧钥材料一致 + 断言签名一致，再放行 claims。
verifier(KeyMat) ->
    fun(Assertion, KeyDigest) ->
        Claims = maps:get(claims, Assertion, undefined),
        Sig = maps:get(sig, Assertion, undefined),
        case
            KeyDigest =:= sha256hex(KeyMat) andalso is_map(Claims) andalso
                Sig =:= sign(Claims, KeyMat)
        of
            true -> {ok, Claims};
            false -> {error, unknown_key}
        end
    end.

sign(Claims, KeyMat) when is_map(Claims) ->
    Jti = maps:get(jti, Claims, <<>>),
    binary:encode_hex(crypto:mac(hmac, sha256, KeyMat, <<"jti:", Jti/binary>>));
sign(_, _) ->
    <<>>.

claims_for(PublicId, Overrides) ->
    Seq = erlang:unique_integer([positive, monotonic]),
    SeqBin = integer_to_binary(Seq),
    maps:merge(
        #{
            iss => <<"shop.example.com">>,
            aud => PublicId,
            widget_id => PublicId,
            sub => <<"shopper-", SeqBin/binary>>,
            exp => ?T0 + 300,
            iat => ?T0,
            jti => <<"jti-", SeqBin/binary>>
        },
        Overrides
    ).

origin() ->
    <<"https://shop.example.com">>.

org(Scope) ->
    maps:get(org_id, Scope).

workspace(Scope) ->
    maps:get(workspace_id, Scope).

contacts(Org) ->
    ?EBFIX:count(Org, 0, contacts).

messages(Org, Ws) ->
    ?EBFIX:count(Org, Ws, messages).

conversation_contact(Org, Conv) ->
    ?EBFIX:scalar(
        <<
            "SELECT contact_id AS v FROM enterprise_conversation"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [Org, Conv]
    ).

session_count(Scope, ContactId) ->
    {ok, Sessions} =
        cs_session_app:list_contact_sessions(
            org(Scope), wp(Scope, #{workspace_id => workspace(Scope), contact_id => ContactId})
        ),
    length(Sessions).

copy_table_count() ->
    ?EBFIX:scalar(
        <<
            "SELECT count(*) AS v FROM information_schema.tables"
            " WHERE table_name LIKE 'customer_service%message%'"
            " OR table_name LIKE 'customer_service%attachment%'"
        >>,
        []
    ).

sha256hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin)).

%% 静态纪律（镜像 cs_closure_tests 的引用扫描思路，只对 cs_widget_app）：
%% 唯一跨单元引用是 enterprise_business_facade；零 elib_pg / cs_pg_ 直引；
%% 全 feature 源码零副本表名。
assert_widget_source_discipline() ->
    WidgetDir = "src/features/customer_service/application/widget/",
    lists:foreach(
        fun(Mod) ->
            Code = strip_comments(read_source(WidgetDir ++ Mod ++ ".erl")),
            Refs = remote_calls(Code),
            Offenders = [
                R
             || R <- Refs,
                lists:prefix("eb_", binary_to_list(R)),
                R =/= <<"enterprise_business_facade">>
            ],
            ?assertEqual([], Offenders),
            ?assertNot(lists:member(<<"elib_pg">>, Refs)),
            ?assertNot(lists:member(<<"cs_pg_widget">>, Refs))
        end,
        ["cs_widget_app", "cs_widget_session_app", "cs_widget_support"]
    ),
    %% 跨裁剪出口里确实锚着 enterprise facade / session 既有路径（正向证据）。
    Gate = strip_comments(read_source(WidgetDir ++ "cs_widget_support.erl")),
    GateRefs = remote_calls(Gate),
    ?assert(lists:member(<<"enterprise_business_facade">>, GateRefs)),
    ?assert(lists:member(<<"cs_session_app">>, GateRefs)),
    %% 全 feature 源码（剥注释后）零副本表名——注释里的否定句不算引用。
    Files = filelib:wildcard("src/features/customer_service/**/*.erl"),
    ?assert(length(Files) > 0),
    lists:foreach(
        fun(F) ->
            Code = strip_comments(read_source(F)),
            ?assertMatch(nomatch, binary:match(Code, <<"customer_service_message">>)),
            ?assertMatch(nomatch, binary:match(Code, <<"customer_service_attachment">>))
        end,
        Files
    ),
    ok.

read_source(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

strip_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([
        [
            case binary:split(Line, <<"%">>) of
                [Before, _] -> Before;
                [Only] -> Only
            end,
            <<"\n">>
        ]
     || Line <- Lines
    ]).

remote_calls(Src) ->
    case
        re:run(Src, "\\b([a-z][a-z0-9_]*):[a-z_][a-z0-9_]*\\s*\\(", [global, {capture, [1], binary}])
    of
        {match, Pairs} -> lists:usort([M || [M] <- Pairs]);
        nomatch -> []
    end.
