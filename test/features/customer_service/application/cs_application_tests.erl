%%% @doc 客服 application 编排套件（注入 fake store / fake id / fake canonical tx）。
%%%
%%% 覆盖 CS-01 的应用侧验收：
%%%   * A01：seat 创建只接受 function_key=customer_service 的 identity
%%%     （应用侧判定 + 假库 conflict）；
%%%   * A02：claim 的 CAS 语义（重复 claim conflict；超 max_concurrent 拒绝；
%%%     suspend 即时拒绝；least-active 派单经 cs_dispatch）；
%%%   * A03：`append_session_message/2` 的唯一写入路径是
%%%     `enterprise_business_facade:append_message`——用 fake canonical tx 捕获
%%%     参数面，断言无任何客服私有消息副本（参数收敛 + 委派形状）；
%%%   * A04：session 绑定 business_identity_id；rebind（换绑 assignment user）
%%%     后 fetch/发消息/transfer 全部连续，主体字段零迁移；
%%%   * A05：visit token 只授予 (Org, contact) 作用域；吊销/过期即失效；
%%%     token 无法用于 seat/member 能力。
%%% 触库部分（DB 级约束、真并发 CAS）全部在 cs_pg_tests，本套件零 SQL。
-module(cs_application_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FAKE, cs_fake_store).
-define(ORG, 810000000000001).
-define(WS, 810000000000002).
-define(CONTACT, 810000000000003).
-define(CONV, 810000000000004).
-define(SEAT_A, 810000000000011).
-define(SEAT_B, 810000000000012).
-define(USER_A, 810000000000021).
-define(USER_B, 810000000000022).
-define(T0, 1700000000).

application_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    ok = ?FAKE:init(),
    cs_fake_id:reset(),
    cs_fake_canonical_tx:reset(),
    {ok, fixed}.

cleanup(_) ->
    ?FAKE:destroy(),
    cs_fake_canonical_tx:reset(),
    %% F6 装配用例注入的 env keyring 不外泄到其他套件（application env 是 VM 级）。
    _ = application:unset_env(imboy, eb_enterprise_keyring),
    ok.

cases(_State) ->
    [
        {timeout, 30, fun a01_seat_requires_customer_service_identity/0},
        {timeout, 30, fun a01_duplicate_seat_conflicts/0},
        {timeout, 30, fun a02_double_claim_second_conflicts/0},
        {timeout, 30, fun a02_max_concurrent_cap_and_dispatch/0},
        {timeout, 30, fun a02_suspended_seat_rejects_claim/0},
        {timeout, 30, fun a03_message_goes_only_through_eb_facade/0},
        {timeout, 30, fun a03_seat_mismatch_and_contact_mismatch_rejected/0},
        {timeout, 30, fun a04_rebind_keeps_session_and_history_continuous/0},
        {timeout, 30, fun a05_visit_token_scope_and_revocation/0},
        {timeout, 30, fun close_then_rate_then_double_actions_rejected/0},
        {timeout, 30, fun facade_delegates_and_validates_shape/0},
        %% F6：主密钥服务端装配（显式注入优先；无注入经 env keyring）。
        {timeout, 30, fun a03_env_keyring_assembly/0}
    ].

%% ===================================================================
%% A01：seat 只引用 customer_service identity
%% ===================================================================

a01_seat_requires_customer_service_identity() ->
    reset_all(),
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_A, <<"customer_service">>),
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_B, <<"sales">>),
    %% sales identity 不能开 seat（应用侧判定；DB 侧 FK 由 cs_pg_tests 复核）
    ?assertMatch(
        {error, {identity_not_customer_service, ?SEAT_B, <<"sales">>}},
        cs_seat_app:create_seat(?ORG, params(#{business_identity_id => ?SEAT_B}))
    ),
    ?assertMatch(
        {error, {identity_not_found, 999}},
        cs_seat_app:create_seat(?ORG, params(#{business_identity_id => 999}))
    ),
    {ok, Seat} = cs_seat_app:create_seat(
        ?ORG,
        params(#{
            business_identity_id => ?SEAT_A, max_concurrent => 2, created_by_user_id => ?USER_A
        })
    ),
    ?assertEqual(?ORG, maps:get(organization_id, Seat)),
    ?assertEqual(2, maps:get(max_concurrent, Seat)),
    ?assertEqual(true, maps:get(enabled, Seat)),
    %% seat 行没有 owner user：created_by_user_id 只是审计快照键，business_identity_id 是 PK
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, Seat)),
    %% A01 有牙齿：identity 职能改成 sales 后同参数被拒，证明判定不是恒真
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_A, <<"sales">>),
    ?assertMatch(
        {error, {identity_not_customer_service, ?SEAT_A, <<"sales">>}},
        cs_seat_app:create_seat(?ORG, params(#{business_identity_id => ?SEAT_A}))
    ).

a01_duplicate_seat_conflicts() ->
    reset_all(),
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_A, <<"customer_service">>),
    {ok, _} = cs_seat_app:create_seat(?ORG, params(#{business_identity_id => ?SEAT_A})),
    ?assertEqual(
        {error, conflict},
        cs_seat_app:create_seat(?ORG, params(#{business_identity_id => ?SEAT_A}))
    ).

%% ===================================================================
%% A02：claim CAS / max_concurrent / suspend
%% ===================================================================

a02_double_claim_second_conflicts() ->
    reset_all(),
    Ctx = claimed_session(?SEAT_A),
    #{session_id := SessionId, version := ActiveVersion} = Ctx,
    %% 过期版本（被第一次 claim 消耗）→ domain CAS 先拒
    ?assertMatch(
        {error, {cas_mismatch, _}},
        cs_session_app:claim(
            ?ORG,
            params(#{
                session_id => SessionId,
                business_identity_id => ?SEAT_A,
                expected_version => ActiveVersion - 1,
                at => ?T0 + 5
            })
        )
    ),
    %% 当前版本但状态已是 active（第二人的 claim）→ 同样 cas_mismatch：恰好一个成功
    ?assertMatch(
        {error, {cas_mismatch, _}},
        cs_session_app:claim(
            ?ORG,
            params(#{
                session_id => SessionId,
                business_identity_id => ?SEAT_A,
                expected_version => ActiveVersion,
                at => ?T0 + 6
            })
        )
    ),
    %% 会话仍保持第一次 claim 的形状（未被第二次写坏）
    {ok, Session} = ?FAKE:fetch_session(?ORG, ?WS, SessionId),
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, Session)),
    ?assertEqual(ActiveVersion, maps:get(version, Session)).

a02_max_concurrent_cap_and_dispatch() ->
    reset_all(),
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_A, <<"customer_service">>),
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_B, <<"customer_service">>),
    {ok, _} = cs_seat_app:create_seat(
        ?ORG,
        params(#{
            business_identity_id => ?SEAT_A, max_concurrent => 1
        })
    ),
    {ok, _} = cs_seat_app:create_seat(
        ?ORG,
        params(#{
            business_identity_id => ?SEAT_B, max_concurrent => 1
        })
    ),
    %% 显式 claim 到 A 后 A 满；再显式 claim 到 A → seat_at_capacity
    S1 = open_session(),
    {ok, Active1} = cs_session_app:claim(
        ?ORG,
        params(#{
            session_id => maps:get(id, S1),
            business_identity_id => ?SEAT_A,
            expected_version => 1,
            at => ?T0 + 1
        })
    ),
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, Active1)),
    S2 = open_session(),
    ?assertEqual(
        {error, seat_at_capacity},
        cs_session_app:claim(
            ?ORG,
            params(#{
                session_id => maps:get(id, S2),
                business_identity_id => ?SEAT_A,
                expected_version => 1,
                at => ?T0 + 2
            })
        )
    ),
    %% dispatch（least-active）：A 满、B 空闲 → 自动派给 B
    S3 = open_session(),
    {ok, Active3} = cs_session_app:claim(
        ?ORG,
        params(#{
            session_id => maps:get(id, S3),
            expected_version => 1,
            at => ?T0 + 3
        })
    ),
    ?assertEqual(?SEAT_B, maps:get(business_identity_id, Active3)),
    %% 两坐席全满 → no_seat_available
    S4 = open_session(),
    ?assertEqual(
        {error, no_seat_available},
        cs_session_app:claim(
            ?ORG,
            params(#{
                session_id => maps:get(id, S4), expected_version => 1, at => ?T0 + 4
            })
        )
    ).

a02_suspended_seat_rejects_claim() ->
    reset_all(),
    Ctx = fresh_seat(?SEAT_A),
    S = open_session(),
    {ok, Suspended} = cs_seat_app:suspend_seat(
        ?ORG,
        params(#{
            business_identity_id => ?SEAT_A, at => ?T0 + 1, reason => <<"break">>
        })
    ),
    ?assertEqual(false, maps:get(enabled, Suspended)),
    ?assertEqual(
        {error, seat_disabled},
        cs_session_app:claim(
            ?ORG,
            params(#{
                session_id => maps:get(id, S),
                business_identity_id => ?SEAT_A,
                expected_version => 1,
                at => ?T0 + 2
            })
        )
    ),
    %% dispatch 也跳过停用坐席
    ?assertEqual(
        {error, no_seat_available},
        cs_session_app:claim(
            ?ORG,
            params(#{
                session_id => maps:get(id, S), expected_version => 1, at => ?T0 + 3
            })
        )
    ),
    %% resume 后恢复
    {ok, Resumed} = cs_seat_app:resume_seat(
        ?ORG, params(#{business_identity_id => ?SEAT_A, at => ?T0 + 4})
    ),
    ?assertEqual(true, maps:get(enabled, Resumed)),
    {ok, Active} = cs_session_app:claim(
        ?ORG,
        params(#{
            session_id => maps:get(id, S),
            business_identity_id => ?SEAT_A,
            expected_version => 1,
            at => ?T0 + 5
        })
    ),
    _ = Ctx,
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, Active)).

%% ===================================================================
%% A03：消息只经 enterprise_business_facade 写 enterprise 真源
%% ===================================================================

a03_message_goes_only_through_eb_facade() ->
    reset_all(),
    Ctx = claimed_session(?SEAT_A),
    #{session_id := SessionId, conversation_id := Conv, identity := Identity} = Ctx,
    {ok, Result} = cs_session_app:append_session_message(
        ?ORG,
        params(#{
            session_id => SessionId,
            business_identity_id => Identity,
            actor_user_id => ?USER_A,
            client_msg_id => <<"cmsg-1">>,
            body => <<"hello from seat">>,
            key_ref => #{key => <<"k">>, key_version => 1},
            accepted_at => ?T0 + 10,
            canonical_tx => cs_fake_canonical_tx
        })
    ),
    ?assertEqual(true, maps:get(accepted, Result)),
    %% 委派形状：恰好一次 canonical tx 调用，参数是 enterprise 的面
    Calls = cs_fake_canonical_tx:calls(),
    ?assertEqual(1, length(Calls)),
    [{Org, Ws, TxParams}] = Calls,
    ?assertEqual(?ORG, Org),
    ?assertEqual(?WS, Ws),
    ?assertEqual(Conv, maps:get(conversation_id, TxParams)),
    %% canonical tx 收到的 sender_type 是 eb_message_app 收敛后的 atom（其冻结合同）
    ?assertEqual(business_identity, maps:get(sender_type, TxParams)),
    ?assertEqual(Identity, maps:get(identity_id, TxParams)),
    ?assertEqual(?USER_A, maps:get(actor_user_id, TxParams)),
    %% 不落客服私有副本：fake store 的 sessions 里没有 body/message 键
    {ok, Session} = ?FAKE:fetch_session(?ORG, ?WS, SessionId),
    ?assertEqual(false, maps:is_key(body, Session)),
    ?assertEqual(false, maps:is_key(body_cipher, Session)).

a03_seat_mismatch_and_contact_mismatch_rejected() ->
    reset_all(),
    Ctx = claimed_session(?SEAT_A),
    #{session_id := SessionId, identity := Identity} = Ctx,
    %% 非当前经办坐席不能以该会话发消息
    ?assertMatch(
        {error, {not_session_seat, _, _}},
        cs_session_app:append_session_message(
            ?ORG,
            params(#{
                session_id => SessionId,
                business_identity_id => ?SEAT_B,
                actor_user_id => ?USER_B,
                client_msg_id => <<"cmsg-bad-seat">>,
                body => <<"x">>,
                key_ref => #{key => <<"k">>, key_version => 1},
                accepted_at => ?T0 + 11
            })
        )
    ),
    ?assertEqual([], cs_fake_canonical_tx:calls()),
    %% 其它 contact 不能冒充本会话访客
    ?assertMatch(
        {error, {not_session_contact, _, _}},
        cs_session_app:append_session_message(
            ?ORG,
            params(#{
                session_id => SessionId,
                contact_id => 999,
                client_msg_id => <<"cmsg-bad-contact">>,
                body => <<"x">>,
                key_ref => #{key => <<"k">>, key_version => 1},
                accepted_at => ?T0 + 12
            })
        )
    ),
    ?assertEqual([], cs_fake_canonical_tx:calls()),
    %% 会话绑定 contact 的入站成立
    {ok, Inbound} = cs_session_app:append_session_message(
        ?ORG,
        params(#{
            session_id => SessionId,
            contact_id => ?CONTACT,
            client_msg_id => <<"cmsg-inbound">>,
            body => <<"hello from visitor">>,
            key_ref => #{key => <<"k">>, key_version => 1},
            accepted_at => ?T0 + 13,
            canonical_tx => cs_fake_canonical_tx
        })
    ),
    ?assertEqual(true, maps:get(accepted, Inbound)),
    [{_, _, TxParams}] = cs_fake_canonical_tx:calls(),
    ?assertEqual(contact, maps:get(sender_type, TxParams)),
    ?assertEqual(?CONTACT, maps:get(contact_id, TxParams)),
    _ = Identity,
    ok.

%% ===================================================================
%% A04：identity rebind 后会话与历史连续
%% ===================================================================

a04_rebind_keeps_session_and_history_continuous() ->
    reset_all(),
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_A, <<"customer_service">>),
    {ok, _} = cs_seat_app:create_seat(?ORG, params(#{business_identity_id => ?SEAT_A})),
    S = open_session(),
    {ok, Active} = cs_session_app:claim(
        ?ORG,
        params(#{
            session_id => maps:get(id, S),
            business_identity_id => ?SEAT_A,
            expected_version => 1,
            at => ?T0 + 20
        })
    ),
    SessionId = maps:get(id, Active),
    Conv = maps:get(conversation_id, Active),
    {ok, Msg} = cs_session_app:append_session_message(
        ?ORG,
        params(#{
            session_id => SessionId,
            business_identity_id => ?SEAT_A,
            actor_user_id => ?USER_A,
            client_msg_id => <<"cmsg-a04">>,
            body => <<"before rebind">>,
            key_ref => #{key => <<"k">>, key_version => 1},
            accepted_at => ?T0 + 21,
            canonical_tx => cs_fake_canonical_tx
        })
    ),
    BeforeMessageId = maps:get(message_id, Msg),
    %% rebind：identity A 的经办人从 USER_A 换成 USER_B（数据零迁移；cs 侧无感知）
    ok = ?FAKE:seed_assignment_user(?ORG, ?SEAT_A, ?USER_B),
    ?assertEqual(?USER_B, ?FAKE:assignment_user(?ORG, ?SEAT_A)),
    %% 会话行与历史连续：同一 session id / org / contact / conversation 仍可读
    {ok, After} = cs_session_app:fetch_session(?ORG, params(#{session_id => SessionId})),
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, After)),
    ?assertEqual(?CONTACT, maps:get(contact_id, After)),
    ?assertEqual(Conv, maps:get(conversation_id, After)),
    %% rebind 后新用户（同一 identity）继续可发消息
    {ok, Msg2} = cs_session_app:append_session_message(
        ?ORG,
        params(#{
            session_id => SessionId,
            business_identity_id => ?SEAT_A,
            actor_user_id => ?USER_B,
            client_msg_id => <<"cmsg-a04-after">>,
            body => <<"after rebind">>,
            key_ref => #{key => <<"k">>, key_version => 1},
            accepted_at => ?T0 + 22,
            canonical_tx => cs_fake_canonical_tx
        })
    ),
    %% 历史消息 id 连续可读（真源在 enterprise；两消息 id 均来自同一 conversation）
    BeforeAfter = [BeforeMessageId, maps:get(message_id, Msg2)],
    ?assertEqual(2, length(BeforeAfter)),
    %% transfer（改绑 identity）主体字段零迁移；目标坐席 B 已开通（真约束：目标必须是 seat）
    ok = ensure_seat(?SEAT_B, 1),
    {ok, Transferred} = cs_session_app:transfer(
        ?ORG,
        params(#{
            session_id => SessionId,
            to_identity_id => ?SEAT_B,
            expected_version => maps:get(version, After),
            at => ?T0 + 23
        })
    ),
    ?assertEqual(?SEAT_B, maps:get(business_identity_id, Transferred)),
    ?assertEqual(SessionId, maps:get(id, Transferred)),
    ?assertEqual(?ORG, maps:get(organization_id, Transferred)),
    ?assertEqual(?CONTACT, maps:get(contact_id, Transferred)),
    ?assertEqual(Conv, maps:get(conversation_id, Transferred)).

%% ===================================================================
%% A05：visit token 权限边界
%% ===================================================================

a05_visit_token_scope_and_revocation() ->
    reset_all(),
    Secret = <<"visit-secret-0001">>,
    {ok, Issued} = cs_access_app:issue_visit_token(
        ?ORG,
        params(#{
            contact_id => ?CONTACT,
            secret => Secret,
            expires_at => ?T0 + 100,
            created_by_user_id => ?USER_A
        })
    ),
    %% 明文只返回一次：返回体带 secret，而存储行只含 digest
    ?assertEqual(Secret, maps:get(secret, Issued)),
    ?assertNotEqual(Secret, maps:get(token_digest, Issued)),
    TokenId = maps:get(id, Issued),
    {ok, Token} = ?FAKE:fetch_visit_token(?ORG, TokenId),
    ?assertEqual(false, maps:is_key(secret, Token)),
    %% 有效校验：只返回 (Org, contact) 作用域，无任何 member/seat 能力键
    {ok, Scope} = cs_access_app:verify_visit_token(
        ?ORG,
        params(#{
            secret => Secret, at => ?T0 + 1
        })
    ),
    ?assertEqual(?ORG, maps:get(organization_id, Scope)),
    ?assertEqual(?CONTACT, maps:get(contact_id, Scope)),
    ?assertEqual(visit, maps:get(scope, Scope)),
    AllowedKeys = lists:sort(maps:keys(Scope)),
    ?assertEqual([contact_id, organization_id, scope], AllowedKeys),
    %% 过期即失效
    ?assertEqual(
        {error, token_expired},
        cs_access_app:verify_visit_token(?ORG, params(#{secret => Secret, at => ?T0 + 100}))
    ),
    %% 吊销后（未到期）立即失效
    ok = cs_access_app:revoke_visit_token(?ORG, params(#{id => TokenId, at => ?T0 + 2})),
    ?assertEqual(
        {error, token_revoked},
        cs_access_app:verify_visit_token(?ORG, params(#{secret => Secret, at => ?T0 + 3}))
    ),
    %% 跨 Org 的 secret 命中不了本 Org 行（not_found，无枚举）
    ?assertEqual(
        {error, not_found},
        cs_access_app:verify_visit_token(?ORG + 1, params(#{secret => Secret, at => ?T0 + 3}))
    ).

%% ===================================================================
%% close → rate → 重复动作负例
%% ===================================================================

close_then_rate_then_double_actions_rejected() ->
    reset_all(),
    Ctx = claimed_session(?SEAT_A),
    #{session_id := SessionId, version := ActiveVersion} = Ctx,
    {ok, Closed} = cs_session_app:close(
        ?ORG,
        params(#{
            session_id => SessionId,
            expected_version => ActiveVersion,
            at => ?T0 + 30,
            reason => <<"solved">>
        })
    ),
    ?assertEqual(closed, maps:get(status, Closed)),
    ?assertEqual(<<"solved">>, maps:get(close_reason, Closed)),
    ClosedVersion = maps:get(version, Closed),
    %% 重复 close（终态）→ domain 先拒
    ?assertEqual(
        {error, session_already_closed},
        cs_session_app:close(
            ?ORG,
            params(#{
                session_id => SessionId,
                expected_version => ClosedVersion,
                at => ?T0 + 31
            })
        )
    ),
    %% 评分 1..5（合法）
    {ok, Rated} = cs_session_app:rate(
        ?ORG,
        params(#{
            session_id => SessionId,
            rating => 5,
            expected_version => ClosedVersion,
            at => ?T0 + 32
        })
    ),
    ?assertEqual(5, maps:get(rating, Rated)),
    %% 重复评分 → 已在 domain 拒（store 未再写）
    ?assertEqual(
        {error, already_rated},
        cs_session_app:rate(
            ?ORG,
            params(#{
                session_id => SessionId,
                rating => 1,
                expected_version => maps:get(version, Rated),
                at => ?T0 + 33
            })
        )
    ),
    %% 越界评分 → invalid_rating
    ?assertMatch(
        {error, {invalid_rating, 0}},
        cs_session_app:rate(
            ?ORG,
            params(#{
                session_id => SessionId,
                rating => 0,
                expected_version => maps:get(version, Rated),
                at => ?T0 + 34
            })
        )
    ),
    %% 审计链完整（fake events）：claimed → transferred/close 分开断言
    ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.claimed">>)),
    ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.closed">>)),
    ?assertMatch([_ | _], ?FAKE:events_with_action(<<"session.rated">>)).

%% ===================================================================
%% facade 委派形状
%% ===================================================================

facade_delegates_and_validates_shape() ->
    reset_all(),
    %% 形状错误 → invalid_argument（不触 store）
    ?assertMatch(
        {error, {invalid_argument, claim}},
        customer_service_facade:claim(?ORG, #{session_id => 1, expected_version => 1})
    ),
    ?assertMatch(
        {error, {invalid_argument, {organization_id, <<"x">>}}},
        customer_service_facade:fetch_session(<<"x">>, #{session_id => 1})
    ),
    %% 委派成立：facade 创建成功；随后 facade / application 同参再建都 conflict（同语义）
    ok = ?FAKE:seed_identity_function(?ORG, ?SEAT_A, <<"customer_service">>),
    {ok, ViaFacade} = customer_service_facade:create_seat(
        ?ORG,
        params(#{
            workspace_id => ?WS, business_identity_id => ?SEAT_A
        })
    ),
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, ViaFacade)),
    ?assertEqual(
        {error, conflict},
        customer_service_facade:create_seat(
            ?ORG,
            params(#{
                workspace_id => ?WS, business_identity_id => ?SEAT_A
            })
        )
    ),
    ?assertEqual(
        {error, conflict},
        cs_seat_app:create_seat(
            ?ORG,
            params(#{
                workspace_id => ?WS, business_identity_id => ?SEAT_A
            })
        )
    ).

%% ===================================================================
%% F6：主密钥服务端装配（RULING-2026-09-15 §七）
%% ===================================================================

%% 调用方不带 key_ref 时，cs_session_app 在参数归一化处经
%% `imboy.eb_enterprise_keyring` 解析当前 active key_ref（服务端装配）；
%% 显式注入（测试/内部合同）优先于 env。env 缺失时不降级——下游
%% canonical tx 的 seal fail-closed（500 面，由 handler/契约套件覆盖）。
a03_env_keyring_assembly() ->
    reset_all(),
    Ctx = claimed_session(?SEAT_A),
    #{session_id := SessionId, conversation_id := Conv, identity := Identity} = Ctx,
    Key = crypto:strong_rand_bytes(32),
    Env = #{active_version => 1, keys => #{1 => binary:encode_hex(Key, lowercase)}},
    ok = application:set_env(imboy, eb_enterprise_keyring, Env),
    try
        %% (1) 不带 key_ref：装配层注入 env 解析出的 active key_ref。
        {ok, _} = cs_session_app:append_session_message(
            ?ORG,
            params(#{
                session_id => SessionId,
                business_identity_id => Identity,
                actor_user_id => ?USER_A,
                client_msg_id => <<"cmsg-env-1">>,
                body => <<"assembled server-side">>,
                accepted_at => ?T0 + 40,
                canonical_tx => cs_fake_canonical_tx
            })
        ),
        [{_, _, TxParams}] = cs_fake_canonical_tx:calls(),
        KeyRef = maps:get(key_ref, TxParams),
        ?assertEqual(1, maps:get(key_version, KeyRef)),
        ?assertEqual(#{1 => Key}, maps:get(keys, KeyRef)),
        %% (2) 显式注入优先于 env（既有测试/内部调用合同不破坏）。
        Explicit = #{key => crypto:strong_rand_bytes(32), key_version => 3},
        ok = cs_fake_canonical_tx:reset(),
        {ok, _} = cs_session_app:append_session_message(
            ?ORG,
            params(#{
                session_id => SessionId,
                business_identity_id => Identity,
                actor_user_id => ?USER_A,
                client_msg_id => <<"cmsg-env-2">>,
                body => <<"explicit wins">>,
                key_ref => Explicit,
                accepted_at => ?T0 + 41,
                canonical_tx => cs_fake_canonical_tx
            })
        ),
        [{_, _, TxParams2}] = cs_fake_canonical_tx:calls(),
        ?assertEqual(Explicit, maps:get(key_ref, TxParams2))
    after
        %% env 是 VM 级：用例内即清理，不让后续用例看见本 keyring。
        _ = application:unset_env(imboy, eb_enterprise_keyring)
    end,
    _ = Conv,
    ok.

%% ===================================================================
%% 构造辅助
%% ===================================================================

params(Extra) ->
    maps:merge(
        #{
            workspace_id => ?WS,
            store => ?FAKE,
            id => cs_fake_id,
            %% ORG-08：archived Org 门（C16）在纯单元套件用恒 active 替身，
            %% archived 分支的真库行为见 cs_org_compat_tests A04。
            org_lifecycle => cs_fake_org_lifecycle
        },
        Extra
    ).

fresh_seat(IdentityId) ->
    ensure_seat(IdentityId, 1),
    ok = ?FAKE:seed_assignment_user(?ORG, IdentityId, ?USER_A),
    {ok, IdentityId}.

ensure_seat(IdentityId, MaxConcurrent) ->
    ok = ?FAKE:seed_identity_function(?ORG, IdentityId, <<"customer_service">>),
    case
        cs_seat_app:create_seat(
            ?ORG,
            params(#{
                business_identity_id => IdentityId, max_concurrent => MaxConcurrent
            })
        )
    of
        {ok, _} -> ok;
        %% 前序用例已建（fake 状态跨用例保留）——幂等
        {error, conflict} -> ok
    end.

reset_all() ->
    ok = ?FAKE:init(),
    cs_fake_id:reset(),
    cs_fake_canonical_tx:reset(),
    ok.

%% 造一个「已 claim 到 IdentityId」的会话；返回上下文。
claimed_session(IdentityId) ->
    {ok, IdentityId} = fresh_seat(IdentityId),
    S = open_session(),
    SessionId = maps:get(id, S),
    {ok, Active} = cs_session_app:claim(
        ?ORG,
        params(#{
            session_id => SessionId,
            business_identity_id => IdentityId,
            expected_version => 1,
            at => ?T0
        })
    ),
    #{
        session_id => SessionId,
        identity => IdentityId,
        conversation_id => maps:get(conversation_id, Active),
        version => maps:get(version, Active)
    }.

open_session() ->
    {ok, S} = cs_session_app:open_session(
        ?ORG,
        params(#{
            contact_id => ?CONTACT,
            conversation_id => ?CONV + cs_fake_store:next_seq(),
            at => ?T0 - 1,
            created_by_user_id => ?USER_A
        })
    ),
    S.
