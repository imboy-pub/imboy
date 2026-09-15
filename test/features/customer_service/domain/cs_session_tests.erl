%%% @doc 客服会话状态机套件（纯 domain，零 I/O、零 mock）。
%%%
%%% 覆盖 CS-01 的状态机核心：
%%%   * queued → active → closed 的合法迁移与非法迁移负例（重复 close）；
%%%   * rating：1..5 校验、仅 closed 可评、不可重复评；
%%%   * CAS 期望值：并发 claim 恰一个成功在 domain 层的判定真源（期望状态+版本）；
%%%   * A04：identity rebind（transfer）前后会话主体字段零迁移的连续性判定。
%%%
%%% 时间、ID、actor 一律由测试显式构造，不读系统时间（铁律 4）。
-module(cs_session_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 固定测试常量（无隐式时间/随机源）
%% ===================================================================

-define(ORG, 700000000000001).
-define(WS, 700000000000002).
-define(CONTACT, 700000000000003).
-define(CONV, 700000000000004).
-define(SEAT_A, 700000000000011).
-define(SEAT_B, 700000000000012).
-define(T0, 1700000000).
-define(T1, 1700000060).
-define(T2, 1700000120).

%% ===================================================================
%% queued → active → closed 主链
%% ===================================================================

session_lifecycle_queued_to_active_to_closed_test() ->
    S = new_session(),
    %% queued → active（claim）
    {ok, Active} = cs_session:transition(S, active, #{at => ?T1, business_identity_id => ?SEAT_A}),
    ?assertEqual(active, maps:get(status, Active)),
    ?assertEqual(?SEAT_A, maps:get(business_identity_id, Active)),
    ?assertEqual(?T1, maps:get(claimed_at, Active)),
    ?assertEqual(maps:get(version, S) + 1, maps:get(version, Active)),
    %% active → closed
    {ok, Closed} = cs_session:transition(Active, closed, #{at => ?T2, reason => <<"done">>}),
    ?assertEqual(closed, maps:get(status, Closed)),
    ?assertEqual(?T2, maps:get(closed_at, Closed)),
    ?assertEqual(<<"done">>, maps:get(close_reason, Closed)),
    ?assertEqual(maps:get(version, Active) + 1, maps:get(version, Closed)).

queued_to_closed_is_allowed_test() ->
    S = new_session(),
    {ok, Closed} = cs_session:transition(S, closed, #{at => ?T1, reason => <<"abandoned">>}),
    ?assertEqual(closed, maps:get(status, Closed)),
    ?assertEqual(<<"abandoned">>, maps:get(close_reason, Closed)).

%% ===================================================================
%% 非法迁移负例
%% ===================================================================

double_close_is_rejected_test() ->
    S = new_session(),
    {ok, Closed} = cs_session:transition(S, closed, #{at => ?T1, reason => <<"first">>}),
    ?assertMatch(
        {error, session_already_closed},
        cs_session:transition(Closed, closed, #{at => ?T2, reason => <<"second">>})
    ),
    ?assertMatch(
        {error, session_already_closed},
        cs_session:transition(Closed, active, #{at => ?T2, business_identity_id => ?SEAT_A})
    ),
    ?assertMatch(
        {error, session_already_closed},
        cs_session:transition(Closed, queued, #{at => ?T2})
    ).

claim_of_active_session_is_rejected_test() ->
    S = new_session(),
    {ok, Active} = cs_session:transition(S, active, #{at => ?T1, business_identity_id => ?SEAT_A}),
    %% 已经 active 的会话不能再被第二个人 claim（A02 的 domain 判定真源）
    ?assertMatch(
        {error, {not_claimable, active}},
        cs_session:transition(Active, active, #{at => ?T2, business_identity_id => ?SEAT_B})
    ).

%% ===================================================================
%% CAS 期望判定（A02 的 domain 侧真源）
%% ===================================================================

cas_expectation_matches_only_current_shape_test() ->
    S = new_session(),
    ok = cs_session:assert_cas_expectation(S, queued, maps:get(version, S)),
    ?assertMatch(
        {error, {cas_mismatch, _}},
        cs_session:assert_cas_expectation(S, active, maps:get(version, S))
    ),
    ?assertMatch(
        {error, {cas_mismatch, _}},
        cs_session:assert_cas_expectation(S, queued, maps:get(version, S) + 7)
    ).

%% ===================================================================
%% rating
%% ===================================================================

rating_accepts_one_to_five_on_closed_session_test() ->
    Closed = closed_session(),
    lists:foreach(
        fun(R) ->
            {ok, Rated} = cs_session:rate(Closed, R, ?T2),
            ?assertEqual(R, maps:get(rating, Rated)),
            ?assertEqual(?T2, maps:get(rating_at, Rated)),
            ?assertEqual(maps:get(version, Closed) + 1, maps:get(version, Rated))
        end,
        [1, 2, 3, 4, 5]
    ).

rating_rejects_out_of_range_and_non_integer_test() ->
    Closed = closed_session(),
    lists:foreach(
        fun(Bad) ->
            ?assertMatch({error, {invalid_rating, _}}, cs_session:rate(Closed, Bad, ?T2))
        end,
        [0, 6, -1, <<"5">>, 3.5, five]
    ).

rating_requires_closed_session_test() ->
    ?assertMatch(
        {error, {rating_requires_closed, queued}},
        cs_session:rate(new_session(), 5, ?T2)
    ),
    {ok, Active} = cs_session:transition(new_session(), active, #{
        at => ?T1, business_identity_id => ?SEAT_A
    }),
    ?assertMatch(
        {error, {rating_requires_closed, active}},
        cs_session:rate(Active, 5, ?T2)
    ).

double_rating_is_rejected_test() ->
    Closed0 = closed_session(),
    {ok, Rated} = cs_session:rate(Closed0, 4, ?T2),
    ?assertMatch({error, already_rated}, cs_session:rate(Rated, 5, ?T2 + 1)),
    ?assertMatch({error, already_rated}, cs_session:rate(Rated, 1, ?T2 + 1)).

%% ===================================================================
%% A04：rebind / transfer 连续性
%% ===================================================================

transfer_keeps_subject_fields_and_bumps_version_test() ->
    {ok, Active} = cs_session:transition(new_session(), active, #{
        at => ?T1, business_identity_id => ?SEAT_A
    }),
    {ok, Transferred} = cs_session:transfer(Active, ?SEAT_B, ?T2),
    ?assertEqual(?SEAT_B, maps:get(business_identity_id, Transferred)),
    %% 主体字段零迁移：org / workspace / contact / conversation / id 不变
    ?assertEqual(maps:get(organization_id, Active), maps:get(organization_id, Transferred)),
    ?assertEqual(maps:get(workspace_id, Active), maps:get(workspace_id, Transferred)),
    ?assertEqual(maps:get(contact_id, Active), maps:get(contact_id, Transferred)),
    ?assertEqual(maps:get(conversation_id, Active), maps:get(conversation_id, Transferred)),
    ?assertEqual(maps:get(id, Active), maps:get(id, Transferred)),
    %% 只有经办 identity 与 version/updated_at 变化
    ?assertEqual(maps:get(version, Active) + 1, maps:get(version, Transferred)),
    ?assertEqual(?T2, maps:get(updated_at, Transferred)).

transfer_of_closed_session_is_rejected_test() ->
    Closed = closed_session(),
    ?assertMatch({error, session_already_closed}, cs_session:transfer(Closed, ?SEAT_B, ?T2)).

rebind_continuity_compare_test() ->
    %% A04 判定真源：rebind 前后主体字段一致才叫连续
    Before = new_session(),
    After = (new_session())#{business_identity_id => ?SEAT_B, status => active, version => 2},
    ?assertEqual(ok, cs_session:assert_rebind_continuity(Before, After)),
    BrokenOwner = (new_session())#{organization_id => 999},
    ?assertMatch(
        {error, {continuity_broken, organization_id}},
        cs_session:assert_rebind_continuity(Before, BrokenOwner)
    ),
    BrokenSubject = (new_session())#{contact_id => 999},
    ?assertMatch(
        {error, {continuity_broken, contact_id}},
        cs_session:assert_rebind_continuity(Before, BrokenSubject)
    ).

%% ===================================================================
%% A05：访客域判定（token 只能作用于绑定的 Org+contact）
%% ===================================================================

visitor_scope_allows_bound_org_and_contact_only_test() ->
    Token = #{
        organization_id => ?ORG,
        contact_id => ?CONTACT,
        expires_at => ?T2,
        revoked_at => undefined
    },
    ok = cs_session:assert_visitor_scope(Token, ?ORG, ?CONTACT),
    ?assertMatch({error, cross_org}, cs_session:assert_visitor_scope(Token, 999, ?CONTACT)),
    ?assertMatch(
        {error, contact_mismatch},
        cs_session:assert_visitor_scope(Token, ?ORG, 888)
    ).

visitor_scope_rejects_expired_token_test() ->
    Token = #{
        organization_id => ?ORG,
        contact_id => ?CONTACT,
        expires_at => ?T1,
        revoked_at => undefined
    },
    ?assertMatch(
        {error, token_expired}, cs_session:assert_visitor_scope(Token, ?ORG, ?CONTACT, ?T1)
    ),
    ok = cs_session:assert_visitor_scope(Token, ?ORG, ?CONTACT, ?T1 - 1).

visitor_scope_rejects_revoked_token_test() ->
    Token = #{
        organization_id => ?ORG,
        contact_id => ?CONTACT,
        expires_at => ?T2,
        revoked_at => ?T0
    },
    ?assertMatch(
        {error, token_revoked},
        cs_session:assert_visitor_scope(Token, ?ORG, ?CONTACT, ?T0 + 1)
    ).

%% ===================================================================
%% 构造辅助
%% ===================================================================

new_session() ->
    #{
        id => 700000000000100,
        organization_id => ?ORG,
        workspace_id => ?WS,
        contact_id => ?CONTACT,
        conversation_id => ?CONV,
        business_identity_id => undefined,
        status => queued,
        rating => undefined,
        rating_at => undefined,
        queued_at => ?T0,
        claimed_at => undefined,
        closed_at => undefined,
        close_reason => undefined,
        version => 1,
        updated_at => ?T0
    }.

closed_session() ->
    {ok, Active} = cs_session:transition(new_session(), active, #{
        at => ?T1, business_identity_id => ?SEAT_A
    }),
    {ok, Closed} = cs_session:transition(Active, closed, #{at => ?T2, reason => <<"done">>}),
    Closed.
