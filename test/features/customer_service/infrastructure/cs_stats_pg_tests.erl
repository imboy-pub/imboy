%%% @doc CS-BE-07（按需客服统计）真库 focused 套件：`cs_pg_session` 三轴
%%% 窗口聚合 SQL（SQL_STATS_*，to_timestamp epoch 比较，与 DB 会话时区无关）
%%% 在真实 PG 上的全链验证——数据经真实写入路径落库（cs_session_app 的
%%% open/claim/close/rate），期望值为独立手算（见各用例注释），不经被测
%%% 代码反推。
%%%
%%% 指标公式（与 cs_stats_tests 的 fake 口径同一冻结语义）：
%%%   * new_sessions = |queued_at ∈ W|（含 start、排外 end）
%%%   * first_response = {count, avg_seconds} over W 内新会话且已 claim
%%%     （avg = 平均 claimed_at − queued_at 秒；未 claim 不进分母）
%%%   * closed_sessions = |closed_at ∈ W|（关闭轴独立）
%%%   * rating = {count, avg} over rating_at ∈ W（未评分不进分母）
%%%   * current = 当前时刻 queued/active 计数
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；无真实数据。
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
%%% 本套件由 A0 在 scratch PG 队列独占运行；本地门禁只做编译自检。
-module(cs_stats_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

%% 2023-11-14 00:00 UTC = 1699920000；窗口 [1699920000, 1700006400)。
-define(DAY, <<"2023-11-14">>).
-define(START, 1699920000).
-define(END, 1700006400).

cs_stats_pg_test_() ->
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
        {timeout, 60, fun metrics_hand_computed_over_real_writes/0},
        {timeout, 60, fun cross_org_isolation_and_empty_semantics/0}
    ];
cases({error, Reason}) ->
    erlang:error({csbe07_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% 用例
%% ===================================================================

%% 手算复核（窗口 [1699920000, 1700006400)，date=2023-11-14 tz=0）：
%%   S1 open(=start，含) → claim(+100s) → close(+200) → rate 5(+300)
%%       → new+1 / frt 样本 100s / closed+1 / rated+1
%%   S2 open(+30000) → claim(+30450)（active 未关闭）
%%       → new+1 / frt 样本 450s / 不进 closed
%%   S3 open(=end-1=1700006399，含) → 仍 queued → new+1
%%   S4 open(=end=1700006400，排外) → 不进 new；current.queued+1
%% 期望：new=3；frt count=2 avg=(100+450)/2=275.0；closed=1；rated=1 avg=5.0；
%% current queued=2 active=1。窗口回显 epoch 秒（UTC instant）。
metrics_hand_computed_over_real_writes() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        %% S1：完整生命周期（open → claim → close → rate）。
        S1 = open_session(Scope, conv(Scope), ?START),
        {ok, Active1} = claim(Scope, S1, ?START + 100),
        {ok, Closed1} = close(Scope, S1, maps:get(version, Active1), ?START + 200),
        {ok, _} = rate(Scope, S1, maps:get(version, Closed1), 5, ?START + 300),
        %% S2：open + claim，保持 active（未关闭未评分）。
        S2 = open_session(Scope, conv(Scope), ?START + 30000),
        {ok, _} = claim(Scope, S2, ?START + 30450),
        %% S3：end-1（含）；S4：end（排外）。
        _S3 = open_session(Scope, conv(Scope), ?END - 1),
        _S4 = open_session(Scope, conv(Scope), ?END),
        {ok, Stats} = cs_session_app:session_stats(Org, #{
            workspace_id => Ws, date => ?DAY
        }),
        ?assertEqual(?START, maps:get(window_start, Stats)),
        ?assertEqual(?END, maps:get(window_end, Stats)),
        ?assertEqual(3, maps:get(new_sessions, Stats)),
        FRT = maps:get(first_response, Stats),
        ?assertEqual(2, maps:get(count, FRT)),
        ?assert(avg_close_to(275.0, maps:get(avg_seconds, FRT))),
        ?assertEqual(1, maps:get(closed_sessions, Stats)),
        Rating = maps:get(rating, Stats),
        ?assertEqual(1, maps:get(count, Rating)),
        ?assert(avg_close_to(5.0, maps:get(avg, Rating))),
        Current = maps:get(current, Stats),
        ?assertEqual(2, maps:get(queued, Current)),
        ?assertEqual(1, maps:get(active, Current)),
        %% 前一日窗口：无任何行（S1 的 closed_at 也在 11-14）。
        {ok, Prev} = cs_session_app:session_stats(Org, #{
            workspace_id => Ws, date => <<"2023-11-13">>
        }),
        ?assertEqual(0, maps:get(new_sessions, Prev)),
        ?assertEqual(0, maps:get(closed_sessions, Prev)),
        %% 后一日：S4（=11-15 start，含）唯一进窗。
        {ok, Next} = cs_session_app:session_stats(Org, #{
            workspace_id => Ws, date => <<"2023-11-15">>
        }),
        ?assertEqual(1, maps:get(new_sessions, Next))
    after
        ?FIX:cleanup(Scope)
    end.

%% 空 Org：计数全 0、均值 null（分母 0 不吐 0 伪装均值）、current 0。
%% 随后向该 Org 直插一行三轴全命中的会话（最小 FK 链），再断言：
%%   * 该 Org 自身可见（new=1）
%%   * 主 Org 的统计不受任何影响（跨 Org 隔离：A org 查不到 B org 行）。
cross_org_isolation_and_empty_semantics() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        %% 空 Org 全 0 / null。
        {ok, Empty} = cs_session_app:session_stats(OtherOrg, #{
            workspace_id => OtherWs, date => ?DAY
        }),
        ?assertEqual(0, maps:get(new_sessions, Empty)),
        ?assertEqual(#{count => 0, avg_seconds => undefined}, maps:get(first_response, Empty)),
        ?assertEqual(0, maps:get(closed_sessions, Empty)),
        ?assertEqual(#{count => 0, avg => undefined}, maps:get(rating, Empty)),
        ?assertEqual(#{queued => 0, active => 0}, maps:get(current, Empty)),
        %% 向 OtherOrg 直插三轴全命中的最小会话行（contact → conversation →
        %% session；queued/claimed/closed/rating 全在窗内）。
        %% 最小 FK 链（跨 Org 的身份/坐席不需要——contact/conversation 的
        %% identity 列均可空，session 的 business_identity_id 在 closed 态
        %% 无非空约束）。
        ContactId = ?FIX:id(),
        ConvId = ?FIX:id(),
        SessionId = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO enterprise_contact"
                " (id,organization_id,imboy_user_id,status,display_name,version)"
                " VALUES ($1,$2,NULL,'active','csbe07-x-org-contact',1)"
            >>,
            [ContactId, OtherOrg]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO enterprise_conversation"
                " (id,organization_id,workspace_id,contact_id,status,version,"
                "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
                " VALUES ($1,$2,$3,$4,'active',1,'csbe07-notice-v1',CURRENT_TIMESTAMP,$5,'synthetic')"
            >>,
            [ConvId, OtherOrg, OtherWs, ContactId, <<"csbe07-consent-x">>]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO customer_service_session"
                " (id,organization_id,workspace_id,contact_id,conversation_id,"
                "  business_identity_id,status,rating,rating_at,queued_at,claimed_at,closed_at)"
                " VALUES ($1,$2,$3,$4,$5,NULL,'closed',5,"
                "  to_timestamp($6::double precision),to_timestamp($7::double precision),"
                "  to_timestamp($8::double precision),to_timestamp($9::double precision))"
            >>,
            [
                SessionId,
                OtherOrg,
                OtherWs,
                ContactId,
                ConvId,
                ?START + 300,
                ?START,
                ?START + 100,
                ?START + 200
            ]
        ),
        %% B org 自身可见。
        {ok, BStats} = cs_session_app:session_stats(OtherOrg, #{
            workspace_id => OtherWs, date => ?DAY
        }),
        ?assertEqual(1, maps:get(new_sessions, BStats)),
        ?assertEqual(1, maps:get(closed_sessions, BStats)),
        ?assertEqual(1, maps:get(count, maps:get(rating, BStats))),
        %% A org（主）查不到 B org 行——仍是空窗。
        {ok, AStats} = cs_session_app:session_stats(Org, #{workspace_id => ws(Scope), date => ?DAY}),
        ?assertEqual(0, maps:get(new_sessions, AStats)),
        ?assertEqual(0, maps:get(closed_sessions, AStats))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 夹具辅助
%% ===================================================================

org(Scope) ->
    maps:get(org_id, Scope).

ws(Scope) ->
    maps:get(workspace_id, Scope).

%% 新 conversation（每会话一条：部分唯一索引 uq_csss_org_conv_open 不允许
%% 同 conversation 并存两个未关闭 session）。
conv(Scope) ->
    ConversationId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'csbe07-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [
            ConversationId,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(service_identity_id, Scope),
            <<"csbe07-consent-", (integer_to_binary(ConversationId))/binary>>
        ]
    ),
    ConversationId.

open_session(Scope, ConversationId, QueuedAt) ->
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => ConversationId,
        at => QueuedAt
    }),
    Session.

claim(Scope, Session, At) ->
    cs_session_app:claim(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => maps:get(id, Session),
        business_identity_id => maps:get(service_identity_id, Scope),
        expected_version => maps:get(version, Session),
        at => At
    }).

close(Scope, Session, ExpectedVersion, At) ->
    cs_session_app:close(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => maps:get(id, Session),
        expected_version => ExpectedVersion,
        at => At
    }).

rate(Scope, Session, ExpectedVersion, Rating, At) ->
    cs_session_app:rate(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => maps:get(id, Session),
        rating => Rating,
        expected_version => ExpectedVersion,
        at => At
    }).

avg_close_to(Expected, Actual) when is_number(Actual) ->
    abs(Expected - Actual) < 0.0001;
avg_close_to(_Expected, undefined) ->
    false.
