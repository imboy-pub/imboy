%%% @doc D03（IMBOY-TIMEZONE-DECISIONS-V1，2026-10-27 冻结）：客服统计
%%% zone 自然日模式——真实 PG focused 套件。
%%%
%%% 覆盖（决策书 §3.5 AC）：
%%%   R16-1 NY 2026-03-08 窗口 05:00Z → 次日 04:00Z（23h）且回显一致；
%%%   R16-2 NY 2026-11-01 窗口 04:00Z → 次日 05:00Z（25h）；
%%%   R16-7 半开区间边界：queued_at=window_start 计入、=window_end 排外；
%%%   R16-5 zone 与 tz_offset 同现 → zone_tz_offset_conflict（HTTP 400）；
%%%         非法 zone → {error,{invalid_zone,_}}（HTTP 422 invalid_zone）；
%%%   R16-3 旧模式（date+tz_offset/全缺省）固定 24h 逐字段不变（回归）。
%%%
%%% 期望窗口 epoch 为独立手算（calendar 格里历换算），不经被测代码反推。
%%% 隔离：cs_pg_test_fixture 随机 TSID scope，无真实数据；环境不可用即
%%% error（不静默 skip）。R16-4 查看者无关性由四时区独立进程矩阵钉住
%%% （同 org/date/zone 请求不因进程 TZ 改变窗口——窗口锚 PG tzdata）。
-module(cs_stats_zone_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).
-define(GREG_EPOCH, 62167219200).

cs_stats_zone_pg_test_() ->
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
        {timeout, 60, fun ny_spring_23h_window/0},
        {timeout, 60, fun ny_fall_25h_window/0},
        {timeout, 60, fun half_open_boundary_membership/0},
        {timeout, 60, fun conflict_and_invalid_zone/0},
        {timeout, 60, fun fixed_offset_regression/0}
    ];
cases({error, Reason}) ->
    erlang:error({d03_pg_suite_db_unavailable, Reason}).

epoch({{Y, Mo, D}, {H, Mi, S}}) ->
    calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, S}}) - ?GREG_EPOCH.

%% ---- 用例 ----

%% R16-1：2026-03-08（春季拨快）：当地午夜=05:00Z（EST），次日午夜=04:00Z
%% （EDT）→ 23 小时窗口；回显 zone/window_mode/date 全一致。
ny_spring_23h_window() ->
    Scope = ?FIX:new_scope(),
    try
        Ws = ws(Scope),
        {ok, Stats} = cs_session_app:session_stats(org(Scope), #{
            workspace_id => Ws,
            date => <<"2026-03-08">>,
            zone => <<"America/New_York">>
        }),
        ExpStart = epoch({{2026, 3, 8}, {5, 0, 0}}),
        ExpEnd = epoch({{2026, 3, 9}, {4, 0, 0}}),
        ?assertEqual(natural_day, maps:get(window_mode, Stats)),
        ?assertEqual(<<"America/New_York">>, maps:get(zone, Stats)),
        ?assertEqual(<<"2026-03-08">>, maps:get(date, Stats)),
        ?assertEqual(null, maps:get(tz_offset, Stats)),
        ?assertEqual(ExpStart, maps:get(window_start, Stats)),
        ?assertEqual(ExpEnd, maps:get(window_end, Stats)),
        ?assertEqual(23 * 3600, maps:get(window_end, Stats) - maps:get(window_start, Stats))
    after
        ?FIX:cleanup(Scope)
    end.

%% R16-2：2026-11-01（秋季回拨）：当地午夜=04:00Z（EDT），次日午夜=05:00Z
%% （EST）→ 25 小时窗口。
ny_fall_25h_window() ->
    Scope = ?FIX:new_scope(),
    try
        {ok, Stats} = cs_session_app:session_stats(org(Scope), #{
            workspace_id => ws(Scope),
            date => <<"2026-11-01">>,
            zone => <<"America/New_York">>
        }),
        ExpStart = epoch({{2026, 11, 1}, {4, 0, 0}}),
        ExpEnd = epoch({{2026, 11, 2}, {5, 0, 0}}),
        ?assertEqual(ExpStart, maps:get(window_start, Stats)),
        ?assertEqual(ExpEnd, maps:get(window_end, Stats)),
        ?assertEqual(25 * 3600, maps:get(window_end, Stats) - maps:get(window_start, Stats)),
        ?assertEqual(natural_day, maps:get(window_mode, Stats))
    after
        ?FIX:cleanup(Scope)
    end.

%% R16-7：半开区间 [start, end)——queued_at=start 计入 new；=end 排外。
half_open_boundary_membership() ->
    Scope = ?FIX:new_scope(),
    try
        Start = epoch({{2026, 3, 8}, {5, 0, 0}}),
        End = epoch({{2026, 3, 9}, {4, 0, 0}}),
        %% 当地 23:30 前夜（=end-1800 与 end 边界对照）：start 精确含入
        _InAtStart = open_session(Scope, conv(Scope), Start),
        _InLate = open_session(Scope, conv(Scope), End - 1),
        _OutAtEnd = open_session(Scope, conv(Scope), End),
        {ok, Stats} = cs_session_app:session_stats(org(Scope), #{
            workspace_id => ws(Scope),
            date => <<"2026-03-08">>,
            zone => <<"America/New_York">>
        }),
        ?assertEqual(2, maps:get(new_sessions, Stats))
    after
        ?FIX:cleanup(Scope)
    end.

%% R16-5：参数冲突与非法 zone。
conflict_and_invalid_zone() ->
    Scope = ?FIX:new_scope(),
    try
        ?assertEqual(
            {error, zone_tz_offset_conflict},
            cs_session_app:session_stats(org(Scope), #{
                date => <<"2026-03-08">>,
                zone => <<"America/New_York">>,
                tz_offset => 480
            })
        ),
        ?assertEqual(
            {error, {invalid_zone, <<"Asia/Foo">>}},
            cs_session_app:session_stats(org(Scope), #{
                date => <<"2026-03-08">>,
                zone => <<"Asia/Foo">>
            })
        ),
        %% 非 binary zone 形状同判 invalid_zone（动作表面 binary，此处钉
        %% app 层域门不被绕过）
        ?assertEqual(
            {error, {invalid_zone, 123}},
            cs_session_app:session_stats(org(Scope), #{
                date => <<"2026-03-08">>,
                zone => 123
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% R16-3：旧模式回归——date+tz_offset 固定 24h 逐字段不变（新键
%% zone=null / window_mode=fixed_offset 为纯增量回显）。
fixed_offset_regression() ->
    Scope = ?FIX:new_scope(),
    try
        %% date=2023-11-14, tz_offset 缺省 0：[1699920000, 1700006400)
        {ok, Stats} = cs_session_app:session_stats(org(Scope), #{
            workspace_id => ws(Scope),
            date => <<"2023-11-14">>
        }),
        ?assertEqual(1699920000, maps:get(window_start, Stats)),
        ?assertEqual(1700006400, maps:get(window_end, Stats)),
        ?assertEqual(fixed_offset, maps:get(window_mode, Stats)),
        ?assertEqual(null, maps:get(zone, Stats)),
        ?assertEqual(0, maps:get(tz_offset, Stats)),
        %% tz_offset=480：窗口西移 8h，仍固定 24h
        {ok, Stats2} = cs_session_app:session_stats(org(Scope), #{
            workspace_id => ws(Scope),
            date => <<"2023-11-14">>,
            tz_offset => 480
        }),
        ?assertEqual(1699920000 - 480 * 60, maps:get(window_start, Stats2)),
        ?assertEqual(1700006400 - 480 * 60, maps:get(window_end, Stats2)),
        ?assertEqual(86400, maps:get(window_end, Stats2) - maps:get(window_start, Stats2))
    after
        ?FIX:cleanup(Scope)
    end.

%% ---- 辅助（与 cs_stats_pg_tests 同构） ----

org(Scope) ->
    maps:get(org_id, Scope).

ws(Scope) ->
    maps:get(workspace_id, Scope).

conv(Scope) ->
    ConversationId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'d03-zone-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [
            ConversationId,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(service_identity_id, Scope),
            <<"d03-zone-consent-", (integer_to_binary(ConversationId))/binary>>
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
