-module(ops_report_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ops_report_logic EUnit 测试（P0-3 A3-4）
%%% 覆盖：compute 统计聚合（mock repo）+ render 文本拼装（纯函数）
%%%       + deliver 配置门控 + run_weekly_report 故障安全。
%%%===================================================================

%% mock ops_report_repo 返回固定统计值
-define(REPO_MECK, [
    {ops_report_repo, [
        {'count_new_users', 2, fun(_, _) -> 12 end},
        {'count_active_users', 2, fun(_, _) -> 34 end},
        {'count_messages', 2, fun(_, _) -> 567 end},
        {'count_reports', 2, fun(_, _) -> 3 end},
        {'top_report_reasons', 3, fun(_, _, _) -> [{<<"刷屏"/utf8>>, 2}, {<<"广告"/utf8>>, 1}] end}
    ]}
]).

%% ===================================================================
%% compute：聚合统计 map
%% ===================================================================

compute_aggregates_stats_test_() ->
    ?WITH_MECKS(?REPO_MECK, fun() ->
        {ok, Stats} = ops_report_logic:compute(),
        ?assertEqual(12, maps:get(new_users, Stats)),
        ?assertEqual(34, maps:get(active_users, Stats)),
        ?assertEqual(567, maps:get(messages, Stats)),
        ?assertEqual(3, maps:get(reports, Stats)),
        ?assertEqual(2, length(maps:get(top_reasons, Stats))),
        %% since/until 是 rfc3339 binary
        ?assert(is_binary(maps:get(since, Stats))),
        ?assert(is_binary(maps:get(until, Stats)))
    end).

%% ===================================================================
%% render：纯函数文本拼装
%% ===================================================================

render_includes_all_metrics_test_() ->
    ?TEST_SIMPLE(fun() ->
        Stats = #{
            since => <<"2026-07-14T00:00:00Z">>,
            until => <<"2026-07-21T00:00:00Z">>,
            new_users => 12,
            active_users => 34,
            messages => 567,
            reports => 3,
            top_reasons => [{<<"刷屏"/utf8>>, 2}]
        },
        Text = ops_report_logic:render(Stats),
        ?assert(is_binary(Text)),
        %% 所有指标数值都出现在文本里
        ?assertNotEqual(nomatch, binary:match(Text, <<"新增用户：12"/utf8>>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"活跃用户：34"/utf8>>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"消息总量：567"/utf8>>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"举报工单：3"/utf8>>)),
        %% 周期范围
        ?assertNotEqual(nomatch, binary:match(Text, <<"2026-07-14"/utf8>>)),
        %% 举报热点
        ?assertNotEqual(nomatch, binary:match(Text, <<"刷屏"/utf8>>))
    end).

render_empty_top_reasons_omits_section_test_() ->
    ?TEST_SIMPLE(fun() ->
        Stats = #{
            since => <<"2026-07-14T00:00:00Z">>,
            until => <<"2026-07-21T00:00:00Z">>,
            new_users => 0,
            active_users => 0,
            messages => 0,
            reports => 0,
            top_reasons => []
        },
        Text = ops_report_logic:render(Stats),
        %% 无举报热点时不渲染该段
        ?assertEqual(nomatch, binary:match(Text, <<"举报热点"/utf8>>))
    end).

%% ===================================================================
%% deliver：未配置 operator_uid/sender_uid 时 no-op
%% ===================================================================

deliver_skips_when_unconfigured_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [{'get', 2, fun(_, _) -> 0 end}]},
            {ai_agent_proactive, [{'send_text', 3, fun(_, _, _) -> ok end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}
        ],
        fun() ->
            %% operator_uid=0 sender_uid=0 → 不发，不调 send_text
            ?assertEqual(ok, ops_report_logic:deliver(<<"周报"/utf8>>)),
            ?assertNot(meck:called(ai_agent_proactive, send_text, '_'))
        end
    ).

deliver_sends_when_configured_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"ops_report.operator_uid">>, _) -> 99;
                    (<<"ops_report.sender_uid">>, _) -> 7
                end}
            ]},
            {ai_agent_proactive, [{'send_text', 3, fun(_, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(ok, ops_report_logic:deliver(<<"周报内容"/utf8>>)),
            %% sender(7) → operator(99)
            ?assert(meck:called(ai_agent_proactive, send_text, [7, 99, <<"周报内容"/utf8>>]))
        end
    ).

%% ===================================================================
%% run_weekly_report：故障安全（恒 ok）
%% ===================================================================

run_weekly_report_crash_safe_test_() ->
    ?WITH_MECKS(
        [
            %% 让 compute 抛崩
            {ops_report_repo, [{'count_new_users', 2, fun(_, _) -> error(db_down) end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}
        ],
        fun() ->
            %% 任何异常不抛，恒 ok
            ?assertEqual(ok, ops_report_logic:run_weekly_report())
        end
    ).

%% ===================================================================
%% R17：周报范围对齐周一午夜（日历周构造，可注入参考时间）
%% ===================================================================

%% R17-1：周一 09:00 与周一 17:00 触发得到同一统计窗口（cron 钟点不影响窗口）
r17_same_window_monday_morning_evening_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 2026-10-19 是周一
        Mon09 = {{2026, 10, 19}, {9, 0, 0}},
        Mon17 = {{2026, 10, 19}, {17, 0, 0}},
        ?assertEqual(
            ops_report_logic:last_week_range(Mon09), ops_report_logic:last_week_range(Mon17)
        )
    end).

%% R17-2：一周内任意星期运行均得到同一窗口（本周一 00:00 ~ 上周一 00:00）
r17_any_weekday_same_window_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 2026-10-19(周一) .. 2026-10-25(周日)，窗口恒为 [2026-10-12 00:00, 2026-10-19 00:00)
        Expected = ops_report_logic:last_week_range({{2026, 10, 19}, {9, 0, 0}}),
        [
            ?assertEqual(Expected, ops_report_logic:last_week_range(DT))
         || DT <- [
                {{2026, 10, 19}, {0, 0, 1}},
                {{2026, 10, 20}, {3, 47, 0}},
                {{2026, 10, 21}, {12, 0, 0}},
                {{2026, 10, 22}, {23, 59, 59}},
                {{2026, 10, 23}, {6, 30, 0}},
                {{2026, 10, 24}, {15, 15, 0}},
                {{2026, 10, 25}, {20, 0, 0}}
            ]
        ],
        {Since, Until} = Expected,
        ?assertEqual(<<"2026-10-12">>, binary:part(Since, 0, 10)),
        ?assertEqual(<<"2026-10-19">>, binary:part(Until, 0, 10))
    end).

%% R17-3：跨年与闰年边界
r17_year_boundary_and_leap_year_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 跨年：2026-01-01 是周四，所在周为 2025-12-29(周一)~2026-01-04(周日)
        %% → 上周 [2025-12-22 00:00, 2025-12-29 00:00)
        {Since1, Until1} = ops_report_logic:last_week_range({{2026, 1, 1}, {10, 0, 0}}),
        ?assertEqual(<<"2025-12-22">>, binary:part(Since1, 0, 10)),
        ?assertEqual(<<"2025-12-29">>, binary:part(Until1, 0, 10)),
        %% 闰年：2024-02-29 是周四 → 上周 [2024-02-19 00:00, 2024-02-26 00:00)
        {Since2, Until2} = ops_report_logic:last_week_range({{2024, 2, 29}, {10, 0, 0}}),
        ?assertEqual(<<"2024-02-19">>, binary:part(Since2, 0, 10)),
        ?assertEqual(<<"2024-02-26">>, binary:part(Until2, 0, 10))
    end).

%% R17-4：窗口两端对齐 00:00:00、since < until、恒差 7 天（[start,end) 半开）；
%%         SQL 侧半开区间（>= $1 AND < $2）由 ops_report_repo 保证
r17_midnight_aligned_half_open_test_() ->
    ?TEST_SIMPLE(fun() ->
        {Since, Until} = ops_report_logic:last_week_range({{2026, 10, 21}, {13, 5, 0}}),
        ?assertEqual(<<"00:00:00">>, binary:part(Since, 11, 8)),
        ?assertEqual(<<"00:00:00">>, binary:part(Until, 11, 8)),
        %% 同格式 rfc3339 字符串比较等价于瞬时比较：since < until（半开下界）
        ?assert(Since < Until),
        SinceDays = calendar:date_to_gregorian_days(
            {
                binary_to_integer(binary:part(Since, 0, 4)),
                binary_to_integer(binary:part(Since, 5, 2)),
                binary_to_integer(binary:part(Since, 8, 2))
            }
        ),
        UntilDays = calendar:date_to_gregorian_days(
            {
                binary_to_integer(binary:part(Until, 0, 4)),
                binary_to_integer(binary:part(Until, 5, 2)),
                binary_to_integer(binary:part(Until, 8, 2))
            }
        ),
        ?assertEqual(7, UntilDays - SinceDays)
    end).

%% R17-4（SQL 侧）：统计查询使用 [start, end) 半开区间（>= 与 <，无 <=）
r17_sql_uses_half_open_interval_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun(Sql, _Args) ->
                    put(r17_sqls, [
                        Sql
                        | case get(r17_sqls) of
                            undefined -> [];
                            L -> L
                        end
                    ]),
                    {ok, [#{<<"n">> => 1}]}
                end}
            ]}
        ],
        fun() ->
            {ok, _Stats} = ops_report_logic:compute(),
            Sqls =
                case get(r17_sqls) of
                    undefined -> [];
                    L -> L
                end,
            ?assert(length(Sqls) >= 4),
            %% 半开区间：每条范围 SQL 同时含 >= 与 <，不含闭区间 <=
            [
                begin
                    ?assertNotEqual(nomatch, binary:match(Sql, <<">= $1">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"< $2">>)),
                    ?assertEqual(nomatch, binary:match(Sql, <<"<= $2">>))
                end
             || Sql <- Sqls
            ]
        end
    ).
