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
