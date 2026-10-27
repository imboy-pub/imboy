-module(review_queue_repo_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc review_queue_repo R12-BE SQL 构造与 PG 时间边界回归。
%%% 覆盖：
%%%   - 纯日期 [start, endExclusive) 半开区间 + AT TIME ZONE 参数化 zone
%%%   - zone 缺省 UTC（与旧 session TimeZone=UTC 语义一致）
%%%   - rfc3339 时刻参数（start_at/end_at）直接瞬间比较
%%%   - 优先级：start_at/end_at > start/end
%%%   - 不引入 DATE(created_at) 包装（保持索引可用）
%%% P3（审查发现修复轮）：zone 传入即 IANA 校验（pg_timezone_names 权威），
%%% 非法 → {error, invalid_zone}（handler 层映射 400），PG 用例验证。
%%%===================================================================

%% 旧纯日期参数 + 缺省 zone → AT TIME ZONE UTC，半开区间
legacy_dates_default_utc_zone_test_() ->
    {Sql, Params} = review_queue_repo:build_where(#{
        start => <<"2026-10-26">>,
        'end' => <<"2026-10-26">>
    }),
    [
        ?_assert(is_binary(Sql)),
        %% 起始：当地日 00:00，zone 参数化（缺省 UTC）
        ?_assertNotEqual(
            nomatch,
            binary:match(Sql, <<"created_at >= (($1::text)::date)::timestamp AT TIME ZONE $2">>)
        ),
        %% 结束：次日 00:00（+1 day），半开 <
        ?_assertNotEqual(
            nomatch,
            binary:match(
                Sql, <<"created_at < ((($3::text)::date + 1)::timestamp AT TIME ZONE $4)">>
            )
        ),
        ?_assertEqual(nomatch, binary:match(Sql, <<"<= $">>)),
        ?_assertEqual([<<"2026-10-26">>, <<"UTC">>, <<"2026-10-26">>, <<"UTC">>], Params)
    ].

%% 纯日期 + 显式 IANA zone（上海/纽约）→ zone 作为参数透传
dates_with_explicit_zone_test_() ->
    {Sql, Params} = review_queue_repo:build_where(#{
        start => <<"2026-10-26">>,
        'end' => <<"2026-10-26">>,
        zone => <<"Asia/Shanghai">>
    }),
    [
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"AT TIME ZONE $2">>)),
        ?_assertEqual(
            [<<"2026-10-26">>, <<"Asia/Shanghai">>, <<"2026-10-26">>, <<"Asia/Shanghai">>],
            Params
        ),
        ?_assert(ny_checks())
    ].

%% rfc3339 时刻参数：直接瞬间比较，不出现 AT TIME ZONE
instant_bounds_direct_compare_test_() ->
    {Sql, Params} = review_queue_repo:build_where(#{
        start_at => <<"2026-10-26T00:00:00+08:00">>,
        end_at => <<"2026-10-27T00:00:00+08:00">>
    }),
    [
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"created_at >= $1::text::timestamptz">>)),
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"created_at < $2::text::timestamptz">>)),
        ?_assertEqual(nomatch, binary:match(Sql, <<"AT TIME ZONE">>)),
        ?_assertEqual(
            [<<"2026-10-26T00:00:00+08:00">>, <<"2026-10-27T00:00:00+08:00">>], Params
        )
    ].

%% 优先级：start_at/end_at 与 start/end 同时传 → 时刻参数生效，纯日期被忽略
instant_priority_over_dates_test_() ->
    {Sql, Params} = review_queue_repo:build_where(#{
        start => <<"2026-01-01">>,
        'end' => <<"2026-01-02">>,
        start_at => <<"2026-10-26T00:00:00Z">>,
        end_at => <<"2026-10-27T00:00:00Z">>
    }),
    [
        ?_assertEqual(nomatch, binary:match(Sql, <<"AT TIME ZONE">>)),
        ?_assertEqual(
            [<<"2026-10-26T00:00:00Z">>, <<"2026-10-27T00:00:00Z">>], Params
        )
    ].

%% 无时间参数：不生成时间条件；status/keyword 仍正常
no_time_filters_omits_conditions_test_() ->
    {Sql, Params} = review_queue_repo:build_where(#{status => <<"pending">>}),
    {Sql0, Params0} = review_queue_repo:build_where(#{}),
    [
        ?_assertEqual(<<" WHERE review_status = $1">>, Sql),
        ?_assertEqual([<<"pending">>], Params),
        %% 空过滤 → 空 WHERE
        ?_assertEqual(<<>>, Sql0),
        ?_assertEqual([], Params0)
    ].

%% 不把 DATE(created_at) 包装扩散到时间过滤（保持 created_at 索引可用）
no_date_wrap_on_column_test_() ->
    {Sql, _} = review_queue_repo:build_where(#{
        start => <<"2026-10-26">>,
        'end' => <<"2026-10-27">>,
        zone => <<"UTC">>
    }),
    [
        ?_assertEqual(nomatch, binary:match(Sql, <<"DATE(created_at)">>)),
        %% 列不被 ::date 包装（索引可用）；::date 只作用于参数侧（$N::date）
        ?_assertEqual(nomatch, binary:match(Sql, <<"created_at::date">>)),
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"($1::text)::date">>))
    ].

%% 时间条件在前 + status/keyword 在后的参数编号正确（端到端一致性）
combined_filters_param_order_test_() ->
    {Sql, Params} = review_queue_repo:build_where(#{
        status => <<"pending">>,
        keyword => <<"ab">>,
        start => <<"2026-10-26">>,
        'end' => <<"2026-10-27">>,
        zone => <<"Asia/Shanghai">>
    }),
    [
        %% $1/$2 起始日+zone，$3/$4 结束日+zone，$5 status，$6/$7 keyword
        ?_assertEqual(
            [
                <<"2026-10-26">>,
                <<"Asia/Shanghai">>,
                <<"2026-10-27">>,
                <<"Asia/Shanghai">>,
                <<"pending">>,
                <<"%ab%">>,
                <<"%ab%">>
            ],
            Params
        ),
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"review_status = $5">>)),
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"ILIKE $6">>))
    ].

%% 纽约 zone 同样参数化（R12-4 23/25 小时日由 PG AT TIME ZONE 保证）
ny_checks() ->
    {Sql2, Params2} =
        review_queue_repo:build_where(#{
            start => <<"2026-03-08">>,
            zone => <<"America/New_York">>
        }),
    nomatch =/= binary:match(Sql2, <<"AT TIME ZONE $2">>) andalso
        Params2 =:= [<<"2026-03-08">>, <<"America/New_York">>].

%% ===================================================================
%% P3（审查发现修复轮）：zone IANA 合法性校验（PG 权威名单）
%% ===================================================================

%% 非法 zone（语法上像 IANA 但不存在的 Area）→ {error, invalid_zone}，
%% 不再透传 SQL（旧行为：纯日期模式下 PG 运行时报错、时刻模式静默忽略）。
invalid_zone_rejected_test_() ->
    ?TEST_WITH_DB(fun() ->
        ?assertEqual(
            {error, invalid_zone},
            review_queue_repo:page(#{zone => <<"Not/AZone">>, start => <<"2026-10-26">>}, 1, 10)
        ),
        %% 时刻模式下同样拒绝（入口早失败，不再静默忽略）
        ?assertEqual(
            {error, invalid_zone},
            review_queue_repo:page(#{zone => <<"Not/AZone">>}, 1, 10)
        )
    end).

valid_zone_passes_test_() ->
    ?TEST_WITH_DB(fun() ->
        case
            review_queue_repo:page(
                #{
                    zone => <<"UTC">>,
                    start_at => <<"2026-10-26T00:00:00Z">>,
                    end_at => <<"2026-10-27T00:00:00Z">>
                },
                1,
                10
            )
        of
            {ok, #{total := Total}} -> ?assert(is_integer(Total));
            {error, Reason} -> ?assertEqual(ok, {zone_query_unexpected_error, Reason})
        end
    end).

%% 缺省/空值 zone 走 UTC 兜底，不触发校验（向后兼容）。
absent_zone_untouched_test_() ->
    ?TEST_WITH_DB(fun() ->
        ?assertMatch({ok, _}, review_queue_repo:page(#{}, 1, 10))
    end).

%% P3 修复轮：pg_timezone_names 校验查询自身失败（瞬态连接故障）→ 原因
%% 原样透传 {error, Reason}，不得误报 invalid_zone（误报会让 handler 把
%% 基础设施故障映射成"zone 参数非法"400，掩盖真实故障）。纯 meck，不依赖 DB。
zone_check_db_error_passthrough_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        meck:expect(elib_pg, one, fun(_Sql, _Args) -> {error, pg_conn_down} end),
        ?assertEqual(
            {error, pg_conn_down},
            review_queue_repo:page(#{zone => <<"Asia/Shanghai">>}, 1, 10)
        )
    after
        meck:unload(elib_pg)
    end.

calendar_day_boundaries_pg_test_() ->
    {setup,
        fun() ->
            {ok, Conn} = eunit_runner:eunit_setup_with_db(),
            Conn
        end,
        fun eunit_runner:eunit_cleanup_db/1, fun(_Conn) -> fun assert_calendar_day_boundaries/0 end}.

assert_calendar_day_boundaries() ->
    Cases = [
        {<<"America/New_York">>, <<"2026-03-08">>, <<"2026-03-08T05:00:00Z">>,
            <<"2026-03-09T04:00:00Z">>},
        {<<"America/New_York">>, <<"2026-11-01">>, <<"2026-11-01T04:00:00Z">>,
            <<"2026-11-02T05:00:00Z">>},
        {<<"Asia/Shanghai">>, <<"2026-12-31">>, <<"2026-12-30T16:00:00Z">>,
            <<"2026-12-31T16:00:00Z">>},
        {<<"Asia/Kathmandu">>, <<"2026-01-31">>, <<"2026-01-30T18:15:00Z">>,
            <<"2026-01-31T18:15:00Z">>},
        {<<"UTC">>, <<"2024-02-29">>, <<"2024-02-29T00:00:00Z">>, <<"2024-03-01T00:00:00Z">>}
    ],
    lists:foreach(
        fun(SessionZone) ->
            ok = elib_pg:with_tx(fun(Conn) ->
                {ok, _} = elib_pg:query(
                    Conn,
                    <<"SELECT set_config('TimeZone', $1, true)">>,
                    [SessionZone]
                ),
                lists:foreach(fun(Case) -> assert_calendar_boundary(Conn, Case) end, Cases)
            end)
        end,
        [<<"UTC">>, <<"Asia/Shanghai">>]
    ).

assert_calendar_boundary(Conn, {Zone, Day, StartAt, EndAt}) ->
    {WhereSql, Params} = review_queue_repo:build_where(#{
        start => Day, 'end' => Day, zone => Zone
    }),
    Sql = <<
        "WITH bounds AS (SELECT $5::text::timestamptz AS start_at,"
        " $6::text::timestamptz AS end_at), samples AS ("
        " SELECT sample.* FROM bounds CROSS JOIN LATERAL (VALUES"
        " ('before', start_at - INTERVAL '1 microsecond'),"
        " ('start', start_at), ('last', end_at - INTERVAL '1 microsecond'),"
        " ('end', end_at)) AS sample(label, created_at))"
        " SELECT label FROM samples",
        WhereSql/binary,
        " ORDER BY created_at"
    >>,
    ?assertEqual(
        {ok, [#{<<"label">> => <<"start">>}, #{<<"label">> => <<"last">>}]},
        elib_pg:query(Conn, Sql, Params ++ [StartAt, EndAt])
    ).

handler_calendar_day_boundaries_pg_test_() ->
    Cases = [
        {<<"America/New_York">>, <<"2026-03-08">>, <<"2026-03-08T05:00:00Z">>,
            <<"2026-03-09T04:00:00Z">>},
        {<<"America/New_York">>, <<"2026-11-01">>, <<"2026-11-01T04:00:00Z">>,
            <<"2026-11-02T05:00:00Z">>},
        {<<"UTC">>, <<"2026-10-27">>, <<"2026-10-27T00:00:00Z">>, <<"2026-10-28T00:00:00Z">>}
    ],
    [
        {
            binary_to_list(<<Zone/binary, " ", Day/binary>>),
            {setup,
                fun() ->
                    {ok, Conn} = eunit_runner:eunit_setup_with_db(),
                    Conn
                end,
                fun eunit_runner:eunit_cleanup_db/1, fun(_Conn) ->
                    fun() -> assert_handler_calendar_boundary(Case) end
                end}
        }
     || {Zone, Day, _, _} = Case <- Cases
    ].

assert_handler_calendar_boundary({Zone, Day, StartAt, EndAt}) ->
    Marker = <<"tzv1:r12:handler:", Zone/binary, ":", Day/binary>>,
    {ok, _} = elib_pg:execute(<<"DELETE FROM public.review_queue WHERE content = $1">>, [Marker]),
    meck:new(adm_acl, [no_link]),
    try
        meck:expect(
            adm_acl,
            ensure_permission,
            fun(#{adm_user_id := 920001}, <<"reports:read">>, _Req) -> ok end
        ),
        seed_handler_calendar_samples(Marker, StartAt, EndAt),
        Query = cow_qs:qs([
            {<<"start">>, Day},
            {<<"end">>, Day},
            {<<"zone">>, Zone},
            {<<"keyword">>, Marker},
            {<<"status">>, <<"approved">>},
            {<<"page">>, <<"1">>},
            {<<"size">>, <<"10">>}
        ]),
        Payload = timezone_acceptance_pg_fixture:response_payload(
            adm_moderation_handler, Query, #{action => review_queue, adm_user_id => 920001}
        ),
        ?assertMatch(
            #{
                <<"total">> := 2,
                <<"page">> := 1,
                <<"size">> := 10,
                <<"total_pages">> := 1
            },
            Payload
        ),
        Rows = maps:get(<<"list">>, Payload),
        ?assertEqual(
            [<<Marker/binary, ":last">>, <<Marker/binary, ":start">>],
            [maps:get(<<"msg_id">>, Row) || Row <- Rows]
        ),
        lists:foreach(
            fun(Row) ->
                ?assertEqual([<<"synthetic">>], maps:get(<<"hit_words">>, Row)),
                ?assertEqual(false, maps:get(<<"overdue">>, Row))
            end,
            Rows
        ),
        ?assert(meck:validate(adm_acl))
    after
        meck:unload(adm_acl),
        {ok, _} = elib_pg:execute(<<"DELETE FROM public.review_queue WHERE content = $1">>, [Marker])
    end.

seed_handler_calendar_samples(Marker, StartAt, EndAt) ->
    {ok, 4} = elib_pg:execute(
        <<
            "WITH bounds AS (SELECT $1::text::timestamptz AS start_at,"
            " $2::text::timestamptz AS end_at)"
            " INSERT INTO public.review_queue (id, msg_id, msg_type, content, from_id,"
            " from_account, to_id, to_type, hit_words, review_status, reviewer_id, reason,"
            " reviewed_at, created_at, metadata)"
            " SELECT 942100000000000 + sample.ordinal, $3::text || ':' || sample.label,"
            " 'text', $3, 920001, 'timezone-fixture', 920002, 'c2c', NULL, 'approved',"
            " NULL, NULL, NULL, sample.created_at, '{\"hit_words\":[\"synthetic\"]}'::jsonb"
            " FROM bounds CROSS JOIN LATERAL (VALUES"
            " (1, 'before', start_at - INTERVAL '1 microsecond'),"
            " (2, 'start', start_at), (3, 'last', end_at - INTERVAL '1 microsecond'),"
            " (4, 'end', end_at)) AS sample(ordinal, label, created_at)"
        >>,
        [StartAt, EndAt, Marker]
    ),
    ok.
