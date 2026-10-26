-module(review_queue_repo_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc review_queue_repo R12-BE 时间边界构造单测（纯 SQL 构造断言，不依赖 PG）。
%%% 覆盖：
%%%   - 纯日期 [start, endExclusive) 半开区间 + AT TIME ZONE 参数化 zone
%%%   - zone 缺省 UTC（与旧 session TimeZone=UTC 语义一致）
%%%   - rfc3339 时刻参数（start_at/end_at）直接瞬间比较
%%%   - 优先级：start_at/end_at > start/end
%%%   - 不引入 DATE(created_at) 包装（保持索引可用）
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
            binary:match(Sql, <<"created_at >= (($1)::date)::timestamp AT TIME ZONE $2">>)
        ),
        %% 结束：次日 00:00（+1 day），半开 <
        ?_assertNotEqual(
            nomatch,
            binary:match(
                Sql, <<"created_at < ((($3)::date)::timestamp AT TIME ZONE $4) + INTERVAL '1 day'">>
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
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"created_at >= $1">>)),
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"created_at < $2">>)),
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
        ?_assertNotEqual(nomatch, binary:match(Sql, <<"($1)::date">>))
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
