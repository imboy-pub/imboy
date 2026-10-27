-module(user_dnd_rule_zone_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% D02（IMBOY-TIMEZONE-DECISIONS-V1，2026-10-27 冻结）：账号级 IANA zone
%%% 免打扰判定——真实 PG（隔离库）验证，时区换算全部锚 PG tzdata：
%%%
%%%   R15-1 上海/纽约同 instant 一静默一不静默（is_dnd_now_at + 规则行）；
%%%   R15-2 跨午夜窗口两端分钟边界（local_minute_of_day 精确到分钟）；
%%%   R15-3 DST：NY 2026-11-01 两次 01:30（05:30Z/06:30Z）都命中静默窗；
%%%              2026-03-08 缺失小时（06:59Z→119、07:00Z→180、07:30Z→210，
%%%              无任何 instant 映射到 150=02:30）不崩溃、不误判；
%%%   R15-4 旧 NULL zone 行按 dnd_fallback_zone 解释；与修复前
%%%         calendar:local_time 逐分钟等价（仅部署同款 TZ=+08 环境断言）；
%%%   R15-5 同账号两次 upsert（多设备语义）仍单行、末次写入胜；
%%%   R15-7 非法 zone（Asia/Foo）写入拒绝 {error, invalid_zone}
%%%         （未来 5a API 映射 422）；合法 zone 通过。
%%%
%%% 判定链全走真实 PG；不触任何真实推送渠道（传输层 mock 证据在
%%% push_notification_logic_tests，与本套件互补）。
%%%===================================================================

-define(UID_SH, 930001).
-define(UID_NY, 930002).
-define(UID_OLD, 930003).
-define(UID_UPS, 930004).

cleanup() ->
    _ = elib_pg:execute(
        <<"DELETE FROM public.user_dnd_rule WHERE user_id IN ($1, $2, $3, $4)">>,
        [?UID_SH, ?UID_NY, ?UID_OLD, ?UID_UPS]
    ),
    %% depcache 1h：清缓存避免跨用例串读
    lists:foreach(
        fun(Uid) -> imboy_cache:invalidate({user_dnd_rule, Uid}) end,
        [?UID_SH, ?UID_NY, ?UID_OLD, ?UID_UPS]
    ),
    ok.

rule(Uid, Zone) ->
    Base = #{
        <<"user_id">> => Uid,
        <<"start_min">> => 23 * 60,
        <<"end_min">> => 8 * 60,
        <<"status">> => 1
    },
    case Zone of
        undefined -> Base;
        _ -> Base#{<<"time_zone">> => Zone}
    end.

%% 合成瞬间（RFC3339Z，测试注入专用）
at(Y, Mo, D, H, Mi) ->
    list_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:00Z",
            [Y, Mo, D, H, Mi]
        )
    ).

with_fallback_zone(Zone, Fun) ->
    Prev = application:get_env(imboy, dnd_fallback_zone, undefined),
    case Zone of
        undefined -> application:unset_env(imboy, dnd_fallback_zone);
        _ -> application:set_env(imboy, dnd_fallback_zone, Zone)
    end,
    try
        Fun()
    after
        case Prev of
            undefined -> application:unset_env(imboy, dnd_fallback_zone);
            _ -> application:set_env(imboy, dnd_fallback_zone, Prev)
        end
    end.

%%%-------------------------------------------------------------------
%%% R15-1：上海/纽约同 instant 一静默一不静默
%%%-------------------------------------------------------------------
r15_1_same_instant_sh_muted_ny_not_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup(),
        ok = upsert(rule(?UID_SH, <<"Asia/Shanghai">>)),
        ok = upsert(rule(?UID_NY, <<"America/New_York">>)),
        %% 16:30Z：上海 00:30（窗口内）静默；纽约 12:30（窗口外）放行
        At = at(2026, 10, 27, 16, 30),
        ?assertEqual(true, user_dnd_rule_ds:is_dnd_now_at(?UID_SH, At)),
        ?assertEqual(false, user_dnd_rule_ds:is_dnd_now_at(?UID_NY, At))
    end).

upsert(Rule) ->
    user_dnd_rule_repo:upsert(Rule).

%%%-------------------------------------------------------------------
%%% R15-2：跨午夜窗口两端分钟边界（PG 换算精确到分钟）
%%%-------------------------------------------------------------------
r15_2_minute_boundaries_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        %% 上海 +08：边界分钟逐点核对（22:59 不在、23:00 在、07:59 在、08:00 不在）
        Cases = [
            {at(2026, 10, 27, 14, 59), 22 * 60 + 59},
            {at(2026, 10, 27, 15, 00), 23 * 60},
            {at(2026, 10, 27, 23, 59), 7 * 60 + 59},
            {at(2026, 10, 28, 00, 00), 8 * 60}
        ],
        lists:foreach(
            fun({At, ExpectMin}) ->
                {ok, Min} = user_dnd_rule_ds:local_minute_of_day(<<"Asia/Shanghai">>, At),
                ?assertEqual(ExpectMin, Min)
            end,
            Cases
        )
    end).

%%%-------------------------------------------------------------------
%%% R15-3：DST 重复小时两次都静默；缺失小时无命中不崩溃
%%%-------------------------------------------------------------------
r15_3_dst_fall_back_repeat_and_gap_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup(),
        %% NY 静默窗 01:00-02:00（60..120）
        ok = user_dnd_rule_repo:upsert(#{
            <<"user_id">> => ?UID_NY,
            <<"start_min">> => 60,
            <<"end_min">> => 120,
            <<"status">> => 1,
            <<"time_zone">> => <<"America/New_York">>
        }),
        imboy_cache:invalidate({user_dnd_rule, ?UID_NY}),
        %% 2026-11-01 秋季回拨：05:30Z 与 06:30Z 都是当地 01:30 → 都静默
        ?assertEqual(true, user_dnd_rule_ds:is_dnd_now_at(?UID_NY, at(2026, 11, 1, 5, 30))),
        ?assertEqual(true, user_dnd_rule_ds:is_dnd_now_at(?UID_NY, at(2026, 11, 1, 6, 30))),
        %% 两 instant 的当地分钟同为 90（01:30）
        {ok, M1} = user_dnd_rule_ds:local_minute_of_day(
            <<"America/New_York">>, at(2026, 11, 1, 5, 30)
        ),
        {ok, M2} = user_dnd_rule_ds:local_minute_of_day(
            <<"America/New_York">>, at(2026, 11, 1, 6, 30)
        ),
        ?assertEqual({90, 90}, {M1, M2}),

        %% 2026-03-08 春季跳变：06:59Z=01:59、07:00Z=03:00、07:30Z=03:30；
        %% 02:30（150）不存在——缺失小时无 instant 命中，判定不崩溃
        {ok, G1} = user_dnd_rule_ds:local_minute_of_day(
            <<"America/New_York">>, at(2026, 3, 8, 6, 59)
        ),
        {ok, G2} = user_dnd_rule_ds:local_minute_of_day(
            <<"America/New_York">>, at(2026, 3, 8, 7, 0)
        ),
        {ok, G3} = user_dnd_rule_ds:local_minute_of_day(
            <<"America/New_York">>, at(2026, 3, 8, 7, 30)
        ),
        ?assertEqual({119, 180, 210}, {G1, G2, G3}),
        %% 缺失小时内不存在 instant → 01:00-02:00 窗在该日 06:59Z 后即刻失效
        ?assertEqual(true, user_dnd_rule_ds:is_dnd_now_at(?UID_NY, at(2026, 3, 8, 6, 59))),
        ?assertEqual(false, user_dnd_rule_ds:is_dnd_now_at(?UID_NY, at(2026, 3, 8, 7, 0)))
    end).

%%%-------------------------------------------------------------------
%%% R15-4：旧 NULL zone 行按部署 fallback 解释 + 与旧行为逐分钟等价
%%%-------------------------------------------------------------------
r15_4_old_null_row_uses_fallback_and_equivalent_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup(),
        %% 旧行：无 time_zone 键（模拟迁移前数据，列值为 NULL）
        ok = user_dnd_rule_repo:upsert(#{
            <<"user_id">> => ?UID_OLD,
            <<"start_min">> => 23 * 60,
            <<"end_min">> => 8 * 60,
            <<"status">> => 1
        }),
        imboy_cache:invalidate({user_dnd_rule, ?UID_OLD}),
        %% fallback=上海：16:30Z（当地 00:30）静默
        with_fallback_zone(<<"Asia/Shanghai">>, fun() ->
            ?assertEqual(true, user_dnd_rule_ds:is_dnd_now_at(?UID_OLD, at(2026, 10, 27, 16, 30)))
        end),
        %% fallback=纽约：同 instant（当地 12:30）放行——fallback 语义真实生效
        with_fallback_zone(<<"America/New_York">>, fun() ->
            ?assertEqual(false, user_dnd_rule_ds:is_dnd_now_at(?UID_OLD, at(2026, 10, 27, 16, 30)))
        end),

        %% 逐分钟等价（R15-4 核心）：与修复前 calendar:local_time() 语义对照。
        %% 修复前分钟 = calendar:universal_time_to_local_time 换算（依赖宿主 TZ）；
        %% 仅当宿主本地偏移 = +08:00（与用户确认的部署 zone 一致）时才可比，
        %% 其他 TZ 运行环境跳过等价断言（不伪造证据）。
        LocalOffSecs = local_utc_offset_seconds(),
        case LocalOffSecs of
            8 * 3600 ->
                lists:foreach(
                    fun(H) ->
                        lists:foreach(
                            fun(Mi) ->
                                AtSecs =
                                    calendar:datetime_to_gregorian_seconds(
                                        {{2026, 10, 27}, {H, Mi, 0}}
                                    ) -
                                        calendar:datetime_to_gregorian_seconds(
                                            {{1970, 1, 1}, {0, 0, 0}}
                                        ),
                                AtMs = AtSecs * 1000,
                                {ok, PgMin} = user_dnd_rule_ds:local_minute_of_day(
                                    <<"Asia/Shanghai">>, AtMs
                                ),
                                {_Date, {EH, EM, _}} = calendar:universal_time_to_local_time(
                                    {{2026, 10, 27}, {H, Mi, 0}}
                                ),
                                ?assertEqual(EH * 60 + EM, PgMin)
                            end,
                            [0, 15, 30, 45]
                        )
                    end,
                    lists:seq(0, 23)
                );
            _ ->
                io:format(
                    "R15-4 equivalence skipped: host TZ offset ~ps != deployment +08:00~n",
                    [LocalOffSecs]
                )
        end
    end).

%% 宿主本地与 UTC 的当前偏移（秒；正=东区）。上海运行环境=28800。
local_utc_offset_seconds() ->
    U = calendar:universal_time(),
    L = calendar:local_time(),
    {Days, {H, M, S}} = calendar:time_difference(U, L),
    Days * 86400 + H * 3600 + M * 60 + S.

%%%-------------------------------------------------------------------
%%% R15-5：同账号多次 upsert（多设备先后设置）单行、末次写入胜
%%%-------------------------------------------------------------------
r15_5_upsert_single_row_last_write_wins_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup(),
        ok = user_dnd_rule_ds:save(rule(?UID_UPS, <<"Asia/Shanghai">>)),
        %% 第二台设备改窗口与 zone：仍单行，值=末次
        ok = user_dnd_rule_ds:save(#{
            <<"user_id">> => ?UID_UPS,
            <<"start_min">> => 60,
            <<"end_min">> => 120,
            <<"status">> => 1,
            <<"time_zone">> => <<"America/New_York">>
        }),
        {ok, [#{<<"count">> := Count}]} = elib_pg:query(
            <<"SELECT count(*) AS count FROM public.user_dnd_rule WHERE user_id = $1">>,
            [?UID_UPS]
        ),
        ?assertEqual(1, Count),
        Row = user_dnd_rule_ds:find_by_uid(?UID_UPS),
        ?assertEqual(60, maps:get(<<"start_min">>, Row)),
        ?assertEqual(<<"America/New_York">>, maps:get(<<"time_zone">>, Row))
    end).

%%%-------------------------------------------------------------------
%%% R15-7：非法 zone 拒写；合法 zone 通过
%%%-------------------------------------------------------------------
r15_7_invalid_zone_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup(),
        ?assertEqual(
            {error, invalid_zone},
            user_dnd_rule_repo:upsert(rule(930005, <<"Asia/Foo">>))
        ),
        ?assertEqual(
            {error, invalid_zone},
            user_dnd_rule_ds:save(rule(930005, <<"Asia/Foo">>))
        ),
        %% 拒写不落行
        {ok, [#{<<"count">> := 0}]} = elib_pg:query(
            <<"SELECT count(*) AS count FROM public.user_dnd_rule WHERE user_id = $1">>,
            [930005]
        ),
        %% 非 binary 类型同样拒绝
        ?assertEqual(
            {error, invalid_zone},
            user_dnd_rule_repo:validate_zone(123)
        ),
        %% 合法 IANA 名通过（含非整小时偏移 zone）
        ?assertEqual(ok, user_dnd_rule_repo:validate_zone(<<"Asia/Shanghai">>)),
        ?assertEqual(ok, user_dnd_rule_repo:validate_zone(<<"Asia/Kathmandu">>)),
        ?assertEqual(ok, user_dnd_rule_repo:validate_zone(undefined)),
        _ = elib_pg:execute(
            <<"DELETE FROM public.user_dnd_rule WHERE user_id = $1">>,
            [930005]
        ),
        ok
    end).
