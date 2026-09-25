%%% @doc CS-BE-07（按需客服统计）application 套件：零 DB、fake store。
%%%
%%% 指标公式（CS-GOV-03A 冻结口径，全部可手算复核）——窗口
%%% W = [window_start, window_end)（epoch 秒，UTC instant；由 date + tz_offset
%%% 显式换算，不依赖 DB 时区）：
%%%
%%%   * new_sessions       = |{s : s.queued_at ∈ W}|（开会话轴）
%%%   * first_response     = {count, avg_seconds} over {s : s.queued_at ∈ W,
%%%                           s.claimed_at ≠ NULL}；avg_seconds = 平均
%%%                           (claimed_at − queued_at) 秒；分母 = count。
%%%                           **未 claim（仍排队）不进分母**；无样本 count=0 且
%%%                           avg_seconds = null。
%%%   * closed_sessions    = |{s : s.closed_at ∈ W}|（关闭轴独立于开会话轴：
%%%                           昨天开的今天关也计入今日关闭量）
%%%   * rating             = {count, avg} over {s : s.rating_at ∈ W}；
%%%                           **未评分不进分母**；无样本 count=0 且 avg = null
%%%   * current.queued/active = 当前时刻 status 计数（不看窗口）
%%%
%%% 空数据/跨日/未关闭/未评分语义均以小 fixture 手算值逐项断言；跨 Org 隔离
%%% （A org 查不到 B org 行）与出站白名单（无 close_reason / visit_token_id /
%%% 任何密文）一并覆盖。
-module(cs_stats_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FAKE, cs_fake_store).
-define(ORG, 831000000000001).
-define(WS, 831000000000002).
-define(WS2, 831000000000003).
-define(ORG_B, 831000000000009).
-define(CONTACT, 831000000000004).
-define(CONV, 831000000000005).

%% 出站白名单（逐字；红线键绝不出现在任何层）。
-define(STATS_KEYS, [
    organization_id,
    workspace_id,
    date,
    tz_offset,
    window_start,
    window_end,
    new_sessions,
    first_response,
    closed_sessions,
    rating,
    current
]).

%% 2023-11-14 00:00 UTC = 1699920000；窗口 [1699920000, 1700006400)。
-define(DAY, <<"2023-11-14">>).
-define(START, 1699920000).
-define(END, 1700006400).

%% application 用例统一注入 fake store；`at` 显式注入（时钟不可自报，
%% 缺省日期推导用）。
p(Opts) ->
    maps:merge(
        #{store => ?FAKE, workspace_id => ?WS, at => 1700000000},
        Opts
    ).

%% foreach：每个用例独立 init/destroy ETS——前序用例的夹具行绝不渗入
%%（空数据用例尤其不能看到别的用例种的行）。
cs_stats_test_() ->
    {foreach, fun setup/0, fun cleanup/1, cases()}.

setup() ->
    ok = ?FAKE:init(),
    ok.

cleanup(_S) ->
    ok = ?FAKE:destroy().

cases() ->
    [
        fun metrics_fixture_hand_computed/0,
        fun empty_data_semantics/0,
        fun day_boundary_membership/0,
        fun unclaimed_not_in_first_response_denominator/0,
        fun unclosed_and_unrated_denominators/0,
        fun cross_org_isolation/0,
        fun default_date_derives_from_server_clock/0,
        fun tz_offset_shifts_window/0,
        fun invalid_date_and_tz_offset_rejected/0,
        fun workspace_scope_narrows/0,
        fun outbound_projection_whitelist/0,
        fun wiring_registered_in_tables/0
    ].

%% ===================================================================
%% 夹具
%% ===================================================================

%% 手算夹具（Org A / WS；窗口 [1699920000, 1700006400)）：
%%
%%   S1  queued=1699920000(=start，含) claimed=1699920100 closed=1699920200
%%       rating=5 rating_at=1699920300
%%       → new +1；frt 样本 (1699920100-1699920000)=100s；closed +1；rated +1
%%   S2  queued=1699950000(窗中) claimed=1699950450 → active
%%       → new +1；frt 样本 450s；不进 closed（未关闭）
%%   S3  queued=1700006399(=end-1，含) → 仍 queued
%%       → new +1
%%   S4  queued=1700006400(=end，排外) → 仍 queued
%%       → 不进 new；current.queued +1
%%   S5  queued=1699919999(=start-1，排外) closed=1699930000(窗中) rating=NULL
%%       → 不进 new；closed +1；不进 rating（未评分）
%%   SB  Org B：三轴全命中——对 A 必须不可见
%%
%% Org A 手算期望：
%%   new_sessions=3 (S1,S2,S3)；first_response count=2 avg=(100+450)/2=275.0；
%%   closed_sessions=2 (S1,S5)；rating count=1 avg=5.0；
%%   current: queued=2 (S3,S4) active=1 (S2)。
seed_fixture() ->
    put_session(#{
        id => 831000000000101,
        status => closed,
        queued_at => 1699920000,
        claimed_at => 1699920100,
        closed_at => 1699920200,
        rating => 5,
        rating_at => 1699920300
    }),
    put_session(#{
        id => 831000000000102,
        status => active,
        queued_at => 1699950000,
        claimed_at => 1699950450,
        closed_at => undefined,
        rating => undefined,
        rating_at => undefined
    }),
    put_session(#{
        id => 831000000000103,
        status => queued,
        queued_at => 1700006399,
        claimed_at => undefined,
        closed_at => undefined,
        rating => undefined,
        rating_at => undefined
    }),
    put_session(#{
        id => 831000000000104,
        status => queued,
        queued_at => 1700006400,
        claimed_at => undefined,
        closed_at => undefined,
        rating => undefined,
        rating_at => undefined
    }),
    put_session(#{
        id => 831000000000105,
        status => closed,
        queued_at => 1699919999,
        claimed_at => 1699919990,
        closed_at => 1699930000,
        rating => undefined,
        rating_at => undefined
    }),
    ok.

seed_org_b_rows() ->
    put_session(#{
        org => ?ORG_B,
        id => 831000000000901,
        status => closed,
        queued_at => 1699950000,
        claimed_at => 1699950100,
        closed_at => 1699950200,
        rating => 1,
        rating_at => 1699950300
    }),
    ok.

put_session(Overrides) ->
    Row = maps:merge(
        #{
            org => ?ORG,
            ws => ?WS,
            contact_id => ?CONTACT,
            conversation_id => ?CONV + maps:get(id, Overrides),
            business_identity_id => undefined,
            visit_token_id => undefined,
            close_reason => <<"internal-reason-never-leak">>,
            version => 1,
            updated_at => 1700000001
        },
        Overrides
    ),
    ok = ?FAKE:put_session_for_list(
        Row#{
            organization_id => maps:get(org, Row),
            workspace_id => maps:get(ws, Row)
        }
    ).

stats(Org, Opts) ->
    cs_session_app:session_stats(Org, p(Opts)).

%% ===================================================================
%% 用例
%% ===================================================================

%% 指标公式 fixture 手算复核：每个指标等于独立手算值（见 seed_fixture/0 注释）。
metrics_fixture_hand_computed() ->
    seed_fixture(),
    seed_org_b_rows(),
    {ok, Stats} = stats(?ORG, #{date => ?DAY}),
    ?assertEqual(3, maps:get(new_sessions, Stats)),
    FRT = maps:get(first_response, Stats),
    ?assertEqual(2, maps:get(count, FRT)),
    assert_float_avg(275.0, maps:get(avg_seconds, FRT)),
    ?assertEqual(2, maps:get(closed_sessions, Stats)),
    Rating = maps:get(rating, Stats),
    ?assertEqual(1, maps:get(count, Rating)),
    assert_float_avg(5.0, maps:get(avg, Rating)),
    Current = maps:get(current, Stats),
    ?assertEqual(2, maps:get(queued, Current)),
    ?assertEqual(1, maps:get(active, Current)),
    %% 窗口回显：date + tz_offset（缺省 0）+ epoch 边界（不依赖 DB 时区）。
    ?assertEqual(?DAY, maps:get(date, Stats)),
    ?assertEqual(0, maps:get(tz_offset, Stats)),
    ?assertEqual(?START, maps:get(window_start, Stats)),
    ?assertEqual(?END, maps:get(window_end, Stats)).

%% 空数据：计数全 0、均值 null（分母为 0 时不吐 0 伪装均值）；current 同为 0。
empty_data_semantics() ->
    {ok, Stats} = stats(?ORG, #{date => ?DAY}),
    ?assertEqual(0, maps:get(new_sessions, Stats)),
    ?assertEqual(#{count => 0, avg_seconds => undefined}, maps:get(first_response, Stats)),
    ?assertEqual(0, maps:get(closed_sessions, Stats)),
    ?assertEqual(#{count => 0, avg => undefined}, maps:get(rating, Stats)),
    ?assertEqual(#{queued => 0, active => 0}, maps:get(current, Stats)).

%% 跨日边界四点：start 含、start-1 排外、end-1 含、end 排外。
day_boundary_membership() ->
    seed_fixture(),
    %% 全窗口（tz=0）：S1(start) 含、S5(start-1) 排外、S3(end-1) 含、S4(end) 排外。
    {ok, W0} = stats(?ORG, #{date => ?DAY}),
    ?assertEqual(3, maps:get(new_sessions, W0)),
    %% 前一日（2023-11-13 = [1699833600, 1699920000)）：S5(1699919999) 进窗，
    %% S1..S4 排外；关闭轴：S5 的 closed_at=1699930000 属 11-14 窗口——
    %% 11-13 的 closed=0（关闭归 closed_at 所在日，不归开会话日）。
    {ok, WPrev} = stats(?ORG, #{date => <<"2023-11-13">>}),
    ?assertEqual(1, maps:get(new_sessions, WPrev)),
    ?assertEqual(0, maps:get(closed_sessions, WPrev)),
    %% 后一日（2023-11-15）：S4(1700006400=start 含) 唯一进窗。
    {ok, WNext} = stats(?ORG, #{date => <<"2023-11-15">>}),
    ?assertEqual(1, maps:get(new_sessions, WNext)).

%% 未 claim（仍排队）不进 first_response 分母；分母 = 已 claim 的今日新会话数。
unclaimed_not_in_first_response_denominator() ->
    seed_fixture(),
    {ok, Stats} = stats(?ORG, #{date => ?DAY}),
    %% new=3（S1,S2,S3）但只有 S1,S2 已 claim → count=2，不是 3 也不是 0。
    ?assertEqual(2, maps:get(count, maps:get(first_response, Stats))),
    %% 只有未 claim 样本的日子（S3/S4 所在窗口的另一面不存在——用 11-15：
    %% 仅 S4 且未 claim）→ count=0 + avg=null。
    {ok, Next} = stats(?ORG, #{date => <<"2023-11-15">>}),
    ?assertEqual(#{count => 0, avg_seconds => undefined}, maps:get(first_response, Next)).

%% 未关闭语义（active 不进 closed_sessions）与未评分语义（closed 但
%% rating=NULL 不进 rating 分母）。
unclosed_and_unrated_denominators() ->
    seed_fixture(),
    {ok, Stats} = stats(?ORG, #{date => ?DAY}),
    %% closed_at ∈ W 的只有 S1、S5 → 2；S2（active 未关闭）不进。
    ?assertEqual(2, maps:get(closed_sessions, Stats)),
    %% rating_at ∈ W 的只有 S1 → count=1；S5（closed 未评分）不进分母。
    ?assertEqual(1, maps:get(count, maps:get(rating, Stats))).

%% 跨 Org 隔离：A org 查不到 B org 行（SB 三轴全命中也不可见）。
cross_org_isolation() ->
    seed_org_b_rows(),
    {ok, Stats} = stats(?ORG, #{date => ?DAY}),
    ?assertEqual(0, maps:get(new_sessions, Stats)),
    ?assertEqual(0, maps:get(closed_sessions, Stats)),
    ?assertEqual(#{count => 0, avg => undefined}, maps:get(rating, Stats)),
    %% B org 自身可见。
    {ok, BStats} = stats(?ORG_B, #{date => ?DAY}),
    ?assertEqual(1, maps:get(new_sessions, BStats)).

%% 缺省 date：由服务端时钟 at（epoch 秒）推导 UTC 当日（不隐式依赖 DB 时区）。
default_date_derives_from_server_clock() ->
    seed_fixture(),
    %% at = 1700006399 = 2023-11-14 23:59:59 UTC → 当日 2023-11-14。
    {ok, Stats} = stats(?ORG, #{at => 1700006399, date => undefined}),
    ?assertEqual(?DAY, maps:get(date, Stats)),
    ?assertEqual(?START, maps:get(window_start, Stats)),
    %% at = 1700006400 = 2023-11-15 00:00:00 UTC → 次日。
    {ok, Next} = stats(?ORG, #{at => 1700006400, date => undefined}),
    ?assertEqual(<<"2023-11-15">>, maps:get(date, Next)).

%% tz_offset 显式平移（ISO 8601 口径：本地时间 = UTC + tz_offset 分钟，东八
%% 区 = +480）：date 在该时区的 [00:00, 24:00) 折算为 UTC 窗口——2023-11-14
%% 的本地日起点 = UTC 前一日 16:00 = 1699891200，终点 1699977600；
%% 窗内 queued：S5(1699919999)、S1(1699920000)、S2(1699950000) → new=3
%%（S3=1700006399 已排外——东八区「11-14」比 UTC 早收窗）。
tz_offset_shifts_window() ->
    seed_fixture(),
    {ok, Stats} = stats(?ORG, #{date => ?DAY, tz_offset => 480}),
    ?assertEqual(480, maps:get(tz_offset, Stats)),
    ?assertEqual(1699891200, maps:get(window_start, Stats)),
    ?assertEqual(1699891200 + 86400, maps:get(window_end, Stats)),
    ?assertEqual(3, maps:get(new_sessions, Stats)).

%% 非法 date / 越界 tz_offset → 结构化 422 原子（cs_http 显式映射）。
invalid_date_and_tz_offset_rejected() ->
    ?assertMatch(
        {error, {invalid_date, <<"2023-13-01">>}},
        stats(?ORG, #{date => <<"2023-13-01">>})
    ),
    ?assertMatch(
        {error, {invalid_date, <<"20231114">>}},
        stats(?ORG, #{date => <<"20231114">>})
    ),
    ?assertMatch(
        {error, {invalid_date, <<"2023-02-30">>}},
        stats(?ORG, #{date => <<"2023-02-30">>})
    ),
    ?assertMatch(
        {error, {invalid_tz_offset, 841}},
        stats(?ORG, #{tz_offset => 841})
    ),
    ?assertMatch(
        {error, {invalid_tz_offset, -841}},
        stats(?ORG, #{tz_offset => -841})
    ),
    %% 合法边界（±840 = UTC±14）不拒。
    ?assertMatch({ok, _}, stats(?ORG, #{tz_offset => 840})),
    ?assertMatch({ok, _}, stats(?ORG, #{tz_offset => -840})),
    %% 非法 workspace 同门口径。
    ?assertMatch(
        {error, {invalid_workspace_id, <<>>}},
        stats(?ORG, #{workspace_id => <<>>})
    ).

%% workspace 收窄：显式 workspace 只统计该 workspace 的行。
workspace_scope_narrows() ->
    seed_fixture(),
    put_session(#{
        ws => ?WS2,
        id => 831000000000106,
        status => queued,
        queued_at => 1699960000,
        claimed_at => undefined,
        closed_at => undefined,
        rating => undefined,
        rating_at => undefined
    }),
    {ok, WsScoped} = stats(?ORG, #{date => ?DAY, workspace_id => ?WS2}),
    ?assertEqual(?WS2, maps:get(workspace_id, WsScoped)),
    ?assertEqual(1, maps:get(new_sessions, WsScoped)).

%% 出站白名单逐字：只有冻结键；close_reason / visit_token_id / 任何密文材料
%% 绝不出站。
outbound_projection_whitelist() ->
    seed_fixture(),
    {ok, Stats} = stats(?ORG, #{date => ?DAY}),
    ?assertEqual(lists:sort(?STATS_KEYS), lists:sort(maps:keys(Stats))),
    RedLines = [
        close_reason,
        visit_token_id,
        body_cipher,
        key_ref,
        secret,
        key_version,
        aad_hash
    ],
    lists:foreach(fun(K) -> ?assertNot(is_map_key(K, Stats)) end, RedLines),
    %% 嵌套视图同样白名单。
    ?assertEqual(
        lists:sort([count, avg_seconds]),
        lists:sort(maps:keys(maps:get(first_response, Stats)))
    ),
    ?assertEqual(
        lists:sort([count, avg]),
        lists:sort(maps:keys(maps:get(rating, Stats)))
    ),
    ?assertEqual(
        lists:sort([queued, active]),
        lists:sort(maps:keys(maps:get(current, Stats)))
    ).

%% 接线核对：port 契约 / facade 调用点 / 动作表三处登记（cs_closure_tests
%% 的四方一致性另覆盖 pg 实现）。
wiring_registered_in_tables() ->
    StoreContract = maps:get(cs_store_port, cs_ports:contracts()),
    ?assert(lists:member({session_stats, 4}, StoreContract)),
    ?assert(lists:member(session_stats, cs_facade_call:actions())),
    {ok, Entry} = cs_actions:tenant(session_stats),
    ?assertEqual(path, cs_actions:org_source(Entry)),
    ?assertEqual(
        enterprise_owner_admin,
        maps:get(auth_context, maps:get(auth, Entry))
    ),
    [Case] = maps:get(cases, Entry),
    ?assertEqual(<<"GET">>, maps:get(method, Case)),
    ?assertEqual(session_stats, maps:get(facade, Case)).

%% 浮点均值断言（PG numeric → float8 / fake 的 float 除法同口径）。
assert_float_avg(Expected, Actual) ->
    case is_number(Actual) andalso abs(Expected - Actual) < 0.0001 of
        true -> ok;
        false -> erlang:error({assert_float_avg, Expected, Actual})
    end.
