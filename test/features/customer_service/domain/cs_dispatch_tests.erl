%%% @doc 客服派单（dispatch）套件（纯 domain，零 I/O、零 mock）。
%%%
%%% 覆盖 CS-01-A02 与附带能力：
%%%   * least-active 选坐席：在可用坐席中选 active 会话数最少者，平局取 identity 最小；
%%%   * max_concurrent 上限：active_count 达到上限的坐席不再可选；
%%%   * suspend seat：enabled=false 的坐席立即不可派单（新 claim 即时拒绝）；
%%%   * 全部不可用 → 可区分的 no_seat_available。
%%%
%%% 坐席快照（含 active_count）由调用方显式传入——domain 不读库、不计时。
-module(cs_dispatch_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORG, 700000000000001).
-define(SEAT_A, 700000000000011).
-define(SEAT_B, 700000000000012).
-define(SEAT_C, 700000000000013).

%% ===================================================================
%% least-active
%% ===================================================================

selects_least_active_seat_test() ->
    Seats = [
        seat(?SEAT_A, 2, 5, 3),
        seat(?SEAT_B, 1, 5, 4),
        seat(?SEAT_C, 3, 5, 5)
    ],
    {ok, Picked} = cs_dispatch:select_seat(?ORG, Seats),
    ?assertEqual(?SEAT_B, Picked).

tie_breaks_by_smallest_identity_test() ->
    Seats = [
        seat(?SEAT_C, 2, 5, 5),
        seat(?SEAT_A, 2, 5, 5),
        seat(?SEAT_B, 2, 5, 5)
    ],
    {ok, Picked} = cs_dispatch:select_seat(?ORG, Seats),
    ?assertEqual(?SEAT_A, Picked).

single_seat_org_works_test() ->
    Seats = [seat(?SEAT_A, 0, 3, 7)],
    {ok, ?SEAT_A} = cs_dispatch:select_seat(?ORG, Seats).

%% ===================================================================
%% max_concurrent 上限（A02）
%% ===================================================================

seat_at_capacity_is_not_selectable_test() ->
    Seats = [
        %% active_count == max_concurrent：满
        seat(?SEAT_A, 5, 5, 9),
        seat(?SEAT_B, 4, 5, 9)
    ],
    {ok, Picked} = cs_dispatch:select_seat(?ORG, Seats),
    ?assertEqual(?SEAT_B, Picked).

all_seats_at_capacity_rejected_test() ->
    Seats = [seat(?SEAT_A, 5, 5, 9), seat(?SEAT_B, 2, 2, 9)],
    ?assertMatch({error, no_seat_available}, cs_dispatch:select_seat(?ORG, Seats)).

claim_allowed_enforces_capacity_test() ->
    ok = cs_dispatch:claim_allowed(seat(?SEAT_A, 4, 5, 7), 4),
    ?assertMatch(
        {error, seat_at_capacity},
        cs_dispatch:claim_allowed(seat(?SEAT_A, 5, 5, 7), 5)
    ),
    ?assertMatch(
        {error, seat_at_capacity},
        cs_dispatch:claim_allowed(seat(?SEAT_A, 6, 5, 7), 6)
    ).

capacity_left_is_never_negative_test() ->
    ?assertEqual(1, cs_dispatch:capacity_left(seat(?SEAT_A, 4, 5, 7))),
    ?assertEqual(0, cs_dispatch:capacity_left(seat(?SEAT_A, 5, 5, 7))),
    %% 超限（例如配置下调后）按 0 处理，不产生负容量
    ?assertEqual(0, cs_dispatch:capacity_left(seat(?SEAT_A, 7, 5, 7))).

%% ===================================================================
%% suspend seat（enabled=false 即时拒绝）
%% ===================================================================

suspended_seat_is_never_selectable_test() ->
    Seats = [
        (seat(?SEAT_A, 0, 5, 9))#{enabled => false},
        seat(?SEAT_B, 3, 5, 9)
    ],
    {ok, Picked} = cs_dispatch:select_seat(?ORG, Seats),
    ?assertEqual(?SEAT_B, Picked).

all_suspended_rejected_test() ->
    Seats = [
        (seat(?SEAT_A, 0, 5, 9))#{enabled => false},
        (seat(?SEAT_B, 0, 5, 9))#{enabled => false}
    ],
    ?assertMatch({error, no_seat_available}, cs_dispatch:select_seat(?ORG, Seats)).

claim_allowed_rejects_suspended_seat_test() ->
    ?assertMatch(
        {error, seat_disabled},
        cs_dispatch:claim_allowed((seat(?SEAT_A, 0, 5, 7))#{enabled => false}, 0)
    ).

suspended_seat_wins_even_if_least_active_is_checked_first_test() ->
    %% 负向对照：least-active 若不先过滤 disabled，会选中 0 负载的停用坐席
    Seats = [
        (seat(?SEAT_A, 0, 5, 9))#{enabled => false},
        seat(?SEAT_B, 4, 5, 9)
    ],
    {ok, Picked} = cs_dispatch:select_seat(?ORG, Seats),
    ?assertEqual(?SEAT_B, Picked).

%% ===================================================================
%% 跨 Org 防混淆
%% ===================================================================

seat_of_other_org_is_ignored_test() ->
    Seats = [(seat(?SEAT_A, 0, 5, 9))#{organization_id => 999}],
    ?assertMatch({error, no_seat_available}, cs_dispatch:select_seat(?ORG, Seats)).

%% ===================================================================
%% 构造辅助
%% ===================================================================

seat(IdentityId, ActiveCount, MaxConcurrent, Version) ->
    #{
        organization_id => ?ORG,
        business_identity_id => IdentityId,
        enabled => true,
        max_concurrent => MaxConcurrent,
        active_count => ActiveCount,
        version => Version
    }.
