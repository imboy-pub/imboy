-module(channel_order_expires_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% R11（IMBOY-TIMEZONE-REPAIR-V1）：订单读时派生「已过期」状态适配真实
%%% PG codec。
%%%
%%% 现象（表述纪律：只支持「状态派生错误」，不表述为已证明的过期付款漏洞）：
%%%   epgsql_codec_rfc3339_bin（sys.tzv1 / sys.runtime codecs 注册）把
%%%   timestamptz 解码为 RFC3339 binary；channel_logic_order:expires_at_ms/1
%%%   原实现只识别 epgsql 默认 {{Y,M,D},{H,Mi,S}} tuple 形状，binary 落兜
%%%   底 0 =「未过期」→ maybe_mark_expired 永不触发 → 过期 pending 订单在
%%%   列表/详情显示为永久待支付（僵尸状态展示）。付款合同不受影响：
%%%   channel_order_repo:pay/2 的 SQL `expires_at > NOW()` 守卫在 PG 端判
%%%   过期，迟到回调宽限（B-08）与已付款/取消状态均不因读时派生改变。
%%%
%%% 覆盖：
%%%   R11-1 过期/未过期 pending 派生，且列表 map 路径与详情单条路径一致
%%%   R11-2 paid/cancelled 不被误改
%%%   R11-3 真实 codec 证据（真库读出 RFC3339 binary）而非仅 tuple mock
%%%   R11-4 付款守卫回归由 channel_logic_order_pay_tests 既有用例承担，
%%%         本套件另证：读时派生只改展示 map，不写库
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip（PG-absent 运行不得计为通过）。
%%%===================================================================

-define(CLEAN_NO, <<"CHORD-r11-%">>).
-define(TEST_CHANNEL_ID, 910001).
-define(PAST, past).
-define(FUTURE, future).

cleanup_test_rows() ->
    _ = elib_pg:execute(
        <<"DELETE FROM public.channel_order WHERE order_no LIKE $1">>,
        [?CLEAN_NO]
    ),
    %% channel_order.channel_id 有 FK（fk_channel_order_channel，ON DELETE CASCADE），
    %% 测试先插最小 channel 行承接；清理顺序 order → channel。
    _ = elib_pg:execute(<<"DELETE FROM public.channel WHERE id = $1">>, [?TEST_CHANNEL_ID]),
    ok.

%% 直接 SQL 插行；expires_at 在 PG 端用 NOW() ± interval 生成，避免客户端
%% 时钟对齐问题。channel 行满足 fk_channel_order_channel。
insert_order(OrderNo, Status, When) ->
    _ = elib_pg:execute(
        <<"INSERT INTO public.channel (id, name, creator_uid) VALUES ($1, $2, $3) ",
            "ON CONFLICT (id) DO NOTHING">>,
        [?TEST_CHANNEL_ID, <<"tzv1-r11-channel">>, 910002]
    ),
    Offset =
        case When of
            ?PAST -> <<"- interval '1 hour'">>;
            ?FUTURE -> <<"+ interval '1 hour'">>
        end,
    Id = tsid_int(OrderNo),
    RowPrefix = io_lib:format(
        "(~B, ~B, 910002, '~s', 9.90, 'CNY', ~B, 'alipay', ",
        [Id, ?TEST_CHANNEL_ID, binary_to_list(OrderNo), Status]
    ),
    Sql = iolist_to_binary(
        [
            <<"INSERT INTO public.channel_order ",
                "(id, channel_id, user_id, order_no, amount, currency, status, ",
                " payment_method, expires_at) VALUES ">>
        ] ++
            [RowPrefix, <<"NOW() ", Offset/binary, ")">>]
    ),
    {ok, _} = elib_pg:execute(Sql, []),
    ok.

tsid_int(OrderNo) ->
    %% 顺序生成稳定整数主键（同套件内唯一即可）
    erlang:phash2(OrderNo, 1_000_000_000) + 7_000_000_000.

fetch_order(OrderNo) ->
    case
        elib_pg:query(
            <<"SELECT id, order_no, status, expires_at FROM public.channel_order ",
                "WHERE order_no = $1">>,
            [OrderNo]
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%%%-------------------------------------------------------------------
%%% R11-3：真实 codec 证据——timestamptz 列经 epgsql_codec_rfc3339_bin
%%% 解码为 RFC3339 binary（修复前后均成立；tuple 形状反而是 mock 现象）。
%%%-------------------------------------------------------------------
codec_decodes_timestamptz_to_rfc3339_binary_test_() ->
    ?TEST_WITH_DB(fun() ->
        insert_order(<<"CHORD-r11-codec">>, 0, ?FUTURE),
        {ok, #{<<"expires_at">> := ExpiresAt}} = fetch_order(<<"CHORD-r11-codec">>),
        ?assert(is_binary(ExpiresAt)),
        ?assertMatch(<<"20", _/binary>>, ExpiresAt),
        ?assert(is_integer(elib_dt:rfc3339_to(ExpiresAt))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R11-1/R11-3：真库行——过期 pending 派生 4，未过期 pending 保持 0。
%%% （修复前：binary 落兜底 0 → 两条都保持 0，第一条断言失败 = 复现）
%%%-------------------------------------------------------------------
expired_pending_pg_row_derives_status4_test_() ->
    ?TEST_WITH_DB_TIMEOUT(15, fun() ->
        insert_order(<<"CHORD-r11-expired">>, 0, ?PAST),
        {ok, Row} = fetch_order(<<"CHORD-r11-expired">>),
        ?assert(is_binary(maps:get(<<"expires_at">>, Row))),
        ?assertEqual(4, maps:get(<<"status">>, channel_logic_order:order_transfer(Row))),
        cleanup_test_rows()
    end).

unexpired_pending_pg_row_stays_pending_test_() ->
    ?TEST_WITH_DB_TIMEOUT(15, fun() ->
        insert_order(<<"CHORD-r11-live">>, 0, ?FUTURE),
        {ok, Row} = fetch_order(<<"CHORD-r11-live">>),
        ?assertEqual(0, maps:get(<<"status">>, channel_logic_order:order_transfer(Row))),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R11-2：已付款(1)/已取消(3) 即便 expires_at 已过也不被读时派生误改。
%%%-------------------------------------------------------------------
paid_and_cancelled_not_touched_test_() ->
    ?TEST_WITH_DB_TIMEOUT(15, fun() ->
        insert_order(<<"CHORD-r11-paid">>, 1, ?PAST),
        insert_order(<<"CHORD-r11-cancelled">>, 3, ?PAST),
        {ok, PaidRow} = fetch_order(<<"CHORD-r11-paid">>),
        {ok, CancelledRow} = fetch_order(<<"CHORD-r11-cancelled">>),
        ?assertEqual(
            1, maps:get(<<"status">>, channel_logic_order:order_transfer(PaidRow))
        ),
        ?assertEqual(
            3, maps:get(<<"status">>, channel_logic_order:order_transfer(CancelledRow))
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R11-1：列表 map 路径（lists:map(fun order_transfer/1, Orders)）与详情
%%% 单条路径对同一行派生结果一致（get_my_orders :447 vs get_order :470）。
%%%-------------------------------------------------------------------
list_and_detail_paths_derive_identically_test_() ->
    ?TEST_WITH_DB_TIMEOUT(15, fun() ->
        insert_order(<<"CHORD-r11-both">>, 0, ?PAST),
        {ok, Row} = fetch_order(<<"CHORD-r11-both">>),
        DetailPath = channel_logic_order:order_transfer(Row),
        [ListPath] = lists:map(fun channel_logic_order:order_transfer/1, [Row]),
        ?assertEqual(maps:get(<<"status">>, DetailPath), maps:get(<<"status">>, ListPath)),
        ?assertEqual(4, maps:get(<<"status">>, ListPath)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% R11-4：读时派生只改返回 map，不写库（迟到回调仍能按 B-08 补单——
%%% 库内 status 仍为 0，派生后的「已过期」不落库）。
%%%-------------------------------------------------------------------
derivation_is_display_only_test_() ->
    ?TEST_WITH_DB_TIMEOUT(15, fun() ->
        insert_order(<<"CHORD-r11-display">>, 0, ?PAST),
        {ok, Row} = fetch_order(<<"CHORD-r11-display">>),
        _ = channel_logic_order:order_transfer(Row),
        {ok, [#{<<"status">> := DbStatus}]} = elib_pg:query(
            <<"SELECT status FROM public.channel_order WHERE order_no = $1">>,
            [<<"CHORD-r11-display">>]
        ),
        ?assertEqual(0, DbStatus),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% 纯单测：binary 边界 / 非法输入安全 / tuple 兼容回归。
%%%-------------------------------------------------------------------
binary_boundary_exact_is_expired_test() ->
    %% maybe_mark_expired 用 millisecond() >= Ms：正好到期 = 已过期
    Now = 1_800_000_000_000,
    ExpiresRfc = elib_dt:to_rfc3339(Now, millisecond, "Z"),
    Order = #{<<"status">> => 0, <<"expires_at">> => ExpiresRfc},
    meck:new(elib_dt, [non_strict, passthrough]),
    meck:expect(elib_dt, millisecond, 0, fun() -> Now end),
    try
        ?assertEqual(4, maps:get(<<"status">>, channel_logic_order:order_transfer(Order)))
    after
        meck:unload(elib_dt)
    end.

binary_boundary_one_ms_before_stays_pending_test() ->
    Now = 1_800_000_000_000,
    ExpiresRfc = elib_dt:to_rfc3339(Now + 1, millisecond, "Z"),
    Order = #{<<"status">> => 0, <<"expires_at">> => ExpiresRfc},
    meck:new(elib_dt, [non_strict, passthrough]),
    meck:expect(elib_dt, millisecond, 0, fun() -> Now end),
    try
        ?assertEqual(0, maps:get(<<"status">>, channel_logic_order:order_transfer(Order)))
    after
        meck:unload(elib_dt)
    end.

invalid_binary_expires_at_is_not_expired_test() ->
    %% 非法 binary 解析失败 → 安全返回 0 → 按「未过期」处理，不误杀
    Order = #{<<"status">> => 0, <<"expires_at">> => <<"not-a-timestamp">>},
    ?assertEqual(Order, channel_logic_order:order_transfer(Order)).

empty_and_missing_expires_at_test() ->
    ?assertEqual(
        #{<<"status">> => 0, <<"expires_at">> => null},
        channel_logic_order:order_transfer(#{<<"status">> => 0, <<"expires_at">> => null})
    ),
    ?assertEqual(
        #{<<"status">> => 0}, channel_logic_order:order_transfer(#{<<"status">> => 0})
    ).

legacy_tuple_shape_still_works_test() ->
    %% epgsql 默认 codec 的 tuple 形状兼容回归（未注册 RFC3339 codec 的环境）
    Tuple = {{2026, 1, 1}, {0, 0, 0}},
    Order = #{<<"status">> => 0, <<"expires_at">> => Tuple},
    meck:new(elib_dt, [non_strict, passthrough]),
    meck:expect(elib_dt, millisecond, 0, fun() -> 1_800_000_000_000 end),
    try
        ?assertEqual(4, maps:get(<<"status">>, channel_logic_order:order_transfer(Order)))
    after
        meck:unload(elib_dt)
    end.

non_pending_status_short_circuits_test() ->
    %% order_transfer 只对 status=0 派生；非 pending 行原样返回
    Order = #{<<"status">> => 1, <<"expires_at">> => <<"2020-01-01T00:00:00Z">>},
    ?assertEqual(Order, channel_logic_order:order_transfer(Order)).
