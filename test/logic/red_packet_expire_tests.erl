-module(red_packet_expire_tests).

%%%===================================================================
%%% @doc B-10：过期红包未领完的余额退回发送者（ecron 入口行为）。
%%%
%%% 判据「24h 后未领完金额退回发送者」的代码侧：扫描 → 逐个退款 → 指标。
%%% 真实的 24h 时序与钱包余额落库需要真实 PG，不在单测范围。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(red_packet_repo, [no_link, passthrough]),
    meck:new(elib_metric, [passthrough, non_strict]),
    meck:expect(elib_metric, increment, fun(_N, _V) -> ok end),
    meck:expect(elib_metric, increment, fun(_N, _V, _L) -> ok end),
    meck:expect(red_packet_repo, expire_and_refund, fun(_Id) -> {ok, 300} end),
    ok.

cleanup(_) ->
    catch meck:unload(elib_metric),
    catch meck:unload(red_packet_repo),
    ok.

stub_expired(Rows) ->
    meck:expect(red_packet_repo, list_expired_active, fun(_Limit) -> Rows end).

outcomes() ->
    [
        maps:get(outcome, L)
     || {_P, {elib_metric, increment, [red_packet_expire_total, _V, L]}, _R} <-
            meck:history(elib_metric)
    ].

expire_refund_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun expired_packet_is_refunded/0,
        fun nothing_expired_is_a_noop/0,
        fun already_settled_is_skipped_not_failed/0,
        fun one_bad_row_does_not_stop_the_batch/0,
        fun batch_size_has_floor/0
    ]}.

%% 过期且有余额 → 调 expire_and_refund
expired_packet_is_refunded() ->
    stub_expired([#{<<"id">> => 111, <<"sender_uid">> => 9, <<"remain_amount">> => 300}]),
    ?assertEqual(ok, red_packet_logic:run_expire_refund(10)),
    ?assertEqual(1, meck:num_calls(red_packet_repo, expire_and_refund, [111])),
    ?assertEqual([refunded], outcomes()).

%% 没有过期红包时不得崩 —— length([])=0 曾会把 elib_metric:increment/2 打成
%% function_clause，整轮被 try 吃掉变成 error 路径（静默失效的典型）
nothing_expired_is_a_noop() ->
    stub_expired([]),
    ?assertEqual(ok, red_packet_logic:run_expire_refund(10)),
    ?assertEqual(0, meck:num_calls(red_packet_repo, expire_and_refund, '_')),
    %% 心跳仍要产出，否则分不清"没过期红包"和"job 死了"
    Names = [N || {_P, {elib_metric, increment, [N | _]}, _R} <- meck:history(elib_metric)],
    ?assert(lists:member(red_packet_expire_run_total, Names)),
    %% 且**不得**走进错误分支：0 条是正常情况，不是故障
    ?assertNot(lists:member(red_packet_expire_error_total, Names)).

%% 并发已结算 → 记 skipped 而非 failed（不是错误，不该污染失败告警）
already_settled_is_skipped_not_failed() ->
    stub_expired([#{<<"id">> => 222}]),
    meck:expect(red_packet_repo, expire_and_refund, fun(_) -> {rollback, already_settled} end),
    ?assertEqual(ok, red_packet_logic:run_expire_refund(10)),
    ?assertEqual([skipped], outcomes()).

%% 单行抛异常不得中断整批（一个坏行不能让所有人的钱都退不回来）
one_bad_row_does_not_stop_the_batch() ->
    stub_expired([#{<<"id">> => 1}, #{<<"id">> => 2}, #{<<"id">> => 3}]),
    meck:expect(red_packet_repo, expire_and_refund, fun
        (2) -> erlang:error(boom);
        (_) -> {ok, 100}
    end),
    ?assertEqual(ok, red_packet_logic:run_expire_refund(10)),
    ?assertEqual(3, meck:num_calls(red_packet_repo, expire_and_refund, '_')),
    ?assertEqual([refunded, failed, refunded], outcomes()).

%% 批量下限：配 0 也不能退化成 LIMIT 0（那样一个都扫不到，job 形同虚设）
batch_size_has_floor() ->
    stub_expired([]),
    ?assertEqual(ok, red_packet_logic:run_expire_refund(0)),
    [{_P, {red_packet_repo, list_expired_active, [Limit]}, _R} | _] =
        meck:history(red_packet_repo),
    ?assert(Limit >= 1).
