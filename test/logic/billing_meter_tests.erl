-module(billing_meter_tests).

%%%===================================================================
%%% @doc billing_meter 软计量单测（金钱相邻）
%%%
%%% 覆盖：活跃订阅解析 + metric 透传裸累加；无订阅 no-op（不 incr）；
%%%       incr 失败不抛错。全程 meck mock imboy_cache / billing_subscription_ds /
%%%       billing_usage_ds，测同步核心 do_meter/2（绕过 async spawn）。
%%%
%%% ⚠️ 红线断言：billing_meter 只调 billing_usage_ds:incr（裸累加），
%%%   绝不经 billing_logic:report_usage（超配额拒发＝事故）。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    %% 强制缓存 miss，走 DB 解析路径；set 直通
    ok = meck:new(imboy_cache, [passthrough]),
    ok = meck:expect(imboy_cache, get, fun(_Key) -> undefined end),
    ok = meck:expect(imboy_cache, set, fun(_K, _V, _T) -> ok end),
    ok = meck:new(billing_subscription_ds, [passthrough]),
    ok = meck:new(billing_usage_ds, [passthrough]),
    ok.

cleanup(_) ->
    catch meck:unload(billing_usage_ds),
    catch meck:unload(billing_subscription_ds),
    catch meck:unload(imboy_cache),
    ok.

%% 有活跃订阅：解析出 sub id 并把 metric/delta 透传给 billing_usage_ds:incr
active_sub_meters_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        meck:expect(billing_subscription_ds, find_active_by_tenant, fun(0) ->
            #{<<"id">> => 123}
        end),
        meck:expect(billing_usage_ds, incr, fun(_SubId, _Metric, _Period, _Delta) ->
            {ok, 1}
        end),
        ok = billing_meter:do_meter(<<"messages_sent">>, 1),
        [
            %% metric/delta/sub_id 透传正确（period 用当前 YYYY-MM，通配）
            ?_assertEqual(
                1, meck:num_calls(billing_usage_ds, incr, [123, <<"messages_sent">>, '_', 1])
            ),
            %% 红线：绝不走 report_usage（未 mock，调到就崩；这里断言只调了 incr 一次）
            ?_assertEqual(1, meck:num_calls(billing_usage_ds, incr, '_'))
        ]
    end}.

%% 无活跃订阅（空 map）：no-op，不调用 incr
no_subscription_noop_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        meck:expect(billing_subscription_ds, find_active_by_tenant, fun(0) -> #{} end),
        meck:expect(billing_usage_ds, incr, fun(_, _, _, _) -> {ok, 1} end),
        ok = billing_meter:do_meter(<<"messages_sent">>, 1),
        [?_assertEqual(0, meck:num_calls(billing_usage_ds, incr, '_'))]
    end}.

%% incr 失败不抛错，do_meter 仍返回 ok
incr_error_swallowed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        meck:expect(billing_subscription_ds, find_active_by_tenant, fun(0) ->
            #{<<"id">> => 9}
        end),
        meck:expect(billing_usage_ds, incr, fun(_, _, _, _) -> {error, db_down} end),
        [?_assertEqual(ok, billing_meter:do_meter(<<"messages_sent">>, 1))]
    end}.
