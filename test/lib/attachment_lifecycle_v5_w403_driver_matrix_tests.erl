%%% 每叶通过真实入口执行；缺入口 harness 是 NO_GO，不能用 raw driver 顶替。
-module(attachment_lifecycle_v5_w403_driver_matrix_tests).
-include_lib("eunit/include/eunit.hrl").

w403_driver_matrix_test_() ->
    [
        {
            atom_to_list(Surface) ++ "/" ++ atom_to_list(Driver) ++ "/" ++ atom_to_list(Scenario),
            {timeout, 180, fun() -> run_leaf(Surface, Driver, Scenario) end}
        }
     || Surface <- [ordinary, enterprise_internal, enterprise_seat, cs_widget],
        Driver <- [disk, garage],
        Scenario <- scenarios()
    ].
scenarios() ->
    [
        confirm_old_put_different_bytes,
        stat_then_different_put,
        inflight_same_bytes_after_confirm,
        put_paused_before_dispatch_until_expiry,
        same_key_different_worker_payload,
        provider_bypass_writer_denied,
        max_supported_file_gateway_budget,
        disk_put_delete_lock_order_or_garage_expiry_fence
    ].

%% 尚无四入口真实 HTTP fixture；逐叶失败，防止底层 raw driver 被误当协议验收。
run_leaf(Surface, Driver, Scenario) ->
    erlang:error({no_go, required_real_surface_harness_missing, Surface, Driver, Scenario}).
