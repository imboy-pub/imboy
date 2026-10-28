%% 合成夹具：setup 失败（RUN05/cancel 事件路径）。
-module(quality_fixture_cancel_tests).
-include_lib("eunit/include/eunit.hrl").

cancel_test_() ->
    {setup, fun() -> erlang:error(synthetic_setup_failure) end, fun(_) -> ok end, fun ok_case/0}.

ok_case() -> ?assert(true).
