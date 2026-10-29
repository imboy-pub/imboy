%% 合成夹具：异常 class error（RUN01 异常路径，保留原始诊断）。
-module(quality_fixture_crash_tests).
-include_lib("eunit/include/eunit.hrl").

crash_test() -> erlang:error(synthetic_crash).
