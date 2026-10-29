%% 合成夹具：断言失败（RUN01 负例；外层必须验证失败被检出）。
-module(quality_fixture_fail_tests).
-include_lib("eunit/include/eunit.hrl").

fail_test() -> ?assertEqual(1, 2).
