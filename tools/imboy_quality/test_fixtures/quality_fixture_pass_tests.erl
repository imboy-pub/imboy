%% 合成夹具：通过用例（只在受控子运行中执行，不进入项目全量 U）。
-module(quality_fixture_pass_tests).
-include_lib("eunit/include/eunit.hrl").

ok_test() -> ?assertEqual(1, 1).
