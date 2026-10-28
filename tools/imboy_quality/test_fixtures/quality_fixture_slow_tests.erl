%% 合成夹具：超 deadline 用例（RUN05；外层 deadline 回收本 VM）。
-module(quality_fixture_slow_tests).
-include_lib("eunit/include/eunit.hrl").

slow_test() ->
    timer:sleep(60000),
    ?assert(true).
