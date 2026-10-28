%% 合成夹具：测试体直接终止本 VM——模拟 opt-run4 中两个 worker VM 被某测试
%% 停掉 kernel 应用的死亡方式（SCHED05 轮换负向自测用；不写 crash dump）。
-module(quality_fixture_vmkill_tests).
-include_lib("eunit/include/eunit.hrl").

vmkill_test() -> erlang:halt(0).
