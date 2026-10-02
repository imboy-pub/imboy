# 完整回归失败修复 / Full regression test recovery

8f250707 全回归：10777通过、2失败，查询性能准备阶段超时取消；本轮为失败，不能作为完整门通过。保留失败终态 /tmp/gz-current-final-full-attempt-1/gz-current-final-full-pair-terminal.json。

仅修复三项测试：HTTPS URL校验精确断言共享guard的 invalid_scheme；归档群消息的零SQL断言只统计同步请求PID，排除运行中后台worker；查询性能项外层预算90秒覆盖50条异步落库准备，保留平均查询<=200ms断言。业务源码和安全措施未改。

隔离PG18/Garage中三模块组合复跑117通过、0失败、0跳过、无取消，exit0。当前提交包含被测试的8f父提交加最小测试补丁；result.json绑定补丁和每个文件SHA。它仅证明受影响组合，随后必须以新冻结提交完成两轮完整回归。

Original full run failed; the real PG/Garage focused recovery passed 117 tests. Production sources and the 200ms query threshold remain unchanged. This is not a complete backend gate or production acceptance.
