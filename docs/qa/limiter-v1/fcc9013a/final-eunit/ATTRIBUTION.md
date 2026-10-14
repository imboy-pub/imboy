== 最终形态全量（绑定 fcc9013a = 分支吸收 main 7931f18a 后的合入前候选）==
环境: 隔离库 imboy_l0_eunit_20261014，端口 25432/29980/29970
结果: Failed 180 / Passed 11522
本任务 6 模块（login_attempt_ds_tests/passport_logic_tests/passport_handler_tests/
elib_req_client_ip_tests/throttle_middleware_tests/api_v1_login_SUITE）：零失败
（NONE-OF-MY-MODULES，按行首模块名归并 180/180 逐条核对）
剩余 180 例：imboy_cache_* 域约 120、enterprise_* 域约 25、agent_hird_* 域 9
（基线既有）、其余散点——均为其他会话 WIP/基线域
历史对照: run1 244（跨会话竞争窗口）→ run2 26（干净窗口）→ 收尾轮 249（main
cache 域 WIP 合入期）→ 本轮 180（966a753d hird 依赖钉扎后，Passed 11413→11522）
合并前定向重验（fcc9013a）: login_attempt_ds 10 / passport_logic 30 /
elib_req_client_ip 14 / throttle_middleware 13 / auth_oidc_logic 36（merge 带入）
全部通过
AC-01（REST 通道）: 见 docs/qa/limiter-v1/124dcbb7/20261014T155741-81062/AC-01/
（oracle 域 api_v1_login 6 ok 0 failed；通道 rc=3 归因 msg 域，非本任务）
