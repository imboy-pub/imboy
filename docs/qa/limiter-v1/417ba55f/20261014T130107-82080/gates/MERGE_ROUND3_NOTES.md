# 第三轮 main 合入记录（收尾最后一轮，授权上限三轮）

- 合入前 main SHA: 25b58405（较上一轮吸收点 791c7d93 前进 23 提交）
- 本分支合入点: 42f5ade5 → 合并提交 d17d6617
- 冲突: 零。main 新 23 提交与本任务 13 个改动文件逐一核查（git log 42f5ade5..main -- <file>）均零交集
- 测试基建影响: main 唯一相关变更为 7a89890e（test/cache fixture 隔离），与本任务测试域无交集
- 重验: merge 后本任务 EUnit 模块逐一定向重验（隔离库 imboy_l0_eunit_20261014，
  端口 25432/29980/29970）：
  - login_attempt_ds_tests: All 10 tests passed
  - passport_logic_tests: All 30 tests passed
  - elib_req_client_ip_tests: All 14 tests passed
  - throttle_middleware_tests: All 13 tests passed
  - passport_handler_tests: All 7 tests passed
  - api_v1_login_SUITE: REST runner 通道复验于候选 124dcbb7（整改后真实退出码）：
    limiter 所在 suite **6 ok, 0 failed of 6**（含 limiter_real_failures AC-01 oracle），
    evidence cross-check case_pass=46/46；通道整体 rc=3 归因 api_v1_msg_SUITE
    fixture 失败（c2c_sequence_missing，非本任务域，见
    docs/qa/limiter-v1/124dcbb7/20261014T155741-81062/AC01_EVIDENCE_NOTES.md）。
    注：早前记录的"AC-01 PASS"系 escript 管道吞退出码产生的假象，旧证据目录
    已清除并以新运行证据取代。
