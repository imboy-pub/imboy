# AC-01 证据整改记录（第三轮评审发现 P1）

## 缘由
第三轮评审发现旧候选目录（417ba55f/20261014T130107-82080/AC-01）的证据自相矛盾：
exit_code=0 但 output.log 仅 2 行 createdb 失败。根因有二：
1. escript run_shell 的命令用 `| tee` 收尾，POSIX 管道下 $? 取 tee 的退出码
   （恒 0），runner 的真实失败被伪装成 PASS（本任务引入的工具缺陷，已修复：
   改为 `> output.log 2>&1`，尾部 printf 标记传播真实退出码）
2. 该次运行环境未提供正确的 REST_PG_PASSWORD（runner 合同要求环境注入），
   createdb 认证失败即退出

旧目录从未进入版本控制（git log 该路径为空），属工作区错误中间产物，已清除。

## 本次真实运行（候选 124dcbb7）
- REST_RUN_ID: limiter-l0-20261014T155741-81062（scratch 库由 runner 自动清理）
- AC-01 oracle（api_v1_login_SUITE, limiter_real_failures case）：
  **TEST COMPLETE, 6 ok, 0 failed of 6** —— 真实链失败计数断链复现成功
- evidence cross-check: case_pass=46/46（实跑 case 全部 PASS）
- 通道整体 rc=3 归因：api_v1_msg_SUITE seed fixture 失败
  （test/rest/suites/api_v1_msg_SUITE.erl:507 badmatch {error,c2c_sequence_missing}，
  msg 序列域；该文件及相关域在本分支区间零变更，指向 main 演进带入的
  messages 域问题（如 0d7295b0 gen_statem 回调类型修复），非本任务引入）
  → 2 个 msg evidence 缺失使 cross-check 期望 48 实有 46，runner 按合同
  判通道 FAIL（case_pass=46/46 并行给出）
- 证据三件套（input.json/output.log/exit_code）经核查无凭据泄漏
