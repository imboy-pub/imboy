# LEE-5：合并后验收与门禁 / Post-merge validation and gates

## 状态与证据 / Status and provenance

PR #67 已于 2026-11-16 合并为 `b4b8ad4c82964326c1a040e036a018c4675b86c3`。
其 PR head `2480ae602363bb3e037bd8a4c6bbaf63fddb2f51` 的 Backend CI
[run 43788239717](https://github.com/imboy-pub/imboy/actions/runs/43788239717) 已通过。
这不等于此后候选、QR 专项或生产部署已通过；历史报告中的 Draft/未运行描述是当时记录。
本页补充 [原审计报告](LEE-5-qr-login-claim-audit-and-fix.md)，不把旧 PASS 移植到新 SHA。

PR #67 is merged. Its successful general CI is historical evidence, not acceptance
of this follow-up or production. This page supersedes the old candidate-status
wording without rewriting its historical limitations.

本补充仅改 CI、测试、验收脚本和文档，不改生产模块、claim SQL、迁移或 Flutter。
编辑环境只执行了 Shell/Python 门禁回归、语法和差异检查；没有 Erlang/PG 工具链。
真实执行结果必须以本 PR 精确 SHA 的 Actions 与下载证据为准，未执行不记 PASS。

This follow-up changes tests and gates only. Local guard tests are synthetic,
not database or multi-node proof. Record exact checkout SHA and Actions run/attempt.

## 可重复验收 / Repeatable acceptance

Backend CI 的现有 E2EE 作业复用其已迁移的临时 PG18，串行执行：

- Handler、SSE、UID contract、PG failure、cleanup-worker 五个回归模块。
- `qr_login_session_repo_tests`：真实状态机与并发领取，含明确标注的版本门禁 mock。
- `qr_login_claim_acceptance_tests`：100 polling、100 SSE、50+50、死连接和已提交结果丢失。
- `cache_v221_qr_entry_dcheck`：两真实 peer、三组入口竞争、每组两个 PG 锁等待者；
  token、uid、uid_str 同时验证。不是浏览器端到端，也不是数据库故障转移演练。

The existing disposable PG image/build is reused. Suites run sequentially in
separate VMs; two peers use the same verified PostgreSQL endpoint. No extra service
or migration is introduced. SQL-error losers are not accepted as normal contention.

入口为 `bash scripts/ci/lee5-acceptance.sh`，仅支持明确授权的 GitHub Actions 临时环境：
`GITHUB_ACTIONS=true`、`LEE5_CI_DISPOSABLE=1`，并核对容器与 TCP 端点 system identifier、
PG18 及 QR 表。配置从 example 生成，不读取开发者的私有 closeout 配置，不自动迁移。
不要在生产或共享库上运行；Repo 清扫测试会删除过期 QR 行。

The CI-only runner refuses unapproved environments and stale evidence directories.
For local work use the original documented commands against an authorized exclusive
fixture. Never point these cleanup/concurrency tests at production or shared data.

## 不允许假绿 / Fail-closed evidence

`LEE5_ACCEPTANCE=1` 覆盖 Repo 与入口验收：DB fixture 失败必须失败，不能返回空用例列表。
`verify_lee5_evidence.py` 同时核对执行成功、真实退出码、当前 checkout SHA、七模块非空
JUnit XML、零 failure/error/skip、双 peer 三场景及唯一凭据/身份断言。
缺前置、缺报告、空报告、错 SHA、超时或跳过均不通过。输出不会把 mock 标成真库证据。

An always-run verifier rejects a skipped runner as well as missing/failed/empty
suite evidence. The uploaded artifact `lee5-acceptance-<sha>-<attempt>` contains
only sanitized `summary.json` (counts, checkout and scope). Raw XML, logs, database
configuration and credentials are not uploaded. FAIL is not replaced by old PASS.

Xref 单独使用 `scripts/ci/xref-gate.sh`：make 非零、tee 失败、空日志、未定义调用均失败。
离线回归：`bash scripts/test/xref_gate_test.sh`；
`python3 -m unittest discover -s scripts/test -p test_lee5_ci_evidence.py -v`。
这些是门禁逻辑测试，不是实际 Xref / Erlang / PostgreSQL 运行证据。

## 生产关闭仍需现场证据 / Production closure remains separate

发布记录必须附目标 SHA/镜像 digest、真实 PG18/schema/权限核验、旧 QR 节点及已建立
SSE 长连接全部退出、浏览器/Flutter 实测和回滚方案。SSE `: service_unavailable` 是
注释而非 onmessage 事件；验证关闭后降级轮询、信封 503 提示和 5203 终态处理。
监控不能只看 HTTP 5xx，因为 REST 仍是 HTTP 200 + 业务错误码。

No CI result closes production LEE-5 by itself. Preserve the PG-authority fix on
rollback, or disable QR; never restore the vulnerable cache path. Database failover
with lost acknowledged writes, restore/replay and browser end-to-end are not proved
by callback observers. Branch protection is a separate owner-admin change and is
not altered by this PR.
