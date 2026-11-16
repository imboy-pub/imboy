# LEE-5：QR 登录 LoginToken 非原子领取——安全审计与修复报告
# QR login single-claim security audit and follow-up

> 基础修复已提交 `main`：`fdd955f5671d249384008a9ee2ad508158ce7dbf`。
> 后续质量修复基线：`77dc4b059779cf9491c0784ca7640932176bbfb8`；本候选须重新验证，不能沿用基础修复的 PASS。
> 影响仓：`imboy`。不新增迁移、不改 Flutter、不引入 Redis/分布式锁、不操作生产数据库。
> 关联：[G-QR 原提案](../architecture/cache-v2.2.1/g-qr-atomic-consumption-proposal.md)。

The original fix is on main. This document distinguishes historical author-reported
results, the follow-up candidate, and production rollout evidence. A merged commit
or a written test is not proof that the current candidate has passed acceptance.

## 0. 最终验收口径 / Acceptance invariant

同一 QR Session 的凭据在 SSE、Polling 和不同后端节点间**最多成功领取一次**。
唯一领取路径为 `qr_login_session_repo:claim_login_token/1`：

```sql
UPDATE public.qr_login_session
SET login_token = NULL, consumed_at = now()
WHERE session_token = $1 AND status = 'confirmed'
  AND login_token IS NOT NULL AND consumed_at IS NULL AND expires_at > now()
RETURNING OLD.login_token, scanned_by;
```

Token 与 scanned_by 来自同一次原子领取；零行、PG 故障、版本不足均不得发放凭据，
也不得降级 Cache。**503 不证明写入已回滚**：提交结果未知时不重试写入、不恢复 token、
不补发凭据；后续请求仍走同一个原子 claim，已消费则返回 5203。

At-most-once claim is not exactly-once network delivery. Evidence here assumes one
authoritative PostgreSQL history; it does not establish durability across promotion
of a replica that has lost an acknowledged commit, split-brain, or database restore.

## 1. 安全审计报告（修复前） / Original defect

| 旧入口 / Old entry | 问题 / Defect |
|---|---|
| Cache polling `cache_handle_status` | get → 读 token → delete 非原子；并发请求可读到同一凭据 |
| Cache SSE `cache_handle_confirm` | 向所有订阅者广播带 token 的 confirmed；没有与 polling 共享的领取裁决 |
| PG polling / SSE | 已有共享原子 claim，但旧代码默认 authority 为 cache |

根因是默认路径缺少全局原子领取，以及把广播通知当成凭据交付。
基础修复删除的是 **QR 专属 Cache 权威业务分支**，不是全局缓存或 syn/SSE 基础设施。

旧代码默认值可由源码确认；**不能仅凭仓内模板、环境变量白名单或迁移文件注释，
断言每个生产实例的实际配置和迁移状态**。这些必须以部署现场证据核验。

The old default was unsafe. Repository defaults alone do not establish which
configuration, schema or image every production node was actually running.

## 2. 修复方案对比与最终选择 / Chosen design

采用方案 A：PostgreSQL 管理 QR 生命周期与原子领取；Cache 无凭据授权权。
复用现有 PG 表、CAS 与 claim，不新增 Redis、分布式锁或迁移。
`qr_login_authority` 默认 pg，接受 pg/postgres；其他值告警并按 pg，不能重新启用旧路径。
SSE 的 confirmed_ready 及 legacy confirmed 仅是领取通知，不信任事件内 token/uid。

PostgreSQL remains the sole authority. This follow-up changes error classification
and tests, not claim SQL, state-transition predicates, signing order or schema.

## 3. 实际代码变更 / Implementation and follow-up

基础提交已落实：删除 QR cache_* 分支、统一 SSE/Polling claim、Repo 层 PG18 门禁、
周期清理 worker、supervisor 接线及 `config/sys.config.example` 的显式 PG 配置。

后续审查发现基础提交仍把部分存储故障合并为 404、5203、过期或无效二维码。
本候选分离 SQL 正常执行但零行与 SQL 执行失败，保持 fail-closed：

| 文件 / File | 后续改动 / Follow-up |
|---|---|
| `src/repo/qr_login_session_repo.erl` | claim / scan / confirm / cancel 执行失败返回 `{error,{pg_error,Reason}}`；零行保持原业务结果；cancel 幂等回查的错误也透传；不自动重试 |
| `src/ds/qr_login_session_ds.erl` | 薄转发与类型合同同步，不新增缓存或编排 |
| `src/api/qr_login_handler.erl` | Session 查询、QR 反查、claim、scan/confirm/cancel 及其回查故障映射业务码 503，不把故障当成业务状态 |
| `src/api/qr_login_sse_handler.erl` | pg_error → `: service_unavailable` 并关闭；正常无可领取记录仍为 claimed_elsewhere |
| `test/api/qr_login_handler_tests.erl` | 区分真不存在的 404 与查询故障的 503，保留既有成功与业务拒绝用例 |
| `test/api/qr_login_pg_failure_tests.erl` | MOCK 故障合同：真实 handler/DS/repo，SQL/运输/签发/设备边界替身；校验各故障阶段、无通知、无自动重试 |
| `test/api/qr_login_claim_acceptance_tests.erl` | 混合竞争按 SSE/Polling 分别校验败者；校验消费落库、实际 SSE UID、唯一帧；补查后 claim 断线及已提交结果丢失场景 |

对外合同 / External contract：

| 情况 / Outcome | REST 信封业务码 / Envelope code | SSE |
|---|---|---|
| 查询正常、Session 不存在 | status 为 404；其他入口保持原业务语义 | 不新增协议 |
| claim 正常执行、零行 | 5203 | `: claimed_elsewhere` + close |
| QR Session 查询或 SQL 执行失败 | 503 | claim 时 `: service_unavailable` + close |
| PG18 门禁失败 | 503 | `: service_unavailable` + close |
| claim 成功 | 原 status/token/uid/uid_str | 原 confirmed 帧 |

REST 错误仍使用现有 **HTTP 200 + 业务 code** 信封；监控不能只看 HTTP 5xx。
客户端不得获得 SQL 错误细节或凭据。这里的故障合同针对 QR Session 存储调用，
不声称已改造所有签发、设备登记或其他业务依赖的异常处理。

## 4. PostgreSQL 版本兼容性结论 / PostgreSQL compatibility

保留 PostgreSQL >=18 要求及 `RETURNING OLD`；不自动升级数据库、不重写领取 SQL。
Repo create/claim 首次使用时检查 server_version_num；这不是一次完整的部署预检。
版本查询失败不缓存；成功查询出的低版本也会记忆，数据库大版本变更后须重启后端节点。
门禁检查版本，不替代迁移、权限、连接和真实登录的验证。

The runtime compatibility gate covers create/claim, including direct SSE claims.
Deployment must independently verify schema, permissions and the actual database endpoint.

## 5. 新旧节点切换方案（混合部署安全） / Rollout

旧节点仍提供 Cache QR 登录时，漏洞仍在；新节点拒绝旧会话不等于旧节点已安全。
60 秒 TTL 不是发布安全保证。发布时必须核验：

1. 目标库版本 >=18，迁移 `00000170_qr_login_session` 已应用，运行身份权限正确。
2. 维护窗口或受控蓝绿切换；旧节点全部退出 QR 路由，**包括已建立的 SSE 长连接**。
3. 逐节点记录 commit SHA/镜像 digest、实际配置、请求路由与进程状态；不能只检查 tag。
4. 完成真实扫码、SSE、polling、错误恢复回归，再宣布生产修复生效。

该 PR 不执行生产迁移、切流或部署。库迁移和配置核验由授权运维按现有手册执行。
No production closure is claimed without node-by-node rollout and end-to-end evidence.

## 6. 过期数据清理实现 / Cleanup and capacity

基础提交的 worker 默认启动延迟 60 秒、每 5 分钟清理一批、默认批量 500；
仅删除过期行。claim 自身检查 expires_at，不依赖清理任务作为安全裁决。
本候选不改 worker 或清理 SQL。

**纠正容量说明**：500 是默认批量，不是当前代码强制的最大值；配置可覆盖。
单节点按默认值的理想处理上限为 `500 / 300 ≈ 1.67 行/秒`，实际还受执行时间与失败影响。
持续创建速率超过有效清理能力就可能持续积压，不是只有 >100/s 才有风险，
也不能声称只靠多轮清理或索引必然收敛。应观察过期积压、清理耗时和数据库负载后调参。

Default batch size is not a hard cap. Capacity and sustained backlog have not been
load-tested here; do not interpret bounded batches as proof of bounded table growth.

## 7. 测试结果与证据边界 / Validation evidence

### 7.1 基础修复的历史记录 / Historical author-reported results

以下保留 2026-11-14 实施者在基础提交中的记录，**不是本候选重新执行的结果**：

| 套件 / Suite | 原记录 / Reported | 证据类别 / Kind |
|---|---|---|
| handler | 28/28 | Mock |
| SSE | 22/22 | Mock |
| UID contract | 15/15 | Mock |
| cleanup worker | 6/6 | Worker unit |
| repo | 19/19 | PG 集成，内含独立标注的 mock 版本门禁用例 |
| acceptance | 5/5，LEE5_ACCEPTANCE=1 | 原版真实 PG + callback/连接边界替身 |
| dcheck | 3/3；每场景 2 个 PG 锁等待者、1 份凭据 | 双 peer 节点连接同一真实 PG；不是浏览器端到端 |

原文的“dialyzer 零警告”与提交说明“改动模块零警告、97 条历史警告”口径不同；
不能据此声称全仓零警告。须附新运行日志、退出码及现有基线门结果，不降低基线。
原 `/tmp/lee5-dcheck.json` 的路径也不等于仓内已保存可下载的执行证据。

### 7.2 本候选新增/加强的验证 / Candidate tests

- 故障合同 mock 用例：QR 反查、Session 首读/二次读、scan/confirm/cancel CAS、
  cancel 回查、claim 及 legacy SSE，明确断言 503/service_unavailable，无通知、无重试。
- 混合竞争：50 SSE + 50 polling；恰一赢家；其余逐入口断言 claimed_elsewhere/5203；
  timeout、unexpected、错误码错配均不得计为正常败者；检查 token 已清空与 consumed_at。
- 真连接故障：初始查询、claim 和 SSE 死连接；确认 CAS 死连接；查询成功后 claim 借到死连接。
- 已提交结果丢失：真实 PG 执行 claim 后，**在 elib_pg 返回边界注入错误**；首次 503，
  数据库仍为已消费，后续 polling/SSE 不重发。这不是实际断网或数据库故障切换演练。
- SSE 观察器读取实际 uid/uid_str，要求恰一帧，并在每次调用前清空旧观察数据。

**本次编辑会话未运行 compile、Dialyzer、EUnit、真实 PG 或双节点 dcheck**：
执行环境没有 Erlang/PostgreSQL 工具链，且无法克隆仓库到构建环境。
因此候选应保持 Draft，待现有 CI 与隔离 PG 验收补齐；不能把测试源码的存在或旧 PASS 当作新 PASS。

Candidate tests are authored, not executed in the editing session. Preserve all
existing gates; report exact candidate SHA, commands, exit codes, skipped counts
and CI/artifact URLs when validation is run.

### 7.3 验证入口 / Revalidation commands

先确认数据库是授权的**隔离测试库**，不是生产或共享工作库；项目测试启动链可能自动迁移。
配置文件 `config/sys.closeout-local.config` 是现场准备的本地文件，不能假定克隆后存在。
按仓库规则串行执行，不并发运行共享 meck/BEAM/数据库套件：

```bash
make compile
make dialyze
make eunit-local t=qr_login_handler_tests
make eunit-local t=qr_login_sse_handler_tests
make eunit-local t=qr_login_uid_contract_tests
make eunit-local t=qr_login_pg_failure_tests
make eunit-local t=qr_login_cleanup_worker_tests
EUNIT_CONFIG=config/sys.closeout-local make eunit-local t=qr_login_session_repo_tests
LEE5_ACCEPTANCE=1 EUNIT_CONFIG=config/sys.closeout-local \
  make eunit-local t=qr_login_claim_acceptance_tests
```

复用 `scripts/cache_v221_qr_entry_dcheck.erl` 的既有编译/加载流程，两个 peer 连接同一隔离 PG；
要求 cases=3、failed=0、skipped=0，并保存脱敏 JSON。最终集成候选仍须执行项目适用全量门禁。
GitHub Actions 的执行状态以 PR checks 的**该候选 SHA** 为准，未触发/未完成/失败不得记作通过。

## 8. 配置迁移说明 / Configuration provenance

基础提交真正受 Git 跟踪的配置变更是 `config/sys.config.example`。
`config/sys.pro.config` 与 `config/sys.local.config` 属于被忽略的本地运行配置；
作者在本地改过不等于已提交或已分发，必须部署逐机核验，**不要提交含密钥的本地配置**。
代码缺省 pg，显式 cache 也被覆盖，因此配置文件未随 Git 分发不构成旧 Cache 路径的重新启用。

Only the example template was part of the Git change. Local/pro configuration and
production migration state require separate deployment evidence.

## 9. 部署与回滚 / Deployment and rollback

本质量补丁回退应保留 `fdd955f` 的 PG 唯一权威修复；没有数据库迁移需要回退。
**不得把恢复旧 Cache 镜像作为普通安全回滚方案**。如应急必须运行旧版本，先隔离全部
QR 入口并关闭旧 SSE 连接，保持 QR 不可用，直到恢复安全版本且完成核验。
不要为回滚代码降级或恢复数据库历史中的已消费凭据。

Rollback must not re-enable the vulnerable QR path. Production operations remain
outside this PR. Monitor envelope code 503 and existing sanitized error signals,
not only HTTP status codes; a 503 must never be interpreted as proof of rollback.

## 10. 未通过及未执行的验收项 / Outstanding verification

本候选尚无运行通过声明。待补：编译/类型检查、定向与适用全量测试、正式 PG 验收、
双节点重跑、对应 SHA 的 CI、实际新旧版本切换与长连接退出演练、真实浏览器/Flutter 回归、
容量/性能测试、生产版本/数据库/迁移核验。

只读代码审查不能确认生产修复已生效；callback 测试不能替代真实 HTTP/SSE 网络交付。
`/subscribe`、`/cancel` 的 session_token 持有者授权设计、数据库 HA/恢复策略与清理容量
仍是单独的安全/运维边界。测试环境故障须如实报告，不通过 skip 或降低门槛取得绿色结果。
