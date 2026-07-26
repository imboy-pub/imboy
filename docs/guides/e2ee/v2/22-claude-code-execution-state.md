# IMBoy E2EE Claude Code 跨会话执行状态

> **用途**：多个 Claude Code 会话之间共享唯一的 E2EE 任务状态，避免重复实现、跳过验收或在协议未批准时越界开发。  
> **详细计划**：[`21-claude-code-execution-playbook.md`](./21-claude-code-execution-playbook.md)  
> **上位验收计划**：[`20-implementation-and-acceptance-plan.md`](./20-implementation-and-acceptance-plan.md)  
> **状态文件**：本文件是任务状态的唯一事实来源；`21` 中的状态表仅作为初始快照。

## 1. 当前总状态

```yaml
state_version: 1
last_updated: 2026-07-26
release_track: PREVIEW
current_gate: G1_P0_CLOSURE
current_batch: HOTFIX
next_task: HOTFIX-01
active_session: null
human_gate:
  adr_14_19: BLOCKED
  adr_14_19_reason: "仍为 Proposed；不得自行代签"
  release_track_choice_after_c2c: PENDING
overall_status: IN_PROGRESS
```

当前发布声明只能是 `Preview`。在下列条件全部满足前，不得对外宣称 `GA-C2C` 或 `GA-Top-Tier`：

- 单聊完成每设备 Olm，而不是 C2C Megolm；
- 设备身份签名、设备信任、Protected Frame v3 和设备撤销完成；
- PFS、PCS、事务性 CryptoStore 和恢复完成；
- 群聊 MLS 完成并通过独立互操作；
- 真实 Android/iOS 多设备测试完成；
- 外部安全审计没有未解决的 Critical/High 问题。

## 2. 每次新 Claude Code 会话的启动指令

将下面的内容作为每次新会话的第一条任务指令：

```text
你正在执行 IMBoy E2EE 分阶段计划。

先阅读：
1. /Users/leeyi/project/imboy.pub/AGENTS.md
2. /Users/leeyi/project/imboy.pub/imboy/CLAUDE.md
3. /Users/leeyi/project/imboy.pub/imboyapp/CLAUDE.md
4. /Users/leeyi/project/imboy.pub/imboy/docs/guides/e2ee/v2/20-implementation-and-acceptance-plan.md
5. /Users/leeyi/project/imboy.pub/imboy/docs/guides/e2ee/v2/21-claude-code-execution-playbook.md
6. /Users/leeyi/project/imboy.pub/imboy/docs/guides/e2ee/v2/22-claude-code-execution-state.md

执行规则：
- 先读取 22 文件的当前状态，不要猜测任务状态。
- 执行 git rev-parse --show-toplevel、git status --short、git log -1 --oneline。
- 若 active_session 非 null，先报告已有会话，不抢占任务。
- 若无 active_session，只领取 next_task 指定的一个任务；不要自行挑选其他任务。
- 领取任务前把 active_session 改成当前会话标识，并把该任务改为 IN_PROGRESS。
- 先新增或补充失败测试，再修改生产代码。
- 不得删除 skipped 测试，不得通过放宽安全策略、开启 fallback 或跳过真机测试来变绿。
- 不得修改 erlang.mk、ios、macos、plugin/r_upgrade 等保留区。
- 不 push、不部署、不访问生产、不通知第三方、不自动创建外部服务资源。
- 完成后必须运行任务验收命令，生成对应 evidence 文件，并回写 22 文件。
- 只有证据完整且验收通过，任务才能从 IN_PROGRESS 改为 PASS。
- 如果遇到阻塞、用户改动冲突、ADR 未签署或测试环境缺失，改为 BLOCKED，记录原因并停止。
- 一次会话最多完成一个任务；完成后停止，不继续领取下一个任务。

会话结束时输出：
- 当前任务和状态；
- 修改文件；
- 新增测试；
- 验收命令和结果；
- evidence 文件；
- 剩余风险；
- 下一任务；
- 最后一行：Ready for feedback.
```

## 3. 状态机

任务状态只允许使用以下值：

| 状态 | 含义 | 是否可以继续下游任务 |
|---|---|---:|
| `PENDING` | 尚未领取 | 否 |
| `IN_PROGRESS` | 当前会话正在执行 | 否 |
| `PASS` | 代码、测试、证据均满足验收 | 是 |
| `PARTIAL` | 局部完成，但尚有明确残留项 | 否 |
| `BLOCKED` | 依赖、人工决定或环境阻塞 | 否 |
| `SUPERSEDED` | 被新协议或新任务替代 | 按替代任务决定 |

状态转换规则：

```text
PENDING -> IN_PROGRESS -> PASS
PENDING -> IN_PROGRESS -> PARTIAL
PENDING -> IN_PROGRESS -> BLOCKED
PARTIAL -> IN_PROGRESS     只能在残留项明确后继续
BLOCKED -> PENDING          只能在阻塞原因解决后继续
PASS -> SUPERSEDED          只能由新的协议决策或任务替代
```

禁止：

- `IN_PROGRESS -> PASS` 没有测试和 evidence；
- 用 `SKIP` 代替真实设备或真实 PostgreSQL 验收；
- 把 `PARTIAL` 或 `BLOCKED` 改成 `PASS`；
- 让多个会话同时拥有 `active_session`；
- 把人工 Gate 当作自动化测试通过。

## 4. 会话领取和释放协议

### 4.1 领取任务

当前会话领取任务时，只修改本文件的状态区：

```yaml
active_session:
  id: "YYYYMMDD-HHMM-claude-code"
  task: "HOTFIX-01"
  repo: "imboyapp"
  started_at: "2026-07-26T00:00:00+08:00"
  owner: "current-session"
```

然后将对应任务状态改成 `IN_PROGRESS`。`id` 使用时间和任务标识即可，不得写入手机号、邮箱或其他个人联系方式。

### 4.2 完成任务

只有以下项目全部具备时，才能改为 `PASS`：

- 代码修改范围符合任务边界；
- 正向、负向、边界测试齐全；
- 任务验收命令通过；
- `git diff --check` 通过；
- 无明文、私钥、token、PII 泄漏；
- evidence 文件已更新；
- 未新增静默降级；
- 真实设备/真实 PostgreSQL 条件已满足，或任务明确不需要；
- 下一任务依赖关系已重新计算。

完成后清空 `active_session`，将 `next_task` 改为依赖满足的第一个任务，并追加会话日志。

### 4.3 阻塞任务

阻塞时必须写明：

```yaml
blocked:
  reason: "真实 Android 设备不可用"
  first_seen: "2026-07-26"
  required_action: "提供真实设备并重新运行 E2EE-019"
  safe_alternative: "只能继续非验收级静态测试，不能标记 PASS"
```

阻塞不能通过猜测、放宽安全等级、删除测试或伪造 evidence 解决。

## 5. 当前任务状态

### 5.1 Hotfix 前置任务

这些任务是当前审计发现的 P0/P1 问题，必须在继续 Protected Frame v3 和 C2C GA 前完成。

| ID | 任务 | 仓库 | 依赖 | 状态 | 验收重点 |
|---|---|---|---|---|---|
| HOTFIX-01 | 删除发送前明文日志 | imboyapp | 无 | `PENDING` | 日志、异常、埋点无消息正文 |
| HOTFIX-02 | 合规群聊密钥失败必须 fail-closed | imboyapp | HOTFIX-01 | `PENDING` | 密钥失败时网络发送次数为 0 |
| HOTFIX-03 | Room Key 包装失败不得静默省略设备 | imboyapp | HOTFIX-01 | `PENDING` | 严格模式无部分设备成功 |
| HOTFIX-04 | 统一 Olm-only v3/RSA decrypt-only 文档和测试 | imboy、imboyapp | HOTFIX-01..03 | `PENDING` | 新写入不生成 RSA wrap |

当前 `next_task` 是 `HOTFIX-01`。Hotfix 必须按顺序执行；每次会话只领取一个 Hotfix。

### 5.2 原有 E2EE 任务

| ID | Batch | 任务主题 | 状态 | 证据/备注 |
|---|---|---|---|---|
| E2EE-000 | B00 | 基线与证据目录 | `PASS` | `evidence/E2EE-000.md` |
| E2EE-001 | B00 | ADR14–19 人工接受 | `BLOCKED` | 仍为 Proposed，不得代签 |
| E2EE-010 | B01 | Policy Gate fail-closed | `PARTIAL` | 基础测试已有；群聊合规路径仍需修复 |
| E2EE-011 | B01 | Room Key 禁止 RSA 静默降级 | `PARTIAL` | 接收端已有；发送端失败闭环待完成 |
| E2EE-012 | B02 | Protected Context 纵向闭环 | `PENDING` | 等待 Hotfix 和 ADR15 |
| E2EE-013 | B03 | 设备所有权与 Token 绑定 | `PARTIAL` | 后端有基础能力，客户端完整闭环待验证 |
| E2EE-014 | B03 | Trust Event、身份新鲜度和幂等 | `PARTIAL` | 后端较完整，客户端真实验签和 UI 待完成 |
| E2EE-015 | B04 | Secret Inventory、登出和残留清理 | `PARTIAL` | 实现已有，真实设备旅程待完成 |
| E2EE-016 | B04 | 备份解析和边界校验 | `PASS` | 仅代表旧备份解析，不代表 Recovery Vault v2 |
| E2EE-019 | B05 | 自动化基线 | `IN_PROGRESS` | 109 passed/10 skipped；真机矩阵待完成 |
| E2EE-020 | B06 | Device Manifest | `PENDING` | 依赖 E2EE-012、ADR14 |
| E2EE-021 | B06 | Signed Capabilities | `PENDING` | 依赖设备身份 |
| E2EE-022 | B06 | 客户端身份签名验证 | `PENDING` | 不能只信服务端返回值 |
| E2EE-023 | B07 | Protected Frame v3 canonical encoding | `PENDING` | 依赖 E2EE-012 |
| E2EE-024 | B07 | Context binding 和 mutation matrix | `PENDING` | 变异拒绝率必须 100% |
| E2EE-025 | B07 | Replay、counter 和 epoch | `PENDING` | 依赖事务性状态 |
| E2EE-026 | B08 | Transactional CryptoStore | `PENDING` | 依赖 E2EE-025 |
| E2EE-027 | B08 | Outbox、dedupe 和 crash recovery | `PENDING` | 依赖 E2EE-026 |
| E2EE-029 | B09 | C2C per-device Olm fan-out | `PENDING` | 新 C2C 禁止 Megolm/RSA |
| E2EE-030 | B10 | PFS | `PENDING` | 真实设备攻击测试 |
| E2EE-031 | B10 | PCS | `PENDING` | 真实设备攻击测试 |
| E2EE-032 | B10 | C2C 多设备集成 | `PENDING` | 2 用户 × 3 设备 |
| E2EE-033 | B11 | Cross-signing | `PENDING` | 依赖 E2EE-022 |
| E2EE-034 | B11 | Safety Number/TOFU | `PENDING` | 包括设备变更提醒 |
| E2EE-035 | B12 | Recovery Vault v2 | `PENDING` | Argon2id、新 DID |
| E2EE-036 | B12 | 恢复权限和历史消息边界 | `PENDING` | 身份恢复与历史恢复分离 |
| E2EE-039 | B13 | GA-C2C 发布门 | `PENDING` | 需要用户选择是否先发布 C2C |
| E2EE-040 | B14 | MLS 技术验证 | `PENDING` | 必须使用成熟实现 |
| E2EE-041 | B15 | MLS Key Package/Welcome | `PENDING` | 依赖 E2EE-040 |
| E2EE-042 | B16 | MLS Commit/epoch | `PENDING` | 依赖 E2EE-041 |
| E2EE-043 | B16 | MLS 成员变更 | `PENDING` | 新成员/移除成员边界 |
| E2EE-044 | B16 | MLS CryptoStore 和恢复 | `PENDING` | 依赖 E2EE-026 |
| E2EE-045 | B17 | MLS 分叉、乱序、重放 | `PENDING` | 负向测试优先 |
| E2EE-046 | B17 | Megolm 历史迁移 | `PENDING` | 旧协议只读解密 |
| E2EE-049 | B18 | MLS 多设备群聊集成 | `PENDING` | 3/10/100/1000 设备 |
| E2EE-050 | B19 | 独立互操作测试 | `PENDING` | C2C 或 Top-Tier scope |
| E2EE-051 | B20 | Fuzz、Property、Chaos | `PENDING` | 10,000 次 crash 注入 |
| E2EE-052 | B20 | 性能和资源预算 | `PENDING` | p95/p99/OOM 门禁 |
| E2EE-053 | B21 | 外部安全审计整改 | `PENDING` | Critical/High 必须为 0 |
| E2EE-054 | B22 | GA-Top-Tier 发布证据 | `PENDING` | 证据包和发布声明 |

## 6. Batch 依赖图

```text
B00 基线/人工 Gate
  -> HOTFIX P0 Closure
  -> B02 Protected Context
  -> B03 Device Trust
  -> B04 Logout/Backup
  -> B05 Real Device Baseline
  -> B06 Device Manifest
  -> B07 Protected Frame v3
  -> B08 Transactional CryptoStore
  -> B09 C2C Olm
  -> B10 PFS/PCS
  -> B11 Cross-signing
  -> B12 Recovery Vault v2
  -> B13 GA-C2C Gate
       -> 先发布 C2C：直接进入 B19-B22
       -> 继续顶级群聊：进入 B14-B18
  -> B19-B22 Interop/Audit/Release
```

执行者不得跨越未通过的依赖。只有人工明确选择后，才能决定在 B13 之后先发布 GA-C2C，还是继续执行 MLS。

## 7. 每个任务的自动化验收要求

### PR 必跑

- `dart analyze lib`；
- Flutter E2EE 单元、负向、边界测试；
- Protected Frame 编解码和 mutation tests；
- Ed25519 身份签名验证；
- replay、duplicate、out-of-order；
- Policy Gate fail-closed；
- Erlang 编译和相关 EUnit；
- 服务端 zero-crypto 检查；
- 明文日志和敏感字段扫描；
- `git diff --check`。

### Nightly

- 临时 PostgreSQL 集成测试；
- OTK 并发领取；
- 设备撤销和信任变化；
- 多设备发送/接收；
- 附件加密；
- 崩溃恢复；
- 备份恢复。

### Release Gate

- Android/iOS 真实设备矩阵；
- 2 用户 × 3 设备单聊；
- 3、10、100、1000 设备群聊；
- 离线、乱序、重复、丢包、前后台和强制杀进程；
- 10,000 次 crash/restart 注入；
- 独立实现互操作；
- 外部安全审计。

关键安全测试不得以 `skip` 代替。GA 阶段要求 Critical security tests 的 skip 数为 0。

## 8. 每次会话结束必须追加的日志

追加到本文件末尾，不覆盖历史记录：

```markdown
### Session YYYY-MM-DD HH:MM — TASK-ID

- Session ID:
- Repository:
- Before HEAD:
- After HEAD:
- Status: PASS / PARTIAL / BLOCKED
- Changed files:
- Tests added:
- Verification commands:
- Verification result:
- Evidence:
- Residual risks:
- Next task:
- Reviewer decision: Pending
```

日志中禁止写入：

- access token；
- 私钥、会话密钥、内容密钥；
- 手机号、邮箱、真实用户 ID；
- 消息明文或完整密文；
- 生产数据库连接信息。

## 9. 发布等级状态

| 发布等级 | 当前状态 | 必须完成 |
|---|---|---|
| Preview | 当前 | 现有能力可用，但不宣称顶级 |
| Strong Preview | 未达成 | P0/P1 关闭、真实设备基础矩阵 |
| GA-C2C | 未达成 | E2EE-020–039 |
| GA-Top-Tier | 未达成 | GA-C2C + E2EE-040–054 |

任何会话不得自行修改发布等级。发布等级、外部审计、生产部署和对外发布均需要用户另行确认。
