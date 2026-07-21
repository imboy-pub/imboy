# IMBoy E2EE Claude Code 逐任务执行手册

> **状态**：Executable Plan（架构决策仍以 ADR 14–19 的签字状态为准）
> **适用执行者**：Claude Code / Codex / 人类工程师
> **上位计划**：`20-implementation-and-acceptance-plan.md`
> **强制规则**：每次最多执行同一 Batch 中的 3 个任务；每个任务独立验收、独立证据、独立提交；Batch 完成后停止并报告 `Ready for feedback.`

---

## 0. 复制给 Claude Code 的启动指令

```text
完整阅读：
1. 工作区根 AGENTS.md；
2. 目标仓库及目标目录的 AGENTS.md；
3. imboy/docs/e2ee/v2/14~21；
4. 当前任务列出的源代码和测试。

然后执行 21-claude-code-execution-playbook.md：
- 从当前目标发布轨道中选择“依赖全部通过”的第一个 Pending Batch；到达 B13 后先让用户选择 C2C GA 或继续 MLS；
- 一次最多执行 3 个任务，不得跨 Batch；
- 每个任务先复现/新增失败测试，再做最小实现，再运行该任务全部验收；
- 验收失败、环境缺失、ADR 未签字或需要外部操作时立即停止，不得猜测或跳过；
- 不 push、不部署、不访问生产、不通知第三方，除非用户在当前会话明确授权；
- 完成 Batch 后输出：修改文件、测试命令与结果、证据文件、提交哈希、残留风险；
- 最后一行写 Ready for feedback.，然后停止。
```

---

## 1. 全局执行协议

### 1.1 仓库边界

- `/Users/leeyi/project/imboy.pub` 不是 Git 仓库。
- 后端/产品文档：`/Users/leeyi/project/imboy.pub/imboy`。
- Flutter：`/Users/leeyi/project/imboy.pub/imboyapp`。
- 写文件前必须在目标仓执行 `git rev-parse --show-toplevel`。
- 不修改 `imboy/erlang.mk`、`imboyapp/ios/*`、`imboyapp/macos/*`、`imboyapp/plugin/r_upgrade`。
- 保留用户已有未提交变更；若与任务重叠，停止并报告。
- Web 独立互操作仓、生产环境、外部审计机构不在默认授权范围。

### 1.2 Git 与提交

- 开始任务：记录两个仓库 `HEAD` 与 `git status --short`。
- 一个任务只做该任务列出的安全边界，不顺手重构。
- 跨仓任务分别提交；两个 commit message body 互相记录 counterpart hash。
- Git author/committer 属于联系方式，执行者必须使用**当前会话经用户明确确认**的身份，不能从本文猜测。
- 不 push。生产部署、迁移和灰度属于外向操作，必须另获人工授权。

### 1.3 测试纪律

- 测试必须先在旧实现上证明能捕获缺口；不能只测新 helper 而绕开生产路由。
- Critical 用例不得 `skip`、不得只验证 mock、不得以“本机无环境”标为通过。
- Flutter 功能验证只认可 Android/iOS 真机；模拟器结果只能用于非验收调试。
- 数据库并发/迁移只认可真实 PostgreSQL；纯 mock 不算验收。
- 任何密钥、口令、token、消息明文、完整密文不得进入测试日志或 evidence。

### 1.4 每个任务的通用完成定义

除任务专属验收外，还必须全部满足：

- [ ] `git diff --check` 通过，且 diff 只含任务范围。
- [ ] 新增/修改代码有正向、负向和边界测试。
- [ ] 没有新增 RSA、明文、陈旧密钥或未知协议静默 fallback。
- [ ] 没有 secret/PII 日志；测试 fixture 只含生成的测试数据。
- [ ] Flutter 变更执行 `dart analyze lib` 和任务列出的 `flutter test`。
- [ ] Erlang 变更执行 `make compile`、相关 EUnit；格式化检查执行 `make format-check`。
- [ ] migration 有真实 PostgreSQL up 验证，并提供 down 或前向修复方案。
- [ ] 更新 `docs/e2ee/v2/evidence/E2EE-XXX.md`（目录不存在时创建）。
- [ ] 使用已确认身份提交；提交后目标仓 `git status --short` 无本任务残留。

### 1.5 Evidence 模板

每个任务生成一份不含秘密的证据文件：

```markdown
# E2EE-XXX Evidence

- Task:
- Date:
- Repositories and before/after commits:
- ADR clauses:
- Changed files:
- Tests added first and old behavior reproduced:
- Verification commands:
- Verification result/count/skip count:
- Real device / PostgreSQL environment:
- Security negative cases:
- Secrets/log scan:
- Migration and rollback result:
- Residual risks:
- Reviewer:
- Decision: PASS / FAIL / BLOCKED
```

证据为 `PASS` 才能把任务改为完成；`BLOCKED` 不得伪装成完成。

### 1.6 强制停止条件

出现任一情况立即停止当前 Batch：

1. ADR 依赖仍为 Proposed 且任务会改变 wire/schema/trust model；
2. 用户未确认 Git 身份、生产/第三方操作或破坏性迁移；
3. 目标文件有无法安全合并的用户改动；
4. 同一验收连续 3 次失败且原因未确定；
5. 只能通过降低安全等级、开启 fallback 或跳过测试才能继续；
6. 密码学库/FFI 行为与 ADR 假设不一致；
7. 真机、真实 PostgreSQL、独立实现或外审是验收条件但环境不可用。

---

## 2. 任务状态与 Batch

状态只允许：`Pending / In Progress / PASS / BLOCKED`。执行者完成任务后更新本表和 evidence，但不得自行把人工 Gate 改为 PASS。

| Batch | Tasks | 依赖 | 状态 |
|---|---|---|---|
| B00 | E2EE-000, E2EE-001 | 无 | E2EE-000 PASS / E2EE-001 BLOCKED（ADR 14-19 仍 Proposed，待人工签字） |
| B01 | E2EE-010, E2EE-011 | B00 | PASS（app c326183a/418bdcbd；用户授权 P0 修复；RSA sender cleanup 待后续 pass） |
| B02 | E2EE-012 | B01 | Pending |
| B03 | E2EE-013, E2EE-014 | B01 | E2EE-013 PASS（0b67aade）/ E2EE-014 PASS（后端：waiver 5e845dc0 + migration 3534e503 + logic da1cb358；11 字段 canonical/freshness/event_id 幂等/单调/撤销；imboyapp 无 trust 客户端故非破坏性；客户端 trust UI 独立后续）+ 安全复核硬化 72cef55d（event_id 抢占+版本 TOCTOU 折进 advisory-lock 单事务；21 eunit 绿）+ 残留 C codec 子件 PARTIAL（app 7ae2c2cc：TrustEventCanonicalFields canonical 编码器逐字节对齐后端 golden SHA256=e8fb84b3…+注入式 Ed25519 签名封装；6 单测绿）+ 客户端逻辑补全（app 2556d744：请求体 builder/freshness 预检/§3.2 转换白名单/广播解析，17 单测绿）+ 安全复核#3 HIGH 修复（canonical 非单射：字符串字段值内 \n 使一签名对多字段拆分有效→信任伪造；双端加 \n 守卫，imboy c2bab1e9 eunit22 绿/app ecdc1805 单测26 绿+M1 broadcast to_state 枚举；evidence E2EE-014-C-codec.md/E2EE-014-C-security-review.md）；#1 DB 半程实证 DONE（真 PG eunit trust_audit_repo_integration_tests，连 imboy_v1 非 meck，4/4 绿：inserted/duplicate 幂等/event_id_conflict/version_rollback；imboy d9d7c9dd；绕开 boot 不动 schema）；⚠️ HTTP handler 层+客户端真 Ed25519 验签仍需真机/起 app，cross-signing/§5 仍 BLOCKED |
| B04 | E2EE-015, E2EE-016 | B01 | E2EE-016 PASS（app，backup parser 确定性 notes 布局+大小/迭代/短文件上限+物理 DID 不被覆盖；10k fuzz 绿）/ E2EE-015 实现+单测 PASS（app e3646285，E2eeSecretInventory 前缀清单+purge 复核 fail-closed+loginAfter 闸门）+ 安全复核 CRITICAL 修复 b05f7180（quitLogin 物理收尾不被 purge 失败中断+SqliteService uid 隔离）；真机 logout→重启→换号旅程待人工。S1 安全复核 evidence=E2EE-S1-security-review.md（Critical/High=0，3 Medium 已处置，3 Low 记录） |
| B05 | E2EE-019 | B02–B04 | Pending |
| B06 | E2EE-020, E2EE-021, E2EE-022 | B05 | Pending |
| B07 | E2EE-023, E2EE-024, E2EE-025 | B06 | Pending |
| B08 | E2EE-026, E2EE-027 | B07 | Pending |
| B09 | E2EE-029 | B06–B08 | Pending |
| B10 | E2EE-030, E2EE-031, E2EE-032 | B09 | Pending |
| B11 | E2EE-033, E2EE-034 | B10 | Pending |
| B12 | E2EE-035, E2EE-036 | B10 | Pending |
| B13 | E2EE-039 | B11–B12 | Pending |
| B14 | E2EE-040 | B09 | Pending |
| B15 | E2EE-041 | E2EE-040=PASS/Go | Pending |
| B16 | E2EE-042, E2EE-043, E2EE-044 | B15 + E2EE-030/032/034 | Pending |
| B17 | E2EE-045, E2EE-046 | B16 | Pending |
| B18 | E2EE-049 | B17 | Pending |
| B19 | E2EE-050 | E2EE-039；Top-Tier 审计另需 E2EE-049 | Pending |
| B20 | E2EE-051, E2EE-052 | B19 + 外审结果 | Pending |
| B21 | E2EE-053 | B20 | Pending |
| B22 | E2EE-054 | B21 | Pending |

### 2.1 B13 后的发布分叉

- **先发布 GA-C2C**：E2EE-039 PASS 后，经用户选择可直接执行 B19→B22，S5 scope=`c2c`；B14–B18 保持 Pending。
- **继续冲刺 GA-Top-Tier**：执行 B14→B18，再执行 B19→B22，S5 scope=`top-tier`。
- 已完成 C2C GA 后继续 MLS：B14→B18 完成后，必须以 scope=`top-tier` **重新执行** E2EE-050..054，不能复用 C2C 外审/红队/灰度结论。
- E2EE-050..054 的 evidence 分别命名为 `E2EE-XXX-c2c.md` 或 `E2EE-XXX-top-tier.md`；任务状态按 scope 独立记录。

执行者不得替用户选择轨道。选择影响外审范围、时间和发布声明，必须在 Batch 报告后由用户明确决定。

---

## 3. B00 — 基线与人工架构门

### E2EE-000 — 可复现基线与证据目录

**性质**：只读核查 + 文档；不修改产品代码。

**实施**

1. 记录 `imboy`、`imboyapp` 的 HEAD、branch、status、远端差异。
2. 用 `rg` 重新定位 ADR 20 §1.2 的每个 Gap，记录符号和行号，不照抄旧 handoff。
3. 运行当前 E2EE 定向测试、`dart analyze lib`、`make compile`、`make eunit`，记录既有失败。
4. 创建 `docs/e2ee/v2/evidence/E2EE-000.md`，不得修复发现。

**验收标准**

- [ ] 两仓 HEAD/status 和所有 Gap 的当前代码证据齐全。
- [ ] 每条 Gap 至少关联一个现有或待新增测试文件。
- [ ] 基线命令、退出码、通过/失败/skip 数量写入 evidence。
- [ ] evidence 不含 token、手机号、生产地址、密钥或消息内容。
- [ ] 除 evidence 和本任务状态外无文件变化。

**停止点**：若代码事实已使 ADR 14–20 的关键假设失效，标记 BLOCKED 并先提 superseding amendment。

### E2EE-001 — ADR 14–19 人工接受与治理更新

**性质**：人工 Gate；执行者不得代签。

**实施**

1. 汇总 ADR 14–19 的未决问题、范围、迁移破坏和产品文案影响。
2. 等待产品、移动端、后端、安全、发布负责人明确接受或提出修改。
3. 仅在明确接受后，把 ADR 14–19 状态改为 Accepted，给 00/03–09/11/13 加精确 superseded-in-part 标记并更新依赖图。

**验收标准**

- [ ] 五类 owner 的决定、日期和适用 ADR 写入 evidence；无执行者冒充签字。
- [ ] 每个旧 ADR 只标记被替代章节，保留的 legacy/decrypt-only 内容没有误删。
- [ ] `rg -n '状态.*Proposed' docs/e2ee/v2/{14,15,16,17,18,19}-*.md` 零命中。
- [ ] 00 Freeze Gate、10 dependency graph、11 compatibility matrix 与新决策一致。
- [ ] `git diff --check` 通过，Markdown 链接无断链。

**停止点**：任一 owner 未接受则 BLOCKED。P0 漏洞修复可另获用户授权执行，但不得实施 PFv3、cross-signing、Recovery v2 或 MLS wire/schema。

---

## 4. B01–B05 — S1 P0 Closure

### E2EE-010 — Policy Gate fail-closed

**依赖**：E2EE-001 PASS，或用户明确授权作为现有安全漏洞修复。

**范围**：`EncryptionModeService`、`E2EEService`、`GroupSessionService`、`ComplianceKeyService` 及定向测试。

**实施**

1. 先新增未初始化、加载超时、异常、离线过期和模式切换测试，证明旧路径会继续发送或漏加审计接收方。
2. 建立单一 `PolicyGate.requireReadyForSend()` 或等价不可绕过入口。
3. Strict/Compliance 所有非 valid 状态返回 typed security error；Optional 只能走显式用户确认路径。

**验收标准**

- [ ] `CB-01/02/09/10` 对应自动化测试通过。
- [ ] Policy 未初始化、异常、超时、过期时，网络 send/encrypt 调用次数均为 0。
- [ ] Compliance 有效时审计 entry 必有；无效时整条拒发，不用 stale cache。
- [ ] Optional 明文 wire 和本地记录均含 `unencrypted=true`，UI 测试显示明确警告。
- [ ] `dart analyze lib` 和新增定向 `flutter test` 全绿、0 skip。

**提交建议**：`fix(e2ee): enforce fail-closed policy gate`

### E2EE-011 — 禁止 room-key Olm→RSA 静默降级

**依赖**：E2EE-010。

**范围**：`imboyapp/lib/service/group_session_service.dart`、协议版本解析、测试。

**实施**

1. 在真实 `handleRoomKeyMessage` 路径覆盖删除 `olm`、空 `sid`、伪造 `sid`、Olm 解密失败和保留 `ek` 的攻击。
2. v3/Strict 精确选择一次 Olm，失败直接拒绝；RSA unwrap 仅允许明确 legacy meta 版本。
3. 保留 legacy fixture 的 decrypt-only 能力，禁止用于新生成 room key。

**验收标准**

- [ ] `PF3-02` 通过；ADR 13 T-13-08 拆为“v3 Strict 拒绝”与“明确 legacy decrypt-only”两组测试。
- [ ] 上述 5 类攻击中 RSA unwrap mock/spy 调用次数均为 0。
- [ ] legacy v1/v2 固定 fixture 仍能 RSA decrypt，且测试证明 encrypt/build 路径不生成新 RSA wrap。
- [ ] 入站失败不落 plaintext、不创建 group session、不更新消息为成功。
- [ ] `flutter test test/service/group_session_service_test.dart` 和相关 E2EE tests 全绿。

**提交建议**：`fix(e2ee): remove strict room-key rsa fallback`

### E2EE-012 — 首条 Protected Context 纵向闭环

**依赖**：E2EE-010/011；ADR 15 Accepted。

**范围**：canonical header 最小实现、实际消息发送/接收路由、mutation tests；不得另造临时 wire 格式。

**实施**

1. 实现 ADR 15 的 canonical CBOR header bytes 与 `header_hash`，只暴露 typed builder/parser。
2. Olm/Megolm 加密的 inner frame 同写 header；AES-GCM 路径绑定 AAD。
3. 接收端在展示/落库前比较 inner/outer header。
4. 为所有路由字段建立 mutation matrix。

**验收标准**

- [ ] `PF3-01/03` 全部通过，覆盖 `id/from/to/type/msg_type/gid/sender_did/session_id/created_at`。
- [ ] 测试进入 `chat_network_service -> E2EEService -> MessageService` 生产路径，不只调用 codec helper。
- [ ] A→B 密文复制到 A→C、群 X→Y 均在落库/展示前拒绝。
- [ ] golden header bytes/hash 在两次独立进程运行中完全一致。

**提交建议**：`feat(e2ee): bind message context in protected frame`

### E2EE-013 — Crypto API 设备所有权热修复

**依赖**：E2EE-001，或用户明确授权的漏洞修复。

**范围**：后端登录/刷新认证上下文、`auth_ds.erl`、`olm_handler.erl`、Olm logic/repo、API tests。

**实施**

1. 登录或设备注册时把 DID 绑定到认证上下文；crypto 写 API 只信认证上下文。
2. body/query DID 只能作为一致性校验，不能覆盖 auth DID。
3. legacy 无 DID token 对 crypto 写操作 fail-closed；读取权限保持最小兼容。

**验收标准**

- [ ] `DT-01/02` 通过：A token 操作 B DID 返回 403，DB 无变化。
- [ ] body DID 缺失、空值、重复字段、超长、Unicode 混淆均不能绕过。
- [ ] 已撤销/禁用设备不能上传 identity/OTK/fallback/capabilities。
- [ ] handler、logic、repo 分层不反向依赖；SQL 全参数化。
- [ ] `make compile`、相关 EUnit、`make format-check` 全绿。

**提交建议**：`fix(auth): bind crypto writes to authenticated device`

### E2EE-014 — Trust Event freshness、唯一性与幂等

**依赖**：E2EE-013。

**范围**：下一可用 PostgreSQL migration、`e2ee_trust_logic.erl`、repo/ds/API、tests。

**实施**

1. 加 `event_id` 唯一约束、`issued_at/expires_at`、actor session/device generation、target identity version。
2. 签名覆盖所有新增字段；同 event id 重放返回原语义结果。
3. 拒绝过期、未来超窗、target version 回退和 revoked actor。

**验收标准**

- [ ] `DT-03/04/08` 全部通过。
- [ ] 同 event id 并发重放 100 次只产生一条审计事件和一次状态变化。
- [ ] 不同 event id 的非法状态转换仍拒绝，不能靠幂等绕过状态机。
- [ ] migration 在真实 PostgreSQL 完成 up、重复 up 行为确认、down/前向修复演练。
- [ ] 时间窗边界 `-1/0/+1 ms` 有测试；服务端错误不泄漏签名 oracle 细节。

**提交建议**：`fix(e2ee): reject replayed and stale trust events`

### E2EE-015 — E2EE Secret Inventory 与 logout 清理

**依赖**：E2EE-010。

**范围**：`user_repo_local.dart`、`storage_secure.dart`、所有 E2EE stores/cache、logout tests。

**实施**

1. 建立可测试的 `E2eeSecretInventory`，枚举 RSA、Olm account/pickle key/session、Megolm、MLS 预留、backup/recovery、SQLCipher key、compliance cache。
2. logout/account switch 经单一 orchestrator 清理；任何清理失败阻止切入另一账号。
3. 清除内存 cache、持久化 store key、临时文件和后台任务句柄。

**验收标准**

- [ ] logout 前为每类 secret 写测试 canary，logout 后逐项读取为 null/不存在。
- [ ] 无旧 key 时旧 SQLCipher DB 无法打开；新账号查不到旧 crypto rows。
- [ ] kill/restart 后清理结论保持，后台 isolate 不重新写回旧 secret。
- [ ] logout 失败显示安全错误且不建立新账号会话。
- [ ] Android/iOS 真机各完成 logout→重启→换号旅程；模拟器不计入 PASS。

**提交建议**：`fix(e2ee): purge complete secret inventory on logout`

### E2EE-016 — Legacy backup parser 边界与 DID 安全

**依赖**：E2EE-010。

**范围**：`e2ee_local_backup_service.dart`、v1 fixture、fuzz/resource tests。

**实施**

1. 统一 notes 的 writer/reader 布局，明确长度字段位置和字节序。
2. 在 `readAsBytes`、KDF 和大分配前验证文件/KDF/字段上限。
3. v1 restore 的 DID 只进入 archived metadata，不覆盖当前物理 DID。

**验收标准**

- [ ] `RV2-03/04/05/06` 的 legacy 子集通过。
- [ ] notes 长度 0、1、上限，截断、尾部垃圾、错误 UTF-8 均确定性处理。
- [ ] 64 MiB+1、极端迭代/长度值在 KDF/大分配前拒绝。
- [ ] 10,000 个固定 seed fuzz 样本无 crash、hang、OOM、部分 secret 落盘。
- [ ] 恢复后当前 DID 与恢复前新设备 DID 相同，未被备份 DID 覆盖。

**提交建议**：`fix(backup): bound legacy parser and preserve physical did`

### E2EE-019 — G1 安全出口

**依赖**：E2EE-010..016 全 PASS。

**性质**：验证/证据，不新增功能。

**实施**

1. 在两仓 clean HEAD 重跑 S1 全部测试、静态分析、后端 EUnit。
2. 执行真机 fail-closed、room-key 攻击、logout、backup 恢复旅程。
3. 安全 reviewer 对 S1 commits 做范围审查并记录 findings。

**验收标准**

- [ ] E2EE-010..016 evidence 全为 PASS，Critical 0 skip。
- [ ] `dart analyze lib`、S1 相关 Flutter tests、`make compile/eunit/format-check` 全绿。
- [ ] Android/iOS 真机矩阵均有设备型号、OS、commit 和结果。
- [ ] reviewer 的 Critical/High finding 为 0；Medium 有 owner/期限。
- [ ] 产品等级仍为 Preview，未因 G1 通过提前宣传 Strong/GA。

**提交建议**：`docs(e2ee): record g1 security gate evidence`

---

## 5. B06–B09 — S2 认证消息与事务状态

### E2EE-020 — Protected Frame v3 严格 codec

**依赖**：E2EE-019；ADR 15 Accepted。

**范围**：Flutter typed model、deterministic CBOR codec、golden fixtures、resource parser；不接业务路由。

**实施**

1. 复核 E2EE-012 选用的 CBOR 实现，记录版本/许可证；若不能满足严格解析和跨语言一致性，在本任务内替换，禁止自研密码学原语。
2. 实现 ADR 15 全部 header/inner/outer 类型、canonical bytes、hash、枚举和资源上限。
3. 使用第二个独立实现生成/校验 fixtures，覆盖跨语言 TSID 文本语义。

**验收标准**

- [ ] `PF3-05/06/08/09` 全部通过。
- [ ] 重复 map key、非最短整数、indefinite length、深度 17、header 8 KiB+1 均在解密前拒绝。
- [ ] Flutter 与独立实现对全部 fixture 的 bytes/hash 逐字节一致。
- [ ] parser 对 10,000 个固定 seed 输入无 crash/hang/OOM。
- [ ] 未知 critical 字段/版本拒绝；未知非关键扩展只按 ADR 明确规则处理，不猜测 fallback。

**提交建议**：`feat(e2ee): add strict protected frame v3 codec`

### E2EE-021 — Protected Frame 全发送/接收路由

**依赖**：E2EE-020。

**范围**：`E2eeSessionProtocol`、Registry、chat send、message receive、Olm/Megolm、历史解析。

**实施**

1. 让 protocol encrypt/decrypt 强制接收 typed protected context，业务层无法传空 map 绕过。
2. 所有新 E2EE 写入使用 PFv3；接收只执行声明协议一次。
3. 只有明确 `meta_version<3` 进入 legacy decrypt-only parser。
4. 只使用验证后的 inner header 展示/落库。

**验收标准**

- [ ] `PF3-01..03/05..09` 全部在生产路由通过，Critical 0 skip；PF3-04/10 明确由 E2EE-027 验收。
- [ ] v3 Olm 失败不试 Megolm/RSA；v3 Megolm 失败不试 RSA。
- [ ] legacy v1/v2 固定 fixtures 仍可读，但所有 legacy encrypt API 抛 typed error。
- [ ] outer header 与 inner header 任一字段不同都不产生可见消息/成功 ACK。
- [ ] `rg` 证明业务层无 suite 字符串 if/else 路由和隐式 fallback。

**提交建议**：`feat(e2ee): route all new messages through protected frame v3`

### E2EE-022 — 后端 PFv3 不透明透传契约

**依赖**：E2EE-020；可与 E2EE-021 同 Batch 协作，但独立提交。

**范围**：Erlang message HTTP/WS handler/logic、schema validation、integration fixtures。

**实施**

1. 只验证外层大小、类型、必填 version；不解码 inner payload、不重建 canonical header。
2. HTTP、WS、DB jsonb round-trip 原样保持 PFv3 未知扩展和 base64url bytes。
3. 加 10 MiB、字段长度和速率边界，避免未授权大分配。

**验收标准**

- [ ] Flutter fixture 经 HTTP→DB→WS 后 `protected_header/ciphertext/header_hash` 完全相同。
- [ ] 未知非关键字段不被裁剪；未知 critical version 按契约拒绝。
- [ ] 超限 payload 在 DB 写入和广播前返回稳定错误。
- [ ] `rg`/守护测试证明服务端无 E2EE content decrypt/private key 逻辑。
- [ ] `make compile`、EUnit、真实 PostgreSQL pipeline integration 全绿。

**提交建议**：`feat(e2ee): preserve protected frame v3 envelope`

### E2EE-023 — 附件独立密钥与分块 AEAD

**依赖**：E2EE-021/022。

**范围**：附件加密 service、上传/下载、PFv3 descriptor、临时文件管理、测试。

**实施**

1. 先冻结 attachment crypto profile：AEAD、key/nonce 长度、chunk nonce 唯一派生、AAD 字节格式、最大块数；由安全 reviewer 接受后编码。
2. 每附件生成独立 256-bit content key 和随机 base nonce。
3. descriptor 全部进入 inner payload；每块 AAD 绑定 header/attachment/index/count。
4. 下载完成验证 tag、顺序、总数、大小和明文 hash 后才交给预览器。
5. 异常/取消/磁盘满时清理临时明文；Garage URL 仍走 `AssetsService.viewUrl`。

**验收标准**

- [ ] `ATT-01..05` 全部通过。
- [ ] crypto profile 无 `TBD`，含 nonce 唯一性论证和跨平台 golden vector，并有安全 reviewer 记录。
- [ ] 对象替换、块交换/删除/重复/截断、descriptor 篡改均不能产生可打开文件。
- [ ] 同一附件重复上传也使用不同 key/nonce/ciphertext。
- [ ] 获得 Garage 原始对象的未授权测试方只能看到密文。
- [ ] Android/iOS 真机覆盖小文件、超过一块的大文件、取消、磁盘不足旅程。

**提交建议**：`feat(e2ee): encrypt attachments with bound chunk aead`

### E2EE-024 — C2C per-device Olm fan-out

**依赖**：E2EE-021、E2EE-013。

**范围**：outbound router、device list、Olm session、自己的其他设备同步、测试。

**实施**

1. 为对端每个 active/non-revoked device 和发送者其他设备独立选择/创建 Olm session。
2. 每设备生成独立 ciphertext/envelope；C2C 新写入禁止 Megolm/RSA。
3. identity version 改变或 device revoked 后旧 session 不再用于新发送。

**验收标准**

- [ ] 2 账号 × 各 3 设备端到端：每个目标 device session/ciphertext 独立。
- [ ] 任何一个目标无安全套件时 Strict 整条拒发，0 个目标收到部分新消息。
- [ ] 撤销设备无法领取新 OTK/密文，其他设备仍正常。
- [ ] identity version 变化触发阻断/核验，不静默信任新 key。
- [ ] `rg` 证明 C2C 新发送没有 Megolm/RSA 路径；legacy 仅 decrypt。

**提交建议**：`feat(e2ee): enable per-device olm for c2c`

### E2EE-025 — OTK/fallback 抗耗尽与幂等租约

**依赖**：E2EE-013/024。

**范围**：后端 OTK claim logic/repo/handler、migration/config、Flutter retry、并发测试。

**实施**

1. 引入 requester/target/identity version/request id 幂等键和有界租约语义。
2. 配置单目标、单请求者、单租户、全局四层限流；硬上限不可被关闭。
3. 重试同 request id 返回同一领取结果；不得让 OTK 回 available 后被第二方重复领取。

**验收标准**

- [ ] `DT-03/09` 及 1000 并发 claim 测试通过，无重复 key。
- [ ] 同 request id 重放 100 次只消费一次；不同攻击 request 触发目标级限流。
- [ ] OTK 耗尽只使用协议允许且身份验证通过的 signed fallback prekey，或拒发。
- [ ] 耗尽/限流绝不触发 RSA/Megolm/明文。
- [ ] 真实 PostgreSQL 并发测试、cleanup/lease 边界和时钟边界全绿。

**提交建议**：`fix(e2ee): harden prekey claims against exhaustion`

### E2EE-026 — Transactional CryptoStore 基础

**依赖**：E2EE-021/024；ADR 14 Accepted。

**范围**：SQLite 下一 schema version、CryptoStore API、per-state actor、加密 at rest、tests。

**实施**

1. 建 `crypto_state/outbox/inbox_dedupe/message_state` schema 与 repository。
2. 每个 Olm/群 state 只允许单 writer actor；业务 service 不直接写 pickle。
3. store transaction 同时更新 state version/hash 和业务状态；跨账号 namespace 隔离。

**验收标准**

- [ ] SQLite migration 从当前版本 up 成功，旧数据读取不丢失；回滚/前向修复说明完整。
- [ ] 并发 100 次更新无 lost update，旧 version compare-and-swap 失败。
- [ ] store 文件和日志不含明文 pickle key/消息；换号无法读取另一 namespace。
- [ ] repository/actor 单元测试使用 SQLite ffi in-memory，另有真机持久化测试。
- [ ] 直接写 pickle 的生产调用经 `rg` 收敛到 CryptoStore 内部允许清单。

**提交建议**：`feat(e2ee): add transactional crypto store`

### E2EE-027 — 原子 outbox/inbox 与故障注入

**依赖**：E2EE-026。

**范围**：send/receive pipeline、immutable outbox、dedupe、ACK、crash harness。

**实施**

1. 加密后原子提交 ratchet state + immutable ciphertext + outbox，再允许网络发送。
2. 网络重发复用同一 ciphertext；不能重新 encrypt。
3. 解密后原子提交新 state + dedupe + message，再展示/ACK。
4. 在每个事务边界加入 test-only kill injection。

**验收标准**

- [ ] `PF3-04/10` 及 10,000 次固定 seed kill/restart 通过。
- [ ] key reuse、state rollback、双重业务提交、已 ACK 消息永久不可解均为 0。
- [ ] 重发 100 次 ciphertext byte-for-byte 相同且 ratchet version 只推进一次。
- [ ] 同一入站消息并发 100 次只展示/落库一次。
- [ ] harness 不打印 secret，随机 seed、commit、平台和失败点写入 evidence。

**提交建议**：`feat(e2ee): commit ratchets with immutable message outbox`

### E2EE-029 — G2 Strong Preview 出口

**依赖**：E2EE-020..027 全 PASS。

**性质**：验证/证据。

**验收标准**

- [ ] PF3-01..10、ATT-01..05、DT-03/09、Olm 多设备、故障注入全绿且 0 skip。
- [ ] 最低受支持 Android/iOS 真机 4 KiB C2C crypto p95≤100 ms、p99≤200 ms。
- [ ] HTTP/WS/DB round-trip 与 Flutter fixtures 一致。
- [ ] 两仓全量相关测试、静态分析、编译、格式检查通过。
- [ ] 安全 reviewer Critical/High=0；产品只允许提升到 Strong Preview，不得 GA-C2C。

**提交建议**：`docs(e2ee): record g2 strong-preview gate evidence`

---

## 6. B10–B13 — S3 身份、透明度与恢复

### E2EE-030 — 可撤销 Device-bound Session 正式化

**依赖**：E2EE-029；ADR 16 Accepted。

**范围**：PostgreSQL session schema、auth middleware/logic/ds、JWT/refresh、Flutter auth client、API contract。

**实施**

1. 建 server-side device session，绑定 uid/did/generation/issued/expires/status/auth_time。
2. JWT 只作声明，写操作同时检查可撤销 session。
3. legacy token 在明确窗口换 token；无法证明 DID 的 token 只能读，不能写 crypto material。

**验收标准**

- [ ] `DT-01..04/08/10` 全部通过。
- [ ] session 撤销后旧 JWT 立即不能上传/claim/trust，不能等 token 自然过期。
- [ ] refresh 不得改变 DID/generation；跨设备 refresh 返回 403。
- [ ] migration 在真实 PostgreSQL 完成新旧 token 并发验证和回滚演练。
- [ ] OpenAPI、Flutter model、Erlang handler 字段/错误码一致。

**提交建议**：`feat(auth): add revocable device-bound sessions`

### E2EE-031 — Account Root 与 Cross-signing 密钥生命周期

**依赖**：E2EE-030；ADR 16/17 Accepted。

**范围**：Flutter key generation/storage、Device Manifest、签名/轮换/重置、纯协议 tests；暂不做 UI。

**实施**

1. 实现 Account Master、device-signing、可选 user-signing 的职责分离。
2. Device Manifest canonical CBOR 同时验证 device/account signatures、version/hash chain。
3. 支持轮换、撤销、全设备丢失后的 root reset；私钥永不上传。

**验收标准**

- [ ] `DT-04/05/06/11/12` 的协议层测试通过。
- [ ] manifest 任一字段改 1 bit、非法 key 长度/点、回滚、并行冲突均拒绝。
- [ ] 服务端替换 identity 并重签 API response 仍因 account signature 失败。
- [ ] root reset 生成新 root，旧 verified 链不继承。
- [ ] secure storage/log/crash scan 无 account/device private key。

**提交建议**：`feat(e2ee): add account-root cross-signing`

### E2EE-032 — 新设备核验、撤销与 Root Reset UX

**依赖**：E2EE-031。

**范围**：QR/近场或扫码流程、设备列表、trust UI、i18n、真机 integration tests。

**实施**

1. 新设备展示待签 manifest hash；旧 verified 设备明确确认后签名。
2. UI 区分 legacy_unverified/verified/revoked/transparency_error/root_reset。
3. 撤销设备和 root reset 显示不可逆影响、历史/未来权限差异。

**验收标准**

- [ ] `DT-05/10/12` 完整用户旅程在两台真实设备通过。
- [ ] 扫错/过期/篡改 QR 不会变 verified；重复扫码幂等。
- [ ] 撤销后目标设备无法获得新消息/群秘密，其他设备收到可见安全事件。
- [ ] root reset 后联系人看到强告警，不能一键静默接受。
- [ ] UI 遵循 DESIGN tokens、44pt 触达、暗色模式，10 种语言源文件/生成物一致。

**提交建议**：`feat(e2ee): add verified-device authorization flows`

### E2EE-033 — Key Transparency 日志与 proof API

**依赖**：E2EE-030/031。

**范围**：Erlang/PostgreSQL append-only event、Merkle tree/head/proof、签名键运维接口、tests。

**实施**

1. 先冻结 transparency profile：hash、leaf/node domain separation、空树值、canonical event bytes、tree-head 签名输入、proof wire、signing-key 轮换；由安全 reviewer 接受。
2. leaf 为 canonical account/device/revocation event hash，只追加不删除。
3. 提供 signed tree head、inclusion proof、consistency proof。
4. 分离 log DB 写权限与 tree-head signing key；设计受控轮换。

**验收标准**

- [ ] `DT-05/06/07` 后端部分通过；标准 Merkle fixture inclusion/consistency 100% 通过。
- [ ] transparency profile 无 `TBD`，domain separation 与签名输入有跨实现 golden vector。
- [ ] 修改/删除/重排任一历史 leaf 后旧/new proof 无法同时通过。
- [ ] 同 tree size 不同 root 被识别为 split view，不能最后写覆盖。
- [ ] 并发 append 1000 events 得到唯一连续位置，真实 PostgreSQL 无丢事件。
- [ ] 签名私钥不在 DB、repo、日志、API；轮换 runbook 有双签过渡和回滚边界。

**提交建议**：`feat(e2ee): add append-only device transparency log`

### E2EE-034 — Transparency 客户端、gossip 与独立 monitor

**依赖**：E2EE-032/033。

**范围**：Flutter proof verifier/cache、PFv3 tree-head digest、独立 monitor、UI 告警。

**实施**

1. 客户端保存每账号最高 tree size/root，验证 inclusion/consistency。
2. PFv3 内携带最近 tree-head digest 做联系人 gossip。
3. monitor 从独立网络比对 signed tree heads；只处理公开 hash。

**验收标准**

- [ ] `DT-05..07` 客户端端到端通过。
- [ ] rollback tree size、错误 proof、同 size 异 root、过期 signing key 均阻断新设备信任。
- [ ] 两客户端收到 split view 后在下一次 gossip 检出并留存无 secret 证据。
- [ ] monitor 人工演练成功告警；monitor 停止时客户端本地 proof 仍 fail-closed。
- [ ] cache 换号隔离，logout 后 signing/trust cache 按 inventory 清理。

**提交建议**：`feat(e2ee): verify and gossip transparency heads`

### E2EE-035 — Recovery Vault v2 codec、KDF 与两档策略

**依赖**：E2EE-031；ADR 17 Accepted。

**范围**：Vault v2 CBOR/AEAD、random Recovery Key、Argon2id、resource parser、tests。

**实施**

1. 先冻结 vault crypto profile：AEAD、nonce、HKDF labels、Recovery Key 编码/checksum、Argon2id 校准/边界和错误语义；由安全 reviewer 接受。
2. 实现 identity_only 默认和 history_recoverable opt-in，清单明确材料范围。
3. 默认 256-bit Recovery Key；口令模式按最低真机基准选择有界 Argon2id 参数。
4. header 作 AAD，先验证 magic/version/length/KDF bounds 再大分配/KDF。

**验收标准**

- [ ] `RV2-02A/03..08/10` 全部通过。
- [ ] vault crypto profile 无 `TBD`，Flutter 与独立实现 golden vector 一致。
- [ ] identity_only 包内扫描不到消息/附件历史 key。
- [ ] header/KDF/length/ciphertext/manifest 任一篡改无部分导入。
- [ ] 64 MiB+1、10^6 entries、极端 Argon2 参数在 KDF/大分配前拒绝。
- [ ] Android/iOS 最低真机 KDF 0.5–2s，峰值内存和参数写入 evidence。

**提交建议**：`feat(e2ee): add bounded recovery vault v2`

### E2EE-036 — ArchivedCryptoStore 与 v1 一次性导入

**依赖**：E2EE-026/035。

**范围**：只读历史 store、RSA/Olm/Megolm/MLS history adapters、v1 importer、恢复旅程。

**实施**

1. archived namespace 固定 original DID/session/epoch，所有 encrypt/update/claim API 永久不可用。
2. 新设备先生成 fresh DID/Olm/MLS identity，再导入；绝不覆盖全局 DID。
3. v1 包只导入存在的 legacy 材料并显示“不完整历史恢复”。

**验收标准**

- [ ] `RV2-01/02/07..10` 全部通过。
- [ ] 两台设备导入同一 vault 后 DID/活跃 identity/session 全部不同。
- [ ] 历史 fixtures 可按档位解密；archived encrypt/update/claim 均抛 typed error。
- [ ] kill 在导入各阶段要么完整提交要么完全回滚，无半导入 secret。
- [ ] logout/account switch 后 active/archived store key 都不可读取。

**提交建议**：`feat(e2ee): restore history into read-only crypto archive`

### E2EE-039 — G3 GA-C2C 候选出口

**依赖**：E2EE-030..036 全 PASS。

**验收标准**

- [ ] DT-01..12、RV2-01..10（含 02A）、PF3/ATT/Olm 回归全绿，Critical 0 skip。
- [ ] 新设备授权、撤销、root reset、identity-only/history recovery 在 Android/iOS 真机通过。
- [ ] split-view 演练可复现、阻断、留证，独立 monitor 工作正常。
- [ ] 外审状态被明确记录；本 Gate 只产生 `GA-C2C candidate`，必须完成 E2EE-050/051/052 后才可能 GA。
- [ ] Evidence manifest 含 SBOM、依赖 hash、真机、interop、fuzz、crash、迁移和 reviewer。

**提交建议**：`docs(e2ee): record g3 ga-c2c candidate evidence`

---

## 7. B14–B18 — S4 MLS 群聊

### E2EE-040 — MLS 移动端 Go/No-Go Spike

**依赖**：E2EE-029；ADR 19 Accepted。

**性质**：隔离 PoC，不接生产 UI/网络，不修改现有 Megolm 写入。

**范围**：`imboyapp/packages/imboy_crypto` 候选 Rust core、FFI PoC、报告和测试。

**实施**

1. 评估维护中的 Rust MLS 实现：标准覆盖、维护、安全响应、许可证、构建平台和持久化 API。
2. 验证 create/add/update/remove/application、state serialize/restore、iOS/Android ABI。
3. 与另一独立 RFC 9420 实现互操作。
4. 对 FFI/codec 做 100,000 输入 fuzz；测 1/10/100/1000 leaf 性能。

**验收标准**

- [ ] ADR 19 §3 每一 Gate 都有证据和 `Go/No-Go` 结论，不允许“未测视为 Go”。
- [ ] 官方/维护方适用向量 100% 通过，无 IMBoy 私有协议分支。
- [ ] 独立实现双向 create/add/update/remove/application 100% 互操作。
- [ ] iOS/Android 真机 build、serialize/restore、前后台恢复通过。
- [ ] 100,000 fuzz 样本无 crash/UB/OOM/secret log；1000 leaf Commit p95≤2s。

**完成语义**：诚实的 No-Go 报告也可使本任务 PASS，但会阻断 E2EE-041；不得转向自研 MLS。

**提交建议**：`spike(e2ee): evaluate mobile mls core`

### E2EE-041 — MLS Profile 人工冻结

**依赖**：E2EE-040 PASS 且结论为 Go。

**性质**：人工 Gate。

**实施**

1. 根据 Spike 冻结库/版本、cipher suite、credential、group id、wire、padding、epoch retention、Update 阈值和 FFI API。
2. 安全 reviewer 复核 ADR 19 与 RFC 9420/9750 偏差。
3. 产品确认 Megolm→MLS 的用户可见语义和回滚边界。

**验收标准**

- [ ] 所有 profile 参数有唯一值，不留 `TBD/任选/运行时猜测`。
- [ ] 每个偏离 RFC 默认的决定有威胁、理由和测试 ID。
- [ ] 依赖版本和 lock hash 固定，许可证/供应链 review 通过。
- [ ] 移动、后端、安全、发布 owner 明确签字。
- [ ] No-Go/未签字时后续任务保持 BLOCKED。

**提交建议**：`docs(e2ee): freeze imboy mls profile`

### E2EE-042 — Rust MLS core 与窄 FFI

**依赖**：E2EE-041。

**范围**：Rust core、Dart bridge、secret zeroization/error mapping、unit/vector/fuzz tests。

**实施**

1. Rust core 负责 RFC 9420 codec/key schedule/state；Dart 只传 typed bytes/opaque handles。
2. FFI 不接受网络对象/map，不返回 secret tree/裸私钥。
3. 状态序列化由 CryptoStore 加密；错误映射稳定且无 oracle 细节。

**验收标准**

- [ ] `MLS-01/02/12` 全部通过。
- [ ] Dart 无 tree math、key schedule、自研 AEAD/KDF；`rg` 守护测试固定边界。
- [ ] FFI malformed length/null/invalid enum/panic 边界均返回 typed error，不 unwind 跨 FFI。
- [ ] secret buffer 生命周期/zeroize 有测试或审计证据，日志无 secret。
- [ ] 所有目标 Android ABI 与 iOS 真机构建、启动、升级通过，未修改保留区文件。

**提交建议**：`feat(e2ee): add audited mls core bridge`

### E2EE-043 — MLS Delivery Service API

**依赖**：E2EE-030/041。

**范围**：Erlang handler→logic→ds→repo、PostgreSQL migrations、OpenAPI、integration tests。

**实施**

1. 加 publish/claim KeyPackage、append/fetch ordered handshake、targeted Welcome、application ciphertext、delivery cursor。
2. 所有写操作 device-bound、签名、幂等、有界；服务端不生成 Commit/leaf secret。
3. KeyPackage 一次性消费；限制对象大小、pending proposals、epoch gap、发布/claim 速率和保留期。

**验收标准**

- [ ] `MLS-08/11` 后端攻击测试通过。
- [ ] 1000 并发 claim 不重复 KeyPackage；同 request id 幂等。
- [ ] 非目标设备无法获取 Welcome；撤销设备所有 MLS 写/claim 返回拒绝。
- [ ] 乱序/重复 handshake 按 `(group,epoch,sequence,idempotency)` 契约确定处理。
- [ ] `rg`/测试证明 server 不含 MLS decrypt、Commit 生成、group secret/private key。
- [ ] migration 真实 PostgreSQL up/down/并发/保留期 cleanup 全绿。

**提交建议**：`feat(e2ee): add protocol-agnostic mls delivery service`

### E2EE-044 — MLS 群状态与业务成员授权

**依赖**：E2EE-026/027/032/034/042/043。

**范围**：Flutter group service、credential validation、Proposal/Commit、CryptoStore/outbox、群 UI。

**实施**

1. 一个物理设备映射一个 leaf；credential 绑定 verified Device Manifest/proof。
2. 群成员/设备增删产生经业务授权和成员签名的 Proposal/Commit。
3. Commit/application state 接入 CryptoStore 原子事务；epoch fork/gap fail-closed。

**验收标准**

- [ ] `MLS-03..09/13` 全部通过。
- [ ] 新 leaf 不能解密加入前内容；移除 leaf 不能解密 Commit 后内容。
- [ ] 服务端伪造成员/credential/Welcome/group id 均验证失败。
- [ ] replay/reorder/delay/duplicate/fork 不导致重复业务提交、epoch rollback 或最后写覆盖。
- [ ] 10,000 次 Commit kill injection 无 key reuse、state/outbox 分裂或永久不可恢复分叉。

**提交建议**：`feat(e2ee): integrate mls group state transactionally`

### E2EE-045 — Megolm→MLS 可见迁移

**依赖**：E2EE-044。

**范围**：capability/feature flag、新群灰度、既有群 migration event、历史 decrypt-only、UI/runbook。

**实施**

1. 先仅新建测试群启用 MLS；既有群创建随机 MLS group 和受认证 migration event。
2. 达到成员确认门槛后停止新 Megolm 写入；Megolm 只读历史。
3. 紧急停止只暂停 MLS 新写入/修复同一 state，不静默回写 Megolm。

**验收标准**

- [ ] 迁移事件绑定旧 gid/session 与新 group/epoch，篡改任一字段失败。
- [ ] 迁移后所有新消息为 MLS；历史 Megolm 仍 decrypt-only。
- [ ] 未确认/旧客户端行为符合兼容矩阵并有明确升级/阻断 UI。
- [ ] 回滚演练未开启 Strict 的 Megolm/RSA/明文静默 fallback。
- [ ] Android/iOS 真机覆盖新群、既有群、离线成员、撤销设备和紧急停止。

**提交建议**：`feat(e2ee): migrate group writes from megolm to mls`

### E2EE-046 — MLS 互操作、性能与对抗总矩阵

**依赖**：E2EE-042..045。

**性质**：测试/优化/证据；性能优化不得改变协议语义。

**实施**

1. 重跑 RFC/维护方向量和独立实现互操作。
2. 执行 1/10/100/1000 leaf 真机基准、弱网、前后台、升级、fork、DoS、fuzz。
3. 执行 Compliance 约束验证；不得新增自定义 group secret export。

**验收标准**

- [ ] `MLS-01..14` 全部通过，Critical 0 skip。
- [ ] 4 KiB application crypto p95≤50 ms，1000 leaf Commit p95≤2s；峰值内存归档。
- [ ] 独立实现互操作 100%，fixture/vector hash 固定。
- [ ] 100,000 FFI/codec fuzz 和 10,000 crash injection 无安全失败。
- [ ] Compliance 只使用 ADR 18 批准的明确审计 leaf/model，无 secret export 旁路。

**提交建议**：`test(e2ee): complete mls security and interop matrix`

### E2EE-049 — G4 GA-Top-Tier 候选出口

**依赖**：E2EE-041..046 全 PASS。

**验收标准**

- [ ] MLS-01..14、迁移、真机、性能、fuzz、crash evidence 完整且可复跑。
- [ ] Megolm 对新 Strict 群为 0 写入，仅历史 decrypt-only。
- [ ] Delivery Service 零 content crypto/private key 守护通过。
- [ ] 紧急停止、状态恢复、credential/root rotation runbook 演练通过。
- [ ] 仍只标 `GA-Top-Tier candidate`，未通过 S5 外审不得 GA。

**提交建议**：`docs(e2ee): record g4 mls candidate evidence`

---

## 8. B19–B22 — S5 独立验证与发布

### E2EE-050 — 外部审计证据包与授权门

**依赖**：C2C 审计只需 E2EE-039；Top-Tier/MLS 审计还必须 E2EE-049。

**性质**：准备可交付材料；向第三方发送前必须人工确认机构、范围、联系人和资料。

**实施**

1. 固定本次审计范围的 commits、构建依赖/SBOM、ADR/profile、测试/interop/fuzz/crash、数据流和威胁模型。
2. 生成最小复现环境与无生产数据 fixtures。
3. 列明审计范围、已知风险、out-of-scope 和报告披露规则。

**验收标准**

- [ ] 审计方能从 clean checkout 重建并运行 Critical tests。
- [ ] evidence 的 commit/hash/SBOM 与候选二进制一致。
- [ ] secret/PII scan 通过，包内无 `.env`、token、生产地址、用户数据或签名私钥。
- [ ] C2C 审计至少覆盖 Protected Frame、Olm、identity/transparency、Recovery、Compliance、storage/logout；只有申请 Top-Tier 时 MLS/FFI/群迁移才可缺一不可。
- [ ] 用户已在当前会话明确确认审计机构、联系人、发送内容和发送动作；否则只生成本地包并停止。

**提交建议**：`docs(e2ee): prepare external audit evidence pack`

### E2EE-051 — 外审发现修复闭环

**依赖**：E2EE-050 + 已收到正式 findings。

**实施**

1. 每个 finding 单独任务/提交，先加入复现测试，再做最小修复。
2. 重新运行受影响任务和所有 Critical 回归。
3. 由审计方或独立 reviewer 复核关闭。

**验收标准**

- [ ] 每个 finding 有 ID、严重级别、根因、复现测试、修复 commit 和复核结果。
- [ ] Critical/High 未修复为 0。
- [ ] Medium 有明确 owner、期限和具名风险接受人；Low 有跟踪项。
- [ ] 修复没有新增 fallback、协议分叉或关闭原测试。
- [ ] 完整 Critical suite 0 skip，候选 evidence/SBOM 重生成。

**提交建议**：每个 finding 单独 `fix(e2ee): remediate audit finding <id>`。

### E2EE-052 — 独立红队攻击矩阵

**依赖**：E2EE-051。

**范围**：ADR 20 §10.2 十类攻击；不得使用生产用户数据。

**验收标准**

- [ ] 服务端 identity/capability/proof 替换被阻断或产生强告警。
- [ ] Olm/MLS 删除诱降、跨会话复制、OTK耗尽、Trust replay 全部失败。
- [ ] Vault parser/KDF DoS、MLS fork/rollback、logout 残留、Compliance 替换全部满足 ADR。
- [ ] Critical/High 红队 finding 为 0；其他 finding 进入 E2EE-051 同等闭环。
- [ ] 报告含工具、commit、seed、环境、实际结果，不含 exploit secret/生产凭证。

**提交建议**：`test(e2ee): record independent red-team matrix`

### E2EE-053 — Canary 灰度与自动停止

**依赖**：E2EE-051/052；生产执行需用户单独明确授权。

**实施**

1. 本地/staging 先演练 `dogfood→1%→5%→25%→100% 新会话→旧会话迁移`。
2. 遥测只收协议版本、错误分类、耗时、状态事件，不收明文/密钥/完整密文。
3. 配置自动停止：认证篡改接受、key reuse、state rollback、跨成员泄密、无法安全回滚任一 >0。

**验收标准**

- [ ] staging 每级达到预设事件量和稳定窗，停止/恢复/回滚演练通过。
- [ ] 自动注入五类 stop signal 均能停止新写入并告警。
- [ ] 遥测 schema 的 secret/PII review 通过。
- [ ] 回滚只停止新协议或恢复同一安全 state，不开启不安全 fallback。
- [ ] 用户在当前会话明确授权具体生产环境、比例、时间窗和回滚负责人后，才可执行生产灰度。

**提交建议**：`ops(e2ee): add guarded canary and rollback runbook`

### E2EE-054 — 最终 GA 人工发布门

**依赖**：E2EE-053 PASS。

**性质**：人工决策；执行者只能汇总证据。

**验收标准**

- [ ] ADR 14 G5 的机密性、附件、完整性、身份、FS/PCS、群成员、恢复、可靠性、互操作、性能、审计、运维全部 PASS。
- [ ] 所有任务 evidence 可追溯到固定 commit，Critical tests 0 skip。
- [ ] 外审/红队 Critical/High=0，Medium 风险接受有效。
- [ ] GA-C2C 与 GA-Top-Tier 分别按实际范围批准；未完成 MLS 不得批准 Top-Tier。
- [ ] 产品安全说明明确不覆盖的通话、推送、索引、导出和流量元数据。
- [ ] 产品、安全、移动、后端、发布负责人显式签字；无人签字时状态保持 Candidate。

**提交建议**：`docs(e2ee): record final ga security decision`

---

## 9. 最低验证命令矩阵

任务卡中的命令是下限，执行者需按实际文件补充更窄或更广测试。

### Flutter

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp
dart format --output=none --set-exit-if-changed lib test
dart analyze lib
flutter test test/service/e2ee/
flutter test test/service/group_session_service_test.dart
flutter test test/service/e2ee_backup_restore_test.dart
```

真机命令中的设备 ID、账号和密码不得写入仓库/evidence；从当前人工授权的安全环境传入。禁止把模拟器结果作为真机验收。

### Erlang

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make compile
make eunit
make format-check
```

涉及迁移、claim、日志并发的任务还必须在隔离的真实 PostgreSQL 测试库运行集成测试；禁止指向生产库。

### 文档与秘密扫描

```bash
cd /Users/leeyi/project/imboy.pub/imboy
git diff --check
git status --short
```

提交时仓库 hook/gitleaks 必须通过；扫描结果只能证明已知模式未命中，不能替代人工 secret review。

---

## 10. Batch 完成报告模板

```markdown
## Batch BXX result

- Tasks: E2EE-XXX ...
- Result: PASS / BLOCKED
- Backend commit(s):
- App commit(s):
- Changed files:
- Acceptance commands and exact result:
- Critical tests: passed / failed / skipped
- Real device / PostgreSQL / interop evidence:
- Security reviewer findings:
- Residual risks:
- Next eligible batch:

Ready for feedback.
```

报告后必须停止。用户确认继续前，不自动进入下一 Batch。
