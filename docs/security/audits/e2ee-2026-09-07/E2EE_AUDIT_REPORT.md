# IMBoy E2EE 安全审计报告（唯一事实源）

原审计日期：2026-09-07

历史 Base C 级重验：2026-09-09（LT-02-C；已被下述 2026-09-11 CURRENT-HEAD OVERRIDE 覆盖，仅作历史证据）

状态：2026-09-11 AI 明文身份门、C2G staging 权威快照、群聊附件 generation ACL 和 `/msg/offline` 旧世代时间线过滤均已完成本地修复；更新后的 migration 108/109 与群历史真库套件因本地 PostgreSQL `econnrefused` 未完成重放。F/R/D/M 与 AI-ID 的用户决策工件仍缺失，旧客户端 rollout、historical room-key grant、备份 epoch metadata 和 A 级攻击复测仍未闭环。当前结论为 `LOCAL_SECURITY_GATE_FAIL / DECISION_EVIDENCE_MISSING / A_LEVEL_ATTACK_RETEST=BLOCKED / E2EE_RELEASE=NO-GO`。

配套执行清单：[`E2EE_ATTACK_MATRIX.md`](./E2EE_ATTACK_MATRIX.md)；群历史决策包：[`docs/planning/e2ee-2026-012-group-history-decision-brief-2026-09-09.md`](../../../planning/e2ee-2026-012-group-history-decision-brief-2026-09-09.md)

本文件统一承载基线、消息路径、密钥所有权、Findings、修复状态和发布结论。旧报告、注释、测试名称及历史 PASS/GO 均不自动继承。

## 0. 2026-09-11 CURRENT-HEAD OVERRIDE

本节覆盖本文其余章节中所有把 2026-09-09 冻结 Base 描述为“当前”的文字；旧段落继续保留，以便审计历史证据和状态变化。当前隔离工作树基线与未提交修复为：

```text
imboy      HEAD a7d76cc23b17600b246921e36e634bc091cad1f2
           branch work/e2ee-security-20260911
           non-doc patch sha256 34c36731b70f9c653e9ab544cde96c2c38da7026811cce8a521ff3cc751964da
imboyapp   HEAD 8ebe49e355ee66b79386f40739432d0bf34c13df
           detached worktree
           non-doc patch sha256 d2d157e670226bd795aaedb8174ceb73906c2e6c888160cb3631230f758e6447
```

当前结论：

| 门 | 结论 | 当前证据与边界 |
|---|---|---|
| 本地实现与回归 | `LOCAL_SECURITY_GATE_FAIL` | C2G staging/DS/Logic/Agent 定向 **91/91 PASS**（35+15+25+16）：生产调用传整数 GID 与最低角色，事务内固化 `conv_seq` 和 active recipient snapshot，worker claim 搬运该 seq，旧列表调用 fail-closed；本轮 15 个纯本地后端模块合计 **324/324 PASS**，App 附件 wiring **28/28 PASS**、上传 API + room-key **36/36 PASS**、10 个改动项 scoped analyze 零问题。migration 108 已为聊天附件建立 `anchor_msg_id -> anchor_conv_seq`，独立 `group_file` 保持当前成员共享语义；migration 109 已为离线 timeline 固化 `conv_seq`，list/count 共用当前 open generation 下界并拒绝 NULL legacy 行。更新后的真库套件因 PostgreSQL `econnrefused` 中止，旧客户端 rollout 与 room-key epoch/backup metadata 仍未完成，因此不能把本地单测升级为安全门 PASS |
| 决策治理 | `DECISION_EVIDENCE_MISSING` | 历史迁移 101、源码和测试注释曾声称 F2/R2/D3/M1“已批准”，App 注释曾引用 `RR/decisions/AI-ID-2026-09-09.md` 及摘要哈希；2026-09-11 已将这些注释校正为“当前实现、批准证据缺失”，但当前仓库、分支与工作区仍未找到对应用户书面决策工件。实现存在不等于用户已经批准，不得据此标记 `RISK_ACCEPTED` 或 `CLOSED` |
| A 级攻击复测 | `A_LEVEL_ATTACK_RETEST=BLOCKED` | 未执行真实账号、真机、建群/退群/踢人、旧 session、附件对象、room-key grant、抓包/MITM/replay、真实 DB/日志/备份/Object Storage/Push、Keychain/Keystore。现有 B/C 级证据不能替代这些路径 |
| 发布 | `E2EE_RELEASE=NO-GO` | 012 尚缺决策证据、migration 108/109 真库证据、旧客户端强制升级 rollout、room-key epoch/backup metadata 与 A 级生命周期闭环；AI 明文门尚无独立签名身份锚和 A 级恶意服务端复测。完成本节不授权进入 GA |

### 0.1 E2EE-2026-012 当前实现和迁移证据

- 已存在迁移 101 与共享群历史授权实现：首次加入/重入建立 append-only generation；leave/remove/workspace remove 关闭世代；history 与 batch sync 复用 `start_seq` 下界。
- 生产 C2G 的三个发送分支现统一调用 `msg_store_ds:stage/12`，传整数 GID 和最低发送者角色（普通消息 1，`@all` 为 3）。Repo 先取得 `msg_store_seq` 顺序锁，再用同一事务的新 READ COMMITTED statement 重新要求 active group、active sender、角色下界和 active recipients；已提交的撤销必被看见，重叠中的成员变更可线性化为 snapshot 之后发生。集合以 `5000+1` 探针 fail-closed。同一事务同时保存 `to_id=GID`、`to_id_list=committed snapshot` 与 `conv_seq`，成功才把快照返回发送逻辑。查询不再额外锁 caller 行，避免与现有 membership 的 `member→seq` 更新顺序形成死锁。旧 `stage/10,11` 的 C2G UID 列表形状现明确返回 `c2g_group_id_required`，不能绕过该不变量。
- worker 的 `claim_pending/2` 与恢复查询均已选择 `conv_seq`；归档因此搬运 staging 固化值，不再因漏列回退到 `next_conv_seq/1`。`staging_prealloc_test_` 也已从 `SELECT *` 改为经真实 claim 查询取行，防止生产 SELECT 漏列而测试假绿；该更新后的真库用例本轮因 PostgreSQL `econnrefused` 未完成，状态为 `BLOCKED_ENV`，不是 PASS。
- 在线投递、队列、引用持久化、Push、mention、Agent 与 Bot 现都消费同一 committed recipient snapshot。`@all` 展开和普通 mention 会按该快照过滤；非快照 Agent 不再进入身份查询/LLM，支付收款人也必须属于快照；Bot 原有 membership 过滤保持不变。staging 的一般数据库错误统一折为可重试 503，不再静默无 ACK/错误帧。相关四套 EUnit 为 **91/91 PASS**（35+15+25+16），`make compile`、scoped `erlfmt --check` 与 diff check 已通过；最终独立复审结论见后续证据更新。
- room-key 公钥枚举已独立收口：`group_member_keys/2` 不再先物化整群 UID 后在 Logic 判断调用者，而由单条 PostgreSQL statement 同时要求 active group/caller/recipient/device，并直接返回设备公钥。成员和设备条目均以客户端既有上限 `4096` 的 `4097` 探针 fail-closed，超限明确返回 409；授权成功但全群无有效公钥与未授权零行可区分。定向 SQL/DS/Logic/Handler/安全合同为 **60/60 PASS**（1+13+20+20+6），编译、erlfmt 与 diff check 通过。
- 同一补丁经三轮独立复审后为 `APPROVE`（0 CRITICAL / 0 HIGH / 1 MEDIUM）。PostgreSQL 18.4 只读 `EXPLAIN` 证明：未授权时 recipient/device 分支均 `never executed`；授权时先由指定群成员索引生成最多 4097 行的 materialized snapshot，再以 `recipient.user_id` 参数化查询设备，无旧版全站设备外侧扫描和 limit 前全量排序。剩余 MEDIUM 是 CI 尚无真实 PostgreSQL fixture 覆盖 sentinel/inactive/4096/4097；当前 mock SQL 合同不能替代该集成证据。
- 本轮补齐两个生产旁路：新建群创建者改走 `group_member_ds:join_group/5`，原子建立首个 open generation；解散群在同一 `conv_seq` 锁下关闭全部 open generation。`authorize_group_history/2` 还同时要求 open generation、active member 和 active group。
- scratch PostgreSQL 使用实际 `erlang_migrate` 完成 `107/f -> goto(100) -> 100/f -> up(all) -> 107/f`。down 后确认 `group_member_generation` 与 `msg_store_staging.conv_seq` 均不存在；legacy fixture 升级后，两个群的 C2G backlog 按 staging id 顺序分别取得 `11,12,13` 与 `1,2`，C2C backlog 保持 `NULL`，`msg_store_seq` 分别为 `13` 与 `2`。
- M1 只回填 legacy active member，inactive member 不回填；“单成员仅一个 open generation”和 interval check 的违规 INSERT 均被 PostgreSQL 拒绝。重复 `up(all)` 后仍为 `{ok,107,false}`，前后状态 SHA-256 均为 `fb62722b46d61982dc890383183af6fbb380acb98c00c926682c9bef3d69bf83`。
- 原始历史证据位于仓库外 `/private/tmp/imboy-e2ee-security-20260911-141840/`。此前 145/145 对当时所运行测试有效，但未覆盖真实 staging 调用形状；当前补丁已修正调用形状与测试入口，更新后的真库重放又因 PostgreSQL `econnrefused` 中止。因此历史结果不能替代当前真库 PASS，本轮记 `BLOCKED_ENV`。
- migration 108 为群聊附件增加客户端声明的 `anchor_msg_id`，C2G staging 在同一顺序锁事务内只把发送者、GID、未绑定状态均匹配的记录升级为权威 `anchor_conv_seq`；下载要求 active group/member/open generation 且 `start_seq <= anchor_conv_seq`。独立群文件不是聊天历史：由 `group_file_id` 明确关联，继续使用当前成员共享 ACL，不能伪装成某条群消息。legacy 聊天附件只给 M1 `start_seq=1` 世代兼容；未知类型或未绑定记录 fail-closed。
- 附件 ACL 在每次签发 GET URL 前重验，能阻止退出/移除后的再次签发，但无法即时撤销此前已签发的 URL；受限资源 URL 当前最长仍有效 600 秒。该 TOCTOU 窗口必须作为产品与威胁模型上限保留，不能对外宣称退出后对象访问“立即撤销”。
- App 的文件、相机图片、相机视频及缩略图、文件选择图片、相册图片、相册视频及缩略图、语音和位置缩略图八个生产上传入口均先生成最终 `messageId`，再以同值发送 `anchor_msg_id`；视频本体与缩略图共用该 ID。API 透传和生产源码接线有 C 级守护。后端强制 anchor 不能先于客户端覆盖率：必须先发布新客户端，再配置现有 `app_version` 的最低版本/强制升级策略并实际证明旧客户端无法进入群附件上传；若现有链只提示升级而不能阻断请求，必须先补最小服务端版本门。证据成立后才部署强制 anchor 后端。该顺序未在真实发布环境执行，记 `BLOCKED_EXTERNAL`；禁止以允许新群附件缺省 anchor 的方式换取兼容。
- migration 109 给 `msg_c2g_timeline` 增加权威 `conv_seq`；worker 缺失 seq 时返回 `c2g_conv_seq_missing`，离线 list/count 共用 active group/member/current open generation 与 `conv_seq >= start_seq` 条件，NULL legacy timeline 不返回。这本地修复关闭了“旧世代 room-key 未 ACK，leave 后 rejoin 又从 `/msg/offline` 取回并自动导入”的已确认代码路径；migration 109 up/down、真实 list/count 和 leave/rejoin 场景仍因 PostgreSQL 不可达而是 `BLOCKED_ENV`，不是攻击复测 PASS。
- 当前 App 备份只保存 `scope:sessionId -> exported key`，没有 generation、首末 `conv_seq`、来源或授权范围；恢复会把解析出的 `megolm_inbound_*` 全部写回安全存储。后端不解析 PFv3 `protected_header`，也没有 `session_ref -> conv_seq interval` 索引。恢复口令只证明用户主动操作，不能证明 session 完整位于获权 epoch。上述公钥枚举和 offline timeline 修复都不能替代 room-key epoch token、historical grant 与 backup metadata，完整 room-key grant 仍未闭环。

### 0.2 LT02-SEC-01 当前实现和剩余上限

- 消息、附件和重试路径现在汇流到 `AiPlaintextGate`；裸 `account_type=1` 只渲染 agent 徽章，不能直接授权明文。授权绑定 deployment、本机 owner UID、目标 UID、identity fingerprint/version 和用户显式确认，异常与缺失状态 fail-closed。
- 本轮修复授权检查的 TOCTOU：已有确认、确认弹窗和落库三个异步边界都复用同一当前绑定复核；owner UID/badge/deployment/identity 任一变化均拒绝。保存后显式重读刚写入的记录，复核失败即按已冻结的 owner UID 删除确认。新增回归覆盖已有记录读取、弹窗与保存期间的账号切换，badge 撤销，deployment/identity 变化，以及 SQLite 撤销持久化。
- 当前 `DeploymentScopedPeerIdentitySource` 的身份值仍只是 `SHA-256(deploymentId + targetUid), version=0`，不是服务端之外独立签名的 agent 身份公钥。因此现状只是一条用户确认型产品边界，不是密码学信任锚，也不覆盖恶意服务端威胁模型。
- `LT02-SEC-01` 的旧“裸 account_type 直接放行”代码根因已本地缓解，但因 AI-ID 决策工件缺失、独立身份锚缺失和 A 级复测阻塞，finding 不得进入 `ATTACK_RETEST_PASS/CLOSED`，仍是发布 `NO-GO` 原因。

## 1. 证据与基线

证据等级：A=本轮隔离环境真实客户端→真实后端→真实存储→真实接收端；B=当前源码/配置/schema/可复现集成证据；C=单元或协议测试；D=文档/注释/历史报告。明确可达的安全边界违反直接判 FAIL；缺少必要运行证据判 UNKNOWN/PARTIAL/BLOCKED。

状态机：`OPEN → ROOT_CAUSE_CONFIRMED → FIXED → REGRESSION_PASS → ATTACK_RETEST_PASS → CLOSED`。跨 SHA 后旧计数只作 D 级线索；2026-09-09 历史 Base 的 C 级重签最高只能回到 `REGRESSION_PASS`，任何 finding 未完成 A 级攻击复测不得 `CLOSED`。

### 1.1 2026-09-09 历史 Base C 级重验实测（LT-02-C）

冻结基线（三仓 detached worktree，全部 tracked clean）：

```text
imboy      63747f8d7a0f9bc27bce4c540549a032141fbc3a
imboyapp   0152560aa741b69411e484cc84c1c220f565b2af
imboyadmin 8c2b8615c292d82257886ad51445db87c366d719
```

| 本轮实测（LT-02-C） | 结果 | 等级与边界 |
|---|---|---|
| 后端 9 EUnit 模块（串行，任务专属 scratch PostgreSQL，strict tracked migrations） | **124 PASS / 0 FAIL / 0 cancelled**；模块明细：group_member_repo 21、group_member_ds 8、group_member_workspace_subset 3、workspace_logic 29、group_member_logic_event 2、group_event_handler 3、messaging_logic 12、msg_handler 11、msg_c2s_logic 35；scratch DB 与连接 residual=0（repo 模块首跑曾因裸 marker DB 缺 timescaledb 扩展 17 例 cancelled，runner 修复扩展预装后单模块重放 21/21，全程 0 断言失败） | C；上会话参考值 124 PASS（D/superseded）与本轮一致；不构成 A 级 |
| Flutter finding 003/005..014 测试组（ENV-01-A3 cleanroom 重放后逐文件串行，离线无设备） | **45 唯一存在文件全绿：410 PASS / 0 FAIL / 1 declared SKIP**；组明细：003=11、005=32、006=23、007=6（含 room-key 导入 1/1）、008=37、009=89、010=63、011=52(+1 SKIP)+1 缺失 DRIFT、013=40（16+13+11）、014=68（e2ee_service_test 同属 003/014，总计数只算一次）；REV-1：011 组 `e2ee_server_backup_service_test.dart`（6 tests）Wave-2 批次曾遗漏未跑，2026-09-09 按审查意见 R1-C1 以同口径补跑 6/6 PASS 计入（45 文件 = Wave-2 44 + 补跑 1） | C；上会话参考值「31 文件 323 PASS / 4 SKIP / 0 FAIL」（D/superseded）与本轮批次定义不同（本轮按报告声明组重建为 45+2+1 项），数量差异如实记录、不折算 |
| Flutter 2 个 integration_test 文件 | **ENV_BLOCKED_ATTEMPTED**（非静默跳过）：`sqlcipher_migration_test.dart`、`sqlite_migration_kill_replay_test.dart` 在 `flutter test` 下进入设备集成模式（Gradle assembleDebug + 需连接设备），离线无设备不可执行；文件头自声明需真实 SQLCipher 设备/外部 kill 编排；两文件各有独立尝试日志（REV-1 补 kill_replay 尝试日志：直接运行立即要求选择设备、exit=1 未执行任何测试，见 evidence `flutter-tests/env-blocked-02-*`） | 环境受限；不得计为 PASS 或 FAIL |
| Flutter 1 个缺失文件（DRIFT） | `integration_test/settings/e2ee_backup_recovery_acceptance_test.dart`（011 破坏性恢复 harness）在冻结 Base 不存在——共享工作区 untracked、从未提交；报告 §1 原文「当前工作区」与此吻合；其声明行为为默认运行 1 SKIP，不构成 PASS 来源 | BLOCKED_DRIFT，如实记录 |
| scoped `flutter analyze`（lib/service/e2ee、e2ee 相关 service/test 14 项目标） | **No issues found**（exit 0） | C |
| Signed Capabilities 生产 wiring 静态扫描 | `CapabilityGuard` 0 生产调用方；`CapabilityNegotiator` lib 内仅 1 处注释引用 + 1 处静态表引用（guard 自身 0 调用方）；`verifyDeviceManifest` 无任何 lib 调用方，`DeviceManifest` 类型仅出现于 capability 三件套（device_manifest/identity_verifier/capability_negotiator）内部、未接线；`olm_session_service.dart:15` 的 identity_verifier import 只使用 `verifyIdentitySignature`（该 import 非未使用）→ **生产调用=0** | B；如实报告，不构成 MITM/降级防护的任何方向证据（见 finding 013） |
| 012 架构缺口当前 Base 重验 | `messaging_logic:history` 与 `msg_c2s_logic:handle_sync` 仍为 `group_ds:is_member` boolean + 整个 `c2g:<gid>` 读取；`group_member` 列仍为 id/group_id/user_id/role/is_join/join_mode/status/created_at/updated_at（无 join boundary/generation）；staging（msg_store_ds 无 conv_seq）→ worker 异步 `msg_archive_repo:archive` → archive 阶段 `next_conv_seq/1` | B；见 §5 finding 012 与决策包 |
| LT02-SEC-01 当前 Base 重验 | `imboyapp/lib/service/e2ee_service.dart:176` 仍为 `if (chatType == 'C2C' && peerAccountType == 1) return false;`——未签名/可污染标记直接触发发送前明文豁免 | B；**保持 HIGH/OPEN**（见 §5.5，关闭属 Task 4/AI-ID 决策） |

本轮重验未运行：A 级攻击复测、真机、账号、群操作、抓包、Keychain/Keystore、生产与第三方（全部维持 BLOCKED，见 §7）。

### 1.2 阶段 A 历史基线（2026-09-07，D/superseded——不在当前 Base 重验范围）

| 仓库 | 分支与 HEAD | 阶段 A worktree |
|---|---|---|
| `imboy` | `main` / `850af103d41d2f24ad8cf7827f816b9c7ca7d0ef` | dirty 8 项，含既有 E2EE 文档改动和未跟踪 `docs/security/` |
| `imboyapp` | `main` / `924347d011e84716a1b89a03cff10ac391010e69` | dirty 79 项，均按既有工作保留 |

依赖：Flutter 3.47.2、Dart 3.13.2、OTP 29、ERTS 17.0.2、GNU Make 3.81、`flutter_vodozemac 0.8.1`、`vodozemac 0.8.0`、`sqflite_sqlcipher 3.4.1`。销售 Compose/Helm 默认 `required`，策略无权威值时回落 `disabled`；运行节点实际模式未查，记 UNKNOWN。被忽略的 `config/sys.pro.config` 含敏感赋值特征但不在 HEAD，本轮未读取其值。

下表为阶段 A 原始证据（**全部 D/superseded**：计数产生于上表旧 SHA，仅作线索保留；当前 Base 替代值见 §1.1）：

| 本轮证据 | 结果 | 等级与边界 |
|---|---|---|
| Flutter E2EE 定向测试 | 72/72 | C；PFv3、TOFU、策略、附件 AEAD、replay、fan-out |
| vodozemac 原生 FFI | 5/5 | C；Olm 建链、PFS/PCS ratchet、room-key-over-Olm |
| 后端定向 EUnit | 124 PASS / 1 FAIL | C；失败为产品 feature 顺序，非密码学断言，但门禁并非全绿 |
| 当前源码/配置/schema 链路追踪 | 完成 | B；不能替代 A 级黑盒证据 |
| Finding 001 定向回归 | 50/50 PASS，编译与格式检查通过 | C；真实 PostgreSQL/前成员攻击复测仍 BLOCKED |
| Finding 002 定向回归 | 28/28 PASS | C；工作区移除后的真机通知、rotation 与旧 session 攻击复测仍 BLOCKED |
| Finding 003 定向回归 | 11/11 PASS；定向 analyze 与 diff check 通过 | C；强刷空结果/异常均不复用旧设备，真实撤销攻击复测仍 BLOCKED |
| 群会话回归 | 25/25 PASS | C；既有合规测试夹具意外访问默认生产只读端点，故不作为隔离环境或 A 级证据 |
| Finding 004 定向回归 | logic 12/12、handler 11/11 PASS；编译与格式检查通过 | C；非活跃成员在归档查询前被拒绝，真实前成员 API 复测仍 BLOCKED |
| Finding 010 定向回归 | 上传接线 18/18、策略/封装 20/20 PASS；定向 analyze 通过 | C；策略未知在上传前抛错，真实对象存储 Canary 复测仍 BLOCKED |
| Finding 010 fail-closed 重开回归 | `imboyapp@2aa76a09`；required E2EE 下绑定缺失（登出半态 senderUid 空、messageId 空）上传前抛 `attachment_binding_missing`，视频主文件/缩略图 partial seal 抛 `attachment_partial_seal`（先于一切 IO）；wiring 25/25、缩略图同生同灭 7/7、附件域回归 660 过（3 红已归因并修复，见下行）；定向 analyze 通过 | C；明文部署与开关关闭两合法路径不抛；真实对象存储 Canary 攻击复测仍 BLOCKED |
| 存量 3 红归因修复 | `imboyapp@4b09eb64`；replay_counter_epoch_test 三红灯（正路径/option C lower-seq/isolation 各自 r2）根因为 fixture 缺陷：buildEnvelope 硬编码 `messageId='msg-001'`，第二条起被 crypto_inbox 的 message_id 全局 dedupe（ADR 15 §7.1，`stageInbound`，生产上正确且必须的重放防护）拒为 duplicate_message，与 epoch_or_counter 序列检查无关；产品代码无回归；修复为 `msg-$sessionId-$sequence` 逐条唯一，4/4 全绿；另 `imboyapp@0532d1bc` e2ee_service 四处 `${e.runtimeType}: $e` 日志收窄（29/29 回归绿）、`imboyapp@95ec0080` e2ee_health_check 测试注入恒失败 HTTP 出口不再真连 pro.imboy.pub（21/21） | 定性为测试带病入库（选项 C 重写时引入，从未跑绿），非产品回归 |
| Finding 008 定向回归 | Safety Number 入口 3/3、关联威胁模型 24/24 PASS；定向 analyze 与 diff check 通过 | C；双方真实 device ID/Olm identity 聚合且验证状态绑定当前号码，双真机换钥与设备增删复测仍 BLOCKED |
| Finding 014 定向回归 | 日志边界守卫 1/1、关联 WS/ACK/离线解密 27/27 PASS；脱敏后守卫+WS 复跑 21/21 PASS；定向 analyze 通过 | C；消息链路已禁止记录原始帧、完整异常、明文 payload/preview、会话对象与标题；真机与后端日志 Canary 扫描仍 BLOCKED |
| Finding 014 重开回归 | `imboyapp@8d655ca7`；group_session/olm_session/chat_network 三 service 残留的完整异常对象、stackTrace、库错误原文（含 Rust/vodozemac pickle 片段、sqflite SQL 回显）收窄为 `errType=<runtimeType>` + 稳定上下文标识；OlmAuthenticationException 构造不再拼接库错误原文；toast 兜底改纯稳定文案 `e2eeErrDefault`，可诊断性由 `olm_wrap_failed` 稳定错误码路由承担；守卫+路由回归 10/10 PASS；定向 analyze 通过 | C；Sentry/后端日志 Canary 扫描待授权；`e2ee_service` 残留四处已同口径收窄（`imboyapp@0532d1bc`，29/29 回归绿）， Finding 014 客户端日志边界完全闭环 |
| Finding 009 定向回归 | SQLCipher 边界 13/13、数据库迁移/快照/schema/uid 隔离关联回归 59/59 PASS；定向 analyze 与 diff check 通过 | C；加密平台不再无密码探测/回退，不再创建或自动清理明文迁移备份，错钥/明文/损坏统一保留原库并停止初始化 |
| Finding 009 Android 限定范围复测 | 物理 Android 9 真机 8/8 PASS；`imboyapp@4baf79a8`；测试文件 SHA-256 `c6804f3f34dab7efdb955f39e626ba1a00fdc7d999c50744d87737a6ce9b7662`；测试 APK SHA-256 `b866a05c53ea4ef49f7937b2669fc0938e57f7b075d4ffbba3e8d4f0370c9dc1` | B（真机集成，非完整 A 级链路）；`Random.secure()` 每轮生成 Canary，正确密钥建库/重开、错钥拒绝、原文件字节不变、无新 `.plain.bak`/`.pre_encrypt.bak`，随机临时目录已清理且测试包已卸载；Secure Storage 为 mock，旧明文库、WAL/SHM 与历史 artifact 未覆盖 |
| Findings 005/006/007 定向回归 | `imboyapp@eb4e3a9f`；PFv3/Olm/SQLCipher staging、ACK 顺序、离线归一化及 replay 组合 85 PASS / 4 SKIP；room-key 导入定向 1/1 PASS；定向 analyze 与 diff check 通过；Debug APK SHA-256 `6fa953c5221df4f19e67f8b8fd588f91a26d2587ec807148900015e6392e0ff5` | C；实时与离线 C2C/C2G 仅在认证、ratchet/digest 与最终消息提交后 ACK；合法完成态可幂等确认，同 ID 改密文或同密文换 ID 被拒绝；真实重启、重传、存储故障和攻击复测仍 BLOCKED |
| Findings 006/007 Android 限定范围复测 | 与 Finding 009 同一物理 Android 9 轮次 8/8 PASS；SQLCipher inbox 用例完成 stage→原子保存解密结果→关闭/重开句柄→恢复→complete，并验证同 ID/同 digest 为 processed、同 ID/改 digest 为 replay，数据库文件字节不含随机 Canary | B（真机 SQLCipher 插件与文件系统，非完整 A 级链路）；未执行真实 App 进程 kill、服务端重投、Olm/Megolm 解密或最终消息库故障，故 006/007 仍仅 REGRESSION_PASS |
| Finding 011 备份边界回归 | `imboyapp@f7fe8dc4`、`imboyapp@2da2ea5e`、`imboyapp@986d1ef3`、`imboyapp@384e8feb`；备份/恢复、Megolm 段、服务端密文包、parser 与恢复写失败边界 57/57 PASS；新增恢复核心定向复跑 11/11 PASS；备份导入 widget 7/7 PASS；定向 analyze、diff check 与提交门禁通过 | C；备份只纳入 legacy RSA 当前私钥和 Megolm inbound，明确排除 Olm account/session 与 TOFU pin；Secure Storage 秘密清单不可读时导出 fail-closed，任一 Megolm session 写入失败时导入不再显示完整成功，成功文案只承诺备份内已成功写入的 session；widget 现注入本地 cloud probe，不再构造生产 URL、读取 token 或进入密钥清理链，并断言失败/有备份两态均恰好探测一次，但仍只构成 C 级 UI 回归；真实换机/重装恢复仍 BLOCKED |
| 破坏性备份恢复验收 harness 安全审查 | 当前工作区 `integration_test/settings/e2ee_backup_recovery_acceptance_test.dart`；默认无授权参数运行只产生 1 个 SKIP，定向 analyze 通过；未运行 destructive 分支 | B；BLOCKED。测试仍不能证明专用 application ID/空白容器，在 UID 核对前清 E2EE Keychain 并可能先退出其他登录账号；登录会写本地库、上报设备密钥和建立 WS；云清理端点按 UID 删除全部版本且存在 TOCTOU；清理异常可能被吞后仍 Green。默认 SKIP 只证明门禁拒绝，不能作为 PASS；在专用测试包、隔离后端/数据库、受控账号/设备和资源级清理闭环完成前不得执行或作为发布证据 |
| Finding 013 Signed Capabilities 取证 | `DeviceManifest`/协商/HWM 纯函数 40/40 PASS；全生产调用者扫描完成 | C+B；模型和单测可用，但客户端没有生成/上传/拉取 manifest，发送路径不调用协商器/HWM，服务端仅有未接线列；故生产 Signed Capabilities 声明不成立 |

## 2. 真实消息路径

**C2C 发送**：Composer → `MessageModel` → `ChatNetworkService.sendWsMsg/sendMessage` → `_shouldEncryptOutbound` → `encryptPayload` → `_encryptC2COlmFanOut` → `E2eeOutboundRouter.encryptV3` → `OlmProtocol.encrypt` → `OlmSessionService.encryptC2CMessage` → 最终 `Session.encrypt`（`imboyapp/lib/service/olm_session_service.dart:642`）→ WS。

**C2C 接收**：WS → `MessageService` → `E2EEService.decryptInboundV3` → `OlmProtocol.decrypt` → `OlmSessionService.decryptC2CMessage` → prekey 首包 `Account.createInboundSession` / 普通包最终 `Session.decrypt`（`:869`）→ 本地 DB/UI。

**C2G 发送**：同一入口 → 群级策略门 → `GroupSessionService.ensureOutboundSession` → `E2eeOutboundRouter.encryptV3` → `MegolmProtocol.encrypt` → 最终 `GroupSession.encrypt`（`imboyapp/lib/service/group_session_service.dart:209`）→ WS；room key 对每个设备经 Olm 包裹分发。

**C2G 接收**：WS → `MessageService` → `E2EEService.decryptInboundV3` → `MegolmProtocol.decrypt` → `GroupSessionService.decryptGroupMessage` → 最终 `InboundGroupSession.decrypt`（`:605`）→ 本地 DB/UI。

**服务端/离线**：WS handler → `message_router_logic` → `msg_c2c_logic/msg_c2g_logic` → deployment/group policy gate → `msg_store` staging → worker → 正式表/归档 → 在线投递或离线拉取 →客户端 ACK。schema 支持密文不等于实际数据库、日志、备份和 Push 已无明文。

## 3. 实际状态机与安全上限

- C2C：设备生成 Olm Account → 上报 identity/OTK/签名 fallback → claim OTK（耗尽时用签名 fallback）→ 验签与 TOFU → 建立/恢复 per-device session → PFv3+Olm 加密；ratchet 与 outbox、dedupe 与接收 ratchet 分别在 SQLCipher 事务中提交。设计上不降级到 RSA/明文。
- C2G：发送端内存维护 outbound Megolm → 导出 room key → Olm 逐设备包裹 → 接收端保存 inbound pickle → 按成员/设备变化、100 条、7 天或进程重启轮换。当前源码已补齐成员撤销传播并使强刷失败/空结果 fail-closed，本地回归通过；真实设备撤销链仍待 A 级复测。
- Megolm rotation 不等于 Double Ratchet PCS。离群后保密依赖“成员撤销传播→新设备快照→下一条消息前轮换”完整成立；当前仅有 B/C 级实现与回归证据。
- 新成员/重新加入是否可读历史尚无明确产品策略，记安全设计缺口。当前 `/msg/history` 与批量 `sync` 均只检查当前 active membership 后查询整个 `c2g:<gid>`；`group_member` 无 join epoch/history boundary，重入 upsert 保留旧 `created_at`，而 `updated_at` 会被角色、备注、禁言等普通操作改写，现有列不能安全充当历史边界。

## 4. 密钥所有权

| 材料 | 客户端生成/存储 | 服务端可见 | 恢复与生命周期 |
|---|---|---|---|
| Olm identity/account 私态 | vodozemac；Secure Storage 中 account pickle/pickle key | Ed25519/Curve25519 公钥 | 不入备份；清数据重建；logout 清理 |
| OTK/签名 fallback 私态 | account pickle | 公钥、签名、claim 状态 | OTK 原子消费；fallback 轮换 |
| Olm session/message key | SQLCipher CryptoStore / ratchet 瞬态 | 预期不可见 | session 不备份，防棘轮分叉；message key 不导出 |
| Megolm outbound | 发送设备内存 | 预期不可见 | 轮换或重启重建 |
| Megolm inbound/room key | 接收设备 Secure Storage | 仅见 Olm 包裹帧 | 保留解历史；进入加密备份 |
| 附件 content key | 客户端 CSPRNG；descriptor 应在 PFv3 内 | 新链路预期不可见 | 每附件独立；解密产物可进入普通缓存 |
| 备份 key | 用户口令 KDF 派生 | 密文、salt、KDF 参数 | 派生 key 不保存；备份不含 Olm account/session/TOFU pin |
| legacy RSA 私钥 | Secure Storage + 加密备份 | 公钥；历史迁移窗口可能可解 | 仅历史解密意图 |
| compliance key | 客户端拉取并 TOFU pin 公钥 | 公钥 | 私钥实际保管方和运行保护 UNKNOWN |
| SQLCipher key | Secure Storage | 预期不可见 | 当前源码已移除无密码探测/回退及明文备份生成；历史备份 artifact 的盘点/清理由设备与数据范围授权后执行 |

## 5. Findings 与修复状态

| ID | 级别 | 根因/影响 | 状态与关闭条件 |
|---|---|---|---|
| E2EE-2026-001 | P0 | `group_member_repo:find/3` 与成员列表未过滤 `status=1`；停用行被群消息、群密钥、附件和多类群 ACL 当成活跃成员 | REGRESSION_PASS（当前 Base 重验 2026-09-09：后端组 32 PASS/0 FAIL——group_member_repo 21 + group_member_ds 8 + workspace_subset 3，scratch DB）；共享查询和列表已限定 active，显式重入改为原子恢复；移除成员攻击复测待授权 |
| E2EE-2026-002 | P0 | 工作区移除只停用下属群记录，未逐群发 leave、清缓存或触发 session stale | REGRESSION_PASS（当前 Base 重验 2026-09-09：后端组 34 PASS/0 FAIL——workspace_logic 29 + group_member_logic_event 2 + group_event_handler 3）；事务提交后已逐群清缓存并发布持久 leave 通知；工作区移除与旧 session 攻击复测待授权 |
| E2EE-2026-003 | P0 | 群设备密钥强刷失败/为空时复用最长 30 分钟旧缓存 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter `e2ee_service_test` 11/11 PASS/0 FAIL）；强刷空结果覆盖旧缓存，最终异常清空整组缓存并抛出；旧设备/旧 session 攻击复测待授权 |
| E2EE-2026-004 | P1 | C2G history 仅构造 `c2g:<gid>`，不校验当前活跃成员 | REGRESSION_PASS（当前 Base 重验 2026-09-09：后端组 23 PASS/0 FAIL——messaging_logic 12 + msg_handler 11）；共享 history 入口已要求当前 active 成员并在归档查询前拒绝；前成员 API 攻击复测待授权；012 join boundary 未决前 history ACL 仅到 boolean 成员级 |
| E2EE-2026-005 | P1 | 客户端在 PFv3 认证、解密、持久化前 ACK；实时与离线两条路径同源 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 32 PASS/0 FAIL——inbound_ack_ordering 2 + message_ack_flow_integration 24 + websocket_server_ack 6）；WS 不再提前 ACK，内容/action/room key 均在处理成功后确认；离线业务 `msg_id` 归一化且 HTTP ACK 拒绝/异常 fail-closed；真实离线、畸形帧和重投攻击复测待授权 |
| E2EE-2026-006 | P1 | PFv3 解密失败未形成有界、可重启恢复的密文状态；Olm ratchet 已提交而消息明文尚未提交时存在崩溃丢信窗口 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 23 PASS/0 FAIL——outbox_crash_recovery 11 + outbox_fail_closed 4 + decrypt_on_read_v3_gap 5 + outbox_read_side_wiring 3）；SQLCipher 先暂存最多 512 条/单帧 256 KiB，ratchet/dedupe/digest 与可恢复解密结果原子提交；真实 App kill/restart 与磁盘故障复测待授权 |
| E2EE-2026-007 | P1 | C2G 无持久 replay 状态；C2C `dedupeAndPersistSession` 捕获全部异常并返回 duplicate，把存储事故与重放混同 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 6 PASS/0 FAIL——replay_counter_epoch 4 + room_key_olm_roundtrip 1/1 + mutation_matrix 1）；C2C/C2G 共用持久 message-id 与受保护信封 digest，存储异常单独抛出；真实重放与乱序攻击复测待授权 |
| E2EE-2026-008 | P1 | Safety Number 入口把 legacy RSA key/kid 当 Olm identity/device ID，本端 device ID 为空，且本地“已验证”未绑定当前号码 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 37 PASS/0 FAIL——safety_number_service 3 + threat_model_guard 24 + safety_number 9 + safety_number_page_widget 1）；双方均取活跃 Olm device ID，identity 走本地权威或签名+TOFU 路径；双真机换钥/增删设备攻击复测待授权 |
| E2EE-2026-009 | P1 | SQLCipher 密码打开失败后尝试 `password:null`；明文探测成功后复制 `.plain.bak` 并删除原库，备份最长保留 7 天 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 89 PASS/0 FAIL——db_migration_encryption 13 + 迁移/降级/快照矩阵 76；另有 2 个 integration_test 设备文件 ENV_BLOCKED_ATTEMPTED，见 §1.1）；加密平台已有库只用当前 key 验证，失败保留原库并终止；旧明文库、WAL/SHM、历史 artifact 和真实 Keystore 仍待授权取证，不得升级为完整 `ATTACK_RETEST_PASS` |
| E2EE-2026-010 | P1 | 附件策略查询异常返回“不封装”，先明文上传、后由消息门拒发 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 63 PASS/0 FAIL——attachment_seal_wiring 25 + thumb_seal 7 + upload_sealed 11 + seal_policy 8 + attachment_binding 12）；策略未知在上传前中止；required 下绑定缺失与 partial seal 上传前失败；对象存储 Canary 攻击复测待授权；AI C2C 明文附件是设计内例外（见 §5.5） |
| E2EE-2026-011 | P1 | 备份刻意不含 Olm account/session/TOFU pin，故不恢复 Olm 身份连续性或 C2C ratchet 历史；旧导出路径在 Secure Storage 枚举失败时会静默生成 RSA-only 不完整备份；旧导入路径吞掉单条 Megolm 写失败后仍报告整体成功 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 52 PASS/0 FAIL/1 declared SKIP——backup_restore 14 + local_backup_boundary 14 + server_backup_service 6（REV-1 2026-09-09 补跑实测 6/6：Wave-2 批次曾漏跑该文件，按 R1-C1 以同口径单文件串行补跑计入，日志见 evidence `flutter-tests/45-e2ee_server_backup_service_test.log`）+ megolm_backup_section 11 + import_widget 7 + backup_api 0 PASS/1 SKIP（TEST_PHONE 未配置，测试自带门限）；枚举合计 14+14+6+11+7+0=52 PASS + 1 SKIP；破坏性恢复 harness 文件在冻结 Base 缺失记 BLOCKED_DRIFT，其声明行为仅 1 SKIP，不影响 PASS 面）；备份仅含 legacy RSA 与 Megolm inbound，排除 Olm/TOFU；真实换机/重装恢复待攻击复测 |
| E2EE-2026-012 | P1 | 新成员/重入群历史访问需要原子 generation/`conv_seq` 边界，并与附件 ACL、room-key grant 共用批准后的语义 | `LOCAL_SECURITY_GATE_FAIL / DECISION_EVIDENCE_MISSING / A_LEVEL_ATTACK_RETEST=BLOCKED`（2026-09-11 override）：生产 C2G staging 已改为顺序锁事务内固化 `conv_seq`、active sender/role/recipient snapshot，并由全部下游与 worker claim 复用；聊天附件 anchor ACL 与 offline timeline generation 过滤已本地接线，核心定向 91/91 PASS。migration 108/109 与更新后的真库套件因 PostgreSQL `econnrefused` 为 `BLOCKED_ENV`；F2/R2/D3/M1 用户书面批准、旧客户端 rollout、room-key epoch/backup metadata、historical grant 和真实生命周期仍未闭环；不得 `CLOSED` |
| E2EE-2026-013 | P2 | 规范声明 Signed Capabilities；客户端只有 `DeviceManifest`/协商/HWM 模型与单测，未进入身份上传、设备查询或发送链，服务端仅有未接线 schema 列 | REGRESSION_PASS（仅声明级；当前 Base 重验 2026-09-09：Flutter 三件套 40/40 PASS——device_manifest 16 + capability_negotiator 13 + capability_guard 11，其中 negotiator/manifest 前两组依赖 vodozemac 原生测试库，环境补齐后全绿；**生产 wiring 静态扫描重验=0 调用方**：CapabilityGuard 无生产 caller，CapabilityNegotiator 仅注释+静态表引用，verifyDeviceManifest 无任何 lib 调用方、DeviceManifest 类型仅存在于 capability 三件套内部）；规范 §8.2/§8.3 维持「未实现/未接线」降级声明，生产降级防护实态为固定套件选择；capability 模型/单测存在不暗示生产接线，本 finding 不构成 MITM/降级防护证据 |
| E2EE-2026-014 | P2 | debug 路径可记录完整 WS/解密后 Conversation payload，解析异常文本也可携带输入片段 | REGRESSION_PASS（当前 Base 重验 2026-09-09：Flutter 组 68 PASS/0 FAIL——plain_text_log 1 + logging_privacy_guard 2 + olm_wrap_failed_message 8 + e2ee_health_check 21 + e2ee_service 11（同属 003）+ crypto_audit_log 12 + log_redactor 13）；消息链路日志已收窄为非敏感元数据和异常类型；真实 Canary 日志扫描待授权 |

安全边界 finding 未完成对应攻击复测不得 CLOSED。上表所有 REGRESSION_PASS 均为当前 Base C 级重签；升级 `ATTACK_RETEST_PASS` 需另行授权的 A 级复测。

### 5.5 AI 明文域与未解决 HIGH blocker LT02-SEC-01

真人 E2EE（C2C Olm/PFv3、C2G Megolm）与 AI 助手 C2C 明文处理是两个不同的可见性域，不得互相覆盖：

- **AI 助手 C2C 是产品设计的非 E2EE 通道**：客户端看到 `peerAccountType=1` 时在消息/附件离开设备前跳过封装；服务端 staging/archive 可见正文，对象存储可见未封装附件，LLM provider 可见送入 prompt 的正文与所选上下文（留存取决于 provider/部署配置，未验证）。该域必须在 UI/隐私/合规文档中显式披露，禁止用「所有消息」「全链」「唯一明文路径」等绝对措辞。
- **LT02-SEC-01（2026-09-11：本地缓解，仍是 release NO-GO 独立原因）**：裸 `peerAccountType/account_type=1` 已不能直接授权明文；消息、附件和重试统一要求当前 agent badge、deployment/owner/target/identity/version 五元组和用户显式确认，并在异步确认/落库边界二次复核，缺失、异常或变化均 fail-closed。剩余边界是 identity 仍由 deployment ID 与目标 UID 派生而非独立签名信任锚，且所引用 AI-ID=B 用户决策工件不存在；恶意服务端、真实传输/存储和身份变化尚无 A 级复测。因此只能记本地 `REGRESSION_PASS_WITH_LIMITS`，不得 `ATTACK_RETEST_PASS/CLOSED`。

## 6. 声明仲裁与文档漂移

| 声明 | 当前结论 |
|---|---|
| C2C Olm/PFv3/per-device | B/C 支持；当前双真机 A 级证据缺失，PARTIAL |
| C2G Megolm/room-key-over-Olm | B/C 级协议、成员撤销传播、密钥强刷与 history ACL 回归通过；A 级撤销复测缺失且新成员历史策略未定义，PARTIAL |
| required/compliance 全链拒绝明文 | 消息门、群密钥强刷和附件上传前策略本地路径均 fail-closed；真实 Transport/对象存储证据仍缺失 |
| legacy RSA 仅历史解密 | 编排符合意图；新消息降级攻击未复测，PARTIAL |
| 附件上传前 AES-256-GCM | B/C 级内核与策略异常 fail-closed 回归通过；对象存储、缩略图和临时文件 A 级证据缺失，PARTIAL |
| Push 固定占位 | B 级正文占位；昵称/群名 metadata 和真实 provider payload 待查，PARTIAL |
| 服务端零知识 | B 级源码/schema 支持；实际 DB/日志/备份/历史窗口未查，UNKNOWN |
| 私钥保护、Replay、MITM、多设备 | SQLCipher 无密码降级与新明文备份路径已修复并通过 C 级回归，但历史 artifact 和 Android/iOS 真机提取抗性未复测；C2C/C2G 已加入持久 message-id 与受保护信封 digest 防重，但真实 replay/乱序仍缺 A 级证据；Safety Number 入口本地回归通过，但双真机换钥/设备增删及“聚合号码验证只上报首个设备”边界未完成 A 级验证；多设备整体仍为 PARTIAL |
| 备份/恢复 | 当前加密包可恢复 legacy RSA 当前私钥与已纳入的 Megolm inbound；不恢复 Olm account/session/TOFU pin，不应被宣传为 C2C 身份或棘轮恢复。秘密清单枚举失败和任一 Megolm 写入失败均已 fail-closed，成功文案不再承诺完整群历史；Secure Storage 无跨条目事务，失败前已写 session 依赖幂等重试收敛；真实换机恢复、恢复中断与 Safety Number 变化告警尚无 A 级证据 |
| Signed Capabilities | 仅模型与单测存在（当前 Base 重验 40/40 PASS 但生产调用方=0）；当前客户端/服务端/发送链未形成签名能力声明协议，不得作为 MITM 或降级防护证据 |
| AI 助手明文通道 | 产品设计的非 E2EE 域（§5.5）；真人 E2EE 声明不得覆盖该域，「服务端零知识」只对真人 C2C/C2G 的 required/compliance 路径成立；`peerAccountType=1` 授权缺信任锚是 LT02-SEC-01 HIGH/OPEN |

漂移：旧红队 GO 早于当前 PFv3 群协议；旧审计声称 P0=0/P1=0 和全部降级 fail-closed；Safety Number 文档声称全设备聚合；Signed Capabilities 规范声明超前于真实产品链；本地备份注释曾声称“服务器不存储”而同一客户端已有云端密文备份；部分规范仍描述旧 RSA/vodozemac；销售默认值不能证明运行配置。当前架构声明不使用 Redis，除非授权目标额外引入，否则 Redis 项为 N/A。

## 7. 运行缺口与发布门

历史授权仅覆盖一台物理 Android 9 真机上的 SQLCipher 随机临时库测试（阶段 A，D/superseded），不含账号、后端、现有 App 数据、真实 Secure Storage/Keystore 提取、旧库或历史 artifact。当前 Base（2026-09-09 重验）授权范围为：隔离 worktree 内 C 级单元/协议测试 + 静态源码对账，专属 scratch PostgreSQL，离线无设备。除此之外，下列项仍为 UNKNOWN/BLOCKED：两用户 C2C、四用户 C2G 真机；Canary 搜索 Transport/PG/队列/日志/备份/对象存储/Push；篡改、MITM、replay、乱序、重连、重启；identity/prekey/session/设备撤销；群加入/退出/移除/重入；Android Keystore、iOS Keychain、附件临时文件和缓存。现有账号、设备、端口、进程或生产地址均不构成默认授权。

当前发布姿态：**NO-GO**。独立原因（各自成立即维持 NO-GO）：

1. `LT02-SEC-01` 已本地缓解但仍缺 AI-ID 决策证据、独立签名身份锚和 A 级攻击复测（§0.2、§5.5）。
2. `E2EE-2026-012` 的 C2G staging、聊天附件 anchor ACL 和 offline timeline generation 过滤已本地修复并通过定向回归，但 migration 108/109 与更新后的真库套件为 `BLOCKED_ENV`，旧客户端 rollout 为 `BLOCKED_EXTERNAL`；F/R/D/M 决策证据、room-key epoch/backup metadata、historical grant 和 A 级生命周期仍未闭环（§0.1 与决策包）。
3. 全部 P0/P1 finding 仅有 C 级 REGRESSION_PASS，A 级攻击复测为 0。
4. 阶段 A 真机与历史 B 级证据产生于旧 SHA，当前 Base 未重签。

最终固定 verdict 仅在决策治理、剩余修复和授权后的攻击复测完成后写入；此前本文件不提供任何最终发布 PASS。2026-09-11 本地回归与旧 Base C 级重验都只是回归底座，不得单独解读为发布放行。
