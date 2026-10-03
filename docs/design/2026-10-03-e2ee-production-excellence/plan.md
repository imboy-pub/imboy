# E2EE 投产闭环与行业领先能力分阶段执行计划 v1.0

**执行对象：** ZCODE Coordinator；按任务卡实施、评审、集成，不在本次编写计划时启动实施。
**Goal：** 在明确威胁模型和产品边界下，建立可复现的 C2C/C2G E2EE 投产候选，再完成身份透明度、群组泄露后恢复、抗量子与隐私能力的分阶段验收。
**Architecture：** 复用现有 Olm/Megolm、vodozemac、PFv3、CryptoStore、附件 AEAD 和群历史 generation/attestation；先修实际缺口，再引入经审计的协议实现。坚持 Handler→Logic→DS→Repo，客户端沿现有 API/Service/页面链接线。
**Tech Stack：** Erlang/OTP、PostgreSQL、Flutter/Dart、vodozemac、EUnit、Flutter test、Patrol/真机。MLS/PQ 实现须经独立兼容性、许可证和平台评估后选择，不自研密码原语。
**编写日期：** 2026-10-03 Asia/Shanghai。

## 1. 基线、范围和结论边界

工作区 `/Users/leeyi/project/imboy.pub` 非 Git 根。本计划位于后端可跟踪的 `docs/design/`，避免被忽略的 `docs/plans/` 导致交付不可见。

| 仓库 | 编写时 HEAD | 状态 |
|---|---|---|
| imboy | 33277f7f64fa0cc1524e2ec7e80c898b8d2d0933 | git status --short 空 |
| imboyapp | 826e123803f8e962ae53c9dda25d96fe91329e0e | git status --short 空 |

运行时必须重采样；基线漂移可在 W0 只读重审后记录新基线，不自动沿用旧 PASS。此计划不是密码安全认证，也不授权生产部署。上一轮分析里的 P0 是优先级，不是已验证漏洞严重度；每条缺口在 C00 分类为 CONFIRMED/ALREADY_FIXED/UNVERIFIED/DESIGN_LIMIT。

范围：真人单聊、群聊、多设备、附件、备份恢复、AI/企业托管边界、端侧秘密、版本迁移、运维和行业领先扩展。Admin 仅在实际配置/版本门影响上述路径时纳入，须先登记其 HEAD、规则和独占路径。

真实产品消息 ID 按当前模型处理：App CLAUDE.md 记录 MessageModel.id 为 Xid 字符串，不得因旧计划泛称 TSID 而把消息 ID 转整数；UID/GID 与 JSON 64-bit ID 按 EntityId/安全解析合同处理。

### 三种终局必须分开

- `LOCAL_CANDIDATE_PASS`：AC-00…AC-21、AC-24…AC-26 全部在冻结候选上 PASS。只是本地实现、真库和域测试完成。
- `PRODUCTION_QUALIFIED`：AC-00…AC-29 全部 PASS；真机、攻击、升级演练、独立审查完成。实际投产仍需用户对明确目标单独授权。
- `TOP_TIER_PROFILE_PASS`：AC-00…AC-35 全部 PASS，且选定威胁模型、平台、规模、剩余风险公开明确。不是“绝对安全”或跨产品排名认证。
- `PRODUCTION_DEPLOYED`：仅用户另外授权后，以真实目标健康/版本/迁移/回滚证据成立；此计划默认 `NOT_EXECUTED`。
- 任一必需项 PENDING/BLOCKED/FAIL/SKIPPED：所属门不得 PASS。批准缩减范围须另建版本，不改原合同冒充完成。

## 2. 权限与保护边界

本次请求只授权计划和提示词。执行者获得此提示词后可开展隔离的本地代码、合成 fixture、测试与本地提交；真实设备/测试账号操作须有明确资源授权记录。不得使用已有账号、历史设备序列号或脚本默认值代替授权。

禁止 push、发布、生产迁移、外部服务写入、发信、通知、联系第三方；禁止真实用户数据、生产 PII 和真实秘密进入仓库。外部审计仅准备审计包，不自行联系机构。

禁止修改 `imboy/erlang.mk`、App `ios/*`、`macos/*`、`plugin/r_upgrade`、AGENTS/CLAUDE/记忆/managed skills；如关键修复需要保留区，记 `BLOCKED_PROTECTED_PATH` 并列出最小所需变更。此限制可能阻止 iOS/macOS 原生集成，不能以跳过这些平台换取全平台 PASS。

新产品决策须提交 reviewable decision card：备份保留范围、历史共享、可信账号根恢复、AI 身份信任锚、强制升级影响、见证节点归属及成本、MLS/PQ 默认启用、历史明文迁移或密钥销毁。已获明确决策需引用证据并核对仍适用，不重复询问。

`AI-ID=B/F2/R2/D3/M1` 是仓内历史审计记录；C00 查明具体定义与用户决策证据后保留兼容。顶尖目标不自动撤销历史风险接受，也不允许将它伪称恶意服务端防护已完成。

## 3. 调度、独占和集成

`MAX_ACTIVE_AGENTS=10`，按协调者、worker、reviewer、verifier 全部计数；默认 Coordinator 1 + Workers ≤7 + Reviewer 1 + Verifier 1。不能为了凑满数量拆无意义任务。ZCODE 能力不足 10 时减少并行，不伪造子 agent。

Coordinator 是 state/acceptance/recovery/resources 的唯一写者；worker 只写所属 worktree 与该卡证据目录。每仓创建独立 branch/worktree（每任务需要的仓分别创建）；不得让多个 agent 改共享目录。worktree 放 `.Codex/worktrees/e2ee-excellence/<run_id>/<card>/<repo>`。创建前确认原仓 Git 根。无需提交/移动/清理外国 WIP。

C00 建立 `ownership.tsv`：repo、exact_path、card、wave、lease_owner。下表路径是候选范围，W0 展开为精确文件；同文件跨卡只能分波串行或申请唯一集成人实现经审查的小补丁。共享路径包括 router、olm_session_service、group_session_service、attachment_api、CryptoStore、migration 目录、feature 生成物、发送/接收入口。未获 lease 不得编辑。

迁移编号仅 Coordinator 在当前所有执行者无冲突后分配，不预定 113 等编号。数据库、端口、vodozemac 原生库构建目录、Flutter build 输出和物理设备均登记独占 lease。共享设备同一时刻只跑一个 journey；不同专属 worktree 可并行纯单测。每个任务记录依赖 commit pair，禁止依赖未提交共享 WIP。

Worker 返回 commit、diff、验收原始证据；Reviewer 检查安全语义/调用链/测试真实性，不能只看测试数。Coordinator 在每仓 integration worktree cherry-pick 独立功能 commit，运行 L1 后更新 dependency SHA；冲突只在 integration worktree 解决并重验。身份命令级使用 `leeyi <leeyisoft@qq.com>`，不改全局 Git 配置；只 stage owned paths。

## 4. 测试层次与命令合同

- L0：受影响纯函数/模块负例，修改前 RED、修改后 GREEN；已有实现可提供当前行为证明，不强造 RED。
- L1：该卡实际生产调用链、相关域回归；禁止只测 mock helper 和源码字符串匹配。
- L2：合成账号真实服务器 + 真机/桌面发送接收、恢复、攻击旅程。不能用模拟器充当真机。
- L3：冻结各仓 candidate SHA；集成全量测试只由 C14 运行，worker 不重复跑全球测试。

C00 必须生成 `commands.json`，每个 AC 有 cwd、argv/env 名称、前置资源、timeout、oracle、expected_exit。以下是已存在入口，执行前核对 CLI 与环境。涉及账号、设备、fixture 写入的入口必须先检查授权。下列命令是入口模板；commands.json 必须填入专属 EUNIT_CONFIG（仅含合成 scratch DB）、EUNIT_RELAY_PORT/EUNIT_RELAY_TARGET/EUNIT_HTTP_PORT/EUNIT_HTTP_ADM_PORT 与 PGHOST/PGPORT/PGUSER/PGPASSWORD。秘密从不入仓的本地环境提供，不记录值。禁止沿用 sys.local、4323、15432、默认账号密码或脚本默认设备；eunit-local 会清自己的 build cache、并按 relay 端口匹配停止进程，因此只能在专属 worktree、专属端口上跑。没有配置 lease 禁止运行：

```sh
# 后端 worktree
make compile
IMBOYENV=local make eunit-local t=e2ee_logic_tests
IMBOYENV=local make eunit-local t=olm_identity_repo_tests
IMBOYENV=local make eunit-local t=group_e2ee_logic_tests
IMBOYENV=local make eunit-local t=e2ee_trust_logic_tests
IMBOYENV=local make eunit-local t=device_revocation_tests
bash scripts/test/run_group_attachment_acl_pg.sh
bash scripts/test/deploy_sequence_test.sh
bash scripts/test/migrate_gate_test.sh

# App worktree，先核对 full-selected 生成物
python3 ../imboy/scripts/generate_product_features.py --check --require-profile full-selected
flutter test test/unit_test/service/e2ee/ --reporter expanded
flutter test test/unit_test/service/e2ee_local_backup_boundary_test.dart test/unit_test/service/e2ee_backup_restore_test.dart --reporter expanded
flutter test test/unit_test/service/e2ee/attachment_encryptor_test.dart --reporter expanded
bash scripts/run_e2ee_suite.sh
# 合成会话跨平台检查，显式填授权 ANDROID_DEVICE_ID 后才允许执行
bash scripts/run_cross_platform_e2ee_interop.sh
bash scripts/run_cross_platform_e2ee_group_interop.sh
```

跨平台脚本目前是合成 session/native interop，不登录/连接真实聊天后端，不能满足 AC-22/23。

W0 如果 eunit `t=` 选择器与当前 Makefile 行为不同，纠正 commands.json 并绑定实际发现的命令，不改变测试意义；实际测试数必须 >0、无意外 skip。不得把空 EUnit、SKIP 或 infra failed 记 PASS。

新增测试放既有域；C00 将 proposed 测试的实际 filename 登记进 command registry，后续未登记测试命令不得成为最终证据。L1 后端按实际域 suite 运行；App 只分析相关 paths，L3 执行 `dart analyze lib test/unit_test --fatal-infos` 和 `flutter test test/unit_test --reporter expanded`。已存在无关失败保留失败证据，不能以“历史问题”免除完整门。

L3 后端 `IMBOYENV=local make eunit-local` 连续两轮 exit=0、nonzero test_count，TSID VM-global/bootstrap 与真 PG suite 按当前 Makefile 隔离策略另跑且逐项登记，禁止只过滤不补跑。PG 严禁共用开发/生产数据库；marker scratch DB 只 loopback，创建/删除准确匹配 owner/run 标记。容器/数据库是否可用先只读探测，不擅自安装依赖或启动外部目标。

## 5. 波次与任务卡

所有卡采用固定小步：①读所有调用方及历史合同；②补能暴露问题的负例并记录 RED（已有修复则行为证明）；③最小实现；④L0/L1；⑤独立 review；⑥本地独立功能 commit；⑦集成重验。不要重造框架、另建全套加密抽象或增加只有一个实现的接口。

### W0 — C00 基线、威胁模型、验收与资源台账（Coordinator）

Owner：此文档目录、运行元数据；只读所有产品源码。
读取：两仓规则、`imboy/docs/security/audits/e2ee-2026-09-07/`、`imboy/docs/reference/e2ee-protocol-specification.md`、`imboy/docs/compliance/`、`imboyapp/lib/service/e2ee/`、所有生产 caller 与真实测试入口。
步骤：重采样 HEAD/WIP → gap ledger 分类 → 合同与数据流图（发送/接收/设备/群/附件/恢复/AI）→ threat/profile decision cards → commands/ownership/resources → 以独立 verifier 核对 36 ID 全集。
AC-00：快照包含源码/规则/plan hash、WIP、生成物 profile、客户端支持矩阵；每条历史 finding 都有当前定位或未知理由。
AC-01：trust boundary/threat model 包含恶意服务端、被盗设备、恶意群成员、重放、离线、恢复及明文例外；待决项有 owner/门禁且不阻止无关工作。
AC-02：所有 36 AC 精确绑定 card/command/oracle/层次/资源；ownership 无同波冲突。W0 未通过不能派发生产代码 worker。

### W1 — 可并行底座（C01–C06；共享路径按 lease 拆开）

**C01 设备身份与撤销（Backend worker）**
Owner：`src/repo/olm_identity_repo.erl`、`src/logic/olm_identity_logic.erl`、`src/ds/olm_identity_ds.erl` 与对应测试；device repo/ds 仅获 lease 后。
步骤：核对 upsert/签名/认证绑定 → 同 DID 换钥生成明确版本事件，不静默覆盖可验证历史 → 设备撤销与 session/token/OTK/密钥枚举联合门 → scratch PG 并发验证。
AC-03：首次注册/合法轮换/非法换钥/重放各有真实 PG oracle，旧 key history 可追溯。
AC-04：撤销提交后新 claim、新密钥枚举、发送授权和重连均拒绝；重叠操作给出线性化点，已发密文不可撤回边界明确。
依赖 C00；L0 identity/device suite，L1 handler→repo 与 scratch PG。migration 由 C12 独占实现。

**C02 客户端协议协商和防降级（App worker）**
Owner：`lib/service/e2ee/device_manifest.dart`、`capability_negotiator.dart`、`capability_guard.dart`、`identity_verifier.dart`、对应测试；bootstrap/olm/路由接线 W2 由 C07 集成。
步骤：确认是否固定套件即可满足合同 → 需要协商时 manifest 签名绑定 uid/did/key version/capability version → 验签和高水位持久化 → 显式兼容错误，不退明文。
AC-05：伪签名、字段删除、旧 manifest 重放、回退套件均拒绝；固定协议方案也须有旧端拒绝负例。
AC-06：正常发送/重试/离线/outbox 恢复生产入口实际调用保护；实现 helper 无调用不能 PASS。
依赖 C00；L0 对应三件套；AC-06 等 C07 接线后验收。

**C03 备份与可信设备恢复（App worker）**
Owner：`e2ee_local_backup_service.dart`、`e2ee_server_backup_service.dart`、`e2ee_backup_setup_service.dart`、`e2ee/megolm_backup_section.dart`、备份导入导出页面及对应测试。
步骤：枚举备份包含/排除材料 → 决策确认历史恢复与身份重建语义 → 随机高熵恢复密钥或经参数评审的口令 KDF → 版本/账号绑定/AEAD/授权范围/抗回滚 → durable progress 与幂等恢复 → 精确成功/部分完成文案。
AC-07：错误口令、篡改、跨账号、旧版本、备份回滚、枚举失败 fail-closed；明确恢复材料，不把 RSA/Megolm 恢复称完整 Olm 恢复。
AC-08：第 N 条写入失败、进程终止和重试不会虚报成功；信任根变化可见。不把活跃 ratchet 克隆到两台设备，新设备有独立 DID/会话。
依赖 C00；历史 grant API 改动由 C08/W2 lease，账号信任根方案由 C09 合同决定。

**C04 附件完整覆盖与卫生（App worker）**
Owner：`lib/store/api/attachment_api.dart`、`lib/service/assets.dart`、`lib/service/e2ee/attachment_*.dart`、`lib/page/chat/chat/attachment_handler.dart` 和对应测试；实际媒体入口 W0 逐路径登记。
步骤：八类入口加转发/重试 trace → 每对象独立 content key/nonce 与认证 descriptor → 截断/换块/nonce 复用/跨会话替换负例 → 加密缩略图与清理临时明文。
AC-09：图片/视频/缩略图/语音/文件/位置图/转发/重试有全覆盖矩阵；服务器对象与消息仅含允许元数据和密文。
AC-10：任意块篡改/重排/截断/descriptor 替换拒绝；打开失败不暴露未验证明文；崩溃/取消/退出后的临时产物按策略处理。
依赖 C00；L0 AEAD/descriptor；L1 实际上传→消息→下载→打开。历史明文迁移不得自行执行。

**C05 端侧秘密、缓存与通知（App worker）**
Owner：`lib/service/e2ee/e2ee_secret_inventory.dart`、`db_encryption_key_service.dart`、`e2ee/crypto_audit_log.dart`、端侧安全相关测试；sqlite/通知/logging path W0 独占展开，不与 C04/C07 同时写。
步骤：secret/data inventory → Keychain/Keystore/SQLCipher 配置核查 → 崩溃/换号/登出/系统备份/通知/日志检查 → 合成 canary 检测。
AC-11：不可读密钥、SQLCipher 失败、换号和清理失败不静默降级；日志/错误/Push 默认不带 E2EE 正文或密钥。
AC-12：端侧缓存、OS 备份、截图/剪贴板/通知边界有可测试策略及平台证据，软件密钥不能标成硬件不可导出。
依赖 C00；本地 test，真机取证待 C13；保留区需要修改则 BLOCKED。

**C06 AI 与企业托管边界（跨仓 worker）**
Owner：`lib/service/e2ee/ai_plaintext_gate.dart`、`compliance_key_service.dart`、对应测试；后端消息/Agent 身份路径 W0 展开独占。不得同波写 C07/C08 shared message paths。
步骤：保留现有历史决策 → 独立签名身份锚设计与 fixture → 用户批准内容/目标/域绑定 → 异步变化与重试复核 → 企业托管恢复/审计权限明确。
AC-13：Agent badge/type 伪造、身份/目标变化、过期确认和重试不能把真人 E2EE 送入明文域。
AC-14：pure required、compliance/组织审计域、AI/企业托管域在 UI、API、附件和说明一致；顶尖 profile 需要独立信任锚，历史 AI-ID=B 风险接受不冒充完成。
依赖 C00 与身份锚 decision；无批准可完成本地威胁/fixture测试，不擅自改变线上默认策略。

### W2 — 跨链闭环（C07/C08/C09/C10；C07 与 C08 的 shared App 路径串行）

**C07 多设备身份、安全码与生产接线（App integration worker）**
Owner：`olm_session_service.dart`、`safety_number_service.dart`、`trust_record_service.dart`、`e2ee/trust_event_client.dart`、`e2ee/e2ee_bootstrap.dart`、`e2ee/e2ee_outbound_router.dart`、`message_retry.dart`、安全码/设备页面、对应测试。
步骤：接 C01/C02 合同 → 可信旧设备批准新设备 → 验证绑定 device-set digest/key version → 设备新增/换钥使信任过期 → sender own-device fanout/离线设备缺钥/outbox 原子性/ratchet 持久化故障测试。
AC-15：完整设备集合验证，不把聚合号码确认仅登记为首个设备；幽灵/遗漏/换钥设备触发策略。
AC-16：同一消息给 peer 与 self 各目标正确密文；重复/并发/崩溃重试不重复推进业务状态、不发未持久密文、不复用危险 ratchet state。
依赖 C01/C02，C03 根恢复合同、C09 identity profile；接线完成后重验 AC-06。

**C08 群成员、room-key 与历史授权（跨仓 worker）**
Owner：`group_session_service.dart`、`e2ee/megolm_protocol.dart`、对应测试；Backend `msg_c2g_logic.erl`、`e2ee_recovery_logic.erl`、group/member/history DS/Repo/attestation 具体路径 W0 展开。
步骤：成员 generation/seq 线性化 → 密钥分发 recipient/device snapshot 与有效 epoch 验证 → join/leave/kick/rejoin/dead device 竞态 → 显式限范围历史 grant → 保持已下载数据边界。
AC-17：撤销与发送/分发并发、漏 S2C、离线旧会话、重连后旧 recipient 均不得解密撤销后消息；失败触发刷新/rotate 或拒发。
AC-18：新加入/重入/恢复不得越过历史授权区间；伪造 room-key/session/gid/sender/seq、扩大 grant 或跨 generation 重放均拒绝。
依赖 C01/C03/C07；不得只凭服务端 attestation 声称抵御恶意服务端，客户端信任与验证纳入 C09/C10。

**C09 账号信任根、交叉签名与 Key Transparency 合同（安全 worker）**
Owner：本计划目录下 `identity-transparency-profile.md` 与 golden vector fixtures；不编辑生产共享源码。
步骤：研究官方协议与成熟方案 → 定义 account/device/root/recovery 签名关系及版本 → canonical leaf/head/域分离/撤销 → checkpoint/证明/防回滚/防分叉/隐私 → 独立 reviewer 评审并形成决策卡。
AC-19：两个独立实现对相同 golden vectors 的 canonical bytes/hash/signature/proof 一致，错误向量拒绝。
AC-20：独立 witness 或客户端交叉核验模型能检测 split-view；记录见证运营、信任锚、数据隐私和故障策略。只有 server-signed tree 不算完成。
依赖 C00；profile 未批准只能本地纯函数实验，不部署见证服务、不新增外部联系方式。

**C10 Key Transparency 与交叉签名实施（跨仓 worker）**
Owner：Backend `src/lib/e2ee_kt_merkle.erl`、新增 `e2ee_kt_*` handler/logic/ds/repo/test；App 新增 `lib/service/e2ee/key_transparency_*.dart`、`cross_signing_*.dart` 与 tests。现有 identity/device/olm caller 按 C01/C07 lease 串行接线。
步骤：append-only identity event 与 log 原子提交 → proofs/checkpoint API → 客户端 verify/cache/rollback检测 → witness/gossip fixture → 实际 upload/claim/recipient enumeration 接线。
AC-21：身份注册/轮换/撤销进入不可变日志；非法 inclusion/consistency、旧 head、目录遗漏、分叉、proof 缺失按策略阻断，不仅告警日志；每次生产 key trust 决策有证明链。
依赖 C01/C09 profile 批准。迁移 C12、router Coordinator 唯一 lease；server compromise oracle 必须独立于同一 server。

### W3 — 安全验证、迁移和冻结门

**C11 攻击矩阵与真实旅程测试（test worker）**
Owner：`imboyapp/integration_test/e2ee_release/`（新增）、`imboy/test/integration/e2ee_release_*`（新增）、本目录 `attack-matrix.md`；不修改生产实现，发现问题回原 owner。
步骤：合成测试账号/fixture contract → 正常单聊/群聊/附件/恢复 → 可控恶意服务器代理模式 → replay/乱序/篡改/身份目录攻击/耗尽 → 收集独立设备与服务器 oracle。
AC-22：两账号真实 C2C，Android/iOS/macOS 按支持矩阵双向覆盖文字/附件/离线/重连/多设备；双方显示内容与密文存储相对应。
AC-23：四账号 C2G 加入/踢人/重入/离线/换机，逐会话/seq 校验新旧成员实际解密能力；只 UI 截图不算 PASS。
依赖 C00 可先写 harness；完整运行依赖 C07/C08/C10、C13 授权资源。

**C12 迁移、旧端版本门、运维和 rollout 演练（Backend ops worker）**
Owner：获分配的 migration 文件、`deploy/`相关精确脚本、`scripts/test/`迁移/版本合同测试；先读 deploy/README 与适用规则。
步骤：schema backfill 最小权限 → 混合版本握手和最低能力/版本门 → staging clone 合成大数据演练 → expand/contract、锁时长和失败恢复 → alarms/runbook。
AC-24：全部新增/受影响迁移 up/down（可逆项）/重复/中断/失败恢复真 PG 通过，约束 predicate 准确；不可逆项事前确认、不得以 down 删除密钥历史。
AC-25：旧端不能绕过 required E2EE/附件 anchor/证明验证；明确客户端先发布与后端强制顺序；失败回滚不降级安全不变量。
AC-26：OTK 耗尽、证明失败、轮换/恢复/解密失败可观测，不暴露 uid/密钥等攻击择时信息；支持规模下负载测量满足冻结 SLO。C00 提出具体发送/恢复成功率、P95/P99、群规模、key fanout、资源预算，评审冻结后不得事后放宽。
依赖 C00、C01/C08/C10 schema；共享 migration/部署目标独占。此卡只本地或获授权预生产演练，不部署生产。

**C13 真机与明文泄露取证（device runner）**
Owner：测试证据；设备/合成账号/loopback backend DB/queue/log/object/Push stub 专属租约，不编辑生产代码。
步骤：用户明确资源授权 → 设备备份与非破坏前置 → 运行 C11 journeys → seed 唯一 canary → 搜索传输/DB/队列/日志/备份/对象/Push → 端侧 OS/临时产物取证 → 恢复 fixture。
AC-27：AC-22/23 + AC-07/08/10/12/15/17/18 所需真机负例在冻结候选重验，未授权/缺平台只能 BLOCKED。
AC-28：真人 E2EE canary 与内容密钥在服务端所有约定面不存在；预期端侧明文只出现在允许出口，服务器截获/篡改不能绕过身份和信封检查。记录检索范围与局限，HTTP200/截图不能代替。
依赖 C11/C12，设备不得沿用脚本默认 ID；只读取证不可改变 OS 保护设置来制造 PASS。

**C14 独立审查、冻结 L3 与发布裁决（Reviewer + Verifier，顺序）**
Owner：review/qualification/verdict；只读源码、账本和不可变 evidence。Verifier 不是实现作者或自评程序。
步骤：冻结每仓 candidate SHA/配置/生成物/依赖锁 → L3 两轮后端与一轮完整 App test+analyze、隔离专项、真实旅程重验 → 安全 reviewer 审查 → hash manifest → 从原始 evidence 重算 AC → 定论。
AC-29：无未解决 CRITICAL/HIGH；MEDIUM 有明确修复或用户接受与产品限制；36 IDs/证据/命令/exit/oracle/candidate 精确对齐，所有生产必需项 PASS；外部独立审计未执行则明确其审计范围未完成，不冒充第三方认证。
依赖 C00–C13；本地门可在 C13 未授权时独立裁决 LOCAL 状态，但绝不能升级 PRODUCTION。

### W4 — 行业领先协议扩展（在现有闭环后推进，不替代 W3）

**C15 MLS 群组泄露后恢复（协议 worker）**
Owner：本目录 `mls-profile.md`、兼容 vectors、App 新 `e2ee/mls_*.dart` 和专属 tests；生产 bootstrap/群发送接线需 C07/C08 后独占。
步骤：选成熟实现/许可证/FFI平台矩阵 → 明确 credential/auth 绑定 C09/C10 → epoch/commit/welcome/fork/offline 负例 → 被盗 key 后 honest update 的恢复攻击实验 → 默认策略与迁移决策 → 生产接线并真机重验。
AC-30：标准 vectors 与独立实现互操作，remove/rejoin/epoch rollback/非法 commit/replay/长离线正确；支持群规模满足冻结预算。
AC-31：攻击者持泄露旧 state，诚实成员完成安全更新后不能解密后续消息；旧 Megolm 历史与新 MLS 世代迁移隔离，UI/API capability 与实际协议一致。
依赖 C09/C10/C14 本地门；没有生产接线与真机证据只能 PROTOTYPE_PASS。

**C16 混合抗量子单聊与持续棘轮（协议 worker）**
Owner：本目录 `pq-profile.md`、vectors、App 新 `e2ee/pq_*.dart` 与专属 tests；olm/outbox/capability caller 与 C15 接线串行。
步骤：采用成熟 PQXDH/混合 handshake 与持续 PQ ratchet 实现，核查上游审计/许可证/版本 → hybrid binding/downgrade/参数/DoS → state durability 与并发 → 平台性能 → rollout 决策与生产接线。
AC-32：独立实现 interop 与官方 vectors/算法对应，混合组件失败/替换/降级均 fail-closed；不能把普通 Olm 或一次 PQ handshake 叫持续抗量子安全。
AC-33：泄露后更新/乱序/重放/崩溃恢复真实实验符合选定协议保证，Android/iOS/macOS 真机与大小/时延预算通过，历史 Olm 与新 suite 清晰隔离。
依赖 C02/C07/C09/C14 本地门；许可/保留区/平台缺失 BLOCKED，不自行改为较弱协议并称完成。

**C17 元数据保护与持续安全维护（privacy/release worker）**
Owner：本目录 `privacy-profile.md`、供应链/漏洞响应文档、相关 scoped test；具体 Push/日志/发送路由精确路径分配后修改。
步骤：枚举可见 uid/gid/时序/长度/IP/设备关系 → 按威胁优先级最小化、padding/Push/发送者隐私评估 → 流量实验 → parser fuzz/恶意长度与资源耗尽 → SBOM、签名发布/可复现范围、漏洞响应演练与审计包。
AC-34：元数据可见性矩阵与真实流量一致；实施的保护有 before/after oracle、开销预算；不宣称隐藏服务器仍可见的信息。
AC-35：冻结 parser/backup/descriptor/proof/handshake fuzz corpus 可重跑，崩溃/DoS 已修；依赖版本/许可证/CVE和漏洞响应有 owner，外部审计包完整。TOP门的独立安全复核覆盖 C15/C16/C17，并重验 AC-29。
依赖 C05/C06/C12，可先做 inventory；全验收依赖 C15/C16。不得擅自发布安全公告或发起第三方联系。

## 6. 权威验收全集（禁止仅按任务数宣告完成）

| Card | Required IDs |
|---|---|
| C00 | AC-00, AC-01, AC-02 |
| C01 | AC-03, AC-04 |
| C02 | AC-05, AC-06 |
| C03 | AC-07, AC-08 |
| C04 | AC-09, AC-10 |
| C05 | AC-11, AC-12 |
| C06 | AC-13, AC-14 |
| C07 | AC-15, AC-16 |
| C08 | AC-17, AC-18 |
| C09 | AC-19, AC-20 |
| C10 | AC-21 |
| C11 | AC-22, AC-23 |
| C12 | AC-24, AC-25, AC-26 |
| C13 | AC-27, AC-28 |
| C14 | AC-29 |
| C15 | AC-30, AC-31 |
| C16 | AC-32, AC-33 |
| C17 | AC-34, AC-35 |

共 18 张卡、36 Required Acceptance IDs。每 AC 可分 subcase，但不得删父 ID 或用 N/A 抹去缺失平台。ALREADY_FIXED 也必须以当前冻结候选行为证明结案。

## 7. 证据、状态与恢复合同

运行根（本地不入 Git）：`.Codex/evidence/e2ee-excellence/<run_id>/`。仅合成数据/脱敏日志，真实秘密与 PII 不入工作区。

必需文件：`baseline.json`、`gap-ledger.tsv`、`decision-register.json`、`ownership.tsv`、`resources.json`、`commands.json`、`state.json`、`acceptance.tsv`、`recovery-ledger.tsv`、`candidate.json`、`manifest.sha256`、`verdict.json`；每卡 `cards/Cxx/` 保存 command/stdout/stderr/exit/testcases/oracle/report。Coordinator 单写状态，append-only recovery ledger 不重写历史。

acceptance 每行必需：`id,card,status,plan_sha,backend_sha,app_sha,other_repo_sha,config_hash,command_id,command,exit_code,test_count,oracle,evidence_path,evidence_sha256,reviewer,completed_at`。PASS 要求 exit=0、非零行为断言、当前候选、原始证据和独立 review；截图只能补充。修改测试/代码/配置/生成物/fixture/runner 后使相关证据失效；默认最终冻结全重验，不按“看起来没影响”重签旧结果。

状态：`PENDING → READY → RUNNING → REVIEW → LOCAL_PASS → INTEGRATED → FROZEN_RETEST_PASS`；失败分 `FAIL_CODE/FAIL_SECURITY/BLOCKED_INFRA/BLOCKED_USER_AUTH/BLOCKED_DECISION/BLOCKED_PROTECTED_PATH/BLOCKED_SHA_DRIFT`。LOCAL_PASS 不直接填 AC PASS；最终 AC PASS 只来自冻结候选证据。

| Failure | Recovery | Retry | Next State |
|---|---|---|---|
| 代码/断言失败 | 原 owner 最小修复、负例、review、新 commit | 同 root cause 最多 2 修复轮，第三次升级协调者根因分析 | READY 或 FAIL_CODE |
| 安全负例失败/明文泄露 | 保留脱敏证据；停止该数据链及依赖卡；修根因、重新安全 review | 禁止仅重跑碰运气 | FAIL_SECURITY，修复后 READY |
| 原生库/编译/infra 缺失 | 核对当前平台/toolchain/已有 bootstrap；获租约修复，不动保留区 | 同 fingerprint 最多 2 重试 | BLOCKED_INFRA |
| 设备/账号/决策未授权 | 输出具体所需资源/动作/影响，继续不依赖的卡 | 无自动重试、无默认值替代 | BLOCKED_USER_AUTH/DECISION |
| merge 冲突/HEAD 漂移 | integration worktree 重采样、范围对比、rebase/cherry-pick、失效证据、重验 | 一次干净重建；仍冲突交 Coordinator | READY 或 BLOCKED_SHA_DRIFT |
| crash/超时/失联 | 先只读对账 commit/WIP/lease/process/fixture，收旧 agent 停止确认后回收该 lease | 不确定消息发送/导入/迁移禁止盲重放；靠 ledger/幂等键证明再续 | READY 或 BLOCKED_INFRA |
| PG fixture/迁移失败 | 保留失败与精确 owner marker；只清该 run 合成资源；回滚遵守可逆性 | 仅隔离库允许 1 次重建复跑 | READY 或 FAIL_CODE |
| 终验失败/证据缺字段/hash不符 | 修源头或补真实重验，不手写 PASS | 修复一次再跑受影响门；必要全 L3 | FAIL_GATE |

每卡启动、完成和长任务每 ≤5 分钟 heartbeat 持久化；15 分钟无 heartbeat 先调查，不立即启动双 worker。纯 L0 默认 15 分钟、compile/L1 30 分钟、PG/设备单 journey 45 分钟、L3 单次 120 分钟；超时先查是否活跃、保留日志，续租需记录原因/新期限，最多一次。最大 wall time 不是 PASS 条件。

恢复顺序：读计划 hash → read state+ledger → 核对实际 HEAD/进程/lease → 修正不一致为 UNKNOWN → 安全回收 → 从最后已验证 checkpoint 继续。任何 uncertain 外部写入都等待用户核实，禁止自动补发或重播。

## 8. 最终验收与交付

Verifier 只读重算：36 ID 精确集合；各层门必需集合；所有 PASS 字段；命令真实运行及 nonzero oracle；manifest；candidate 是否含全部已审 commit；冻结候选与最终集成 HEAD 一致；无遗留运行 worker/冲突 lease；授权/决策与残余风险。

输出 `LOCAL_CANDIDATE=PASS|FAIL|PARTIAL|BLOCKED`、`PRODUCTION_QUALIFIED=PASS|NO_GO`、`TOP_TIER_PROFILE=PASS|NO_GO`、`PRODUCTION_DEPLOYED=NOT_EXECUTED`，以及未满足 ID、原因、下一步和设备/外部边界。发现外部权限缺失时停对应卡，继续独立工作；不能把整个任务提前称完成。

交付：每仓可审查独立 commit、精确 candidate pair、协议/恢复/隐私合同、36-ID账本、immutable evidence、攻击矩阵、迁移/rollout/回滚 runbook、独立审查、最终 verdict。不 push、不自动部署。
