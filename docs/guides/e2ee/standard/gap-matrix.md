'# Gap Matrix：标准 → 现状 → 证据 → 关闭任务（2026-08-01 核查基线）

> **层**：差距层 ｜ **基线日期**：2026-08-01（三代理 file:line 实证核查）
> **维护**：每任务完成/新差距发现时改行；**不重排结构**。任务编号=Hardening Plan（`.claude/PRPs/plans/e2ee-top-tier-hardening.plan.md`）。
> **图例**：✅具备（待证=验收未闭环） / 🟡部分 / ❌缺失 / 📋路线图

## A 类：协议正确性

| TT | 现状 | 证据（file:line） | 关闭任务 |
|---|---|---|---|
| A1 | 🟡 实现完成待真机 | Olm 收发链 `chat_network_service.dart:595-699`；`olm_session_service.dart:735-818`；⚠️真机双端从未验证 | P2-1/P2-4/P2-5 |
| A2 | 🟡 版本有，单一规范文档缺 | PFv3/meta_version v3 在产；无统一 spec 白皮书 | P5-3 |
| A3 | 🟡 轮换已实现，单调性对抗缺，成本未实测 | 三触发器 `group_session_service.dart:60-65,186-191`；矩阵#16❌ | P3-10、P4-4 |
| A4 | 🟡 | E2EE-031 PENDING；ratchet PFS/PCS 单测在 `olm_ratchet_pfs_pcs_test.dart` | P3-6、P2（真机腿） |
| A5 | 🟡 依赖库已审计；本地 pickle 层未核 | vodozemac LA 2022 Issue A；`olm_session_service.dart` pickle/`_secureBytes` 曾记弱随机（待复核现状） | P5-4（核验+登记） |
| A6 | ✅待证 | canonical/AAD 体系（trust 11 字段/附件 chunk_count 自洽门/域分离前缀） | P1 重验闭环 |

## B 类：用户可验证性

| TT | 现状 | 证据 | 关闭任务 |
|---|---|---|---|
| B1 | ❌ 算法在零 UI；交叉签名地基零接线 | `safety_number.dart` 生产零调用；`identity_verifier.dart:63-125` 仅 test 引用 | P3-4、P3-5 |
| B2 | ✅待证 | TOFU fail-closed `olm_session_service.dart:692-725`；S2C 告警 `e2ee_peer_key_warning_rule.dart`+`chat_page.dart:632` | P1/P2 闭环 |
| B3 | 🟡 Merkle 库完成未接线；profile 未签字；身份键就地覆盖无痕 | `e2ee_kt_merkle.erl`（golden 钉死）；`olm_identity_repo.erl:46` ON CONFLICT DO UPDATE；bigserial 失效已实证 | P3-8（依赖 P0-1） |
| B4 | 🟡→**群聊已可恢复**（2026-08-02，imboyapp `9426e7e4`）：备份含 `megolm_inbound` 段（收集/回填/往返实证 9/9，v1 旧包兼容）；**1:1 Olm 历史仍不可恢复=有意设计**（跨设备还原双棘轮会 key reuse/ratchet 分叉，同 Signal/Matrix）；**残留=该限制的 UI 明示与恢复文档未做** | `megolm_backup_section.dart`；`e2ee_local_backup_service` pack/unpack；导入页 `_applyRestoredKeys` | UI 明示 + P5-4 文档 |
| B5 | ❌ 无策略文档 | 强制验证先例=Element 2026-10 | P3-4 配套、P5-1 |
| B6 | 🟡 TOFU+变更告警有；KT 自动检测无 | 同 B2/B3 | P3-8 |

## C 类：自动化测试

| TT | 现状 | 证据 | 关闭任务 |
|---|---|---|---|
| C1 | 🟡 双实现 golden 有先例，未体系化 | trust canonical SHA256 锚；`e2ee_kt_merkle_tests.erl` golden | P4-3 |
| C2 | ❌ 零跨进程双端 | 仅单进程双 Account round-trip（`room_key_olm_roundtrip_test.dart`） | P4-2（旗舰） |
| C3 | 🟡 覆盖较强但 6❌ | `TEST_COVERAGE_MATRIX.md`：#3/#4/#7/#9/#15/#16❌，#12⚠️ | P4-4、P3-5/3-6/3-10 |
| C4 | ❌ | 无进程/网络操纵能力 | P4-2 配套 |
| C5 | 🟡 回归散落在各测试，无公开索引 | evidence 系列 vs 测试名未建映射 | P4-9 |
| C6 | ❌ **CI 从未运行**（origin→gitee；GitHub 落后 84/114/12） | 2026-07-31 审计报告；`.github/workflows/backend-ci.yml` 存在但未执行 | P0-3、P0-4、P4-1 |
| C7 | 🟡 auditor 未建 | KT 未接线 | P3-8+P4-1 |

## D 类：审计就绪

| TT | 现状 | 证据 | 关闭任务 |
|---|---|---|---|
| D1 | ❌ | ADR 00-30 散落，无统一白皮书 | P5-3 |
| D2 | 🟡 08-threat-model 停留在旧协议态 | `v2/08-threat-model.md`（未含 PFv3/附件/KT） | P5-1 |
| D3 | ❌ 无清单；AGPL 未标注 | pubspec.yaml:221-222 vodozemac ^0.5.0 AGPL-3.0；仓内 grep AGPL 零命中 | P5-2（联动 P0-6） |
| D4 | ❌ | 生命周期各环节散落各 ADR | P5-4 |
| D5 | 🟡 就绪包未组装 | 审计交付物惯例已调研（research/public-audit-cases.md §3） | P5-6/5-7/5-8 |
| D6 | 🟡 残留项散落各 evidence，无统一台账 | evidence/E2EE-*.md 残留段 | P5-5 |
| D7 | 🟡 本地可跑但无审计向说明 | `make eunit-local`/`make e2ee-verify` 在；全量 eunit >40min 不可用 | P4-7、P5-6 |

## E 类：服务端零信任与运维

| TT | 现状 | 证据 | 关闭任务 |
|---|---|---|---|
| E1 | 🟡 机器可查下限有，全量可执行证明无；**吊销级联缺失** | PEM 拒收/密文保真/FTS 排除；`user_device_repo:delete/2` 不清 olm 三表；`user_ds:delete_all_related_data` 不含 olm_* 与 e2ee_key_backups | P3-3、P4-6 |
| E2 | 🟡 PBKDF2-310k+端点限流；无 HSM/OPAQUE；XFF 限流被推翻（别线#5） | `e2ee_crypto_service.dart`；`sys.config` throttle；`elib_req:first_forwarded_ip/2` 取最左 | P3-9、台账 Acknowledged |
| E3 | 🟡 trust_audit 有；KT 日志无 | 迁移 44/47 append-only | P3-8 |
| E4 | ❌ 吊销不清键=被吊销设备仍可被 claim | 同 E1 证据 | P3-3（🔒需重新拍板，2026-07-25 曾暂缓） |
| E5 | 🟡 append-only 有，树头签名无 | `trust_audit_repo.erl`（无 update/delete） | P3-8 后补签名 |
| E6 | 🟡 SECURITY.md 待审 | — | P5-9 |

## R 类：路线图登记

| TT | 状态 | 说明 |
|---|---|---|
| R1 PQ | 📋 | 用户 2026-08-01 决策不纳入；扩展位=capability_negotiator securityRank+ProtocolSuite 占位 |
| R2 MLS | 📋 | ADR 19 维持 Proposed；ADR 31 记录"属性达标即可"决策 |
| R3 形式化 | 📋 | 成本锚=2.5 人月（PQ3 Tamarin） |
| R4 元数据 | 📋 | 台账 Acknowledged 项 |

## 横切执行风险（非标准条目，但必须跟踪）

| # | 项 | 状态 | 任务 |
|---|---|---|---|
| X1 | E2EE-012/024/025 三个 PASS 复核判不成立（验收建立在生产旁路） | ✅ **已重签 PASS**（2026-08-02：回退→生产路径电池 40/40+套件 588+后端 387 全绿，evidence/E2EE-P1-reacceptance-2026-08-02.md） | 已关闭 |
| X2 | 真机双端从未验证（真机腿全在 22 号文件停放区） | ⛔人工门 | P2 全阶段 |
| X3 | 双仓 411 提交未推送（imboy 248/app 163） | ✅ **已推送三仓对齐**（2026-08-02：imboy→345da003、imboyapp→9711b3b8，gitee/github/gitcode） | P0-4（CI 首轮观察） |
| X4 | pro 缺 Olm 端点部署（代码已全，路由 155-163） | ✅ **已在线（2026-08-02 实证）**：生产跑 alpha.17（07-29 部署），8 Olm 端点+trust/record+backup 全部 200/902 鉴权正常；前提记忆已过时。**全量升级**（当前 main + 迁移 53）另行——迁移 53 是 sellable 线 user_collect_info_recrypt（连带密钥轮换），须该线 runbook 对齐，不可顺手跑 | P2 端点侧已解锁 |
| X5 | AGPL 发布门（flutter_vodozemac AGPL-3.0） | ✅ **已拍板③自建 Apache-2.0 绑定**（2026-08-02） | X15 实施；分发前必须完成 |
| X6 | ADR 14-19 签字门（cross-signing/064/KT profile 全 BLOCKED） | ✅ **已全签**（2026-08-02 leeyi solo；19 维持 Proposed） | P3-4/3-8/3-11 解锁 |
| X7 | 全量 eunit >40min 不可用（harness 结构性慢） | ❌ | P4-7 |
| X8 | e2ee 三 API 文件 fail-open（olm_api/e2ee_api/e2ee_backup_api 0 throwIfFailed） | ✅ **第一批已修**（2026-08-02，imboyapp `8b4330fb`）：olm 三写操作+备份链三方法改抛（e2ee 591/591 零回归）；**保留=有意决策**：countPrekeys null 语义、查询侧经下游 fail-closed、reportDeviceKey ok:false 已有检查——见下轮复核 | P3-2 第二批（查询侧+调用方深化） |
| X9 | policy 缓存无 TTL | ❌ | P3-9 |
| X10 | E2EE-027 outbox 残留（读侧未接线/非同一事务） | ✅ **读侧已关闭**（2026-08-02：confirmOutbox 接入 ACK 汇聚点+重发 byte-for-byte 实证 3/3；纠正"重发重新 encrypt"为不实——实际复用库中信封；原子性维持 Acknowledged=ADR 02 冻结项） | evidence/E2EE-027-read-side-wiring-2026-08-02.md |
| X11 | E2EE-062 残留 7 项（拼接实证/租约 TTL/fallback 验签守护/降级守护/告警规则等） | 🟡 | P3-7 |
| X12 | 附件加密开关未翻开（Slice 9 真机 BLOCKED+两项拍板） | 🟡 | P2-2 |
| X13 | 并发会话领地：imboy 9 个 staged 文件（AI 助手/消息策略线）、imboyapp 7 个 staged 文件——**勿碰勿卷入**；e2ee-verify 当前红=并发线 `user_handler→user_export_logic` 模块边界违规，归该线 owner 修，本线不代修 | ⛔别线领地 | P0-8（已登记，2026-08-02） |
| X14 | XFF 限流根基被推翻（sellable 线 #5：`elib_req:first_forwarded_ip/2` 取最左=攻击者可控）——OTK claim 限流、备份端点限流的有效性**依赖该修复**；修复落地后本线复核（P3-9） | ⛔别线依赖 | P0-8（已登记）→P3-9 复核 |
| X15 | 自建 Apache-2.0 FFI 绑定替换 flutter_vodozemac（R4 裁定 ③）：基于 Apache-2.0 vodozemac crate 写自有绑定（flutter_rust_bridge 先例在仓），保持 `fvod` 调用面兼容使 lib/ 零改动；完成后 NOTICE 更新+许可证扫描进 CI | ❌ 待实施（排期建议 P3 段，不阻塞 P1/P2） | P0-6 后续（2026-08-02 裁定） |
| X16 | 全量套件 6 例环境依赖失败（CI 首跑暴露，2026-08-02 全部定位）：e2ee_health_check_service_test ×5（直接实例化 E2EEApi 打真实后端，本地 9800 在跑则全绿）+ db_migration_encryption_test ×1（isEncryptionSupported 断言 macOS 平台行为）。处置=ubuntu CI glob 暂排两文件（已加注释）；**正解=测试注入 API mock/平台守卫（非排除）**，P4 段复核撤销排除 | 🟡 暂排中（coverage 步已随全量绿恢复阻塞力） | P4 复核 |
| X17 | 格式门红因定位（2026-08-02 worktree 复刻实证）：`test/page/chat/chat/chat_page_utils_test.dart` **HEAD 提交版**是旧格式；别线工作树已改好（本地门 0 changed）但未提交→CI 检出旧版必红。处置=格式门暂排该单文件（已加注释），**别线提交落仓后必须撤销排除** | 🟡 门暂排中 | 别线提交后撤销排除即关 |
