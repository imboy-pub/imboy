# 已知问题台账（Known Issues Ledger）

> 审计就绪包**第 5 件工件**（P5-5）。对外披露口径的单一来源。
> 最后核对：2026-08-02 ｜ 负责人：leeyi（solo）

## 0. 这份台账和 gap-matrix 的区别

| | gap-matrix | 本台账 |
|---|---|---|
| 组织方式 | 按标准条款（TT-A/B/C/D/E） | 按**问题** |
| 目的 | 追踪进度 | **对外披露** |
| 读者 | 我们自己 | 审计方 / 买家安全团队 |

**收录标准**：只收「已知且今天未消除」的问题。已修复项归 gap-matrix，不在此处邀功。

**状态定义**：
- `Acknowledged` —— 已知、已评估、**有意接受**，附理由。
- `Open` —— 已知、**未接受**，正在或计划修复。
- `Blocked` —— 已知，修复受外部条件阻塞（凭证、真机、他人决策）。

**严重度**：按对**机密性/完整性**的实际影响定级，不按修复难度。

---

## 1. 分发阻断（Critical）

未解决前**不得对外分发**：包括试用版、演示 APK、应用商店上架、私有化交付。
内部开发使用不触发。

| ID | 问题 | 状态 | 理由 / 处置 |
|---|---|---|---|
| IMB-2026-001 | `vodozemac` / `flutter_vodozemac` 0.5.0 为 **AGPL-3.0**。网络条款要求向使用者提供完整对应源码，与私有化售卖不相容 | `Open` | 已拍板③：基于上游 **Apache-2.0** 的 vodozemac Rust crate 自建 FFI 绑定，保持 `fvod` 调用面兼容使 `lib/` 零改动。追踪 **X15**，未实施 |
| IMB-2026-002 | `simple_captcha` 无 LICENSE 正文，`app.src` 的 `licenses` 为空 | `Open` | **无许可证 = 无再分发权**，与 AGPL 同级阻断。我方在 gitee 的 fork，需查明上游补回 LICENSE；查不到则替换或自研 |
| IMB-2026-003 | `ic_storage_space` 的 LICENSE 文件内容**仅有 `Copyright 2021`**，无任何授权条款 | `Open` | 同上 |
| IMB-2026-004 | `jwerl` 无 LICENSE 正文，仅 `app.src` 元数据声明 BSD-3 | `Open` | 低风险，从上游补回正文即可 |

> 001–004 由 `scripts/license_inventory.sh --check` 机器可查。详见
> [`../../../legal/third-party-licenses.md`](../../../legal/third-party-licenses.md)。

---

## 2. 名义防御与运行时不符（High）

**这一节是本台账最重要的部分。** 以下防御的代码与测试都在仓内，但**运行时不生效**。
仅凭代码存在性或测试通过来判断防御到位，会得出错误结论。

| ID | 问题 | 状态 | 理由 / 处置 |
|---|---|---|---|
| IMB-2026-005 | **附件今天以明文存于对象存储**。分块 AEAD、AAD 绑定、加密闸门、临时明文清理、缩略图独立 key 全部已实现且有 13 个测试文件，但**加密开关未翻开** | `Blocked` | Slice 9 真机验证 BLOCKED + 两项待拍板（**X12**）。后果：威胁模型 **T10** 的 ATT-01..05 全部不成立。**这是当前状态，不是设计意图** |
| IMB-2026-006 | **Safety Number 生产零调用**。`grep -rl "SafetyNumber" lib/` 只命中它自己 —— 算法有守护测试 `e2ee_safety_number_test`，产品用不到 | `Open` | 威胁模型 §4 矩阵原本把它列为 T2/T8 的防御点，**该防御在今天的产品里事实上不存在**。追踪 **B1 / P3-4 / P3-5** |
| IMB-2026-007 | **KT 未部署**，服务端分叉视图（split-view / non-inclusion）不可检测 | `Open` | Transparency Profile v1（ADR 29）已冻结树结构/STH/proof/golden vectors，无运行时。追踪 **P3-8**。与 006 叠加后，T11 今天**无任何可用检测手段** |

---

## 3. 有意接受的设计取舍（Medium，`Acknowledged`）

以下均为**主动选择**，不是疏漏。每条给出为什么这个取舍是对的。

| ID | 问题 | 理由 |
|---|---|---|
| IMB-2026-008 | **单聊历史换设备后不可恢复** | Olm session 状态含发送侧棘轮位置，跨设备恢复会造成密钥重用 + 棘轮分叉，前向保密与后向保密同时失效。Megolm inbound session 只读故可备份，群聊历史**可**恢复。Signal 与 Matrix 同此取舍 |
| IMB-2026-009 | **元数据不保护**（发送方/接收方/时间/长度/在线状态/关系图谱/流量模式） | IM 固有特性，与 Signal / WhatsApp 一致。隐藏需要 mixnet 级代价 |
| IMB-2026-010 | 备份 KDF **无 HSM、无硬件绑定** | PBKDF2-HMAC-SHA256 / 310,000 轮对齐 OWASP 建议下限。硬件绑定会使跨设备恢复失效，与备份的目的冲突 |
| IMB-2026-011 | **无后量子保护** | 明确列为路线图，不在当前范围。R3 已裁定不做 MLS/PQ 深水区 |
| IMB-2026-012 | 附件对象大小与上传时序仍是元数据 | 同 009。填充会显著抬高流量成本 |
| IMB-2026-013 | Megolm room key 分发列表长度暴露群设备数量级 | 4096 条上限只为防 DoS，不隐藏规模 |
| IMB-2026-014 | 设备 `is_active` 查询在 DB 不可用时 **fail-open**（放行，最长 60s 缓存 TTL） | fail-closed 会让一次 DB 抖动把所有 did 绑定 token 的用户**全端踢下线**。上限明确：故障期间已移除设备最长多存活 60s。需强一致吊销时改 fail-closed + 熔断降级 |

---

## 4. 已知未决（Medium / Low，`Open` 或 `Blocked`）

| ID | 问题 | 状态 | 说明 |
|---|---|---|---|
| IMB-2026-015 | **`trust_audit` 在账号注销时未清**（含 actor_uid / target_uid） | `Blocked` | 审计留存 vs 被遗忘权是**政策判断，不是代码判断**，需显式拍板，不在代码里默默删。olm 三表与两张备份表的注销级联已补 |
| IMB-2026-016 | **fallback key 签名非必填** | `Acknowledged` | 今天若强制必填，无客户端发签名 → 所有设备发布不了 fallback key → 每次耗尽变 `no_prekey_available`，是**可用性事故**。用指标 `olm_fallback_unsigned_total` 判断第二阶段启动时机。攻击者可「干脆不带签名」绕过——这是接受的窗口 |
| IMB-2026-017 | **SQLCipher 分组密码 / HMAC / KDF 全用库默认值**，我方未显式选定，也未做安全评估 | `Open` | `cipher_page_size` / `kdf_iter` / `cipher_hmac_algorithm` 三个 PRAGMA 在 `lib/` 零命中。**2026-08-02 密码学清单新发现** |
| IMB-2026-018 | 客户端 e2ee API **fail-open 残留**（查询侧） | `Open` | 第一批写操作已改抛（`8b4330fb`）；查询侧经下游 fail-closed 属有意保留，待第二批复核。追踪 **X8 / P3-2** |
| IMB-2026-019 | **XFF 限流根基被推翻**：取最左 forwarded IP = 攻击者可控 | `Blocked` | OTK claim 限流与备份端点限流的有效性**依赖该修复**，而修复在别线（sellable #5）。追踪 **X14** |
| IMB-2026-020 | 身份键就地覆盖**无痕迹** | `Open` | 无 append-only 历史则旧身份被替换不可追溯。追踪 **B3** |

---

## 5. 验证与流程缺口（Low / Info）

不直接构成漏洞，但影响审计方对证据强度的判断 —— **不列出即是隐瞒**。

| ID | 问题 | 状态 | 说明 |
|---|---|---|---|
| IMB-2026-021 | **真机双端从未验证** | `Blocked` | Olm 收发链实现完成，但双设备真机端到端从未跑过。追踪 **X2 / A1 / P2** |
| IMB-2026-022 | **无跨进程双端测试 harness** | `Open` | 今天只有单进程双 Account round-trip；无进程/网络操纵能力，故无法做降级、分叉、故障注入类测试。追踪 **C2 / C4 / P4-2** |
| IMB-2026-023 | elvis 与文件大小**棘轮基线于 2026-08-02 重设**（8824/239→10352/298；6→13） | `Acknowledged` | **债务被承认，不是债务被修复**。两道门自建立起从未真正跑过（origin 指向 gitee），期间代码长出 7 个新超限文件与 1528 条新违规。停在旧值只会让门永久常红，常红的门等于没有门。13 个超限文件清单已写进 workflow 注释 |
| IMB-2026-024 | 全量 eunit **> 40 分钟**不可用 | `Open` | harness 结构性慢，导致全量套件不进 CI 关键路径。追踪 **X7 / P4-7** |
| IMB-2026-025 | **SonarCloud 未接通** | `Blocked` | `Not authorized or project not found`，需 `SONAR_TOKEN` 与在 sonarcloud.io 建项目。其余 4 条后端工作流已全绿 |
| IMB-2026-026 | 全量套件 **6 例环境依赖失败**，CI 中暂排两个文件 | `Acknowledged` | 5 例直接实例化 `E2EEApi` 打真实后端 + 1 例断言 macOS 平台行为。**正解是测试注入 mock / 平台守卫，不是排除**；P4 段复核撤销。追踪 **X16** |
| IMB-2026-027 | 附件加密、KT、交叉签名的**文档先于实现** | `Info` | ADR 27/29 与威胁模型均已写明「未部署 / 开关未翻开」，但阅读时仍需注意：**规范冻结 ≠ 已上线** |

---

## 6. 审计方使用说明

1. **不要把 gap-matrix 的 ✅ 读成"运行时生效"**。本台账 §2 三条就是反例。
2. 分发阻断项（§1）机器可查：`scripts/license_inventory.sh --check`，当前**必然退出 1**（AGPL 还在），这是预期行为。
3. `Acknowledged` 项若你不认可其理由，请把它当作 finding 提出 —— 理由写在这里正是为了让它可被反驳。
4. 本台账**不含**已修复项。想看修了什么，查 gap-matrix 与 git log。
