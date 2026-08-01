# IMBoy E2EE 顶级标准（Top-tier Standard）2026 版

> **版本**：2026.1 ｜ **生效**：2026-08-01 ｜ **上位决策**：`../v2/31-top-tier-definition-2026-revision.md`
> **定位**：IMBoy E2EE"行业顶级"的唯一权威定义。所有验收、任务立项、对外口径以本文件为准。
> **维护规则**：行业标准演进 → 升版本号（2026.1→2026.2）+ 修订史，**旧版归档不删**；禁止原地改写已生效条目。

---

## 1. 范围与符合性模型

**范围**：IMBoy 单聊（C2C）与群聊（C2G）消息及附件的端对端加密，含密钥生命周期、用户可验证性、自动化测试、审计就绪、服务端零信任运维。

**符合性级别**（RFC 2119 语义）：
- **MUST**：不达成即不得宣称"顶级"；全部 MUST 绿 = 可申请 GA-Top-Tier 验收。
- **SHOULD**：不达成必须在已知问题台账登记 Acknowledged + 理由。
- **ROADMAP**：本版不强制；触发条件（行业默认化/买家硬需求）满足时升版转 MUST/SHOULD。

**验收方式类型**：`AUTO`=CI 可执行断言；`SCRIPT`=可复现脚本/命令；`DOC`=文档工件；`MANUAL`=人工门禁（真机/签字/演练）。每条需求在 `gap-matrix.md` 有现状、在 `evidence-matrix.md` 有证据指针。

**核心原则（ADR 31）**：顶级按**安全属性**定义，不按协议名定义。满足本标准全部 MUST 的实现——无论基于 Olm/Megolm 还是 MLS——均为顶级。

---

## 2. A 类：协议正确性

| ID | 级别 | 需求 | 行业锚 | 验收方式 |
|---|---|---|---|---|
| TT-A1 | MUST | 1:1 会话逐消息棘进（双棘轮或等价），提供逐消息前向保密 | Signal Double Ratchet 规范 | AUTO+MANUAL |
| TT-A2 | MUST | 会话建立规范有版本号与公开修订记录；实现与规范逐版本对齐 | PQXDH rev1→rev3 先例 | DOC |
| TT-A3 | MUST | 群成员变更双向密钥隔离（新成员不可读旧/离群不可读新），且成员变更成本有明确上界并实测 | Megolm 轮换规则；RFC 9420 TreeKEM 对照 | AUTO+SCRIPT |
| TT-A4 | MUST | 密钥泄露后协议自愈（PCS）：攻击者持有当前状态后，后续消息恢复机密性 | CRYPTO 2020；PQ3 Tamarin 属性 | AUTO+MANUAL |
| TT-A5 | SHOULD | 密钥材料内存清零（zeroization）；依赖库该性质有审计结论 | Least Authority vodozemac Issue A | DOC+AUTO |
| TT-A6 | MUST | 签名/MAC/AAD 带域分离，防跨协议重放与上下文混淆 | ETH Threema 教训二；ToB-DISCE2EC-5 | AUTO |

## 3. B 类：用户可验证性

| ID | 级别 | 需求 | 行业锚 | 验收方式 |
|---|---|---|---|---|
| TT-B1 | MUST | 带外 SAS/QR 验证通道；验证状态持久化并可跨设备传递（交叉签名或等价） | Signal safety number；Matrix 交叉签名 | AUTO+MANUAL |
| TT-B2 | MUST | 身份密钥变更对会话方可见、可追溯，不得被静默吞掉 | WhatsApp/Signal 变更提示惯例 | AUTO |
| TT-B3 | MUST | 身份密钥目录可审计：追加式日志+Merkle 一致性证明，且存在可独立运行的校验器 | WhatsApp AKD+Plexi；signalapp/key-transparency-auditor | AUTO+SCRIPT |
| TT-B4 | MUST | 换设备/新设备有显式恢复路径（备份/验证），且**各会话类型的历史可恢复性有公开文档说明** | Matrix 4S；WhatsApp E2EE 备份白皮书 | DOC+MANUAL |
| TT-B5 | SHOULD | 未验证设备的能力边界是显式策略（可否收发 E2EE 消息），非实现缺陷 | Element 强制验证公告（2026-10 生效） | DOC+AUTO |
| TT-B6 | MUST | 服务端替换身份密钥（MITM）时客户端可自动检测并告警，不依赖人工对码 | Signal KT beta 7.70；Apple CKV | AUTO（对抗实证） |

## 4. C 类：自动化测试

| ID | 级别 | 需求 | 行业锚 | 验收方式 |
|---|---|---|---|---|
| TT-C1 | MUST | 密码原语与编码层有 golden 测试向量（自研双实现互验或参考实现互操作），向量及 hash 归档 | 2key-ratchet 先例；NIST CAVP 惯例（注：libsignal/vodozemac 均无官方 KAT 包） | AUTO |
| TT-C2 | MUST | 双端 E2E CI：两个真实客户端实例经真实后端完成加密→投递→解密回路，含同构组合矩阵 | complement-crypto 客户端矩阵 | AUTO |
| TT-C3 | MUST | 对抗测试集显式覆盖：重放/乱序/丢弃/身份密钥替换/OTK 耗尽/降级诱导/备份竞态 | complement-crypto TEST_HITLIST；ETH 七攻击目录 | AUTO |
| TT-C4 | SHOULD | 测试可操纵进程与网络状态（kill/restart/清存储/断网）验证状态机恢复 | complement-crypto 架构 | AUTO |
| TT-C5 | MUST | 每个历史安全漏洞有命名回归测试，索引公开可点检 | vodozemac advance_to 修复先例 | DOC+AUTO |
| TT-C6 | MUST | 加密测试套件为合并硬门（red 不可合入）；Critical 测试禁止 skip | complement-crypto GitHub Action 模式 | AUTO |
| TT-C7 | MUST | KT 一致性校验脚本可独立运行并纳入 CI | signalapp/key-transparency-auditor | SCRIPT+AUTO |

## 5. D 类：独立审计就绪

| ID | 级别 | 需求 | 行业锚 | 验收方式 |
|---|---|---|---|---|
| TT-D1 | MUST | 公开协议规范白皮书：wire 格式/canonical 编码/版本号/修订史/golden 向量附录 | Signal specs；DAVE protocol.md；Threema 白皮书 | DOC |
| TT-D2 | MUST | 公开威胁模型：攻击者能力分级 + 每威胁→防御→证据三对照 | Apple PQ3 Level 0-3；ETH 三威胁模型 | DOC |
| TT-D3 | MUST | 密码学清单：原语/参数/曲线/库/版本/许可证，锚 commit 与 SBOM hash | DAVE ciphersuite 声明；NCC 版本锚定惯例 | DOC |
| TT-D4 | MUST | 密钥生命周期文档：生成/存储/轮换/备份/销毁/吊销级联全链路 | WhatsApp 备份白皮书；Apple CKV | DOC |
| TT-D5 | SHOULD | 第三方白盒审计（本版降级为**审计就绪包**：SOW 模板+可复现环境+自审计报告，第三方随时可进场） | NCC/Cure53/SRLabs/ToB 交付物惯例 | DOC |
| TT-D6 | MUST | 已知问题台账：Resolved/Acknowledged 状态机，含遗留理由 | vodozemac Issue J 先例；Threema ETH 回应页 | DOC |
| TT-D7 | MUST | 可复现构建与测试环境说明（审计方可独立跑通测试套件） | Cure53 白盒惯例 | DOC+SCRIPT |

## 6. E 类：服务端零信任与运维

| ID | 级别 | 需求 | 行业锚 | 验收方式 |
|---|---|---|---|---|
| TT-E1 | MUST | 私钥永不离开客户端设备（服务端零明文私钥）；该性质规范声明+CI 可执行验证 | WhatsApp/Signal 架构声明；NCC 审计范围惯例 | AUTO+DOC |
| TT-E2 | SHOULD | 口令保护的恢复材料有抗爆破机制（限次/限流/KDF 成本），与 HSM/OPAQUE 级的差距在台账声明 | WhatsApp HSM Backup Key Vault | AUTO+DOC |
| TT-E3 | MUST | 身份密钥发布/变更全部进入可复算日志（第三方可独立重放验证一致性） | WhatsApp AKD epoch；IETF keytrans 宪章 | SCRIPT |
| TT-E4 | MUST | 设备吊销即时生效：被吊销设备不可再领取密钥、不可解密吊销后新消息；吊销事件广播对端 | Signal/WhatsApp 设备管理惯例 | AUTO+MANUAL |
| TT-E5 | MUST | 密钥相关安全事件有不可篡改审计日志（追加式；KT 启用后含树头签名） | trust_audit append-only；AKD | AUTO |
| TT-E6 | SHOULD | 安全披露政策与响应 SLA 公开 | matrix.org/Threema 披露页惯例 | DOC |

## 7. R 类：路线图（本版不强制）

| ID | 触发升级条件 | 需求 | 行业锚 |
|---|---|---|---|
| TT-R1 | Signal/Apple 之外 ≥1 家主流 IM 生产部署 PQ；或买家合同要求 | PQ 混合握手（X25519+ML-KEM），先握手后评棘轮 | PQXDH rev3；PQ3；FIPS 203 |
| TT-R2 | MLS 生态库 1.0+审计全覆盖；或大群（>1000）/互通成产品硬需求 | MLS 群聊替代/并存 Megolm | RFC 9420；RCS UP 3.0；OpenMLS |
| TT-R3 | 标准升 2027 版评估 | 核心协议形式化验证（Tamarin/ProVerif），工件公开 | ePrint 2023/1738；ePrint 2024/1395（2.5 人月锚） |
| TT-R4 | 产品线决策 | 元数据最小化（sealed sender 级/传输层身份隐藏） | Signal Sealed Sender；Noise Pipes |

## 8. 符合性声明规则

1. 一个版本可宣称 **"IMBoy E2EE Top-tier 2026 符合"** 当且仅当：`evidence-matrix.md` 中全部 MUST 行状态=绿，全部 SHOULD 行=绿或台账 Acknowledged。
2. 宣称必须带**版本号与日期**（标准版本+实现 commit 锚），禁止无锚宣称。
3. 任何 R 类条目对外表述必须使用"路线图"措辞，禁止暗示已具备。
4. 本标准由 ADR 31 授权；修订走 README 维护规则，重大变更（MUST 增删）需五方签字。

## 9. 修订史

| 版本 | 日期 | 变更 |
|---|---|---|
| 2026.1 | 2026-08-01 | 首版。基于行业调研（research/ 四工件 2026-08 快照）与用户四决策（不迁 MLS/审计就绪包/PQ 列路线图/在途收口先行）。取代 v2/20 号文 G5 中"GA-Top-Tier 必须 MLS 群聊"的协议名定义（见 ADR 31）。 |
