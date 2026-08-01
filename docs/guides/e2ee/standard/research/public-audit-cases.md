# 公开密码学审计案例库（2026-08 快照）

> **层**：Research ｜ **复核周期**：半年 ｜ **本次日期**：2026-08-01
> **用途**：审计就绪包（D 类需求）的格式依据；`../audit-ready-package.md` 的 SOW 模板对标。

## 1. 案例表

| 对象 | 审计方 | 时间 | 规模 | 结果要点 | 报告 |
|---|---|---|---|---|---|
| Olm 1.3.0（双棘轮+Megolm） | NCC Group | 2016-09/10 执行，2016-11-01 公开 | — | 报告公开；finding 明细本次 403 未取 ⚠️ | nccgroup.com/research/public-report-matrix-olm-cryptographic-review |
| **vodozemac**（我方依赖库） | Least Authority | 2022-03-30 终稿 | — | 10 issue(A-J)+8 建议；zeroization 不全/key ID 错/整数溢出/旧棘轮状态清除(新增 advance_to)/MAC 截断 8B 协议遗留(Issue J=Acknowledged 先例) | leastauthority.com/static/publications/LeastAuthority-Matrix_vodozemac_Final_Audit_Report.pdf |
| WhatsApp AKD | NCC Group | 2023-08 | 20 人日 | 全部发现已修复 | nccgroup.com/research/public-report-whatsapp-auditable-key-directory-akd-implementation-review |
| WhatsApp E2EE 备份 | NCC Group | 2021-10-27 | 35 人日 | +UC 框架学术证明(ePrint 2023/843) | nccgroup.com/media/fzwdxklh/_ncc_group_whatsapp_e001000m_report_2021-10-27_v12.pdf |
| Threema 移动 App | Cure53 | 2020-10 | 3 人×16 天 | 8 发现均非实际漏洞；"代码质量异常扎实" | threema.com/assets/6-resources/audits/2020_cure53_audit_mobile_apps.pdf |
| Threema 桌面 App | Cure53 | 2024-01 | 5 人×16 天 | 7 发现（3 漏洞+4 弱点），无 Critical/High | cure53.de/pentest-report_threema-desktop.pdf |
| OpenMLS | SRLabs（STA 赞助） | 2025 | — | 8 发现（1 High=MAC 认证不当），7 项修复发布于 0.8.1/0.7.3 | blog.phnx.im/openmls-independent-security-audit |
| Discord DAVE（MLS 音视频） | Trail of Bits | 2024-08 设计评审 4 人周 + 2024-09 代码评审 5 人周 | 9 人周 | TOB-DISCE2EC-5：AES-GCM 非 key-committing，被 DAVE 规范直接引用 | github.com/trailofbits/publications（reviews/2024-08/09-discord-dave-*） |
| Meta WhatsApp Private Processing | Trail of Bits | 2025-08 | 12 人周 | TEE 私密推理 | 同 publications 仓库 |

**学术攻击类（非委托审计，同等重要）**：
- ETH "Three Lessons From Threema"（USENIX Sec 2023）：三威胁模型 7 攻击，数周内修复并催生 Ibex。https://breakingthe3ma.app/
- "Practically-exploitable Cryptographic Vulnerabilities in Matrix"（IEEE S&P 2023）：设备冒充/跨签名绕过。https://nebuchadnezzar-megolm.github.io/static/paper.pdf
- PQXDH 形式化分析（ePrint 2023/1738，ProVerif+CryptoVerif）；PQ3 Tamarin（ePrint 2024/1395）；"Matrix Reloaded" Tamarin（arxiv 2408.12743）。

## 2. 证伪清单（防止以讹传讹）

| 流传说法 | 核实结果 |
|---|---|
| "Trail of Bits 审计过 libsignal" | ❌ **不存在**。已核查 ToB 官方 publications 仓库全库；libsignal 的验证形式=学术形式化分析+soatok 2025 公开评审系列 |
| "vodozemac 2024 年被 Least Authority 审计" | ⚠️ 年份错：**2022-03-30 终稿** |
| "libsignal/vodozemac 有官方 KAT 向量包" | ❌ 均无。行业替代=双端集成测试（complement-crypto）或第三方向量（2key-ratchet） |
| "棘轮状态机有公开 property-based testing 实践" | ❌ 行业空白；由形式化验证+对抗性集成测试替代 |

## 3. 审计标准交付物惯例（我方就绪包对标格式）

1. **范围与版本锚定**：精确到库版本/commit，含排除项声明（NCC 锚 Olm 1.3.0；SRLabs 锚 crate 0.8.x）。
2. **人日/人周账目**：公开报告普遍披露（20 人日/35 人日/16 天×N/4-12 人周）。
3. **方法论声明**：白盒代码审计+协议层分析（Cure53 明示"source code shared with auditors"）；形式化类注明工具（Tamarin/ProVerif/CryptoVerif/hax）与模型假设。
4. **编号化 findings**：唯一 ID（TOB-DISCE2EC-5 / vodozemac Issue A-J）、严重度分级、漏洞 vs 弱点区分。
5. **修复验证（retest）轮**：Least Authority verification phase；NCC Olm 十月 fix review；OpenMLS 修复发布于指定版本。
6. **Resolved/Acknowledged 状态机**：允许"协议层限制不修"（vodozemac Issue J MAC 截断继承自 libolm）。
7. **公开授权发布**：报告经客户同意公开，被审方博客背书（Least Authority 明示惯例）。
8. **被审方需提供**：协议规范文档、源代码（白盒）、可复现构建/测试环境、威胁模型声明、答疑通道。

→ 我方 SOW 模板（`../audit-ready-package.md` §SOW）逐项对齐此 8 条。
