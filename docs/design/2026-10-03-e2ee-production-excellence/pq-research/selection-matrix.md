# C16 PQ 实现选型矩阵（profile 研究阶段，PROTOTYPE 上限）

run-20261003-094804 / C16 / 编写日期 2026-10-03 / 状态：研究文档，未批准任何生产接线

每条事实标注置信级别：`[确证]` = 官方文档/一手来源 fetched；`[推断]` = 基于确证事实的工程推理；`[UNVERIFIED]` = 二手转述或未核对一手来源，批准前必须补验。

## 0. 需求回顾（来自 plan.md C16 / AC-32）

- 需要：PQXDH 风格 hybrid handshake（X3DH + ML-KEM）+ 持续 PQ ratchet 的**成熟实现或成熟原语组合**；「不自研密码原语」（plan §Tech Stack）。
- 平台矩阵：Android / iOS / macOS（Flutter + flutter_rust_bridge 栈已有先例 flutter_vodozemac 0.8.x）。
- 许可证须与 imboy 商业化部署兼容（私有化部署、可能闭源分发）。
- 上游须有审计或等效质量证据；interop 用第二实现（Python oqs 对拍）。

## 1. 参考实现层候选（PQXDH + Double Ratchet 整体方案）

| # | 候选 | 状态 | 许可证 | 可否复用 | 结论 |
|---|------|------|--------|----------|------|
| R1 | Signal libsignal（PQXDH 生产实现，Rust） | 2023-09 起生产部署 PQXDH；2025-10 SPQR（Triple Ratchet）[确证] | AGPL-3.0（旧 C 库 GPLv3）[确证] | **不可 vendor/不可链接**：AGPL 对 imboy 私有化闭源分发构成许可证传染；Signal 仅公开授权「GPL 兼容应用上 App Store」[确证] | 作为**规范与设计参考**（阅读规范+论文，clean-room 语义实现），不复制代码 |
| R2 | Matrix vodozemac 上游 PQ 分支 | 截至 2026-10 未检索到已发布的 PQXDH/ML-KEM 支持；Matrix.org 有迁移意向讨论 [UNVERIFIED——检索 2026-10-03 未见正式 roadmap 交付] | Apache-2.0 / Apache-2.0 OR MIT [确证（本仓 pubspec 0.8.x 即此栈）] | 当前 0.8.x API（`Account.createOutboundSession`）不暴露注入外部熵的入口 [确证（本仓 olm_session_service.dart:725-730 实测）] | **不等上游**；PQ 层在 vodozemac 之外包封（见 hybrid-handshake-draft.md），vodozemac 保持原样 |
| R3 | OpenMLS（MLS 群组栈） | 面向 MLS/group，非单聊 PQXDH 替代；属 C15 范畴 | （C15 另行评估） | — | 不纳入 C16 单聊候选，避免与 C15 边界混淆 |

**结论（参考实现层）**：没有任何一个可直接复用的「整体 PQXDH+PQ ratchet」库同时满足许可证 + 平台 + 成熟度三条件。正确路径是**规范参考（Signal PQXDH/SPQR 规范）+ 成熟 ML-KEM 原语库 + 本仓自有的组合层**（组合层只做 KDF/AEAD 编排，不写密码原语，符合 plan「不自研密码原语」边界）。

## 2. ML-KEM 原语层候选（≥3）

### M1 RustCrypto `ml-kem` crate（**推荐**）

| 维度 | 事实 | 置信 |
|------|------|------|
| 语言/形态 | 纯 Rust，FIPS 203（ML-KEM-512/768/1024），无 C 依赖 | [确证] |
| 许可证 | Apache-2.0 OR MIT 双许可（任选）——对闭源/私有化分发零摩擦 | [确证] |
| 审计 | README 原文：*"The implementation contained in this crate has never been independently audited!"* | [确证] **这是首要风险** |
| 形式化验证 | 无（RustCrypto 团队探索 hax，未覆盖此 crate）[UNVERIFIED 是否有后续] | [UNVERIFIED] |
| 打包/背书 | 进入 Debian sid / Ubuntu（`rust-ml-kem`）；RustCrypto 生态常规 CI + 跨架构测试 | [确证] |
| MSRV | Rust 1.85+（MSRV 提升可能在 patch 版本发生——锁版本策略要求） | [确证] |
| FFI/Dart | 纯 Rust → 与 flutter_rust_bridge 栈同构（flutter_vodozemac 先例），无 C 工具链新增 | [推断] |
| 平台矩阵 | Android/iOS/macOS 交叉编译随 Rust 标准目标三元组覆盖；无平台特化汇编依赖 | [推断，PROTOTYPE 阶段实测冻结] |
| 审计缓解 | NIST ACVP ML-KEM KAT vectors + Python liboqs 独立对拍（§4 vectors-plan）；ml-kem crate 自带已知答案测试 | [推断] |

### M2 `aws-lc-rs`（`ml_kem` 模块，**备选**）

| 维度 | 事实 | 置信 |
|------|------|------|
| 形态 | Rust binding 到 AWS-LC（C/汇编），rustls 默认后端之一 | [确证] |
| 许可证 | Apache-2.0 WITH LLVM-exception（静态链接对象文件不传染）——合规可用但审核文案比 MIT/Apache 双许可复杂 | [确证] |
| 合规背书 | AWS-LC 有 FIPS validated 模块（aws-lc-fips-sys）；NIST CAVP 列出 AWS-LC 的 ML-KEM 验证记录 | [确证]（证书号与 ML-KEM 具体验证范围 **[UNVERIFIED]**，批准前查 CSRC 现行列表） |
| 生产使用 | rustls、AWS SDK 生态生产使用 | [确证] |
| 代价 | 引入 AWS-LC C 构建（cmake/nasm 工具链）进 iOS/macOS 交叉编译链；产物体积比纯 Rust 大；flutter_rust_bridge 集成需捆绑 C 源构建 | [推断] |
| 适用 | 若商业买家要求 FIPS 证据链，M2 升为首选 | [推断] |

### M3 liboqs / liboqs-c（**排除生产、保留为 interop 对拍工具**）

| 维度 | 事实 | 置信 |
|------|------|------|
| 许可证 | MIT（部分第三方组件 Apache-2.0/BSD-3/CC0，各子目录标注） | [确证] |
| 生产就绪 | README 原文：*"WE DO NOT CURRENTLY RECOMMEND RELYING ON THIS LIBRARY IN A PRODUCTION ENVIRONMENT OR TO PROTECT ANY SENSITIVE DATA."* | [确证] → **排除生产集成** |
| 定位 | 研究与原型 —— 恰好匹配它的**第二实现 interop** 用途（liboqs-python 对拍），AC-32 的「独立实现 interop」正好落在它官方定位内 | [推断] |
| ML-KEM | Tier 1（核心），基于 pq-code-package/mlkem-native | [确证] |

### M4 libcrux（Cryspen，观察名单）

| 维度 | 事实 | 置信 |
|------|------|------|
| 形态 | Rust，ML-KEM 经 hax/F* 形式化验证提取 | [确证（Cryspen 公开材料）；主仓库 README fetch 失败，许可证与版本细节 [UNVERIFIED] |
| 风险 | 成熟度/发布节奏/许可证未核对一手（常见为 Apache-2.0 OR MIT，**待验**）；被选为 mlkem-native 验证参照之一 | [UNVERIFIED] |

### 已淘汰算法（任务书点名排除项，一律不评估）

- Rainbow（及同类多变量 NIST 签名方案）：2022 被 NIST 击破并淘汰 [确证]
- SIKE（等距同源）：2022 被 Castryck-Decru 攻击击破 [确证]
- BIKE/NTRU-Prime 等 liboqs 内未被 NIST 选中的 KEM：不进入候选 [确证]
- 本计划只评估 **ML-KEM（FIPS 203）**；HQC 为 NIST 备选 KEM 但标准落地与实现生态未成熟到可选 [推断]

## 3. 推荐与备选（ML-KEM 原语）

- **推荐 M1 RustCrypto `ml-kem`**：许可证最干净（Apache-2.0 OR MIT）、纯 Rust 与既有 flutter_rust_bridge 栈同构、无 C 工具链新增、发行版打包背书。**已知代价：未经独立审计** —— 缓解 = ACVP KAT 全量 vectors + liboqs-python 独立对拍 + （可选）aws-lc-rs 三向对拍；并在 D-04 向用户如实披露「无第三方审计」风险项。
- **备选 M2 aws-lc-rs**：FIPS/CAVP 证据链完整、生产背书强；代价是构建链复杂度与许可证审核文案。若目标客户有 FIPS 采购要求则切换首选。
- **M3 liboqs 仅作 interop 工具**（Python 侧），不入生产二进制。
- 双实现锁版本：选 M1 后 Cargo 锁精确版本 + SBOM 登记（衔接 C17 AC-35）。

## 4. 平台与分发注意（含 iOS export compliance——只列注意项，不下法律结论）

- Apple CryptoKit 在新 OS 已暴露 ML-KEM-768/1024 与 X-Wing HPKE [确证]；Apple 自家 PQ3 即用 ML-KEM [确证]。**若**未来把 KEM 换成 CryptoKit 调用，走「Apple 提供的加密」豁免路径的方向会不同；但 imboy 需跨 Android/iOS/macOS 同一实现，统一 Rust 路径下**自带第三方加密实现**。
- 自带第三方加密实现时：App Store `ITSAppUsesNonExemptEncryption` 申报方向通常为 true（non-exempt）并走 App Store Connect 附加文档流程；法国区历史上有 ANSSI/SGLSAT 声明要求，EU 2021/821 协调后大体简化但 ASC 流程可能仍提示 [确证（Apple 官方指引存在）+ UNVERIFIED（法国现行细则）]。**法律边界：此处仅列核对清单，最终申报口径由用户/法务决定。**
- 保留区约束（plan §2）：`imboyapp/ios/*`、`macos/*` 禁改。参照 flutter_vodozemac 的集成方式（pub 依赖自带 podspec，不需手改 ios 目录）[确证（本仓该依赖即如此集成）]；**若**实际接线发现必须改 Podfile/xcconfig，则记 `BLOCKED_PROTECTED_PATH`，不得为绕过而手改保留区。
- 体积增量：纯 Rust ML-KEM-768 编译产物预估数量级为几十 KB/架构 [UNVERIFIED——PROTOTYPE 构建后实测冻结，写进 D-04 批准材料]。

## 5. 来源

- Signal PQXDH 规范: https://signal.org/docs/specifications/pqxdh/ （fetched 2026-10-03）
- Signal SPQR 公告: https://signal.org/blog/spqr/ （2025-10-02，fetched 2026-10-03）
- RustCrypto/KEMs ml-kem README（fetched 2026-10-03，审计声明原文）
- liboqs README（fetched 2026-10-03，生产警告原文）
- aws-lc-rs / aws-lc-fips-ss crates.io、NIST CSRC CAVP 列表（searched 2026-10-03）
- Apple: https://developer.apple.com/news? export compliance 指引、https://security.apple.com/blog/imessage-pq3/
- libsignal 许可证：signalapp/libsignal GitHub（AGPL-3.0）、signal.org 2016 license 声明（searched 2026-10-03）
