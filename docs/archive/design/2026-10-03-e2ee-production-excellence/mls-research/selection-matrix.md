# C15 — MLS 成熟实现选型对比表（研究基线）

> run: run-20261003-094804 | card: C15 | 状态：**研究/PROTOTYPE 阶段**（D-03 未批，不接生产）
> 检索时间：2026-10-03（GitHub API / crates.io API / pub.dev / 官方博客直接核实）。
> 每行结论标注【确证】= 该轮直接核实；【推断】= 依据核实信息推理；UNVERIFIED = 无法核实。

## 1. 候选全景（6 项）

| # | 实现 | 语言 | 许可证 | 维护方 | 最近 push | 定位 |
|---|---|---|---|---|---|---|
| 1 | **OpenMLS** (`openmls/openmls`) | Rust | MIT【确证：GitHub repo license 字段】 | Phoenix R&D + CE Labs【确证：README】 | 2026-10-02【确证：GitHub API pushed_at】 | 嵌入式 MLS 库（无传输/身份假设） |
| 2 | **mls-rs** (`awslabs/mls-rs`) | Rust | Apache-2.0 / MIT 双许可【确证：GitHub API】 | AWS Labs | 2026-09-17【确证：GitHub API】 | 高性能嵌入式 MLS 库（Wickr 血统） |
| 3 | **mlspp** (`cisco/mlspp`) | C++17 | BSD-2-Clause【确证：GitHub API】 | Cisco | 2026-09-21【确证：GitHub API】 | C++ MLS 实现（Webex 线） |
| 4 | **openmls_dart** (`djx-y-z/openmls_dart`，pub 包名 `openmls`) | Dart(FRB) | MIT【确证：Cargo.toml/pub.dev】 | 个人（djx-y-z），2026-02 建【确证：GitHub API created_at】 | 2026-09-29【确证】 | OpenMLS 0.9.0 的第三方 Flutter/Dart 绑定 |
| 5 | **matrix-rust-sdk crypto** | Rust | Apache-2.0 / MIT（Matrix 组织惯例） | Matrix 基金会 | 持续 | 全功能 SDK（Olm/Megolm 现役 + MLS 实验线 SSSCS） |
| 6 | **libMLS**（Inria 历史实现） | C++ | — | 学术原型 | 停滞【推断：搜索结果显示已被 mlspp 取代、不再显著维护】 | 历史 draft 实现 |

## 2. 六维评估

### 2.1 许可证

| 实现 | 许可证 | 对本项目影响 |
|---|---|---|
| OpenMLS | MIT | 宽容；与 imboy 商业化（私有化部署销售）兼容，无 copyleft【确证】 |
| mls-rs | Apache-2.0/MIT 双 | 宽松；Apache-2.0 含专利授予，商用同样友好【确证】 |
| mlspp | BSD-2-Clause | 宽松；但 C++ 链接方式（静态/动态）需在发布物声明【确证】 |
| openmls_dart | MIT | 宽松【确证】 |
| matrix-rust-sdk | Apache-2.0 等 | 宽松，但引入整个 SDK 才能取 MLS 线（见 2.2） |
| 对照：现役 flutter_vodozemac | **AGPL-3.0**【确证：pub.dev】 | 已在用（vodozemac 0.8.1），说明 AGPL 依赖已被项目接受/评估过；MLS 候选均为更宽容许可，不劣化现状 |

### 2.2 审计状态（本轮核实的关键差异点）

| 实现 | 审计状态 | 证据 |
|---|---|---|
| OpenMLS | **有独立第三方审计**：SRLabs 架构与基线安全评估（2026-03-11 发布于 blog.openmls.tech），发现 **8 个问题，严重度 high → informational，多数偏低危**；2026-05-27 Phoenix R&D 博客（blog.phnx.im）公布结果与后续修复【确证：两篇官方博客检索摘要】。**注意：具体 8 个 issue 的逐条清单与修复状态 UNVERIFIED（未获取审计报告原文）** | [blog.openmls.tech 2026-03-11](https://blog.openmls.tech)（OpenMLS Security Assurance Assessment）、[blog.phnx.im 2026-05-27](https://blog.phnx.im) |
| mls-rs | **无第三方安全审计**（README 自述：已验证 RFC 符合性"但尚未接受完整的第三方安全审计"）【确证：README】 | [awslabs/mls-rs README](https://github.com/awslabs/mls-rs) |
| mlspp | 无公开第三方审计记录【确证：repo 页面 Security and quality 为空】 | — |
| openmls_dart | 无审计（个人项目）【确证】 | — |
| matrix-rust-sdk crypto | vodozemac（Olm/Megolm 部分）2022 有审计；**MLS 线为实验性，未见 MLS 部分审计**【确证 vodozemac 审计/推断 MLS 部分】 | [matrix.org 2022-05-16](https://matrix.org) |
| libMLS | 无 | — |

### 2.3 FFI 可行性（Dart/Flutter via flutter_rust_bridge）

| 实现 | 路径 | 评估 |
|---|---|---|
| OpenMLS | 纯 Rust、无 IO/传输假设、crypto 后端可插拔（`openmls_rust_crypto` / `libcrux_crypto`）【确证：README】。**已存在 FRB 绑定先例 openmls_dart（FRB 2.13.0 + openmls 0.9.0 tag，13 套件端到端测试 CI）证明 API 面可桥接**【确证】 | **首选**。项目已有 flutter_vodozemac 同模式（FRB 进程级单次 init，见 `vodozemac_init.dart` 的 BUG#72 教训），工程路径已被本仓踩过 |
| mls-rs | 官方提供 `mls-rs-ffi` 与 `mls-rs-uniffi`（Swift/Kotlin 现成）【确证：README】 | UniFFI → Dart 无官方后端（uniFFI 无 Dart target），仍需自建 FRB 或 C FFI 层【推断】 |
| mlspp | C++，需 Dart `dart:ffi` 手写绑定 + 三平台构建链（vcpkg/OpenSSL） | 成本高；C++ 供应链（OpenSSL 版本矩阵）进入 App 是新增风险面【推断】 |
| openmls_dart | 现成 pub 包（Android/iOS/macOS/Linux/Windows/Web） | 可作 PoC 快速路径；个人项目规模（9 stars）不宜直接依赖生产【确证：规模】 |
| matrix-rust-sdk | 整 SDK 面大，绑定成本最高；且其 MLS 绑定 Matrix 协议概念 | 不作绑定目标【推断】 |

### 2.4 平台矩阵（项目要求 Android/iOS/macOS）

| 实现 | Android | iOS | macOS | 备注 |
|---|---|---|---|---|
| OpenMLS | CI 仅构建不测试（跨编译 target）【确证】 | 同左 | CI 实测 aarch64 macOS【确证】 | 移动端真机验证属 C13/后续卡职责；openmls_dart 已提供三平台构建钩子先例 |
| mls-rs | mls-rs-ffi/uniffi 移动支持【确证】 | 同左 | CryptoKit 后端倾向 Apple【确证】 | |
| mlspp | 无官方移动支持【确证：README 仅 Linux/macOS/Windows 构建说明】 | — | — | |
| openmls_dart | SDK 24+ arm64/armv7/x64【确证】 | iOS 13+【确证】 | 10.15+【确证】 | dart2wasm 不支持（FRB 2.13 限制）【确证】 |

### 2.5 维护活跃度（2026-10-03 实测）

| 实现 | 最近 push | stars | 版本 |
|---|---|---|---|
| OpenMLS | 2026-10-02 | ~1k | crates.io 0.9.0（2026-08-25），累计下载 812,794【确证：crates.io API】 |
| mls-rs | 2026-09-17 | 257 | crates.io 0.56.0（2026-08-19），362,406【确证】 |
| mlspp | 2026-09-21 | 152 | — | |
| openmls_dart | 2026-09-29 | 9 | pub.dev 3.2.1（2026-09-30），周下载 ~1.31k；CI 每日自动跟 openmls 上游【确证】 |

### 2.6 RFC 9420 合规度

| 实现 | 状态 |
|---|---|
| OpenMLS | 声明实现 RFC 9420；参与 IETF interop（`interop_client`/`compat_tests`）【确证】；Matrix 基金会以其为底做自己的 MLS 线（fork）【确证：社区检索】 |
| mls-rs | 声明 100% RFC 9420 符合性（全部默认 credential/proposal/extension 类型，预计算 vectors 验证）【确证：README】 |
| mlspp | README 引用的是 draft-ietf-mls-protocol 而非正式 RFC【确证】；interop 生态悠久（与 OpenMLS 互测）【确证：interop 目录】 |
| openmls_dart | 随 openmls 0.9.0【确证】；PQ 套件为 draft 状态明确标注实验性【确证】 |

## 3. 结论：推荐 + 备选

**推荐：OpenMLS（Rust）+ 自建 flutter_rust_bridge 绑定层**

理由（按权重排序）：
1. **唯一持有独立第三方审计的候选**（SRLabs 2026，8 issues 且多数低危——注意逐条清单 UNVERIFIED）；mls-rs 自述无审计。
2. MIT 许可 + 无传输/身份假设的嵌入式设计，与 imboy「自有路由/身份层（C09 KT）+ 只换群密码层」的集成边界最干净。
3. FRB 桥接可行性已被 openmls_dart（FRB 2.13.0 + openmls 0.9.0）实证；本仓已有 flutter_vodozemac 同栈运维经验（含 FRB 单次 init 的 BUG#72 教训）。
4. 维护最活跃（2026-10-02 push；81 万下载；Matrix 基金会派生使用）。

**备选：AWS mls-rs**
- 场景：若 PoC 中 OpenMLS 在群规模/性能预算（AC-30 冻结预算）不达标，mls-rs（性能导向、SQLite/内存可配置存储、WASM/移动 FFI 现成）是唯一同级 Rust 备选。
- 代价：无第三方审计（需在 D-03 决策中显式接受或另行委托审计）；绑定层同样自建。

**不推荐直接依赖 openmls_dart（pub 包 `openmls`）于生产**：个人项目（9 stars、未验证上传者、2026-02 才建立），但其仓库结构（FRB 2.13.0 锁定、openmls git tag 锁定、SQLCipher 加密存储、13 套件全生命周期 CI、每日上游跟踪）**作为自建绑定的参考实现价值高**——PoC 阶段允许直接用它加速实验，生产接线换自建/审计过的绑定。

**排除**：mlspp（C++ FFI 成本 + 无审计 + 官方无移动支持）、matrix-rust-sdk（MLS 实验性且绑 Matrix 协议栈）、libMLS（停滞）。

## 4. 依赖新增面预估（D-03 输入）

以「OpenMLS + 自建 FRB 绑定」计：
- Rust 侧：`openmls`（+`openmls_rust_crypto` 或 `libcrux_crypto` 二选一）、`flutter_rust_bridge =2.x`、序列化若干（FRB 生态自带）。PQ 套件（draft-ietf-mls-pq-ciphersuites feature）**默认关闭**（归 C16 域）。
- Dart 侧：无新增第三方（FRB 生成代码 + 现有 SQLCipher 存储栈）。
- 不动 pubspec（本轮）；生产接线时的版本锁定、THIRD_PARTY_NOTICES、SBOM 条目归 C17。

## 5. 来源清单

- https://github.com/openmls/openmls （repo、license、CI、README）
- https://crates.io/api/v1/crates/openmls 、 /mls-rs （版本/下载量）
- https://blog.openmls.tech （OpenMLS Security Assurance Assessment，2026-03-11）
- https://blog.phnx.im （OpenMLS independent security audit，2026-05-27）
- https://github.com/awslabs/mls-rs （README：审计自述、平台、FFI/UniFFI）
- https://github.com/cisco/mlspp （README：C++17/OpenSSL、vcpkg、Catch2）
- https://github.com/djx-y-z/openmls_dart + https://pub.dev/packages/openmls （v3.2.1、FRB 2.13.0、openmls 0.9.0 tag、平台矩阵）
- https://pub.dev/packages/flutter_vodozemac （AGPL-3.0 对照）
- matrix.org 博客/This Week in Matrix 2022-10-28、2026 Matrix Summit CFP（BWI MLS 线）——SSSCS 实验性状态【推断级：社区信息，非 spec 承诺】
