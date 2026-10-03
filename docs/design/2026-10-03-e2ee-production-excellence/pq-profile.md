# PQ Profile —— 混合抗量子单聊与持续棘轮（C16，PROTOTYPE 上限）

run-20261003-094804 / C16 / 编写 2026-10-03 / 状态：**profile 研究阶段。未写生产代码、未动 pubspec、未接线。任何生产启用以 D-04 批准为前提。**

配套文档：`pq-research/selection-matrix.md`（选型）、`pq-research/hybrid-handshake-draft.md`（握手草案）、`pq-research/vectors-plan.md`（vectors 方案）。

置信标注：`[确证]` / `[推断]` / `[UNVERIFIED]`，与 selection-matrix.md 一致。

---

## 1. 背景与基线

- 现状：C2C = X3DH + Double Ratchet（vodozemac 0.8.x，`flutter_vodozemac ^0.8.1`）[确证（pubspec + olm_protocol.dart）]。
- spec §5.3 已如实声明：**同一发送链内对称链密钥可前向推导**，同链 PFS 仅依赖消息密钥内存销毁；跨 DH ratchet 步的 PFS/PCS 是密码学无条件（在 DH 假设下）[确证]。PQ 层设计必须继承同等诚实度的边界声明（§3）。
- 行业先例（全部 2026-10-03 fetched/搜索确证）：
  - Signal PQXDH（2023-09 生产、规范 2024-01）：X25519 + PQ KEM 混合握手；**认证与 ratchet 非量子安全**。
  - Signal SPQR（2025-10-02 公告）：把 ML-KEM-768 引入持续 ratchet（Triple Ratchet）：稀疏/分块（EK 1184B 分 37 块、CT 1088B 分 34 块）+ 擦除码搭便车 + 串行 epoch + ML-KEM Braid；ProVerif 验证；支持对端不支持时的优雅降级。
  - Apple PQ3（2024-02）：iMessage 的 PQ + 持续再封装设计。
- 结论：**「一次 PQ handshake ≠ 持续抗量子」已是行业共识且存在成熟先例路径**（SPQR），本 profile 采纳「握手 PQ + epoch 稀疏 PQ ratchet」两级设计。

## 2. 选型摘要（详见 pq-research/selection-matrix.md）

- 参考实现层：无可整体复用的库（libsignal=AGPL 不可链接；vodozemac 无注入口与已发布 PQ 支持）→ **规范参考 + 成熟原语 + 自有组合层（不写原语）**。
- ML-KEM 原语：**推荐 RustCrypto `ml-kem`**（纯 Rust、Apache-2.0 OR MIT、Debian 打包；**未经独立审计**——以 ACVP KAT + liboqs-python 独立对拍缓解并在 D-04 披露）；**备选 aws-lc-rs**（FIPS/CAVP 背书，构建链较重）；**liboqs 仅作 interop 工具不进生产二进制**。
- 淘汰：Rainbow/SIKE 等已破/已淘汰算法一律不评估。

## 3. 持续 PQ ratchet 评估（AC-32 核心合同）

### 3.1 三档开销模型

| 档位 | 每消息开销 | 抗量子保证 | 评价 |
|---|---|---|---|
| A. 每消息 KEM | +1~2.3KB（EK 1184B/CT 1088B [确证]）+ KEM 运算 ×2 | 逐消息量子 FS/PCS | 消息膨胀 5–30 倍，IM 单聊不可接受 [推断]；无先例采用 |
| B. 每 N 消息（epoch） | 均摊 ~（2272B/N）+ 固定 PQ AEAD 头 ~24B | epoch 粒度量子 FS/PCS；epoch 内为对称棘轮 | **SPQR 同款策略**（Signal 生产先例）[确证]；本 profile 采用 |
| C. 仅 handshake | 稳态 +0（仅首条 +~2.3KB） | 仅防 harvest-now-decrypt-later；**会话中段泄露后无量子自愈** | 不满足「持续」定义；**AC-32 明文禁止把 C 称为持续抗量子** |

### 3.2 选定档位与如实声明（合同条款草案）

本 profile 选 **B**（默认 N=100，可配置；方向变化不强制触发——与 SPQR 的稀疏哲学一致），并声明：

1. PQ 层同链对称棘轮继承 spec §5.3 完全相同的边界：**同一 PQ 链内，攻破当前链密钥可前向推导后续链密钥**；同链内逐消息保密依赖消息密钥销毁。[确证（与既有合同同构）]
2. 量子级前向保密与泄露后自愈以 **epoch** 为粒度：新 epoch KEM 交换完成且旧 DK 销毁后，此前泄露的 PQ 状态无法解密后续消息。[推断（设计属性，待 AC-33 实验证实）]
3. 认证（身份绑定/防主动 MITM）仍由经典 X25519/Ed25519 承担——**对可解离散对数的主动量子攻击者不提供认证保护**（PQXDH 规范同款边界）[确证]。
4. 混合组合安全：明文保密 = Olm 层 ∧ PQ 层同时被攻破才失守（双层包封语义，见 handshake 草案 §1）。
5. **禁止任何 UI/文档把本方案表述为「量子绝对安全」或「逐消息量子加密」**（措辞合同进 D-04）。

### 3.3 性能预算（建议值区间；真机实测前全部为待验证预算）

| 项 | 预算建议 | 依据 |
|---|---|---|
| 首条消息（握手）增量 | +2.2~2.6 KB（pq_ct 1088B + PQ prekey 元数据 + 双层头） | ML-KEM-768 参数 [确证] |
| 稳态每消息增量（N=100） | +40~80 B（PQ AEAD tag 16B + epoch/seq 头 ~8B + 均摊 KEM 22.7B×2/100 + envelope 长度域） | 参数推导 [推断] |
| epoch 换钥消息 | 峰值 +2.3 KB（整块携带；SPQR 分块版可降到每消息 +32B 但状态机更复杂，列为优化项） | §3 草案 [推断] |
| KEM 运算时延 | encap/decap 各预算 ≤2 ms/次（移动端）；逐消息加密总时延增幅预算 ≤15% | 公开测量称 ML-KEM 在移动级 CPU 为亚毫秒~毫秒级 [UNVERIFIED：Apple PQ3 公告称 negligible、Cloudflare 服务端测量 ~μs 级；**具体到目标真机必须实测冻结**] |
| 内存/状态增量 | 每会话 +~1.5 KB（PQ chain + epoch 计数 + 当前 DK） | 参数推导 [推断] |

## 4. 负例清单（AC-32：fail-closed 断言点）

以下每条对应至少一个 vectors 族（vectors-plan.md §2）或真实旅程负例；实现卡必须先写这些负例的 RED：

| # | 场景 | fail-closed 行为 |
|---|---|---|
| N1 | PQ prekey 签名验证失败 / key_id 伪造 | 拒绝建会话，不回退纯 Olm |
| N2 | PQ 公钥与 pin 不匹配（换绑攻击） | `IdentityChangedException` 同语义阻断，需用户显式确认 |
| N3 | claim 响应剥离 `pq_kem` 但 manifest 声称 pq capability | 视为剥离/降级攻击，拒绝（不静默降级） |
| N4 | 外层 PQ AEAD 认证失败（含 SEAD 式换绑 AD） | 整条消息拒绝；**不得**尝试「跳过外层只解内层」 |
| N5 | 内层 Olm 解密失败 | 整条拒绝（外层通过不等于可信） |
| N6 | epoch 回退 / CT 跨 epoch 重放 / seq 回退 | 拒绝且不推进任何棘轮状态 |
| N7 | HWM=pq-olm 的对端降级出现无 pq manifest | `CapabilityDowngradeException`（C02 语义扩展） |
| N8 | PQ 状态持久化失败（CryptoStore 不可用） | 拒绝发送/接收推进（RT-P2-02 同语义：无持久化不得推进棘轮） |
| N9 | 崩溃恢复时 epoch 推进半途（DK 已换、CT 未送达） | 回退到上一完整 epoch 状态；不使用未确认新 DK 加密 |
| N10 | ml-kem 原语返回错误（decapsulation failure） | 显式异常上抛，禁止静默零值/猜测 SS |
| N11 | vectors 版本不匹配 / 未知 suite 元数据 | 拒绝加载/拒绝解密（不猜测格式） |
| N12 | 旧 Olm 会话收到 pq-olm 元数据（或反向） | 命名空间隔离：按未知 suite 拒绝，不自动转换升级 |

## 5. 泄露后恢复实验设计（AC-33）

### 5.1 实验矩阵（合成账号、受控环境）

| 实验 | 步骤 | 通过 oracle |
|---|---|---|
| E1 泄露后-PQ-自愈 | 拷贝设备 A 的完整 PQ 状态（含当前 epoch DK 与 chain）→ B 正常收发至 epoch 推进（N 条或手动触发）→ 用旧状态解新消息 | 推进后全部失败；推进前消息可解（边界与 §3.2 声明一致） |
| E2 泄露后-双层 | 只泄露 Olm 层（或只泄露 PQ 层）状态 → 解任意后续消息 | 全部失败（混合组合安全：单层泄露不解密） |
| E3 乱序 | epoch 换钥帧与数据帧乱序/交错到达；消息乱序跨 epoch 边界 | 状态机收敛到正确 epoch；Olm skipped-keys 语义不被破坏 |
| E4 重放 | 旧 epoch CT 重放、旧 frame 重放、重复投递（同 messageId） | 拒绝/静默去重（复用现有 dedupe），无棘轮推进 |
| E5 崩溃恢复 | 在「DK 换新→持久化→发送」各点 kill -9 注入崩溃后重启 | 无未持久化密文外发；恢复后按 N9 语义回退并重协商；不出现双活 epoch |
| E6 离线积压 | 对端离线 >N 条消息后重连（多个 epoch 跨越） | 逐 epoch 顺序解密或明确部分失败文案；不跳过未验证明文 |
| E7 真机性能 | Android/iOS/macOS 真机（授权后）跑握手 + 1000 消息 + 10 次 epoch 换钥 | §3.3 预算区间全项实测落入；超预算 → FAIL 不放宽 |

### 5.2 与 AC-33 的对应

AC-33 要求「泄露后更新/乱序/重放/崩溃恢复真实实验符合**选定协议保证**」——即实验结论必须引用 §3.2 的声明条款逐条对账（例如 E1 的通过标准就是条款 2 的 epoch 粒度自愈，而非夸大为逐消息）。真机部分依赖设备授权（C13/BLOCKED_USER_AUTH 机制），未授权平台只能 BLOCKED，不得以模拟器替代（plan §4 L2 约束）。

## 6. 历史 Olm 与新 suite 隔离设计（AC-33）

| 维度 | Olm（历史） | pq-olm.v1（新） |
|---|---|---|
| suite 标识 | `ProtocolSuite('olm', 1)` | `ProtocolSuite('pq-olm', 1)` 独立注册（registry 键隔离 [确证（registry 机制）]） |
| 会话存储 | 既有 Olm session pickle | 独立前缀 `crypto_pq_state:*`（chain/epoch/DK/PQ pin 分表） |
| 身份 pin | curve25519 指纹 | curve25519 pin 保留 + **新增独立 PQ pin**（同一 peer 两条 pin） |
| capability | manifest `capabilities` 含 'olm' | 含 'pq-olm.v1'（C02 签名 manifest 内） |
| 升级策略 | **不自动升级**：现存 olm 会话继续按 olm 收发至自然终结 | 仅新建会话且开关开启时可选；旧会话收到对方 pq 元数据 → N12 拒绝 |
| 服务端 | 既有 prekey 表 | PQ prekey 独立字段/池；旧端忽略未知字段，双向兼容 |
| UI/文案 | 「端到端加密」 | 「抗量子混合加密（PROTOTYPE）」——措辞受 §3.2 条款 5 约束 |

## 7. 决策卡草案 D-04（待用户批准；批准前生产接线禁止）

```
D-04 混合抗量子单聊默认启用策略
──────────────────────────────
状态：DRAFT（C16 profile 阶段产出；无本卡批准 → pq-olm 保持 off，不进任何 release）

Q1 默认启用？
   推荐选项 a)：默认关闭（feature flag off），仅测试构建可开启。
   理由：原语层（ml-kem crate）无第三方审计；export compliance 影响未裁决；
   真机预算未实测。先以 PROTOTYPE + 可选 opt-in 积累证据，再议默认开。
   备选 b)：对 capability 齐备的对端默认开——需先满足 Q2/Q3/Q4 全绿。

Q2 依赖新增面
   - RustCrypto ml-kem（推荐）：Apache-2.0 OR MIT；纯 Rust；无审计（披露项）。
     引入方式：flutter_rust_bridge 插件（参照 flutter_vodozemac 模式），
     pubspec 变更 + 可能的插件 podspec——**不动 ios/*/macos/** 保留区；
     若集成验证发现必须改保留区 → BLOCKED_PROTECTED_PATH 上报，不绕行。
   - 产物体积增量：预估几十 KB/架构 [UNVERIFIED，构建实测后写入本卡终版]。
   - 备选 aws-lc-rs：FIPS 背书 vs C 工具链/体积成本（客户有 FIPS 要求时切换）。
   - Python 侧（仅 CI/interop）：liboqs-python——不进客户端二进制。

Q3 iOS/macOS App Store 合规注意项（法律边界：仅列核对项，结论归用户/法务）
   - 自带第三方加密实现（Rust ml-kem）方向上属 non-exempt 申报路径：
     ITSAppUsesNonExemptEncryption=true + ASC 附加文档流程。
   - 法国区声明（ANSSI/SGLSAT 历史要求；EU 2021/821 后大体简化但 ASC 可能
     仍提示）→ 发布前核对当时 Apple 官方指引 [UNVERIFIED 现行细则]。
   - 年终 self-classification report（5D992 mass-market）义务核对。
   - Android 侧无对应申报门，但 SBOM/CVE 监控义务进 C17 台账。

Q4 批准前置条件（全部满足才可提交本卡为 APPROVED）
   1. vectors-plan 三层全绿（ACVP KAT + 双源 golden vectors + 规范映射表 review）；
   2. §5 实验矩阵 E1–E6 本地全绿；E7 真机预算落入 §3.3 区间；
   3. §4 负例 N1–N12 全部有 RED→GREEN 证据；
   4. ml-kem 版本锁定 + SBOM/CVE owner 落 C17 台账；
   5. export compliance 核对项全部有用户/法务结论；
   6. 独立 reviewer 复核（含对 §3.2 声明的逐条措辞审查）。
```

## 8. 实施边界建议（给后续实施卡）

1. **本卡（C16-profile）到此为止**：不写 `lib/service/e2ee/pq_*.dart`、不生成 fixture 实现、不改 pubspec——上述属于 D-04 批准后按 plan §5 C16 步骤的新卡（且 olm/outbox/capability caller 与 C15 接线串行，lease 走 ownership.tsv）。
2. 实施顺序建议：vectors 层（第 1+2 层）→ 握手层 → ratchet 层 → 负例全量 → 恢复实验 → 真机预算；每步独立 commit。
3. 服务端 PQ prekey 存储/claim 扩展走 Backend 迁移卡（C12 lease 独占），客户端先行时用 fixture/stub server。
4. 若上游 vodozemac 未来发布官方 PQ 支持（selection-matrix R2），应触发本 profile 复审——优先上游方案减少自有组合层面积。
5. 所有 `[UNVERIFIED]` 项（体积、时延、aws-lc CAVP 证书范围、法国细则、libcrux 细节、Matrix roadmap）在 D-04 终版前必须消解或保持披露。

## 9. 参考（一手来源，检索/抓取于 2026-10-03）

- PQXDH 规范：https://signal.org/docs/specifications/pqxdh/
- SPQR 公告（2025-10-02）：https://signal.org/blog/spqr/
- RustCrypto/KEMs ml-kem：https://github.com/RustCrypto/KEMs（README 审计声明）
- liboqs：https://github.com/open-quantum-safe/liboqs（生产警告原文）
- aws-lc-rs：https://github.com/aws/aws-lc-rs ；NIST CSRC CAVP 列表
- Apple PQ3：https://security.apple.com/blog/imessage-pq3/ ；Apple 出口合规指引（developer.apple.com）
- libsignal 许可证：https://github.com/signalapp/libsignal （AGPL-3.0）；signal.org 2016 许可声明
- 仓内基线：docs/reference/e2ee-protocol-specification.md §5.3/§8；imboyapp pubspec（flutter_vodozemac ^0.8.1）；lib/service/e2ee/olm_protocol.dart、device_manifest.dart、capability_negotiator.dart（只读勘察）
