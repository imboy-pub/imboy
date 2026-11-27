# E2EE v2 架构体系入口 / E2EE v2 Architecture Index

> 本目录是 E2EE v2 架构的编号 ADR 体系（docs/README 目录口径备注：`guides/e2ee/v2/` 内部编号 ADR 属于该体系，不并入 `docs/adr/`）。本页只做**导航索引**，不新增权威内容；协议规范真源见 [E2EE Protocol Specification](../../../reference/e2ee-protocol-specification.md)，体系定位与标准驱动流程见 [standard/README](../standard/README.md)。

## 阅读顺序 / Reading order

1. [00 Change Governance（冻结门）](./00-freeze-gate.md) — 体系变更治理规则
2. [01 Overview](./01-overview.md) — 架构总览
3. [02 Protocol Registry](./02-protocol.md) → [03 Device Identity](./03-device-identity.md) → [04 Capability Negotiation](./04-capability-negotiation.md) → [05 Metadata Versioning](./05-metadata-version.md) → [06 Device Trust](./06-device-trust.md) → [07 Storage](./07-storage.md) → [08 Threat Model](./08-threat-model.md) → [09 Rejected Decisions](./09-rejected-decisions.md)
4. [10 Dependency Graph](./10-adr-dependency-graph.md) · [11 Backward Compatibility Matrix](./11-backward-compatibility-matrix.md) · [12 Reference Implementation](./12-reference-implementation.md) · [13 Room Key over Olm](./13-room-key-over-olm.md)

## 现行决策 / Active decisions（supersede 链）

| # | 主题 | 取代 |
|---|------|------|
| [14](./14-supersedes-08-security-baseline.md) | 行业顶级安全基线与发布治理 | 08 |
| [15](./15-supersedes-05-13-authenticated-envelope.md) | Protected Frame v3 与禁止未认证降级 | 05/13 |
| [16](./16-supersedes-03-04-06-device-trust.md) | Device-bound Auth、Cross-signing 与 Key Transparency | 03/04/06 |
| [17](./17-supersedes-07-recovery-vault-v2.md) | Recovery Vault v2 | 07 |
| [18](./18-supersedes-07-compliance-boundary.md) | Strict 与 Compliance 信任边界 | 07 |
| [19](./19-supersedes-09-r3-mls.md) | MLS 1.0 群聊目标架构 | 09 |
| [24 HOTFIX-04](./24-unified-olm-only-and-rsa-decrypt-only.md) | Unified Olm-only v3 与 RSA decrypt-only | — |
| [26](./26-supersedes-15-counter-semantics.md) | `epoch_or_counter` 语义定案 | 15 §3.1/§7.2 |
| [31](./31-top-tier-definition-2026-revision.md) | Top-tier Definition 2026 修订 | — |

## 专题 / Topic docs

| # | 主题 |
|---|------|
| [25 提案](./25-proposal-replay-counter-semantics.md) | replay/counter 语义提案（已被 26 定案取代） |
| [27](./27-e2ee-061-attachment-encryption-design.md) | E2EE-061 附件独立 content key 与分块 AEAD |
| [28](./28-e2ee-065-066-key-transparency-research.md) | E2EE-065/066 Key Transparency 调研与设计 |
| [29](./29-e2ee-065-transparency-profile-v1.md) | Transparency Profile v1（冻结草案） |

> 编号缺口（20–23、30）为已废弃提案的空位，按 00 冻结门规则不复用编号。
