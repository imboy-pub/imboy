# ADR 10 — Dependency Graph

> **状态**：Architecture Freeze
> **用途**：可视化 ADR 之间的依赖关系，明确变更传播路径（改 ADR X 会影响哪些 ADR）。

---

## 1. 依赖图

```
                    ┌─────────────────┐
                    │  08-threat-model │  ← 地基：定义 T1-T9 + Protected Assets
                    │  (地基，无依赖)   │
                    └────────┬────────┘
                             │ 所有 ADR 的防御决策必须可追溯到 T*
            ┌────────────────┼────────────────┐
            ▼                ▼                 ▼
    ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
    │ 02-protocol  │  │ 03-device-   │  │ 07-storage   │
    │  (协议抽象)  │  │   identity   │  │  (存储契约)  │
    └──────┬───────┘  └──────┬───────┘  └──────┬───────┘
           │                 │                  │
           │  ProtocolSuite  │  device keys     │  私钥落盘
           │  被引用          │  持久化语义       │
           ▼                 ▼                  │
    ┌──────────────────────────────┐            │
    │   05-metadata-version        │            │
    │   (元数据容器版本化)          │◄───────────┘
    └──────────────┬───────────────┘  存储字段约束
                   │
                   │ meta_version 字段
                   ▼
    ┌──────────────────────────────┐
    │   04-capability-negotiation  │
    │   (协议协商)                  │
    └──────────────┬───────────────┘
                   │
                   │ 协商结果决定 trust 需求
                   ▼
    ┌──────────────────────────────┐
    │   06-device-trust             │
    │   (Trust State / Safety Num)  │
    └──────────────────────────────┘
```

---

## 2. 依赖矩阵（精确版）

| 源 ADR | 依赖目标 ADR | 依赖内容 | 变更影响 |
|---|---|---|---|
| **02-protocol** | 08-threat-model | T2(降级)/T5(PFS) 驱动协商顺序 | 08 新增威胁可能要求 02 追加守护测试 |
| **02-protocol** | 05-metadata | ProtocolSuite 通过 metadata 传输 | 05 字段冻结影响 02 序列化 |
| **03-device-identity** | 02-protocol | RecipientDevice.publicKey 语义 | 02 接口签名变更影响 03 数据模型 |
| **03-device-identity** | 04-capability | capabilities 字段被协商消费 | 04 协商算法变更可能要求 03 字段调整 |
| **03-device-identity** | 08-threat-model | T2/T4/T7/T9 驱动签名设计 | 08 新增威胁可能要求 03 追加审计字段 |
| **04-capability** | 02-protocol | 协商输出 ProtocolSuite | 02 套件增减影响 04 fallback 表 |
| **04-capability** | 06-device-trust | trust state 参与协商（revoked 拒发） | 06 trust 语义变更影响 04 协商 |
| **04-capability** | 08-threat-model | T2(降级攻击) 是核心驱动 | 08 T2 定义变更影响 04 防御层 |
| **05-metadata** | 02-protocol | ProtocolSuite 双写策略 | 02 套件演进触发 05 版本升级 |
| **05-metadata** | 08-threat-model | T7(重放)/T9(rollback) 驱动 counter | 08 新威胁可能要求 05 新字段 |
| **06-device-trust** | 03-device-identity | trust_state 列存于 user_device | 03 表结构变更影响 06 持久化 |
| **06-device-trust** | 08-threat-model | T2(MITM)/T8(社工) 驱动验证 | 08 威胁定义变更影响 06 流程 |
| **07-storage** | 02-protocol | 各协议的 pickle/key 落盘规范 | 02 新协议要求 07 追加存储规范 |
| **07-storage** | 06-device-trust | trust state 本地缓存 | 06 trust 模型变更影响 07 本地存储 |
| **07-storage** | 08-threat-model | T3(DBA)/T5(设备)/T6(备份) 驱动 | 08 新威胁可能要求 07 加固 |

---

## 3. 变更传播路径（改某个 ADR 时需同步检查哪些）

| 触发变更 | 直接影响 | 二级影响 | 检查清单 |
|---|---|---|---|
| 02 接口签名改 | 03/04/05 | 06/07 | 检查所有 `implements E2eeSessionProtocol` |
| 03 表结构改 | 02/04/06 | 05/07 | migration 脚本 + capabilities 查询 + trust 持久化 |
| 04 协商算法改 | 02/06 | 03 | fallback 表 + trust 交互 |
| 05 meta_version 升 | 02 | 全部 | 双写期 + legacy 解析 |
| 06 trust 模型改 | 03/04 | 07 | trust_state 列 + 协商 + 本地缓存 |
| 07 存储契约改 | 02/06 | 全部 | 各协议 pickle + trust 缓存 |
| 08 新增威胁 | 全部 | — | 可追溯矩阵 §4 追加防御点 |

---

## 4. 关键依赖约束（不可破坏）

1. **08 是所有 ADR 的根**：任何防御决策必须可追溯到 T1-T9，否则属过度设计（08 §4）。
2. **02 是协议层的根**：ProtocolSuite 定义只在 02，其余 ADR 引用不可重定义（02 §3 冻结）。
3. **03 是设备数据的根**：user_device / olm_identity 表结构只在 03 定义，06/07 引用不可改字段名（03 §7 冻结）。
4. **无循环依赖**：上述矩阵验证无 A→B→A 环路（除 02↔05 双向引用，但二者通过 01 freeze 流程协调）。

---

## 5. 与其他 ADR 的关系

- **01-overview**：本图是 01 §2 文档结构的可视化补充；
- **00-freeze-gate**：freeze 前的 cross-ADR 一致性核查依据本图。
