# 对标矩阵：IMBoy vs 行业六玩家（2026-08 快照）

> **层**：Research ｜ **复核周期**：季度 ｜ **本次日期**：2026-08-01
> **用途**：差距可视化总表；Gap Matrix 的行业侧输入。
> **图例**：✅生产级 / 🟡部分或在建 / ❌无 / 📋路线图 ｜ IMBoy 列数据源=`../gap-matrix.md`（2026-08-01 三代理核查）

| 能力 | Signal | WhatsApp | iMessage | Matrix/Element | Threema | Wire | **IMBoy** |
|---|---|---|---|---|---|---|---|
| 1:1 协议 | Signal Protocol+PQXDH | Signal Protocol | PQ3 | Olm | Ibex | MLS | **Olm（vodozemac）🟡 真机未验** |
| 群协议 | Signal(SK) | Signal(SK) | 自有 | Megolm | Ibex 群 | MLS | **Megolm+room-key-over-Olm 🟡** |
| 逐消息 FS | ✅ | ✅ | ✅ | ✅ | ✅(5.0) | ✅ | **✅（Olm 层）** |
| PCS | ✅ | ✅ | ✅（周期 rekey） | 🟡（Megolm 轮换滞后） | ✅ | ✅（每 commit） | **🟡 E2EE-031 PENDING** |
| PQ 握手 | ✅ | ❌ | ✅ | ❌ | 📋 | ❌ | **📋 TT-R1** |
| PQ 棘轮 | ✅(SPQR) | ❌ | ✅ | ❌ | ❌ | ❌ | **📋 TT-R1** |
| SAS/QR 验证 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | **❌ 算法在零 UI（TT-B1）** |
| 交叉签名 | ✅ | ✅ | ✅ | ✅ | ❌ | ✅(E2EI) | **❌ 地基在零接线（TT-B1）** |
| Key Transparency | 🟡(beta 7.70) | ✅(AKD+Plexi) | ✅(CKV) | ❌ | ❌ | 🟡 | **🟡 Merkle 库在未接线（TT-B3）** |
| E2EE 备份 | 🟡 | ✅(HSM+OPAQUE) | ✅(iCloud) | ✅(4S) | ✅ | ✅ | **🟡 仅 RSA；Megolm 未入（TT-B4）** |
| 换设备 1:1 历史 | ❌（设计如此） | ✅（备份） | ✅ | 🟡（验证/备份） | ❌ | ✅ | **❌ 不可恢复=设计选择，文档缺失（TT-B4）** |
| 设备吊销级联 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | **❌ 服务端/客户端均不清 Olm 键（TT-E1）** |
| 强制设备验证策略 | ❌ | ❌ | 🟡(高风险用户) | ✅(2026-10 生效) | ❌ | ✅ | **❌（TT-B5）** |
| 第三方审计 | ❌公开（形式化代替） | ✅(NCC×2) | ✅(Tamarin 学术) | ✅(NCC/LA) | ✅(Cure53×2) | ✅(SRLabs 系) | **📋 就绪包形态（TT-D5 降级）** |
| 形式化验证 | ✅(ProVerif/hax) | 🟡(备份 UC) | ✅(Tamarin) | ✅(Tamarin 2024) | ✅(Ibex) | 🟡 | **📋 TT-R3** |
| 双端 E2E CI | 内部 | 内部 | 内部 | ✅(complement-crypto 公开) | 内部 | 内部 | **❌（TT-C2）** |
| KAT/互操作向量 | ❌官方包 | ❌ | ❌ | ❌官方包 | ❌ | ❌ | **🟡 双实现 golden（TT-C1）** |
| 元数据最小化 | ✅(sealed sender) | ✅(Noise Pipes) | 🟡 | ❌ | ✅ | 🟡 | **❌（台账 Acknowledged）** |

## 读法与结论

1. **IMBoy 协议栈位置**：Olm/Megolm 与 Matrix 同族——vodozemac 已审计（LA 2022）是我们的最大现成资产；Matrix 列的 🟡/❌（PCS 滞后、无 KT、无 PQ）就是我们的先天短板，KT 实施（P3-8）后我们将在该列局部反超 Matrix 现状。
2. **与第一梯队（Signal/Apple）的真实差距**：PQ（两项）、形式化验证、强制设备验证、KT 生产级、元数据。前三项按用户决策列路线图；后两项 KT 在计划内、元数据台账登记。
3. **与第二梯队（WhatsApp/Wire）的可比口径**：WhatsApp 无 PQ 仍被市场视为顶级——证明"安全属性达标+可验证性+审计透明"可以不依赖 PQ 成立。这是 ADR 31 的核心论据。
4. **可宣称边界**：本矩阵 IMBoy 列任何 🟡/❌ 项，对外材料一律不得写 ✅ 口径（与 `../evidence-matrix.md` 不可宣称清单联动）。
