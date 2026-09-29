# Rollback Decision Table — 旧版回滚禁写合同（TSID-08 / AC-08D）

> 核心禁令：**新版 generator 一旦产生任何写入（新 ID 落库），旧版 generator 禁止再接写。**
> 原因：旧版无 durable fence——其内存 cursor 从 0/时钟起步，不知道新版已推进的
> 高水位，重启后（甚至立即，若其逻辑时钟落后）必然生成与新版已用区间重叠的 ID。

## 判定表

| # | 场景 | 新版是否已写新 ID | 旧版回滚允许？ | 理由 / 操作 |
|---|---|---|---|---|
| R1 | 新版启动失败，从未 ready，零写入 | 否 | ✅ 允许 | durable store 只有 bootstrap floor，无消费；旧版直接接回 |
| R2 | 新版 ready 但零业务流量，readyz 从未 200 前流量未切 | 否（generate 未被业务调用） | ✅ 允许（有条件） | 条件：无法证明零 generate 调用（如启动期内部探测）时按 R3 处理 |
| R3 | 新版 ready 且接受过流量 | **是** | ❌ **禁止旧版接写** | 旧 cursor 无知新水位 → 必然重复 ID。唯一路径：走 R5 重爬水位 |
| R4 | 新版 FENCED（存储故障），fence 未越 | fence 内可能有少量写入 | ❌ 禁止 | fence 内写入已落库（tls < safe_before 的 ID 是真实数据）——同 R3 |
| R5 | 回滚诉求 + 新版已写入 | — | ✅ **有条件回滚**：执行"再 cutover" | 步骤：①再停写 ②对新版写入后的库重跑 scanner（§runbook 1-2）③新 floor=新高水位+1 ④旧版**仍不得接写**；升级修复后的新版以新 floor bootstrap ⑤验证首批 ID > 新高水位 |
| R6 | 数据回档（restore 到 cutover 前备份） | 否（数据已回退） | ✅ 允许 | 库回到旧版水位；但新版 durable store 的 floor 已超前 → 新版重启前必须**重新 bootstrap**（floor 重算），否则 clock_behind 拒启（安全方向） |

## R5（再 cutover）检查单

1. [ ] 旧版保持停写（严禁直接切回）
2. [ ] 重跑 scanner 双扫，digest 一致
3. [ ] 新 floor_candidate = 高水位 + 1（含新版写入的 ID）
4. [ ] 新 floor 超前墙钟 ≤ max_initial_lead_ms（60s）——超出则 BLOCKED_CUTOVER
5. [ ] durable store 重新 bootstrap（fresh 清空后按新 floor 预置）
6. [ ] 修复后的新版以 existing 启动，首批 ID ts ≥ 新 floor
7. [ ] 全程业务不可用窗口 = 再停写时长；须事先公告

## 为什么"旧版停写"是硬合同（AC-08D）

- 旧版 cursor：内存 atomics，重启失忆（TSID-00 RED-2 已实证：重启后首 ID 从 0 重来）。
- 新版已用区间持久在两处：数据行 + durable store fence。
- 旧版无法读取 fence（不知道新水位）→ 它生成的任何 ID 都可能与已用区间重叠。
- 检测手段（事后）：scanner 的 `negative_id` / 跨表同 ts 无法完全兜底重复——
  **预防（R3/R4 禁令）是唯一可靠防线，事后检测只作稽核不作防线。**
