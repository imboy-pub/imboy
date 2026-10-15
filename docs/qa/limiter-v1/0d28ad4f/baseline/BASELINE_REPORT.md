# Limiter V1 AC-16 旧基线报告（L0，HEAD 0d28ad4f）

冻结输入表：同目录 [FROZEN_INPUTS.md](./FROZEN_INPUTS.md)。
原始数据：[baseline_a.json](./baseline_a.json)、[baseline_b.json](./baseline_b.json)。
可复现脚本：[baseline_a.escript](./baseline_a.escript)、[baseline_b.escript](./baseline_b.escript)。
测量日期：2026-10-15。测量人：A4（验收工具/真实入口/性能）。

## 基线 A：E2E 旧真实成本（真实链，N=500 错误密码 ×3 重复）

真实 `passport_logic:do_login/5`（type=account）→ `login_security_logic` →
`login_attempt_ds`（真实 gen_server+ETS+imboy_cache/depcache）→ `verify_user`
（真实 elib_password 校验）。mock 边界：仅 user_ds find_by_* 内存 stub；
Ip 恒 `<<>>`（旧代码硬编码，真实本地直连形态）。

| 指标 | run1 | run2 | run3 |
|---|---|---|---|
| p50 | 31.0 µs | 30.8 µs | 30.2 µs |
| p95 | 33.7 µs | 32.8 µs | 32.9 µs |
| p99 | 37.4 µs | 36.1 µs | 33.7 µs |
| max | 80.1 µs | 92.5 µs | 42.4 µs |
| 吞吐 | 31896 req/s | 32284 req/s | 32823 req/s |
| reductions/req | 5164 | 5161 | 5161 |
| 实际账户记账数（500 请求 get_attempts>0 的数量） | **0** | **0** | **0** |
| check_ip_rate_limit Cache 计数写入（键 `{login_attempt_ip_limit, <<>>}`） | **未写入（null）** | 同 | 同 |

**缺陷 oracle 复现**：500 次真实错误密码后账户记账数恒 0、IP 计数键从未写入
——`record_login_failure` 先做 `check_ip_rate_limit(<<>>)` → invalid_ip 提前
返回，账户 ETS 从未被写（计划 §5.F1 形态）。三次重复一致。

候选（新实现）测量时，同口径链上**每次请求计数必须 +1**（AC-01 fixed 已在
REST 全链证明 1..F 递增），行为差异报告以此基线为对照。

## 基线 B：同有效记账工作量（真实旧 D/Cache/throttle，100k 事件/场景 ×3 重复）

真实旧 `login_attempt_ds`（gen_server+ETS+imboy_cache/depcache）+ 真实
`throttle`（passport_per_ip {10, per_minute}）。输入非空规范键；全部记账
事件有效（check_ip 每 IP ≤ 默认 100/h 限内）。

| 场景 | workers | 有效事件 | p50 µs | p95 µs（3 次区间） | p99 µs（3 次区间） | 吞吐 eps（3 次区间） | reductions/事件 |
|---|---|---|---|---|---|---|---|
| record_failure/2 | 1 | 100000 | 18.5–18.7 | 20.5–20.7 | 24.5–28.0（p99 区间） | 52631–52947 | 30 |
| check_ip_rate_limit/1 | 1 | 100000 | 3.3–3.4 | 3.9–4.0 | 5.1–6.4 | 281684–287289 | 26 |
| throttle:check(passport_per_ip, Ip) | 1 | 100000（9000 允许/91000 拒绝） | 0.2 | 0.2 | 0.3 | 4944579–5016889 | 26 |
| reset/2 | 1 | 100000 | 0.1 | 0.3 | 0.3 | 6772133–6842265 | 33 |
| 混合 80/15/5 | 1 | 100000 | 3.4 | 17.8–18.7 | 19.3–22.2 | 169946–185724 | 21 |
| 混合 80/15/5 | 8 | 100000 | 35.1 | 140.8–143.1 | 146.9–165.4 | 20824–20947 | 22 |

**owner（gen_server）健康度**：全部场景 3 次重复收齐后 `login_attempt_ds`
mailbox_len = 0（有界）；max 等待受调度/GC 抖动影响（观测区间 record
~1.0–1.3ms、混合 8w 0.6–2.2ms），原始值见 JSON。

## C.9 预算对照（候选测量时的验收阈值，来自附录 C.9）

1. 同有效记账工作量：新 owner 单事件成本 ≤ 旧实现同口径成本 × 1.5
   —— 旧口径即本报告基线 B 各场景 µs 值（record_failure p50 ≈ 18.5µs 为
   最贵路径；新实现 admit/failure/check/reset 各自对齐同场景比较）。
2. 账户域操作 p99 ≤ 100 ms、max ≤ 500 ms —— 旧侧单事件 µs 级，无排队。
3. owner mailbox 峰值有界 —— 旧侧无 owner 排队（call 同步），mailbox 恒 0。
4. 混合负载不被饿死 —— 旧侧混合 8w p99 ≈ 165µs 量级，为候选对照下限参照。

## 测量顺序与复测

顺序：环境冻结 → 探针验证最小启动 → warmup → 场景（A 后 B；B 内按
record → check_ip → throttle → reset → mixed_1w → mixed_8w）→ 每场景 3 次
独立重复（每次前清态）。同条件复测时保留各次与汇总区间，不测后改阈值。
