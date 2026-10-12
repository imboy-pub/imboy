# C12 性能窗口结果（CACHE_V221_BACKEND_CLOSEOUT_V1）

> 执行时间：2026-10-12T04:10Z–04:35Z（Asia/Shanghai 12:10–12:35）
> 候选绑定：f1aa1ce016eaa6e4db18e52e6ee6fba51cdb745303c057aaeff650c9b990020c（git 2873dbfd67）
> 证据根：~/.codex/cache-closeout-runs/zcode-20261011T130725Z/raw/perf/
> 机器态：uptime 8 天+（历史 W5-07 记录同构件吞吐 -74%~-79% 机器态退化未消除）

## 1. 判定：PARTIAL（按原计划 19.7 降级口径）

- **有**：候选在两个 profile 上的绝对吞吐、轮内方差、绑定到 CANDIDATE_SHA256 的原始 JSON。
- **缺**：同环境、同 ops 口径的 pre-closeout 基线对照——W0 历史轮（r6/r7/r8）全部
  `W0_BLOCKED`（load 15-21）且 1000 ops 口径不可比（w507 轮偏差在案）；
  本轮机器处于历史退化态，重测基线只会得到同污染的对照。
- 按 19.7：机器状态异常时性能窗口记 PARTIAL，不阻塞正确性结论；生产 SLO 归属
  AC-26/27（BLOCKED_EXTERNAL，external-inputs-c13.md）。

## 2. V2 全链 profile（cache_v2_bench，10000 ops/kind，OTP 29 × 12 调度器）

| kind | r4 | r5 | r6 | 轮间波动 |
|---|---:|---:|---:|---|
| rep_put_m（1KB replicate put，全链） | 50,617 | 50,599 | 50,760 | ±0.2% |
| rep_put_s（64B） | ~53k | ~53k | ~54k | ±1% |
| rep_put_l（8KB） | ~54k | ~54k | ~55k | ±1% |
| rep_put_m_w8（8 worker） | ~46k | ~46k | ~47k | ±1%（单 sync 进程串行化瓶颈，符合架构预期） |
| rep_inval（fenced invalidate） | ~112k | ~119k | ~119k | ±3% |
| rep_get | ~3.2M | ~3.3M | ~3.3M | ±2% |
| loc_put_m（local put 对照） | ~72万 | ~73万 | ~73万 | ±1% |
| memo_refill | ~24万 | ~24万 | ~24万 | ±1% |
| mixed（4 worker 混合） | 115,374 | 128,344 | 128,131 | ±5% |

- rep vs loc put 差 ≈12.5×：replicate 每 op 含 policy 校验 + gen_call 同步往返 +
  版本分配 + rev/rindex/depcache 三重写 + syn 自环回流（≈18µs）——fail-closed
  一致性链路的真实架构成本（非缺陷；与 G-QR/wire 核查口径一致）。
- 绑定：v2_cand_4/5/6.json 均携带 candidate_sha256=f1aa1ce0…（r1-r3 轮为绑定
  修复前探测轮，保留在 perf/ 下供轮间方差参照，不入终验系列）。

## 3. W0 profile（cache_w0_bench 直驱，10000 ops/worker，1 worker 摘录）

| kind | r1 | r2 | r3 | 备注 |
|---|---:|---:|---:|---|
| get | 2,398,082 | 1,649,077 | 2,049,600 | r2 轮 -31%：机器态抖动典型样本 |
| get_wait | 2,751,032 | 3,462,604 | 3,117,207 | ±13% |
| set | 679,671 | 674,992 | 726,586 | ±4% |
| mixed | 2,384,927 | 2,055,921 | 2,324,500 | ±7% |
| memo | 3,196,931 | 3,265,839 | 3,397,893 | ±3% |
| syn | 354,660 | 368,609 | 372,148 | ±2% |

- self_checks（单飞 loader / 失效竞态 / loader 异常 / 写者崩溃 / 手工回填）三轮全过。
- w0_cand_1/2/3.json 未带 candidate 绑定（直驱路径无 env 契约——W0 bench 是
  depcache 原语级 profile，非 V2 证据系列；绑定要求由 C14 校验器按系列划定）。

## 4. 工具层缺口（登记，不阻塞）

- `run_cache_w0.py` census 静态分类器与当前代码漂移：
  (a) account_list owner 断言（已随 G-01 键消除修复——registry_rows 移除）；
  (b) `EXCEPTION_UNRESOLVED` 生产调用断言仍拦（forwarded_key 形态的 find/3
  调用方清单被当前分类器判未解析）。W0 全套静态普查属 W0 阶段工具，
  其维护不在本轮范围——登记为未闭合项（工具债，非候选缺陷）。

## 5. 原始证据

- V2：raw/perf/v2_cand_{4,5,6}.json（绑定系列）+ v2_cand_{1,2,3}.json（探测轮）
- W0：raw/perf/w0_cand_{1,2,3}.json + 各 .log
- 历史参照：.Codex/runs/cache-v2.2.1-w0/r6-r8（W0_BLOCKED，不可比，仅存档）
