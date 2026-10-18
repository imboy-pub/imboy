# Shared cache validation / 可重建共享缓存验证

## Scope / 范围

This change retains one existing cache/synchronization engine and adds no dependency.
It exposes the explicitly opted-in `{shared_cache, Key}` namespace, reuses identical
configuration fingerprints within a bounded caller-local cache, and advances the
existing reconciliation timer after READY nodeup events. QR authority, atomic
consumption, local credentials and existing sensitive classifications are unchanged.

本次保留现有单一同步引擎，不增加依赖。depcache 提供并发 ETS 读取、single-flight
和依赖失效；syn 仍被其他业务模块使用，因此没有为了缓存单独增加第二套成员协议。
新命名空间只承载可重建、非敏感值，默认白名单为空。部署与回滚见
[cache-cluster.md](cache-cluster.md#11-可重建数据的显式-set-接口--opt-in-reconstructible-set)。

## Reproducible checks / 可复现检查

Use OTP 29.1.2, matching compiled depcache/syn/jsone dependencies, and independent
baseline/candidate directories. These drivers boot cache services only, without a DB.
Compile the cache modules with `ERL_LIBS=deps`, `+debug_info`, their normal `include`/`src/lib` include
paths, and `elib_metric`/`elib_log`. Compile the following driver modules into both
runtime ebins from the **same candidate source**:

```sh
erlc +debug_info -I include -I src/lib -o ebin \
  scripts/cache_v2_bench.erl scripts/cache_v21_dcheck.erl \
  scripts/cache_stress_bench.erl scripts/cache_digest_bench.erl \
  scripts/cache_shared_dcheck.erl scripts/cache_digest_upgrade_check.erl

# Complete behavioral checks on the candidate; isolated loopback epmd port.
export ERL_EPMD_PORT=43729
epmd -daemon -address 127.0.0.1
ERL_AFLAGS='+S 3:3' erl -setcookie imboy_cache_isolated_bench -noshell \
  -pa ebin deps/*/ebin \
  -eval 'cache_shared_dcheck:main(["/tmp/shared-cache.json"]),halt().'

# Before/after profiles; never run competing tests simultaneously.
CACHE_BASELINE_DIR=/absolute/baseline \
CACHE_CANDIDATE_DIR=/absolute/candidate \
CACHE_RESULTS_DIR=/tmp/cache-comparison \
  scripts/run_cache_comparison.sh

# Caller memory and code replacement checks are separate from cache throughput.
CACHE_DIGEST_CALLERS=1000 CACHE_DIGEST_CALLS=20 \
CACHE_DIGEST_OUTPUT=/tmp/digest-memory.json \
  erl -noshell -pa ebin deps/*/ebin -s cache_digest_bench run
erl -noshell -pa ebin deps/*/ebin -s cache_digest_upgrade_check run
```

The baseline has no shared_cache registry entry, so the performance harness adds
that entry through the pre-existing test override. The candidate uses its real
shared_cache entry. The fault driver explicitly asserts **zero registry overrides**.
The benchmark's separate local-mode fixture entry is not a production namespace.
Keep baseline product beams unchanged; every JSON result records loaded module
paths/hashes, limits, CPU denominator, errors and sampled resources.

基线与候选必须使用同一测试驱动、合法 TTL、相同数据和预算。100k 配置使用显式
扩大预算的测试配置，不能宣称默认配置可存 100k；默认 `n_max=50000`、64 B 值的负向验证
应恰好接受 50,000 个、拒绝后续 50,000 个并返回 `index_full`。没有吞吐结果时，
不能用失败的预填充冒充性能样本。

## Measurement contract / 测量口径

- 1/2/3 same-host BEAM nodes; 10k/50k/100k keys; 64 B/1 KiB values.
- 32 workers per node, 10-second measured windows. Mixed means 20% SET, 80% GET.
- Preparation/full-key verification is outside API latency timing. Every value is
  checked before and after; fixed-value stress is complemented by changing-value
  correctness and partition/restart tests.
- Latency p50/p95/p99 are power-of-two histogram **upper bounds**, not exact samples.
- Successful throughput excludes safe mixed-workload cache misses; completed API
  throughput and miss counts are separate. A read-only miss is an error.
- Actual aggregate BEAM user+system CPU comes from `/proc/PID/stat`, not scheduler
  configuration. It includes the harness, validation and histogram/sampling work.
- This cloud exposes affinity CPUs 0–8 (9 CPUs); its cgroup quota file is unavailable.
  Results use that explicit conservative denominator. `CACHE_STRESS_CPUS` can supply
  a verified quota elsewhere. No spin loops are used to inflate utilization.
- BEAM memory and queue maxima are sampled every 100 ms; these are observations,
  not hard RSS/mailbox bounds. Continuous overload can reject writes.

## Observed results / 实测结果

The final two-slot policy and nodeup timer revision gave the following matched
baseline/candidate run. Cache sources are byte-identical between PR19's merged
head and main `83899ae2`; the raw reports record loaded beam hashes and paths.

| Profile | Baseline successful ops/s | Candidate ops/s | Candidate CPU / 9 CPUs | Errors |
|---|---:|---:|---:|---:|
| 1 node, 10k, 64 B, read | 1,762,132 | 1,914,544 | 87.0% | 0 |
| 1 node, 10k, 64 B, write | 12,011 | 31,592 | 25.4% | 0 |
| 1 node, 100k, 1 KiB, read | 1,198,536 | 1,230,558 | 86.7% | 0 |
| 2 nodes, 10k, 1 KiB, mixed | 53,608 | 103,721 | 61.0% | 0 |
| 3 nodes, 50k, 64 B, mixed | 53,339 | 105,759 | 77.2% | 0 |

The 3-node mixed workload consumed approximately 6.95 CPU cores, exceeding the
requested two-thirds of affinity-visible capacity. Write-only throughput remains
serialized by the existing admission/apply pipeline and does not meet that CPU
utilization target. Single-node read samples vary across runs; **no read-speed gain
is claimed**. The write p99 upper bound improved from 4.096 ms to 2.048 ms.
Two-node mixed safe misses were 2 baseline / 5 candidate; three-node misses were
zero in both arms. Every final key verified. The 100k/1 KiB candidate used about
206.6 MiB sampled BEAM memory; 3-node sampled memory summed to 207.4 MiB versus
203.5 MiB baseline. These are same-host measurements, not production SLOs.

内存验收曾发现仅限制 ETF 字节数不够：3,000 字符列表因紧凑编码被保留，导致
1,000 个调用者增加约 228 MB。加入结构与底层二进制限制后，同一场景降至约
88 KB 调用者保留增量；三个配置波次后无存活调用者。完整 VM 内存有运行时分配器
和代码装载变化，不能将调用者增量等同 RSS。原始失败样本与修正结果均保留。

## Simpler final choice / 最终简化选择

Three balanced-order single-node write repetitions compared the original policy,
normalization alone, and three-slot memoization. Median successful throughput was
13,446 / 14,713 / 32,964 ops/s respectively. Normalization alone removed all caller
state but retained little of the measured improvement, so it was not selected.

A further matched comparison removed the budget-digest slot while keeping the
configuration and registry fingerprints cached. The two-slot median was 32,427
ops/s versus 33,319 for three slots (97.3% retained). In the three-node, 50k-key,
64-byte mixed workload, two slots achieved 105,354 ops/s versus 107,076 (98.4%
retained), with zero errors and 76.4% of the nine affinity-visible CPUs in actual
BEAM CPU time. These are separate matched comparisons, not ratios assembled from
unmatched runs. The final implementation therefore uses **two slots** and hashes
the budget digest directly.

For 1,000 live callers after GC, both two- and three-slot versions retained about
88 KB above the measured caller baseline across small, 3,000-character and
5,000-character configuration cases. Normalization alone retained approximately
0–112 bytes. All workers exited after measurement. The structural admission guard
is retained because the earlier ETF-only guard allowed approximately 228 MB in the
3,000-character case. The bound limits memo admission, not arbitrary application
configuration size, total VM RSS, or the existing replication engine's state.

## Correctness and limitations / 正确性与限制

The final two-slot candidate passed 220 focused EUnit tests on PR19 follow-up
`0ce03f30`; the same cache sources were preserved when aligning to latest-main
follow-up `f76c9806` and its identical-tree merge to main `f3ca4fbf`.
The tests span 14 cache modules. A separate real code-replacement check verified changed
fingerprints, code-token replacement, soft purge, and restoration of the original
module. These focused checks do not replace the combined full native EUnit gate.

The final real-namespace driver passed default SET/DELETE denial; existing sensitive,
QR and unregistered namespace denial; 1/2/3-node CRUD; unchanged original expiry;
actual stale/duplicate frame replay; concurrent writer convergence; real bidirectional
partition/rejoin with failed reconnect attempts before and after partitioned writes; sync/cache restart snapshots; same-name node restart; tombstone
replay; and index/value admission exhaustion and recovery.

With distinct directional cookies and explicitly failed reconnect attempts, natural
rejoin measured 1.016 s / 1.020 s for two / three nodes on the final candidate.
Six deterministic timer regressions cover 10,000 nodeups,
no postponement, canceled/stale/queued references, non-READY state preservation,
periodic fallback, and timer-owner death. Nodeup only advances the existing timer;
it does not declare READY or weaken any proof. If syn visibility is delayed, the
60-second fallback remains. Sustained node churn can trigger roughly one additional
reconciliation per second. No universal one-second freshness guarantee is offered.

接收方扣除 5 秒时钟偏差余量并向下取整，极短 TTL 可能仅本地可见。全节点重启
丢失可重建数据；有限 TTL、墓碑/日志预算和既有恢复切点约束仍然适用。
并发和分区下提供最终收敛，不提供线性一致、原子消费、分布式锁或权限权威。
同机测试不等同真实跨机网络、磁盘持久化或任意长分区证明。
