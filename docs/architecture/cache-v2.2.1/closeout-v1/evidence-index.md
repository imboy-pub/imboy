# C14 证据索引与复算盘点 — evidence-index.md

> 2026-10-15 接续核验：以下 2026-10-12 盘点保留历史。
> 当前源码观察点为 `4c48e666`；新运行根
> `/Users/leeyi/.codex-runs/cache-closeout-round3-20261015/`。
> 第 4 轮 `zcode-closeout-20261014` 原运行根当前不存在，旧 manifest 和矩阵回执
> 不能复核，不继承为新候选 PASS。15 套件首轮串行预验完成：12 项 exit=0、
> 3 项 exit=2；metadata_component 为 9 过/9 超时/有取消，冻结前置不满足。
> `preflight.json` 记录活动进程；`source-inputs-provisional.json` 仅为编译前盘点；
> `c14/` 含 96 项语义复算及 verifier 原始命令/exit/log 摘要；`c11/` 含消费链
> 探针草案和离线检查；`raw/w0_anchor/` 与 `w0-anchor-copy-receipt.json` 保存
> r7/r8 原生文件及复制摘要。verifier 合成 fixture 不计入产品回执。
> 本次静态通过：IDs/计划摘要、53/53 verifier 自测、63 条告警规则检查及规则测试。
> `targeted/results.json` 与各模块日志保存首轮真实结果；
> `targeted-config-recheck/` 保存配置适配后 capability 15/15、journal Test passed，
> 两项均 exit=0；原始失败保留。`prevalidation-evidence-index.json` 保存日志摘要。
> 所有这些结果均为冻结前预验，不是最终候选回执；metadata 不再重试。
> 本次 C09/C10/QR/迁移/全量/性能和实际 C11 消费均未运行。当前判定与未闭合集合
> 以 closeout-report-v1.md 文末“本次接续核验”为准。

> 任务：CACHE_V221_BACKEND_CLOSEOUT_V1 / C14 证据复算前置盘点
> 本文性质：只读取证产出的证据清单（EVIDENCE_INDEX / NOT_EXECUTED）。
> 盘点快照时点：2026-10-12T01:33:11Z（本地 2026-10-12 09:33:11 +0800）
> 证据根：`~/.codex/cache-closeout-runs/zcode-20261011T130725Z/`（下文以 `<RUN_ROOT>` 指代）
> 盘点命令：仅 `ls` / `cat` / `python3 -c json.load`（只读）与
> `python3 scripts/check_cache_closeout_v1.py --exits --candidate`（只读校验器）。
> 红线遵守：未运行 dcheck/eunit/任何服务；未执行任何 git 操作；未改动 raw/ 与 logs/ 任何文件。

---

## 0. 活跃写入警示（先读）

盘点期间 `<RUN_ROOT>/raw/` 仍在被外部进程写入：首次扫描（本地 09:26 目录 mtime）时
`diag_c9d` 系列只有 r18–r20 共 3 个文件，数秒后 **diag_c9d_r21.json 新落盘**
（utc 2026-10-12T01:31:22Z，本地 09:31）。至快照时点 01:33:11Z 未再新增。
**结论：C9d 诊断轮在盘点时仍在进行**，本清单数据以 01:33:11Z 快照为准；终验前须以
同口径重扫确认轮次封口。

`<RUN_ROOT>` 下仅 `raw/`、`logs/`、`ebin_drv/` 三项；**不存在 w507 子目录**，
raw/ 内亦无任何子目录（无 `perf_*`、无 `w507_*`）、无 `candidate_freeze.json`。

---

## 1. raw/ 证据清单总表（17 个 JSON，全字段实测）

场景字段名为 `scenarios[{name,status,detail}]`；utc 为 JSON 顶层原值（UTC，本地=+8h）。

### 1.1 three_node_c09 系列（8 个，全 22 场景/轮）

| 文件 | 轮次 | 场景数 | pass | fail | elapsed_ms | utc（JSON 内） | candidate_sha256 | task_id | 失败场景 |
|---|---|---|---|---|---|---|---|---|---|
| three_node_c09_r1.json | 1 | 22 | 15 | 7 | 128625 | 2026-10-11T15:09:36.630Z | **字段缺失** | 缺失 | F5,C9a,C9b,C9c,C9d,C9e,C9f |
| three_node_c09_r2.json | 2 | 22 | 15 | 7 | 138569 | 2026-10-11T15:17:05.371Z | **字段缺失** | 缺失 | F5,C9a,C9b,C9c,C9d,C9e,C9f |
| three_node_c09_r3.json | 3 | 22 | 17 | 5 | 262469 | 2026-10-11T15:23:57.663Z | **字段缺失** | 缺失 | F5,C9c,C9d,C9e,C9f |
| three_node_c09_r4.json | 4 | 22 | 16 | 6 | 73117 | 2026-10-11T15:26:48.250Z | **字段缺失** | 缺失 | F3,F4,F5,C9c,C9d,C9e |
| three_node_c09_r6.json | 6 | 22 | 16 | 6 | 330670 | 2026-10-11T15:53:07.728Z | **字段缺失** | 缺失 | F5,C9a,C9b,C9c,C9d,C9e |
| three_node_c09_r7.json | 7 | 22 | 17 | 5 | 382949 | 2026-10-11T16:01:34.445Z | **字段缺失** | 缺失 | F5,C9a,C9d,C9e,C9f |
| three_node_c09_r8.json | 8 | 22 | 17 | 5 | 382972 | 2026-10-11T23:17:27.465Z | **字段缺失** | 缺失 | F5,C9a,C9d,C9e,C9f |
| three_node_c09_r9.json | 9 | 22 | 19 | 3 | 102331 | 2026-10-12T01:21:18.072Z | **null（键在值空）** | CACHE_V221_BACKEND_CLOSEOUT_V1 | C9d,C9e,C9f |

系列事实：**r5 缺号**（成因见 §2 dcheck_r7/r8）；无任何一轮全 pass；
所有轮均无 64hex 候选绑定；r1–r8 连 `candidate_sha256` 键都不存在，r9 键存在但值为
`null`。演进轨迹：fail 数 7→7→5→6→6→5→5→3（最新 r9 仍余 C9d/C9e/C9f）。

### 1.2 diag_f5 系列（5 个，单场景/轮）

| 文件 | 轮次 | 场景数 | pass | fail | elapsed_ms | utc（JSON 内） | candidate_sha256 | task_id | 结果 |
|---|---|---|---|---|---|---|---|---|---|
| diag_f5_r12.json | 12 | 1 | 0 | 1 | 163728 | 2026-10-11T23:25:54.484Z | null | CACHE_V221_BACKEND_CLOSEOUT_V1 | F5 fail |
| diag_f5_r13.json | 13 | 1 | 0 | 1 | 164047 | 2026-10-11T23:29:34.826Z | null | 同上 | F5 fail |
| diag_f5_r14.json | 14 | 1 | 0 | 1 | 164025 | 2026-10-11T23:34:51.619Z | null | 同上 | F5 fail |
| diag_f5_r15.json | 15 | 1 | 0 | 1 | 175433 | 2026-10-12T01:10:26.838Z | null | 同上 | F5 fail |
| diag_f5_r16.json | 16 | 1 | 1 | 0 | 43075 | 2026-10-12T01:19:24.879Z | null | 同上 | **F5 pass**（elapsed 从 ~164s 骤降至 43s） |

场景名统一为 `F5 early eviction: survivors-only snapshot, no resurrection`。

### 1.3 diag_c9d 系列（4 个，单场景/轮）

| 文件 | 轮次 | 场景数 | pass | fail | elapsed_ms | utc（JSON 内） | candidate_sha256 | task_id | 结果 |
|---|---|---|---|---|---|---|---|---|---|
| diag_c9d_r18.json | 18 | 1 | 0 | 1 | 11161 | 2026-10-12T01:22:12.253Z | null | CACHE_V221_BACKEND_CLOSEOUT_V1 | C9d fail |
| diag_c9d_r19.json | 19 | 1 | 0 | 1 | 11180 | 2026-10-12T01:24:57.238Z | null | 同上 | C9d fail |
| diag_c9d_r20.json | 20 | 1 | 0 | 1 | 11448 | 2026-10-12T01:26:54.487Z | null | 同上 | C9d fail |
| diag_c9d_r21.json | 21 | 1 | 0 | 1 | 11153 | 2026-10-12T01:31:22.233Z | null | 同上 | C9d fail（含 detail，见下） |

场景名统一为 `C9d incarnation change compensates fenced key`。
r21 的 detail 携带错误项 `error:{c9d_no_compensation,0,0,#{...}}`（fkey_gen=0、
sync_counter=0、peer_incarnation 已变化但未触发补偿），crash 栈位于
`cache_v21_dcheck:c9d_incarnation_compensation/2`（scripts/cache_v21_dcheck.erl L1175）
——**C9d 修复至快照时点未收敛**（四连 fail）。

全表公共事实：17/17 文件 `driver_node` 均为 `nonode@nohost`；utc 均为带 `Z` 时区的
合法 ISO8601（2026-10-11T15:09Z ～ 2026-10-12T01:31Z），无 1970 哨兵（C12 §3.4 维度合规）。

---

## 2. logs/dcheck_r*.log ↔ raw JSON 对应关系

映射依据：汇总行 elapsed 与 JSON `elapsed_ms` **逐毫秒一致** + JSON `utc`+8h 与 log
文件 mtime 一致。**结论：json 轮号与 log 编号整体错位，禁止用编号直接对应。**

| log 文件（mtime 本地） | 汇总行（原样） | 对应 JSON | 备注 |
|---|---|---|---|
| dcheck_r1.log (10-11 23:01:45) | 无汇总行（crash） | 无 | `econnrefused` / `net_kernel nodistribution`，集群启动失败 |
| dcheck_r2.log (10-11 23:04:46) | 无汇总行（crash） | 无 | `{badmatch,timeout}` @ start_cluster L559 |
| dcheck_r3.log (10-11 23:09:36) | `== cache_v21_dcheck: 15 passed, 7 failed, 128625 ms ==` | **three_node_c09_r1.json** | elapsed/mtime 双吻合 |
| dcheck_r4.log (10-11 23:17:05) | `== cache_v21_dcheck: 15 passed, 7 failed, 138569 ms ==` | **three_node_c09_r2.json** | |
| dcheck_r5.log (10-11 23:23:57) | `== cache_v21_dcheck: 17 passed, 5 failed, 262469 ms ==` | **three_node_c09_r3.json** | |
| dcheck_r6.log (10-11 23:26:48) | `== cache_v21_dcheck: 16 passed, 6 failed, 73117 ms ==` | **three_node_c09_r4.json** | |
| dcheck_r7.log (10-11 23:43:17) | 无汇总行（crash） | 无 | eval 目标即 `raw/three_node_c09_r5.json`，`undef cache_v21_dcheck:main` —— **r5 缺号第 1 次成因** |
| dcheck_r8.log (10-11 23:47:21) | 无汇总行（crash） | 无 | 同目标 r5，`badrpc undef init_on_peer` —— **r5 缺号第 2 次成因** |
| dcheck_r9.log (10-11 23:53:07) | `== cache_v21_dcheck: 16 passed, 6 failed, 330670 ms ==` | **three_node_c09_r6.json** | |
| dcheck_r10.log (10-12 00:01:34) | `== cache_v21_dcheck: 17 passed, 5 failed, 382949 ms ==` | **three_node_c09_r7.json** | |
| dcheck_r11.log (10-12 07:17:27) | `== cache_v21_dcheck: 17 passed, 5 failed, 382972 ms ==` | **three_node_c09_r8.json** | json utc 23:17:27Z+8h=本地 10-12 07:17:27 与 mtime 吻合 |
| dcheck_r12…r16.log | **不存在** | — | diag_f5_r12–r16 编号沿用轮次号但 **diag 轮未落任何 dcheck log** |
| dcheck_r17.log (10-12 09:21:18) | `== cache_v21_dcheck: 19 passed, 3 failed, 102331 ms ==` | **three_node_c09_r9.json** | |
| （diag_c9d_r18–r21） | 无对应 log | diag_c9d_r18..21.json | 同上，diag 轮无 log 落盘 |

错位规律：成功轮里 json rN ↔ log r{N+2}（N=1..8，因 r1/r2/r7/r8 四次失败尝试占号）；
r9 之后 log 编号跳至 r17（r12–r16 未落盘）。

logs/ 其余日志（盘点完整性记录，C14 不扫 logs/，不构成证据）：`sync_tests_r1–r7`、
`imboy_cache_tests_r1`、`imboy_cache_policy_tests_r1`、`imboy_cache_v2_tests_r1`、
`account_ds_tests_r1`、`qr_repo_r1–r6`、`cache_adjacent_r1`、`adjacent_summary` ——
均为定向套件/邻接预跑日志，raw/ 扁平层无对应 JSON。

---

## 3. 三类证据状态分类

### (a) 可直接作为 C14 终验输入（全 pass + 64hex candidate 绑定）：**0 个**

无任何文件同时满足"scenarios 全 pass"与"携带合法候选绑定"。最接近的
diag_f5_r16（单场景 pass）绑定为 null 且属 diag 前缀。

### (b) 需要终轮重跑（有 fail 或缺 candidate 绑定）：**8 个**

`three_node_c09_r{1,2,3,4,6,7,8,9}.json` 全部：
- 8/8 均无候选绑定（r1–r8 键缺失，r9 值为 null）；
- 8/8 均有 fail 场景（最优 r9 仍 fail C9d/C9e/C9f）；
- C12 §4.1 前置要求 C09 required 场景"至少两轮独立通过"，当前 **0 轮全过**。

### (c) 纯诊断轮（diag_* 前缀，不入证据链）：**9 个**

`diag_f5_r{12,13,14,15,16}.json`（5 个）+ `diag_c9d_r{18,19,20,21}.json`（4 个）。
价值摘要：diag_f5 系列证明 F5 修复在 r16 才收敛（r12–r15 连 fail，r16 pass 且
耗时 164s→43s 量变）；diag_c9d 系列四连 fail（至 r21 未收敛，detail 见 §1.3）。
**警示**：这些文件当前位于 raw/ 扁平层，C14 verifier 默认扫描扁平 `*.json` 会将其
计入（§4 --exits 的 52 个问题中 9 个来自 diag 场景）。终验前须由协调者决定迁移至
子目录（verifier 不扫子目录，C12 §3 性能证据两层结构先例）或保留并接受计入；
本任务只读未迁移。

计数核对：0 + 8 + 9 = 17 = raw/ JSON 总数。

---

## 4. C14 验证器交叉核对（输出原样记录）

命令（工作目录 `/Users/leeyi/project/imboy.pub/imboy`，全部默认路径，只读）：

```bash
python3 scripts/check_cache_closeout_v1.py --exits
python3 scripts/check_cache_closeout_v1.py --candidate
```

### 4.1 `--exits` 原样输出

```text
[FAIL] exits: 52 problem(s) across 17 evidence file(s)
    - diag_c9d_r18.json: scenarios[0] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - diag_c9d_r19.json: scenarios[0] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - diag_c9d_r20.json: scenarios[0] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - diag_c9d_r21.json: scenarios[0] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - diag_f5_r12.json: scenarios[0] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - diag_f5_r13.json: scenarios[0] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - diag_f5_r14.json: scenarios[0] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - diag_f5_r15.json: scenarios[0] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[16] 'C9a stale epoch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[17] 'C9b hash mismatch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[18] 'C9c seq gap compensates fenced generation' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r1.json: scenarios[21] 'C9f nmax ceiling rejects new replicate' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[16] 'C9a stale epoch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[17] 'C9b hash mismatch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[18] 'C9c seq gap compensates fenced generation' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r2.json: scenarios[21] 'C9f nmax ceiling rejects new replicate' status='fail' (non-zero exit)
    - three_node_c09_r3.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r3.json: scenarios[18] 'C9c seq gap compensates fenced generation' status='fail' (non-zero exit)
    - three_node_c09_r3.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r3.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r3.json: scenarios[21] 'C9f nmax ceiling rejects new replicate' status='fail' (non-zero exit)
    - three_node_c09_r4.json: scenarios[13] 'F3 hard crash (halt) + same-name incarnation recovers' status='fail' (non-zero exit)
    - three_node_c09_r4.json: scenarios[14] 'F4 bounded sweep removes expired meta incl. rindex' status='fail' (non-zero exit)
    - three_node_c09_r4.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r4.json: scenarios[18] 'C9c seq gap compensates fenced generation' status='fail' (non-zero exit)
    - three_node_c09_r4.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r4.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r6.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r6.json: scenarios[16] 'C9a stale epoch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r6.json: scenarios[17] 'C9b hash mismatch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r6.json: scenarios[18] 'C9c seq gap compensates fenced generation' status='fail' (non-zero exit)
    - three_node_c09_r6.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r6.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r7.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r7.json: scenarios[16] 'C9a stale epoch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r7.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r7.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r7.json: scenarios[21] 'C9f nmax ceiling rejects new replicate' status='fail' (non-zero exit)
    - three_node_c09_r8.json: scenarios[15] 'F5 early eviction: survivors-only snapshot, no resurrection' status='fail' (non-zero exit)
    - three_node_c09_r8.json: scenarios[16] 'C9a stale epoch event rejected cross-node' status='fail' (non-zero exit)
    - three_node_c09_r8.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r8.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r8.json: scenarios[21] 'C9f nmax ceiling rejects new replicate' status='fail' (non-zero exit)
    - three_node_c09_r9.json: scenarios[19] 'C9d incarnation change compensates fenced key' status='fail' (non-zero exit)
    - three_node_c09_r9.json: scenarios[20] 'C9e partition reconnect returns authority value' status='fail' (non-zero exit)
    - three_node_c09_r9.json: scenarios[21] 'C9f nmax ceiling rejects new replicate' status='fail' (non-zero exit)
SUMMARY: 0/1 checks passed (failed: exits)
```

核对：52 = three_node 系列 44（r1:7 + r2:7 + r3:5 + r4:6 + r6:6 + r7:5 + r8:5 +
r9:3）+ diag_f5 系列 4（r12–r15 各 1，**r16 pass 不列**）+ diag_c9d 系列 4
（r18–r21 各 1）。与验证器逐文件行数完全一致。

### 4.2 `--candidate` 原样输出

```text
[FAIL] candidate: candidate binding inconsistent
    - 17 evidence file(s) carry no candidate binding: diag_c9d_r18.json, diag_c9d_r19.json, diag_c9d_r20.json, diag_c9d_r21.json, diag_f5_r12.json, diag_f5_r13.json, diag_f5_r14.json, diag_f5_r15.json, diag_f5_r16.json, three_node_c09_r1.json, three_node_c09_r2.json, three_node_c09_r3.json, three_node_c09_r4.json, three_node_c09_r6.json, three_node_c09_r7.json, three_node_c09_r8.json, three_node_c09_r9.json
SUMMARY: 0/1 checks passed (failed: candidate)
```

### 4.3 交叉核对结论

验证器两项均 FAIL，且与本清单 §3 分类完全互相印证：
- `--exits` 52 问题 → (b) 类 8 文件全有 fail；(c) 类 diag fail 文件亦被计入
  （r16 pass 未列，佐证其"pass"判定）。
- `--candidate` 17/17 无绑定 → (a) 类为 0 的根因之一（另一根因是场景未全绿）。
- 验证器未报"series 混候选"（因全部无绑定，无从比较）——一旦补绑定，须全系列
  同一 CANDIDATE_SHA（C12 §3.1 系列内禁止混候选）。

---

## 5. 与 C12 窗口计划（§3 证据 schema）的差距清单

按阻断程度排序；"需终轮补"列明确到系列：

| # | 差距 | 影响系列 | C12 条款 | 终轮动作 |
|---|---|---|---|---|
| G1 | **候选绑定全缺**：17/17 无 64hex 绑定。r1–r8 连键都无；r9 与 diag_* 键在值 null（验证器 `check_candidate` 视同无绑定）。驱动侧尚未输出候选字段 | 全部（three_node_c09、diag_f5、diag_c9d） | §3.3、§7（"C09 终轮证据补 candidate_sha256 绑定字段的驱动侧改造……属 C09 收尾 owner 的改动"） | `cache_v21_dcheck.erl` 增顶层候选字段后，**three_node 终轮全系列重跑并绑定 CANDIDATE_SHA**；diag 轮无需补（见 G3） |
| G2 | **场景未全绿**：three_node_c09 最优 r9 仍 fail C9d/C9e/C9f；C9d 修复未收敛（diag_c9d 四连 fail，r21 detail `c9d_no_compensation`）；C12 §4.1"至少两轮独立通过"当前 0 轮 | three_node_c09 | §4.1 前置 1 | C9d/C9e/C9f 闭合后 ≥2 轮独立全 pass（建议新起 `three_node_c12` 系列，C12 §6.2 口径，不与 C09 混编） |
| G3 | **diag_* 位于 raw/ 扁平层**：C14 verifier 默认扫描扁平 `*.json`，9 个 diag 文件（含 8 个 fail 场景）被计入 --exits，终验必挂 | diag_f5、diag_c9d | §3（verifier 只扫扁平层；两层结构先例） | 协调者决定：迁移至子目录（verifier 不扫）或删除/归档；本任务只读未动 |
| G4 | **r5 缺号**：dcheck_r7（`undef main`）与 dcheck_r8（`badrpc undef init_on_peer`）两次启动失败所致；verifier 按 series 分组不要求轮次连续，非阻断 | three_node_c09 | §3.1（仅约束系列内同候选） | 记录在案即可；终轮若新起系列则自然规避 |
| G5 | task_id 不一致：r1–r8 无该字段，r9 与 diag_* 有 `CACHE_V221_BACKEND_CLOSEOUT_V1`；schema 未强制，非阻断 | three_node_c09（旧轮） | §3（未列 task_id 为必填） | 终轮统一携带即可 |
| G6 | **C12 系列证据整体缺失**（前置性）：raw/ 无 `candidate_freeze.json`、无 `three_node_c12_*` / `mixed_c12_*` / `qr_c12_*` / `eunit_full_c12_*` / `perf_c12_*`（两层结构原始层+汇总层均无）、无 w507 子目录 | （尚未产生） | §4.3、§6.1–6.5、§9 | 按 C12 §6 顺序执行窗口后产生；本清单证实**当前一个 C14 终验输入都不存在** |
| G7 | 合规项确认（无差距）：utc 17/17 均为带时区 ISO8601 且时域正常；driver_node 全部 `nonode@nohost` 一致，无系列内节点混号 | 全部 | §3.4 | 无 |

---

## 6. 结论计数（最终报告口径）

- raw/ JSON 总数：**17**（three_node_c09 ×8、diag_f5 ×5、diag_c9d ×4）；快照时点
  2026-10-12T01:33:11Z；**目录仍处活跃写入**（盘点期间新增 diag_c9d_r21）。
- **(a) 可直接作 C14 终验输入：0 个**
- **(b) 需终轮重跑：8 个**（three_node_c09_r{1,2,3,4,6,7,8,9}，均双重不达标：有 fail + 无绑定）
- **(c) 纯诊断轮：9 个**（diag_f5_r{12..16}、diag_c9d_r{18..21}，不入证据链，但现居
  扁平层会污染 C14 扫描，见 G3）
- 验证器：`--exits` FAIL（52 问题/17 文件）、`--candidate` FAIL（17/17 无绑定），
  输出已原样记录于 §4。
- 差距核心：G1（绑定全缺，驱动侧改造未落地）+ G2（C9d/C9e/C9f 未闭合，C9d 未收敛）
  是 C14 终验的两个硬阻断；G3（diag 位置）与 G6（C12 系列未产生）次之。

## 归档注记（协调者，2026-10-12T02:35Z 后）

G3 建议已执行：全部 `diag_f5_*` / `diag_c9d_*` / `diag_c9e_*`（24 个诊断轮 JSON）
已从 raw/ 扁平层迁移至 `raw/diag/` 子目录，C14 verifier 终验扫描时不再被计入
证据链。扁平层现仅保留：candidate_freeze.json、mixed_c10_r1/r2.json、
three_node_c09_r1~r15.json（含终验绿轮 r14/r15）。本文 §1/§2 中 diag 文件的
扁平层路径引用自此注记起以 `raw/diag/` 前缀为准。

## 归档注记二（终态口径，2026-10-12 收尾后补记）

候选冻结与终验完成后，扁平层曾残留 20 个非最终轮 JSON（早期 fail 轮、被替代轮与
无绑定轮：`three_node_c09_b1/r1~r15`、`mixed_c10_b1/b2/r1/r2`、`diag_srace_b2`），
导致上文 §4 记录的校验命令在终态复跑时 FAIL（--exits 计入 fail 场景、--candidate
计 19 文件无绑定）。已执行终态归拢（纯 `mv`，未改写任何 JSON 字节；ledger 引用的
`three_node_c09_b2/b3` 不在移动集合内）：

- 移入 `raw/diag/`：上述 20 个非最终轮（与注记一的 24 个 diag 轮同目录归档）。
- `raw/` 顶层终态仅保留绑定绿灯轮 4 个：`three_node_c09_b2.json`、
  `three_node_c09_b3.json`、`mixed_c10_b3.json`、`mixed_c10_b4.json`
  （另有 `perf/` 子目录；`candidate_freeze.json` 位于 `<RUN_ROOT>` 根）。

归拢后校验器复跑原样输出（命令同 §4，另加 `--utc --ids` 与 ledger 交叉）：

```text
[PASS] ids: 96/96 required ids found in ledger; duplicates=0 extras=0 both-forms=0
[PASS] candidate: 4 evidence file(s) in 4 series, single candidate f1aa1ce016ea.. consistent with ledger
[PASS] exits: all 70 scenario(s) in 4 evidence file(s) status=pass, no missing fields
[PASS] utc: 19 timestamp(s) validated (evidence + ledger), all tz-aware ISO8601 in sane window
SUMMARY: 4/4 checks passed
```

候选绑定独立重算：`git archive 2873dbfd | shasum -a 256` =
`f1aa1ce016eaa6e4db18e52e6ee6fba51cdb745303c057aaeff650c9b990020c`，与
`candidate_freeze.json` 及 4 个绿灯轮记录值逐字节一致。校验器脚本
`scripts/check_cache_closeout_v1.py` 已于同日入库（此前仅存于证据根，未受 git 跟踪）。

## 归档注记三（第 4 轮 ZCODE 2026-10-15：合并树 C10 首执行 + 三层恢复修复）

本轮证据根：`~/.codex/cache-closeout-runs/zcode-closeout-20261014/`。
候选冻结链：a0ea3a69…（8745 文件）→ b0f62c81…（8746 文件）→
9b9ed755f8e5879ee12c55a9d78d3e60ca625e9df275224470f747b66e3c67ae
（最终，8748 文件，head=efc92a3c）。关键回执（raw/ 顶层）：

- `three_node_c09_newb_r1.json` / `three_node_c09_newb_r2.json`：
  C09 38/38 ×2，绑定 b0f62c81（含 cf3f1196）。
- `mixed_c10_v3r1_fix5.json` / `mixed_c10_v3r2_fix5.json` /
  `mixed_c10_newb_r1.json`：C10 三轮 12/16，失败集合逐轮一致
  （M5/M8/M9/M10，开放项）。
- `mixed_c10_base1.json`：0d28ad4f 基线归属判定（与 v3 同 8 挂，
  证明失败为合并树固有）。
- `qr_v3.json`：QR 3/3 PASS，rows_remaining=0。
- `mixed_c10_v3r1.json`（缝合怪无效轮留档）、`mixed_c10_v3r1_rebuilt.json`、
  `mixed_c10_v3r1_fix1/fix2/fix3.json`（修复过程轮）。
- `agent-out/ledger-closeout-tasks.json`：C09/C10/QR/C11/MIGRATION/
  FULL_EUNIT 终态回执。
- legacy 树重建：3a01829d worktree（`~/.codex/tmp/cache-closeout-legacy-3a01829d`，
  handoff 第 8 条执行；本轮收尾后按用户指令清理）。
- 迁移环境阻塞：v1 库被外部移至 ver=2 dirty（已恢复 179 clean）、
  模板占用克隆不一致、165 基座纯 up 166 失败（app 侧函数依赖）。
