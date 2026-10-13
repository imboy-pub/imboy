# C12 — 最终候选冻结 + 全量 EUnit + 本地性能窗口执行计划

> 当前执行入口修订（2026-10-13）：下文盘点数字和旧冻结格式保留为历史。
> 当前候选绑定以 `evidence-schema-v2.md`、`ledger-v2.json` 及 C14 严格校验器为准，
> 不再使用旧台账或旧运行目录作默认终验。当前运行前冻结全部源码、配置、迁移、
> 测试驱动及依赖字节，再从隔离目录执行；外来 WIP 保持原位，干净工作树不是前提。
> 正式性能继续使用本文 §6.5 的既定阈值。W0 环境锚保持 TTL3600；两个 V2 比较臂
> 均遵守 MaxTTL600，记录 profile 差异，不能提升产品预算来制造同口径。
> V2 历史比较臂固定 `2873dbfd67dc52a526cd3411def9f6b10f91ed27`，使用同一新驱动
> 在各自独立集群执行；其非缓存应用构件不作为历史全量应用证据。
> 全量使用 GNU Make 4 的 `eunit-local`、既有专属库配置 `sys.closeout-local`，
> 依赖构件必须与冻结摘要一致；复制环境中的旧绝对路径构建索引不得混入结果。

## dot 接续状态（2026-10-14）

- 计划锚未变：closeout SHA256=`806114da93a904108dfa9a2747d1ee59d4efcb1a2562a92ae7ea6ae905882f0f`，W0 原计划 SHA256=`a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713`。当前分支 `main`；C14 依赖输入绑定修复为 `2494757b`，矩阵文档对齐为 `ce59d570`，依赖文件链接内容摘要回归为 `d35f9975`。干净交接 worktree `/private/tmp/imboy-unified-main-merge-20261013` 已快进到包含这些变更的 `main`。ZCODE `completion-20261012` 的 HEAD 仍为 `85e559ff` 且有大量活动 WIP；上轮尝试 no-commit merge 期间观测到该工作树发生 reset/stash 状态变化，当前未提交差异还覆盖多个 `main` 改动路径，不能认定已建立合并关系。不要清理或重置该目录；待 ZCODE 停止后先留存并逐项核对 WIP，再建立真实 merge ancestry。attachment-lifecycle、产品化策略、micro-community checksum 三项未跟踪 WIP 属其他任务，不并入本收口候选；原主机隔离副本及 `/tmp` driver 不保证 Dot 可见。
- 上一轮 `b025b340…` 源码输入摘要及更早候选绑定已失效；当前**未冻结候选**，提交后代码、依赖、配置和驱动需按 C14 清单重算绑定。C14 现拒绝 raw 非零 `exit`/`exit_code`、无效 UTC、缺失/篡改证据、混用候选和 ignored build source；依赖候选输入覆盖 `deps/` 全文件并拒绝外逃/循环 symlink。最新 `--self-test` 为 35/35（1 正向 + 34 负向），`--ids --sha256` 为 2/2（96 IDs 与两份计划摘要）；候选尚未冻结。性能驱动修正仍使用 `invalidate/1` 建立 memo 冷路径；`memo_cold_path=invalidate_then_memo_including_invalidation` 吞吐/延迟包含失效与回填，不代表纯 memo miss。旧 baseline tiny r4 的 72 个 loader oracle 错误保留，不覆盖或豁免；统一修正驱动尚未跑正式 benchmark。
- 最新驱动矩阵经只读核对为 C09 **38**（35 primary + C9m + C9n/C9o）和 C10 **16**（14 primary + 隔离尾部 M4/M5）；C10 历史 13/13、C09 历史 22/22 均不覆盖当前矩阵。`c10-mixed-plan.md` 已对齐场景数，C09 入口注释不再写旧的 22 场景。规则静态检查 `promtool check rules` 63 条通过，`promtool test rules` 通过；当前候选仍未冻结，38/16 场景两轮验证、C11 Dmax 故障链、最终全量及性能窗口均未完成。
- 资源采样（2026-10-13 23:45 +08）：观察到另一会话正在运行 `msg_c2c_logic_tests`/PG relay，端口 15432 与 PG 4323 均占用；未启动新的 EUnit、C09/C10 或 PG 验证。接续时须重新采样活跃进程、端口和租约，再取得串行测试窗口。
- 2026-10-13 接续复验：当前提交涉及的四个 Erlang 文件重编译通过；`imboy_cache_snapshot_coverage_tests` **20/20 PASS**。C14 `--ids --sha256` 为 **2/2 PASS**（96/96 required IDs、两份计划摘要），`--self-test` 为 **25/25 PASS**。上述均非最终候选签发证据。C09 有四个场景曾各 1/1 诊断通过，但输出无候选绑定，且随后新增严格 member-loss fence oracle；这些诊断结果全部作废，不可继承。当前另一个会话在运行 `ai_agent_reply` EUnit/PG relay，另有两个长驻 BEAM；遵守重型测试互斥租约，本次未启动 C09/C10/全量或性能门禁。
- 最新 C06 修正处理 `moment_logic:admin_post_detail/1` 的 ACL 查询错误；新增回归用含诊断字样的 binary reason，当前 ACL 分支统一返回安全通用提示，不透传 repo 文本。源码及回归文件已用 `erlc +debug_info -I include -I src/lib -I test/common` 编译通过，隔离全应用 `gmake compile SKIP_DEPS=1` exit=0。此前在旧 `0d15ab4b…` 副本尝试定向命令 `IMBOYENV=local gmake eunit-local t=moment_logic_tests:admin_post_detail_propagates_acl_read_error_test_ EUNIT_CONFIG=config/sys.closeout-local`，exit=2、测试尚未执行：隔离副本依赖构建引用不存在的旧绝对路径 `/private/tmp/imboy_base2/deps/cowlib/include/cow_inline.hrl`。当前 `e4c3c3df…` 候选已通过隔离直接行为断言（`erl -noshell -pa ebin -pa deps/meck/ebin -eval ...`）：mock ACL 返回含 `database password=secret` 的 binary 后，实际调用返回通用 `查询失败`，exit=0；这不等价于 EUnit fixture。Dot 仍需重建/修复隔离依赖后运行定向 EUnit。共享 `normalize_error/2` 在其他旧错误路径仍可能原样返回 binary/charlist；本次只修复新增 ACL 错误边界，不代表全模块脱敏审计通过。
- C11 有定向 metrics handler 2 PASS、旧 core actor 14 PASS；二者早于最新 Dmax timeout 改动。本轮为 `deadline_timeout` 新增 Prometheus 告警、规则正反例、Grafana 面板及 C14 运行配置候选绑定；规则检查、规则测试、dashboard JSON、C14 verifier 自测 25/25 均通过。隔离本机 HTTP `/metrics`→Prometheus scrape→告警消费测试 exit=0：注入生产指标 API 后验证 timeout 计数查询、Grafana 表达式有数据、`ImBoyCacheSyncRecoveryTimedOut` firing。该次注入不是实际 Dmax 恢复故障；`expired_recovery_deadline_emits_real_timeout_signal_test_` 当前树定向 EUnit 已通过（exit=0），最终冻结候选 consumer 重跑待执行。原先两次未覆盖 60s 窗口的 exit=1 尝试仍保留为历史失败。临时 runner `/tmp/cache-c11-owned/run-timeout.py` 不随仓库传输，Dot 须在自己的隔离目录重建消费者验证。
- 可继承的其他证据只用于局部历史参考：C14 verifier 自测现为 25/25（1 正向 + 24 负向，含 cancelled 计数非零、raw result 非零 exit 拒绝及监控运行配置候选绑定）；candidate tiny bench 60 行、0 错误，但每 worker 仅 2 次操作。正式性能 baseline tiny r4 在 loader 计数 oracle 下 exit=2（60 行中 72 个 memo_cold 错误）；失效机制检查显示这些错误与 baseline `flush/1` 未推进 fenced generation 相符，属于旧驱动冷重置不成立，不能据此判为并发噪声。修正后的统一 `invalidate/1` 驱动未运行；不得降 oracle 或把 tiny 结果当正式性能 PASS。非本候选全量 EUnit 曾报 11180 Passed、0 Failed、0 Skipped 后仍有测试取消，`make` exit=2，配置为 `sys.uc-run`；不可继承。
- C04 当前提交定向验证：`imboy_cache_snapshot_coverage_tests` 20/20 PASS（exit=0；当前测试和恢复模块隔离编译后直接 EUnit 运行）。历史 `imboy_cache_manifest_merge_tests` 18/18、旧 snapshot 17/17 仅作阶段证据。此处均非冻结候选原始收据，不能替代 AC-19 至少两轮三节点逐键内容/删除/READY 屏障验收；最终候选冻结后须重跑并保存完整原始输出及候选绑定。
- 历史终轮证据：candidate `f1aa1ce0…` 上 C09 `three_node_c09_b2/b3` 各 22/22 PASS，C10 `mixed_c10_b3/b4` 各 13/13 PASS；对应旧 V2 性能 `v2_cand_4/5/6` 有数据但因缺同环境基线判 PARTIAL。它们不能证明当前候选：之后主线提交 `8ee8f899`、`bc39858b`、`f93a842a` 等改动了同步恢复/缓存实现；当前 C14 输入集也已扩展到监控运行文件。旧轮保留作历史，不得改写候选摘要或伪造重绑定。
- 全量 EUnit 历史仍未通过：最新 `eunit_full_postrefactor.log` 记录 10,555 passed / 207 failed / 0 skipped，且有测试取消，`make` exit=2；前一轮 `eunit_full_candidate_r5.log` 为 10,762 passed / 6 failed / 0 skipped、取消，`make` exit=2。都不是当前冻结候选的全量 PASS。
- 尚未闭合：C04/AC-19 最终三节点内容覆盖与 READY 屏障；当前候选 C09 两轮全矩阵、C10 两轮混合/回滚与 QR；C11 实际 Dmax 故障及最终候选 HTTP/Scrape/告警重验；最终候选全量 EUnit；正式本地性能三组交错对照；独立审查和 C14 签发。C04 静态审查未发现 READY 误放行，但不能代替三节点实测。全节点丢失 authority 后无可信恢复来源，系统会保守停在 RECOVERING/DEGRADED，应保留为结构性可用性缺口。生产 SLO/硬件/峰值输入继续独立 BLOCKED，生产验收不签发。

Dot 上限 4 个活动 agent（含协调者），建议这样分工，先并行只读审查/准备，按租约串行跑重门禁：

1. A0 协调者：接收 95 条 WIP、复核两份计划 SHA 与当前候选；处理 C06 脱敏边界及定向测试，整合后冻结唯一候选；串行执行最终 EUnit、正式性能窗口、独立审查/C14，维护唯一权威台账。
2. A1：只负责 C04/AC-19 与 C09 驱动、三节点矩阵和证据；不得改动共享 sync 核心，发现实现缺口交 A0 指派唯一写者。
3. A2：只负责 C10 混合/回滚与 QR 交叉验证；先审计回滚 oracle，再按计划跑两轮；不得与 A1 同时持有故障/集群租约。
4. A3：只负责 C11 最终 consumer/告警规则及 C14 独立 verifier 负向审查；补上 `deadline_timeout` 注入、HTTP 和 Prometheus 查询，证明观察窗口覆盖规则周期。重故障、混合、全量和性能期间不抢共享资源。

执行顺序：传输并校验完整 WIP → 修完 C06 调用方/脱敏与必要回归 → 定向测试 → 冻结源码/配置/依赖/driver 并重算 SHA → 串行 C09、C10、QR、最终 EUnit、正式性能 → 独立审查与 C14。任一源码或运行配置改动使后续候选证据失效，按计划重跑受影响门禁；最终全量必须绑定最后候选。性能 baseline 若仍不满足同一 loader oracle，保留精确失败/例外，不能签发 PASS。

> 本节是 Dot 的接续清单，不表示任务已完成；C12 仍 `NOT_EXECUTED`。最终并列结论仍按总计划签发：后端本地收口 PASS 或精确未闭合集合、原计划 LOCAL_CANDIDATE 独立判定（生产输入缺失继续 BLOCKED）、PRODUCTION_ACCEPTANCE=`NOT_ISSUED`。

> 任务：CACHE_V221_BACKEND_CLOSEOUT_V1 / C12（最终候选测试及本地性能）
> 本文性质：只读盘点产出的执行计划（PLAN_READY / NOT_EXECUTED）。执行由协调者
> （A0）按第 6 节顺序串行完成，本文不含任何已执行的测试结果。
> 盘点时间基准：2026-10-12 07:11 Asia/Shanghai；HEAD `675b07444955ea87626a3173ea74feb9d39f0054`
> 上游文档：`backend-closeout-plan-v1.md` §6 C12 / §4 租约 / §7 证据规则；
> 原计划 `w0/plan-v2.2.1.md` 19.1 / 19.7 / 12.4 / W4；C01 冻结
> `closeout-v1/protocol-budget-frozen.md`；C00 `closeout-v1/baseline.json`、
> `resources.json`、`ledger-v1.json`。

---

## 1. 盘点快照（执行前事实，机器核实）

| 项 | 现状 |
|---|---|
| HEAD | `675b0744`（G-QR C08 已提交；C02～C05 提交 `7db4e0af`；G-01 提交 `e001fb8e`） |
| 工作树 | **未冻结**：`scripts/cache_v21_dcheck.erl`、`scripts/cache_v21_mixed_dcheck.erl`、`src/lib/imboy_cache_sync.erl`、`src/lib/imboy_cache_sync_proto.erl`、`src/lib/imboy_cache_sync_snapshot.erl`、`test/lib/imboy_cache_sync_tests.erl`、`closeout-v1/protocol-budget-frozen.md` 均为已修改状态（C09/C10 修复在途） |
| C09 | 进行中：`raw/three_node_c09_r{1,2,3,4,6,7}.json`（r5 缺号）；r7 仍有失败场景（F5/C9a/C9d/C9e/C9f，见 JSON detail）；`logs/dcheck_r1..r10.log` |
| C10 | 未开始：raw/ 无任何 mixed 系列证据 |
| C11 | `closeout-v1/observability-check.md` 已交付核查（A 类 3 项 + B 类 3 项缺口记录在案） |
| C13 | `closeout-v1/external-inputs-c13.md` 已交付只读取证（AC-26/27 维持 BLOCKED_EXTERNAL） |
| 机器态 | uptime 8 天（**未重启**），load 5.53/5.02/4.35（12 核 12 调度器）；W5-07 已证同字节构件 get w1 吞吐较 W0 基线 -74%~-79% 的机器态退化**尚未消除**（见 `w1-w5-report-20261011.md` W5-07 节） |
| 行数红线 | `src/lib/imboy_cache_sync.erl` 现 1203 行 > 800 行规范（拆分后仍超，冻结审查须先处理：继续最小拆分或登记明确例外，禁止借机重写） |
| 磁盘 | 33Gi 可用（`closeout-v1/baseline.json` environment） |

---

## 2. W0 cache profile 与压测/测试入口盘点

### 2.1 唯一对应 W0 cache profile 的入口

**`scripts/run_cache_w0.py`**（19.1 指定执行入口）。它编译 `scripts/cache_w0_bench.erl`
到临时目录，起独立 BEAM（随机节点名 + 本次随机 cookie，loopback，不启动 IMBoy/PG/Garage，
不读业务配置），1+2 个 peer 组成三真实节点，输出整套证据目录。

实际命令行（19.1 原文 + W0 基线 r7/r8 实参）：

```bash
cd /Users/leeyi/project/imboy.pub/imboy
python3 scripts/run_cache_w0.py --operations 10000 --output <本次专用空目录>
```

冻结 profile 口径（19.1 + `w1-w5-addendum-20261011.md` §5，**不可在本轮调整**）：

- 并发 1/8/32；keyspace 1024；value 256 bytes；TTL=3600s；每 worker **10000** ops；
- 负载种类：GET、SET、90/10 mixed、memo 热命中、get_wait、syn（逐条等全部接收者
  depcache:set 完成的 ACK，测应用完成延迟而非 publish 返回）；
- depcache `memory_max=64MB`（bench 内 hardcode）；
- 同字节微基准仅测 `deps/{depcache,syn,jsone}` 原语 —— C12 原文定位：**只作环境控制，
  不能替代新 V2 代码性能验证**。
- 可比性警告：W0 基线 r7/r8（`.Codex/runs/cache-v2.2.1-w0/r7-20261010-140457`、
  `r8-20261010-140511`）实参 `--operations 10000`；历史 w507 对照轮误用默认 1000，
  属可比性偏差，C12 一律用 10000。
- 退出码约定（19.1）：runner 进程**恒退 2（W0_BLOCKED 设计行为）**，不等于压测失败；
  判定看 `baseline.json` 的 `compile_exit`/`run_exit` 与 `performance.json` 是否产出。

### 2.2 正确性/故障入口（C12 复跑用）

| 入口 | 用法 | 退出码 |
|---|---|---|
| `scripts/cache_v21_dcheck.erl`（三节点 dcheck，C09/C12） | `erlc -I src/lib -o ebin_drv scripts/cache_v21_dcheck.erl` 后 `erl -noshell -pa ebin_drv -pa ebin -pa deps/depcache/ebin -pa deps/syn/ebin -eval 'cache_v21_dcheck:main(["<out.json>"]), halt(0).'`（本会话惯例：驱动编译到 `~/.codex/cache-closeout-runs/zcode-20261011T130725Z/ebin_drv/`） | 0 全过 / 2 有失败 / 1 参数错误 |
| `scripts/cache_v21_mixed_dcheck.erl`（混合版本，C10/C12） | `LEGACY_EBIN=<旧树 ebin> NEW_EBIN=<新树 ebin> erl -noshell -pa <driver_dir> -eval 'cache_v21_mixed_dcheck:main(["<out.json>"]).'`；旧树 = C10 冻结的 INTEGRATION_BASE（V2 引入前 main）独立构建 | 0 全过 / 2 有失败 / 1 参数错误 |
| `IMBOYENV=local make eunit-local t=<suite>`（定向） | Makefile:557 起；PG relay 动态高端口 + 私有 HTTP 端口，勿与其他会话并发 | make exit 码 |
| `IMBOYENV=local make eunit-local`（全量） | 同上；前置本地 imboy_v1 schema 已应用最新迁移 | make exit 码 |

C12 定向套件集（自本会话 logs/ 实际使用归纳）：
`imboy_cache_tests`、`imboy_cache_policy_tests`、`imboy_cache_v2_tests`、
`imboy_cache_sync_tests`、`account_ds_tests`（G-01）、
`qr_login_session_repo_tests` / `qr_login_handler_tests` / `qr_login_sse_handler_tests` /
`qr_login_event_ds_tests`（C08 G-QR）。

### 2.3 明确不是 cache profile 入口的脚本

- `scripts/bench_websocket.sh` —— WebSocket 并发连接基准，需**已启动的后端** + TOKEN，
  属业务长连接面，与 W0 cache profile 无关，C12 不使用。
- `scripts/smoke_extended.sh`、`scripts/smoke/`、`run_feature_flag_smoke.sh` —— 业务冒烟，
  需后端运行，C12 不使用。
- `scripts/cache_w0_timeout_probe.erl` —— W0 时期的 depcache 超时探针，非 profile 主入口。

### 2.4 缺口：V2 路径 bench 驱动（必须先补，属波次 1 A7 职责）

现状：仓内**不存在**覆盖 V2 新 API/协议路径（`imboy_cache:put/4` V2 信封、同步栅栏、
fenced generation、epoch/hash 校验）的性能驱动；`cache_w0_bench.erl` 只打 depcache/syn
原语。C12 候选臂无法直接开跑。A7 须在候选冻结**前**交付驱动并自测（不占用正式窗口），
驱动合同见第 6.5 节；未交付前 C12 性能窗口只能以环境控制臂先行，候选臂保持
`WAITING_RESOURCE`，禁止拿微基准充候选臂证据。

---

## 3. 证据 JSON 结构约定（自既有 raw/ 与 C14 verifier 机械提取）

C14 verifier `scripts/check_cache_closeout_v1.py`（未跟踪新文件，A8 交付）对
`~/.codex/cache-closeout-runs/zcode-20261011T130725Z/raw/` 下**扁平 `*.json`** 的硬性要求：

1. **命名/系列**：`<series>_r<N>.json`；系列名 = 去掉 `_r\d+` 后缀（`series_of()`）。
   既有系列：`three_node_c09`。**同一系列内禁止混候选**（不同 CANDIDATE_SHA 不得进同一
   系列编号）。
2. **结构（check_exits）**：顶层对象，`scenarios` 为非空列表，每项必含
   `name`（非空字符串）/`status`（必须逐项 == `"pass"`）/`detail`（字符串）；
   `elapsed_ms` 为非负数值。空 scenarios 直接拒绝（反伪 PASS）。
3. **候选绑定（check_candidate）**：顶层须携带
   `candidate_sha256` / `candidate` / `candidate_digest` / `working_tree_digest`
   任一键，值为 64 位十六进制 sha256；全部证据的候选须与台账最终候选一致。
   ⚠️ 既有 `three_node_c09_r*.json` **均未携带候选绑定**——C09 终轮证据必须补齐该字段，
   否则 C14 必挂（执行 C12 时同样适用）。
4. **UTC（check_utc）**：顶层 `utc` 或任意 `*_utc` 字段必须为带时区偏移的 ISO8601，
   晚于 1971-01-01 且不超未来 24h（拒绝 1970 哨兵时间戳——即纠正历史混合驱动 1970 问题）。
5. dcheck/mixed 驱动现状输出（`three_node_c09_r*.json` 实样）：顶层
   `driver_node` / `elapsed_ms` / `utc` / `scenarios[{name,status,detail}]`，
   stdout 汇总行 `== cache_v21_dcheck: N passed, M failed, X ms ==` 落 `logs/dcheck_r*.log`。
   C12 复跑沿用该 schema，**追加** `candidate_sha256` 顶层字段。

性能证据两层结构（沿用 W1-W5 会话 w507 先例 + verifier 兼容）：

- **原始层**（子目录，不被 verifier 扫描）：`raw/perf_c12_<arm>_r<N>/` =
  `run_cache_w0.py --output` 整目录产物（`performance.json`、`baseline.json`、
  `compile.log`、`raw.log`、`sha256.json` + census 文件）。`baseline.json` 内嵌
  `head`/`working_tree_status`/`load`/`dependency_hashes`/`script_hashes`/
  `compile_exit`/`run_exit` —— 基线与候选的区分即由此 + 臂标签 + 交错序实现，
  历史先例：`raw/w507_main_r{1,2}/`、`w507_quiet_r{1,2}/`、`w507_probe_load365/`。
- **汇总层**（扁平，verifier 兼容）：`raw/perf_c12_r<N>.json`，顶层含
  `candidate_sha256`、`utc`、`elapsed_ms`、`scenarios[]`（每臂每指标组一个
  scenario：profile 自检、吞吐组、P99 组、资源包络组…，status 由第 6.5 节判据产生）、
  `arm`、`round`、`load_before/after`、`operations=10000`，并内嵌指向原始层目录的
  相对路径与 `sha256.json` 摘要。

全量 EUnit 与 QR 的原始日志落 `logs/`（本会话惯例 `<kind>_r<N>.log`，如
`eunit_full_c12_r1.log`、`qr_c12_r1.log`），make 的 exit 码原样记录，禁止只抄结论行。

---

## 4. 候选冻结步骤与 CANDIDATE_SHA 记录格式

### 4.1 冻结前置（全部满足才可进入第 6 节窗口）

1. C09 全部 required 场景**至少两轮独立通过**且修复已提交（当前 r7 尚有 F5/C9a/
   C9d/C9e/C9f 失败 + 工作树未提交，均须闭合）；
2. C10 混合/回滚矩阵两轮完整零失败，LEGACY_EBIN 旧版本 SHA/构建树冻结；
3. C11 B 类代码项（status/epoch/queue gauge 导出等）落定或明确登记为非阻断缺口；
4. `imboy_cache_sync.erl` 1203 行超限问题处理完毕（最小拆分或登记例外）；
5. A9 代码审查无阻断项（波次 3 门）；
6. 外来 WIP（2 个未跟踪 docs 路径）保持不接管、不 stage。

### 4.2 冻结流程（机械步骤）

```bash
# 1. 停写声明 + 记录 HEAD
git -C /Users/leeyi/project/imboy.pub/imboy rev-parse HEAD        # → freeze.head
git -C ... status --porcelain=v1                                   # → 仅剩已知外来 WIP
# 2. 干净重建
make compile && echo compile_exit=0
# 3. 计算 frozen_set（下表）逐文件 sha256 → 规范序列化 → candidate_sha256
# 4. 写 ~/.codex/cache-closeout-runs/zcode-20261011T130725Z/raw/candidate_freeze.json
#    + 更新 closeout-v1/ledger-v1.json 顶层 candidate 字段
```

`frozen_set`（候选字节面；执行时逐项展开为精确路径清单）：

| 组 | 路径模式 |
|---|---|
| 缓存核心 | `src/lib/imboy_cache.erl`、`src/lib/imboy_cache_sync*.erl`、`src/lib/imboy_cache_policy.erl` |
| 缓存测试 | `test/lib/imboy_cache*_tests.erl`、`test/lib/imboy_cache_sync_tests.erl` |
| 驱动/runner | `scripts/cache_v21_dcheck.erl`、`scripts/cache_v21_mixed_dcheck.erl`、`scripts/run_cache_w0.py`、`scripts/cache_w0_bench.erl`、A7 交付的 V2 bench 驱动 |
| 配置 | `config/` 中缓存同步相关段（执行时点名为准） |
| QR（G-QR 面已随 C08 提交，一并纳入指纹以绑定全量） | `src/ds/qr_login*`、`src/logic/qr_login*`、`src/api/qr_login*`、`test/lib/qr_login*_tests.erl`（执行时按 C00 ownership 展开精确清单） |

### 4.3 CANDIDATE_SHA 记录格式（`raw/candidate_freeze.json`）

```json
{
  "candidate_sha256": "<64hex：frozen_set 全部 {path,sha256} 按路径排序后的规范 JSON 序列化的 SHA-256>",
  "working_tree_digest": "<64hex：sha256(HEAD + '\\n' + git status --porcelain=v1 全文)>",
  "frozen_at_utc": "<带时区 ISO8601>",
  "head": "<40hex git commit>",
  "head_subject": "<git log -1 --format=%s>",
  "compile_exit": 0,
  "budget_digest": "budget_v1: Nmax=50000 Nmax_bytes=16MiB Tmax=20000 Tmax_bytes=8MiB Bmax=2048 Bmax_bytes=32MiB Pmax=256 Pmax_bytes=4MiB Rmax=2 Rmax_rounds=3 Dmax=300s MaxValue=32768B MaxTTL=600s HighWater=10000 W=720s",
  "protocol_budget_frozen_sha256": "<closeout-v1/protocol-budget-frozen.md 的 sha256>",
  "frozen_set": [{"path": "src/lib/imboy_cache.erl", "sha256": "<64hex>"}, "..."],
  "perf_thresholds_frozen": {
    "source": "plan-v2.2.1.md 12.4 候选值，测试前冻结，不得事后调整",
    "local_get_p99_us_max": 2000,
    "local_set_p99_us_max": 5000,
    "three_node_set_converge_p99_ms_max": 1000,
    "relative_get_p99_regression_max_pct": 10,
    "sustained_throughput_vs_baseline_min_pct": 95
  }
}
```

同一 `candidate_sha256` 必须随后写入 C12 每份证据（`raw/*.json` 顶层
`candidate_sha256` 字段）与台账。**重冻规则**（C12 原文）：候选代码/依赖/配置/driver
任何变更 → 重新计算 CANDIDATE_SHA → 受影响门禁重跑；全量 EUnit 必须绑定**最后**代码
候选。

---

## 5. 互斥窗口总顺序（A0 串行租约）

resources.json 已冻结互斥项：`full_eunit` / `heavy_fault_stress`（C09 轮）/
`official_perf_window` 均为串行租约。C12 窗口内部一行顺序：

**全量 EUnit → 三节点 dcheck（≥2 轮）→ mixed dcheck → QR 定向 → 本地性能
（环境锚 1 轮 + 基线/候选交错 ≥3 组）**

排序理由：全量最先暴露最广失败面；故障/混合次之（负载中等）；性能最后，要求独占
静默机器。窗口期间不安排其他 10 路高负载并发（收口计划 §4）。

---

## 6. 窗口分步执行卡（命令 / PASS / PARTIAL / 证据）

### 6.0 窗口前置检查（每步之前重复）

- `uptime`、`sysctl -n hw.ncpu`、`top -l 1` 抽查无并行会话全量测试/容器满载；
- PG（127.0.0.1:4323）`pg_isready`；磁盘可用 ≥ 10Gi；
- 端口纪律：eunit-local 独占 relay 15432→4323 与 HTTP 19980/19970（并发时按本会话
  惯例随机高端口）；dcheck/mixed/perf 全部随机节点名 + 随机 cookie，禁用现役 cookie。

### 6.1 全量 EUnit（步骤 ①）

```bash
cd /Users/leeyi/project/imboy.pub/imboy
IMBOYENV=local make compile 2>&1 | tee ~/.codex/cache-closeout-runs/zcode-20261011T130725Z/logs/c12_step1_compile_r1.log
IMBOYENV=local make eunit-local 2>&1 | tee ~/.codex/cache-closeout-runs/zcode-20261011T130725Z/logs/eunit_full_c12_r1.log
echo "exit=$?"
```

- **PASS**：`make compile` exit=0；全量 `make eunit-local` exit=0，日志末尾
  `Failed: 0`，`Passed` 计数与套件汇总一致，非预期取消/跳过 = 0（eunit cancel 归零），
  原始 exit 保存。失败即 **FAIL**：隔离复跑不能替代整轮通过；无关失败不擅自修复，
  保留外部归属与阻塞证据（C12 原文）。
- **PARTIAL**：不适用——全量只有 PASS/FAIL（历史教训：Failed 4/Passed 11078/
  exit 非零只能记全量 FAIL + 定向复跑 PASS，不得写成全量 PASS）。
- 证据：`logs/eunit_full_c12_r1.log` + 扁平 `raw/eunit_full_c12_r1.json`
  （顶层 candidate_sha256/utc/elapsed_ms/scenarios[]：compile、full_run、count_consistency
  三个 scenario）。

### 6.2 三节点 dcheck（步骤 ②，依赖 C09 完成）

```bash
mkdir -p <RUN_ROOT>/ebin_drv && erlc -I src/lib -o <RUN_ROOT>/ebin_drv scripts/cache_v21_dcheck.erl
erl -noshell -pa <RUN_ROOT>/ebin_drv -pa ebin -pa deps/depcache/ebin -pa deps/syn/ebin \
  -eval 'cache_v21_dcheck:main(["<RUN_ROOT>/raw/three_node_c12_r1.json"]), halt(0).'
```

- **PASS**：驱动退出码 0；JSON `scenarios[].status` 全 `"pass"`；**≥2 轮独立通过**
  （C09 口径带入）；JSON 顶层含 `candidate_sha256`/`utc`/`elapsed_ms`。
- **FAIL**：任一场景 fail 或退出码 2；按 C12 只对受影响场景重跑 + 相邻回归，
  不得静默改 oracle。
- 证据：`raw/three_node_c12_r{1,2}.json` + `logs/dcheck_c12_r{1,2}.log`。
  （新系列 `three_node_c12`，不与 C09 系列混编——系列内候选一致原则。）

### 6.3 mixed dcheck（步骤 ③，依赖 C10 完成）

```bash
LEGACY_EBIN=<C10 冻结旧树 ebin> NEW_EBIN=<候选树 ebin> \
erl -noshell -pa <RUN_ROOT>/ebin_drv \
  -eval 'cache_v21_mixed_dcheck:main(["<RUN_ROOT>/raw/mixed_c12_r1.json"]).'
```

- **PASS**：退出码 0；M1～M6 全场景 pass + candidate 绑定；旧版本 SHA 与 C10 冻结
  记录一致（禁"新代码模拟旧版本"）。
- **FAIL**：任一场景 fail / 退出码 2 / 旧树 SHA 不一致。
- 证据：`raw/mixed_c12_r{1,2}.json`（两轮，C10 矩阵口径）+ `logs/mixed_c12_r{1,2}.log`。

### 6.4 QR 定向（步骤 ④，依赖 C08 已提交；不依赖 C09/C10 的套件内容）

```bash
for t in qr_login_session_repo_tests qr_login_handler_tests qr_login_sse_handler_tests qr_login_event_ds_tests; do
  IMBOYENV=local make eunit-local t=$t 2>&1 | tee <RUN_ROOT>/logs/qr_c12_${t}_r1.log
done
```

- **PASS**：每套件 make exit=0 且 `Failed: 0`；claim/consume 原子性、SSE/status 竞争
  断言全部通过（C08 oracle 既有断言，不新造）。
- **FAIL**：任一失败；属产品回归走修复预算（每 root cause ≤ 2 轮自动修复）。
- 证据：`logs/qr_c12_*_r1.log` + 扁平 `raw/qr_c12_r1.json`（四套件各一个 scenario）。

### 6.5 本地性能窗口（步骤 ⑤）

**静默判据（每轮开跑前记录，任一不满足→该轮标记 noisy，不作为 gate 数据）**：
1min load ≤ 6（12 调度器之半，w507_quiet 同量级）、15min load ≤ 8、无并行会话满载。

**三臂定义**（全部同 W0 冻结 profile：1024 Key/256B/TTL=3600/并发 1/8/32/10000 ops）：

| 臂 | 内容 | 输出 |
|---|---|---|
| env（环境锚） | `python3 scripts/run_cache_w0.py --operations 10000 --output raw/perf_c12_env_r1/` 同字节微基准，对照 W0 r7/r8 判机器态 | `raw/perf_c12_env_r1/` |
| base（基线臂） | **同一候选字节** + V2 复制关闭（默认生产配置：`cache_sync_protocol=legacy`、fenced_namespaces=[]） | `raw/perf_c12_base_r{i}/` |
| cand（候选臂） | **同一候选字节** + 本地显式覆盖配置启用 V2（fixture namespace），走新 API/协议；覆盖 S/M/L Key/Value 分布、混合业务负载、资源包络 | `raw/perf_c12_cand_r{i}/` |

**交错序列**（≥3 组，机器漂移在交错中抵消）：

```text
env_r1 → base_r1 → cand_r1 → base_r2 → cand_r2 → base_r3 → cand_r3
（env 锚 1 轮 + 3 组交错 = 7 轮；每轮全新空输出目录，轮间记录 load before/after）
```

base/cand 臂由 A7 交付的 V2 bench 驱动执行（第 2.4 节缺口）；驱动合同：
同 profile 参数、输出 verifier 兼容 JSON + 全量指标（吞吐、P50/P95/P99、错误率、
三节点同步收敛时间、mailbox/heap/metadata 峰值）、成功退出码 0。
A7 驱动未交付期间，env 臂可先行，base/cand 记 `WAITING_RESOURCE`。

**PASS 判据（全部满足）**：

1. 每轮 `baseline.json` `compile_exit=0` 且 `run_exit=0`（runner 进程退 2 为设计行为，
   不算失败）；静默判据每轮满足；
2. bench 内建 self_checks（单飞 memo、失效竞态、loader 异常、崩溃恢复、get_wait 移交等）
   与 W0 基线**逐项语义值一致**（正确性零回归）；
3. 冻结本地相对阈值（12.4 候选值，测试前冻结不得调整）：本地 GET P99 ≤ 2ms、
   本地 SET P99 ≤ 5ms、同机房三节点 SET 收敛 P99 ≤ 1s、相对 GET P99 退化 ≤ 10%、
   同等负载持续吞吐 ≥ 冻结基线 95%；
4. 资源包络不超 C01 冻结预算（budget_digest 三方一致），报告吞吐、P99、错误率、
   同步收敛时间、mailbox/heap/metadata 峰值全项；
5. ≥3 组交错中候选臂无系统性劣于基线臂超出阈值的情形。

**PARTIAL 判定（引用 19.7 原文与 W5-07 既定纪律）**：

> 19.7：「W0 结束时只有业务峰值/目标硬件已知，才冻结绝对吞吐、P99、错误率和
> headroom。本机微基准不反向编造业务 SLO；未知项明确 W0_BLOCKED。」
> 「当前保留 A；若缺少最终 SLO，仅表示"B 不获准"，不能伪称 A 已满足生产负载。」

触发条件与处置：

- **机器态异常**（当前最大风险：uptime 8 天未重启，W5-07 已证同字节退化 -74%~-79%）：
  env 臂与 W0 r7/r8 同字节对照（30 项构件 SHA256 逐项 MATCH 方法）出现超出噪声带
  （±10%）的吞吐/P99 偏离，或静默判据无法满足 → 该窗口性能记 **PARTIAL**，
  归因机器态（哈希清单铁证），**不调阈值、不用满载数据充 gate**；需要用户主机重启
  才能继续时记 `WAITING_USER_ACTION`（C12 原文：先验证环境适用性，不预设"重启必能
  修复"）。机器重启后（若用户授权）重开窗口从 env 锚重新起算。
- **候选臂真实退化**（排除机器态后仍超阈值）：按 FAIL 处理进入修复预算，
  修复后重冻候选并重跑受影响门禁；超预算 → `NO_GO`。
- **SLO 面**：无论本地结果如何，绝对生产 SLO 维持 W0_BLOCKED / AC-26/27
  BLOCKED_EXTERNAL（C13 已判定），本地 PASS 不推导生产资格。

**证据**：原始层 `raw/perf_c12_{env,base,cand}_r{N}/`（整目录 + sha256.json）；
汇总层 `raw/perf_c12_r{N}.json`（每轮一个，顶层 candidate_sha256/utc/elapsed_ms/
arm/round/load/scenarios[]，scenarios 含 profile_self_check、throughput、p99、
convergence、resource_envelope 五组，status 由上述判据产生）。

---

## 7. 依赖 C09/C10 完成才能执行的步骤清单

| # | 步骤 | 依赖 | 说明 |
|---|---|---|---|
| 1 | §4 候选冻结（CANDIDATE_SHA）整体 | **C09 + C10** | 当前工作树未提交修改即 C09/C10 WIP；冻结 = 停止产品代码写入，必须待两者全部 required 场景闭合并提交 |
| 2 | §6.2 三节点 dcheck 复跑 | **C09** | 场景集以 C09 终版为准（脚本仍在修改中）；C12 复跑绑候选 |
| 3 | §6.3 mixed dcheck 复跑 | **C10** | 需 C10 冻结的 LEGACY_EBIN 旧版本构建树与两轮绿基线 |
| 4 | §6.1 全量 EUnit / §6.4 QR / §6.5 性能 | 冻结（传递依赖 C09+C10） | 逻辑上依赖冻结成立；QR 套件本身依赖 C08（已提交 `675b0744`），与 C09/C10 无关 |

**不依赖 C09/C10、可与 C09/C10 并行先做的准备**：

- A7 的 V2 路径 bench 驱动编写与自测（§2.4 缺口，唯一硬前置缺口）；
- W0 r7/r8 基线指纹核对（30 项构件 SHA256 清单预生成）；
- 机器态预检脚本化（uptime/load/磁盘采样）、证据目录脚手架、本计划阈值表复核；
- C09 终轮证据补 `candidate_sha256` 绑定字段的驱动侧改造（`cache_v21_dcheck.erl`
  输出增加顶层候选字段，属 C09 收尾 owner 的改动）。

---

## 8. 红线（执行时同样适用）

- 窗口执行者只按本计划串行运行；禁止在盘点/计划阶段运行任何测试/bench、占用端口、
  `git add/commit`（本文写作即遵守）。
- 不得连接共享/生产目标；禁现役 cookie、真实凭据/数据；日志不含生产 PII。
- 性能阈值与 C01 预算**测试前冻结**，失败后不得调高获取 PASS（19.7）。
- 全量非零退出不能用隔离复跑签 PASS；loopback 不代表三机/生产容量。
- 每步原始 exit 必须保存；证据缺候选绑定/1970 时间戳/空 scenarios 一律 C14 拒绝。

## 9. 交付物

| 文件 | 位置 |
|---|---|
| 本执行计划 | `docs/architecture/cache-v2.2.1/closeout-v1/c12-window-plan.md` |
| 候选冻结记录 | `~/.codex/cache-closeout-runs/zcode-20261011T130725Z/raw/candidate_freeze.json` |
| 窗口证据（原始） | 同目录 `raw/`（`three_node_c12_r*` / `mixed_c12_r*` / `qr_c12_r1` / `eunit_full_c12_r1` / `perf_c12_*`）与 `logs/` |
| 机器摘要/台账更新（执行后由协调者写） | `closeout-v1/ledger-v1.json` 候选字段 + 后续 C12 报告 |
