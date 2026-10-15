# C14 当前候选证据合同

本合同配合 `scripts/check_cache_closeout_v1.py --all`，不修改历史证据。
默认台账为 `ledger-v2.json`；不再默认读取任何旧运行目录。
终验须显式指定 `--evidence-dir <本轮 raw 结果目录>`。`--scope backend`
和 `--scope original` 分别裁决后端本地收口与原计划，不能相互替代。
仅单独通过 `--ids` / `--sha256` 等诊断检查不代表整体 PASS。

## 候选

`--candidate-manifest` 指向 JSON，默认同目录 `candidate-manifest.json`：

- `files`：按路径排序、无重复的 `{path, sha256}` 数组。
- `candidate_sha256`：`sha256(json.dumps(files, sort_keys=True, separators=(",", ":")).encode())`。
- `frozen_at_utc`：冻结时间，带明确时区；命令启动不能早于该时间。

输入集合由当前仓库机械枚举：Git 跟踪及未忽略的 `src/`、`include/`、
`config/`、`scripts/`、`test/`、`priv/migrations/`、`asn1/`、`mibs/`、`lib/`、
Makefile、erlang.mk、VERSION、rebar.lock；被 Git 忽略但可被构建/迁移消费的源码拒绝冻结，
避免源码漏出候选摘要；
另绑定实际存在的本地测试配置 `config/sys.closeout-local.config`（仅摘要，不输出内容）；
并绑定 Prometheus/Grafana 运行链路：`deploy/docker-compose.prod.yml`、抓取配置、cache-sync
规则及规则测试、专板、Grafana dashboard 与 datasource provisioning（文件存在时）；
另绑定 `deps/` 下所有文件字节（包括无扩展名运行工具及平台构建源码/配置）。
2026-10-15 v3 按用户指定算法补入顶层 `src/test/priv/scripts/include/config/deploy/deps`
的全部物理文件，以及根 `Makefile/VERSION/erlang.mk/rebar.lock`，与原来源集合取并集，
全局排除 `*.dump`；根级 dotfile 不纳入，子目录隐藏文件、`.git/_build` 不额外排除。
依赖锁文件无法覆盖本地依赖树中的修改。`deps/` 根和依赖目录符号链接拒绝冻结，
依赖文件链接仅允许解析到仓库内，防止循环遍历或将仓外文件混入候选。
源码、配置、测试驱动、依赖或运行依赖字节改变后必须重新冻结；新加输入也拒绝。
此清单不证明用于执行的主应用 BEAM 等价，执行命令仍须从冻结源码隔离编译，
记录构建命令与环境，由独立审查核对实际加载路径。

## 台账与原始命令回执

台账顶层 `candidate_sha256` 必须等于当前候选。`entries` 保留原 96 个 ID，
每项 `status` 与非空 `receipts`；另提供 `closeout_tasks`，包含 C00–C14 各一次。
每份引用为 `{path, sha256}`，相对台账所在目录或绝对仓外路径。
所有 required 必须 PASS；backend 仅允许原 W0-09、AC-26、AC-27 为
BLOCKED_EXTERNAL，并须有 owner、missing_inputs、resume_condition。
original 不接受上述豁免，生产结论仍独立为 NOT_ISSUED。

命令回执字段：`task_id`、`candidate_sha256`、`covers`（明确覆盖的台账 ID），
`command`、`exit_code`（原始整数 0）、`oracle`、`input_sha256`（等于候选），
`started_at_utc`、`finished_at_utc`、`gate`、`count_kind`、`counts`、`log`、`result`。
counts 含整数 passed/failed/cancelled/skipped，passed > 0，后三项为 0。
log/result 均为 `{path, sha256}`，相对回执目录；不得截断或过滤原始日志。
count_kind 为 scenarios/tests/checks；tests 还核对 EUnit 原生终计数。
full_eunit 强制使用 tests；标准 OTP 终止日志及组件存活失败均拒绝。

raw result 需包含候选、counts、utc、elapsed_ms，以及非空
`scenarios[{name,status,detail}]`，所有 status 为 pass。scenarios 模式数量须匹配；
如果 raw result 提供顶层 `exit` 或 `exit_code`，该值必须是整数 `0`，不得与成功的
shell receipt 相矛盾；缺少这些可选字段时仍以原始命令 receipt 的 exit_code 为准。
完整日志须有唯一终计数 `CLOSEOUT_COUNTS ` 后跟同一 counts 的紧凑排序 JSON。
该 footer 只作统计交叉检查，不能替代真实 exit 或原生 EUnit 总结。
失败、超时、异常取消、摘要不符、缺文件、未覆盖当前 ID 均拒绝。

C12 必须覆盖最终候选的 compile、targeted、three_node、mixed、rollback、qr、
full_eunit、performance、independent_review 九类 gate。回执声明不代替具体 oracle
的语义审查；比如一场景的 PASS 不能宣称完整故障矩阵或正式性能窗口通过。

`performance` gate 必须由 7 份独立、摘要绑定的逐轮结果按序组成：
`env_r1, base_r1, cand_r1, base_r2, cand_r2, base_r3, cand_r3`。每轮均须绑定当前
candidate/profile/budget，且 raw command receipt 的执行区间严格递增、不重叠；结果内 UTC
必须落在其自身 receipt 区间。每轮 `operations=10000`、`concurrency=[1,8,32]`、`keyspace=1024`、
value_bytes=256、load 前后采样和 compile/run exit=0；W0 env 使用 TTL3600，V2 base/cand
使用 TTL600。env 的六类 workload 吞吐/P99 必须与 W0 r7、r8 同字节组件清单各自相差
不超过 10%。W0 env/r7/r8 必须各引用 runner 原生 `baseline.json`、`performance.json`、
`sha256.json` 三份 evidence-dir 内文件；校验器核实 sha256.json 对前两者的摘要，比较
baseline 中完整 30 项 dependency/script SHA 映射，并直接读取 performance 的 18 项单机与
3 项三节点原始指标。不得以自造聚合 JSON 或自报基线哈希替代 runner 输出。V2 结果必须提供五组 named scenarios；五项 self-check 全为 true，GET/SET
P99、三节点收敛和错误数由校验器重算，错误数必须为 0。`resource_envelope` 需报告
mailbox/heap/metadata 峰值及 C01 `budget_usage` 的 N/T/journal/buffer/page/generation/
grace/peer 计数与字节，校验器按冻结上限逐项比较，不能以自报 `within_budget` 替代数值检查。
三组 base/cand
按相同 profile 做 GET P99 ≤10% 回退、吞吐 ≥95% 比较；缺臂、乱序、混合 profile 或任一
失败均拒绝签发。
`profile_sha256` 是对 `{operations, concurrency, keyspace, value_bytes, ttl_seconds}`
规范 JSON（排序 key、紧凑分隔符）的 SHA-256；env TTL3600 与 V2 TTL600 分别绑定，
base/cand 之间必须相同。

## 校验器自身验证

`python3 scripts/check_cache_closeout_v1.py --self-test` 在唯一仓外临时目录执行
一个正向 fixture 及 52 个负向 fixture：缺/重复 ID、计划摘要篡改、失败场景、
混用候选、伪 PASS、required FAIL、真实 exit 非零、结果篡改、当前源码改变、
失败日志，把无关回执复用于所有 ID、covers 错误类型、缺失场景列表以及缩减 required 集合。
另含 full_eunit 伪 checks 计数、两类 OTP 崩溃日志、组件存活失败、迁移输入及被忽略的测试配置变更。
并包括新增 Prometheus/Grafana 运行配置是否变化候选摘要的负例。fixtures 是 verifier 自测，不是产品验收。
性能负例覆盖单臂、零时长命令区间/耗时、真实回执区间乱序及结果时间脱离命令区间、重复 workload 行、
非零错误率、缺失 P95、非零或布尔错误计数、超出 C01 数值上限、W0 env 漂移/原始
baseline 篡改、布尔退出码和畸形 profile digest；W0 r7/r8 结果从 runner 原生
artifacts 中独立摘要绑定，并要求操作数、worker、kind 行集准确且无重复/额外行。正例使用
七份逐轮 performance receipt，不将聚合 fixture 冒充运行原始结果。
