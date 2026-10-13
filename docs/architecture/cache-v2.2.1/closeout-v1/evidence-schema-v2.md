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
`config/`、`scripts/`、`test/`、`priv/migrations/`、Makefile、erlang.mk、VERSION、rebar.lock；
另绑定实际存在的本地测试配置 `config/sys.closeout-local.config`（仅摘要，不输出内容）；
另绑定 deps 中实际 `.erl/.hrl/.beam/.app/.so/.dylib` 字节，排除 `.git/_build`。
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
完整日志须有唯一终计数 `CLOSEOUT_COUNTS ` 后跟同一 counts 的紧凑排序 JSON。
该 footer 只作统计交叉检查，不能替代真实 exit 或原生 EUnit 总结。
失败、超时、异常取消、摘要不符、缺文件、未覆盖当前 ID 均拒绝。

C12 必须覆盖最终候选的 compile、targeted、three_node、mixed、rollback、qr、
full_eunit、performance、independent_review 九类 gate。回执声明不代替具体 oracle
的语义审查；比如一场景的 PASS 不能宣称完整故障矩阵或正式性能窗口通过。

## 校验器自身验证

`python3 scripts/check_cache_closeout_v1.py --self-test` 在唯一仓外临时目录执行
一个正向 fixture 及 21 个负向 fixture：缺/重复 ID、计划摘要篡改、失败场景、
混用候选、伪 PASS、required FAIL、真实 exit 非零、结果篡改、当前源码改变、
失败日志，把无关回执复用于所有 ID、covers 错误类型、缺失场景列表以及缩减 required 集合。
另含 full_eunit 伪 checks 计数、两类 OTP 崩溃日志、组件存活失败、迁移输入及被忽略的测试配置变更。
fixtures 是 verifier 自测，不是产品验收。
