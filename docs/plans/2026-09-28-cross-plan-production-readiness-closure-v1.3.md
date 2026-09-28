# IMBoy 跨计划生产就绪收口执行计划 V1.3

> 日期：2026-09-28
>
> 计划性质：可分步实施、可并行、可机械验收的生产就绪执行合同
>
> 计划仓库：`/Users/leeyi/project/imboy.pub/imboy`
>
> 关联仓库：`imboy`、`imboyapp`、`imboyadmin`
>
> 历史输入：V1.2 正式结果 `PASS 51 / FAIL 8 / BLOCKED 5 / PENDING 0`
>
> 初始结论：`PRODUCTION_READY=NO_GO`
>
> 执行模型：A0 唯一协调/集成/生产操作者；最大并发 8（含 A0），最多 7 个 Worker
> 重要边界：本计划获准执行不等于授权 push、发布、部署、生产写、密钥轮换、证书或入口切换、旧 TURN 停止/删除、第三方通知。

---

## 0. 目标、完成定义与终态

### 0.1 唯一目标

把 V1.2 已合入的功能候选和剩余 8 个 FAIL、5 个 BLOCKED 收敛为一个可复现、可回滚、
可观测、经真实设备和受控生产灰度验证的发布候选。只有以下五层全部 PASS，才允许写：

```text
T1_LOCAL_RELEASE_CANDIDATE=PASS
T2_DEVICE_QUALIFICATION=PASS
T3_PRODUCTION_PREFLIGHT=PASS
T4_PRODUCTION_CANARY=PASS
T5_PRODUCTION_READY=PASS
PRODUCTION_READY=PASS
```

任一层非 PASS 时，最终结论必须是 `PRODUCTION_READY=NO_GO`。代码存在、测试文件存在、
HTTP 200、容器 healthy、截图、mock E2E、跳过测试、旧报告或历史 PASS 都不能单独构成 PASS。

### 0.2 V1.2 遗留项到 V1.3 的映射

| V1.2 项 | 旧终态 | V1.3 解除路径 | 新硬门 |
|---|---|---|---|
| `CP-TD-A02`、`CP-FINAL-A01/A02` | FAIL | 隔离 TEST beam、真实 `pg_conf`、全量 EUnit 0 fail/0 cancel | `PR-W1-A01..A03`、`PR-W4-A01` |
| `CP-TD-A09`、`CP-FINAL-A06` | FAIL | Dialyzer 执行真实性、相位隔离、`NEW=0`、高风险存量清零 | `PR-W1-A04/A05`、`PR-W4-A01` |
| `CP-TD-A07` | FAIL | App 八项合同重新盘点并逐项 red-green | `PR-W2-A01/A02` |
| `CP-ASSET-A08` | FAIL | Widget 配对门接入两仓 CI，负例能拦截 | `PR-W2-A03` |
| `CP-DQ-A03` | FAIL | 客服真机测试结构和四项合同重判/修复 | `PR-W2-A04`、`PR-W5-A02` |
| `CP-CON-A09` | BLOCKED_ENV | 独立 Admin E2E 后端、种子和短期凭据，禁 skip 假绿 | `PR-W2-A05` |
| `CP-DQ-A04..A07` | BLOCKED_EXTERNAL | 当前 LiveKit/TURN 复测；升级、修复或获授权的生产承载三选一后实测 | `PR-W5-A03..A05`、`PR-W6-A04` |
| 泄漏 Cookie/key | OPEN | 盘点影响面、双 key 兼容轮换、撤销旧 key、负例验证 | `PR-W6-A02` |

映射不是自动继承判定。W0 必须在当前三仓 `main` 上重判文件、提交、测试和环境；已消失或
已修复的项目以当前候选证据改为 `PASS` 或 `NOT_APPLICABLE`，不得为了匹配旧计数重新制造任务。

### 0.3 五层完成定义

| 层 | PASS 的全部条件 |
|---|---|
| T1 | W0-W4 所有 Required Acceptance PASS；冻结三仓 SHA 后，A0 在同一 L3 日志中一次跑完全部腿，0 fail、0 cancel、0 新告警、0 skip 假绿 |
| T2 | Android 真机 + macOS App 完成企业、客服、消息和 C2C 音视频真实双端旅程；信令、LiveKit participant/track、RTP/relay 四方同会话关联；负例 fail-closed |
| T3 | 生产目标、镜像 digest、迁移、容量、证书、密钥、监控、备份恢复和回滚方案均通过只读或隔离预演；无开放 P0/P1 |
| T4 | 用户对精确目标和窗口重新授权后，灰度发布、合成流量、故障注入、回滚演练和观测窗口全部通过 |
| T5 | post-verifier 全绿、SLO 无回退、审计证据完整；旧 TURN 是否退出由第二次独立授权决定，不得用“不删除旧 TURN”阻止新栈投产，但必须保留已验证回滚路径 |

### 0.4 非目标

- 不在本计划中增加新产品功能、重做 UI、清理与投产无关的全部历史 lint。
- 不强制 iOS 进入 T2；iOS 只做 best-effort，失败单列且不冒充 Android/macOS 证据。
- 不自行购买云资源、创建付费服务、联系第三方、push 或对外发布。
- 不把 457 条 Dialyzer 存量全部机械改到 0；投产门要求执行链真实、`NEW=0`、P0/P1/P2
  风险告警清零，其余逐条有分类、owner、到期日且基线只减不增。若无法完成分类，则 T1 FAIL。

---

## 1. 冻结输入与计划绑定

### 1.1 编制时采样（执行时必须重采样）

| 仓库 | 编制时 `main` | 已知状态 |
|---|---|---|
| `imboy` | `c215125f8d27e31dfabf351a54a7423d2c147129` | 主工作树有大量用户 staged WIP，严禁触碰 |
| `imboyapp` | `28347e4f57f95662b791f4f02667283f9df7b602` | clean |
| `imboyadmin` | `66ac0baad010b204f063fa43288cca238af9f8f1` | clean |

以上 SHA 只是计划编制锚，不是执行候选。执行者必须输出 `control/input-manifest.json`，记录：

- 计划文件 SHA-256 和 sidecar 一致性；
- 三仓绝对路径、HEAD、分支、status porcelain、upstream、remote URL（凭据脱敏）；
- V1.2 三个候选是否为当前 `main` 祖先；
- 所有 worktree/branch/process/port/DB/device 及其 owner；
- 用户 WIP 的 `git diff --cached --name-status` 和 `git diff --name-status` 指纹，仅记录路径和 SHA，
  不复制内容、不 stash；
- 生产只读采样的 DNS、TLS 到期日、开放端口、现役版本和入口 owner；未经授权不得 SSH 写入。

### 1.2 计划 SHA 规则

同目录 `2026-09-28-cross-plan-production-readiness-closure-v1.3.md.sha256` 是唯一静态绑定。
运行时复制计划和 sidecar 到 `RUN_ROOT/input/`，先执行：

```bash
cd /Users/leeyi/project/imboy.pub/imboy/docs/plans
shasum -a 256 -c 2026-09-28-cross-plan-production-readiness-closure-v1.3.md.sha256
```

失败即 `BLOCKED_PLAN_DRIFT`，不得继续。计划正文不嵌入自身哈希，避免自引用不动点缺陷。

### 1.3 历史证据规则

V1.2 run `crossplan-v12-20260927T000125Z-b7fd6733` 只用于定位回归和比较计数。
它不能给 V1.3 任何 Acceptance 自动赋 PASS。每个 PASS 必须绑定 V1.3 的候选 SHA、命令、
exit code、计数、真实 oracle、证据路径和证据 SHA-256。

---

## 2. Git、并发和资源纪律

### 2.1 工作树与所有权

1. A0 为三仓各建立一个 `run/$RUN_ID/integration` 分支和隔离 worktree；每个 Worker 使用
  自己的 `run/$RUN_ID/CARD_ID` 分支/worktree。禁止在共享 `main` 写文件。
2. Worker 只能改卡片声明的 Exclusive paths；发现重叠立即 `BLOCKED_CONFLICT`，交 A0 排序。
3. Worker 不得 merge/rebase/cherry-pick 到 integration；只提交独立、本地、最小功能提交并
   报告 commit SHA。A0 是唯一集成者。
4. 禁止 `git reset --hard`、`git clean`、`git stash`、`git checkout -- <path>`、`branch -D`、
   blanket `git add -A`。不得吸收主工作树用户 WIP。
5. Git author/committer 固定为 `leeyi <leeyisoft@qq.com>`，仅用于本地提交，不授权 push。
6. 并发上限 8 包含 A0。共享 PostgreSQL、设备、端口、迁移目录、候选冻结和生产目标均需租约；
   lease 过期或 owner 不明时停止，不抢占。

### 2.2 禁入路径

- `imboy/erlang.mk`；
- `imboyapp/ios/**`、`imboyapp/macos/**`、`imboyapp/plugin/r_upgrade/**`；
- 三仓主工作树已有 staged/unstaged 文件；
- 外来 worktree、stash、beam 节点、Docker volume、数据库和 `.Codex/runs`；
- 含生产 PII、明文密钥、Cookie、token、手机号、邮箱或真实凭据的证据文件。

如修复必须进入禁入路径，只产出 `WAITING_USER_AUTH`/`BLOCKED_SCOPE` 说明，不能自行扩权。

### 2.3 资源租约

`control/leases.tsv` 至少包含 `resource owner card acquired_at expires_at release_state`。
固定资源名：`PG_SCRATCH`、`PORT_9800`、`PORT_8082`、`ANDROID_1`、`MACOS_APP_1`、
`L3_GATE`、`PROD_TARGET`。心跳间隔不超过 15 分钟，30 分钟无心跳由 A0 标记 stale，
只允许确认进程归属后释放。

### 2.4 集成顺序

每仓提交按 `测试/守卫 -> 根因修复 -> CI 接线 -> 文档` 排序。A0 对每个提交先执行
`git diff --check` 和卡级 L0/L1，再 cherry-pick。冲突不得自动选 ours/theirs；交回原 Worker
在最新 integration HEAD 上重放。每次集成后更新 `control/candidates.txt`，冻结前不得跑 L3。

---

## 3. 状态机、证据与重试

### 3.1 合法状态

```text
PENDING
RUNNING
PASS
FAIL
BLOCKED_ENV
BLOCKED_EXTERNAL
BLOCKED_CONFLICT
BLOCKED_PLAN_DRIFT
BLOCKED_SCOPE
WAITING_USER_AUTH
NOT_APPLICABLE
```

- `PASS`：所有 Acceptance 同一候选、同一轮次满足。
- `FAIL`：候选自身不满足判据；不能用 BLOCKED 美化。
- `BLOCKED_ENV`：缺本地环境/设备/凭据，且已有可执行解除条件。
- `BLOCKED_EXTERNAL`：上游或第三方缺陷，必须有协议级复现和替代路径评估。
- `WAITING_USER_AUTH`：下一动作会造成外向、不可逆、付费、生产写或第三方影响。
- `NOT_APPLICABLE`：W0 证明旧问题在当前输入不存在，须 A0 + 独立 Reviewer 双签。

### 3.2 证据目录

```text
/Users/leeyi/project/imboy.pub/.Codex/runs/prodready-v13-YYYYMMDDTHHMMSSZ-8hex/
  input/
  control/{input-manifest.json,candidates.txt,acceptance.tsv,ledger.tsv,leases.tsv,heartbeats.tsv}
  evidence/ACCEPTANCE_ID/
  reports/CARD_ID.md
  checkpoints/W0.md ... W8.md
  FINAL/{RESULT.json,acceptance-matrix.md,final-report.md,evidence-manifest.sha256}
```

`acceptance.tsv` 固定列：

```text
acceptance_id card_id wave required owner repo base_sha candidate_sha command timeout_s
attempt exit_code observed_count oracle state evidence_path evidence_sha256 started_at finished_at note
```

所有命令同时保存 stdout/stderr 和 `${PIPESTATUS[0]}`；证据写完立即 `sha256sum`。禁止回填伪造
heartbeat、改旧日志或只摘录有利片段。凭据只记录“来源类型 + 指纹后 8 位 + 到期时间”，不得落明文。

### 3.3 测试阶梯与重试

| 层级 | 谁执行 | 内容 | 规则 |
|---|---|---|---|
| L0 | Worker | 格式、静态、单文件/单模块守卫 | 每次改动 |
| L1 | Worker | 所属域单元/合同/迁移负例 | 必须 red-before/green-after，旧问题不适用时说明 |
| L2 | Worker/A0 | 真实 scratch DB、浏览器、双端设备旅程 | mock 不能替代 |
| L3 | 仅 A0 | 冻结三仓候选完整门 | 只在 W1-W3 全绿后跑一次；失败后解冻修复，再生成新候选，只允许最终再跑一次 |
| L4 | 仅 A0 | 获授权后的生产灰度、观测与回滚 | 必须有授权串和回滚 owner |

每个确定性命令最多 2 次尝试；第二次前必须记录第一次失败分类和改变了什么。不得原样重试到绿。
网络/上游探针最多 3 次，间隔和结果全部保留。超限即 FAIL 或相应 BLOCKED。

### 3.4 每卡统一合同

后续表格中的每张卡均继承：`base_sha` 为开卡时 integration HEAD，`candidate_sha` 为 Worker
提交；cwd 是表中仓库 worktree；timeout 见命令；evidence 为 `evidence/ACCEPTANCE_ID/`；
rollback 是只撤该卡提交或恢复隔离资源；stop 条件为越权、WIP 漂移、资源无 lease、证据含敏感数据、
命令目标不明确或连续两次失败。表中未声明的路径一律只读。

---

## 4. 波次与任务卡

### W0：重基线、隔离和未完成项重判

W0 完成前禁止写业务代码。A0 可并行派发只读盘点，但自己负责最终输入冻结。

| 卡 / Owner | 依赖、仓库、Exclusive paths | 命令与 timeout | Acceptance / oracle | 回滚与停止 |
|---|---|---|---|---|
| `PR-W0-C01` A0 | 无；三仓；仅 `RUN_ROOT/control/**` | 三仓 `git status --porcelain=v2 --branch`、`git rev-parse HEAD`、`git worktree list --porcelain`、`git branch -vv`；120s | `PR-W0-A01`：manifest 三仓齐全；主树 WIP 指纹前后相同；计划 sidecar PASS | 删除本 run 新建的空 worktree/branch；任何主树变化立即 FAIL |
| `PR-W0-C02` A0 | A01；三仓；仅新 worktree | 从 V1.2 `control/candidates.txt` 逐仓读取 SHA 后执行 `git merge-base --is-ancestor "$V12_SHA" main`；进程/端口/设备/DB 只读盘点；300s | `PR-W0-A02`：资源 owner 和冲突矩阵完整，无未声明占用 | 不结束 foreign 进程；冲突标 `BLOCKED_CONFLICT` |
| `PR-W0-C03` Reviewer | A01；三仓只读 | 从 V1.2 `acceptance.tsv` 读取 13 个非 PASS 的 evidence/path 后逐项 `rg`、`git log --all -- "$AFFECTED_PATH"`、聚焦命令；1800s | `PR-W0-A03`：每项为 `REPRODUCED/FIXED_CURRENT/ABSENT/BLOCKED`，且有当前 SHA 和 oracle | 不修改；无法证明不得记已修 |
| `PR-W0-C04` Security | A01；生产只读 | TLS/DNS/HTTP/nc、镜像/Compose 静态、密钥引用路径扫描；600s | `PR-W0-A04`：泄漏 key 影响节点、证书到期、443 owner、epmd/管理端口、现役 TURN/LiveKit 版本清单 | 禁止 SSH 写和明文凭据；需认证即 WAITING_USER_AUTH |
| `PR-W0-C05` A0 | A01-A04；`RUN_ROOT/**` | 生成 acceptance.tsv、leases、DAG；校验 ID 唯一、依赖无环、Required 无空 oracle；120s | `PR-W0-A05`：全部检查 exit 0；每个旧非 PASS 恰映射一次 | 不允许用裸 `-` 状态；失败不进 W1 |

W0 checkpoint 必须明确：当前三个 base SHA、用户 WIP、仍需修复的真实集合、资源冲突、
生产只读事实、计划是否漂移。旧报告计数与当前事实冲突时，以 W0 当前证据为准并解释差异。

### W1：后端测试与静态分析可信化

W1-1 与 W1-2 可在不同 backend worktree 并行；两者都不得改 `erlang.mk`。

| 卡 / Owner | 依赖、仓库、Exclusive paths | 命令与 timeout | Acceptance / oracle | 回滚与停止 |
|---|---|---|---|---|
| `PR-W1-C01` Backend-1 | W0；imboy；`Makefile`、`scripts/check/beam_*`、`scripts/run/isolated_eunit.sh`、必要 `test/common/**` | `IMBOYENV=local make compile` 900s；隔离 `make eunit-local EUNIT_CONFIG=config/sys` 7200s；app/test 相位负例 | `PR-W1-A01`：app 与 TEST beam 不混居；全量 exit 0、Failed 0、cancelled 0、missing_config 0；同候选连续两轮失败集均空 | 撤本卡；若需改 erlang.mk 则 BLOCKED_SCOPE |
| `PR-W1-C02` Backend-2 | W0；imboy；`test/common/eunit_runner.erl` 及 W0 仍复现的最小测试文件 | 五个历史 DS 套件 solo + 全量，单项 900s/全量 7200s | `PR-W1-A02`：就绪等待有界；0 `[readiness_gate]` skip；不得包裹测试体；五套件和全量都绿 | 轮询总预算超过 EUnit setup 边界或掩蔽真错立即停止 |
| `PR-W1-C03` Backend-3 | W0；imboy；`scripts/check_dialyzer_baseline.sh`、`Makefile` dialyze 目标、相位守卫 | `make dialyze-check` 5400s；截断 log 负例；TEST beam 负例 | `PR-W1-A03`：缺 `Proceeding with analysis` 必红；执行失败和 ratchet RED 分开；`NEW=0`；baseline <=457 | 不得靠 `|| true` 把执行崩溃判绿；PLT/dep 缺失归因后再重试 |
| `PR-W1-C04` Backend-4 | A03；imboy；仅 W0 分类为高风险告警涉及文件、`dialyzer.baseline` | 按模块 `make dialyze-check` + 聚焦 EUnit；5400s | `PR-W1-A04`：457 条全量分类；P0/P1/P2=0；保留项有 owner/reason/expiry/test，`NEW=0` 且只减不增 | 禁止批量加 baseline、改类型规避分析或无测试删分支 |
| `PR-W1-C05` Backend-5 | W0；imboy；`.github/workflows/nightly.yml` 或最小新 workflow、`.qa/**` | workflow 语法检查；真 PG 隔离 runner 全量；7200s | `PR-W1-A05`：nightly full EUnit 注入 config 和 scratch PG；NEW 分类红、known 稳定绿；无 `make eunit || true` | 不开启 cron/push；只完成本地/静态 CI 接线，远端 run 需后续授权 |

### W2：合同、客服、Admin 和 Widget 收口

W2 可并行 5 卡，迁移和共享合同文件必须串行租约。

| 卡 / Owner | 依赖、仓库、Exclusive paths | 命令与 timeout | Acceptance / oracle | 回滚与停止 |
|---|---|---|---|---|
| `PR-W2-C01` Flutter-1 | W0-A03；imboyapp；`lib/modules/enterprise/infrastructure/enterprise_api.dart`、`lib/modules/customer_service/infrastructure/cs_api.dart`、对应 tests | 对 W0 `control/rebaseline.tsv` 中该卡的每个 `focused_test_path` 逐一执行 `flutter test "$focused_test_path"`，单项 1800s；scratch HTTP 合同 oracle 1800s | `PR-W2-A01`：W0 仍复现的八项漂移逐项 red-green；方法/字段/分页/错误码与冻结后端一致；8/8 有独立证据 | 后端 BY_DESIGN 合同不得为迁就 App 而改；有歧义交 A0 |
| `PR-W2-C02` Contract | A01；imboy + app；后端 `.contract/**`、合同导出涉及最小文件 | `make contract-check`、`make rest-contract-check`、`python3 scripts/check_product_feature_cross_repo.py`；1800s | `PR-W2-A02`：运行时 route = OpenAPI = Postman = App/Admin；31ops/14scope 或 W0 当前合法新基线三向一致；TSID 均安全解析 | 生成文件必须由权威 exporter 产生，不手改 JSON |
| `PR-W2-C03` Widget | W0；imboy + imboyadmin；两仓 `.github/workflows/**`、既有 pairing scripts | 两仓 pairing 正例；临时篡改单边 hash 负例；workflow 语法/act dry-run；900s | `PR-W2-A03`：两仓 CI 均实际调用同一互验入口；正例 0、负例 2；临时篡改无残留 | 不新增重复脚本；负例必须在临时副本/cleanup trap 内 |
| `PR-W2-C04` Flutter-2 | W0-A03；imboyapp；当前实际客服 workbench 测试及最小实现 | `flutter test` 聚焦；Android 真机命令留给 W5；1800s | `PR-W2-A04`：顶层 `expect` 问题不存在；四项旧漂移逐项 CURRENT/FIXED；聚焦测试 0 fail/0 skip | 若旧文件当前不存在，不重建，走 NOT_APPLICABLE 双签 |
| `PR-W2-C05` Admin | W0；imboyadmin；`tests/e2e/**`、`support/**`、专用 seed/配置、workflow | `bun test --isolate` 1800s；`bun run build` 1800s；专用后端上 `bun run test:e2e` 3600s | `PR-W2-A05`：unit 0 fail；build 0 warn；E2E 0 fail、Required 0 skip；短期账号由 scratch seed 创建，不依赖个人/生产账号；2 条过期用例删除或按现合同改写 | 凭据不得入仓；无法创建隔离账号则 BLOCKED_ENV，不得用 skip |

### W3：发布安全、数据和运维能力

W3 不接触生产，只在隔离/临时环境验证投产基本能力。

| 卡 / Owner | 依赖、仓库、Exclusive paths | 命令与 timeout | Acceptance / oracle | 回滚与停止 |
|---|---|---|---|---|
| `PR-W3-C01` Database | W0；imboy；`priv/migrations/**`、迁移检查和必要测试 | `make migrations-check`；空库 up、快照库 up、down/up；3600s | `PR-W3-A01`：序号唯一、up/down 成对；空库和脱敏快照升级成功；关键表/索引/约束 oracle 一致；失败可回退 | 只用 scratch DB；任何生产 DSN 立即停止 |
| `PR-W3-C02` Security | W1/W2；三仓；安全配置、现有扫描规则的最小修复 | gitleaks git+working tree、依赖漏洞、SBOM diff、鉴权负例；3600s | `PR-W3-A02`：新增 secret 0；可利用 Critical/High 0（例外需 owner+expiry+缓解）；JWT/WS/Internal API 越权和跨租户负例 fail-closed；SBOM 可追溯 | 不扩大 ignore；不得把真实 secret 放进测试 |
| `PR-W3-C03` Backup | W0；imboy；`scripts/backup_*`、`restore_pg.sh`、`restore_smoke.sh` 及最小修复 | scratch 备份、校验、异机目录恢复、恢复后 smoke；3600s | `PR-W3-A03`：备份可解密/校验，恢复数据计数与抽样 hash 一致；测得 RPO/RTO 并满足已声明目标；Garage+PG 一致性说明完整 | 绝不覆盖现有 DB/volume；恢复目标名必须含 RUN_ID |
| `PR-W3-C04` Ops | W0；imboy；deploy/monitoring/runbook 最小修复 | `bash deploy/preflight.sh`（隔离 env）；Compose config；alert render tests；900s | `PR-W3-A04`：健康、错误率、延迟、DB pool、WS、LiveKit/TURN、磁盘、证书均有指标和告警；告警负例能触发；runbook 含 owner/止损/回滚 | 不发送真实告警；用本地 sink |
| `PR-W3-C05` Performance | W1/W2；三仓；仅测试/阈值文件 | `scripts/bench_websocket.sh`、关键 REST/WS/上传基准；3600s | `PR-W3-A05`：冻结硬件/数据量/并发；p95/p99、错误率、资源水位不劣于基线阈值；持续 30min 无泄漏趋势；容量结论含安全余量 | 不压生产；无稳定基线则先形成 baseline，不得拍脑袋 PASS |
| `PR-W3-C06` Release | W1-W3；三仓；release manifest/构建脚本最小修复 | clean checkout 可重复构建两次；比较版本、Git SHA、digest、SBOM；3600s | `PR-W3-A06`：同输入产物可追溯；版本三仓兼容；镜像 pin digest；release note 含迁移/回滚/已知限制 | 不 push 镜像、不签发正式 release |

### W4：冻结候选和唯一完整 L3

W1-W3 所有 Required Acceptance PASS 后，A0 串行执行。Worker 全部释放共享资源并停止写入。

1. A0 按后端 -> App -> Admin 的依赖顺序集成，逐提交跑 L0/L1。
2. 生成三仓 `candidate_sha`、dirty=0、submodule 状态、工具版本和 lockfile 指纹。
3. 连续采样 120 秒确认三仓候选、DB、端口和进程无漂移。
4. 取得 `L3_GATE` 独占租约，在同一总日志顺序执行：

```bash
# imboy candidate，timeout 150m
IMBOYENV=local make compile
IMBOYENV=local make eunit-local EUNIT_CONFIG=config/sys
IMBOYENV=local make dialyze-check
make contract-check
make rest-contract-check
make migrations-check
bash scripts/check_module_boundaries.sh
bash scripts/check_feature_architecture.sh

# imboyapp candidate，timeout 120m
dart format --output=none --set-exit-if-changed .
flutter analyze
flutter test

# imboyadmin candidate，timeout 90m
bun install --frozen-lockfile
bun test --isolate
bun run lint
bun run typecheck
bun run build
bun run verify:widget-pairing
bun run test:e2e
```

| Acceptance | PASS oracle |
|---|---|
| `PR-W4-A01` | 总日志以 `L3_PASS` 结束；每步实际执行且 exit 0；EUnit 0 fail/0 cancel；Dialyzer `NEW=0` 且真实完成；无 fail-fast 跳腿 |
| `PR-W4-A02` | Flutter format 零 diff、analyze 0 issue、全量 test 0 fail；skip 逐项在 allowlist 且不覆盖 Required |
| `PR-W4-A03` | Admin unit/lint/type/build/widget/E2E 全部 0 fail；E2E 使用隔离真实后端，Required 0 skip |
| `PR-W4-A04` | 三仓在 L3 前后 HEAD、tree、lockfile 指纹相同；证据 manifest 全 MATCH |

若 L3 失败：立即 `T1=FAIL`，解冻并按失败卡回到 W1-W3；修复后生成全新候选。全计划最多
允许一次最终 L3 重跑，仍失败则终止，不继续 T2/T3。L3 PASS 后任何代码改动都会使 T1 失效。

### W5：Android 真机 + macOS App 设备资格

前置：T1 PASS；取得两个设备租约；两端安装包必须来自 W4 冻结候选。禁止模拟器替代。
测试账号和数据只在隔离测试库创建，标记 RUN_ID，结束后按 ledger 精确清理。

| 卡 / Owner | 命令/旅程 | Acceptance / 真实 oracle | 停止条件 |
|---|---|---|---|
| `PR-W5-C01` Device-1 | Android + macOS 各执行登录、个人/企业切换、组织/成员/角色/越权负例 | `PR-W5-A01`：双端 UI + API + DB 三方状态一致；跨租户/越权均 401/403 且无落库 | 账号或环境不属于 run 即停 |
| `PR-W5-C02` Device-2 | 客服工作台进入、排队、分配、消息、已读、转接/关闭、无坐席留言 | `PR-W5-A02`：Android 连续 2 轮 0 fail；Admin/DB/客户端状态机一致；非 mock | 任一 Required skip 即 FAIL |
| `PR-W5-C03` Device-3 | 两账号建立好友；Android 发起 C2C 视频，macOS 接听；反向再跑；Wi-Fi 与受限网络各一轮 | `PR-W5-A03`：双方都出现来电 UI、接听后双方计时、双向音视频帧非零、挂断同步；不是“已响铃”单侧现象 | 无媒体帧或会话 ID 不一致即 FAIL |
| `PR-W5-C04` RTC | 同会话采集 backend allocation、LiveKit room/participant/track、TURN allocation/permission/channel、RTP packet/byte | `PR-W5-A04`：四类证据可由 room/session ID 关联；relay-only 下双向 RTP >0；不得用 Allocate 成功代替转发成功 | LiveKit 上游仍复现 0 packet -> BLOCKED_EXTERNAL |
| `PR-W5-C05` Negative | 过期/伪造 JWT、错误 room、撤权、断网重连、对端拒接、摄像头/麦克风拒权 | `PR-W5-A05`：全部 fail-closed，无越权媒体、无无限响铃、无幽灵 participant；恢复后可再次正常通话 | 任一 fail-open 为 P0，立即停止 |

设备证据保存脱敏事件、时间线和必要截图/短视频；不得保存真实聊天内容。若 relay 缺陷仍存在，A0 必须
比较三条最小路径：升级到已验证版本、最小 upstream/backport 修复、获授权的现役 TURN 回滚承载。
仅在其中一条通过同一探针后才能解除 BLOCKED；改为 `NO_STRICT_443` 需要用户明确接受产品限制。

### W6：生产前检查与授权门

W6 分为只读/隔离预演和生产写两部分。`PR-W6-A01/A03/A05` 可先做；其余必须等待新授权。

| 卡 / Owner | 前置与动作 | Acceptance / oracle | 权限门 |
|---|---|---|---|
| `PR-W6-C01` A0/Ops | T1/T2；只读采样生产 topology、版本、TLS、DNS、防火墙、HAProxy/Nginx/LiveKit/TURN、DB/对象存储容量 | `PR-W6-A01`：目标 host/service/region/IP/digest 明确；证书余量 >=30 天；管理口和 epmd 不公网暴露；配置与 runbook 一致 | 只读；SSH 写前停止 |
| `PR-W6-C02` Security | 影响面清单、双 key 兼容方案、节点滚动顺序、旧 key 撤销负例 | `PR-W6-A02`：所有受影响节点含历史节点均换新；旧 key 认证失败、新 key 正常；日志/仓库无明文 | 必须取得含目标节点、窗口、回滚的精确授权串 |
| `PR-W6-C03` Database | 生产备份只读核验；在隔离恢复目标演练；迁移 dry-run | `PR-W6-A03`：最近备份可恢复；RPO/RTO 满足目标；迁移耗时/锁等待/磁盘余量可接受；回滚 SQL 验证 | 恢复不得指向生产；生产迁移另授权 |
| `PR-W6-C04` Network | L4 SNI/HAProxy 配置静态校验、证书续签 hook、TURN 443 relay 探针、失败回切演练 | `PR-W6-A04`：全部现役 SNI 路由不丢失；HTTPS/WS/RTC/TURN 同时通过；证书自动续签 dry-run；回切 <=目标时间 | 入口切换和 reload 必须精确授权 |
| `PR-W6-C05` Observability | dashboard/alert/日志保留和合成探针只读核对 | `PR-W6-A05`：发布前基线 30min；告警 receiver 有 owner，但本轮不发送第三方通知；关键 SLI 均可查询 | 真实通知测试需再次确认联系方式 |
| `PR-W6-C06` A0 | 固化 release manifest、部署命令、目标 digest、迁移集合、canary 比例、窗口、rollback trigger | `PR-W6-A06`：Reviewer 逐字段复核；命令不含占位符/明文 secret；授权申请可直接逐字确认 | 未确认则 `WAITING_USER_AUTH` |

授权申请必须是单一、精确、可审计文本，至少包含：目标环境/主机/服务、候选 SHA 和镜像 digest、
时间窗、预计影响、生产写清单、密钥/证书/入口动作、canary 比例、回滚阈值、回滚 owner、是否发送通知。
用户只确认其中一部分时，未确认部分继续 WAITING_USER_AUTH。不得复用旧会话“确认”。

### W7：受控生产 Canary、观测与回滚演练

只有 T3 PASS 且用户逐字确认 W6 授权申请后执行。A0 是唯一生产操作者，Worker 只读观察。

1. 发布前 5 分钟再次核对 target、digest、备份、lease、回滚命令和当班 owner。
2. 先 DB 向后兼容迁移，再后端单节点/最小流量，再 Admin/Widget，再客户端兼容性验证；禁止一次全量。
3. canary 从内部/合成账号开始，流量阶梯 `1% -> 5% -> 25% -> 100%`；每阶至少 15 分钟，
   100% 后观测至少 60 分钟。低流量环境用“固定合成旅程数”替代百分比并记录原因。
4. 每阶跑健康、登录、消息、客服、企业权限、上传、RTC relay 合成旅程；同时观察错误率、p95/p99、
   DB pool/lock、WS 断线、LiveKit room/packet、TURN allocation/relay、CPU/内存/磁盘。
5. 在 canary 范围执行一次可控故障：终止 canary 实例或断开其上游，验证摘流和恢复；不得故障注入全量节点。
6. 执行一次真实回滚到上一个已知 digest，跑 post-rollback verifier，再恢复候选并重新走 canary；
   若业务窗口不允许恢复候选，则以回滚成功结束，`T4=FAIL/NO_GO`，不得声称已投产。

| Acceptance | PASS oracle |
|---|---|
| `PR-W7-A01` | 每阶部署对象 digest 与 manifest 相同，迁移状态唯一，无 mixed unknown version |
| `PR-W7-A02` | 每阶所有合成旅程 100% 通过；无真实用户数据进入证据 |
| `PR-W7-A03` | 相比 30min 基线：5xx、WS 断线、RTC 失败率、p95/p99、资源水位未越阈；0 新 P0/P1 |
| `PR-W7-A04` | 故障实例自动摘流，既有连接按 runbook 恢复，无消息重复/丢失 oracle |
| `PR-W7-A05` | 回滚在目标时间内完成；旧版本 + 数据 schema 可运行；post-rollback 旅程全绿 |
| `PR-W7-A06` | 候选恢复后 60min 观测全绿，告警 0 unresolved，审计日志完整 |

任一回滚触发条件命中（数据错误、越权、消息丢失、RTC 单向、5xx/延迟越阈、不可解释资源增长、
监控盲区）立即停止升级并回滚，不等待第二次确认。回滚是已授权发布动作的安全组成；不得扩大目标。

### W8：投产判定、旧 TURN 第二授权与清理

1. A0 在生产最终状态执行 post-verifier，生成 T1-T5 分层结果；Reviewer 独立重算 Acceptance 计数、
   evidence hash、候选 SHA、部署 digest、迁移版本和观测窗口。
2. 旧 TURN 保留为回滚能力，直到：新 LiveKit/TURN relay 的 T2+T4 通过、证书自动续签已验证、
   旧客户端最低版本/兼容政策生效、回滚演练通过、稳定观察期至少 7 天或用户另行批准更短窗口。
3. 满足上述条件后只生成旧 TURN 退出变更单：流量归零 -> 禁新 allocation -> 观察 -> stop -> 再观察 ->
   删除资源。`stop` 和 `delete` 分别需要新的第二次授权，不能由本计划执行授权推导。
4. 精确清理本 run 创建的测试账号、scratch DB、端口、容器和已合并 worktree；清理前后 ledger 对账。
   未合并分支只列清单，不 `-D`。主工作树用户 WIP 指纹必须与 W0 相同。

| Acceptance | PASS oracle |
|---|---|
| `PR-W8-A01` | FINAL 产物四件套 hash 全 MATCH；Required 无 PENDING、无缺证 PASS、无候选漂移 |
| `PR-W8-A02` | 生产最终 digest、迁移、配置、证书和 dashboard 与 release manifest 一致 |
| `PR-W8-A03` | 7 天稳定期内 SLO 满足；若尚未到期则状态保持 `WAITING_OBSERVATION`，不得提前 PASS |
| `PR-W8-A04` | 旧 TURN 未授权时明确 `RETAINED_AS_ROLLBACK`；获第二授权后每阶段 verifier 通过 |
| `PR-W8-A05` | run 自有资源清理完成，foreign 资源和主树 WIP 零变化 |
| `PR-W8-A06` | T1-T5 全 PASS 时才输出 `PRODUCTION_READY=PASS`；否则机械输出 `NO_GO` 和最小解除清单 |

---

## 5. 依赖 DAG 与并行排程

```text
W0
 ├─ W1 backend test/dialyzer/CI ─┐
 ├─ W2 app/admin/widget/contracts ├─ W4 frozen L3 ─ W5 devices ─ W6 preflight
 └─ W3 security/data/ops/perf ───┘                         │
                                                           ├─ WAITING_USER_AUTH
                                                           └─ W7 canary ─ W8 final/observe
```

建议并发（含 A0 不超过 8）：

| 阶段 | Worker 席位 | 说明 |
|---|---:|---|
| W0 | 3 | A0 + Git/resource reviewer + security read-only |
| W1-W3 第一批 | 7 | Backend beam、Dialyzer、Flutter contract、Admin E2E、DB、Security、Ops/backup |
| W1-W3 第二批 | 5 | CI、Widget、performance、release、独立 review；复用已完成席位 |
| W4 | 0 | 只有 A0 写；其余只读 reviewer |
| W5 | 3 | Android、macOS、RTC 证据；设备租约控制串行交互 |
| W6-W8 | 2 | A0 生产操作 + 独立只读 observer；不得并行写生产 |

Worker 禁止跑全量 L3。卡级 L0/L1/L2 的通过只允许合入候选，不允许发布。

---

## 6. 生产质量阈值与止损线

若项目已有更严格阈值，取更严格值；没有历史 SLO 时使用下表作为本次最低门槛并在 W3 固化：

| 项目 | 最低门槛 |
|---|---|
| 可用性 | canary/观测窗口关键 API 和 WS 合成成功率 >= 99.9% |
| HTTP | 5xx < 0.1%；关键读 API p95 < 300ms、p99 < 800ms（不含上传体传输） |
| 消息 | C2C/C2G 端到端成功率 100%（测试样本），无重复/丢失；WS 重连后补偿一致 |
| RTC | 呼叫建立成功率 100%（资格矩阵样本）；relay-only 双向 packet/byte > 0；无单向计时/响铃悬挂 |
| DB | 迁移无不可接受长锁；连接池不耗尽；回滚后 schema/应用兼容 |
| 资源 | 30min 稳态 CPU/内存/FD/连接无持续无界增长，磁盘余量 >= 30% |
| 安全 | 新 secret 0；可利用 Critical/High 0；越权/跨租户/伪造凭据全部 fail-closed |
| 恢复 | 备份恢复数据 oracle 一致；实际 RTO/RPO 不超过 W3 声明值 |

任何数据损坏、跨租户读取、认证绕过、密钥泄漏扩大、消息不可恢复丢失、回滚不可用为 P0，
立即 `NO_GO`。P1 未关闭不得投产。P2 必须有 owner、缓解、到期日并经 A0/Reviewer 接受。

---

## 7. GLM 5.3 执行合同（复制整段作为启动提示词）

```text
你是 IMBoy Production Readiness V1.3 的 A0 协调器。严格执行只读计划源：
/Users/leeyi/project/imboy.pub/.Codex/worktrees/crossplan-production-readiness-v1.3/docs/plans/2026-09-28-cross-plan-production-readiness-closure-v1.3.md

先确认该计划源分支为 plan/crossplan-production-readiness-v1.3，且计划提交是用户交付时给出的 PLAN_COMMIT；
计划源 worktree 只读，实施必须另建 run-scoped worktree，不得直接在计划源中开发。

目标不是“尽量多完成”，而是按计划生成可机械复核的 T1-T5 判定。先校验计划 sidecar，随后创建唯一 RUN_ID：
prodready-v13-YYYYMMDDTHHMMSSZ-8hex（时间取当前 UTC，8hex 取安全随机值），证据根固定为 /Users/leeyi/project/imboy.pub/.Codex/runs/$RUN_ID。

硬规则：
1. /Users/leeyi/project/imboy.pub 是 umbrella，不是 Git 仓库；独立采样 imboy/imboyapp/imboyadmin。
2. 共享 main 只读。当前 imboy 主工作树有用户 staged/unstaged WIP；严禁 reset/clean/stash/restore/blanket-add，严禁吸收或覆盖。
3. 所有写入在 run-scoped worktree/branch；A0 是唯一 integration、L3、final 和生产操作者。最大并发 8 含 A0，最多 7 Worker。
4. Worker 只跑 L0/L1/L2；冻结三仓候选后仅 A0 跑一次完整 L3。失败必须解冻修复并形成新候选，全计划最多一次最终重跑。
5. 每个 PASS 必须绑定 candidate SHA、完整命令、exit、计数、真实 oracle、证据路径和 SHA-256。旧报告、代码存在、HTTP 200、container healthy、mock、截图和 skip 均不能单独 PASS。
6. 合法状态只有计划 §3.1。无法执行时如实 BLOCKED/WAITING_USER_AUTH，不得虚构证据、改变判据或把 FAIL 美化为环境问题。
7. 不得将 secret/PII 写入仓库或证据。凭据只记录来源类型、指纹后 8 位和到期时间。
8. 本启动只授权本地 worktree 内实现、测试和本地提交；不授权 push、PR、镜像发布、部署、生产写、密钥轮换/撤销、证书或 HAProxy/LiveKit 入口切换、旧 TURN stop/delete、云资源购买、第三方通知或使用真实联系方式。
9. W6 遇到外向动作，输出一条含精确目标、SHA/digest、窗口、影响、动作、回滚阈值和 owner 的授权申请，状态置 WAITING_USER_AUTH 并停止该动作；不得沿用历史会话授权。
10. Git author/committer 使用 leeyi <leeyisoft@qq.com>，只授权本地提交。每个独立功能单独提交，不混入用户或其他计划改动。

执行顺序：完成 W0 并提交 checkpoint；按 DAG 并行 W1-W3；A0 集成并完成 W4；T1 PASS 后才做 W5；T2 PASS 后做 W6 只读/隔离 preflight；需要生产写时停在授权门。每波结束更新 acceptance.tsv、ledger、checkpoint 和证据 manifest。连续工作直到 PASS、明确 FAIL/BLOCKED，或到达授权门，不向用户询问可由仓库、测试或只读采样自行确定的问题。

最终报告必须首屏给出：T1/T2/T3/T4/T5、PRODUCTION_READY、PASS/FAIL/BLOCKED/WAITING 计数、三仓候选 SHA、生产 digest（未部署写 NOT_PERFORMED）、未完成项的最小解除条件，以及 PUSH/DEPLOY/PRODUCTION_WRITE/KEY_ROTATION/LEGACY_TURN_REMOVAL 的实际状态。
```

---

## 8. 最终报告模板

```text
RUN_ID=
PLAN_SHA256=
IMBOY_CANDIDATE_SHA=
IMBOYAPP_CANDIDATE_SHA=
IMBOYADMIN_CANDIDATE_SHA=

T1_LOCAL_RELEASE_CANDIDATE=PASS|FAIL|BLOCKED_*
T2_DEVICE_QUALIFICATION=PASS|FAIL|BLOCKED_*
T3_PRODUCTION_PREFLIGHT=PASS|FAIL|WAITING_USER_AUTH|BLOCKED_*
T4_PRODUCTION_CANARY=PASS|FAIL|NOT_EXECUTED|WAITING_USER_AUTH
T5_PRODUCTION_READY=PASS|FAIL|NOT_EXECUTED|WAITING_OBSERVATION
PRODUCTION_READY=PASS|NO_GO

ACCEPTANCE_TOTAL=
PASS=
FAIL=
BLOCKED_ENV=
BLOCKED_EXTERNAL=
BLOCKED_CONFLICT=
WAITING_USER_AUTH=
NOT_APPLICABLE=
PENDING=

PUSH=PERFORMED|NOT_PERFORMED
DEPLOY=PERFORMED|NOT_PERFORMED
PRODUCTION_WRITE=PERFORMED|NOT_PERFORMED
KEY_ROTATION=PERFORMED|NOT_PERFORMED
LEGACY_TURN_REMOVAL=PERFORMED|RETAINED_AS_ROLLBACK|NOT_PERFORMED
```

报告正文顺序：结论 -> 未完成/解除条件 -> 三仓与生产物料 -> Acceptance 矩阵 -> 测试和设备证据 ->
安全/数据/容量/观测 -> canary/回滚 -> 权限与红线审计 -> 清理结果。若未获得生产授权，正确终态是
T1/T2/T3 可 PASS、T4/T5 `WAITING_USER_AUTH/NOT_EXECUTED`、`PRODUCTION_READY=NO_GO`，不得将其描述为失败的本地候选。

---

## 9. 计划质量机械自检

执行前和每次计划修订后必须全部通过：

- [ ] sidecar 与计划 SHA-256 一致；输入副本只读。
- [ ] Acceptance ID 唯一；所有依赖指向存在项且 DAG 无环。
- [ ] 每卡有 owner、依赖、仓库/cwd、Exclusive paths、命令、timeout、oracle、evidence、rollback、stop。
- [ ] 三仓 main 只读，用户 WIP 前后指纹一致。
- [ ] 最大并发含 A0 不超过 8；共享资源均有 lease。
- [ ] Worker 没有跑 L3、写 integration、写生产或删除 foreign 资源。
- [ ] 每个 PASS 有 current candidate、exit/count/oracle 和 evidence SHA；无 mock/skip/旧报告冒充。
- [ ] L3 所有腿来自同一冻结候选和同一总日志；失败未继续下游。
- [ ] Android/macOS RTC 有双向真实媒体和服务端/RTP 关联，不是单侧 UI。
- [ ] 备份做过恢复，回滚做过真实演练，监控做过告警负例。
- [ ] 生产动作具有本轮精确授权；联系方式/第三方通知另行确认。
- [ ] 旧 TURN stop/delete 使用第二授权且稳定期已满足。
- [ ] FINAL 计数可由 acceptance.tsv 重算；四件套 hash 全 MATCH；`PENDING=0`。

本清单任何一项失败，计划执行质量不得评为 10/10，且 `PRODUCTION_READY` 不得为 PASS。
