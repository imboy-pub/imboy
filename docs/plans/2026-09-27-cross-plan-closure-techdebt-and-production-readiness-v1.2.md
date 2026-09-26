# 跨计划功能完善、技术债清偿与生产就绪执行计划 V1.2

> 日期：2026-09-27
>
> 目标：在不新增无关产品能力的前提下，把 2026-09-20 至 2026-09-26 各计划已识别但未闭环的功能、合同、测试、设备和生产门逐项完善到可复现、可验收、可恢复状态。
>
> 上游：V1 审计输入、V1.1 评审候选、2026-09-27 质量复核结论。
>
> 本文件取代 V1.1 的执行资格；V1/V1.1 只保留为历史输入。

## 0. 状态与边界

| 信号 | 当前值 |
|---|---|
| `PLAN_STATUS` | `READY_FOR_LOCAL_W0_EXECUTION`，只授权安全的本地 W0；不代表已授权实际启动 |
| `SAFE_TO_EXECUTE_AS_ONE_SHOT_ZCODE` | `NO`，必须按波次和冻结门执行 |
| `LOCAL_IMPLEMENTATION_READY` | `YES_AFTER_W0_BINDING` |
| `DEVICE_READY` | `NO`，依赖冻结候选和 DEV1 修复链 |
| `PRODUCTION_READY` | `NO_GO`，只能由独立生产子计划推导 |
| `PUSH` | `NOT_AUTHORIZED` |
| `DEPLOY` | `NOT_AUTHORIZED` |
| `PRODUCTION_WRITE` | `NOT_AUTHORIZED` |

本计划包含三个相互独立的终态：

1. `T1_LOCAL_CLOSURE`：代码、合同、测试、脚本和本地真实集成闭环。
2. `T2_DEVICE_QUALIFICATION`：Android 真机 + macOS App 的企业、客服和 RTC 旅程闭环；iOS 仅 best-effort。
3. `T3_PRODUCTION_CHILD_PLANS`：只登记独立子计划状态，本计划永不自动执行生产、第三方、发布或删除动作。

```mermaid
flowchart LR
  W0[W0 基线/围堵/租约] --> W1[W1 安全与配置]
  W1 --> W2[W2 合同与数据]
  W2 --> W3[W3 测试债清零]
  W3 --> W4[W4 资产与可运维性]
  W4 --> W5[W5 集成候选与 L3]
  W5 --> T1[T1 LOCAL_CLOSURE]
  T1 --> W6[W6 Android + macOS 设备门]
  W6 --> T2[T2 DEVICE_QUALIFICATION]
  T2 --> P[独立生产子计划]
  P --> T3[T3 仅登记各子计划终态]
```

## 1. 冻结输入

### 1.1 三仓基线

执行 W0 时必须重新采样；下表是计划编制基线，不得跳过运行时复核。

| 仓库 | 计划基线 HEAD | 主工作树状态 | 受保护资产 |
|---|---|---|---|
| `imboy` | `fa50dc60cde089640626542b0fa61e0f156b9c92` | `main` 干净 | `/private/tmp/intbe03-baseline` 外来 detached worktree，只读 |
| `imboyapp` | `6f179511e689744bc9ab3a2faacef988bdd0481a` | `main` 有已暂存 `macos/Podfile.lock` | 该 blob 和 index 状态逐位保护 |
| `imboyadmin` | `60a65af11259a0b5806dc82dec3eb3843d14fb7e` | `main` 干净 | 无本计划可写的 foreign WIP |

迁移编制基线为 `00000149`。迁移号 `150` 只能由 `CP-CON-02` 在运行时重新确认 head 仍为 149 后领取；否则标记 `BLOCKED_MIGRATION_DRIFT`，禁止猜测新编号。

### 1.2 计划绑定

- 本文件必须与同名 `.sha256` sidecar 一起强制加入 Git 跟踪。
- W0 校验计划 SHA、三仓 HEAD、主工作树 dirty、worktree/branch、迁移 head、进程/端口/DB/设备占用。
- 任一输入漂移时，只允许生成 `control/baseline-drift.md`；禁止继续执行旧 DAG。
- 允许的漂移只有用户明确确认的 foreign WIP，且必须记录所有权和保护指纹。

### 1.3 历史证据权威性

- 历史 `.Codex/runs/**` 只读，不能被本计划修改或作为当前 candidate 自动 PASS。
- 历史证据可用于复现步骤、已知 DDL、runbook 和失败样本。
- 每个 PASS 必须绑定本次 `RUN_ID + candidate SHA + command + exit code + evidence SHA-256`。

## 2. Git、并发和资源合同

### 2.1 目录与分支

设：

```text
RUN_ID=crossplan-v12-<UTC>-<8hex>
RUN_ROOT=/Users/leeyi/project/imboy.pub/.Codex/runs/$RUN_ID
WT_ROOT=/Users/leeyi/project/imboy.pub/.Codex/worktrees/$RUN_ID
```

规则：

1. A0 是唯一集成者，也是唯一能写 `control/`、冻结 candidate 和运行 L3 的角色。
2. 每个写卡在独立 worktree、独立分支执行；禁止 Worker 在三个主工作树直接写文件。
3. 分支格式：`run/$RUN_ID/<repo>/<card-id>`；集成分支：`run/$RUN_ID/<repo>/integration`。
4. Worker 只提交 `owned_paths`；Git author/committer 使用 `leeyi <leeyisoft@qq.com>`，仅命令级设置。
5. A0 在集成 worktree按 DAG 顺序 cherry-pick；冲突即 `BLOCKED_INTEGRATION_CONFLICT`，不得自行删除一侧语义。
6. 未获得单独确认前，不合并 `main`、不 push、不发布。
7. 任务 worktree 只在提交可达、证据冻结、状态干净且无租约后用 `git worktree remove` 和 `git branch -d` 清理；禁止 `-D`。

### 2.2 并发上限

- `MAX_ACTIVE_AGENTS=8`，A0 占 1 席，最多 7 个非协调器。
- 完成席位可复用；波次中的“可并行”不表示一次性全部启动。
- 同一 `owned_paths`、迁移目录、同一设备、共享 DB、同一 integration 分支均为排他租约。

### 2.3 禁入路径

全局禁入：

```text
imboy/erlang.mk
imboyapp/ios/**
imboyapp/macos/**
imboyapp/plugin/r_upgrade/**
/private/tmp/intbe03-baseline/**
.Codex/runs/**（除本 RUN_ROOT）
三个主工作树中的 foreign WIP
```

如工具在任务自有 worktree生成 `macos/Podfile.lock` 变化，设备卡立即停止，保存 diff 后把任务 worktree恢复到其已知基线；绝不触碰主工作树的已暂存文件。

### 2.4 越界处理

发现未租约路径变化时：

```text
STOP card
snapshot git status/diff
mark LEASE_VIOLATION
quarantine task worktree
notify A0
```

禁止自动 `reset`、`checkout`、`restore`、stash、clean、覆盖或删除未知改动。

### 2.5 资源租约

- PostgreSQL：默认只用 `imboy_cp12_<runid>_<card>` scratch DB，用后按精确库名 DROP。
- 共享 `imboy_test_v1` 默认只读；修复其账本必须另取用户授权并先克隆验证。
- 端口、容器、Erlang node、测试账号必须带 `RUN_ID` 前缀并登记 `control/leases.json`。
- Android、macOS、iOS 一次一张设备卡，设备租约串行。

## 3. 状态机、证据和测试阶梯

### 3.1 卡状态

```text
PENDING -> LEASED -> IN_PROGRESS -> EVIDENCE_SUBMITTED
        -> VERIFIED_PASS | VERIFIED_FAIL | BLOCKED_USER_ACTION
        | BLOCKED_EXTERNAL | BLOCKED_ENV | BLOCKED_DECISION
        | BLOCKED_PLAN_DRIFT | BLOCKED_INTEGRATION_CONFLICT
```

- Worker 每 10 分钟写 heartbeat；A0 每 5 分钟 reconcile。
- retry 默认 2 次；同一命令同一根因连续失败 3 次后停止，不得放宽 Acceptance。
- 会话恢复依据 append-only ledger、租约和 candidate SHA；不得靠聊天记忆翻 PASS。

### 3.2 Acceptance 注册表

`control/acceptance.tsv` 固定字段：

```text
card_id acceptance_id required repo cwd candidate_sha command expected status evidence_sha256 evidence_path note
```

- 每个 Required Acceptance 必须有 `PASS/BLOCKED/FAIL` 终态，禁止空值。
- `control/verifier.sh` 从 TSV 生成 `FINAL/RESULT.json`、`FINAL/acceptance-matrix.md` 和 `FINAL/final-report.md`。
- verifier 必须校验 evidence 文件存在、SHA 匹配、candidate 可达、重复 Acceptance ID 为零。

### 3.3 测试阶梯

| 层级 | 内容 | 执行者 |
|---|---|---|
| L0 | 格式、静态检查、编译、shell 语法 | Worker，每次提交前 |
| L1 | 单卡 focused red→green | Worker |
| L2 | scratch PG、真实 Cowboy HTTP、真实 Playwright 后端、设备专项 | 卡内 Owner |
| L3 | 三仓冻结候选全量门 | 仅 A0 |

L3 按 candidate 计数：同一 candidate 每仓最多一次。失败则该 candidate 作废；修复进入新 SHA 后允许新 candidate 再跑一次，禁止在同一 SHA 上无意义重试。

### 3.4 每卡必填字段

每张执行卡必须有：`repo`、`cwd`、`owner`、`owned_paths`、`forbidden_paths`、`depends`、`steps`、`red_test`、`focused_commands`、`Acceptance IDs`、`evidence`、`retry`、`timeout`、`stop_conditions`。缺一则 A0 不派卡。

为避免重复文字造成漂移，文档中的简写卡按以下默认值编译；显式字段覆盖默认值：

```yaml
forbidden_paths: 全局禁入路径 + 本卡 owned_paths 之外的所有路径
depends: 所在波次前一硬门 + 卡内显式依赖
red_test: 行为变更必须先有失败断言；纯文档/证据卡填写 NOT_APPLICABLE_WITH_REASON
evidence: $RUN_ROOT/evidence/<card-id>/
retry: 2
timeout: 180min
stop_conditions:
  - 受保护 WIP 指纹变化
  - 需要越过 owned_paths
  - 基线/迁移/合同发生未登记漂移
  - 测试只能通过 skip、吞错或放宽断言
```

CP-00-02 必须把每张待派卡编译为 `control/cards/<card-id>.yaml`。`control/verifier.sh --cards` 对上述 13 个字段逐卡判非空；缺字段、未知路径、重复 Owner 租约或不存在的命令任一出现时返回非零。Worker 只接收编译后的卡，不允许从 prose 自行补意图。

## 4. W0：绑定、安全围堵与决策门

### CP-00-01 基线和计划绑定

- `repo/cwd`：三仓分别执行；A0。
- `owned_paths`：仅 `RUN_ROOT/control/**`。
- `steps`：双采样三仓 HEAD/dirty/worktree/branch，间隔至少 60 秒；校验本计划 sidecar；登记 migration head、进程、端口、DB、设备和 foreign WIP 指纹。
- `CP-00-A01`：两次采样一致，或漂移已明确归属且冻结。
- `CP-00-A02`：计划 SHA 与 sidecar一致，三仓基线与 §1.1 一致。
- `stop`：任一不一致即 `BLOCKED_PLAN_DRIFT`。

### CP-00-02 隔离 worktree 和租约建立

- `repo/cwd`：umbrella；A0。
- `steps`：建立三仓 integration worktree、Worker 模板、`leases.json`、heartbeat、TSV 和 verifier 初版。
- `CP-00-A03`：主工作树指纹不变；integration 分支起点等于冻结 HEAD。
- `CP-00-A04`：故意写重复 Acceptance ID 时 verifier 必须非零；恢复后为零。

### CP-SEC-01 本地百炼密钥立即围堵

- `repo/cwd`：`imboy`；`/Users/leeyi/project/imboy.pub/imboy`。
- `owner`：W-SEC；`owned_paths`：本地忽略的 `config/sys.pro.config`、`config/sys.dev.config`。
- `forbidden_paths`：Git 历史、runtime/example config、任何第三方控制台。
- `depends`：CP-00-01；不等待其他产品决策。
- `steps`：将两文件的 Bailian `api_key` 替换为 `{env, <<"BAILIAN_API_KEY">>}`；不记录真实值。
- `red_test`：围堵前仅记录前缀命中数和指纹，不保存 secret。
- `focused_commands`：`make compile`；secret-prefix 扫描。
- `CP-SEC-A01`：真实 `sk-ws-` 值零命中，允许 example 注释描述。
- `CP-SEC-A02`：两泄漏指纹均不再能从当前配置提取。
- `CP-SEC-A03`：`make compile` exit 0。
- `evidence`：fingerprint-only；`retry=1`；`timeout=20min`。
- `stop`：发现新未知 secret 类型时扩大扫描但不外传值，标记安全事件。

### CP-DEC-01 有行为影响的决策冻结

只有以下三项需要用户在对应卡开工前确认，其他安全本地卡可继续：

| 决策 | 默认建议 | 阻塞卡 |
|---|---|---|
| `DEC-VISIT-TOKEN` | 冻结合同优先，伪造 token 返回 401 `visit_token_invalid` | CP-SEC-05 |
| `DEC-INT23-COMPAT` | internal v1 按冻结合同直接切 keyset，旧 page/size 返回 versioned 400 | CP-CON-02 |
| `DEC-SHARED-DB` | 不直接修共享库，先 scratch clone 证明；共享库写入另行授权 | CP-CLN-01 |

## 5. W1：安全与配置卫生

### CP-SEC-02 历史泄漏台账

- `repo/cwd`：imboy repo；W-SEC。
- `owned_paths`：`docs/security/KNOWN_LEAKED_KEYS.md`；禁止历史 rewrite。
- `depends`：CP-SEC-01。
- `steps`：只登记两个指纹、引入/删除 commit、当前轮换状态和 owner；不写 key 或专属端点全文。
- `CP-SEC-A04`：台账含两个指纹和处置状态。
- `CP-SEC-A05`：台账 secret regex 零命中。
- `focused_commands`：`rg` 指纹/secret 扫描；`retry=1`；`timeout=30min`。

### CP-SEC-03 删除 eturnal 死配置

- `repo/cwd`：imboy repo；W-SEC；仅本地忽略的 pro/dev config。
- `depends`：CP-SEC-01，同路径串行。
- `steps`：删除 `eturnal_secret`、`eturnal_turn_urls`、`eturnal_stun_urls`。
- `CP-SEC-A06`：pro/dev config 上述三键零命中。
- `CP-SEC-A07`：`make compile` exit 0。
- `focused_commands`：`make compile`；`timeout=30min`。

### CP-SEC-04 LiveKit ws_url 收敛

- `repo/cwd`：imboy repo；W-SEC；同 config 路径。
- `depends`：CP-SEC-03。
- `steps`：pro/dev `ws_url` 收敛为 `wss://rtc.imboy.pub`。
- `CP-SEC-A08`：pro/dev/runtime 三份有效值一致。
- `CP-SEC-A09`：`make eunit t=rtc_room_logic_tests` exit 0。

### CP-SEC-05 visit token 合同对齐

- `repo/cwd`：imboy repo；W-CS。
- `owned_paths`：widget handler及对应测试；禁止无关客服模块。
- `depends`：`DEC-VISIT-TOKEN`。
- `red_test`：伪造 token 当前返回非合同值的测试先红。
- `steps`：修为 401 + `visit_token_invalid`；保持不存在资源和无权限语义分离。
- `focused_commands`：分别运行 `make eunit t=cs_widget_handler_tests` 与 `make eunit t=cs_widget_public_frame_tests`，禁止重复 `t=` 写法。
- `CP-SEC-A10`：red→green 证据存在。
- `CP-SEC-A11`：两个 focused 命令分别 exit 0。
- `retry=2`；`timeout=90min`；合同歧义即停。

## 6. W2：功能合同与数据一致性

### CP-CON-01 INT-16/17 CURSOR-V2

- `repo/cwd`：imboy repo；W-INT。
- `owned_paths`：directory logic/handler、对应测试、OpenAPI internal bundle、Postman；cursor 公共库只读。
- `depends`：W1 完成。
- `steps`：复用现有 Cursor V2 HMAC/24h/family；旧 unsigned cursor 返回 400；同步 runtime/OpenAPI/Postman。
- `red_test`：valid、tampered、malformed、foreign-family、expired 五类先覆盖。
- `focused_commands`：三个套件分三条命令执行，再跑 `make contract-check`。
- `CP-CON-A01`：五类游标用例全绿，篡改 1 byte 返回 400。
- `CP-CON-A02`：identity-mappings/directory 正例回归绿。
- `CP-CON-A03`：31 ops/14 scopes 三向一致。
- `retry=2`；`timeout=240min`；公共 cursor 库需修改则拆卡。

### CP-CON-02 INT-23 keyset + migration 150

- `repo/cwd`：imboy repo；W-INT2。
- `owned_paths`：webhook handler/test、迁移 150 up/down、OpenAPI、Postman。
- `depends`：`DEC-INT23-COMPAT`、运行时 migration head=149。
- `steps`：落 `(organization_id, application_id, created_at DESC, delivery_id DESC)` partial index；切换 Cursor V2；page/size 返回 versioned 400。
- `red_test`：旧 offset 行为、五类游标、重复 created_at 稳定排序、边界翻页无重无漏。
- `CP-CON-A04`：migration up/down/up exit 0，账本和 schema 恢复一致。
- `CP-CON-A05`：EXPLAIN 命中新索引；before/after 入证据。
- `CP-CON-A06`：分页无重无漏；旧参数按决策返回 400。
- `CP-CON-A07`：`make contract-check && make migrations-check` exit 0。
- `retry=2`；`timeout=300min`；head 漂移立即阻塞。

### CP-CON-03 is_default 服务端真源

- `repo/cwd`：imboy + imboyadmin，各自独立 Worker 卡后由 A0 串接。
- `owned_paths`：组织投影和测试；Admin OrganizationDetailPage 和测试。
- `steps`：服务端投影 `is_default`；前端删除本地推导。
- `CP-CON-A08`：每组织恰好一个 default=true 的测试绿。
- `CP-CON-A09`：Admin 使用服务端字段，相关 bun unit/e2e 绿。
- `timeout=180min`。

### CP-CON-04 Widget 无坐席留言状态

- `repo/cwd`：imboyadmin repo；W-WGT。
- `owned_paths`：widget UI、i18n、对应测试。
- `steps`：queued 且无 online 坐席时显示可留言状态；不得阻断发消息。
- `CP-CON-A10`：有/无坐席两种状态 red→green 测试全绿。
- `CP-CON-A11`：`bun test src/widget/customer_service` exit 0。
- `timeout=90min`。

## 7. W3：测试债务清零

所有测试债卡只能修根因，禁止 skip、排除、吞错或放宽断言。

### CP-TD-01 后端全量 EUnit 清零

拆成顺序明确的子卡，每卡独立 worktree：

| 子卡 | 范围 | focused gate |
|---|---|---|
| `CP-TD-01A` | 两处 TSID register 补 `group_info, channel` | 两个组织套件分别 exit 0 |
| `CP-TD-01B` | billing_logic 7 红 | 对应套件 exit 0 |
| `CP-TD-01C` | msg_store_worker 9 红，先按日志确定真实文件 | 对应套件 exit 0 |
| `CP-TD-01D` | adm_plugin_handler 8 红 | 对应套件 exit 0 |
| `CP-TD-01E` | channel_logic_order 3 红 | 对应套件 exit 0 |
| `CP-TD-01F` | 3 个跨套件隔离失败 | 单套件与组合顺序均 exit 0 |

- 默认只改测试；根因在 `src` 时必须另立同 ID `-SRC` 子卡并更新租约。
- `CP-TD-A01`：所有子卡 red→green。
- `CP-TD-A02`：A0 在冻结 candidate 上运行全量 `make eunit`，exit 0、failed=0、无取消。

### CP-TD-02 Flutter route/env/analyze

- `repo/cwd`：imboyapp repo；W-TFE。
- `owned_paths`：route registry、test env、实际 analyze 落点；禁 macos/ios/plugin。
- `steps`：补 4 条 SmokeRoute；测试 API base 固定 loopback/合成域；清理全部 analyze issue。
- `CP-TD-A03`：route smoke 0 fail。
- `CP-TD-A04`：全量测试日志生产域名零命中。
- `CP-TD-A05`：`flutter analyze` exit 0、No issues found。

### CP-TD-03 DEV1 合同漂移评估与修复链

- `repo/cwd`：imboyapp + imboy scratch backend；W-DEV1。
- `steps`：逐项复现 D1/D2/D3a/D3c/D3d/D3e/D4/D5a，分类 `REAL_DEFECT/PROBE_STALE/BY_DESIGN`；每个 REAL_DEFECT 独立修复卡。
- 每个修复卡必须有 App API red→green、真实 scratch HTTP oracle、落库/响应形状断言。
- `CP-TD-A06`：8/8 有复现命令和分类。
- `CP-TD-A07`：所有 REAL_DEFECT 修复卡 VERIFIED_PASS。
- `CP-TD-A08`：探针不再把 404/405/422 当作成功旁路。

### CP-TD-04 Dialyzer、部署序列和 Admin build

- `CP-TD-04A`：`make dialyze` exit 0，警告 0。
- `CP-TD-04B`：`bash scripts/test/deploy_sequence_test.sh` exit 0，失败 0。
- `CP-TD-04C`：`bun run build` exit 0，`INEFFECTIVE_DYNAMIC_IMPORT` 零命中。
- 三卡分别独立 Owner/paths；禁止用 baseline accepted 代替修复。
- `CP-TD-A09/A10/A11`：分别绑定完整日志和 candidate SHA。

## 8. W4：资产、审计和可运维性

### CP-ASSET-01 客服迁移门脚本

- `repo/cwd`：imboy repo；W-DEP。
- `owned_paths`：`scripts/customer_service_migration_gate.sh`、Makefile 最小接线、测试。
- `steps`：复用现有迁移检查能力，不复制数据库解析逻辑。
- `CP-ASSET-A01`：空库、foreign sentinel、脏库、空 oracle 四负例均非零。
- `CP-ASSET-A02`：正常 scratch DB 正例 exit 0；`bash -n` 通过。

### CP-ASSET-02 hosted Widget 真实 E2E 入仓

- `repo/cwd`：imboyadmin repo；W-E2E。
- `owned_paths`：新 hosted spec/config/fixture/README；既有 mock spec 只读。
- `steps`：从历史 run 提取后脱敏并适配当前合同；禁止直接复制凭据或 PII。
- `CP-ASSET-A03`：真实四域本地环境 6/6 passed。
- `CP-ASSET-A04`：mock 与 real 两入口用途明确，CI 可选 job 可重复运行。

### CP-ASSET-03 prod compose CS overlay

- `repo/cwd`：imboy repo；W-DEP。
- `owned_paths`：新 overlay、CS Nginx template 注释、运维文档；本地 ignored prod compose 禁改。
- `CP-ASSET-A05`：`docker compose ... config` exit 0。
- `CP-ASSET-A06`：overlay 与 community CS 结构机械等价；文档含安装、升级、回滚、卸载。

### CP-ASSET-04 跨仓 Widget 资产配对门

- `repo/cwd`：imboy + imboyadmin；W-CI。
- `steps`：检查后端资产常量与 Admin manifest 输出名一致。
- `CP-ASSET-A07`：故意改单边时非零；恢复后一致时 exit 0。
- `CP-ASSET-A08`：两个仓库 CI 都调用同一合同数据或互验产物，避免复制常量成为双真源。

### CP-ASSET-05 八处治理写操作事务内审计

- `repo/cwd`：imboy repo；W-AUDIT。
- `steps`：逐处从弱 `audit/5` 迁入业务事务；审计失败必须回滚治理写。
- `CP-ASSET-A09`：8/8 调用点有处置表。
- `CP-ASSET-A10`：8/8 注入审计失败时业务写回滚。
- `CP-ASSET-A11`：弱审计函数无生产治理调用者。

### CP-CLN-01 共享测试库账本

- 默认只在共享库 dump 的 scratch clone 复现和验证修复 SQL。
- `CP-CLN-A01`：clone 前后 `schema_migrations` 与 history 一致，恢复演练成功。
- 对真实 `imboy_test_v1` 写入需要新的精确授权、`pg_stat_activity` owner 证明、备份校验和回滚窗口；没有授权保持 `BLOCKED_USER_ACTION`，不阻断其他本地功能卡。

### CP-CLN-02 badarg 根因与 crash dump 取证

- badarg：先构造载荷复现，再在共享入口修根因并留回归测试；不可达则以调用图证明后删除死代码。
- crash dump：9/9 仅取证、脱敏和分类；删除清单另行批准。
- `CP-CLN-A02`：badarg 有确定根因与 red→green，或有可复核不可达证明。
- `CP-CLN-A03`：crash dump 9/9 分类，证据 secret scan 零命中。

## 9. W5：集成候选与 T1 验收

### 9.1 集成顺序

1. 安全/config 卡。
2. 合同和迁移卡。
3. 后端测试债和审计卡。
4. Flutter 功能/测试卡。
5. Admin 功能/资产卡。
6. 跨仓 pairing 卡。

每批 cherry-pick 后运行受影响 L1；全部进入 integration 后冻结：

```text
IMBOY_CANDIDATE_SHA
IMBOYAPP_CANDIDATE_SHA
IMBOYADMIN_CANDIDATE_SHA
MIGRATION_HEAD
PLAN_SHA256
```

### 9.2 L3 唯一门

在明确 cwd 分别运行：

```bash
# /Users/leeyi/project/imboy.pub/.Codex/worktrees/$RUN_ID/imboy/integration
make compile
make eunit
make dialyze
make contract-check
make migrations-check

# .../imboyapp/integration
flutter test
flutter analyze

# .../imboyadmin/integration
bun test
bun run build
```

Required Acceptance：

- `CP-FINAL-A01`：后端五命令全部 exit 0，无 failed/cancelled/warning。
- `CP-FINAL-A02`：Flutter 全量测试 exit 0，analyze 0 issue。
- `CP-FINAL-A03`：Admin test/build exit 0，build warning 0。
- `CP-FINAL-A04`：31 ops/14 scopes/Cursor families/迁移 up-down-up 全部一致。
- `CP-FINAL-A05`：secret scan、diff-check、受保护 WIP 指纹全绿。
- `CP-FINAL-A06`：所有 Required T1 Acceptance PASS，证据 SHA 逐个匹配。

`T1_LOCAL_CLOSURE=PASS` 只在 A01-A06 全 PASS 时成立。主分支合并仍需单独用户确认；T1 PASS 不等于 push、发布或生产完成。

## 10. W6：T2 设备与 RTC 专项

设备测试只能绑定 W5 冻结 candidate，不得测试流动 main。

### CP-DQ-01 Android 企业/组织旅程

- Android `XWE6R19916004085`，13 步真实 UI 旅程连续两轮。
- `CP-DQ-A01`：两轮 EXIT=0；页面、UI 断言、后台 HTTP/SQL oracle、设备信息、完整日志齐全。

### CP-DQ-02 macOS 企业/组织旅程

- macOS App 同一 13 步旅程连续两轮。
- `CP-DQ-A02`：两轮 EXIT=0；主工作树 `macos/Podfile.lock` 前后 blob/index 指纹不变。

### CP-DQ-03 Android 客服工作台

- scratch backend + 合成账号；两轮完成登录、队列、claim、消息、presence、恢复。
- `CP-DQ-A03`：两轮 EXIT=0，关键写操作有数据库 oracle。

### CP-DQ-04 Android + macOS RTC relay

- 两端真实账号、真实页面、生产 `turn.imboy.pub:443`；E2EE 本轮仍不作为 RTC Gate。
- 串行采集同一 session ID 的客户端 selected-candidate、RTP、服务端 participant/track/allocation。
- 覆盖 Wi-Fi 和可用的移动/热点网络；不可用网络如实 BLOCKED，不可用截图替代。
- 负例：过期 token、篡改 token、relay 端口阻断、证书 hostname 错误全部 fail-closed。
- `CP-DQ-A04`：Android/macOS 都是 selected-candidate=relay。
- `CP-DQ-A05`：同一 session 的双向音视频 RTP 包/帧非零并与服务端 participant/track/allocation 对齐。
- `CP-DQ-A06`：网络矩阵和四项负例均有机械结果。
- `CP-DQ-A07`：挂断后 Room、allocation 和客户端资源释放。

### CP-DQ-05 iOS best-effort

- 仅在 USB、证书和用户时间允许时执行，最多两次。
- 只记录 `IOS_EXECUTED_PASS`、`IOS_EXECUTED_FAIL` 或 `IOS_NOT_EXECUTED`，不参与 T2 PASS。

所有 Required 设备卡每轮必须留：设备列表、完整日志、关键帧/录屏、UI 断言、后台 oracle、candidate SHA。仅 `All tests passed` 不构成 PASS。

`T2_DEVICE_QUALIFICATION=PASS` 当且仅当 CP-DQ-A01 至 A07 全 PASS；iOS 状态单列。

## 11. T3：独立生产子计划入口

每个子计划必须单独建 Markdown、单独 SHA 绑定、单独确认串，并包含 preflight、canary、rollback、观测窗口、post-verifier、停止条件。未经确认不得启动。

### PP-SEC-ROTATE

- 用户在百炼控制台吊销两把泄漏 key并重签；Agent 不代替用户操作第三方账号。
- 新 key 仅进入 secret/env；完成前保持 `SECURITY_INCIDENT_OPEN`。
- 外部 LLM 冒烟可能产生费用，必须再次单独授权。

### PP-TURN-CERT

- 执行 Let’s Encrypt staging renewal dry-run、证书 hook、HAProxy/LiveKit reload 和失败回滚。
- 这是 `PP-LK-REMOVE` 的硬前置，不是并列可选项。

### PP-CS-WIDGET-PROD / PP-EADM-09 / PP-GHCR / PP-EXT

- 分别处理客服 Widget 上线、企业生产初始化、镜像公开发布、JPush/OA/白标/客户验收。
- 目标域名、IP、账号、联系方式和第三方输入必须由用户在子计划中重新确认，不能继承旧聊天值。

### PP-LK-REMOVE 旧 TURN 分阶段退出

开工前所有硬门必须 PASS：

1. `T2_DEVICE_QUALIFICATION=PASS`，尤其 CP-DQ-A04 至 A07。
2. `PP-TURN-CERT=PASS`。
3. 最低支持客户端版本由用户书面确认，并有版本覆盖/兼容说明。
4. 生产 Linux/端口/image/owner 原子基线完成。
5. 生产故障注入 -> 回滚 -> 再切换演练 PASS。
6. embedded TURN canary 和旧 TURN 停止观察窗口 PASS。
7. 第二次精确确认串：`CONFIRM_REMOVE_LEGACY_TURN_<RUN_ID>`。

执行阶段：

```text
R0 只读预检、备份、恢复包验证
R1 embedded TURN canary，旧服务保留但不承载新流量
R2 Android+macOS relay/RTP/服务端对齐复验
R3 观察窗口和日志/端口/连接统计
R4 停止并禁用旧服务，继续观察
R5 再次确认后卸载软件、删除精确配置和端口规则
R6 residual scan + 真机 post-verifier + 回滚能力证明
```

任一步失败立即回滚到上一稳定阶段；不得把“已停止”写成“已删除”，不得把 TURN Allocate、HTTP 200 或客户端单侧 relay 当作删除门 PASS。

## 12. 分步波次与功能先后

| 波次 | 内容 | 可并行范围 | 退出条件 |
|---|---|---|---|
| W0 | 基线、sidecar、worktree、立即密钥围堵 | CP-00 与 CP-SEC-01 在租约建立后串接 | CP-00-A01..A04、CP-SEC-A01..A03 |
| W1 | 安全/config/visit token | config 三卡串行；token 卡可并行 | CP-SEC 全 PASS |
| W2 | Cursor、keyset、is_default、Widget 文案 | 最多 4 卡，迁移独占 | CP-CON 全 PASS |
| W3 | 后端/App/Admin 测试债与 DEV1 | 最多 7 Worker，按仓错峰 | CP-TD 全 PASS |
| W4 | 迁移脚本、E2E、overlay、pairing、审计、取证 | 同路径串行 | CP-ASSET/CLN 全 PASS 或明确用户阻塞 |
| W5 | A0 集成、候选冻结、L3 | 串行 | T1 PASS |
| W6 | Android、macOS、RTC、可选 iOS | 设备租约串行 | T2 PASS |
| W7+ | 独立生产子计划 | 一卡一授权 | 各子计划独立终态 |

## 13. 失败、恢复与最终报告

- 任一安全、数据完整性、受保护 WIP、计划漂移问题出现时全线停止。
- 单卡功能失败只停止依赖链；无依赖卡可继续，但不得绕过失败卡进入冻结门。
- 每波次生成 checkpoint：输入 SHA、提交、命令、exit、证据路径、剩余阻塞。
- 最终报告必须分别输出：

```text
T1_LOCAL_CLOSURE
T2_DEVICE_QUALIFICATION
各 T3 子计划状态
PUSH
DEPLOY
PRODUCTION_WRITE
LEGACY_TURN_REMOVAL
```

禁止用 T1/T2 PASS 推导发布；禁止用历史报告、代码存在、容器健康、HTTP 200、mock E2E、skip 或单张截图推导生产完成。

## 14. 计划质量自检

派工前 A0 必须机械确认：

- [ ] 本计划与 sidecar已跟踪且 SHA 一致。
- [ ] 三仓 HEAD/dirty/worktree 双采样一致。
- [ ] 每张派发卡 13 个必填字段齐全。
- [ ] Acceptance ID 全局唯一、Required 非空。
- [ ] 所有命令都有明确 cwd；不存在重复 `t=`。
- [ ] A0 + Worker 不超过 8 席。
- [ ] 所有写卡使用隔离 worktree；main 无直接写。
- [ ] foreign WIP 指纹已登记且 verifier 会校验不变。
- [ ] 生产/第三方/删除动作均在独立子计划且有确认串。
- [ ] `PP-LK-REMOVE` 依赖证书、RTC、回滚、兼容和第二次授权全绿。

只有以上十项全勾选，计划才可从 `READY_FOR_LOCAL_W0_EXECUTION` 提升为 `READY_FOR_LOCAL_EXECUTION`，进入 W1 及后续波次。
