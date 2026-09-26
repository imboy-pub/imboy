# V1.2 跨计划收口无人值守 ZCODE 执行提示词 V1

<task>
你是本 run 的 A0 协调器。请在 `/Users/leeyi/project/imboy.pub` 执行：

`imboy/docs/plans/2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2.md`

目标不是快速制造绿色状态，而是按计划把功能合同、测试债、可运维资产、Android+macOS 设备证据和 RTC relay 证据分波次闭环，形成可机械复核的 T1/T2 终态；T3 生产/第三方项目只登记独立阻塞，不在本 run 自动执行。

不要等待用户在线，不要为日常技术选择提问。对允许范围采用下文默认裁决持续推进；对未授权外部动作直接记录 `BLOCKED_*`，继续全部不依赖卡，最后统一报告。
</task>

<default_follow_through_policy>
默认采用最合理的低风险解释并持续推进，不为常规实现细节停下来提问。只有缺失信息会导致数据损坏、secret 泄漏、foreign WIP 被覆盖或未授权生产动作时，才停止对应依赖链；其他独立卡继续执行。
</default_follow_through_policy>

<grounding_rules>
所有完成声明必须来自当前工具输出、candidate SHA和本 RUN_ROOT 证据。历史报告只作复现输入。事实、推断和未知必须分开；不得猜测生产状态、设备状态、账号凭据、测试结果或第三方状态。
</grounding_rules>

<missing_context_gating>
缺仓库事实时先用只读工具采样；缺凭据、设备、生产权限或外部输入时记录精确 BLOCKED 和解除键，不编造默认值、不索取或输出真实 secret、不阻塞无依赖卡。
</missing_context_gating>

<action_safety>
变更严格限制在计划卡的 owned_paths。不要做无关重构、重命名、依赖升级、格式化整仓或元数据清理。任何写入前确认真实 Git 根、任务 worktree和租约；任何删除前确认 RUN_ID 所有权和可恢复性。
</action_safety>

<plan_binding>
计划绝对路径：
`/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-09-27-cross-plan-closure-techdebt-and-production-readiness-v1.2.md`

计划期望 SHA-256：
`e26e0b29fe6accf3e6a6bf4b7b30d5c53ce5ff07ded3144d6cdaab776c47d918`

sidecar：同名 `.sha256`。

业务代码基线：
- imboy: `fa50dc60cde089640626542b0fa61e0f156b9c92`
- imboyapp: `6f179511e689744bc9ab3a2faacef988bdd0481a`
- imboyadmin: `60a65af11259a0b5806dc82dec3eb3843d14fb7e`

启动后先按计划 W0 双采样。`imboy` 当前 HEAD 可在业务代码基线之后包含本 V1.2、sidecar和本提示词文档提交；必须证明 `fa50dc60..HEAD` 的业务路径 diff 为空。imboyapp/imboyadmin 必须满足计划 §1.1。任何其他 HEAD/dirty/迁移/worktree 漂移标记 `BLOCKED_PLAN_DRIFT`，不得把旧证据强行套到新基线。
</plan_binding>

<authorization_boundary>
本提示词授权以下无人值守本地动作：
- 读取三仓、计划和历史 run 证据。
- 创建 RUN_ROOT、隔离 worktree、任务分支、integration 分支和 run-scoped scratch DB/容器/端口/测试账号。
- 修改计划明确 owned_paths 内的本地代码、测试、脚本和文档。
- 按卡 red→green，使用固定 Git 身份创建任务本地提交，并由 A0 集成到 run-scoped integration 分支。
- 在已连接且可用的 Android 真机和本机 macOS App 上运行计划定义的测试；只使用合成/run-scoped 测试账号和数据。
- 对 IMBoy 自有生产入口做正常客户端级瞬时 RTC 连接和只读网络观测，但不得修改生产配置、服务、证书、数据库或流量路由。
- 精确清理本 RUN_ID 创建的 worktree、分支、scratch DB、容器、端口、进程和合成账号。

本提示词不授权以下动作；不要询问，直接登记对应阻塞并继续：
- push、PR、发布、上架、公开镜像、对外通知或消息。
- 合并或直接写三个 `main`；最终只交付 integration candidate 和本地任务提交。
- 任何生产 SSH 写操作、部署、服务重启、证书 renewal dry-run、故障注入或数据库写入。
- 第三方控制台操作、百炼 key 吊销/重签、JPush/OA/白标/客户验收。
- 写共享 `imboy_test_v1`；只允许对其 dump 的 scratch clone 验证。
- 删除 crash dump、旧 eturnal/coturn、配置、端口规则、备份或 recovery assets。
- 改写 Git 历史、强删分支、reset/clean/stash/checkout/restore 未知 WIP。
</authorization_boundary>

<unattended_defaults>
无需用户回填，采用以下本 run 固定裁决：
- `DEC-VISIT-TOKEN=FIX_401_VISIT_TOKEN_INVALID`
- `DEC-INT23-COMPAT=DIRECT_CURSOR_V2_PAGE_SIZE_RETURNS_VERSIONED_400`
- `DEC-SHARED-DB=CLONE_ONLY_NO_SHARED_WRITE`
- Git 历史不重写；泄漏凭据只做本地围堵和 fingerprint 台账，第三方轮换记 `BLOCKED_USER_ACTION`。
- Dialyzer、EUnit、deploy_sequence、Flutter analyze、Admin build均按严格清零，不接受 baseline waiver。
- 八处弱审计全部迁移事务内语义，不接受注释豁免。
- crash dump只取证、脱敏、分类，不删除。
- iOS best-effort；Android+macOS required。

不得因为无人值守而降低验收、伪造授权或把 BLOCKED 翻成 PASS。
</unattended_defaults>

<team_and_concurrency>
你是 A0，计入并发席位。`MAX_ACTIVE_AGENTS=min(8, 当前环境可用席位)`；非协调器最多为 `MAX_ACTIVE_AGENTS-1`。

按以下 Owner 切分，禁止重复 ownership：
- W-SEC：安全台账和 Git 跟踪配置；ignored pro/dev config 例外由 A0-SEC-LOCAL 亲自执行。
- W-CS：visit token 合同。
- W-INT/W-INT2：Cursor 16/17 和 INT-23 migration 150，迁移目录单租约。
- W-ADM-BE/W-WGT：is_default 和 Widget 文案。
- W-TBE：后端测试债，按 01A..01F 子卡分派。
- W-TFE/W-DEV1：Flutter route/env/analyze 和 DEV1 链。
- W-DEP/W-E2E/W-CI/W-AUDIT：迁移门、真实 E2E、overlay、pairing、事务审计。
- W-DEVICE：Android、macOS、RTC 卡；同一设备一次一卡。

每个 Git 写卡必须有独立 worktree/分支。A0 是唯一 integration 分支写入者、唯一 L3 执行者和唯一 `control/` 写入者。不要让 Worker 同时写共享主工作树或共享 index。
</team_and_concurrency>

<run_bootstrap>
生成：

```text
RUN_ID=crossplan-v12-<UTC>-<8hex>
RUN_ROOT=/Users/leeyi/project/imboy.pub/.Codex/runs/$RUN_ID
WT_ROOT=/Users/leeyi/project/imboy.pub/.Codex/worktrees/$RUN_ID
```

必须先创建：
- `control/acceptance.tsv`
- `control/ledger.tsv`
- `control/leases.json`
- `control/cards/`
- `control/heartbeats/`
- `control/verifier.sh`
- `checkpoints/`
- `FINAL/`

先执行 W0：计划/sidecar、三仓 HEAD/dirty/worktree双采样、migration head、foreign WIP 指纹、资源租约。建立三仓 integration worktree后，再派发 Git 写卡。

`CP-SEC-01/03/04` 是唯一主工作树 ignored-config 例外：由 A0 在 `/Users/leeyi/project/imboy.pub/imboy/config/sys.pro.config` 和 `sys.dev.config` 精确执行；只保存前后指纹，不打印 secret，不 stage/commit ignored 文件，不碰其他主工作树路径。
</run_bootstrap>

<card_compilation_contract>
派卡前，把计划中的每张简写卡编译成 `control/cards/<card-id>.yaml`，必须包含：

```text
repo
cwd
owner
owned_paths
forbidden_paths
depends
steps
red_test
focused_commands
acceptance_ids
evidence
retry
timeout
stop_conditions
```

运行 `control/verifier.sh --cards`：字段缺失、命令不存在、路径不存在且非声明新建、Acceptance ID 重复、租约相交任一情况必须非零。卡未通过编译不得派发。

每卡生命周期：
`PENDING -> LEASED -> IN_PROGRESS -> EVIDENCE_SUBMITTED -> VERIFIED_PASS|VERIFIED_FAIL|BLOCKED_*`。

Worker 每 10 分钟 heartbeat。失败最多按卡 retry budget 重试；同根因连续 3 次停止该卡，禁止放宽 Acceptance。
</card_compilation_contract>

<execution_waves>
严格依赖、动态补位执行；波次内只有 owned_paths、DB、设备和迁移租约不相交的卡才能并行。

W0：基线、sidecar、worktree/租约、A0 ignored-config 密钥围堵。

W1：安全台账、eturnal 死配置、LiveKit ws_url、visit token 401 合同。config 三卡串行；visit token可并行。

W2：
- CP-CON-01 INT-16/17 Cursor V2
- CP-CON-02 INT-23 keyset + migration 150
- CP-CON-03 is_default 后端/前端真源
- CP-CON-04 Widget 无坐席留言状态

W3：
- CP-TD-01A..01F 后端 EUnit 根因清零
- CP-TD-02 Flutter route/env/analyze
- CP-TD-03 DEV1 8 项评估及按 REAL_DEFECT 生成的修复链
- CP-TD-04A/B/C Dialyzer、deploy_sequence、Admin build

W4：迁移门、hosted Widget真实 E2E、prod compose overlay、跨仓 pairing、八处事务审计、共享库 clone 验证、badarg 根因、crash dump 只读取证。

W5：A0 按计划顺序集成；每批后跑 L1；冻结三仓 candidate；每个 candidate 每仓只跑一次 L3。L3 失败则 candidate 作废，根因修复进入新 SHA 后再跑。

W6：只有 T1 candidate 产生后才开始：
- Android 企业/组织两轮
- macOS 企业/组织两轮
- Android 客服工作台两轮
- Android+macOS RTC relay 同会话证据
- iOS仅设备已连接且无需用户动作时 best-effort

若生产 RTC 只读/瞬时测试无法在不触碰生产配置的情况下取得服务端 participant/track/allocation、RTP 或负例证据，对相应 Acceptance 记 `BLOCKED_PRODUCTION_AUTH`，不得伪造；继续其他设备卡。
</execution_waves>

<implementation_rules>
- 先复现或写 red test，再改实现，再跑 focused green。
- 修根因，不通过 skip、exclude、catch-and-ignore、放宽断言或 mock 替代真实 oracle。
- 优先复用仓库已有 Cursor V2、迁移检查、审计事务和 E2E 模式；不新增无必要依赖或抽象。
- EUnit 多套件必须分别执行，禁止在同一次 Make 调用中重复赋值测试选择变量。
- 所有命令记录 cwd、完整命令、开始/结束时间、exit code和日志 SHA。
- Worker 只 stage owned_paths；提交前 `git diff --check` 和 L0/L1 通过。
- Git 身份命令级固定为 `leeyi <leeyisoft@qq.com>`。
- A0 cherry-pick前比较 base/candidate diff；发现越界立即隔离，不自动回滚未知改动。
- 不修改 `erlang.mk`、iOS/macOS保留区、r_upgrade、foreign worktree或其他 run 目录。
</implementation_rules>

<verification_loop>
每张卡：
1. 验证 red 真实命中目标缺陷。
2. 验证实现只改 owned_paths。
3. 运行 L0 和所有 focused L1。
4. 需要 DB/HTTP/浏览器/设备的卡运行 L2 真实 oracle。
5. A0 校验 evidence SHA、candidate SHA、Acceptance结果后才翻 `VERIFIED_PASS`。

W5 冻结候选必须运行：

```bash
# imboy integration cwd
make compile
make eunit
make dialyze
make contract-check
make migrations-check

# imboyapp integration cwd
flutter test
flutter analyze

# imboyadmin integration cwd
bun test
bun run build
```

任何非零、failed、cancelled、warning或 analyze issue按计划记失败；不得用“相对基线无新增”替代严格门。

设备 PASS必须同时有设备列表、完整日志、真实页面证据、UI断言、后台 HTTP/SQL oracle和冻结 candidate SHA。RTC 还必须同 session 对齐 selected-candidate=relay、双向 RTP、participant/track/allocation、网络矩阵、四类 fail-closed 负例和释放证据。
</verification_loop>

<no_human_intervention_policy>
不要因为以下情况暂停等待用户：
- 设备未连接或需要物理信任：记 `BLOCKED_DEVICE`。
- Docker/PG/四域环境不可用且安全自修两次失败：记 `BLOCKED_ENV`。
- 第三方、生产写、证书、key 轮换、共享库写、push/发布/删除需要授权：记准确 `BLOCKED_USER_ACTION` 或 `BLOCKED_EXTERNAL`。
- iOS不可用：记 `IOS_NOT_EXECUTED`，不阻塞 T2。
- 单卡阻塞：继续全部不依赖卡。

只有以下情况终止整条 run：
- 计划 SHA/业务基线发生未解释漂移。
- foreign WIP 或受保护文件指纹变化。
- 可能发生数据丢失、secret 外泄或生产写入。
- 租约无法确定所有权。
- 同一根因导致基础 compile/build 连续 3 次失败且无法隔离。

终止时仍必须生成 checkpoint和 FINAL 报告，不得只在聊天里说“卡住了”。
</no_human_intervention_policy>

<cleanup_contract>
只清理本 RUN_ID 明确拥有的资源。清理前验证精确名称、租约、工作树状态和提交可达性。

- scratch DB/container/port/process/test account：删除并证明零残留。
- Worker worktree：提交已进入 integration、证据冻结、状态干净后删除。
- 分支：只用 `git branch -d`；删不掉就保留并报告，禁止 `-D`。
- integration worktree/branch和 RUN_ROOT默认保留用于复核。
- foreign worktree、主工作树 WIP、crash dump、生产备份和旧 TURN资产原样保护。
</cleanup_contract>

<structured_output_contract>
持续把完整证据写入 RUN_ROOT。最终聊天答复只给高信号摘要和可点击路径，必须包含：

1. RUN_ID、计划 SHA、三仓启动 SHA和冻结 candidate SHA。
2. Required Acceptance 总数、PASS/BLOCKED/FAIL/PENDING计数。
3. T1、T2 和每个 T3 子计划的独立状态。
4. 每波次完成卡、提交、测试命令和真实计数。
5. 所有 BLOCKED 按 LOCAL_ENV、DEVICE、USER_ACTION、EXTERNAL、PRODUCTION_AUTH 分类，并给精确解除条件。
6. 明确写出 `MAIN_MERGE`、`PUSH`、`DEPLOY`、`PRODUCTION_WRITE`、`LEGACY_TURN_REMOVAL` 的真实状态。
7. foreign WIP保护结果和 run-scoped 清理结果。
8. `FINAL/RESULT.json`、`FINAL/acceptance-matrix.md`、`FINAL/final-report.md`、`FINAL/cleanup.txt` 的路径。

禁止把历史 PASS、代码存在、容器健康、HTTP 200、TURN Allocate、单侧 relay、mock/skip或任务卡自报成功写成当前 PASS。
</structured_output_contract>

<completeness_contract>
从 W0 持续执行到所有可授权本地/设备卡都有终态，并完成 integration、L3、设备门、证据冻结和清理。不要在完成第一批修复后停止，不要把剩余卡留成无说明 PENDING。

无人值守不等于所有项必须 PASS：无法合法执行的外部动作必须诚实 BLOCKED；完成的定义是“所有 Required Acceptance 均有可复核终态，所有获授权范围尽可能闭环，所有未授权范围有精确解除键”。
</completeness_contract>

<progress_updates>
只在 W0 完成、每个波次切换、候选冻结、设备门完成和最终报告生成时输出简短进度。不要为每个命令刷屏，不要请求用户确认。
</progress_updates>
