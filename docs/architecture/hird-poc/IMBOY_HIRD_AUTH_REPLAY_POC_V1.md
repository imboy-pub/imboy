# IMBOY_HIRD_AUTH_REPLAY_POC_V1 — 计划与结果

> 任务：验证现有 Hirð 集成在补齐「真实授权接线 + 真正离线 Replay」后，是否给
> IMBoy 带来值得承担其维护成本的增量价值。
>
> 状态图例：PENDING / RUNNING / PASS / FAIL / BLOCKED
> 本文档同一文件内先冻结计划（§2），执行后在 §4 回填结果。

## 1. 实际被测基线（复核实证）

| 项 | 值 | 证据 |
|---|---|---|
| imboy HEAD | `0a5ade3cd8e8c005a1cc22991633dedd81af0445`（main）| agent/hird 相关文件在主工作树全部干净（未提交修改集中于 cache-v2.2.1 任务，与本任务无交集）|
| 任务 worktree | `.worktrees/hird-auth-replay-poc-v1`（分支 `hird/auth-replay-poc-v1`，自上述 HEAD 创建）| 不含主工作树未提交修改；被测源码=HEAD 提交内容 |
| hird HEAD | `2bb12f35a8209915849b5e103b44349c5c2a5595`（main，v0.4.0，工作树干净）| 相邻仓库只读 |
| OTP/ERTS | OTP 29 / ERTS 17.0.2（`/opt/homebrew/opt/erlang`）| P0 以测试 VM 实测为准 |
| Rust | rustc/cargo 1.97.1（满足 hird MSRV 1.97）| 编译器重建用 |

### 复核确认的关键事实（P0/P1/P2 设计输入）

- **F1（P0 阻塞实证）**：`agent_hird_mini_spike_tests` 的 `?OUT_ROOT` 硬编码指向
  旧 worktree `.worktrees/track-agent-v31-20260916/.../spike_ag31_10a`，该目录
  残留 9/28–10/9 的旧 BEAM；`ensure_built/1` 见 BEAM 即复用（不重新编译）。
  PATH 上无 `hird`（`which hird` 失败），不设 `IMBOY_HIRD_BIN` 时 build 分支
  静默失败 → 套件实际跑的是「恰好存在的旧 BEAM」。
- **F2（runtime 接口实锚）**：本地 hird HEAD 的 runtime 导出
  `hird_tool_dispatch:call/4`（ToolName, Caller, Handlers, Args）；handler 协议
  `fun(Args, Handlers) -> Result`，域失败 = `throw {hird_exn, Error}`（ADR-016：
  audit 记 `{err,...}` 后原栈 rethrow）。
- **F3（宿主拦截通道）**：codegen（emit.rs）注释+编译实证——actor 回调体内
  tool 调用「run against no in-scope handler map, fall back to the runtime
  registry」，生成 `hird_tool_dispatch:call(Tool, Caller, #{}, Args)`；resolve
  顺序 = threaded map `{tool,Name}` → `hird_handlers` 注册表 → miss 崩溃。
  因此**宿主经 `hird_handlers:with_handlers/2` 作用域安装桥 handler，即可拦截
  生成程序 actor 实际发出的业务 tool 调用**（main 内词法 handle 块绑定被编译
  为硬编码 threaded map，宿主不可替换——业务调用必须放在 actor 消息处理内）。
- **F4（replay 真源）**：`hird_replay:start_link/2(LogPath, Tables)` 为严格顺序
  cursor：active 期间**一切 dispatch 路由到 cursor，不查 map/registry**
  （handler 不执行）；`offer/2` 匹配 tool+args（全等）则返回记录结果（`{err,E}`
  以原 `{hird_exn,E}` throw 回放）；`finish/0` 校验日志全消费。加载失败
  `{replay_load_error,…}`。
- **F5（伪 replay 现状）**：`imboy_hird_replay` 是「重复执行 + 字符串截取归一化
  + 排序行 hash 比对」，G16/G17/A18 以此冒充 replay——P2 需修正命名并建真
  cursor replay。
- **F6（授权链真源）**：`agent_tool_authorizer:authorize/3` 十步 fail-closed，
  需要 epgsql conn + 真实表（agent_run/agent_effect/agent_grant/
  agent_grant_capability/agent_grant_workspace + org facts：organization/
  organization_member/workspace/workspace_member）。cross_org 拒绝在步骤 1
  （`resource.organization_id ≠ run.organization_id`）；Grant explicit ws scope
  下无 workspace 的资源调用一律拒（scope_in_grant）。catalog 默认空集、
  resource_policy 默认恒 deny、hitl 默认 readonly→no_approval——三者均为
  app env seam 模块（`agent_capability_catalog_module` /
  `agent_resource_policy_module` / `agent_hitl_policy_module`），PoC 经 env 显式
  启用单工具目录/策略，生产默认不变。
- **F7（业务工具选型）**：`workspace_repo:internal_find_tx(Conn, OrgId, WsId)`
  —显式 conn 只读 SELECT、Org 边界 + status='active' + IDOR 防存在性泄露、
  返回最小投影（id/name/owner_id/version/created_at）。选择理由：最简单稳定、
  有明确 Organization 资源边界、复用既有 repo（不为 PoC 新建业务模型）。
- **F8（隔离库供给）**：`inttest_marker_db:provision/1`——每次运行新建一次性
  `inttest_<prefix>_<微秒>_<rand>` 库 → 12 扩展 → `erlang_migrate:up`（strict）
  → release 时 DROP DATABASE；无 skip 语义。安全证明 = 一次性随机命名库 +
  全新迁移 + 用后即删。`make eunit-local` 起全量 app 连本地 imboy_v1——
  **不作为本任务隔离验证环境**。
- **F9（既有套件数量基线）**：mini_spike 6 + gates 14 + acceptance 3 = 23 用例
  （对应历史 23/23 PASS；gates/acceptance 全部 meck mock PG/Grant/Membership/
  Catalog 面）。
- **F10（同工具递归防护机制）**：dispatch resolve 是 threaded map 优先——桥
  handler 内 `imboy_hird:run_tool/5` 传入的宿主 HandlerMap 含同名 tool 键时，
  内层 dispatch 命中 map 中业务实现（不再落注册表）→ 无递归；同时以
  depth-guard 显式防护。

## 2. 冻结计划（FACT → GAP → CHANGE → TEST → PASS_CRITERIA → EVIDENCE）

### P0 — 重建可复现构建基线 〔RUNNING〕

- FACT：F1/F2；编译器真源 = hird HEAD `2bb12f35`。
- GAP：套件依赖旧 worktree 旧 BEAM；PATH 无 hird；编译器二进制与源码的来源
  对应未证明；runtime 导出接口无实锚记录；产物加载路径无记录。
- CHANGE：
  1. worktree 内新增任务运行脚本 `scripts/hird_poc_v1/`（编译器指纹+套件重跑）；
  2. 修 `agent_hird_mini_spike_tests.erl` 的 `?OUT_ROOT` 硬编码 →
     `os:getenv("IMBOY_HIRD_OUT_ROOT", "_build/hird_poc_v1/spike")`；
  3. 三套件 `ensure_built` 增加 `IMBOY_HIRD_FORCE_REBUILD=1` 强制重建路径
     （清 out 目录后重编），保留原幂等语义为默认；
  4. 从 hird HEAD 源码以独立 `CARGO_TARGET_DIR`（任务证据目录内）+
     `--locked` 重建 release 编译器，记录二进制 SHA-256。
- TEST：重建编译器 → 三套件强制重跑（新 OUT_ROOT、专用 VM、记录
  `code:which` 实际加载路径 + 全部 hird_*.beam SHA-256）。
- PASS_CRITERIA：编译器可从源码重建且指纹记录；三套件在全新输出目录通过，
  用例数与 F9 基线一致（6/14/3），0 skip 0 cancel；加载路径全部指向本轮
  输出目录。
- EVIDENCE：`_build/hird_poc_v1/logs/p0_*.log` + 指纹清单 JSON。

### P1 — Hirð 生成程序 → 真实授权 → 只读业务 Tool 〔PENDING〕

- FACT：F3/F6/F7/F10。
- GAP：无「生成程序 actor 实际发起业务调用 → 授权器 → 真实业务读取」闭环
  （gates G18/G19 的 E2E 是宿主直调 run_tool 且全 meck）。
- CHANGE：
  1. 新增 `src/features/agent/infrastructure/imboy_hird/imboy_hird_host_bridge.erl`：
     `with_handlers` 作用域内运行生成程序；桥 handler 绑定可信宿主解析的
     RunCtx（run/agent/org/grant 身份不来自 tool 参数）；deny/approval 不触
     dispatch；depth-guard 防同工具递归；readonly 工具 deny 经
     `throw {hird_exn, {tool_denied, Reason}}` 以域错误回传；
  2. 测试侧 PoC 目录/策略/原生注册表模块（env 显式启用单工具
     `workspace.detail/read/workspace`，未注册工具与未覆盖资源继续拒）；
  3. 新增隔离集成套件 `test/features/agent/integration/imboy_hird_p1_auth_tool_tests.erl`
     （marker DB + 真 epgsql conn + 生成程序 actor + 注册表桥 + atomics
     业务执行计数器贯穿 worker 进程）；
  4. PoC .hird 程序（actor Runner：ModelChat 模拟 Provider + WsDetail 业务）。
- TEST（必测）：合法请求（真实 fixture 数据返回、业务执行恰 1 次）；无授权
  （拒、执行 0 次）；Grant 撤销提交完成后（拒、执行 0 次）；跨 org 资源
  （cross_org 拒、不泄露）；伪造/缺失执行上下文、未知工具（拒）；绕过标准
  入口直调注册表业务 handler（仍需授权或拒）；同工具递归防护；默认关闭
  （不配 env 时 catalog 空 + policy 恒拒 + 桥未装 → 程序调用崩为 unhandled）。
- PASS_CRITERIA：上述全部按预期，计数来自 atomics（非进程字典），audit
  caller 带 run scope。
- EVIDENCE：`_build/hird_poc_v1/logs/p1_*.log` + audit JSONL + 计数快照。

### P2 — 真正的有限顺序轨迹离线 Replay 〔PENDING〕

- FACT：F4/F5。
- GAP：无真 cursor replay 验证；现有 G16/G17/A18 命名冒充 replay。
- CHANGE：
  1. 新增测试侧 cursor replay 编排（录制=P1 授权链真实执行+audit sync 落盘
     校验；回放=`hird_replay:start_link` + 程序重跑 + `finish` 全消费校验）；
  2. 「调用即失败」断言：模拟 Provider 查表函数与业务 handler 内 atomics——
     replay 期间任何真实执行尝试即 error（覆盖 dispatch worker 派生进程）；
  3. JSON 逐行结构化解析（jsone），归一化仅限时间戳字段；调用事件保持顺序、
     不排序数组、不忽略参数；
  4. 修正 `imboy_hird_replay` moduledoc 与 G16/G17/A18 用例命名/说明
     （明确为「确定性重执行一致性」，不再冒充 replay；保留有效语义）；
  5. replay 失败路径不回退 live：cursor 崩溃/divergence 断言。
- TEST（必测）：正常轨迹回放成功且真实执行尝试=0；参数偏移/次序偏移/工具名
  偏移/日志不足/多余记录/格式损坏 → 明确失败；已记录领域错误按原语义回放
  （录制含 deny 轨迹：{err} 行 + actor 崩溃语义复现）；replay 无法启动 →
  不退回真实执行。
- PASS_CRITERIA：全部按预期；录制/回放 audit 结构化对比一致（除时间戳）；
  finish=ok。
- EVIDENCE：`_build/hird_poc_v1/logs/p2_*.log` + 两轮 audit JSONL + 对比结果。

### P3 — 现有 Erlang 路径最小收益对照 〔PENDING〕

- FACT：F6/F7；同一业务工具/授权器/fixture 可不经 hird 直跑。
- GAP：无同口径对照记录。
- CHANGE：新增对照用例（Erlang 原生：直调 authorize + 业务 handler，同
  fixture 同输入）+ 对照记录表（新增 Tool 修改面、参数/结果结构变更同步点、
  未声明副作用变更的编译期发现、参数/顺序变化的发现阶段、是否需改 hird
  编译器、构建/加载/测试/排错新增环节、关闭 Hirð 后授权/数据合同可复用性）。
- TEST：对照用例通过（证明 Erlang 路径等价可达）。
- PASS_CRITERIA：对照表每项有实测或代码位置证据，无臆测性能倍率。
- EVIDENCE：对照用例日志 + §4 对照表。

### 约束（全程）

- 不修改 hird 仓任何文件（含 target/——编译器重建用独立 CARGO_TARGET_DIR
  于任务证据目录）；不 commit/push；仅清理本任务产物。
- 本轮零真实模型调用、零真实用户数据；PG 仅一次性 marker 库 + 合成 fixture。
- 不放宽任何默认拒绝；PoC 启用面全部经测试内显式 env。

## 3. 测试与验证环境

- 专用 VM：`erl -pa <worktree>/ebin -pa <worktree>/test -pa deps/*/ebin …`
  （不起 imboy app，不加载 sys.local——与 AG31 门同口径）。
- DB：`inttest_marker_db:provision(#{env_prefix => <<"HIRDPOC">>})`，
  env `HIRDPOC_PG_HOST/PORT/USER/PASSWORD[/MAINT_DB]` 由运行脚本从
  `config/sys.local.config` 提取注入（不回显敏感值）。
- 全量 EUnit：全部改动完成后按仓库门禁跑一次（如被无关基线阻塞则如实记录）。

## 4. 结果回填（2026-10-13 执行）

### 4.1 阶段状态

| 阶段 | 状态 | 数量 | 关键证据 |
|---|---|---|---|
| P0 基线 | PASS | 23/23（6+14+3）| 编译器自 hird HEAD 源码重建（`--locked`，SHA-256 `66628dd2…`）；强制重建+全新输出目录；`code:which` 实测全部产物加载自本轮目录；F1 硬编码旧 worktree 缺陷修复 |
| P1 授权闭环 | PASS | 10/10 | 零 meck：生成 actor 实际 dispatch → 注册表桥 → 十步授权（marker 库真 PG）→ `workspace_repo:internal_find_tx`；allow 恰一次业务读取+authorized effect 落库；无授权/撤销后/跨 org/伪造 Run/未知 capability 全拒且业务执行 0 次；绕过直调被 effect 凭证拦截；`run_tool` 递归防护生效；默认关闭用例证生产面未放开 |
| P2 离线 Replay | PASS | 8/8 | 真 `hird_replay` cursor 回放 P1 授权链录制日志：正常轨迹全消费且 Provider/业务 trap+atomics 证明零真实执行；参数偏移 args_mismatch、日志删行 log_exhausted、多余记录 finish=incomplete、损坏行 replay_load_error 带行号、deny 轨迹 `{err}` 按原语义回放、cursor 无法启动不退回 live。历史 G16/G17/A18 命名修正为「重复执行一致性」 |
| P3 对照 | PASS | 3/3 | 同一工具/授权器/fixture/输入：路径 A（原生直调）与路径 B（Hirð 桥）业务结果逐字节一致、deny 语义等价（grant_revoked）、两条路径授权事实同落 agent_effect |

最终回归：六套件 44/44 全绿（`logs/*.20261013T102454Z.log`）。

### 4.2 全量门禁口径

- 仓库门禁 `make eunit-local` 在任务 worktree **环境阻塞**：worktree deps 为只读
  symlink（erlang.mk deps 目标会改写主树 deps 的 `.d`；cowboy 旧 `.d` 指向已删除的
  `/private/tmp/imboy_base2`），重建 deps 必然写主树违反隔离。
- 已执行次级门禁：全部 1033 个 `test/**/*.erl` 编译级检查 → 1021 通过；12 个
  `imboy_cache_*` 测试因基线缺少 `imboy_cache_sync_state.hrl`（主树 cache-v2.2.1
  任务未提交文件）失败——与本任务无关、HEAD 固有。
- 变更模块全部调用方（gates/acceptance/P1–P3/spike）实跑全绿。

### 4.3 P3 对照表（实测 + 代码位置）

| 维度 | A：Erlang 原生路径 | B：Hirð 路径 | 证据 |
|---|---|---|---|
| 新增一个 Tool 的修改面 | 注册表条目 + 调用方 descriptor（同语言 2 处）| `.hird` tool 声明 + handle/install 绑定 + 宿主注册表桥条目 + resource_of/biz 适配（跨语言 4 处）| P1/P3 源码 |
| 参数/结果结构变更同步点 | handler 与调用方（同语言手工同步；无跨模块签名互检证据）| `.hird` 签名 + 宿主 resource_of/biz + audit 形状断言；audit 为键序敏感类型化 JSONL（jsone 重编码即不兼容，P2 实测）| P2 mutate_log |
| 未声明副作用变更 | 测试/审查发现 | **编译期拒绝**（C0030：效果行不匹配即编译失败，P1 开发实测）| `hird` 编译器 |
| 参数/顺序变化的发现阶段 | 运行时/测试 | B：编译期签名检查（C0034）+ 回放 divergence（args_mismatch 实测）| P2 |
| 是否需改 Hirð 编译器 | — | 本轮零改动（全部桥接在宿主侧；hird 仓工作树干净）| git status |
| 新增维护环节 | —（原生路径零新增） | 实测新增：编译器重建与指纹管理、产物构建/加载路径（旧 BEAM 复用陷阱实测存在于**文件层与 code server 层两处**，F1 与 CLOSEOUT）、audit 格式跨语言兼容、注册表/audit 节点全局态的 VM 级隔离约束 | P0/P2/CLOSEOUT |
| 关闭 Hirð 后可复用性 | — | 授权器、Grant/Membership port、业务 repo、registry 合同、effect 账本全部独立可用（路径 A 同 fixture 同结果实测）| P3 |

**有证据的增量收益**：效果行静态检查（编译期拒绝未处理/未声明效果）；类型化 audit +
严格顺序 cursor 回放——**调用轨迹差异检测**：参数/工具名/调用次序/行数/JSON 格式/
schema 版本偏移均确定性失败（CLOSEOUT 补齐运行时变体与次序漂移实证；注意这是轨迹
一致性检测，录制文件本身无签名、无防篡改能力）；声明式 actor/supervisor
生成（P0 spike）；宿主可控的业务调用唯一通道（注册表桥，P1 实测 deny 零接触）。
**推测收益（本轮无证据）**：真实 LLM 循环中的调试价值；多 Agent fleet 生产编排价值；
工具链对 Agent 程序开发体验的改善。**实测成本**：跨语言同步点、编译器工具链、格式
脆弱性、全局态串行化约束。

### 4.4 未证明能力与真实模型条件

未证明：真实模型端到端链路；生产 AgentRun 并发；撤销回收已开始调用的能力。
三项保证分别陈述（CLOSEOUT 修正，不再混称）：**防篡改——不存在**：audit 为普通
JSONL 文件，无签名/完整性保护，回放只检测调用轨迹差异；**自动脱敏——不存在**：
脱敏是调用方纪律（只喂合成/低敏数据），runtime 无脱敏机制；**持久化**——宿主
管理的 append-only 文件（audit sink 由 boot 序列启停），仅此而已。

「进入真实模型验证只差一处 Provider 替换」经 CLOSEOUT **证伪**（C5）：当前 Hirð
程序为**固定业务流程**（无条件顺序调用 model_chat → ws_detail，模型输出不参与
工具决策——Provider 变体实测：纯文本回复下 WsDetail 仍被固定执行，
`imboy_hird_p1_auth_tool_tests:t_provider_variant`）。真实模型接入还需要：程序层
工具决策循环（模型输出解析 → 工具请求 → 结果回送 → 再调用）、终止条件与循环
上限、错误返回路径；`imboy_llm_registry`（`chat/3` 合同）只是 provider 调用面的
替换点之一。

### 4.5 采用建议：HOLD

结论与依据见最终报告（对话内交付）。要点：关键前提（真实授权接线 + 真离线回放）
本轮全部补齐并通过，但增量收益相对既有 Erlang 路径仍集中在「编译期效果检查 + 类型化
回放调试」两点，其真实模型场景价值未测；维护成本（跨语言、工具链、全局态隔离）已有
实测。既无证据支持 CONTINUE_POC 的扩量，也无证据支持 STOP_EXPANSION 的否决——
下一轮建议以有限真实模型验证（CONTINUE_POC 的前置）再裁决。

### 4.6 CLOSEOUT（2026-10-13 第二轮：IMBOY_HIRD_AUTH_REPLAY_POC_V1_CLOSEOUT）

#### 4.6.1 基线变更与一处 P0 误判修正

- worktree 自 0a5ade3c **fast-forward 至 main 头部 dba3ab89**（分支无自有提交，纯
  fast-forward）：main 领先 12 提交（cache v2.2.1 收尾、customer-service 含新迁移
  `00000172_customer_service_visitor_read`、TSID 修复等），与本任务未提交修改零重叠；
  迁移编号 00000171→00000172 严格连续，无冲突无重排。
- **修正 P0 结论一处**：全量 test 编译检查中 12 个 `imboy_cache_*` 失败的根因是任务
  脚本 `compile.sh` 的 `-I` 缺 `src/lib`（`imboy_cache_sync_state.hrl` 为**已提交**
  文件，基线提交 8ee8f899），非「主树未提交 hrl 的基线固有」；修复后合并基线全量
  编译检查 **1054/1054 零失败**（含 main 新增 cache 测试）。

#### 4.6.2 C1：派发与审计/回放对应关系（源码 + 原始 JSONL）

实际结构**只有一次 dispatcher**：生成 actor 回调体 → `hird_tool_dispatch:call`
（codegen 唯一发射点）→ 注册表桥 `bridge_handler` → `authorize` → biz（registry
handler 直调，不再过 dispatcher）。`imboy_hird:run_tool/5` 是宿主程序化入口
（authorize → dispatch_with_timeout → 同一 dispatcher），非嵌套第二层。
一次业务操作 = 一次 dispatch = **恰一条** hird audit 记录（原始 JSONL 样本：
ModelChat `{ok}` + WsDetail `{ok,原值}` 两行，caller=`ProbeRunner.handle_msg/ProbeRun`）；
授权决策另行落 `agent_effect`（PG），与 JSONL 是两个审计面，无重复记录。
回放对应：`replayed/3` 中 cursor 是唯一权威（handler/registry 全不查），每次
dispatch 恰 offer 一次、不匹配不前移；**finish 行数守恒 ⟺ 回放 dispatch 序列与
录制逐点全等**——证明的是实际消费路径而非仅结果相同。无丢弃/过滤（负向用例用的
变形日志只存在于负向用例，正向回放直接复用录制文件）；deny 以 `{err}→{hird_exn}`
原语义回放（t_err_semantics 两轮 audit 一致）。

#### 4.6.3 C2：门禁十项矩阵（全部有执行证据）

| # | 要求 | 执行证据 |
|---|---|---|
| 1 | 正常回放全消费 + 零执行 | P2 `t_replay_ok`（finish ok + atomics 零增长 + 两轮 audit 一致）|
| 2 | 参数漂移 | P2 `t_args_mismatch`（args_mismatch、游标不前移）|
| 3 | 次序漂移（≥2 可区分调用）| offline `t_call_order_divergence`（逆序程序变体：首个调用即 tool_mismatch、consumed=0）|
| 4 | 工具名漂移 | offline `t_runtime_tool_mismatch`（同签名别名变体：**加载放行、调用匹配期** tool_mismatch）+ P2 `t_tool_mismatch`（签名不兼容**加载期**拒绝）|
| 5 | 日志不足 | P2 `t_log_exhausted`（log_exhausted）|
| 6 | 多余尾部记录 | P2 `t_extra_records`（finish replay_incomplete，不静默截尾）|
| 7 | 格式/类型/版本不兼容 | P2 `t_corrupt_line`（decode_error 带行号）+ offline `t_schema_version_gate`（`unsupported_schema_version` 加载期拒绝，hird_types.erl:222-225）|
| 8 | 领域错误原语义回放 | P2 `t_err_semantics`（`{err}`→`{hird_exn}` throw，两轮 audit 一致）|
| 9 | 启动失败不进真实执行 | P2 `t_start_failure`/`t_corrupt_line`（active=false + 计数零增长）|
| 10 | 运行中 cursor 退出 | offline `t_cursor_death`（monitor 确定性触发 DOWN + 回放护栏在资源解析/**授权之前**拦截 + atomics 证明零真实执行）|

**C2#10 缺陷与宿主侧修复**：`hird_replay:active/0` 是 `whereis` 检查——cursor 进程
死亡后 dispatch **立即回退 live** 并查询注册表（hird runtime 语义；本轮不改 hird
源码）。修复：`imboy_hird_host_bridge` 新增**回放护栏**
`arm_replay_guard/0`/`disarm_replay_guard/0`（persistent_term），`bridge_handler`
在资源解析与授权之前检查——护栏激活期一律拒绝（deny 语义域错误）；回放编排 cursor
存活期 arm、出口 disarm、cleanup 兜底。默认未装填，live 录制路径零影响。
构建面附带修复：生成 actor/supervisor 模块名跨程序变体**同名**（派生自 actor 名），
code server 会缓存先加载版本遮蔽本轮产物——`ensure_built`/`ensure_variant_built`
构建后按输出目录 purge（P0 F1「旧 BEAM 复用陷阱」的 code server 层补全）。

#### 4.6.4 C3：权限证据与安全声明范围

已有执行证据映射（20261013T110108Z 日志）：合法读取 `t_legal` / 无授权 `t_not_granted`
/ 撤销后新请求 `t_revoked`（真实 `revoke_cas_tx`）/ 跨组织 `t_cross_org`（含目标
数据不泄露断言）/ 伪造上下文 `t_forged_ctx` / 未知工具 `t_unknown_tool` / 绕过入口
`t_bypass_direct` + `t_bypass_dispatch` / 默认关闭 `t_default_off`。
本轮补强：effect 凭证原为 `{EffectId, RunId}` **存在性**校验——凭证窗口内跨资源
放行缺陷；修复为三元组 `{EffectId, RunId, 资源键}`，凭证与请求的
organization_id + workspace_id 全等才放行（`imboy_hird_poc_registry`）；证据
offline `t_effect_credential_binding`（无凭证/跨 org 跨 ws/同 org 跨 ws 全拒 +
trap 边界信号 + 清凭证恢复拒绝）。
范围声明：本 PoC 证明的是**受控接入路径**（生成程序唯一业务通道 = 注册表桥 +
fail-closed 授权 + 凭证资源绑定 + 回放护栏），不是任意 Erlang 代码的安全沙箱，
也不证明未来所有 Hirð 程序天然不可绕过——绕过面评估属独立安全审计。原生与 Hirð
共享的授权器/Grant/Membership port/effect 账本是 **IMBoy 公共能力**（P3 已证两
路径同 fixture 同结果），非 Hirð 独占收益。

#### 4.6.5 C4：数据库与证据可复核性

- 上一轮事实（源码核实）：`run_pg_suite.sh` 旧版 eval `emit_pg_env.py` 从
  sys.local.config `{pg_conf…}` 片段提取五元组输出 export（密码只进 env）；marker
  库 `inttest_<prefix>_<μs>_<rand>` 一次性建库 → 12 扩展 → `erlang_migrate:up`
  strict → release 时 DROP（reuse 引用计数）。
- 本轮口径变更：`run_pg_suite.sh` 改为**显式 PoC 测试连接**（脚本内显式声明、env
  可覆盖），不再解析 sys.local.config；`emit_pg_env.py` 仅存为上一轮行为证据。
- 资源台账：探针库 `hirdpoc_closeout_probe_*`（5432 trust 实例）建成即 DROP；
  marker 尝试库（5432，pg_jieba 缺失 provision 失败**自动清理**）；一次性容器
  `hirdpoc-closeout-pg`（项目镜像 initdb 不支持全新初始化）失败即删。终检：5432
  无 `inttest_hirdpoc%`/`hirdpoc%` 残留、无 hirdpoc 容器、本轮日志无连接串/密码。
- **BLOCKED**：P1/P2/P3 marker 库端到端复验需获准的功能完备实例——5432（trust
  可连、可建库）缺 pg_jieba 等扩展；4323/4330（imboy/pg18 容器，扩展齐全）凭据在
  「不解析日常配置」约束下不可获得。证据：`logs/imboy_hird_p1_auth_tool_tests.
  20261013T112550Z.log`（provision 扩展失败）。P1 新增 3 用例
  （`t_same_org_other_ws` / `t_effect_resource_binding` / `t_provider_variant`）
  已实现，待该实例可用后执行（共享授权器 `grant_scope_mismatch` 拒绝码见
  `agent_tool_decision.erl:376`）。

#### 4.6.6 C5：真实模型接入前提（仅静态与离线，零真实调用）

当前数据流：`main` → supervise(ProbeSup) → actor ProbeRun →
`model_chat({prompt:"plan"})`（脚本化 Provider 查表）→ **无条件**
`ws_detail({ws_id:912001})` → 注册表桥授权链 → 业务读取 → audit。模型返回值不参与
任何决策。检查项结论：模型不请求工具时不执行业务 Tool——**不成立**（固定流程，
`t_provider_variant` 离线取证）；模型请求工具走完整授权链——N/A（无工具决策机制；
但「被调用的工具必过授权链」成立）；未知工具/非法参数/越权资源——授权链拒绝面
成立（P1 实证）；最终输出接收业务结果——成立（audit `{ok,原值}` + ProbeStatus）；
终止/循环上限/错误返回——当前无循环（单轮 ProbeRun→ProbeDone→Shutdown），域错误
经 `Exn<AgentToolError>` 传播（P1/P2 实证）。
**如实标注**：当前是固定业务流程 + 一次模型调用点缀，不是模型驱动的工具循环；
不为「Agent」名称补写完整循环（留给下一轮真实模型验证）。`imboy_llm_registry`
（`lookup/1` → `chat/3` behaviour，主树 `src/lib/imboy_llm_*.erl`）只是 provider
调用面的替换点之一。

#### 4.6.7 测试执行台账、修改清单与候选指纹

| 套件 | 结果 | 日志（logs/）|
|---|---|---|
| imboy_hird_replay_offline_tests（新，零 PG）| **5/5** | `*.20261013T114843Z.log` |
| imboy_hird_gates_tests | 14/14 | `*.20261013T113625Z.log` |
| imboy_hird_acceptance_tests | 3/3 | `*.20261013T113625Z.log` |
| agent_hird_mini_spike_tests | 6/6 | `*.20261013T113625Z.log` |
| P1/P2/P3（PG marker 库）| **BLOCKED** | provision 失败日志 `*.20261013T112550Z.log` |
| 全量 test 编译检查 | 1054/1054 | compile.sh 输出 fails=0 |

修改清单（本轮 CLOSEOUT）：`imboy_hird_host_bridge.erl`（回放护栏）；
`imboy_hird_poc_registry.erl`（凭证三元组资源绑定）；`imboy_hird_p1_auth_tool_tests.erl`
（凭证三元组、显式 pg_conf、purge、Provider 覆盖、+3 用例）；
`imboy_hird_p2_replay_tests.erl`（编排护栏化、断言通用化）；
新增 `imboy_hird_replay_offline_tests.erl`；`scripts/hird_poc_v1/`（compile.sh
`-I src/lib` 修复与离线套件入列、run_pg_suite.sh 显式连接、run_suites.sh 空数组
修复）。候选完整指纹：`_build/hird_poc_v1/closeout_fingerprints.json`
（base dba3ab89；5 tracked + 14 untracked 文件 SHA-256；编译器 `66628dd2…` 与 P0
同源；hird HEAD 2bb12f3、runtime 9 源、产物 52 项；OTP 29）。

#### 4.6.8 最终裁决（CLOSEOUT）

- **PoC 后续：CONTINUE_POC**。C1~C5 核查完成：C2 十项全部有执行证据，两项宿主侧
  缺陷（回放护栏缺失、凭证粒度不足）已修复并验证；既有有证据收益（编译期效果行
  检查、类型化回放轨迹检测）继续成立。下一轮 = 程序层工具决策循环（C5 实测缺口）
  + 有限真实模型验证（需独立授权与预算），不要求先完成生产能力。
- **生产采用：HOLD（本轮不批准）**。护栏/凭证绑定是 PoC 级实现（节点全局
  persistent_term、专用 VM 串行约束），生产并发模型未验证；合并后基线的 PG 端到端
  复验 BLOCKED；且真实模型场景价值仍无证据。
- **真实模型执行：NOT_AUTHORIZED（本轮未执行）**。本轮零真实调用、零密钥读取、
  零对外请求；后续真实模型测试需明确的独立授权与预算。
