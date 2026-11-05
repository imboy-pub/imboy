# 广州交付 V3.1 — Codex/ZCODE共用接续提示词 / Portable resume prompt

请接续 `/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-23-guangzhou-delivery-v3/plan-v3.1.md`，并读取其引用的原V3。

先完整读取该计划、baseline.json、requirements.json、requirements-v3.1.json、evidence-index.json、sources/timezone-review-provided.md 和各仓 AGENTS.md/CLAUDE.md。默认先恢复现场；若我仅要求完善计划，不解除USER_PAUSED、不开始实施。只有我明确要求“接管并执行”，才恢复本run的已授权执行；不能自动恢复其他专项。

本提示词适用于本机Codex或ZCODE。ZCODE按实际可用性选择用户希望的GLM-5.3/GLM-5.3-Flash，核对真实模型ID，不假设套餐可用性；模型选择不产生业务供应商付费调用授权。普通文件、Git、本机Python/Bun/Flutter/Erlang脚本是接续接口，不依赖聊天上下文、functions cell/session ID或Codex专属MCP。

唯一运行根 `/Users/leeyi/.codex/imboy-delivery-v3/`。三个工作树已成功创建：`worktrees/imboy`、`worktrees/imboyapp`、`worktrees/imboyadmin`。复用它们，不新建run/worktree，不从变动后的main静默切候选，不复制外来WIP。已提供时区审查按TZ01–TZ13逐项核当前代码，建议不自动成为产品决定；链接会话ADD-CONVERSATION-001正文/附件未提供，仍BLOCKED_SOURCE/NOT_MERGED。停止搜索additional-conversation.md和尝试读取该链接，不把时区文本冒充该会话全文。

当前边界：允许本机隔离树内已证实缺陷修复、针对性测试、本任务独占合成资源；用户后续已授权最多10个agent；实际并发取当前宿主容量与10的较小值（本宿主含主控共4个）。主控唯一写state/events、统一数据库写入和重型构建/全量槽；子agent明确owned_paths，不能改外来WIP或自行启动重操作。该授权不使既往同轮审查自动成为独立终审。不提交、不合并、不push、不部署、不外发通知、不调用付费供应商；不改erlang.mk、App ios/*、macos/*、plugin/r_upgrade，不动生产/共享数据。后续仅用户明确修订才能扩展，不从旧文档或模型切换推导权限。

1. 只读查 `/Users/leeyi/.codex/imboy-delivery-v3/control/state.json`。不存在时说明尚未启动，不自行生成PASS或从头覆盖历史；获执行授权后按V01/V02首次准入。不要恢复已删除的 `.codex-runs`、`.codex/task-runs`，不要重跑旧checkpoint的NEXT_UNLOCK。
2. 已存在时核plan/source hash、state_version、run_id/epoch、候选三仓SHA/tree/WIP、真实资源和控制器锁。读事件尾部与最近有效receipt；不扫描/打印全部历史大JSON，不输出Key。
3. 按V02.HANDOFF先停止旧控制器接新任务、清点在途命令、记录释放和handoff_id，再原子接管epoch。旧进程仍活跃且未明确释放只能只读，不凭旧锁或额度不足kill它。现有`control/tools/state_store.py`已实现release组件及定向守卫测试，但实际Codex/ZCODE往返尚未验证，也不宣称完整防双写；先复用并核验组件，不重造交接工具，不能手改JSON假装已通过。中断任务先对账进程/PG/供应商事实，未知副作用保留RECONCILE_REQUIRED，不自动重复提交。USER_PAUSED只有用户明确接管执行才解除；`claim(resume_paused=True)`仍保留暂停状态，须在该授权下另用新claim返回的fence调用独立`Store.transact(..., fence=新fence)`，记录授权依据并恢复本run为ACTIVE，不恢复其他专项。
执行入口现要求显式身份和副作用范围：接管成功后，将 `claim` 返回的 `{run_id, controller}` 写入本run内新epoch专属0600 fence文件；每次 `run_check.py` 在位置参数之前传 `--controller-fence-file 该文件 --effect-scope process-only|owned-resource|external`（或对应 `IMBOY_CONTROLLER_FENCE_FILE` / `IMBOY_CHECK_EFFECT_SCOPE` 环境变量）。不得直接借用活跃旧控制器的fence。身份载入不等于恶意同用户进程隔离；超时/异常后进程组未证实退出，或owned/external结果未知时保持待对账。SIGTERM/KeyboardInterrupt完整收尾尚未证明，遗留RUNNING须先对账，不能据此直接释放。13项组件守卫通过不代替真实跨工具往返。

4. 先处理NEXT_UNLOCK最小条件，再按依赖与独占资源执行已授权READY卡。旧失败/预算/费用保留；没有来源或无法核验的Required记BLOCKED，不删叶。外部阻断不妨碍独立准备。
5. 修改仅限本卡明确owned_paths，保护main及其他会话WIP；复用既有能力。实现步骤定向检查，最终候选统一适用全量；真实PG/对象/浏览器/Android真机/macOS/供应商证据不得用mock代替。
6. 每次结果持久记录command/cwd/安全环境指纹、候选、资源、原始exit/count/skip、日志hash、具体oracle。候选变化按影响闭包失效；不沿用旧分支绿灯。
7. 会话结束前写最小checkpoint：已做/未做、当前命令与进程、资源归属、累计费用/预算、当前叶状态、精确NEXT_UNLOCK。区分广州技术、面聊技术、UAT、发布和生产，不写“全完成”掩盖未验收项。

8. 先核已有三份receipt，不因计划升级或模型切换重跑：状态工具10项、迁移文件223组、Moya独占PG8项曾exit0，只接纳原scope。源码/依赖/环境/断言变化按影响闭包失效。供应商禁止只阻断真实ASR/GLM及依赖叶，不阻断CRUD/录音/BYOK/共享准备。
9. 开发期定向验证；稳定候选统一原合同后端两轮全EUnit及排除套件、App/Admin/Widget/适用CI全门，Required skip/cancel/0执行/缺exit/候选不符不得PASS。真机、供应商、连续24h、独审与UAT不以mock/短测替代；Flash与非Flash均守同一质量线。
10. 复用SDK和依赖，每仓一树；按本机实测余量安排重型构建/全量/故障/24h互斥。不每attempt复制环境，不删外来数据/共享缓存/唯一证据。模型额度不足时准备持久交接，不标COMPLETE、不清零预算、不让两个工具同时写。

我希望减少人工介入，持续做完已解锁且已授权工作。只有无法从原合同/现场确认且会影响权限、费用、数据、外部动作的决策才询问。提交/合并/push/部署不是本提示词自动授权的动作；新授权记录到state后按授权边界执行。

V01续接注意：读取 requirements-v3.1.json 的 inherited_sets 与 inheritance_rules；新映射默认未验收，52个EWS协调ID不是验收叶。保留未展开的叙述子场景和历史runner重绑缺口，不用近期组件测试批量关闭要求；恢复时只读已绑定源，不导入外来WIP。


【2026-10-24候选变更】先读取plan-v3.1.md末尾main同步补充与scratch/main-sync-20261024-r1/commits.json，以及最新state.candidate。Backend迁移228替代本run旧227；main227保持收藏E2EE。App生产SQLite=v34，历史v32测试保留。旧硬编码commit/profile/runtime manifest只作历史证据，不直接执行。先核当前commit/tree/owned WIP、按影响重绑重验；预算和失败不清零。用户已授权本会话两仓提交与本地双向main合并，未授权push/部署/生产迁移。
