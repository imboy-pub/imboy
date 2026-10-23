# 同一份续接提示词 / Reusable execution prompt

> 2026-10-23 收拢说明：本轮执行已按用户要求停止。本文保留设计与历史任务约束，不作为恢复旧 RUN_ROOT 的指令；待环境清理后另行整理广州 V3。源码合并不等于功能验收通过，真机闭环、真实模型质量及最终专项证据仍待补齐。


修订10候选（2026-10-23）：**DRAFT_NOT_ACCEPTED**。用户已批准豆包 ASR 2.0 + 智谱 `glm-5.3-flash`；当前方案以 [DOUBAO-API.md](DOUBAO-API.md) 为异步增量合同、[design-preview-doubao.html](design-preview-doubao.html) 为候选交互原型。本文不是代码、真实供应商、真机或最终验收证明。仓外执行状态仍属修订9；主控制器审查、保留旧快照并显式迁移前，不得刷新旧证据为PASS。

用户已批准使用公开或合成样本进行真实 API 验收，本轮预算目标不超过人民币5元；Key仅在当前账号本设备填写并加密保存，不进IMBoy服务端、日志、云备份或设备迁移。此授权不扩展至真实用户录音、无限费用或部署；智谱年度Max不默认覆盖通用API额度。当前文档修改不执行任何调用。

以下正文供用户在执行会话中完整发送；文件本身不授权当前设计会话开始开发。会话中断后原样再次发送即可。需要仍可访问同一磁盘，或先恢复任务文件、工作树与证据；不会自动跨机器同步。

---

请执行或继续 IMBoy「面聊记录 talk_record V1」计划。

修订10候选的文件布局：设计目录以当前仓库检出路径为准，以下个人绝对路径仅帮助定位既有运行，不是跨机器默认值。真实 `execution-state.json`、锁、日志、秘密和执行证据放仓外 RUN_ROOT；从已有控制器/运行凭据找回其准确位置。使用 `python3 <设计目录>/validate_plan.py --directory <设计目录> --state <RUN_ROOT>/execution-state.json` 校验，不因仓内缺少台账重新初始化。仓内 `fixture-execution-state.json` 仅供测试，不是当前进度或授权。旧修订证据保持原绑定；迁移到修订10须保留快照并重新核验受影响任务，不重置台账或把旧证据提升为BYOK验收。

我确认按计划的首版范围、个人/企业默认权限和V4设计方向进入本地实施；允许先完成契约、原型布局及可行性门槛，再分步骤开发和测试。允许委派子agent，最多8个worker，并按环境槽位降并发；允许创建和复用本任务独立worktree。不授权commit、push、合并main、发布部署、生产变更、超出已批准公开或合成样本及5元预算目标的外部处理服务、联系或影响第三方，以上需另行明确授权。保护其他会话改动，不清理、重置、暂存或提交他人WIP。模型选型、外部服务、保留平台文件例外若待确认，先做独立可做项，再提出具体选项，不自行决定。

工作区 /Users/leeyi/project/imboy.pub 不是git仓库。
权威目录 /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-10-20-face-to-face-notes/
读取适用AGENTS.md/CLAUDE.md和该目录：PLAN.md、DESIGN.md、UI-SPEC.md、design-preview.html、design-preview-doubao.html、DOUBAO-API.md、BACKEND-API.md、仓外execution-state.json、plan-manifest.json、执行校验合同execution-contract.json。台账不是完成证明，实际源码与有效证据优先。

先恢复审计，不重新规划：
1. 核对两仓HEAD/分支/WIP及归属，任务worktree、owner、活跃worker、进程、设备锁和证据。写文件前确认git归属。
2. 按PLAN.md获取唯一主控制器锁。另一个控制器或worker活跃则不重复派发；心跳超时不能直接强制接管。
3. 将可信用户授权来源、基线及计划hash写入台账；在 /Users/leeyi/.codex/task-runs/talk-record-v1/<run_id>/ 保存状态/设计备份和受控源码恢复快照。恢复必须核对来源与hash，不覆盖其他会话改动。
4. 核验accepted任务的代码和证据绑定，失效标needs_recheck。确认旧执行者退出后才能回收running/claimed，保留原分支/WIP。不从T01重做已验证工作，也不凭历史总结跳过任务。

一个主集成者调度，worker最多min(8,实际可用容量)，主控制器占槽时预留。不得递归扩队或新建用户可见会话绕过容量。按PLAN.md的25任务DAG与八条工作流派发；每个worker独占明确任务/路径，知道其他人也在代码库中，不还原他人修改。同路径、迁移编号及执行、全局路由/生成物、共享构建输出、同设备麦克风必须串行。Q审查占空闲槽，不额外增加第9个worker。

按任务卡实施和验证：每步跑受影响模块及直接调用方，最终同一候选统一适用全量与A01—A09真机旅程；不跳过hooks/CI。UI fixture/mock只证明组件行为，ASR/分人/摘要、附件权限/GC/治理、跨账号共享和录音恢复需要实际服务/设备证据。仅用获授权非敏感或合成测试数据，不用模拟器冒充真机，不抢其他会话设备。模型质量、语言与资源阈值在T03冻结，不事后降低阈值换PASS。

任务领取、关键实现/测试、失败/等待、集成和中断前保存检查点。仅主控制器原子更新execution-state.json，worker写独立任务结果。证据包括任务ID、base/commit或WIP文件hash、模型/配置/fixture版本、命令/退出码、时间、证据hash及FAIL/SKIP/ENV。不用/tmp作为唯一续接来源；秘密和真实用户数据不入仓。缺少外部输入记waiting_input，继续其他无依赖阻塞任务。

worker交review后主集成者核验才accepted。不得把未独立审查称为已审查。无提交授权时保留可恢复未提交候选、路径及内容hash，不为保存进度擅自commit。只有必要任务全accepted、最终候选证据有效且关键旅程通过才称完整交付；关键失败/ENV/跳过应报告PARTIAL。未授权部署不得报告上线。

现在先简要报告恢复出的有效完成项、阻塞、下一批就绪任务与实际并发上限，随后继续执行。已在可信用户指令中授权的动作不重复索取；这份提示词再次发送只表示续接，不重置台账或扩大权限。

续接补充：遵循PLAN.md §9—§12，区分component_ready与accepted，采用state_version和controller_epoch；旧epoch结果不能直接集成。没有commit/merge-main授权时只在独立集成工作树应用本任务补丁完成候选验收。执行前验证计划hash、台账状态与依赖，不因“相同提示词”而跳过源码及证据核验。


单会话与跨会话资源补充：优先一个会话持续执行到完整本地验收，任务阶段结束不逐项停下来问继续。已有授权内的实现选择自行决定、记录依据；缺外部服务/模型只挂起对应模型任务，其他录音/附件/权限/UI继续。遵守PLAN.md §14—§15，已知广州收口任务计划与RUN_ROOT只读受保护；真机/数据库/构建资源先验证归属与隔离，无证明则waiting_resource并继续独立任务。自建锁不代表对方会话配合；不杀未知进程、不重启共享DB/adb、不覆盖同包名、不改对方台账。测试中发生资源抢占则记RESOURCE_PREEMPTED，失效证据重跑；绝不拼成PASS。当前提示词不授权向其他会话发消息。用同一提示词续接，无需改路径或重新说明上次进度。

本候选未获审查迁移前，不执行计划或修复revision mismatch：保持仓外修订9状态与其旧合同配对。主控制器显式迁移后，开始派发前运行只读validate_plan.py，运行test_validate_plan.py校验调度反例；未通过先修复本任务计划/状态的一致性，不重置外来资源。每任务执行前冻结test_bindings；component_ready也要真实组件证据，accepted要覆盖required_checks，不能拿无关PASS代替。

首版方案：App直连豆包ASR2.0与GLM-5.3-Flash，Key仅当前账号本设备保存。读取DOUBAO-API.md、design-preview-doubao.html及共享profile fixture；knownID只query，未知无ID不能自动submit，prepared未派发和claim/prepare丢包按服务端恢复动作处理。公开或合成样本、5元预算目标已批准；仍需设备内Key与实际受控执行输入。附件生命周期、个人企业权限、原质量线和最终适用全量不减。
