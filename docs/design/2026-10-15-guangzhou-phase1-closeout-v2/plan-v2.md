# IMBoy 广州一期交互 V2 收口计划 / Guangzhou Phase 1 Closeout V2

编制：2026-10-15 CST。版本：V2.2。状态：`PLAN_DRAFT / STATIC_REVIEWED / NOT_ADMITTED`。

目标：先交付广州一期真实可用候选，再按原合同收口五项专项。本轮解决执行出口和调度问题，不以达到 9.9/10 为修改目标；静态设计、执行准入、实际测试、客户签收分别判断。

本次用户授权仅为精修现有计划和必要执行映射；映射内嵌本文，沿用本路径，不新增平行主计划。不启动实施、测试环境、24h、子 agent、worktree、提交、合并、推送或部署。本文中的未来角色、命令、分支和资源安排均不自行产生授权。原下载文档、旧合同、历史证据与现有 WIP 保留。

## 计划评估 / Plan Assessment

原稿：`/Users/leeyi/Downloads/IMBoy_Phase1_Interaction_V2_Closeout_Checklist.md`，335 行。优点是区分广州交付与五专项，保留原合同、真实平台、权限负例、候选绑定和有限重试。主要问题是还把决定执行质量的事项集中推给 W0，缺少显式业务卡和可机械判断的出口。

撤回 V2.1 的 98/100 静态自评：原合同递归全集和逐叶执行映射尚未闭合，不能给相关维度满分。本版保留原稿评分作历史评估，不给未经独立复核的新版综合分。下表是静态成熟度判断；执行准入仍为 NOT_ADMITTED，实际测试为 NOT_RUN，客户签收为 PENDING。

| 维度 | 权重 | 原稿历史分 | V2.2 静态设计判断 | 尚未取得的依据 |
|---|---:|---:|---:|---|
| 范围完整与合同继承 | 20 | 17 | 已保留义务；待全集核验 | Step 2 来源差集与每叶映射 |
| 当前事实与准入 | 15 | 12 | 历史观察与新起点分开 | 执行时远端、三仓冻结及授权回执 |
| 任务、写集及依赖 | 20 | 16 | 26 卡分阶段依赖可静态检查 | 具体接口版本、写集与资源授予 |
| 业务 oracle 与实际测试门 | 20 | 17 | 身份及测量断言细化 | 逐叶 argv、完整样本与真实行为 |
| 资源、恢复及回滚 | 10 | 9 | 提前预检及中断出口明确 | 持续执行器、独占窗口及恢复演练 |
| 证据及终态防误签 | 10 | 9 | 分层签发与失效规则保留 | 真实 receipt、变异拒绝及独立终审 |
| ECC 路由与授权安全 | 5 | 4 | 路由与任务状态解耦 | 现场工具能力和完整范围绑定 |
| 合计 | 100 | 84 | 不出综合分 | 静态修订不折算成测试或客户通过 |

关键修订：

- 朋友圈、附近的人在本地广州交接与一期来源中明确保留，原稿 E01–E16 和任务卡未给它们独立去向；补入 Step 13，不能因产品“私域优先”自行删除现有合同。
- 客服原合同包含 CX-01 首次开通、CX-06 实际接入/物理扫码、CX-07 运营事实、CX-08 容量/24h、CX-09 亲验和 CX-10 资格。原稿四张 C 卡不足以证明全部继承，补 Step 15–19、23–25。
- 原稿把非空升级/失败回退全过作为 W0 出口，导致与迁移无关的源码分析也被阻塞；本版 W0 先冻结来源和可执行卡，实际迁移另立 Step 4，阻断受影响路径。
- 原稿的“外部门禁可继续其他工作”保留，并明确哪些阻断技术候选、哪些阻断客户 UAT；不可通过重新分类删 Required。
- 普通 IM 回归保护历史密文；不恢复已暂停 E2EE 专项。广州 E2EE OFF 的具体 profile 依原合同冻结，TLS、静态加密及历史数据安全继续保留。

## 当前事实与来源 / Current Facts and Sources

以下保留 V2.1 的本机历史只读采样，非 V2.2 执行起点或远端现状。该次三仓采样时间：`2026-10-15T10:59:27.236Z`（CST 18:59:27）；本轮没有 fetch、访问 CI 或重跑历史测试。后端当前还有既有暂存改动，不能从旧表推断工作区干净；Step 1 必须重新清点全部 WIP。

| 仓库 | 该次 HEAD = local main = cached origin/main | 保护项 |
|---|---|---|
| imboy | `75b7c38249fd33dc7f5ea6e57d04fd0b271f8ada` | 两个既有未跟踪文档，不能纳入任务提交 |
| imboyapp | `7b2ed5f0b3a4219f4b84bf7d419a6282b2a28e55` | `ios/Podfile.lock`、`ios/Runner.xcodeproj/project.pbxproj` 已修改 |
| imboyadmin | `0965b0cd6addd54629df3f3a3e77208908359a66` | 既有未跟踪 `zcode-resume-prompt-v5.md` |

同一勘察期间 App 曾采到不同 HEAD，之后回到上表；这只说明取样不能替代执行冻结，不据此归因、清理或重置。后端已存在多个 Cache 专用 worktree；其存在不证明 owner 存活或资源可用。Step 1 必须重新核查。

已实际读取本地 `control/guangzhou-handoff.md`、`final-stop.json` 和 `resume-prompt.md`。停机 JSON 为 `USER_PAUSED_PARTIAL`；交接文档与续跑文件包含其后的 CX 实施、提交及未合 main 记录，正文也混有旧暂停段落。它们提供线索，不直接证明当前分支、授权、完成状态。核对真实用户指令来源、提交祖先、文件字节和资源回执后再继承；文档中引用的“允许实施/提交”不授权本次任务。

采用现有四层主架构；客服已有 feature slice，沿其现有 application/domain/infrastructure/interfaces 维护，不机械搬回旧目录。App 复用 Riverpod/go_router；Admin 复用现有查询与工作台；不引入 Redis/Mnesia、新消息引擎或通用代理调度平台。

| 来源 ID | 位置 | 用途 |
|---|---|---|
| S0 | 原下载清单 | 本次改进对象；旧任务 ID 见下文对应关系 |
| S1 | `docs/design/2026-10-11-unified-delivery-closeout/plan-v1.md`、`materials-index.json`、`source-contracts/` | 原统一收口、一期、企业、微社群的继承 |
| S2 | `docs/architecture/2026-10-12-organization-positioning-amendment.md` §8.2–8.3 | OA 目录与双入口，组织不是租户 |
| S3 | `docs/architecture/cache-v2.2.1/backend-closeout-plan-v1.md`、`closeout-v1/zcode-handoff-20261014.md` | Cache 合同、既有 owner、剩余重试和完整专项 |
| S4 | `docs/architecture/attachment-lifecycle-v5/06-zcode-handoff.md`、同目录 `execution-contract.json`、`01`–`05`、`manifest.sha256` | 附件执行合同，不能以交接头部状态代替当前实现 |
| S5 | `docs/design/2026-10-08-customer-service-experience-production/plan-v1.md`、`acceptance-required.tsv`、`inherited-seat-required.tsv` | CX 体验、55 个新增父项与 67 条继承来源记录；122 行不是独立功能数 |
| S6 | `docs/plans/2026-10-14-three-platform-ci-micro-community-closeout-v2.md`、原 V1 | V2 是增量工程收口，继承 V1 全部 26 父项及子场景 |
| S7 | `/Users/leeyi/.codex-runs/unified-delivery-repair-20261014/control/` 三个上述文件及其中的 `cx-source-contract/manifest.json` | 停止点、CX 来源归档、较新成果线索 |

权威优先级：可核实的用户范围修订 → 对应修订后的原合同 → 本计划调度细化。日期、文件头或“最新”字样不独立构成优先级。源缺失先按 materials-index、归档清单、原 Git blob 定位；`docs/plans` 符号链接按真实目标读。冲突保留 `BLOCKED_CONTRACT`，其他独立任务继续。

本次关键输入 SHA-256（观察值，不覆盖原 expected hash）：

| 输入 | SHA-256 |
|---|---|
| S0 原下载文档 | `2eb2f27ff5cc534a258a8a9eadfb9b6bc63334dd8686f8a798ba9fd9f4e3be7e` |
| S1 plan-v1 | `913a4c76a19d80dd89da9ae807c71b1534dbacffdd85934bfab40d320cd399d1` |
| S1 materials-index | `eec16f364e2da6d7d2900183ab4e716ab6e9d645c6f3db5dc84be11ca313b98a` |
| S2 组织定位 | `cc58294d81d30b4721ed16a77967892d55e03843da68fd6f29642d8ab1114bcc` |
| S5 CX plan-v1 | `14289f917fb3e273e495b0e91de27f393844eb7f4c9cdb717bced1d94407f7fc` |
| S5 acceptance-required | `585919a0046991a97c03534225660c095d6d12b89094af27cb13c9ce2d28e5c7` |
| S5 inherited-seat-required | `2a4a6b4b3f34fe59cc4b46654e961cc729788707a03ae025ae174dea76f7ab97` |
| S6 微社群 V2 | `51daccd01d4e426812873d8c13910062145e144afd23684d125c3bab7152db8f` |
| S7 guangzhou-handoff | `0c6f3834a75f0df860dea934c299aa166f90ac5da709748a6552d83f95443096` |
| S7 final-stop | `6a1caadd994aeac5c3b813c94a9e10ffd0ab7b9fc9c609c195b0e41639bee6c3` |
| S7 resume-prompt | `8f9ecc5450a27fd82de9b054f87172b8028dc499306ca22e47fc6b33dd3985ce` |

执行时源漂移须解释差异并重新绑定适用合同，不能自动接受新 hash。本文不宣称已经完整读取 S1–S7 的全部递归来源、所有实现或所有原始 receipt；全集核验属于 Step 2。

## 范围与执行合同 / Scope and Admission

广州主线：基础 IM → OA 主动同步目录/身份双入口 → 组织通讯录和用户主动聊天 → 朋友圈/附近的人 → 人工客服 → 消息/附件/推送恢复 → 安装升级和交付。

一期延期保持原授权：E2EE 专项、RTC/直播、AI/Bot/Agent/MCP 产品入口、OA 应用授权表单、外部自动建群/代发通知、钱包与付费频道。不删除 General 群/公告频道的已有依赖、不自动改写历史密文、不取消未启用能力对现有数据的安全责任。

Required 与缺陷等级分开：P0 为越权/身份错绑/误删/可重放凭据/候选不可信；P1 为承诺主流程、数据一致性或恢复失败；P2/P3 为已证明不影响本期的专项与维护债务。低严重度不能豁免 Required。

每个原始要求都保留 `source_plan/source_hash/source_id/source_text_ref`；展开 `subcase/platform/role`，额外记录资源目标 scope、flavor、配置和故障维度。`cell_id` 使用既有规范；若既有工具无规范，Step 2 冻结五字段规范序列化及 SHA-256，禁止执行中改算法。五字段不足以区分的业务组合必须拆 subcase，不能覆盖同一叶子。

一个共同业务运行可以满足多个原来源义务，但须逐项保留映射，不双算功能或删除来源。门类明确为：`LOCAL_REQUIRED`、`CUSTOMER_UAT_REQUIRED`、`RELEASE_REQUIRED`、`DEFERRED_BY_USER`。同一要求的层级只能由原合同或可核实修订决定，不能因缺资源改类别。Android 真机/macOS 原生及 Required 推送缺证据，仍阻断对应技术候选；真实客户 OA 验收和用户亲验另列 UAT。

W0 的出口是 Step 1–3 可核查，且至少一个任务满足 READY；不要求所有外部条件或全量迁移已过。每卡 READY 必须同时有：唯一 owner、真实授权范围、精确 owned_paths（含调用方/测试/fixtures/config/exports）、依赖版本、无占用冲突的资源、完整命令、fixture、正负 oracle、超时和回滚。下面路径是已定位的入口候选，不能把目录通配当写入授权。

执行模式默认单执行器；确获多代理授权时总活动上限 4，含协调者与独立 reviewer。Skill 本身只生成命令，不启动这些角色或保证命令内部不提交。提交、分支创建、合并、push、部署分别遵守届时授权；没有 commit 授权时保留可评审 patch 和完整字节绑定，不以 commit 为通过的必需条件。未来固定 Git 身份只用于已获授权的提交。

全卡共享范围声明，逐卡继承：

> Out of scope: 未获授权的提交、建分支、合并、push、部署、生产数据操作、联系方式配置、第三方通知，以及当前任务 owned_paths 之外的修改。

## 执行卡 / Execution Cards

卡正文保留业务目标、来源、写集、断言和回滚；数字前置描述相关叶的运行/最终验收条件，分阶段调度以文末唯一执行映射为准，不能据此阻断已解锁的准备。

原稿任务映射：B00/B01→1，B02→2，B03→3，B04→4；P01→5，P02→6，P03→7；G01→8，G02→9，G03→10，G04→11，G05→12，G06→14；C01→15，C02→16，C03→17，C04→18/19；A01→20，A02→21，A03→22/23，A04→24，A05→25，A06/D01–D04→26。原稿没有独立卡的产品包、朋友圈/附近的人归13；CX 原始 ID 仍由 Step 2 全集映射，不能由此表代替。

<a id="step-1"></a>

### Step 1 — 冻结来源、现场及成果归属 / Source and Workspace Freeze

- **责任/依赖**：协调者；无。只读先行，不接管旧 run。
- **读取与写集**：S0/S7、三仓 Git/worktree/status、适用规则与当前 owner 记录；未来只写仓外 baseline/checkpoint。核对旧 worktree 是否存在、每个提交是否已入 main、WIP 和各资源创建回执。工作树隔离不等于运行环境隔离。
- **未来获执行授权后的起点**：逐仓核实实际 remote URL、归属和 main → 从已核实远端 fetch main → 固定三仓精确 commit SHA/tree、来源与时间 → 各从对应 SHA 创建全新隔离 worktree → 冻结本 run 基线与初始候选。实际入口只在 C01 维护；缺远端权限仅阻断该仓新起点。不从旧专项工作树继续开发，不 reset 原 main，不吸收其他会话未提交修改，不随远端 main 持续漂移。后续授权修复产生新候选向量并按影响闭包重验，不自动重新拉 main。
- **验收**：三仓 HEAD/tree/status/锁文件/受保护 WIP 摘要齐全；每个停止点分类为已集成、待交接、失效或阻断；所有后续卡能追到来源，且授权不从旧提示词自动继承。
- **回滚**：R0，只读与计划登记；不得 reset/clean/stash 或清理旧资源。

<a id="step-2"></a>

### Step 2 — 冻结完整验收集合与范围裁决 / Exact Requirements

- **责任/依赖**：合同负责人；1。只写本 run 的 requirements/decisions。
- **工作**：从 S1/S3/S4/S5/S6 原文重算父项与所有叶子，定位原 CX 两表、Widget/hosted/enterprise-admin 并集；核验 S6 对 V1 的增量关系；分别记录原合同与修订后状态。形成 API/错误码/身份/上传/平台/profile/指标合同及每卡 requirement_refs。
- **24h 提前预检之一**：单列 S5 CX00-A05 容量确认的原始来源、确认人/时间和适用硬件/网络/负载；没有真实确认即登记待核验与最小确认项，不把建议档当授权。CX08-A03 是本地技术资格 Required，CX09-A03/A04 是客户亲验；CX10 发布资格及其生产至少24h观察另列，不调整层级。先暴露 Step 23 缺口，继续无关合同和任务。
- **验收**：来源义务全集与映射的差集为空，重复和无归属为零；缺源保留父项占位且阻断该来源签发；朋友圈/附近的人、群治理、物理扫码、24h、全部外部资格均有责任卡。用户未确认的容量不由执行者拍板。
- **回滚**：R0；范围修订追加，不覆盖原文/历史 hash。

<a id="step-3"></a>

### Step 3 — 绑定资源、命令与失败预算 / Resource and Command Binding

- **责任/依赖**：环境负责人；1，与2可交叉准备，依赖的合同定稿后才准执行。
- **写集**：只在届时授权的 run-owned 配置和仓外 control；复用现有 runner。记录实际 PG DB/schema、对象 bucket/prefix、Disk root、端口、节点/cookie 引用、构建目录、浏览器和设备；秘密不入日志。
- **24h 提前预检之二**：其他修复前逐项核实 CPU/内存/磁盘、版本、已确认负载、RTT/丢包及测量方式；独占机器/DB/对象/浏览器槽与运行权限的授予及窗口；既有持续执行器的名称、实际启动方式、会话结束后的存活依据及可覆盖热身30min+连续24h+收尾的期限；采样入口、timeout、中断/恢复、原始证据持久化位置与保留方式。逐项记录 CONFIRMED / UNCONFIRMED / UNAVAILABLE、依据、owner、NEXT_UNLOCK。自填租约或“可后台运行”不算能力证明；无既有足时执行器只阻断 Step 23 启动，不新建守护平台。
- **验收**：每个命令都有真实 cwd/argv/环境来源、runner hash、expected exit、oracle、资源证明、timeout；错目标探针在连接/写入前拒绝；启动握手验证 DB/schema/对象前缀属于本 run。无有效租约为 BLOCKED_RESOURCE；不得复用业务默认库。
- **回滚**：R4。先只读审 runner 默认值；首次副作用前完成资源验证，不能以自填 leases.json 充当跨会话授权。

<a id="step-4"></a>

### Step 4 — 验证迁移历史与旧数据升级 / Migration Qualification

- **责任/依赖**：迁移 owner；2+3；同一 DB/构建槽独占。
- **写集/入口**：`priv/migrations/`、App `lib/service/migrations/manifest_all.dart` 及既有迁移门；先验证，只修复已复现迁移缺陷。完整盘点 Up/Down 与实际 ledger，不把改号记录等同存量兼容。
- **验收**：空库安装、支持旧版本非空合成数据升级、重复升级、任一关键迁移故障后回退/恢复读取均通过；目录177、附件178/179及后续181等以当前目录/ledger为准；已执行正文不可悄悄变更。SQLite 生成物与 manifest 一致。
- **回滚**：R2。未过只阻断受影响 schema 的运行验收；源码定位和不依赖该 schema 的独立卡可继续。最终候选 schema 改变须重验本卡。

<a id="step-5"></a>

### Step 5 — 审查 Cache、撤权与 QR 依赖交接 / Cache Handoff Review

- **责任/依赖**：协调者+V2 指定 Cache 复验 owner；2+3；按下列三分流交接，不以旧会话恢复作为永久前置。
- **范围**：读取 S3 和当前 run/源码/receipt，复用已有实现。旧 owner 活跃且写集/资源重叠，必须交接，不抢占；旧任务已停止且相关成果可证明已合 main，在新的明确执行授权下由 V2 owner 从 Step 1 冻结 main 独立复验，不无限等待旧会话恢复；状态无法确认则不碰旧资源，可证明隔离的只读审查与独立任务继续。未合入成果的使用及归属仍需交接。广州运行闭包含恢复路径、启动 worker、缓存权限、QR、配置及实际节点数，不只看 UI 开关。
- **验收**：COMMIT 后失效、ROLLBACK 不广播、旧会话后续受限动作拒绝、PG故障不放权、QR完整HTTP并发单消费和重启安全有证据；启用 snapshot/READY/多节点时相应门不得延期。原141/6、53/53等只作历史线索。失败交当前有授权的唯一 owner；无可接手授权只阻断对应权限/QR/恢复叶，不阻断独立源码准备。
- **授权与预算继承**：Step 6 附件及 Step 15–19 客服统一使用上述三分流。旧用户暂停/禁止不会因新 run 或 main 已含成果自动失效；历史累计修复预算不清零。授权/剩余预算不清楚时只阻断对应写入、复跑或资源操作，不停无关只读工作。
- **回滚**：R4。共享权限/QR缺陷升P0/P1；不为一期签发改写 Cache 原专项通过状态。

<a id="step-6"></a>

### Step 6 — 修复附件已复现的交付缺口 / Attachment Safety

- **责任/依赖**：附件 owner；2+3，运行验证需相关 schema/授权叶；按 Step 5 三分流核查 S4 owner、已合 main 成果、写集和累计预算。活跃重叠须交接，停止且已合 main 可经新明确授权独立复验，未知旧资源不碰。
- **入口**：`attach_handler`、`attachment_upload_logic`、`attach_pending_repo`、`attachment_ds`、storage driver、delete worker 及直接调用方/测试；沿现有代码，不复制平台。
- **验收**：真实 PUT/confirm/view/Range 与字节 hash 对应；未知结果重试不重复、不允许确认后旧凭据覆盖；撤权、TTL与切账号边界明确；引用/hold/retention对象不误删，崩溃重试安全，切配置后历史对象仍按记录 driver 读取。实际与历史仍在用的 driver 全覆盖。
- **回滚**：R1/R3；新凭证/对象协议须兼容旧客户端。禁用 destructive worker 只能缓解，不能代签 required 清理门。

<a id="step-7"></a>

### Step 7 — 修复普通 IM、群治理与恢复缺口 / Core IM

- **责任/依赖**：消息 owner；2.contract/3.admission允许准备和实现；运行验收需4.schema/5.auth/6.upload的相关叶。
- **入口**：`msg_ack_logic`、`msg_c2g_*`、消息存储/拉取/归档、群生命周期及 App 消息直接调用方；定向测试先用现有 msg_ack/msg_c2g suites。
- **验收**：SERVER_ACK 已持久化、设备送达与用户已读分离；重复/乱序/ACK前后崩溃/断线分页不丢不重；转让/退出/踢人/解散事务失败不通知、成功只一次且离线能收敛；受限历史/附件/推送不能绕过成员权限。保留原密文安全，不恢复 E2EE 新研发。
- **回滚**：R1/R2；不更换消息引擎、排序真源或默认归档语义。

<a id="step-8"></a>

### Step 8 — 修复 OA 目录同步已复现缺口 / Directory Sync

- **责任/依赖**：目录后端 owner；2.contract/3.admission允许准备；真实写入需4.schema/5.auth对应叶。
- **入口**：`enterprise_directory_sync_logic`、`enterprise_directory_source_repo`、`enterprise_directory_repo`、directory handler、同步 PG/logic/admission 测试；现有 `sync/3`、`claim_login_tx/4` 已存在，不按未实现重写。
- **验收**：同来源稳定标识、重试幂等、同key异内容冲突、版本单调；漏对象不删除；部门循环/跨业务归属父引用拒绝；按 S2 §8.3，组织 A 离职仅撤 A 关系/资源，Owner 先交接，实例账号、组织 B 关系和个人聊天保留。实测旧会话访问 A 被拒且有效 B/个人资源仍可访问；账号停用与来源撤销分别按后文身份判定表。写入/映射/审计原子，来源凭据不能代发、建群或读文件。
- **回滚**：R1/R2。具体业务权限按当前用户/成员/来源/目标资源校验，禁止把 organization 当租户或全域权限。

<a id="step-9"></a>

### Step 9 — 修复身份关联及 OA 入站登录缺口 / Inbound Identity

- **责任/依赖**：身份 owner；8.api及本卡2.contract/3.admission固定即可准备；真实验收需4.schema/5.auth/8.directory对应叶。
- **入口**：`enterprise_oa_login_logic/repo/handler`、`enterprise_identity_logic/handler`、现有 OA login PG/HTTP 测试与App入口直接调用方。
- **验收**：先同步/先关联/先OA登录及并发不会生成重复或错绑；关联已有账号需双边身份，新建账号由用户明确选择；不按姓名/手机/邮箱合并；旧绑定不可静默换绑。按 S2 §8.3.1 逐次复核 Application/发行凭据/外部身份/intent/PKCE/DID 及最终 active Human/epoch：过期、重放、串应用/账号、账号停用/注销或发行授权失效按对应合同拒绝；仅退出 A 不直接推导全账号禁登或全部票据作废。拒绝不新建会话/映射/用户/目录数据，审计及安全消费按合同记录，不把合法防重放记账误判为业务副作用。目录同步不能签发用户会话。
- **回滚**：R1/R2；当前注册/会话合同保持，身份数据修复不自动批量执行。

<a id="step-10"></a>

### Step 10 — 修复 IMBoy 到 OA 免登缺口 / Outbound SSO

- **责任/依赖**：SSO owner；2.contract/3.admission允许准备，与8写集不重叠可并行；真实验收需4.schema/5.auth对应叶。
- **入口**：`enterprise_oa_sso_logic`、SSO handler/code repo、App JSBridge/WebView 当前实现及现有 SSO/context tests。
- **验收**：真实点击→getAuthCode→OA兑换只成功一次，串应用/过期/重放/实例账号停用、应用或兑换授权失效按实际出站合同拒绝；可选 mapping 按 S2 回退 user_id，不强制 org。仅退出 A 只改变 A 资源权限，有效 B/个人入口按合同可用；若某种 code 绑定特定来源身份/授权，须列绑定字段与拒绝条款后才判失效，不以入站规则替代出站规则。注销/切账号/在途回调不串身份；拒绝无新会话/映射/业务数据副作用，秘密不进入URL/日志。合成OA本地资格与客户真实OA确认分别记录。
- **回滚**：R1/R4；getAuthCode已有代码优先补证，不要求管理员手填一期已延期的授权表单。

<a id="step-11"></a>

### Step 11 — 修复 App 目录到聊天交互缺口 / App Journey

- **责任/依赖**：App owner；8–10接口冻结，可先组件验证；真实旅程需对应后端通过。
- **入口**：`lib/modules/organization/`、`lib/modules/enterprise_oa/`、现有路由/会话与相邻测试。先读 DESIGN 和子目录规则；禁止写受保护原生目录。
- **验收**：实际gzCustomer包可从双入口进入目录、检索、查看同事并主动聊天；加载/空/错误/拒绝/离线/恢复态齐备；Android真机和macOS原生分别验证长ID、中文输入、返回/深链、快速切账号/组织与注销回调。不以standard包代签gzCustomer。
- **回滚**：R1/R4；保留Riverpod/go_router及设计token，不新增平行状态或导航系统。

<a id="step-12"></a>

### Step 12 — 修复 Admin 目录与接入状态缺口 / Admin Directory

- **责任/依赖**：Admin owner；8–10相关合同固定；与11可并行但不得共享构建槽。
- **入口**：`src/modules/organization/pages/`、现有 enterprise 接入页面及 API/type/query 直接调用方；具体文件由 Step 3/本卡准入确认。
- **验收**：真实浏览器展示真实目录/关联/接入失败状态；搜索分页和TSID不失真；外部目录字段与本地治理字段冲突有明确反馈；无权/停用拒绝且不能显示同步成功；开通不依赖管理员手工SQL或理解Grant表单。
- **回滚**：R1；不删除历史授权数据，不把目录可见误当消息可读。

<a id="step-13"></a>

### Step 13 — 对齐一期产品包及朋友圈/附近的人 / Delivery Profile

- **责任/依赖**：产品包集成 owner；2确认 AC00–30、P1-LOC-02/P1-MOM-01等映射后；运行需4–7。
- **入口**：`imboy_profile_preset`、`imboy_feature`/policy、既有feature manifest/generator及App/Admin入口；`moment_handler/logic`、`location_handler/logic`和现有测试。共享manifest/router由本owner串行维护。
- **验收**：朋友圈发布/查看/权限/附件、附近位置授权/拒绝/更新/查询按原子场景验证；phase1/gzCustomer入口、API、WS、异步worker、深链/通知入口一致；缺配置、旧全开缓存和运行时覆盖不能重新开放延期项。E2EE OFF仍有TLS/历史/归档，不降级历史密文。
- **回滚**：R1/R4；验收前冻结正向白名单，不能只隐藏UI。未部署能力的安全隔离结论需运行闭包证据。

<a id="step-14"></a>

### Step 14 — 验证真实推送和后台恢复 / Push Qualification

- **责任/依赖**：App/推送 owner；7+11+13，3已落实自有设备及真实通道授权。
- **入口**：`push_notification_ds/logic`、App现有通知接收/路由与真机runner；沿用JPush，不更换provider。
- **验收**：前台、后台、杀进程、弱网、点击、DND、token过期/撤销与切账号逐格验；只到达正确有权会话，拒绝受限正文；Android与macOS适用能力按合同分别资格化。无真实通道为BLOCKED_EXTERNAL，mock仅用于错误分类单测，不补Required。
- **回滚**：R4；只允许明确授权的自有测试设备，不能用真实用户试推。

<a id="step-15"></a>

### Step 15 — 修复客服队列、搜索及待回复缺口 / CS Queue

- **责任/依赖**：客服集成 owner；2+3，运行需相关 schema/授权/profile 叶；旧 CX 按 Step 5 三分流交接，停止且成果已合 main 时可经新明确授权由 V2 owner 独立复验，不无限等待旧 owner；活跃重叠与不明旧资源仍受保护。Step 16–19 继承本规则。对应CX-01/CX-02。
- **入口**：Admin `queuePanel.tsx`、`contract.ts`、`workbenchApi.ts`、`workbenchHooks.tsx`；Backend现有 `cs_pg_session`、`cs_seat_console_app`及queue summary测试。
- **验收**：首次进入能找到待接待/我的/历史；多页外数据可搜且排序稳定；计数、未读、待回复与SQL真源一致，已读不等于回复；跨scope不泄漏，不按行N+1。覆盖原视口、键盘焦点、200%缩放及完整空错状态。
- **回滚**：R1；已实现服务端搜索先验证，不再造一套本地过滤。

<a id="step-16"></a>

### Step 16 — 修复客服编辑、附件及未知发送结果 / CS Composer

- **责任/依赖**：同一客服owner；15.api/6.api稳定且共享写集释放可准备；运行需15.queue/6.upload及schema/auth相关叶。对应CX-03与CX-07附件部分。
- **入口**：既有 `sessionView`/发送hook/`attachmentUpload.ts`及发送事务/幂等直接调用方；沿现有client_msg_id。
- **验收**：中文IME确认不误发送；A/B会话、账号/scope切换不串草稿或在途回调；双击、响应丢失、confirm成功后发送失败按同参重试唯一消息；编辑失败内容使用新payload；文本、图片、多图、图文、文件、原合同订单引用各自有证据。
- **回滚**：R1/R3；草稿仅现有内存边界，刷新提示、注销清理，不新加云草稿或敏感本地持久化。

<a id="step-17"></a>

### Step 17 — 修复客服长历史、已读与 SSE 恢复 / CS History

- **责任/依赖**：同一客服owner；15.api/7.api固定且共享文件释放可准备；运行需15.queue/16.send及schema/auth相关叶，不等待16整卡收口。对应CX-04。
- **入口**：`seatMessageHistory`、`messageViewport`、现有消息/可见ACK hooks、`cs_read_cursor_pg_tests`与SSE链路。
- **验收**：200条至少跨4个50条页，无重复遗漏且滚动锚点保持；可见本会话消息才ACK，后台/离线不误读；断网60s、SSE乱序/重复/超窗与重启后正确resync；会话切换cursor不串用，DB与UI收敛。
- **回滚**：R1/R4；保留真实代理链，不用HTTP200或重连事件代替恢复后的业务集合。

<a id="step-18"></a>

### Step 18 — 修复客服认领、转接及结束竞态 / CS Lifecycle

- **责任/依赖**：同一客服owner；15/16/17.api固定且写锁可用则准备；运行需15.queue/16.send/17.history及5.auth相关叶。对应CX-05。
- **入口**：`sessionActions`、`detailPanel`/presence、`cs_session_app`、`cs_pg_session`、capacity/presence/transaction测试。
- **验收**：两个独立坐席并发只有一方认领，过期version不覆盖；away/offline/满容量规则可信；转接取消零写入，确认后旧owner只读、新owner可续接；关闭取消/在途未知结果/真实409有反馈；关闭后再联系、历史和审计一致。不能以同凭证双请求代替双身份竞争。
- **回滚**：R1/R4；沿queued/active/closed及CAS，不增加第二会话真源。

<a id="step-19"></a>

### Step 19 — 修复客服接入、Widget与运营事实缺口 / CS Onboarding

- **责任/依赖**：接入/运营owner；6/15/18.api固定可准备；运行需15.queue/16.send/17.history/18.lifecycle及6.upload对应叶。对应CX-06/CX-07剩余项。
- **入口**：既有开通页、snippet、`seatQrLoginPanel`、Widget/controller/chatMachine、context/rating/stats；复用 `harness-seat-embed.sh`和相邻测试。
- **验收**：真实开通→实际生成的两种snippet→访客→物理扫码接待；Widget全态/刷新重入、origin/sandbox、Visitor/Seat/Admin凭据隔离；附件撤权、真实评价/首响/接待/关闭/队列统计与DB一致。移动坐席资格、原视口和无PII诊断有证据。
- **回滚**：R1/R4；客户上下文仅既有最小授权事实，不造CRM；二维码辅助API不代替物理扫码。

<a id="step-20"></a>

### Step 20 — 验证冻结候选的三端业务旅程 / Integrated Acceptance

- **责任/依赖**：集成测试owner；广州所选叶对应的 Step 4–19 接口/实现版本稳定，按执行映射逐叶准入，不等待无关专项全收口。缺外部条件时运行独立已解锁子集，缺项仍阻断相应签发。
- **输入**：先完成最终正式构建，再冻结三仓candidate、patch/tree、依赖/配置/runner/fixture/产物向量；矩阵见后文。一个完整集成候选上逐叶执行，不拼各分支绿灯。
- **验收**：UI→HTTP/WS/SSE→PG→对象字节→另一终端显示可关联；正例状态正确，越权/重放/取消有无副作用断言；全部LOCAL_REQUIRED映射receipt。每个适用平台、角色、flavor及故障组合分别有证据，UAT仍另判。
- **回滚**：R1/R3；集成后发现缺陷回到原owner最小修复、重冻并使相关证据失效。

<a id="step-21"></a>

### Step 21 — 验证正式产物安装、升级及恢复 / Installation and Recovery

- **责任/依赖**：交付owner；4+20候选稳定；独占合成DB/对象/安装设备窗口。
- **工作**：用同候选正式构建产物新装、从支持旧版本升级；备份非空PG与对象，恢复到另一run-owned目标。SQLite历史升级与失败回退同验。
- **验收**：恢复后实际登录、目录、消息读取、附件下载成功；精确对拍ID/行/关系/对象hash；有不可恢复变更时使用经演练前向修复/备份路径，不能自动Down删数据。RTO/RPO按冻结合同计时，缺目标指标不自行承诺生产SLA。
- **回滚**：R2/R3/R4；备份命令RC0不等于恢复通过。

<a id="step-22"></a>

### Step 22 — 验证最终候选全量质量门 / Final Quality Gates

- **责任/依赖**：主集成者统一窗口；20+21同候选稳定。子owner不各自全量。
- **工作**：执行后文真实入口并核最新CI配置、排除套件与专项门；全部argv在Step3展开。后端EUnit按S1连续两轮，原始Dialyzer、架构/安全门及三端产物与生成物门补齐。重新构建的产物须与Step20冻结摘要一致；发生字节变化时先解释并重新绑定、重验Step20/21对应旅程，不能沿用旧包验收。
- **验收**：正常门raw exit=0、failed=0、required无skip/cancel/not-run；每轮独立receipt，不能用baseline/ratchet代替原始`make dialyze`。默认排除的相关suite按自身隔离前置串行补验，不默认全量已覆盖。
- **回滚**：R1/R4；失败明确产品/runner/资源根因并修复，禁止放宽ignore/删断言/吞exit。

<a id="step-23"></a>

### Step 23 — 验证客服容量与连续24h稳定性 / Capacity and Soak

- **责任/依赖**：性能owner；22通过且同候选冻结；Step 2/3 提前预检已逐项确认。来源为 S5 §4.3、CX-08 与 CX08-A03（本地技术资格 Required）；客户签收和 CX-10 生产至少24h另验，不降为可选。
- **工作**：按后文测量合同冻结负载、硬件/网络、时钟、采样和故障日程，热身30min后观察完整连续24h；全量/构建/性能不得争抢同机资源。先在隔离故障场景通过恢复门，再按冻结日程纳入24h，保留正常/故障/恢复全部原始样本。
- **验收**：全部冻结阈值达标，泄漏/重复业务消息/不可恢复持久消息/非预期崩溃/未处理异步错误为0；计划内服务重启另记，须满足故障条款，不能豁免安全与持久化不变量。期间候选/配置不变，每小时留资源与中断记录。健康任务不因短心跳超时被误杀；执行或观测中断不能证明连续完整、缺陷修复或稳定性相关候选变化均重开完整窗口，不拼接、不排除失败刷指标。
- **回滚**：R4；无容量确认/专属机器/持续执行器为具体BLOCKED，不拿微型fixture或本地soak替代生产观察。

<a id="step-24"></a>

### Step 24 — 独立复算合同与证据 / Independent Review

- **责任/依赖**：非实施reviewer；20–23可执行required完成；可提前审结构但不得提前签最终结论。
- **读取范围**：原来源全集、requirements、候选/产物、真实receipt与日志、回滚；reviewer只读，不自修自己所签证据。
- **验收**：缺/多/重复叶、错SHA/hash、日志复用、非零却PASS、子项缺失父PASS、错误平台、漏绑dirty、foreign混入等单故障变异各自被对应规则拒绝；cell_id/内容hash/关系独立重算。真实完整包与如实PARTIAL包分别获得与事实一致结论，自测绿不补业务证据。
- **回滚**：R0；实现者不得兼任最终独立签发人，无独立复核条件保留BLOCKED_REVIEWER。

<a id="step-25"></a>

### Step 25 — 准备交付包并登记用户与外部验收 / Delivery and UAT

- **责任/依赖**：交付owner；24技术审查；外向联调/签收安排须另有真实用户授权。
- **产物**：本地可审的安装包、scope/profile/平台、配置与最小目录调用示例、双方向OA/客服脚本、恢复runbook、容量/支持边界、许可和限制；优先更新现有权威说明。
- **阶段拆分**：prepare 为 DIRECT_TASK，输入版本固定后可自动整理本地草包，Step 24 通过后才可封交付包；uat 为 HUMAN_GATE，由用户执行五项亲验并逐项确认，外部联调另核授权。prepare 完成仅代表该阶段完成，不能令本卡 TASK_PASS 或代签 UAT。入口只引用 C25。
- **验收**：CX09五项亲验（找最久等待并接待；中文多行+附件；两访客草稿及未回复；转接结束查历史；断网恢复判断发送结果）由用户独立完成、逐项确认；真实客户OA/平台/通道资格逐项签收并绑定候选。未签收保持PENDING，不代签、不自动配置联系人或发送邀请。
- **回滚**：R0/R4；生产为NOT_EXECUTED；客户不接受即不签UAT，整改后重冻重验。

<a id="step-26"></a>

### Step 26 — 制定五专项剩余收口与续跑清单 / Remaining Work

- **责任/依赖**：协调者；2起可登记，最终状态需24。不反向占用广州已冻结候选。
- **范围**：Cache完整C09/C10/READY/混版/指标/性能；附件V5完整M1/M2/M3、引用/队列/lease/reaper/driver故障；微社群V1全26父项和V2增量CI；CX未完成本地/UAT/资格；一期原合同逐项余额。工程文档债务只列有调用/引用证据的必要项，不新开整仓重构。
- **验收**：每个旧required有唯一去向、owner、source、依赖、资源、命令/oracle、累计预算、NEXT_UNLOCK；正式延期保留修订依据和原合同差异；若“广州不受影响”证明失败，相关缺陷重新进入广州阻断集合。五专项分账，不把子项重复算成新项目。
- **回滚**：R0；不改旧verdict/hash、不删除旧run、不暂停或抢走其他会话任务。

## 业务矩阵与验收标准 / Business Matrix and Oracles

原E01–E16继续作为索引，新增覆盖缺口。下表不是有限16/20个测试的完成率；Step2须展开原合同实际叶子。

| 旅程 | 主卡 | 必须证明 |
|---|---|---|
| 目录新增/修改/重试/版本/层级/遗漏 | 8 | 映射与审计同事务，越权零写入，App/Admin一致 |
| 调动/暂停/组织离职/Owner及多组织关系 | 5/7/8 | 撤 A 资源与旧会话对 A 的权限；不删实例账号/B/个人聊天，B 与个人资源正例仍通过 |
| 三种进入顺序、双边关联、并发身份 | 9 | 单一身份、无静默合并换绑，拒绝时无会话/映射副作用 |
| OA两个方向与注销竞态 | 9/10/11 | 按下表区分账号、来源/应用和组织关系；入站与出站分证，单消费与迟到回调不串号 |
| 目录到聊天，群退出/转让/踢人/解散 | 7/11/12 | 有权才读写，状态和通知收敛，事务失败不通知 |
| ACK/已读、离线分页、重连与崩溃 | 7 | 双端实际业务集合，不只计数；不丢不重，游标合法 |
| 各消息类型及上传全链、旧PUT、driver切换 | 6/7/16 | 同payload安全重试，真实对象hash与归属一致 |
| URL撤权、TTL、hold/retention及删除恢复 | 6/21 | 不再新获未授权内容，不误删；不承诺收回已下载数据 |
| 前后台/杀进程/弱网/通知点击/DND/切账号 | 14 | 真通道到有权会话，拒绝受限正文 |
| 朋友圈与附近的人（正向保留） | 13 | 原P1-MOM/P1-LOC子项逐个映射，位置权限与内容ACL无旁路 |
| 广州产品包（延期能力关闭） | 13 | UI/API/WS/worker/深链/旧缓存一致，保留现有依赖和历史数据 |
| 客服首次开通、两种snippet、物理扫码 | 15/19 | 正式产物从零开通接待；API扫码替身不代真机 |
| 队列/跨页检索/未读/待回复/计数 | 15 | SQL/API/UI一致，跨scope拒绝且不泄漏总数 |
| IME/草稿/迟到请求/响应丢失/重复发送 | 16 | 不误发不串稿，未知结果唯一业务消息 |
| 长历史/锚点/可见ACK/SSE故障 | 17 | 200条多页正确，恢复后UI/DB集合一致 |
| 双独立坐席竞争/容量/转接/关闭/再联系 | 18 | 唯一归属，CAS/真实409，取消零写，旧owner撤权 |
| 嵌入来源/三类凭据/移动资格/统计评价 | 19 | origin/sandbox与凭据拒绝有效，统计定义/时区与DB一致 |
| 安装/旧升级/失败回退/非空业务恢复 | 4/21 | 同正式产物恢复登录、目录、消息和对象字节 |
| 容量及连续24h | 23 | CX08-A03 本地技术资格；完整足时证据、全分母与分窗复算，生产至少24h另验 |
| 原视口/键盘/焦点/200%/用户五项任务 | 11/12/15/19/25 | 1440×900、1280×720、1024×768、嵌入768×600、访客390×844按CX原合同，用户不被代理代签 |

身份判定表同时约束 Step 8/9/10、App/Admin 与 C08–C10；每条断言绑定相应来源叶，不用“离职”一词推断全局禁登：

| 事件 | 合同与身份绑定 | 应验证的拒绝/保留行为 |
|---|---|---|
| 实例账号停用/注销 | S2 §8.3、§8.3.1 的 active Human、DID 与持久化 epoch；会话吊销按现有账号合同 | 该账号登录/签发与旧会话后续动作按账号合同拒绝或吊销；不将组织 A 退出当此事件 |
| OA 来源身份失效/关联退役 | 入站票据绑定 Application、发行凭据、external_user_id、intent、challenge；核对 person/binding 有效性与当前来源合同 | 仅拒绝绑定该失效来源身份且合同要求拒绝的流程；记录具体状态、字段和条款。目录离职不自动等于来源身份失效；未明确的来源状态转移先待核验，不扩成实例账号停用 |
| 应用停用/发行凭据或 Grant 撤销 | S2 §8.3.1 签票、pending、会话返回前复核发行权限；出站按自身应用/兑换合同 | 各方向分别证明受影响票据/授权被拒且没有新会话、映射、孤儿用户或目录写入；不连带吊销无关应用/组织 |
| 仅退出组织 A | S2 §8.3 明确只处理对应组织关系；出站 S2 §8.3.2 organization_id 仅业务归属，mapping 可选 | A 资源后续访问拒绝，B 仍有效关系与个人聊天可用。是否影响某种票据必须指出它额外绑定的身份/授权和要求拒绝的条款；没有依据不得追加全账号禁登 |

每组都做变更前、事务提交后、回滚后及在途请求/旧会话的正负对照。拒绝路径前后对拍用户/映射/目录/会话；审计、防重放消费及拒绝码按合同核对，不要求抹去合法安全记录。来源身份失效的客户状态映射或出站撤销语义无法从材料确认时，仅将对应叶记 BLOCKED_CONTRACT，最小解锁为该接口字段/状态/条款的确认与回归实例。

S5容量与指标沿原合同，不能由本计划代替CX00-A05的用户预先确认：建议档为2组织×2工作区、20在线坐席、100 active/200 queued、每会话200条、历史1000会话、混合10msg/s；附件取10MiB与产品上限较小值，另验上限+1字节。组织是业务维度，原表“跨租户泄漏”按S2解释为跨身份/目标资源越权，不重新引入租户架构。

- 点击反馈≤150ms；foreground且RTT≤50ms/无丢包时，双方向各≥200个端到端可见样本，p95≤2s、p99≤5s。
- 首屏/搜索/切会话p95≤1.5s；正常与故障样本分开，必须保留完整分母/失败数，不能只报平均值。
- 60s断网恢复后≤10s队列/归属/消息与DB收敛；超窗full resync，不重复业务消息。
- 连续24h零泄漏、零重复业务消息、零持久化后不可恢复消息、零非预期崩溃、零未处理异步错误；仅冻结日程中的受控重启单列计划事件。热身30min后逐小时采样，最后6h内存中位数较前6h增长≤20%，连接/查询无持续爬升，结束≤5min回收。
- 用户五主任务全部成功，无阻断/误发/串台/不可恢复草稿丢失且用户逐项接受。实际容量或生产目标缺失时如实BLOCKED，不捏造SLA。

### 24h 测量与故障合同 / Soak Measurement Contract

来源必须分别登记：CX00-A05 确认容量；CX08-A01 故障恢复；CX08-A03 容量/连续24h本地技术资格；CX09-A03/A04 客户亲验；CX10 原发布资格及生产至少24h观察。后者仍需独立生产授权和真实负载，本地 soak 不冒充生产观察。

- **起止与时钟**：热身30min不计入24h。冻结 T0（负载稳定且全链观测就绪）和 T1≥T0+86400s，业务/观察器连续运行；同机用 monotonic/performance 时钟计算耗时，UTC 墙钟仅关联日志。跨进程/终端记录时钟偏差和校准上界，优先由同一观察器记录提交与实际可见；无法量化跨钟误差的延迟结果不合格。固定浏览器版本、foreground、RTT/丢包测量点和采样实现/hash。
- **端到端分母**：t_start 为访客/坐席实际提交操作，t_end 为另一端对应业务消息实际渲染可见；每方向正常窗口至少200个样本。每条保留 direction/window/session/client_msg_id、开始/完成/超时、发送/持久化/可见结果及重试关系；重试不重复算成新业务消息。用冻结的 nearest-rank（ceil(p×N)）复算 p95/p99，未完成/超时在全分母中记失败并以 +∞ 参与尾延迟，不只对成功子集给达标结论。失败计数、HTTP状态和持久化后最终可恢复性同时输出，不能靠重试删除失败样本。
- **交互与查询**：pending 从操作触发到首次可见反馈；首屏/搜索/切会话从操作到目标状态及数据实际可见。所有尝试进分母，记录取消、超时、错误；阈值沿 S5，测试 timeout 在启动前按既有 runner/隔离基准冻结，不能失败后调大。正常、故障、恢复窗都保留，分别算全分母/尾延迟/失败，另给整窗计数；失败不能事后改窗或排除。
- **计划内事件**：仅允许 S5 CX-08 明确的60s断网、SSE重复/乱序/超窗、受控服务重启、PG不可用、Garage上传失败、token过期/吊销、429；只在本 run 隔离目标，启动前固定目标、次数、起止、注入/撤除方法及预期错误。60s断网恢复及其他故障撤除/目标可用的可观测时刻为恢复计时零点，按 CX08-A01 要求≤10s达到队列/归属/消息与DB一致；超窗 full resync 仍须满足恢复时限和不丢重。服务重新可用的启动上限、PG/Garage故障持续时间材料未固定，须在 Step 2/3 按现有基准和用户容量确认冻结；缺值只阻断该故障场景及足时观察启动，不自行声称已满足。
- **事件与失败**：已列且按时完成的受控停机不记非预期崩溃，但有独立事件receipt；计划外退出/重启、注入超界、恢复超时、未处理异步异常、任意越权/业务重复/持久化消息不可恢复均 FAIL；计划内故障不豁免这些不变量。正常窗口请求失败也记失败并定位，不以环境噪声删除。资源污染或观测缺口无法排除则本轮 INVALID/PARTIAL，不能签 PASS；修复或无效窗重开完整24h。
- **内存与资源**：T0 冻结采样进程清单（backend BEAM/Port 子进程、Admin/Widget 浏览器主进程与renderer、PG、Garage及负载/观察器分别分组），记录PID+启动身份、RSS/可用PSS定义、BEAM内存、连接/查询计数与单位。各被测组件分别比较前6h/末6h固定采样点的中位数，不能用总和抵消单组件增长；重启后的PID必须跟踪到同一组件，不能仅统计存活进程。每小时同偏移采样，缺点记缺失，原始序列保留；“持续爬升”的统计判据与噪声范围在准入时固定，不事后挑区间。终止负载后≤5min回到冻结的连接/查询基线范围；缺基线/判据即待核验。
- **中断与保存**：原始事件、样本、每小时资源与进程身份持续写仓外持久位置并保留hash/checkpoint。控制会话断开而既有执行器与完整观测仍存活时，可核验后重连同一run；业务/负载/观测确有中断或无法证明连续，则保存现场并启动新完整窗，不能拼两段。没有持久执行方式就如实 RESUME_REQUIRES_RELAUNCH / BLOCKED_ENV；不把 goal 名称视作持续执行能力证明。

## 依赖、资源与工期 / Dependencies and Scheduling

不再按一条全串行主链调度。文末26行台账分别给 implementation_dependencies 和 acceptance_dependencies：前者允许范围内准备/实现，后者决定对应叶或阶段的最终通过；接口稳定不等于运行验收通过。Step 26 从2开始登记，Step 25 可提前整理草包；都不得提前签最终状态。接口合同已冻结时 App/Admin 可准备独立测试与交互，真实集成等相关 schema、权限、服务、平台和产物就绪。

采用一次集成冻结，开发期只跑影响模块和直接调用方；同一`workbenchHooks/contract/workbenchApi/cs_pg_session`由客服集成owner串行，router/policy/manifest、elib_pg、迁移号和App消息入口由指定owner维护。目录与SSO文件交叉时同样串行。按 Step 5 三分流管理旧 owner；活跃重叠未交接不占写集，停止且成果已合 main 可经新明确授权独立复验。

租约记录规范resource_id、grantor/授权来源、run/host/PID/启动身份、路径/端口、创建回执、取得/到期/释放条件；已有共同协调入口才可授予共享槽。TTL过期不等于持有进程死亡，不抢锁、不杀不明进程。隔离新worktree也共享Git对象/refs，禁止多worker写同一index。

每60s心跳；5min无心跳且无日志/进度变化触发诊断，不直接判失败。每条业务命令启动前写下有效timeout：采用现有runner限值；无现值时用隔离试运行耗时依据冻结，长全量/24h独立登记，禁止无限等待。只终止创建回执和启动身份均匹配的本run进程，保存日志/进程树/资源现场后温和停止；中断记TIMEOUT，不能记RC0。

同根因默认最多两轮修复尝试；继承S3/S4更严格预算及真实剩余授权，不因新run/新agent清零。历史预算不清楚时先诊断并登记BLOCKED_BUDGET，不擅开第三轮。预算到达后停该项，继续合法独立任务。

不承诺“几小时全收口”：Step1–3完成后按真实未闭合叶、owner和资源重估。CX原18–30人日是旧总投入线索，不是本次余额；关键路径至少包含前置实现/全门耗时+30min热身+连续24h+独审/UAT，真机/客户等待另计。没有有效持久执行器就标`RESUME_REQUIRES_RELAUNCH`；不新增后台守护、不承诺会话结束后自动继续。

## 实际命令与最终门 / Actual Commands and Final Gates

实际入口只在文末“任务执行映射”维护；本节和各卡仅引用 C01–C26 / K-*，不另存逐卡命令副本。Step 3 将现场 cwd/argv/环境来源、超时和资源绑定登记到同一 run 台账；如需 commands.sh，仅从该绑定机械导出并标源 hash，禁止反向独立编辑，不能按文件顺序替代依赖调度。未展开的设备、客户地址、PG/对象目标和 runner 参数保持待绑定，不是假定可执行的占位命令。

当前Makefile全量排除涉及`elib_tsid`、Agent PG相关suite、`cs_pg_widget`、`cs_seat_console_pg_tests`。Step2/22按当前CI和原来源判断适用门，TSID及客服相关专项不得漏；已延期Agent专项是否适用必须有范围依据，不因名称直接删除或强行扩成一期功能。EUnit排除含source模块名与test模块名，须防`t=`空列表假绿。

CI全矩阵按S6单列每工作流/平台/命令/状态；本次未访问远端CI，不把本地通过当Actions成功。若读远端需要连接能力，只做授权内读取；触发工作流、push等另按用户授权。Cache C09/C10各两轮及性能七回执属于其原完整专项，不能和本计划EUnit双轮合并计数。

## 证据、失效与签发 / Evidence and Verdicts

未来仓外RUN_ROOT建议为`/Users/leeyi/.codex-runs/guangzhou-phase1-closeout-v2-<run_id>/`。复用既有control/ledger/receipts结构，最小包含baseline、source hashes/decisions、requirements映射、task ledger、资源/命令、candidate manifest、checkpoint；原始日志按任务/叶子/轮次保存，final只保留verdict、交接和续跑入口。能由台账推导的摘要不重复造报告。

每张卡状态：`TODO→READY→RUNNING→TESTING→REVIEW→TASK_PASS`，复现缺陷走`FIXING→TESTING`。阻断细分SOURCE/CONTRACT/ENV/RESOURCE/EXTERNAL/REVIEWER/BUDGET；缺独立设备不能记NOT_APPLICABLE。准确的PARTIAL证据包可校验有效，但不等于产品通过。

最小receipt：task_id、五字段leaf/cell_id、source hash、candidate三SHA/tree/patch摘要、dirty白名单/产物/依赖/配置/fixture/runner hash、平台/设备/角色/scope/flavor、argv/cwd、开始结束时间、raw exit、executed/failed/skipped/cancelled、正负业务断言、原始日志路径/区间/hash、reviewer、attempt、下一解锁条件。日志一个文件一个hash；敏感值只留安全引用，不进入台账。

同一日志可覆盖多个叶子，只在每叶有独立case/assertion定位且候选等指纹一致时复用；只绑定全量日志或截图不够。receipt匹配而实际没有发送VIEW、物理扫码或真实恢复行为的，依然未通过。校验器必须检查业务断言与完整性，不能只验JSON格式。

每次候选变动生成影响闭包：共享权限/cache/schema/profile/排序影响全部关联调用链；UI变动至少重验对应平台/产物/旅程。旧全量永不替代新候选最终全量；最终候选有源码/runner/关键配置改变，重跑适用全量与相关专项，Step23若变化影响稳定性则整窗重来。只写“影响不大”不能豁免。

回滚类别：R0只读/计划恢复；R1在本run工作区按已授权方式恢复本任务patch或撤销本任务提交并复验，不触碰foreign WIP；R2以兼容expand/演练过的前向修复或备份恢复为先，不把git revert当DB回滚；R3恢复run-owned合成数据/对象，先保留引用和对象再清理；R4恢复冻结配置/产物并只停本run资源。真实业务数据与第三方动作一律不在本地回滚演练授权内。

| 结论 | 充分条件 |
|---|---|
| `PLAN_ADMITTED` | Step 1 新基线完成，Step 2/3 对至少一张卡所需合同/资源/预算准入完成且该卡 READY，授权有效；其他来源/24h缺口逐项保留，不封死独立工作；不是业务PASS |
| `GZ_V2_LOCAL_CANDIDATE_PASS` | Step 2 原合同全集及分类闭合，广州全部LOCAL_REQUIRED逐叶通过；无未闭P0/P1；同候选Step20–24通过，含适用真端/推送/CX08-A03连续24h |
| `GZ_V2_UAT_ACCEPTED` | 上项+用户亲验/真实客户OA及相应外部Required明确签收同候选 |
| `CS_LOCAL_TECH_PASS / CS_UAT_ACCEPTED / CS_RELEASE_ELIGIBLE` | 按S5各层原Required分别判定，不能相互替代；CX00-A05等未确认仍保持原合同未闭 |
| `FIVE_PLAN_CLOSEOUT` | 五项原合同各自PASS/PARTIAL/BLOCKED/正式延期及修订后状态；不得由广州PASS推出 |
| `PRODUCTION_STATUS=NOT_EXECUTED` | 本计划上限是本地候选/可审交付包；生产须独立目标限定授权、部署及生产复验 |

所有阻断项至少写source/leaf、当前事实、owner、已用预算、最小NEXT_UNLOCK、候选/证据定位和可复制恢复步骤；不把“需要用户”泛化为停止全部独立任务的借口。

## 任务执行映射（唯一命令真源） / Execution Map

26 卡不增不减。下面台账是唯一调度映射，C01–C26 是唯一任务说明入口，K-* 是复用脚本/命令目录；其他正文不再复制 argv。不保留人工维护的逐卡 slash 命令与 Batch 双份正文，也不要求每卡使用 ECC。确需批量文件时只能由本台账的现场绑定机械生成，标注来源 hash，并仍按依赖、写锁和资源逐项准入。

每个 C 入口的完整范围为：本计划对应 step-N 正文 + 该卡原合同 requirement_refs + 身份/测量合同 + 当前授权/预算/资源 + 候选向量 + 本节操作规程。未来直接执行器先读取这些输入，给出实际 cwd/argv 或具体检查动作、oracle、timeout、expected exit，再按获准范围运行；这已经是 DIRECT_TASK 的执行说明，不需要因 ECC 路由不适用改由人工操作。尚无现成脚本覆盖的行为明确为直接任务，不声称已有工具可自动完成。

**状态与里程碑**：task_status 是业务卡状态，本轮全部 TODO / NOT_ADMITTED；execution_mode 是阶段方式；ecc_route_status 仅说明当前已核命令面是否能携带完整 scope。UNSUPPORTED 不构成业务 BLOCKED，AVAILABLE 也不代表可执行或有授权。九张原路由不适用卡为4/5/14/20/21/22/23/24/25，全部有 C 入口；最后一张含自动准备及人工签收两个阶段。

依赖记为 N.output（Step N 的局部输出，不等于整卡 TASK_PASS）。除 baseline 外，每个输出都按本卡 requirement_refs、接口版本/源码 SHA、适用 profile 和验收叶限定范围，并登记实际指纹；空占位不算满足。contract 是已核实接口/范围合同；admission 是该操作的授权/预算/目标检查；api 是既有或修复后经确认稳定的接口/DTO/错误语义；其余输出是对应叶的运行验收receipt。2.contract 与3.admission可按叶局部闭合，23 的容量缺口不能使所有卡失去准入；但最终签发前2的全集必须闭合。

- implementation_dependencies 满足、范围/写锁/所需资源齐备，且实际操作的前置全部满足，才可记 READY；未检查保持 TODO，不从静态表推定 READY。
- 仅实现/准备前置满足，运行所需验收依赖或资源尚缺时记 PREPARE_ONLY；只做不触及缺失条件的源码、独立测试准备及交互，不能执行依赖该条件的集成测试。验收型卡可提前整理方案，真实调用前仍须满足 C 入口列出的运行前置。
- acceptance_dependencies 决定相关阶段/叶的签发；所有依赖、资源、业务断言、证据和独立复核齐全才 TASK_PASS。25.prepare 可以先完成，25.uat 缺用户仍 PENDING；26.balance 可滚动登记，终稿才等24。
- 缺设备/凭据/权限/持续执行器/独立 reviewer或预算时，记录受影响叶的 BLOCKED_* 和 NEXT_UNLOCK，继续其他合法 READY/PREPARE_ONLY。同卡允许一个阶段受阻而另阶段继续，不将全卡受阻扩散成全计划停机。

资源缩写：CTL=本run仓外台账/证据与只读源；WT=Step1全新冻结worktree及各自构建槽；PG=独占合成DB/schema；OBJ=专属bucket/prefix或Disk root；SVC=明确端口/节点/BEAM槽；WEB=真实浏览器/独立身份；DEV=Android真机/macOS原生与对应构建槽；OA=合成OA或单独授权真实OA；PUSH=授权自有设备/真实推送；SOAK=足时独占机器/负载/观察器与持久证据；REV=非实施独立reviewer；USER=用户亲验。资源可按阶段使用，不要求静态准备占运行资源。WT不隔离CPU/DB/端口/设备。

write_conflicts 为互斥写集标签，0 表示只读来源/只写独立台账；同标签只允许当前唯一 owner。MIG=迁移版本/DDL及SQLite manifest；AUTH=权限/Cache/QR/共享repo；ATT=附件存储/driver/回收；MSG=消息和App公共入口；DIR=目录/身份映射；SSO=OA/Bridge；PROFILE=router/policy/feature manifest；CS=hooks/DTO/contract/workbenchApi/cs_pg_session及客服共享事务入口；APP/ADMIN=各端共享导航/类型/构建配置。实际文件交叉即补同一锁，不以标签不同证明隔离；迁移、router、manifest等公共文件仍单写者。卡正文 owned_paths 和逐文件核查共同约束写集，锁不授权扩大范围。

### 26 卡调度台账 / Task Ledger

| Step | task_status | implementation_dependencies | acceptance_dependencies | write_conflicts | required_resources | execution_mode | ecc_route_status | command_ref |
|---:|---|---|---|---|---|---|---|---|
| 1 | TODO | — | — | 0 | CTL/WT | DIRECT_TASK | AVAILABLE | [C01](#cmd-01) |
| 2 | TODO | 1.baseline | 1.baseline | 0 | CTL/USER(容量确认) | DIRECT_TASK | AVAILABLE | [C02](#cmd-02) |
| 3 | TODO | 1.baseline | 2.contract | 0 | CTL/各卡资源授予 | DIRECT_TASK | AVAILABLE | [C03](#cmd-03) |
| 4 | TODO | 2.contract;3.admission | 2.contract;3.admission | MIG | CTL/WT/PG | EXISTING_SCRIPT | UNSUPPORTED | [C04](#cmd-04) |
| 5 | TODO | 2.contract;3.admission | 4.schema | AUTH | CTL/WT/PG/SVC | DIRECT_TASK | UNSUPPORTED | [C05](#cmd-05) |
| 6 | TODO | 2.contract;3.admission | 4.schema;5.auth | ATT | CTL/WT/PG/OBJ/SVC | DIRECT_TASK | AVAILABLE | [C06](#cmd-06) |
| 7 | TODO | 2.contract;3.admission | 4.schema;5.auth;6.upload | MSG | CTL/WT/PG/OBJ/SVC/DEV | DIRECT_TASK | AVAILABLE | [C07](#cmd-07) |
| 8 | TODO | 2.contract;3.admission | 4.schema;5.auth | DIR/AUTH | CTL/WT/PG/SVC | DIRECT_TASK | AVAILABLE | [C08](#cmd-08) |
| 9 | TODO | 2.contract;3.admission;8.api | 4.schema;5.auth;8.directory | DIR/SSO/AUTH | CTL/WT/PG/SVC/OA | DIRECT_TASK | AVAILABLE | [C09](#cmd-09) |
| 10 | TODO | 2.contract;3.admission | 4.schema;5.auth | SSO/AUTH | CTL/WT/PG/SVC/OA/DEV | DIRECT_TASK | AVAILABLE | [C10](#cmd-10) |
| 11 | TODO | 2.contract;3.admission;8.api;9.api;10.api | 7.message;8.directory;9.inbound;10.sso;13.profile | APP/MSG/SSO | CTL/WT/DEV/SVC | DIRECT_TASK | AVAILABLE | [C11](#cmd-11) |
| 12 | TODO | 2.contract;3.admission;8.api;9.api;10.api | 8.directory;9.inbound;10.sso;13.profile | ADMIN/DIR | CTL/WT/WEB/SVC | DIRECT_TASK | AVAILABLE | [C12](#cmd-12) |
| 13 | TODO | 2.contract;3.admission | 4.schema;5.auth;6.upload;7.message | PROFILE/APP/ADMIN | CTL/WT/PG/OBJ/SVC/WEB/DEV | DIRECT_TASK | AVAILABLE | [C13](#cmd-13) |
| 14 | TODO | 2.contract;3.admission;7.api;11.api | 7.message;11.app;13.profile | MSG/APP | CTL/WT/DEV/PUSH/SVC | DIRECT_TASK | UNSUPPORTED | [C14](#cmd-14) |
| 15 | TODO | 2.contract;3.admission | 4.schema;5.auth;13.profile | CS/ADMIN | CTL/WT/PG/SVC/WEB | DIRECT_TASK | AVAILABLE | [C15](#cmd-15) |
| 16 | TODO | 2.contract;3.admission;15.api;6.api | 4.schema;5.auth;6.upload;15.queue | CS/ATT | CTL/WT/PG/OBJ/SVC/WEB | DIRECT_TASK | AVAILABLE | [C16](#cmd-16) |
| 17 | TODO | 2.contract;3.admission;7.api;15.api | 4.schema;5.auth;15.queue;16.send | CS | CTL/WT/PG/SVC/WEB | DIRECT_TASK | AVAILABLE | [C17](#cmd-17) |
| 18 | TODO | 2.contract;3.admission;15.api;16.api;17.api | 5.auth;15.queue;16.send;17.history | CS/AUTH | CTL/WT/PG/SVC/WEB(双身份) | DIRECT_TASK | AVAILABLE | [C18](#cmd-18) |
| 19 | TODO | 2.contract;3.admission;6.api;15.api;18.api | 6.upload;15.queue;16.send;17.history;18.lifecycle | CS/ATT/ADMIN | CTL/WT/PG/OBJ/SVC/WEB/DEV | DIRECT_TASK | AVAILABLE | [C19](#cmd-19) |
| 20 | TODO | 2.contract;3.admission | 4.schema;5.auth;6.upload;7.message;8.directory;9.inbound;10.sso;11.app;12.admin;13.profile;14.push;15.queue;16.send;17.history;18.lifecycle;19.onboard | 0 | CTL/WT/PG/OBJ/SVC/WEB/DEV/OA/PUSH | DIRECT_TASK | UNSUPPORTED | [C20](#cmd-20) |
| 21 | TODO | 2.contract;3.admission;4.schema | 20.candidate;4.schema | 0 | CTL/WT/PG/OBJ/DEV/WEB | DIRECT_TASK | UNSUPPORTED | [C21](#cmd-21) |
| 22 | TODO | 2.contract;3.admission | 20.candidate;21.recovery | 0 | CTL/WT/PG/OBJ/SVC/WEB/DEV | EXISTING_SCRIPT | UNSUPPORTED | [C22](#cmd-22) |
| 23 | TODO | 2.capacity;3.soak_admission | 20.candidate;22.quality | 0 | CTL/WT/PG/OBJ/SVC/WEB/SOAK | DIRECT_TASK | UNSUPPORTED | [C23](#cmd-23) |
| 24 | TODO | 2.contract;3.review_admission | 20.candidate;21.recovery;22.quality;23.soak | 0 | CTL/REV | DIRECT_TASK | UNSUPPORTED | [C24](#cmd-24) |
| 25 | TODO | 2.contract;3.admission | 24.review | 0 | CTL/USER/OA/DEV/WEB | prepare=DIRECT_TASK;uat=HUMAN_GATE | UNSUPPORTED | [C25](#cmd-25) |
| 26 | TODO | 2.contract | 24.review | 0 | CTL | DIRECT_TASK | AVAILABLE | [C26](#cmd-26) |

台账依赖仅取相应接口/叶：5.auth 不等整个 Cache C09/C10 专项终结；6.api/upload 不等附件全 M1/M2/M3 闭合。若实际 profile 启用 snapshot/READY/多节点，5.auth 的运行闭包必须包含其强制门，不能以局部名称删门。迁移4.schema按受影响表/版本取证，未涉及的 schema 故障不阻断独立叶。共享权限、schema、profile、runner变化仍按影响闭包失效重验。

3.soak_admission 包含全部24h预检，3.review_admission 包含独立reviewer身份和窗口；它们不要求等23/24执行后才确认。20.candidate 表示同候选业务集成receipt闭合，初始候选manifest可早于它准备。允许先执行20的独立子集，但20未通过不得开始依赖完整稳定候选的21/22/23正式门。阶段输出必须记录证据/授权，不仅是台账填值。

**并发预算**：未来获得多 Agent 授权时，协调者+实施者+独立 Reviewer 同时活动总数≤4。可用编排形态为协调者1+实施者2+Reviewer1；Reviewer未活动时可临时3实施者，启动Reviewer前先释放实施槽。独立不等于换一个自报角色：审查者不能是被审变更/证据的实施者。共享 CS/AUTH/MIG/PROFILE 等同写锁串行；同机测试、BEAM/ETS/meck、DB、端口及构建资源未经隔离证明也串行。单执行器只可先实施和整理证据，无法提供独立reviewer时阻断终审；不伪造独立性。

### 复用入口目录 / Existing Entry Points

命令是已定位的模板，本轮未运行。cwd 中 B/A/D 分别指 Step1 冻结的 backend/App/Admin 新 worktree。所有 PG*/应用配置、环境和资源须在3中核实实际值与默认目标，秘密仅保留安全引用；未绑定不启动。模板中的尖括号不可原样执行。

| ID | cwd | 实际入口或源码 | 能力边界 |
|---|---|---|---|
| K-FOCUS | B | `IMBOYENV=local make eunit t=<核实存在且非空的模块>`；替代入口 `make eunit-local t=<核实模块>` | 受影响模块及直接调用方，核配置/共享状态后串行；不是纯函数runner |
| K-MIG | B | `bash scripts/check_migrations.sh` | 仅静态文件/版本配对，不代表空库或非空升级通过 |
| K-PG-MIG | B | `escript scripts/drill_migrate.escript version`、`escript scripts/drill_migrate.escript up` | 已核支持 version/up/down；先绑定全部 PG*，先满足其编译依赖。down仅在演练过且已授权的合成库，不默认执行 |
| K-CS-MIG | B | `bash scripts/customer_service_migration_gate.sh` | libpq连接已绑定后核客服实际ledger；不替代全部业务迁移 |
| K-SQLITE | A | `dart run tool/generate_sqlite_migrations.dart --check`；`bash scripts/run_sqlite_migration_gate.sh --local` | 已有合成历史fixture；缺依赖exit2为BLOCKED，不能当PASS |
| K-BE-FINAL | B | `make compile`；`IMBOYENV=local make eunit`（连续两轮分别执行）；`make dialyze`；`make arch-check`；`make security-gate`；`bash scripts/check_server_zero_crypto.sh` | 主集成者最终窗口；各命令raw RC0，另补当前适用排除套件 |
| K-APP | A | `flutter analyze`；`flutter test` | 真实平台/正式构建另绑 C11，不以单测代签 |
| K-ADMIN | D | `bun run test`；`bun run lint`；`bun run typecheck`；`bun run deadcode`；`bun run build` | test真实展开为bun test --isolate，不改裸bun test |
| K-WIDGET | D | `bun run build:widget`；`bun run verify:widget`；`bun run verify:widget-pairing` | 正式Seat/Widget/manifest摘要配对 |
| K-WEB | D | `bun run test:e2e:customer-service:real`；`bun run test:e2e:customer-service:p2` | 真实PG/Garage/backend，先读两个实际Playwright配置并绑定全部目标 |
| K-SEAT | D | `SC153_E2E_EXECUTE=1 bunx playwright test --config=playwright.customer-service-seat-embed.config.ts` | 先读 `tests/e2e/customer-service-seat-embed/fixtures/harness-seat-embed.sh`；不继承默认服务授权，不代物理扫码 |

### 直接任务及人工步骤 / Scoped Procedures

下列是可供未来已授权执行器直接执行的任务说明，不是新的CLI/脚本名称。使用K入口前均满足C03；所有执行都沿卡内业务断言和receipt规则。AVAILABLE的17卡只记录ECC可传scope这一元信息（规划类1/2/3/26、既有修复6–12/15–19、行为对齐13），不强制路由，也不让wrapper内部的commit/review越过当前授权。

<a id="cmd-01"></a>
**C01 → Step 1**：只读重采三仓规则、WIP、remote与旧owner；获明确建新worktree/执行授权后，逐仓用 `git remote -v` 核对来源，使用 `git fetch <已核实remote> refs/heads/main:refs/remotes/<已核实remote>/main` 取main，用 `git rev-parse <对应remote-main>^{commit}` / `git rev-parse <冻结SHA>^{tree}` 固定身份；再 `git worktree add --detach <新隔离路径> <冻结SHA>`。所有占位先换成核实值，创建回执与冻结向量入baseline。缺clone/远端权限不得从旧专项工作树顶替；不要checkout/reset原main。冻结后版本只按本run授权变更更新，不跟remote漂移。

<a id="cmd-02"></a>
**C02 → Step 2**：逐个读S1–S6来源及归档，按五字段叶建原合同/修订映射和2.contract；对CX00-A05核容量确认来源并输出2.capacity或精确待确认项。缺源用materials-index/Git blob定位，仍缺就保留父占位和NEXT_UNLOCK；不让某源缺口阻止已证独立合同准备。无业务脚本可替代合同裁决。

<a id="cmd-03"></a>
**C03 → Step 3**：按单卡操作核授权、旧owner三分流、历史累计预算、实际资源与配置；只在已准许的自有隔离目标做连接前/握手目标核验。逐命令填cwd/argv/runner hash、模块列表、expected exit、timeout、日志和业务oracle。分别产出3.admission、3.soak_admission、3.review_admission；无有效执行器或reviewer如实列缺。原24h采样/负载入口、真机argv和升级/恢复目标未绑定前保持BIND_REQUIRED，不造命令；预算疑点只阻断相关操作。

<a id="cmd-04"></a>
**C04 → Step 4**：复用K-MIG、K-PG-MIG、K-CS-MIG和K-SQLITE；所需编译沿K-BE-FINAL的编译入口，不提前执行最终全门。静态配对后，在独占合成PG分别验证空库、支持旧版本非空fixture的升级、二次up、关键迁移故障后的恢复读取；版本/ledger/行及关系与fixture预期精确对拍，重复升级不得重复数据。SQLite按现有gate核历史fixture。故障注入仅限已授权演练副本，不修改已执行迁移正文；具体旧版本、注入方法和恢复argv由3绑定，不足则只运行已准入子场景。K-MIG RC0不能签整卡。全部适用迁移叶合格才输出4.schema。ECC不适用不要求人工接棒。

<a id="cmd-05"></a>
**C05 → Step 5**：按三分流确认旧owner、main祖先和新授权；只读追S3的实际调用链/receipt，列本profile所需COMMIT/ROLLBACK、撤权、QR并发/重启、PG故障、snapshot/READY/节点门；复用相应现有suite并经K-FOCUS按非空模块定向验证。静态审查可先行，运行等待4.schema及相应资源，缺陷交当前有权唯一owner，不等待不存在的旧会话；审查者不以实现者自测冒充独审。相关权限/QR/恢复叶receipt合格才输出5.auth。

<a id="cmd-06"></a>
**C06 → Step 6**：读S4执行合同和 `scripts/attachment_lifecycle_v5_runner.py` 的实际admit/verify/accept参数与授权含义，只在同范围兼容时复用，不将V2授权伪装为S4授权。复现卡内真实对象、旧PUT、撤权和清理问题后最小修复，定向验证直接调用方；无缺陷仅补证。公开上传接口/错误码/driver指纹为6.api，实际PUT/confirm/Range/字节及授权叶为6.upload；清理专项其余门继续留26，不挪用完整专项PASS。

<a id="cmd-07"></a>
**C07 → Step 7**：从现有msg_ack/msg_c2g及调用链选择实际suite用K-FOCUS复现；固定消息/成员API为7.api，待4.schema/5.auth/6.upload所需叶就绪后做双端消息、ACK、群治理和崩溃恢复，集合/顺序/通知及权限断言齐全才7.message。不恢复已暂停E2EE研发。

<a id="cmd-08"></a>
**C08 → Step 8**：沿现有sync/3、claim_login_tx/4及directory logic/PG/handler suites用K-FOCUS；先固定8.api，再用合成A/B组织和同一实例账号验证增量/幂等/事务/Owner及A离职。A撤权与B/个人可访问同时断言，账号停用和来源撤销单独fixture，结果绑定身份判定表及S2具体条款才8.directory。

<a id="cmd-09"></a>
**C09 → Step 9**：沿现有OA login logic/PG/HTTP和关联入口定向验证；8.api固定后可准备，真实登录需表中运行依赖。按票据绑定字段和S2 §8.3.1验证三种进入顺序、双边身份证明、过期/重放/发行撤销/账号停用，以及仅退A仍允许有效B/个人访问。对每个拒绝前后比较用户/映射/会话/目录；不将退A直接当全局票据失效。9.api是接口指纹，9.inbound为真实运行receipt；客户OA另签。

<a id="cmd-10"></a>
**C10 → Step 10**：沿当前SSO/context/HTTP兼容suite与App Bridge实现定向验证，先固定10.api，再证真实getAuthCode→兑换单消费及授权/账号状态。mapping可选、organization_id业务归属与大整数/null分别测试；退A不抹掉B/个人权限，绑定失效来源的code按具体出站合同判定。拒绝不留新会话/映射/业务数据；真实客户OA未接通不得冒称10.sso对应客户叶通过。

<a id="cmd-11"></a>
**C11 → Step 11**：接口8/9/10.api固定即可在独立App槽准备交互/定向测试及K-APP；11.api登记App路由/接口使用版本。真实旅程等待相应acceptance_dependencies和DEV。读取App规则、 `docs/build-and-release.md` 与 `integration_test/` 后在3绑定真实gzCustomer Android及macOS原生完整argv、设备ID与产物摘要；本轮未找到可直接代填的广州真端完整argv，保持BIND_REQUIRED，不拿standard命令代替。双入口、中文输入/长ID/切账号/组织及注销回调逐平台签11.app。

<a id="cmd-12"></a>
**C12 → Step 12**：接口固定后准备既有Admin目录/接入状态和相邻测试，复用K-ADMIN中受影响门；真实WEB与后端条件就绪才跑分页/TSID/冲突/停用和失败态。目录可见不等于消息可读，不能要求手工SQL开通；输出12.admin对应叶receipt。

<a id="cmd-13"></a>
**C13 → Step 13**：按2.contract固定phase1/gzCustomer白名单，在单写者PROFILE/APP/ADMIN锁内复用现有feature/policy/manifest生成与校验；以原P1-MOM/P1-LOC叶补朋友圈/附近正例与ACL负例。配置/旧缓存/worker/API/WS/深链一起验证，依赖的消息/附件/schema就绪后才签13.profile；保持历史密文和已有General/公告依赖。

<a id="cmd-14"></a>
**C14 → Step 14**：自动整理平台×前后台/杀进程/弱网/点击/DND/token/切账号矩阵和自有设备名单；在授权真实推送通道、DEV、7.message/11.app/13.profile就绪后，以现有App通知接收与路由流程实际发到自有设备，逐格记录到达/会话/权限事实。Android/macOS适用能力按合同分别验；真实provider凭据/设备/权限缺失只阻断相关叶，不能用mock或ECC替身签14.push。

<a id="cmd-15"></a>
**C15 → Step 15**：按Step5三分流保护旧CX成果和资源；2的CX接口合同固定即可准备UI/API/SQL断言，单CS写者最小修复首次开通/队列搜索与待回复。发布15.api及查询/统计语义版本，待相关schema/auth/profile就绪后用K-WEB/K-SEAT和SQL对拍，精确记录CX-01/CX-02及继承叶，输出15.queue。

<a id="cmd-16"></a>
**C16 → Step 16**：15.api与6.api稳定后准备中文IME、两访客草稿、附件、未知发送结果和同payload重试测试；共享CS/ATT串行。实际验收等6.upload/15.queue及授权/schema相关叶，复用K-WEB/K-SEAT并以唯一业务消息和对象hash作oracle；API/幂等合同记16.api，运行结果记16.send。

<a id="cmd-17"></a>
**C17 → Step 17**：15.api/7.api稳定后准备200条长历史/跨页锚点/可见ACK/SSE重复乱序恢复场景，按共享CS锁最小修复；真集成等15.queue/16.send及权限/schema。K-WEB/K-SEAT与UI/DB业务集合对拍，禁止截图代oracle；输出17.api和17.history。

<a id="cmd-18"></a>
**C18 → Step 18**：基于15/16/17.api准备CAS/容量/取消/转接/关闭断言，CS/AUTH单写者；运行等对应叶，使用两个独立真实坐席身份竞争，K-WEB/K-SEAT复用现有harness。409、取消零写、旧owner撤权及历史/审计一致，接口合同为18.api，验收为18.lifecycle。

<a id="cmd-19"></a>
**C19 → Step 19**：相关api固定后准备从零开通、真实生成两种snippet、Widget全态/credential/origin以及运营统计口径，复用K-WIDGET/K-WEB/K-SEAT。真实链待运行依赖就绪，物理扫码必须自有Android真机操作，不能以辅助API代扫；统计/评价和SQL按冻结时区对账，记录19.onboard。缺扫码设备仅阻断该叶，保留其他准备。

<a id="cmd-20"></a>
**C20 → Step 20**：先按矩阵与requirements生成逐叶执行次序，复用K-WEB/K-SEAT、K-FOCUS和C11/C14真端流程；对已有正式构建产物冻结candidate向量，按已解锁业务子集执行UI→HTTP/WS/SSE→PG→对象→另一端可见。表中4–19仅取该叶相关运行输出，独立叶可先跑；全部LOCAL_REQUIRED集成叶及候选绑定完整才20.candidate，不拼分支绿灯。ECC无scope不妨碍直接执行。

<a id="cmd-21"></a>
**C21 → Step 21**：在20.candidate稳定和3确认目标后用同正式产物新装/旧升级；复用现有 `scripts/backup_pg.sh`、`scripts/backup_garage.sh`、`scripts/restore_pg.sh` 与既有部署恢复手册。先核全部脚本默认目标、参数和metrics/外向副作用，冻结源/目标/备份hash再运行；restore目标必须是本run另一合成目标，不能复制生产默认值，不设置生产绕过开关。具体完整恢复argv/对象恢复步骤待3绑定，缺失只阻断对应恢复叶。恢复后实际登录、目录、消息、对象字节及RTO/RPO对拍，不以备份exit0签21.recovery。

<a id="cmd-22"></a>
**C22 → Step 22**：方案/排除套件清单可先准备；20.candidate/21.recovery和独占窗口就绪后由主集成者统一执行K-BE-FINAL、K-SQLITE、K-APP、K-ADMIN、K-WIDGET及原合同/当前CI适用专项和真端门。EUnit两轮分别留exit/执行数；原始Dialyzer RC0不可用baseline替代。构建产物或候选改变先回查20/21受影响旅程，相关全门重验；全量和专项完整才22.quality。

<a id="cmd-23"></a>
**C23 → Step 23**：在2.capacity、3.soak_admission不足时仅做负载/观测方案准备并登记最小缺口；20.candidate/22.quality通过、持续执行器与独占窗口确认后，按测量合同热身30min并运行连续24h。现有负载/采样入口的确切文件/argv尚待3现场绑定，不伪称已有soak命令；可由获授权直接执行器使用经核实的既有方式执行，不能让短会话代替持续能力。完整原始数据复算指标和故障恢复才23.soak；失效重开整窗。

<a id="cmd-24"></a>
**C24 → Step 24**：REV可提前只读审合同/台账结构；最终签发前等待20–23的required完整。非实施reviewer从原来源重算exact-set/子叶、候选/产物/日志hash、命令exit/执行数及正负业务断言；按卡内单故障变异逐项验证拒绝，不自修所签证据。没有适用ECC scope不需人工接棒；没有真实独立reviewer则BLOCKED_REVIEWER，不能由执行者改名自签。输出24.review。

<a id="cmd-25"></a>
**C25 → Step 25**：prepare（DIRECT_TASK）可自动整理本地可审草包/缺口；24.review通过后封同候选正式交付包并登记技术结论。uat（HUMAN_GATE）由用户本人无指导执行卡内五项任务并逐项确认，真实客户OA/平台/通道按授权联调并单独签收；记录版本hash、成功/失败、耗时、意见和整改重验。prepare成功不代uat；不自动发邀请、填联系人或执行生产。最终TASK_PASS仅在本卡全部Required含用户/外部确认满足时成立。

<a id="cmd-26"></a>
**C26 → Step 26**：2的已知来源可先滚动列余额，精确保留每个原Required、当前证据、owner/三分流状态、历史预算及NEXT_UNLOCK；最终余额与24独审一致。五专项各自分账，未证明不影响广州的缺陷回到相应阻断集合；只写既有台账，不重开整仓研发或覆盖历史verdict。

### 首次执行验证卡：Step 4 / First Execution Rehearsal

指定已有 Step 4，不新增第27卡；它无需旧Cache owner、真实OA、推送或24h窗口，前置为1.baseline、迁移相关2.contract/3.admission及独占合成PG/App构建槽。先从静态子场景开始，但只跑K-MIG不能称整卡完成。

1. **准入**：在未来执行授权下完成Step1新main起点，限定Step4全部迁移场景及旧版本fixture、资源创建/权限、累计预算、runner和回滚路径。缺PG等条件可先完成只读计划与静态门，整卡保持PREPARE_ONLY；未绑定的升级/故障恢复argv不启动。
2. **执行与业务断言**：按C04/K入口顺序执行空库、非空旧版本、重复升级、关键故障恢复及SQLite gate；独占资源串行。恢复后原ID/成员/消息/对象引用关系保留、重复升级无重复写、失败回退/恢复后可读；命令exit与这些断言同时核验。
3. **证据记录**：按本文receipt保存每子场景candidate/tree、源码/runner/fixture/配置hash、PG目标、argv、raw exit/执行数、ledger/数据对拍及日志hash；未跑子场景明确NOT_RUN，不用一个总日志签全卡。
4. **独立复核**：未来被授权且未实施本卡的reviewer重算receipt与业务对拍，至少拒绝错candidate/缺场景/非零却PASS；这是本卡复核，不提前签Step24整计划终审。缺reviewer仅阻断签收，保留真实执行证据。
5. **中断续跑**：在完整子场景receipt与资源checkpoint后暂停本run执行进程，再从该点重新读取授权/租约/预算、进程身份、候选与hash；不杀不明进程、不复用未知资源。中断场景记INTERRUPTED而非PASS，核对PG ledger/dirty及数据，必要时从本run备份重建并重跑该场景；已完整且指纹未变的独立子场景可复用。预算不清零。全部场景及独立复核完成才TASK_PASS；无法安全续跑就只阻断本卡并给NEXT_UNLOCK。

该路径本轮只明确，不创建资源、不运行迁移、不做中断演练、不启动reviewer。
