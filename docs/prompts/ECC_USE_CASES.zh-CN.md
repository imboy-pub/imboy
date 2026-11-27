# IMBoy ECC 使用案例 / Backend

> 本文是可直接复制到 Codex 会话的 ECC 使用案例库。每个案例均可独立复制，以 `$skill-name` 开头。原 72 个工程案例各提供一个提示词；新增功能场景提供 A 新增功能、B 优化现有功能、C 测试现有功能三个独立提示词。
>
> **使用约定**
> - 在已安装 ECC 的 Codex 会话中，复制单个案例的完整代码块执行。
> - 先确认当前工作目录和目标仓库；跨仓案例需明确各仓库实际 checkout 路径。
> - 仓库内 AGENTS.md、CLAUDE.md、项目架构文档、测试执行合同和安全规则优先于 ECC 通用建议。
> - 提示词中的“只读”案例不得修改文件；实施案例仅允许修改当前任务必需文件。
> - 默认不得 commit、push、合并、部署、执行生产写入或接触真实用户数据；需要时另行授权。
> - 不得通过删除/弱化测试、扩大 skip、关闭门禁或伪造证据制造 PASS。
> - 报告实际执行的命令、退出码、结果、未执行项和证据位置；未执行不得声称通过。
>
> **案例编号全局唯一**：ECC-001 至 ECC-121。四个仓库按职责分配，后端主仓提供统一索引。

## 本仓库范围

本文件包含后端与跨仓工程案例 44 个，以及 IMBoy 公共功能场景/新模型复审案例 24 个；合计 68 个（ECC-001、005–022、043–045、051–089、110–116）。
## 案例清单

### 项目理解与架构分析

#### ECC-001 · 后端完整架构梳理

**Skills：** `codebase-onboarding`

```text
$codebase-onboarding

只读分析当前 imboy 后端仓库。梳理 Erlang/OTP 技术栈、应用启动链路、Supervisor 树、HTTP/WS 路由、Handler → Logic → DS → Repo 四层架构、数据库、业务模块和测试体系。输出真实文件路径、关键调用链、模块职责、现有架构约束和风险。优先复用现有架构地图，不创建或修改文件。
```

#### ECC-005 · 单聊消息完整调用链

**Skills：** `change-impact`

```text
$change-impact

只读追踪单聊消息从客户端发送、后端 WebSocket 接收、消息校验、业务处理、PostgreSQL 持久化、在线投递、离线投递、ACK 到客户端状态更新的完整链路。逐项定位真实代码，识别可靠性边界、重试、幂等性、多设备和潜在丢消息风险。区分代码事实、推断和待验证项。
```

#### ECC-006 · WebSocket Action 注册和扩展

**Skills：** `change-impact` · `contract-first`

```text
$change-impact $contract-first

分析 WebSocket Action 的注册、路由、分派和处理机制，重点检查 imboy_ws_action_registry、message_router_logic 及相关 Logic 模块。评估新增 Action 的最小改动范围、协议兼容性、认证授权、输入校验和测试要求。只读输出，不修改代码。
```

#### ECC-007 · 三仓依赖关系分析

**Skills：** `contract-first`

```text
$contract-first

分析 imboy、imboy-flutter、imboy-admin-frontend 三仓依赖关系。以实际代码、OpenAPI、API 契约、错误码、TSID/EntityId 和 WebSocket 消息格式为依据，建立提供方、消费方和契约真源矩阵。指出字段/枚举/可空性/错误语义漂移及尚未机器验证的接口。核对各仓实际 HEAD。
```

#### ECC-008 · 文档与代码一致性审计

**Skills：** `docs-governance` · `living-docs-governance`

```text
$docs-governance $living-docs-governance

只读审计后端文档。对比 AGENTS.md、CLAUDE.md、README、架构地图、模块文档、测试说明与实际代码。找出过期命令、错误路径、已完成事项仍标记未完成、文档矛盾和重复真源。优先复用 docs/README.md 的分类规则，输出按风险排序的问题，不自动重写文档。
```

### Erlang/OTP 后端

#### ECC-009 · 新增 REST API

**Skills：** `api-design` · `contract-first` · `tdd-workflow`

```text
$api-design $contract-first $tdd-workflow

在 IMBoy 后端实现指定 REST API。先检查现有路由、Handler、Logic、DS、Repo 和 OpenAPI 约定，定义接口契约，先写失败测试，再实现最小功能。遵守四层架构、TSID、统一错误码、认证授权和现有命名。完成后运行相关测试、契约和架构门禁。仅修改本任务必需文件。
```

#### ECC-010 · API 错误码和分页治理

**Skills：** `api-design` · `contract-first`

```text
$api-design $contract-first

检查 REST API 的错误码、分页参数、返回结构和字段命名。识别不一致及兼容性问题，核对 OpenAPI 与真实 Handler/Logic 实现及客户端消费。输出统一建议和受影响消费者；未经批准不改变公共 API。
```

#### ECC-011 · 新增 WebSocket Action

**Skills：** `contract-first` · `tdd-workflow`

```text
$contract-first $tdd-workflow

为 IMBoy 新增指定 WebSocket Action。先研究 Action Registry、消息分派、协议字段和权限机制，定义输入输出契约，编写正向、非法输入、权限失败、重复请求和兼容性测试，再实施。复用现有注册机制，不得硬编码旁路分派。
```

#### ECC-012 · 单聊 ACK 和可靠投递

**Skills：** `change-impact` · `tdd-workflow` · `test-collaboration`

```text
$change-impact $tdd-workflow $test-collaboration

审查单聊消息 ACK、重发、离线投递和设备维度确认机制。覆盖重复投递、ACK 丢失、客户端重连、消息乱序、多设备同步和节点切换。先建立可复现测试，再修复确认的缺陷；保留现有 QoS 语义。
```

#### ECC-013 · 群聊消息时序治理

**Skills：** `change-impact` · `module-regression`

```text
$change-impact $module-regression

分析群聊消息 conv_seq、accepted_at、消息排序、成员可见性和多节点投递机制。找出乱序、重复、消息缺失和时间线不一致路径。输出影响模块、数据不变量和回归命令；不要推断未实测的分布式保证。
```

#### ECC-014 · 消息撤回和编辑时间窗

**Skills：** `ai-regression-testing` · `tdd-workflow`

```text
$ai-regression-testing $tdd-workflow

审查消息撤回、编辑及可信时间窗逻辑。验证边界时间、低可信时间回退、服务端时间、重复请求、客户端 ACK 和多端状态同步。先写能复现问题的测试，再进行最小修复；保留历史协议兼容。
```

#### ECC-015 · 消息投递性能和背压

**Skills：** `latency-critical-systems` · `benchmark-optimization-loop`

```text
$latency-critical-systems $benchmark-optimization-loop

分析单聊、群聊消息投递热路径。测量吞吐量、p50/p95/p99 延迟、队列积压、进程数量、RPC 开销、重试次数和数据库耗时。先建立可重复基线，每次只验证一个优化假设，不允许通过丢消息、跳过 ACK 或降低可靠性换取性能。未获得环境授权时只输出测试计划。
```

#### ECC-016 · OTP Supervisor 容错分析

**Skills：** `change-impact` · `architecture-decision-records`

```text
$change-impact $architecture-decision-records

审计 OTP Supervisor 树、Worker 生命周期、重启策略和进程间依赖。分析子进程崩溃、重复重启、依赖不可用和节点重启时的影响。给出故障传播图、恢复机制和测试缺口。若涉及架构决策，先提供 ADR 草案，不自动写入。
```

#### ECC-017 · 四层架构越层调用检查

**Skills：** `module-governance` · `module-regression`

```text
$module-governance $module-regression

审计 Handler → Logic → DS → Repo 四层架构。通过真实调用关系和现有 check_module_boundaries 脚本检查跨层调用、循环依赖和例外。识别新增违规与历史例外，输出最小治理方案；不得为消除告警而随意搬迁业务逻辑。
```

#### ECC-018 · PostgreSQL 慢查询优化

**Skills：** `benchmark` · `latency-critical-systems`

```text
$benchmark $latency-critical-systems

审查 PostgreSQL 慢查询和热点 SQL。结合 Repo 层真实 SQL、索引定义、执行计划和现有性能测试，识别全表扫描、重复查询、低效分页和锁竞争。先建立可复现性能基线，再提出优化方案。遵守全栈不引入 Redis 的项目约束。
```

#### ECC-019 · 数据库迁移安全审查

**Skills：** `database-migrations` · `change-impact`

```text
$database-migrations $change-impact

审计 priv/migrations 中数据库迁移。检查 up/down 配对、锁风险、历史数据兼容、索引创建、expand/contract 顺序和蓝绿发布兼容性。遵循仓库迁移编号与 check_migrations.sh 等现有门禁。不要执行生产迁移，不修改已发布迁移。
```

#### ECC-020 · E2EE 服务端零密码学约束

**Skills：** `security-review` · `ai-regression-testing`

```text
$security-review $ai-regression-testing

审计服务端 E2EE 零密码学约束。检查私钥、明文、密钥派生、加解密和敏感数据是否进入服务端处理路径。使用现有 zero-crypto 检查脚本和相关测试验证。不得通过关闭检查、扩大白名单或泄漏敏感数据掩盖问题。
```

#### ECC-021 · API 鉴权、权限和限流

**Skills：** `security-review` · `tdd-workflow`

```text
$security-review $tdd-workflow

审计 REST、WebSocket、Admin API 的认证、授权、资源归属、越权访问、频率限制和输入验证。重点关注用户身份、群成员权限、管理员权限和跨组织数据访问。对确认的问题补充自动化安全回归测试；未经授权不执行攻击性或生产测试。
```

#### ECC-022 · 附件上传与下载安全

**Skills：** `security-review` · `change-impact`

```text
$security-review $change-impact

审计附件上传、对象存储、访问 URL、下载授权、文件类型、文件大小和生命周期。检查现有存储适配及附件 registry。验证跨用户访问、过期链接和未授权下载问题。输出代码证据与回归建议，不下载或披露真实附件和凭据。
```

### 三仓契约与 geo_regions

#### ECC-043 · OpenAPI 契约漂移检查

**Skills：** `contract-first` · `api-design`

```text
$contract-first $api-design

检查后端 OpenAPI 真源、生成产物和 Flutter/Admin 消费端。验证请求响应字段、可空性、错误码、枚举、认证和版本兼容。运行仓库已有 contract gate，输出真实漂移位置、消费方和修复顺序；核对三仓实际 SHA。
```

#### ECC-044 · 三端 EntityId/TSID 一致性

**Skills：** `contract-first` · `test-collaboration`

```text
$contract-first $test-collaboration

审计后端、Flutter 和 Admin 的 TSID/EntityId 定义、序列化、解析和比较。检查 64 位精度、JSON 整数、字符串映射、数据库字段和 API 传输。找出类型不一致并建立可执行契约测试；不得用 JavaScript Number 或 parseInt 处理 TSID。
```

#### ECC-045 · 错误码和枚举一致性

**Skills：** `contract-first` · `tdd-workflow`

```text
$contract-first $tdd-workflow

审计后端错误码、业务枚举及 Flutter/Admin 镜像定义。以后端权威定义为准，检查遗漏、废弃值、错误映射和前端未处理分支。运行现有 contract gate，不允许静默忽略未知枚举。
```

### 测试、文档、安全和运维

#### ECC-051 · 建立后端测试资产地图

**Skills：** `test-collaboration`

```text
$test-collaboration

为后端建立测试资产地图。检查 EUnit、Common Test、integration、performance、stress 和 CI。识别关键业务规则、稳定 TEST-ID、测试入口、外部依赖和覆盖缺口。优先复用 test/README.md 和现有测试索引，不创建重复测试说明。
```

#### ECC-052 · 历史缺陷自动回归

**Skills：** `ai-regression-testing` · `tdd-workflow`

```text
$ai-regression-testing $tdd-workflow

检查当前修复涉及的历史缺陷、测试失败记录和已有回归用例。找出尚未被自动化保护的重要问题，为每个问题建立最小可复现测试。必须证明新测试能在修复前失败、修复后通过；不得删除或弱化既有测试。
```

#### ECC-053 · 下游模块回归分析

**Skills：** `module-regression` · `change-impact`

```text
$module-regression $change-impact

分析本次代码变更对其他模块的影响。基于真实调用关系、公开接口和跨仓依赖，确定必须重跑的测试。输出变更模块、下游消费者、精确回归命令、执行结果和未验证边界。
```

#### ECC-054 · CI 假绿和跳过测试审计

**Skills：** `test-collaboration` · `verification-loop`

```text
$test-collaboration $verification-loop

审计 GitHub Actions 测试门禁。检查 continue-on-error、条件跳过、缺少报告、零执行、异常退出码、未批准 skip 和证据未对账。识别哪些绿灯不能证明业务通过。不得修改门禁以降低通过标准。
```

#### ECC-055 · 文档完整性审计

**Skills：** `docs-governance`

```text
$docs-governance

审计后端文档的相对链接、重复内容、孤立文档、过期 ADR、错误引用和目录索引。遵循 docs/README.md 与 documentation-system 规则，先识别权威文档，不创建第二套文档体系。输出可自动修复和需人工确认的问题。
```

#### ECC-056 · 记录架构决策 ADR

**Skills：** `architecture-decision-records`

```text
$architecture-decision-records

针对当前架构决策整理背景、约束、备选方案、选择理由、正负面影响和回滚风险。先读取现有 ADR，避免重复记录。先展示完整 ADR 草案，未经确认不要创建或修改文件。
```

#### ECC-057 · 长期项目文档治理

**Skills：** `living-docs-governance`

```text
$living-docs-governance

为后端建立长期文档治理方案。盘点 AGENTS.md、CLAUDE.md、README、架构地图、路线图、状态文档和历史记录。映射规则、地图、状态和历史四类角色。优先复用现有文件，不创建平行真源；遵循 docs/README.md 对执行计划和临时证据的入库限制。
```

#### ECC-058 · 全项目安全审查

**Skills：** `security-review`

```text
$security-review

对后端进行分层安全审查。覆盖认证授权、会话、E2EE、TSID、文件上传、附件访问、数据库、日志、敏感配置和跨组织隔离。每个发现必须提供真实文件和代码证据，区分已证实漏洞与待验证风险。
```

#### ECC-059 · AI Agent 安全治理

**Skills：** `agent-security-hardening` · `agent-architecture-audit`

```text
$agent-security-hardening $agent-architecture-audit

审计 IMBoy AI Agent、Bot、MCP 和工具调用体系。分析 Prompt Injection、工具越权、外部内容污染、跨用户数据泄漏、敏感信息输出和自动执行风险。明确每个工具的权限、身份校验、输入校验和审计证据。
```

#### ECC-060 · MCP 依赖供应链审计

**Skills：** `mcp-dependency-review`

```text
$mcp-dependency-review

只读审计仓库中的 MCP 配置。检查服务器来源、版本固定、可变依赖、启动命令、环境变量、凭据暴露和执行权限。不得启动未知 MCP 服务，不安装未经审查的依赖，不输出秘密值。
```

#### ECC-061 · 合并后的生产风险审计

**Skills：** `production-audit`

```text
$production-audit

针对当前已合并到 main 的代码进行生产风险审计。检查实际 HEAD、最近变更、CI、数据库迁移、环境配置、部署脚本、健康检查、日志和回滚路径。输出 SHIP/BLOCK 建议及证据。只读，不实际部署。
```

#### ECC-062 · 蓝绿部署和数据库迁移审查

**Skills：** `deployment-patterns` · `change-impact`

```text
$deployment-patterns $change-impact

审查 scripts/imboy-deploy.sh 蓝绿发布流程。检查本地 HEAD、新版本构建、expand migration、旧节点停止、数据库备份、迁移台账、健康检查、流量切换和回滚边界。以现有脚本为准，不套用通用发布模板。只读分析。
```

#### ECC-063 · Docker 和安装脚本治理

**Skills：** `docker-patterns` · `production-audit`

```text
$docker-patterns $production-audit

审计 Docker Compose、Dockerfile、安装包、安装脚本和环境配置。检查依赖健康检查、启动顺序、数据持久化、权限、镜像固定、升级和失败恢复。遵守全栈禁 Redis及现有部署架构，不复制 ECC 示例中的基础设施配置。
```

#### ECC-064 · PostgreSQL 备份恢复演练方案

**Skills：** `production-audit` · `database-migrations`

```text
$production-audit $database-migrations

审查 PostgreSQL 备份、恢复和恢复冒烟流程。检查备份完整性、扩展依赖、附件存储一致性、恢复顺序、恢复后校验和 RTO/RPO 证据。先输出隔离环境演练计划，不操作生产数据库。
```

#### ECC-065 · GitHub PR 和 CI 管理

**Skills：** `github-ops`

```text
$github-ops

检查 imboy-pub/imboy 的 GitHub PR、CI 失败、未解决 Review、过期分支和 Release 状态。结合真实 GitHub 数据分类处理优先级。先输出处理建议，不自动合并、关闭 PR 或删除分支。
```

#### ECC-066 · Git Worktree 和分支清理

**Skills：** `git-workflow`

```text
$git-workflow

审计本地 worktree、分支、远端分支和 GitHub PR 关系。识别已合并、过期、仍有 WIP 或被其他任务使用的工作区。输出逐项清理清单和安全检查命令。未经确认不删除文件、分支或 worktree。
```

### 性能优化与 Agent 协作

#### ECC-067 · WebSocket 性能基线

**Skills：** `benchmark` · `latency-critical-systems`

```text
$benchmark $latency-critical-systems

为 IMBoy WebSocket 建立性能基线。覆盖连接建立、认证、心跳、消息发送、接收、ACK、广播和断线重连。记录并发连接数、吞吐量、p50/p95/p99、CPU、内存、进程数和错误率。区分已有实测数据与待执行测试，不得宣传未经实测的百万连接能力。
```

#### ECC-068 · 性能持续优化

**Skills：** `benchmark-optimization-loop` · `latency-critical-systems`

```text
$benchmark-optimization-loop $latency-critical-systems

针对当前性能瓶颈建立基线和可重复优化循环。先定位瓶颈，再提出单变量优化假设，逐一实施和测量。保留正确性、消息可靠性和安全约束。记录命令、环境、结果、变化幅度及回归证据；生产负载测试需另行授权。
```

#### ECC-069 · CI 和部署耗时优化

**Skills：** `benchmark-optimization-loop` · `parallel-execution-optimizer`

```text
$benchmark-optimization-loop $parallel-execution-optimizer

分析 GitHub Actions、scripts 和部署链路耗时。记录依赖安装、编译、测试、打包、上传和远程部署各阶段时间。找出重复工作和可安全并行环节。优化后证明发布始终使用最新已授权代码，不能复用过期产物提速。
```

#### ECC-070 · 最多 5 个 Agent 并行开发

**Skills：** `team-agent-orchestration` · `parallel-execution-optimizer`

```text
$team-agent-orchestration $parallel-execution-optimizer

为当前任务设计最多 5 个 Agent 的并行执行方案。先检查依赖关系和共享资源，划分独立 worktree、分支、文件写入范围、负责人、验收命令和合并条件。主 Agent 负责集成和最终验证。遵守仓库 AI 工程任务契约与协调锁；禁止多个 Agent 同时写同一文件或共享数据库。
```

#### ECC-071 · IMBoy AI Agent Runtime 架构审计

**Skills：** `agent-architecture-audit` · `agent-security-hardening`

```text
$agent-architecture-audit $agent-security-hardening

全面审计 IMBoy AI Agent Runtime。检查 Prompt、上下文、记忆、模型调用、工具注册、MCP、权限、执行器、重试、错误处理和结果持久化。追踪用户输入到工具副作用的完整链路，识别隐藏重试、工具越权、记忆污染和多租户隔离风险。
```

#### ECC-072 · AI 编码 Agent 能力对比

**Skills：** `agent-eval` · `benchmark-optimization-loop`

```text
$agent-eval $benchmark-optimization-loop

设计 IMBoy 专用 Coding Agent 评测集，比较 Codex、Claude Code 等 Agent 在 Erlang、Flutter、React、跨仓契约、Bug 修复、测试治理和复杂重构方面表现。固定仓库提交、任务输入、验收命令和环境，记录成功率、耗时、成本、回归缺陷及重复运行一致性。先制定评测方案，不编造测试成绩。
```


## IMBoy 公共业务场景复刻（Backend / 跨端业务真源）

> 本组面向客户业务场景复刻。每个场景提供三条独立提示词：A 新增功能；B 优化现有功能（重点适用于新模型复审旧 AI 代码）；C 测试现有功能。将提示词中的“客户”替换为具体客户、行业、角色和目标流程。



#### ECC-073 · 客户私聊与多设备消息

**场景：** 个人即时通讯

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。

基于已确认的个人即时通讯客户方案，在 IMBoy 后端实施明确批准的差异项。先核对路由、契约、四层架构、conv_seq、设备 ACK、离线补投和三端消费。先补契约与失败测试，再最小实现。不得改变消息顺序、幂等、投递和兼容语义。运行定向 EUnit/CT、契约和架构门禁；提交前停止，不 commit/push。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。

基于已确认的个人即时通讯客户方案，在 IMBoy 后端实施明确批准的差异项。先核对路由、契约、四层架构、conv_seq、设备 ACK、离线补投和三端消费。先补契约与失败测试，再最小实现。不得改变消息顺序、幂等、投递和兼容语义。运行定向 EUnit/CT、契约和架构门禁；提交前停止，不 commit/push。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。

基于已确认的个人即时通讯客户方案，在 IMBoy 后端实施明确批准的差异项。先核对路由、契约、四层架构、conv_seq、设备 ACK、离线补投和三端消费。先补契约与失败测试，再最小实现。不得改变消息顺序、幂等、投递和兼容语义。运行定向 EUnit/CT、契约和架构门禁；提交前停止，不 commit/push。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。

基于已确认的个人即时通讯客户方案，在 IMBoy 后端实施明确批准的差异项。先核对路由、契约、四层架构、conv_seq、设备 ACK、离线补投和三端消费。先补契约与失败测试，再最小实现。不得改变消息顺序、幂等、投递和兼容语义。运行定向 EUnit/CT、契约和架构门禁；提交前停止，不 commit/push。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。

基于已确认的个人即时通讯客户方案，在 IMBoy 后端实施明确批准的差异项。先核对路由、契约、四层架构、conv_seq、设备 ACK、离线补投和三端消费。先补契约与失败测试，再最小实现。不得改变消息顺序、幂等、投递和兼容语义。运行定向 EUnit/CT、契约和架构门禁；提交前停止，不 commit/push。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户私聊与多设备消息（个人即时通讯）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。

基于已确认的个人即时通讯客户方案，在 IMBoy 后端实施明确批准的差异项。先核对路由、契约、四层架构、conv_seq、设备 ACK、离线补投和三端消费。先补契约与失败测试，再最小实现。不得改变消息顺序、幂等、投递和兼容语义。运行定向 EUnit/CT、契约和架构门禁；提交前停止，不 commit/push。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-074 · 客户群聊、社群运营与群治理

**场景：** 群组与社群协作

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。

针对已批准的群组/社群差异实施后端增量。先确认现有 Group API、角色 0–5、workspace/personal scope 和子功能能否组合满足需求。仅对真实缺口新增能力；定义契约、权限不变量、迁移兼容和失败测试。完成定向验证，提交前停止。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。

针对已批准的群组/社群差异实施后端增量。先确认现有 Group API、角色 0–5、workspace/personal scope 和子功能能否组合满足需求。仅对真实缺口新增能力；定义契约、权限不变量、迁移兼容和失败测试。完成定向验证，提交前停止。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。

针对已批准的群组/社群差异实施后端增量。先确认现有 Group API、角色 0–5、workspace/personal scope 和子功能能否组合满足需求。仅对真实缺口新增能力；定义契约、权限不变量、迁移兼容和失败测试。完成定向验证，提交前停止。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。

针对已批准的群组/社群差异实施后端增量。先确认现有 Group API、角色 0–5、workspace/personal scope 和子功能能否组合满足需求。仅对真实缺口新增能力；定义契约、权限不变量、迁移兼容和失败测试。完成定向验证，提交前停止。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。

针对已批准的群组/社群差异实施后端增量。先确认现有 Group API、角色 0–5、workspace/personal scope 和子功能能否组合满足需求。仅对真实缺口新增能力；定义契约、权限不变量、迁移兼容和失败测试。完成定向验证，提交前停止。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户群聊、社群运营与群治理（群组与社群协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。

针对已批准的群组/社群差异实施后端增量。先确认现有 Group API、角色 0–5、workspace/personal scope 和子功能能否组合满足需求。仅对真实缺口新增能力；定义契约、权限不变量、迁移兼容和失败测试。完成定向验证，提交前停止。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-075 · 客户内容频道、知识订阅与付费内容

**场景：** 频道与订阅内容

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。

实施已批准的频道客户化差异。复用 Channel 访问元组 visibility/access_type/join_policy、订阅/购买状态、讨论群和内容接口。先补契约与访问控制正反例，再实施最小变更；未授权用户必须 fail-closed。核对 App/Admin 消费和支付兼容，不擅自改变收费语义。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。

实施已批准的频道客户化差异。复用 Channel 访问元组 visibility/access_type/join_policy、订阅/购买状态、讨论群和内容接口。先补契约与访问控制正反例，再实施最小变更；未授权用户必须 fail-closed。核对 App/Admin 消费和支付兼容，不擅自改变收费语义。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。

实施已批准的频道客户化差异。复用 Channel 访问元组 visibility/access_type/join_policy、订阅/购买状态、讨论群和内容接口。先补契约与访问控制正反例，再实施最小变更；未授权用户必须 fail-closed。核对 App/Admin 消费和支付兼容，不擅自改变收费语义。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。

实施已批准的频道客户化差异。复用 Channel 访问元组 visibility/access_type/join_policy、订阅/购买状态、讨论群和内容接口。先补契约与访问控制正反例，再实施最小变更；未授权用户必须 fail-closed。核对 App/Admin 消费和支付兼容，不擅自改变收费语义。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。

实施已批准的频道客户化差异。复用 Channel 访问元组 visibility/access_type/join_policy、订阅/购买状态、讨论群和内容接口。先补契约与访问控制正反例，再实施最小变更；未授权用户必须 fail-closed。核对 App/Admin 消费和支付兼容，不擅自改变收费语义。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。

实施已批准的频道客户化差异。复用 Channel 访问元组 visibility/access_type/join_policy、订阅/购买状态、讨论群和内容接口。先补契约与访问控制正反例，再实施最小变更；未授权用户必须 fail-closed。核对 App/Admin 消费和支付兼容，不擅自改变收费语义。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-076 · 客户动态、朋友圈与内容互动

**场景：** 朋友圈与轻社交

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。

针对已批准的 Moment 客户差异实施后端变更。验证作者权限、可见性 ACL、denylist 优先、删除/禁用内容不可读、管理员访问隔离、分页和附件授权。先更新契约和回归测试，再改实现；不扩大普通用户可见范围。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。

针对已批准的 Moment 客户差异实施后端变更。验证作者权限、可见性 ACL、denylist 优先、删除/禁用内容不可读、管理员访问隔离、分页和附件授权。先更新契约和回归测试，再改实现；不扩大普通用户可见范围。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。

针对已批准的 Moment 客户差异实施后端变更。验证作者权限、可见性 ACL、denylist 优先、删除/禁用内容不可读、管理员访问隔离、分页和附件授权。先更新契约和回归测试，再改实现；不扩大普通用户可见范围。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。

针对已批准的 Moment 客户差异实施后端变更。验证作者权限、可见性 ACL、denylist 优先、删除/禁用内容不可读、管理员访问隔离、分页和附件授权。先更新契约和回归测试，再改实现；不扩大普通用户可见范围。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。

针对已批准的 Moment 客户差异实施后端变更。验证作者权限、可见性 ACL、denylist 优先、删除/禁用内容不可读、管理员访问隔离、分页和附件授权。先更新契约和回归测试，再改实现；不扩大普通用户可见范围。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。

针对已批准的 Moment 客户差异实施后端变更。验证作者权限、可见性 ACL、denylist 优先、删除/禁用内容不可读、管理员访问隔离、分页和附件授权。先更新契约和回归测试，再改实现；不扩大普通用户可见范围。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-077 · 客户企业组织、团队空间和成员管理

**场景：** 组织与工作区

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。

实施已批准的组织/工作区差异。遵守 active 真人 owner 唯一性、owner 不可直接移除、部门防环、workspace 成员状态、request_id 幂等和 scope 约束。先补迁移/契约/失败测试，评估旧数据兼容及回滚；禁止绕过数据库触发器或把 organization 改成租户隔离边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。

实施已批准的组织/工作区差异。遵守 active 真人 owner 唯一性、owner 不可直接移除、部门防环、workspace 成员状态、request_id 幂等和 scope 约束。先补迁移/契约/失败测试，评估旧数据兼容及回滚；禁止绕过数据库触发器或把 organization 改成租户隔离边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。

实施已批准的组织/工作区差异。遵守 active 真人 owner 唯一性、owner 不可直接移除、部门防环、workspace 成员状态、request_id 幂等和 scope 约束。先补迁移/契约/失败测试，评估旧数据兼容及回滚；禁止绕过数据库触发器或把 organization 改成租户隔离边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。

实施已批准的组织/工作区差异。遵守 active 真人 owner 唯一性、owner 不可直接移除、部门防环、workspace 成员状态、request_id 幂等和 scope 约束。先补迁移/契约/失败测试，评估旧数据兼容及回滚；禁止绕过数据库触发器或把 organization 改成租户隔离边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。

实施已批准的组织/工作区差异。遵守 active 真人 owner 唯一性、owner 不可直接移除、部门防环、workspace 成员状态、request_id 幂等和 scope 约束。先补迁移/契约/失败测试，评估旧数据兼容及回滚；禁止绕过数据库触发器或把 organization 改成租户隔离边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户企业组织、团队空间和成员管理（组织与工作区）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。

实施已批准的组织/工作区差异。遵守 active 真人 owner 唯一性、owner 不可直接移除、部门防环、workspace 成员状态、request_id 幂等和 scope 约束。先补迁移/契约/失败测试，评估旧数据兼容及回滚；禁止绕过数据库触发器或把 organization 改成租户隔离边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-078 · 客户项目、任务、里程碑和活动流

**场景：** 项目协作

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。

基于批准的项目协作需求实施缺失能力。保持 Project ⊆ Workspace 成员不变量、同 Workspace 频道关联、里程碑状态与 reached_at 一致、业务写与 project_event 同事务。先定义契约和测试，再实施；不得绕开触发器和事务边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。

基于批准的项目协作需求实施缺失能力。保持 Project ⊆ Workspace 成员不变量、同 Workspace 频道关联、里程碑状态与 reached_at 一致、业务写与 project_event 同事务。先定义契约和测试，再实施；不得绕开触发器和事务边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。

基于批准的项目协作需求实施缺失能力。保持 Project ⊆ Workspace 成员不变量、同 Workspace 频道关联、里程碑状态与 reached_at 一致、业务写与 project_event 同事务。先定义契约和测试，再实施；不得绕开触发器和事务边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。

基于批准的项目协作需求实施缺失能力。保持 Project ⊆ Workspace 成员不变量、同 Workspace 频道关联、里程碑状态与 reached_at 一致、业务写与 project_event 同事务。先定义契约和测试，再实施；不得绕开触发器和事务边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。

基于批准的项目协作需求实施缺失能力。保持 Project ⊆ Workspace 成员不变量、同 Workspace 频道关联、里程碑状态与 reached_at 一致、业务写与 project_event 同事务。先定义契约和测试，再实施；不得绕开触发器和事务边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户项目、任务、里程碑和活动流（项目协作）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。

基于批准的项目协作需求实施缺失能力。保持 Project ⊆ Workspace 成员不变量、同 Workspace 频道关联、里程碑状态与 reached_at 一致、业务写与 project_event 同事务。先定义契约和测试，再实施；不得绕开触发器和事务边界。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-079 · 客户网站在线客服、排队和坐席工作台

**场景：** 客服中心与访客挂件

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。

实施已批准的客服挂件/坐席差异。遵守 x-cs-visit-token 头传递、URL 禁止凭证、jti 防重放、来源校验、workspace_id 必填、会话 CAS expected_version、同企业单活会话和消息不复制原则。先补安全负例与契约测试；不将访客创建为普通用户。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。

实施已批准的客服挂件/坐席差异。遵守 x-cs-visit-token 头传递、URL 禁止凭证、jti 防重放、来源校验、workspace_id 必填、会话 CAS expected_version、同企业单活会话和消息不复制原则。先补安全负例与契约测试；不将访客创建为普通用户。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。

实施已批准的客服挂件/坐席差异。遵守 x-cs-visit-token 头传递、URL 禁止凭证、jti 防重放、来源校验、workspace_id 必填、会话 CAS expected_version、同企业单活会话和消息不复制原则。先补安全负例与契约测试；不将访客创建为普通用户。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。

实施已批准的客服挂件/坐席差异。遵守 x-cs-visit-token 头传递、URL 禁止凭证、jti 防重放、来源校验、workspace_id 必填、会话 CAS expected_version、同企业单活会话和消息不复制原则。先补安全负例与契约测试；不将访客创建为普通用户。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。

实施已批准的客服挂件/坐席差异。遵守 x-cs-visit-token 头传递、URL 禁止凭证、jti 防重放、来源校验、workspace_id 必填、会话 CAS expected_version、同企业单活会话和消息不复制原则。先补安全负例与契约测试；不将访客创建为普通用户。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。

实施已批准的客服挂件/坐席差异。遵守 x-cs-visit-token 头传递、URL 禁止凭证、jti 防重放、来源校验、workspace_id 必填、会话 CAS expected_version、同企业单活会话和消息不复制原则。先补安全负例与契约测试；不将访客创建为普通用户。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-080 · 客户联系人、企业会话、资产和离岗交接

**场景：** 企业客户经营 CRM

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。

实施已批准的企业客户经营差异。遵守渠道标识 HMAC+掩码不存明文、未同意不得持久化消息、企业消息与个人 C2C 分离、retain_until 只能后移、hold 阻断 purge、离岗交接先于成员移除。先补测试和契约，再最小实施。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。

实施已批准的企业客户经营差异。遵守渠道标识 HMAC+掩码不存明文、未同意不得持久化消息、企业消息与个人 C2C 分离、retain_until 只能后移、hold 阻断 purge、离岗交接先于成员移除。先补测试和契约，再最小实施。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。

实施已批准的企业客户经营差异。遵守渠道标识 HMAC+掩码不存明文、未同意不得持久化消息、企业消息与个人 C2C 分离、retain_until 只能后移、hold 阻断 purge、离岗交接先于成员移除。先补测试和契约，再最小实施。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。

实施已批准的企业客户经营差异。遵守渠道标识 HMAC+掩码不存明文、未同意不得持久化消息、企业消息与个人 C2C 分离、retain_until 只能后移、hold 阻断 purge、离岗交接先于成员移除。先补测试和契约，再最小实施。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。

实施已批准的企业客户经营差异。遵守渠道标识 HMAC+掩码不存明文、未同意不得持久化消息、企业消息与个人 C2C 分离、retain_until 只能后移、hold 阻断 purge、离岗交接先于成员移除。先补测试和契约，再最小实施。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。

实施已批准的企业客户经营差异。遵守渠道标识 HMAC+掩码不存明文、未同意不得持久化消息、企业消息与个人 C2C 分离、retain_until 只能后移、hold 阻断 purge、离岗交接先于成员移除。先补测试和契约，再最小实施。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-081 · 客户知识助手、群内助手和受控自动化

**场景：** AI 助手与 Agent

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。

实施已批准的 Agent 能力差异。遵守 Grant DEFAULT=DENY、能力只能收窄、真人 delegator、Run/Effect/Grant event append-only、dispatching 先持久化再调用 adapter、HITL 审批和 MCP 按 tool 授权。不得让 E2EE 群消息触发服务端 LLM；先补安全与状态机测试。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。

实施已批准的 Agent 能力差异。遵守 Grant DEFAULT=DENY、能力只能收窄、真人 delegator、Run/Effect/Grant event append-only、dispatching 先持久化再调用 adapter、HITL 审批和 MCP 按 tool 授权。不得让 E2EE 群消息触发服务端 LLM；先补安全与状态机测试。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。

实施已批准的 Agent 能力差异。遵守 Grant DEFAULT=DENY、能力只能收窄、真人 delegator、Run/Effect/Grant event append-only、dispatching 先持久化再调用 adapter、HITL 审批和 MCP 按 tool 授权。不得让 E2EE 群消息触发服务端 LLM；先补安全与状态机测试。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。

实施已批准的 Agent 能力差异。遵守 Grant DEFAULT=DENY、能力只能收窄、真人 delegator、Run/Effect/Grant event append-only、dispatching 先持久化再调用 adapter、HITL 审批和 MCP 按 tool 授权。不得让 E2EE 群消息触发服务端 LLM；先补安全与状态机测试。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。

实施已批准的 Agent 能力差异。遵守 Grant DEFAULT=DENY、能力只能收窄、真人 delegator、Run/Effect/Grant event append-only、dispatching 先持久化再调用 adapter、HITL 审批和 MCP 按 tool 授权。不得让 E2EE 群消息触发服务端 LLM；先补安全与状态机测试。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。

实施已批准的 Agent 能力差异。遵守 Grant DEFAULT=DENY、能力只能收窄、真人 delegator、Run/Effect/Grant event append-only、dispatching 先持久化再调用 adapter、HITL 审批和 MCP 按 tool 授权。不得让 E2EE 群消息触发服务端 LLM；先补安全与状态机测试。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-082 · 客户第三方系统机器人和事件集成

**场景：** 开发者 Bot 与 Webhook

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。

实施已批准的 Bot/Webhook 差异。保持 api_token 与 verify_token 分离、密钥仅安全展示、Webhook 验签、防重放、超时/重试/死信可观测和 Bot 身份判定。先写签名篡改、重复投递、超时和越权测试；不得在日志输出 token。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。

实施已批准的 Bot/Webhook 差异。保持 api_token 与 verify_token 分离、密钥仅安全展示、Webhook 验签、防重放、超时/重试/死信可观测和 Bot 身份判定。先写签名篡改、重复投递、超时和越权测试；不得在日志输出 token。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。

实施已批准的 Bot/Webhook 差异。保持 api_token 与 verify_token 分离、密钥仅安全展示、Webhook 验签、防重放、超时/重试/死信可观测和 Bot 身份判定。先写签名篡改、重复投递、超时和越权测试；不得在日志输出 token。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。

实施已批准的 Bot/Webhook 差异。保持 api_token 与 verify_token 分离、密钥仅安全展示、Webhook 验签、防重放、超时/重试/死信可观测和 Bot 身份判定。先写签名篡改、重复投递、超时和越权测试；不得在日志输出 token。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。

实施已批准的 Bot/Webhook 差异。保持 api_token 与 verify_token 分离、密钥仅安全展示、Webhook 验签、防重放、超时/重试/死信可观测和 Bot 身份判定。先写签名篡改、重复投递、超时和越权测试；不得在日志输出 token。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。

实施已批准的 Bot/Webhook 差异。保持 api_token 与 verify_token 分离、密钥仅安全展示、Webhook 验签、防重放、超时/重试/死信可观测和 Bot 身份判定。先写签名篡改、重复投递、超时和越权测试；不得在日志输出 token。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-083 · 客户私密聊天、群组加密与密钥恢复

**场景：** 端到端加密通信

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。

实施已批准的 E2EE 差异。遵守服务端零密码学边界、设备 capability 协商、群成员世代隔离、房间密钥不透明中转、备份仅密文、恢复分片门限和兼容性矩阵。先写客户端/服务端契约与安全回归，不将服务器解密作为方案。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。

实施已批准的 E2EE 差异。遵守服务端零密码学边界、设备 capability 协商、群成员世代隔离、房间密钥不透明中转、备份仅密文、恢复分片门限和兼容性矩阵。先写客户端/服务端契约与安全回归，不将服务器解密作为方案。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。

实施已批准的 E2EE 差异。遵守服务端零密码学边界、设备 capability 协商、群成员世代隔离、房间密钥不透明中转、备份仅密文、恢复分片门限和兼容性矩阵。先写客户端/服务端契约与安全回归，不将服务器解密作为方案。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。

实施已批准的 E2EE 差异。遵守服务端零密码学边界、设备 capability 协商、群成员世代隔离、房间密钥不透明中转、备份仅密文、恢复分片门限和兼容性矩阵。先写客户端/服务端契约与安全回归，不将服务器解密作为方案。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。

实施已批准的 E2EE 差异。遵守服务端零密码学边界、设备 capability 协商、群成员世代隔离、房间密钥不透明中转、备份仅密文、恢复分片门限和兼容性矩阵。先写客户端/服务端契约与安全回归，不将服务器解密作为方案。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。

实施已批准的 E2EE 差异。遵守服务端零密码学边界、设备 capability 协商、群成员世代隔离、房间密钥不透明中转、备份仅密文、恢复分片门限和兼容性矩阵。先写客户端/服务端契约与安全回归，不将服务器解密作为方案。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-084 · 客户文件共享、图片视频和安全下载

**场景：** 附件与文件中心

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。

实施已批准的附件差异。复用现有 presign/confirm、attachment registry、内容授权代理和生命周期状态机。验证 MIME/大小、跨用户访问、过期 URL、重复 confirm、部分上传、账号切换和对象孤儿。附件 URL 不得绕过授权直接暴露。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。

实施已批准的附件差异。复用现有 presign/confirm、attachment registry、内容授权代理和生命周期状态机。验证 MIME/大小、跨用户访问、过期 URL、重复 confirm、部分上传、账号切换和对象孤儿。附件 URL 不得绕过授权直接暴露。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。

实施已批准的附件差异。复用现有 presign/confirm、attachment registry、内容授权代理和生命周期状态机。验证 MIME/大小、跨用户访问、过期 URL、重复 confirm、部分上传、账号切换和对象孤儿。附件 URL 不得绕过授权直接暴露。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。

实施已批准的附件差异。复用现有 presign/confirm、attachment registry、内容授权代理和生命周期状态机。验证 MIME/大小、跨用户访问、过期 URL、重复 confirm、部分上传、账号切换和对象孤儿。附件 URL 不得绕过授权直接暴露。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。

实施已批准的附件差异。复用现有 presign/confirm、attachment registry、内容授权代理和生命周期状态机。验证 MIME/大小、跨用户访问、过期 URL、重复 confirm、部分上传、账号切换和对象孤儿。附件 URL 不得绕过授权直接暴露。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户文件共享、图片视频和安全下载（附件与文件中心）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。

实施已批准的附件差异。复用现有 presign/confirm、attachment registry、内容授权代理和生命周期状态机。验证 MIME/大小、跨用户访问、过期 URL、重复 confirm、部分上传、账号切换和对象孤儿。附件 URL 不得绕过授权直接暴露。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-085 · 客户一对一通话、群通话和直播互动

**场景：** 音视频通话与直播

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。

实施已批准的 RTC/直播差异。保留 IM 信令管理呼叫状态、媒体传输职责分离。核对房间路由、TURN、权限、设备切换、网络中断和多端生命周期。不得假设 Redis 多节点能力或群通话已经开放；真实双端验收需独立安排。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。

实施已批准的 RTC/直播差异。保留 IM 信令管理呼叫状态、媒体传输职责分离。核对房间路由、TURN、权限、设备切换、网络中断和多端生命周期。不得假设 Redis 多节点能力或群通话已经开放；真实双端验收需独立安排。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。

实施已批准的 RTC/直播差异。保留 IM 信令管理呼叫状态、媒体传输职责分离。核对房间路由、TURN、权限、设备切换、网络中断和多端生命周期。不得假设 Redis 多节点能力或群通话已经开放；真实双端验收需独立安排。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。

实施已批准的 RTC/直播差异。保留 IM 信令管理呼叫状态、媒体传输职责分离。核对房间路由、TURN、权限、设备切换、网络中断和多端生命周期。不得假设 Redis 多节点能力或群通话已经开放；真实双端验收需独立安排。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。

实施已批准的 RTC/直播差异。保留 IM 信令管理呼叫状态、媒体传输职责分离。核对房间路由、TURN、权限、设备切换、网络中断和多端生命周期。不得假设 Redis 多节点能力或群通话已经开放；真实双端验收需独立安排。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户一对一通话、群通话和直播互动（音视频通话与直播）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。

实施已批准的 RTC/直播差异。保留 IM 信令管理呼叫状态、媒体传输职责分离。核对房间路由、TURN、权限、设备切换、网络中断和多端生命周期。不得假设 Redis 多节点能力或群通话已经开放；真实双端验收需独立安排。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-086 · 客户实名/注册、联系人和关系管理

**场景：** 账号、好友与社交关系

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。

实施已批准的身份/好友差异。遵守 account_type 权威枚举、TSID 精度、设备身份、Token 刷新、QR 登录一次性领取、好友关系双向语义、denylist 优先和 Admin 独立认证。先补认证与越权负例测试，不降低验证码、限流或设备安全要求。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。

实施已批准的身份/好友差异。遵守 account_type 权威枚举、TSID 精度、设备身份、Token 刷新、QR 登录一次性领取、好友关系双向语义、denylist 优先和 Admin 独立认证。先补认证与越权负例测试，不降低验证码、限流或设备安全要求。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。

实施已批准的身份/好友差异。遵守 account_type 权威枚举、TSID 精度、设备身份、Token 刷新、QR 登录一次性领取、好友关系双向语义、denylist 优先和 Admin 独立认证。先补认证与越权负例测试，不降低验证码、限流或设备安全要求。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。

实施已批准的身份/好友差异。遵守 account_type 权威枚举、TSID 精度、设备身份、Token 刷新、QR 登录一次性领取、好友关系双向语义、denylist 优先和 Admin 独立认证。先补认证与越权负例测试，不降低验证码、限流或设备安全要求。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。

实施已批准的身份/好友差异。遵守 account_type 权威枚举、TSID 精度、设备身份、Token 刷新、QR 登录一次性领取、好友关系双向语义、denylist 优先和 Admin 独立认证。先补认证与越权负例测试，不降低验证码、限流或设备安全要求。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户实名/注册、联系人和关系管理（账号、好友与社交关系）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。

实施已批准的身份/好友差异。遵守 account_type 权威枚举、TSID 精度、设备身份、Token 刷新、QR 登录一次性领取、好友关系双向语义、denylist 优先和 Admin 独立认证。先补认证与越权负例测试，不降低验证码、限流或设备安全要求。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-087 · 客户充值、支付、账单和付费权益

**场景：** 钱包、支付与订阅

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。

实施已批准的支付差异。先定义订单、回调、幂等键、状态迁移、补偿和对账契约，验证签名、金额币种、重复/乱序回调、超时和并发扣款。不得在测试中使用真实支付账号或资金；遵守 Payment Mandate 约束。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。

实施已批准的支付差异。先定义订单、回调、幂等键、状态迁移、补偿和对账契约，验证签名、金额币种、重复/乱序回调、超时和并发扣款。不得在测试中使用真实支付账号或资金；遵守 Payment Mandate 约束。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。

实施已批准的支付差异。先定义订单、回调、幂等键、状态迁移、补偿和对账契约，验证签名、金额币种、重复/乱序回调、超时和并发扣款。不得在测试中使用真实支付账号或资金；遵守 Payment Mandate 约束。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。

实施已批准的支付差异。先定义订单、回调、幂等键、状态迁移、补偿和对账契约，验证签名、金额币种、重复/乱序回调、超时和并发扣款。不得在测试中使用真实支付账号或资金；遵守 Payment Mandate 约束。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。

实施已批准的支付差异。先定义订单、回调、幂等键、状态迁移、补偿和对账契约，验证签名、金额币种、重复/乱序回调、超时和并发扣款。不得在测试中使用真实支付账号或资金；遵守 Payment Mandate 约束。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。

实施已批准的支付差异。先定义订单、回调、幂等键、状态迁移、补偿和对账契约，验证签名、金额币种、重复/乱序回调、超时和并发扣款。不得在测试中使用真实支付账号或资金；遵守 Payment Mandate 约束。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-088 · 客户用户、群组、频道和内容发现

**场景：** 搜索与发现

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。

实施已批准的搜索差异。遵守 FTS 不索引 E2EE 密文、好友/群/频道可见性授权、denylist 规则和分页合同。先定义查询/过滤契约与安全测试，再优化索引；不得通过扩大索引内容泄露私密消息。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。

实施已批准的搜索差异。遵守 FTS 不索引 E2EE 密文、好友/群/频道可见性授权、denylist 规则和分页合同。先定义查询/过滤契约与安全测试，再优化索引；不得通过扩大索引内容泄露私密消息。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。

实施已批准的搜索差异。遵守 FTS 不索引 E2EE 密文、好友/群/频道可见性授权、denylist 规则和分页合同。先定义查询/过滤契约与安全测试，再优化索引；不得通过扩大索引内容泄露私密消息。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。

实施已批准的搜索差异。遵守 FTS 不索引 E2EE 密文、好友/群/频道可见性授权、denylist 规则和分页合同。先定义查询/过滤契约与安全测试，再优化索引；不得通过扩大索引内容泄露私密消息。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。

实施已批准的搜索差异。遵守 FTS 不索引 E2EE 密文、好友/群/频道可见性授权、denylist 规则和分页合同。先定义查询/过滤契约与安全测试，再优化索引；不得通过扩大索引内容泄露私密消息。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户用户、群组、频道和内容发现（搜索与发现）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。

实施已批准的搜索差异。遵守 FTS 不索引 E2EE 密文、好友/群/频道可见性授权、denylist 规则和分页合同。先定义查询/过滤契约与安全测试，再优化索引；不得通过扩大索引内容泄露私密消息。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-089 · 客户白标入口、功能开关和版本升级

**场景：** 应用配置与版本运营

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。

实施已批准的白标/版本运营差异。以 app manifest/policy 和仓库配置真源为准，验证 ETag、默认值、未知 feature fail-closed、客户端兼容、版本策略和升级上报。不得把 UI 隐藏当安全边界，也不得绕过 phase profile 交付锁。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。

实施已批准的白标/版本运营差异。以 app manifest/policy 和仓库配置真源为准，验证 ETag、默认值、未知 feature fail-closed、客户端兼容、版本策略和升级上报。不得把 UI 隐藏当安全边界，也不得绕过 phase profile 交付锁。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。

实施已批准的白标/版本运营差异。以 app manifest/policy 和仓库配置真源为准，验证 ETag、默认值、未知 feature fail-closed、客户端兼容、版本策略和升级上报。不得把 UI 隐藏当安全边界，也不得绕过 phase profile 交付锁。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。

实施已批准的白标/版本运营差异。以 app manifest/policy 和仓库配置真源为准，验证 ETag、默认值、未知 feature fail-closed、客户端兼容、版本策略和升级上报。不得把 UI 隐藏当安全边界，也不得绕过 phase profile 交付锁。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。

实施已批准的白标/版本运营差异。以 app manifest/policy 和仓库配置真源为准，验证 ETag、默认值、未知 feature fail-closed、客户端兼容、版本策略和升级上报。不得把 UI 隐藏当安全边界，也不得绕过 phase profile 交付锁。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

当前功能场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。目标客户/目标业务流程：[请填写客户行业、用户角色、业务目标、关键流程、特殊规则和优先级]。目标范围：[请填写本次要覆盖的端/模块/子功能]。

先读取当前仓库规则和权威文档，再核对实际代码与测试。不得把设计、文档或 API 存在等同于功能已交付；明确标注 CURRENT、TARGET、LOCKED、UNKNOWN 和 NOT_VERIFIED。

任务类型：新增功能。

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。

实施已批准的白标/版本运营差异。以 app manifest/policy 和仓库配置真源为准，验证 ETag、默认值、未知 feature fail-closed、客户端兼容、版本策略和升级上报。不得把 UI 隐藏当安全边界，也不得绕过 phase profile 交付锁。

请先判断需求能否由现有能力配置/组合完成；只有确认存在真实能力缺口才新增代码。输出 capability contract：参与角色、用户旅程、状态机、权限/数据边界、API/WS/客户端界面、迁移兼容、观测和验收标准。客户输入不完整或涉及产品决策时，列出阻塞问题并停在方案阶段。输入充分且任务明确时，按 TDD 先写失败测试再实现，遵循项目已有架构和测试命令。遵守 Gate：计划/契约先审阅，实施后完整验证；不 commit、不 push、不合并、不部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-110 · 新一代 AI 模型复审与改进旧 AI 生成代码

**场景：** 当更强的新 AI 模型发布时，选择一个由旧 AI 模型实现的 IMBoy 功能切片，让新模型独立重建事实、识别盲点、补测试并证明改进价值。

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

你正在接手 IMBoy 现有代码库。目标不是重写整个项目，而是为以下客户/产品需求增加一个明确的新能力：[填写需求、目标用户、成功标准]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构地图、相关 API/WS 契约、数据库迁移和测试合同。识别现有能力与真正缺口，禁止把已有功能重复实现。输出 capability contract：角色、流程、状态机、权限、数据归属、接口、兼容性、失败恢复、观测和验收标准。若需求不完整，列出阻塞问题并停在方案阶段。

需求明确后按 orch-add-feature 流程执行：先提出最小任务计划并等待计划门禁批准；先写失败测试，再实现；遵循当前仓库架构与测试入口。不得引入不必要依赖，不修改无关模块，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

你是新一代 AI 模型，负责独立复审 IMBoy 中可能由旧 AI 模型编写或修改的代码。目标功能/目录：[填写功能、模块、文件或用户旅程]。本次目标是让已有功能更正确、更稳定、更安全、更高效、更容易维护，而不是为了展示新模型能力而重写代码。

按 slice-first 工作方式，将范围控制在约 15 个文件或 2000 行以内；先核对当前 HEAD、未提交改动和项目规则，保护其他会话的工作。把现有代码、注释、旧模型的自评和已有测试都视为待验证证据。独立重建真实行为，追踪调用链、数据流、权限边界、错误路径、并发/竞态、资源生命周期和跨端契约；建立改动前测试/性能基线。

重点查找 AI 常见盲点：只改响应未改查询/持久化、只修主路径漏掉 sandbox/production 或其他分支、字段类型/可空性漂移、异常吞掉、重试不幂等、权限只在 UI 检查、测试与实现共享同一假设、mock 通过但真实路径失败、过度抽象和无证据的性能优化。

每个发现必须提供文件/行号、可复现证据、影响等级、行为是否变化和最小修复方案。优先行为保持型优化；先补能证明问题的测试，再做最小改动。涉及公共 API、WS、数据库结构、E2EE、权限语义或产品行为变化时先停止并请求确认。遵循本仓测试/静态检查命令，不套用 ECC 默认 npm 命令。

不要自动创建 docs/retro、重复计划或审计报告；先在会话中输出基线、差距登记和改进方案。完成批准范围内的修改后运行定向测试、完整相关门禁和 diff review，报告改动前后证据、未验证项和剩余风险。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 / 独立验证旧 AI 实现 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

对 IMBoy 的以下现有功能进行独立测试审计：[填写功能、模块、用户旅程或文件范围]。本任务默认不修改业务实现。

先盘点现有单元、集成、契约、E2E、真机/平台测试和 CI 门禁，建立“需求/不变量 → 测试资产/TEST-ID → 命令 → 证据”矩阵。不要以测试文件存在、测试数量或 CI 绿灯替代行为验证。

重点设计正常路径、权限拒绝、非法输入、边界值、空值、重复请求、并发、超时、断线恢复、旧数据升级/降级、跨账号隔离、跨端一致性和真实依赖失败场景。针对 AI 生成代码，特别检查：mock/sandbox 与真实路径不一致、数据库 SELECT/写入字段遗漏、序列化字段遗漏、仅覆盖 happy path、异常被吞、重试重复副作用、测试 fixture 过度迎合实现等问题。

优先新增能区分正确与错误实现的测试；不要复制重复用例、降低阈值、扩大 skip 或修改断言使测试变绿。使用项目已有测试命令和执行合同。逐项报告 PASS/FAIL/BLOCKED/NOT RUN，提供真实命令、退出码、测试数量和证据位置。发现缺陷时给出最小复现和修复建议；未经授权不修改业务代码。不得将未执行或环境缺失标记为 PASS。
```




#### ECC-111 · 设备、推送与通知中心

**场景：** 多设备在线、推送送达和通知偏好

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：设备、推送与通知中心（多设备在线、推送送达和通知偏好）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 user_device、设备注册/吊销、APNs/FCM/JPush 适配、在线状态、S2C 通知、消息推送、免打扰和设备级 ACK。核对平台差异、凭据边界、失效 token 清理与消息/推送职责分离。输出当前已实现和各 provider 未验证状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 user_device、设备注册/吊销、APNs/FCM/JPush 适配、在线状态、S2C 通知、消息推送、免打扰和设备级 ACK。核对平台差异、凭据边界、失效 token 清理与消息/推送职责分离。输出当前已实现和各 provider 未验证状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：设备、推送与通知中心（多设备在线、推送送达和通知偏好）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 user_device、设备注册/吊销、APNs/FCM/JPush 适配、在线状态、S2C 通知、消息推送、免打扰和设备级 ACK。核对平台差异、凭据边界、失效 token 清理与消息/推送职责分离。输出当前已实现和各 provider 未验证状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 user_device、设备注册/吊销、APNs/FCM/JPush 适配、在线状态、S2C 通知、消息推送、免打扰和设备级 ACK。核对平台差异、凭据边界、失效 token 清理与消息/推送职责分离。输出当前已实现和各 provider 未验证状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：设备、推送与通知中心（多设备在线、推送送达和通知偏好）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 user_device、设备注册/吊销、APNs/FCM/JPush 适配、在线状态、S2C 通知、消息推送、免打扰和设备级 ACK。核对平台差异、凭据边界、失效 token 清理与消息/推送职责分离。输出当前已实现和各 provider 未验证状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 user_device、设备注册/吊销、APNs/FCM/JPush 适配、在线状态、S2C 通知、消息推送、免打扰和设备级 ACK。核对平台差异、凭据边界、失效 token 清理与消息/推送职责分离。输出当前已实现和各 provider 未验证状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-112 · 用户关系扩展与位置服务

**场景：** 收藏、标签、黑名单、好友分类和附近的人

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：用户关系扩展与位置服务（收藏、标签、黑名单、好友分类和附近的人）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点用户资料、收藏、用户标签/关系、好友分组、denylist、位置上报/附近的人及隐私设置。核对位置精度、授权、数据保留、黑名单优先、好友关系状态和可见性。输出 API、客户端入口和后台治理边界。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点用户资料、收藏、用户标签/关系、好友分组、denylist、位置上报/附近的人及隐私设置。核对位置精度、授权、数据保留、黑名单优先、好友关系状态和可见性。输出 API、客户端入口和后台治理边界。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：用户关系扩展与位置服务（收藏、标签、黑名单、好友分类和附近的人）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点用户资料、收藏、用户标签/关系、好友分组、denylist、位置上报/附近的人及隐私设置。核对位置精度、授权、数据保留、黑名单优先、好友关系状态和可见性。输出 API、客户端入口和后台治理边界。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点用户资料、收藏、用户标签/关系、好友分组、denylist、位置上报/附近的人及隐私设置。核对位置精度、授权、数据保留、黑名单优先、好友关系状态和可见性。输出 API、客户端入口和后台治理边界。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：用户关系扩展与位置服务（收藏、标签、黑名单、好友分类和附近的人）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点用户资料、收藏、用户标签/关系、好友分组、denylist、位置上报/附近的人及隐私设置。核对位置精度、授权、数据保留、黑名单优先、好友关系状态和可见性。输出 API、客户端入口和后台治理边界。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点用户资料、收藏、用户标签/关系、好友分组、denylist、位置上报/附近的人及隐私设置。核对位置精度、授权、数据保留、黑名单优先、好友关系状态和可见性。输出 API、客户端入口和后台治理边界。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-113 · 举报、内容审核与申诉

**场景：** 社区内容审核、用户举报和申诉工作流

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：举报、内容审核与申诉（社区内容审核、用户举报和申诉工作流）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 report、moderation、appeal、敏感词、审核队列、管理员动作、申诉状态、证据保留和操作审计。区分用户端举报入口、平台后台审核面和业务资源真实状态；检查重复举报、幂等、误伤恢复和审计记录。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 report、moderation、appeal、敏感词、审核队列、管理员动作、申诉状态、证据保留和操作审计。区分用户端举报入口、平台后台审核面和业务资源真实状态；检查重复举报、幂等、误伤恢复和审计记录。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：举报、内容审核与申诉（社区内容审核、用户举报和申诉工作流）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 report、moderation、appeal、敏感词、审核队列、管理员动作、申诉状态、证据保留和操作审计。区分用户端举报入口、平台后台审核面和业务资源真实状态；检查重复举报、幂等、误伤恢复和审计记录。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 report、moderation、appeal、敏感词、审核队列、管理员动作、申诉状态、证据保留和操作审计。区分用户端举报入口、平台后台审核面和业务资源真实状态；检查重复举报、幂等、误伤恢复和审计记录。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：举报、内容审核与申诉（社区内容审核、用户举报和申诉工作流）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 report、moderation、appeal、敏感词、审核队列、管理员动作、申诉状态、证据保留和操作审计。区分用户端举报入口、平台后台审核面和业务资源真实状态；检查重复举报、幂等、误伤恢复和审计记录。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 report、moderation、appeal、敏感词、审核队列、管理员动作、申诉状态、证据保留和操作审计。区分用户端举报入口、平台后台审核面和业务资源真实状态；检查重复举报、幂等、误伤恢复和审计记录。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-114 · 直播间、通话记录与录制治理

**场景：** 直播、RTC 房间、通话记录和录制回放

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：直播间、通话记录与录制治理（直播、RTC 房间、通话记录和录制回放）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 live_room、rtc room join、LiveKit、talk_record、参与者、客户端音频、provider、播放/分段、恢复与清理治理。区分媒体服务、IM 信令、录制 provider 和管理端合规治理；标明群通话、录制和多节点能力的交付锁/实测状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 live_room、rtc room join、LiveKit、talk_record、参与者、客户端音频、provider、播放/分段、恢复与清理治理。区分媒体服务、IM 信令、录制 provider 和管理端合规治理；标明群通话、录制和多节点能力的交付锁/实测状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：直播间、通话记录与录制治理（直播、RTC 房间、通话记录和录制回放）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 live_room、rtc room join、LiveKit、talk_record、参与者、客户端音频、provider、播放/分段、恢复与清理治理。区分媒体服务、IM 信令、录制 provider 和管理端合规治理；标明群通话、录制和多节点能力的交付锁/实测状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 live_room、rtc room join、LiveKit、talk_record、参与者、客户端音频、provider、播放/分段、恢复与清理治理。区分媒体服务、IM 信令、录制 provider 和管理端合规治理；标明群通话、录制和多节点能力的交付锁/实测状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：直播间、通话记录与录制治理（直播、RTC 房间、通话记录和录制回放）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 live_room、rtc room join、LiveKit、talk_record、参与者、客户端音频、provider、播放/分段、恢复与清理治理。区分媒体服务、IM 信令、录制 provider 和管理端合规治理；标明群通话、录制和多节点能力的交付锁/实测状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 live_room、rtc room join、LiveKit、talk_record、参与者、客户端音频、provider、播放/分段、恢复与清理治理。区分媒体服务、IM 信令、录制 provider 和管理端合规治理；标明群通话、录制和多节点能力的交付锁/实测状态。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-115 · 插件、企业应用与单点登录

**场景：** 客户插件扩展、OA 集成和 OIDC/SSO

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：插件、企业应用与单点登录（客户插件扩展、OA 集成和 OIDC/SSO）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 Plugin 管理、插件契约/生命周期、企业应用登记、Credential、OA SSO code/state/exchange、OIDC 登录、Webhook 回调和授权范围。明确平台内插件、外部 OA 应用和 MCP 工具三者边界；检查密钥签发、回调域名、重放防护与撤销机制。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 Plugin 管理、插件契约/生命周期、企业应用登记、Credential、OA SSO code/state/exchange、OIDC 登录、Webhook 回调和授权范围。明确平台内插件、外部 OA 应用和 MCP 工具三者边界；检查密钥签发、回调域名、重放防护与撤销机制。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：插件、企业应用与单点登录（客户插件扩展、OA 集成和 OIDC/SSO）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 Plugin 管理、插件契约/生命周期、企业应用登记、Credential、OA SSO code/state/exchange、OIDC 登录、Webhook 回调和授权范围。明确平台内插件、外部 OA 应用和 MCP 工具三者边界；检查密钥签发、回调域名、重放防护与撤销机制。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 Plugin 管理、插件契约/生命周期、企业应用登记、Credential、OA SSO code/state/exchange、OIDC 登录、Webhook 回调和授权范围。明确平台内插件、外部 OA 应用和 MCP 工具三者边界；检查密钥签发、回调域名、重放防护与撤销机制。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：插件、企业应用与单点登录（客户插件扩展、OA 集成和 OIDC/SSO）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 Plugin 管理、插件契约/生命周期、企业应用登记、Credential、OA SSO code/state/exchange、OIDC 登录、Webhook 回调和授权范围。明确平台内插件、外部 OA 应用和 MCP 工具三者边界；检查密钥签发、回调域名、重放防护与撤销机制。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点 Plugin 管理、插件契约/生命周期、企业应用登记、Credential、OA SSO code/state/exchange、OIDC 登录、Webhook 回调和授权范围。明确平台内插件、外部 OA 应用和 MCP 工具三者边界；检查密钥签发、回调域名、重放防护与撤销机制。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


#### ECC-116 · 运营通知、反馈与可观测性

**场景：** 公告、用户反馈、平台统计和运营审计

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

当前功能场景：运营通知、反馈与可观测性（公告、用户反馈、平台统计和运营审计）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：新增功能。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点公告发布/版本、反馈提交与处理、平台操作日志、统计指标、健康检查、metrics 和审计事件。识别运营面权限、数据脱敏、指标口径、保留期和高基数标签风险。输出管理员工作流及观测缺口。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点公告发布/版本、反馈提交与处理、平台操作日志、统计指标、健康检查、metrics 和审计事件。识别运营面权限、数据脱敏、指标口径、保留期和高基数标签风险。输出管理员工作流及观测缺口。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先判断现有能力能否通过配置或组合满足需求，只有确认存在真实能力缺口才新增代码。输出 capability contract：角色、用户旅程、状态机、权限与数据归属、接口、迁移兼容、失败恢复、观测和验收标准。需求不完整时列出阻塞问题并停在方案阶段；需求明确后按 TDD 先写失败测试再实现，遵守仓库架构和测试合同。先过计划门禁，不 commit/push/合并/部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

当前功能场景：运营通知、反馈与可观测性（公告、用户反馈、平台统计和运营审计）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：优化现有功能，特别用于新一代 AI 模型独立复审旧 AI 模型生成的实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点公告发布/版本、反馈提交与处理、平台操作日志、统计指标、健康检查、metrics 和审计事件。识别运营面权限、数据脱敏、指标口径、保留期和高基数标签风险。输出管理员工作流及观测缺口。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点公告发布/版本、反馈提交与处理、平台操作日志、统计指标、健康检查、metrics 和审计事件。识别运营面权限、数据脱敏、指标口径、保留期和高基数标签风险。输出管理员工作流及观测缺口。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

按 brownfield-retrospective 的 slice-first 方法，将范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、代码注释或现有测试完整性；独立重建真实行为，追踪调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约。先建立改动前测试/性能/错误行为基线。

检查正确性、边界条件、异常处理、幂等/重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性。重点排查 AI 常见盲点：只改响应没改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享同一假设。

每个发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。以 Handler → Logic → DS → Repo、Erlang/OTP、Cowboy、PostgreSQL 和现有 API/WS 契约为准。

先在会话中输出基线、差距登记和改进方案；不要自动创建 docs/retro 或重复审计文档。完成批准范围内的修改后运行定向测试、相关静态检查和 diff review，报告改动前后证据及未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

当前功能场景：运营通知、反馈与可观测性（公告、用户反馈、平台统计和运营审计）。目标客户/业务流程：[填写客户行业、角色、业务目标、关键流程和特殊规则]。目标范围：[填写端、模块和子功能]。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、权威概念文档、接口契约、实际代码和测试。把实现状态明确标注为 CURRENT、TARGET、LOCKED、UNKNOWN 或 NOT_VERIFIED；不得把文档/API 存在等同于端到端交付。

任务类型：测试现有功能，独立验证旧 AI 实现。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点公告发布/版本、反馈提交与处理、平台操作日志、统计指标、健康检查、metrics 和审计事件。识别运营面权限、数据脱敏、指标口径、保留期和高基数标签风险。输出管理员工作流及观测缺口。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

目标客户/用户角色/业务目标：[填写]。客户业务规则和验收指标：[填写]。本次目标端/模块：[填写]。

先读取仓库规则、当前 HEAD、工作区状态、对应概念文档/接口契约/代码/测试。区分 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把设计或 API 存在说成端到端交付。

任务类型：新增功能。

盘点公告发布/版本、反馈提交与处理、平台操作日志、统计指标、健康检查、metrics 和审计事件。识别运营面权限、数据脱敏、指标口径、保留期和高基数标签风险。输出管理员工作流及观测缺口。

判断现有功能能否通过配置或组合满足需求，只有真实缺口才新增。输出能力合同（角色、流程、状态机、权限、数据归属、接口、失败恢复、观测、兼容和验收）。输入不完整时列出阻塞问题并停在方案阶段。输入明确时按 TDD 先写失败测试再实现，使用现有架构和测试命令。先过计划门禁；不 commit/push/合并/部署。

先不要修改业务实现。盘点单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。使用现有 EUnit、Common Test、契约门禁和 scripts 中的权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、数量和证据位置。发现缺陷先提供复现与最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```


## 维护说明

- 本文只维护本仓库职责范围内的提示词；跨仓案例的统一索引位于 [imboy 后端主仓 docs/prompts/README.md](https://github.com/imboy-pub/imboy/blob/main/docs/prompts/README.md)。
- 修改案例时保持 ECC 编号不变；新增案例先在后端主仓索引登记编号和归属，避免重复。
- Skill 名称必须与 ECC `skills/<name>/SKILL.md` 中的目录名一致；ECC 升级后应重新核对可用性。
- 提示词是任务入口，不是执行结果或质量证明；验收以真实代码、测试和 CI 证据为准。
