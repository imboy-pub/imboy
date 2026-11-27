# IMBoy ECC 使用案例 / Backend

> 本文是可直接复制到 Codex 会话的 ECC 使用案例库。每个案例均为独立提示词，以 `$skill-name` 开头。
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
> **案例编号全局唯一**：ECC-001 至 ECC-072。四个仓库按职责分配，后端主仓提供统一索引。

## 本仓库范围

本文件包含后端、三仓共性、测试治理、运维、性能和 Agent 工程案例，共 44 个（ECC-001、005–022、043–045、051–072）。

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

> 本组面向客户业务场景复刻。每个场景提供两条独立提示词：A 用于需求/能力映射；B 用于在批准后实施客户差异。将提示词中的“客户”替换为具体客户、行业、角色和目标流程。

### ECC-073 · 客户私聊与多设备消息

**场景：** 个人即时通讯

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

面向客户“个人即时通讯”场景，评估 IMBoy 的账号、好友、C2C 会话、消息可靠投递、离线同步、已读/输入状态、消息撤回编辑、转发引用和多设备能力。先读 messaging-model、message_api_contract、WS 协议及真实代码。按客户角色、关键旅程、现有能力 CURRENT、受交付锁能力、缺口、非目标和验收指标输出复刻方案。不得把未开放/未验收能力写成现成功能。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-075 · 客户群聊、社群运营与群治理

**场景：** 群组与社群协作

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻群聊/社群场景，分析 IMBoy 群组创建、成员邀请与角色、群公告、标签/分类、@提及、相册、文件、日程、任务、投票、群发现、群管理日志和群消息。读取 group_api_contract、协作层级概念及对应 API/Flutter/Admin 代码。输出客户场景映射、角色权限矩阵、生命周期、CURRENT/TARGET/交付锁边界和缺口。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-077 · 客户内容频道、知识订阅与付费内容

**场景：** 频道与订阅内容

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻内容频道/知识订阅场景。盘点 IMBoy Channel 的 personal/workspace scope、发现、公开/私有、免费/付费、订阅/购买、帖子、评论、讨论群绑定、精华/置顶、标签、Webhook 和内容访问控制。以 channel-api-contract-v1、微社群契约及真实代码为准，输出能力映射、付费与权限边界、缺口和验收旅程。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-079 · 客户动态、朋友圈与内容互动

**场景：** 朋友圈与轻社交

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻动态/朋友圈场景。分析 Moment 发布、媒体附件、可见性（公开/好友/私密/包含名单/排除名单）、互动、评论、收藏、举报和后台审核。以 moment-api-contract-v1、好友/黑名单规则和现有客户端/管理端实现为依据，输出用户旅程、隐私矩阵、审核流程、现有能力与差异。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-081 · 客户企业组织、团队空间和成员管理

**场景：** 组织与工作区

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻企业组织/团队空间场景。分析 Organization、部门树、owner/admin/member、邀请、成员暂停/恢复/离岗、owner 转移、Workspace、团队码加入、成员角色、品牌、归档恢复和默认工作区。以 collaboration-hierarchy、组织冻结契约和现有 API 为准。特别区分 organization 是业务对象而非租户，授权边界是部署实例。输出权限矩阵、生命周期和缺口。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-083 · 客户项目、任务、里程碑和活动流

**场景：** 项目协作

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻轻量项目协作场景。分析 IMBoy Project 的 workspace 归属、成员约束、任务 todo/doing/review/done、里程碑、频道关联和项目活动事件流。明确项目不是通用 Jira 替代品；列出可复用能力、客户差异、角色操作矩阵和不做范围。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-085 · 客户网站在线客服、排队和坐席工作台

**场景：** 客服中心与访客挂件

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻网站在线客服场景。盘点 Visitor token、Shop Key、Widget 安装、来源白名单、访客会话 queued/active/closed、队列、认领、转接、关闭、评分、坐席并发、App 坐席工作台、Admin 运营面和独立 dist-widget。读取 customer-service 概念和 Widget 契约。明确访客不是 user、消息真源在 enterprise_message、交付/真实验收状态。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-087 · 客户联系人、企业会话、资产和离岗交接

**场景：** 企业客户经营 CRM

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻轻量 CRM/客户经营场景。分析 Business Identity、Enterprise Contact、经办人分配、客户会话、消息、附件资产、同意书、留持策略/hold、审计和 Offboarding Case。区分企业联系人与个人好友，企业消息 owner 是组织。输出客户流程、数据归属、隐私/合规边界、现有功能和差异。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-089 · 客户知识助手、群内助手和受控自动化

**场景：** AI 助手与 Agent

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻 AI 助手场景。区分 account_type=1 Agent、account_type=3 Developer Bot、account_type=2 Channel System Bot。盘点助手发现、角色模板、群内 @触发、agent_task 审批、Grant→Run→Effect、HITL、MCP tool 授权与审计。标明 phase1_delivery 交付锁和 App/Admin 消费缺口，不能把 mock Hirð runtime 说成生产 runtime。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-091 · 客户第三方系统机器人和事件集成

**场景：** 开发者 Bot 与 Webhook

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻第三方 Bot/Webhook 集成场景。分析 Bot 注册、account_type=3、username/@调用、API Token 与 verify token 分离、事件订阅、Webhook outbox 重试、投递状态、命令和 API 权限。明确 Bot 与 Agent、频道 system bot 的区别，输出外部系统集成合同和安全威胁。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-093 · 客户私密聊天、群组加密与密钥恢复

**场景：** 端到端加密通信

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻隐私通信场景。分析 C2C Olm、C2G Megolm、设备信任、安全码/二维码核验、密钥轮换、4S 密文备份、社交恢复、加密附件和 E2EE 档位。以 e2ee 概念、协议规范和交付 profile 为准，明确服务端永不持有的密钥/明文及一期交付锁。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-095 · 客户文件共享、图片视频和安全下载

**场景：** 附件与文件中心

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻文件共享/媒体中心场景。盘点附件上传 presign/confirm、Garage/S3、分块上传、E2EE 附件、附件引用、群文件/相册、企业资产、授权下载、短期 URL 和生命周期清理。区分个人附件、群文件和企业资产的数据归属。输出容量、权限、留存和恢复要求。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-097 · 客户一对一通话、群通话和直播互动

**场景：** 音视频通话与直播

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻音视频沟通场景。分别评估一对一 RTC、群通话、LiveKit 房间、TURN、直播房间和录制能力。核对现有呼叫信令状态机、flutter_webrtc 采集/渲染、LiveKit 媒体路径及产品交付锁。区分已实现、仅计划、仅单节点和未做真实双端验收的能力。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-099 · 客户实名/注册、联系人和关系管理

**场景：** 账号、好友与社交关系

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻账号和社交关系场景。分析注册、邮箱/手机/账号登录、验证码、找回密码、二维码登录、OIDC、设备管理、个人资料、好友申请、备注/分组/标签、黑名单和在线状态。区分 Human/Agent/Bot/System Bot 和 Platform Admin。输出身份生命周期、风控与数据迁移需求。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-101 · 客户充值、支付、账单和付费权益

**场景：** 钱包、支付与订阅

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻钱包/支付/订阅场景。盘点钱包余额、充值订单、支付宝/微信回调、交易流水、账单、频道购买/订阅、billing subscription/invoice 和 Agent payment mandate。明确支付状态机、幂等、退款/补偿、对账与合规边界；区分已实现 API 与真实支付渠道验收状态。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-103 · 客户用户、群组、频道和内容发现

**场景：** 搜索与发现

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻统一搜索/发现体验。盘点用户搜索、好友/群组/频道/Agent 发现、全文检索、消息历史、标签筛选和内容可见性。核对 FTS 对 E2EE 密文的排除规则、分页方式和权限过滤。输出搜索对象、排序、权限、索引和隐私边界。
```

**B ·  · 客户差异实施**

```text


undefined
```

### ECC-105 · 客户白标入口、功能开关和版本升级

**场景：** 应用配置与版本运营

**A · `product-lens` + `product-capability` · 客户场景映射**

```text
$product-lens $product-capability

为客户复刻多品牌/白标客户端运营场景。分析 init、app/features、app/manifest、policy、app_entries/admin_entries、插件、品牌、版本检查、强制/推荐升级和升级事件上报。核对 build flavor、产品 profile、交付锁和部署配置。输出客户品牌差异、远程配置边界和升级回滚方案。
```

**B ·  · 客户差异实施**

```text


undefined
```

## 维护说明

- 本文只维护本仓库职责范围内的提示词；跨仓案例的统一索引位于 [imboy 后端主仓 docs/prompts/README.md](https://github.com/imboy-pub/imboy/blob/main/docs/prompts/README.md)。
- 修改案例时保持 ECC 编号不变；新增案例先在后端主仓索引登记编号和归属，避免重复。
- Skill 名称必须与 ECC `skills/<name>/SKILL.md` 中的目录名一致；ECC 升级后应重新核对可用性。
- 提示词是任务入口，不是执行结果或质量证明；验收以真实代码、测试和 CI 证据为准。
