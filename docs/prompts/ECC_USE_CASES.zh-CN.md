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
> - 跨仓任务按真实依赖建图：Backend 向 Flutter/Admin 提供 REST/WS；geo_regions 是独立地区数据 SDK，当前由 Flutter 消费，不是后端 API 服务。
> - 三端 EntityId/TSID 只比较 Backend、Flutter、Admin；geo_regions 的 country/region ID 与 pathIds 是独立稳定标识，不得强行当作 TSID。
> - 功能状态必须结合编译 profile、服务端 feature gate、权限与真实验收判断；Handler、页面、测试或配置存在不等于功能已对用户开放。
>
> **案例编号全局唯一**：ECC-001 至 ECC-125。四个仓库按职责分配，后端主仓提供统一索引。

## 本仓库范围

本文件包含后端与跨仓工程案例 44 个，以及 IMBoy 公共功能场景/新模型复审案例 24 个；合计 68 个（ECC-001、005–022、043–045、051–089、110–116）。

> **时间估算说明**：时间为单个案例的一次 A/B/C 模式执行预算，包含仓库阅读、代码分析、必要实施及相关验证；不是保证完成时间。A/B/C 只能选择一种执行。30 分钟及以上建议安排 ZCODE 闲时任务。遇到缺少设备、数据库、凭据、外部服务或需求决策时，应报告 BLOCKED/NOT RUN，不能为了符合预算而跳过验证。复杂度按当前仓库结构与案例提示词范围估算，实际会随工作树状态、测试环境和缺陷数量变化。
## 案例清单

### 项目理解与架构分析

#### ECC-001 · 后端完整架构梳理

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `codebase-onboarding`

```text
$codebase-onboarding

只读分析当前 imboy 后端仓库。梳理 Erlang/OTP 技术栈、应用启动链路、Supervisor 树、HTTP/WS 路由、Handler → Logic → DS → Repo 四层架构、数据库、业务模块和测试体系。输出真实文件路径、关键调用链、模块职责、现有架构约束和风险。优先复用现有架构地图，不创建或修改文件。
```

#### ECC-005 · 单聊消息完整调用链

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `change-impact`

```text
$change-impact

只读追踪单聊消息从客户端发送、后端 WebSocket 接收、消息校验、业务处理、PostgreSQL 持久化、在线投递、离线投递、ACK 到客户端状态更新的完整链路。逐项定位真实代码，识别可靠性边界、重试、幂等性、多设备和潜在丢消息风险。区分代码事实、推断和待验证项。
```

#### ECC-006 · WebSocket Action 注册和扩展

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `change-impact` · `contract-first`

```text
$change-impact $contract-first

分析 WebSocket Action 的注册、路由、分派和处理机制，重点检查 imboy_ws_action_registry、message_router_logic 及相关 Logic 模块。评估新增 Action 的最小改动范围、协议兼容性、认证授权、输入校验和测试要求。只读输出，不修改代码。
```

#### ECC-007 · 三仓依赖关系分析

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `contract-first`

```text
$contract-first

分析 imboy、imboy-flutter、imboy-admin-frontend 与 geo_regions 四仓的真实依赖关系。分别建立 Backend REST/WS → Flutter、Backend REST/WS → Admin、geo_regions SDK → Flutter 的提供方、消费方和契约真源矩阵；不要假设四仓两两直接依赖。核对 OpenAPI、API/WS 字段、错误码、枚举、可空性、TSID/EntityId、WebSocket 消息格式，以及 geo_regions 的 PROTOCOL.md、CONSUMER_CONTRACT.md、stable region ID/pathIds、locale/revision/hash/quality 状态。geo_regions 是独立地区数据 SDK，不是 REST/WS 服务，也不纳入 TSID 对齐。逐仓核对实际 HEAD、工作树、依赖版本和尚未机器验证的契约。
```

#### ECC-008 · 文档与代码一致性审计

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `docs-governance` · `living-docs-governance`

```text
$docs-governance $living-docs-governance

只读审计后端文档。对比 AGENTS.md、CLAUDE.md、README、架构地图、模块文档、测试说明与实际代码。找出过期命令、错误路径、已完成事项仍标记未完成、文档矛盾和重复真源。优先复用 docs/README.md 的分类规则，输出按风险排序的问题，不自动重写文档。
```

### Erlang/OTP 后端

#### ECC-009 · 新增 REST API

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `api-design` · `contract-first` · `tdd-workflow`

```text
$api-design $contract-first $tdd-workflow

在 IMBoy 后端实现指定 REST API。先检查现有路由、Handler、Logic、DS、Repo 和 OpenAPI 约定，定义接口契约，先写失败测试，再实现最小功能。遵守四层架构、TSID、统一错误码、认证授权和现有命名。完成后运行相关测试、契约和架构门禁。仅修改本任务必需文件。
```

#### ECC-010 · API 错误码和分页治理

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `api-design` · `contract-first`

```text
$api-design $contract-first

检查 REST API 的错误码、分页参数、返回结构和字段命名。识别不一致及兼容性问题，核对 OpenAPI 与真实 Handler/Logic 实现及客户端消费。输出统一建议和受影响消费者；未经批准不改变公共 API。
```

#### ECC-011 · 新增 WebSocket Action

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `contract-first` · `tdd-workflow`

```text
$contract-first $tdd-workflow

为 IMBoy 新增指定 WebSocket Action。先研究 Action Registry、消息分派、协议字段和权限机制，定义输入输出契约，编写正向、非法输入、权限失败、重复请求和兼容性测试，再实施。复用现有注册机制，不得硬编码旁路分派。
```

#### ECC-012 · 单聊 ACK 和可靠投递

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `change-impact` · `tdd-workflow` · `test-collaboration`

```text
$change-impact $tdd-workflow $test-collaboration

审查单聊消息 ACK、重发、离线投递和设备维度确认机制。覆盖重复投递、ACK 丢失、客户端重连、消息乱序、多设备同步和节点切换。先建立可复现测试，再修复确认的缺陷；保留现有 QoS 语义。
```

#### ECC-013 · 群聊消息时序治理

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `change-impact` · `module-regression`

```text
$change-impact $module-regression

分析群聊消息 conv_seq、accepted_at、消息排序、成员可见性和多节点投递机制。找出乱序、重复、消息缺失和时间线不一致路径。输出影响模块、数据不变量和回归命令；不要推断未实测的分布式保证。
```

#### ECC-014 · 消息撤回和编辑时间窗

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `ai-regression-testing` · `tdd-workflow`

```text
$ai-regression-testing $tdd-workflow

审查消息撤回、编辑及可信时间窗逻辑。验证边界时间、低可信时间回退、服务端时间、重复请求、客户端 ACK 和多端状态同步。先写能复现问题的测试，再进行最小修复；保留历史协议兼容。
```

#### ECC-015 · 消息投递性能和背压

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `latency-critical-systems` · `benchmark-optimization-loop`

```text
$latency-critical-systems $benchmark-optimization-loop

分析单聊、群聊消息投递热路径。测量吞吐量、p50/p95/p99 延迟、队列积压、进程数量、RPC 开销、重试次数和数据库耗时。先建立可重复基线，每次只验证一个优化假设，不允许通过丢消息、跳过 ACK 或降低可靠性换取性能。未获得环境授权时只输出测试计划。
```

#### ECC-016 · OTP Supervisor 容错分析

**预计执行时间：25 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `change-impact` · `architecture-decision-records`

```text
$change-impact $architecture-decision-records

审计 OTP Supervisor 树、Worker 生命周期、重启策略和进程间依赖。分析子进程崩溃、重复重启、依赖不可用和节点重启时的影响。给出故障传播图、恢复机制和测试缺口。若涉及架构决策，先提供 ADR 草案，不自动写入。
```

#### ECC-017 · 四层架构越层调用检查

**预计执行时间：25 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `module-governance` · `module-regression`

```text
$module-governance $module-regression

审计 Handler → Logic → DS → Repo 四层架构。通过真实调用关系和现有 check_module_boundaries 脚本检查跨层调用、循环依赖和例外。识别新增违规与历史例外，输出最小治理方案；不得为消除告警而随意搬迁业务逻辑。
```

#### ECC-018 · PostgreSQL 慢查询优化

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `benchmark` · `latency-critical-systems`

```text
$benchmark $latency-critical-systems

审查 PostgreSQL 慢查询和热点 SQL。结合 Repo 层真实 SQL、索引定义、执行计划和现有性能测试，识别全表扫描、重复查询、低效分页和锁竞争。先建立可复现性能基线，再提出优化方案。遵守全栈不引入 Redis 的项目约束。
```

#### ECC-019 · 数据库迁移安全审查

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `database-migrations` · `change-impact`

```text
$database-migrations $change-impact

审计 priv/migrations 中数据库迁移。检查 up/down 配对、锁风险、历史数据兼容、索引创建、expand/contract 顺序和蓝绿发布兼容性。遵循仓库迁移编号与 check_migrations.sh 等现有门禁。不要执行生产迁移，不修改已发布迁移。
```

#### ECC-020 · E2EE 服务端零密码学约束

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `security-review` · `ai-regression-testing`

```text
$security-review $ai-regression-testing

审计服务端 E2EE 零密码学约束。检查私钥、明文、密钥派生、加解密和敏感数据是否进入服务端处理路径。使用现有 zero-crypto 检查脚本和相关测试验证。不得通过关闭检查、扩大白名单或泄漏敏感数据掩盖问题。
```

#### ECC-021 · API 鉴权、权限和限流

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `security-review` · `tdd-workflow`

```text
$security-review $tdd-workflow

审计 REST、WebSocket、Admin API 的认证、授权、资源归属、越权访问、频率限制和输入验证。重点关注用户身份、群成员权限、管理员权限和跨组织数据访问。对确认的问题补充自动化安全回归测试；未经授权不执行攻击性或生产测试。
```

#### ECC-022 · 附件上传与下载安全

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `security-review` · `change-impact`

```text
$security-review $change-impact

审计附件上传、对象存储、访问 URL、下载授权、文件类型、文件大小和生命周期。检查现有存储适配及附件 registry。验证跨用户访问、过期链接和未授权下载问题。输出代码证据与回归建议，不下载或披露真实附件和凭据。
```

### 三仓契约与 geo_regions

#### ECC-043 · 跨仓 API 与 SDK 契约漂移检查

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `contract-first` · `api-design`

```text
$contract-first $api-design

检查后端 OpenAPI 真源、生成产物和 Flutter/Admin REST/WS 消费端；另行检查 geo_regions 的 PROTOCOL.md、CONSUMER_CONTRACT.md 与 Flutter host adapter，把它作为独立 SDK/数据契约，不要混成 OpenAPI。验证请求响应字段、可空性、错误码、枚举、认证、版本兼容，以及地区 stable ID/pathIds、locale、revision、manifest/hash 和质量状态。运行各仓已有 contract gate，输出真实漂移位置、消费方、当前 SHA 和修复顺序。
```

#### ECC-044 · 三端 EntityId/TSID 一致性

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `contract-first` · `test-collaboration`

```text
$contract-first $test-collaboration

审计 Backend、Flutter 和 Admin 三端的 TSID/EntityId 定义、序列化、解析和比较。检查 64 位精度、JSON 整数、字符串映射、数据库字段和 API 传输。找出类型不一致并建立可执行契约测试；不得用 JavaScript Number 或 parseInt 处理 TSID。geo_regions 不使用 IMBoy TSID：单独核验 countryCode、稳定 region ID、pathIds 与 revision 的身份语义，禁止把地区 ID 与用户/群组实体 ID 混用。
```

#### ECC-045 · 错误码和枚举一致性

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `contract-first` · `tdd-workflow`

```text
$contract-first $tdd-workflow

审计后端错误码、业务枚举及 Flutter/Admin 镜像定义。以后端权威定义为准，检查遗漏、废弃值、错误映射和前端未处理分支。运行现有 contract gate，不允许静默忽略未知枚举。
```

### 测试、文档、安全和运维

#### ECC-051 · 建立后端测试资产地图

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `test-collaboration`

```text
$test-collaboration

为后端建立测试资产地图。检查 EUnit、Common Test、integration、performance、stress 和 CI。识别关键业务规则、稳定 TEST-ID、测试入口、外部依赖和覆盖缺口。优先复用 test/README.md 和现有测试索引，不创建重复测试说明。
```

#### ECC-052 · 历史缺陷自动回归

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `ai-regression-testing` · `tdd-workflow`

```text
$ai-regression-testing $tdd-workflow

检查当前修复涉及的历史缺陷、测试失败记录和已有回归用例。找出尚未被自动化保护的重要问题，为每个问题建立最小可复现测试。必须证明新测试能在修复前失败、修复后通过；不得删除或弱化既有测试。
```

#### ECC-053 · 下游模块回归分析

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `module-regression` · `change-impact`

```text
$module-regression $change-impact

分析本次代码变更对其他模块的影响。基于真实调用关系、公开接口和跨仓依赖，确定必须重跑的测试。输出变更模块、下游消费者、精确回归命令、执行结果和未验证边界。
```

#### ECC-054 · CI 假绿和跳过测试审计

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `test-collaboration` · `verification-loop`

```text
$test-collaboration $verification-loop

审计 GitHub Actions 测试门禁。检查 continue-on-error、条件跳过、缺少报告、零执行、异常退出码、未批准 skip 和证据未对账。识别哪些绿灯不能证明业务通过。不得修改门禁以降低通过标准。
```

#### ECC-055 · 文档完整性审计

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `docs-governance`

```text
$docs-governance

审计后端文档的相对链接、重复内容、孤立文档、过期 ADR、错误引用和目录索引。遵循 docs/README.md 与 documentation-system 规则，先识别权威文档，不创建第二套文档体系。输出可自动修复和需人工确认的问题。
```

#### ECC-056 · 记录架构决策 ADR

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `architecture-decision-records`

```text
$architecture-decision-records

针对当前架构决策整理背景、约束、备选方案、选择理由、正负面影响和回滚风险。先读取现有 ADR，避免重复记录。先展示完整 ADR 草案，未经确认不要创建或修改文件。
```

#### ECC-057 · 长期项目文档治理

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `living-docs-governance`

```text
$living-docs-governance

为后端建立长期文档治理方案。盘点 AGENTS.md、CLAUDE.md、README、架构地图、路线图、状态文档和历史记录。映射规则、地图、状态和历史四类角色。优先复用现有文件，不创建平行真源；遵循 docs/README.md 对执行计划和临时证据的入库限制。
```

#### ECC-058 · 全项目安全审查

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `security-review`

```text
$security-review

对后端进行分层安全审查。覆盖认证授权、会话、E2EE、TSID、文件上传、附件访问、数据库、日志、敏感配置和跨组织隔离。每个发现必须提供真实文件和代码证据，区分已证实漏洞与待验证风险。
```

#### ECC-059 · AI Agent 安全治理

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `agent-security-hardening` · `agent-architecture-audit`

```text
$agent-security-hardening $agent-architecture-audit

审计 IMBoy AI Agent、Bot、MCP 和工具调用体系。分析 Prompt Injection、工具越权、外部内容污染、跨用户数据泄漏、敏感信息输出和自动执行风险。明确每个工具的权限、身份校验、输入校验和审计证据。
```

#### ECC-060 · MCP 依赖供应链审计

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `mcp-dependency-review`

```text
$mcp-dependency-review

只读审计仓库中的 MCP 配置。检查服务器来源、版本固定、可变依赖、启动命令、环境变量、凭据暴露和执行权限。不得启动未知 MCP 服务，不安装未经审查的依赖，不输出秘密值。
```

#### ECC-061 · 合并后的生产风险审计

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `production-audit`

```text
$production-audit

针对当前已合并到 main 的代码进行生产风险审计。检查实际 HEAD、最近变更、CI、数据库迁移、环境配置、部署脚本、健康检查、日志和回滚路径。输出 SHIP/BLOCK 建议及证据。只读，不实际部署。
```

#### ECC-062 · 蓝绿部署和数据库迁移审查

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `deployment-patterns` · `change-impact`

```text
$deployment-patterns $change-impact

审查 scripts/imboy-deploy.sh 蓝绿发布流程。检查本地 HEAD、新版本构建、expand migration、旧节点停止、数据库备份、迁移台账、健康检查、流量切换和回滚边界。以现有脚本为准，不套用通用发布模板。只读分析。
```

#### ECC-063 · Docker 和安装脚本治理

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `docker-patterns` · `production-audit`

```text
$docker-patterns $production-audit

审计 Docker Compose、Dockerfile、安装包、安装脚本和环境配置。检查依赖健康检查、启动顺序、数据持久化、权限、镜像固定、升级和失败恢复。遵守全栈禁 Redis及现有部署架构，不复制 ECC 示例中的基础设施配置。
```

#### ECC-064 · PostgreSQL 备份恢复演练方案

**预计执行时间：45 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `production-audit` · `database-migrations`

```text
$production-audit $database-migrations

审查 PostgreSQL 备份、恢复和恢复冒烟流程。检查备份完整性、扩展依赖、附件存储一致性、恢复顺序、恢复后校验和 RTO/RPO 证据。先输出隔离环境演练计划，不操作生产数据库。
```

#### ECC-065 · GitHub PR 和 CI 管理

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `github-ops`

```text
$github-ops

检查 imboy-pub/imboy 的 GitHub PR、CI 失败、未解决 Review、过期分支和 Release 状态。结合真实 GitHub 数据分类处理优先级。先输出处理建议，不自动合并、关闭 PR 或删除分支。
```

#### ECC-066 · Git Worktree 和分支清理

**预计执行时间：20 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `git-workflow`

```text
$git-workflow

审计本地 worktree、分支、远端分支和 GitHub PR 关系。识别已合并、过期、仍有 WIP 或被其他任务使用的工作区。输出逐项清理清单和安全检查命令。未经确认不删除文件、分支或 worktree。
```

### 性能优化与 Agent 协作

#### ECC-067 · WebSocket 性能基线

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `benchmark` · `latency-critical-systems`

```text
$benchmark $latency-critical-systems

为 IMBoy WebSocket 建立性能基线。覆盖连接建立、认证、心跳、消息发送、接收、ACK、广播和断线重连。记录并发连接数、吞吐量、p50/p95/p99、CPU、内存、进程数和错误率。区分已有实测数据与待执行测试，不得宣传未经实测的百万连接能力。
```

#### ECC-068 · 性能持续优化

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `benchmark-optimization-loop` · `latency-critical-systems`

```text
$benchmark-optimization-loop $latency-critical-systems

针对当前性能瓶颈建立基线和可重复优化循环。先定位瓶颈，再提出单变量优化假设，逐一实施和测量。保留正确性、消息可靠性和安全约束。记录命令、环境、结果、变化幅度及回归证据；生产负载测试需另行授权。
```

#### ECC-069 · CI 和部署耗时优化

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `benchmark-optimization-loop` · `parallel-execution-optimizer`

```text
$benchmark-optimization-loop $parallel-execution-optimizer

分析 GitHub Actions、scripts 和部署链路耗时。记录依赖安装、编译、测试、打包、上传和远程部署各阶段时间。找出重复工作和可安全并行环节。优化后证明发布始终使用最新已授权代码，不能复用过期产物提速。
```

#### ECC-070 · 最多 5 个 Agent 并行开发

**预计执行时间：90 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `team-agent-orchestration` · `parallel-execution-optimizer`

```text
$team-agent-orchestration $parallel-execution-optimizer

为当前任务设计最多 5 个 Agent 的并行执行方案。先检查依赖关系和共享资源，划分独立 worktree、分支、文件写入范围、负责人、验收命令和合并条件。主 Agent 负责集成和最终验证。遵守仓库 AI 工程任务契约与协调锁；禁止多个 Agent 同时写同一文件或共享数据库。
```

#### ECC-071 · IMBoy AI Agent Runtime 架构审计

**预计执行时间：60 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `agent-architecture-audit` · `agent-security-hardening`

```text
$agent-architecture-audit $agent-security-hardening

全面审计 IMBoy AI Agent Runtime。检查 Prompt、上下文、记忆、模型调用、工具注册、MCP、权限、执行器、重试、错误处理和结果持久化。追踪用户输入到工具副作用的完整链路，识别隐藏重试、工具越权、记忆污染和多租户隔离风险。
```

#### ECC-072 · AI 编码 Agent 能力对比

**预计执行时间：25 分钟**（单次执行；包含代码阅读、定向实施/分析与相关验证）

**Skills：** `agent-eval` · `benchmark-optimization-loop`

```text
$agent-eval $benchmark-optimization-loop

设计 IMBoy 专用 Coding Agent 评测集，比较 Codex、Claude Code 等 Agent 在 Erlang、Flutter、React、跨仓契约、Bug 修复、测试治理和复杂重构方面表现。固定仓库提交、任务输入、验收命令和环境，记录成功率、耗时、成本、回归缺陷及重复运行一致性。先制定评测方案，不编造测试成绩。
```


## IMBoy 公共业务场景复刻（Backend / 跨端业务真源）

> 本组面向客户业务场景复刻。每个场景提供三条独立提示词：A 新增功能；B 优化现有功能（重点适用于新模型复审旧 AI 代码）；C 测试现有功能。将提示词中的“客户”替换为具体客户、行业、角色和目标流程。



#### ECC-073 · 客户私聊与多设备消息

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 个人即时通讯

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户私聊与多设备消息（个人即时通讯）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户私聊与多设备消息（个人即时通讯）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户私聊与多设备消息（个人即时通讯）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-074 · 客户群聊、社群运营与群治理

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 群组与社群协作

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户群聊、社群运营与群治理（群组与社群协作）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户群聊、社群运营与群治理（群组与社群协作）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户群聊、社群运营与群治理（群组与社群协作）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-075 · 客户内容频道、知识订阅与付费内容

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 频道与订阅内容

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户内容频道、知识订阅与付费内容（频道与订阅内容）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-076 · 客户动态、朋友圈与内容互动

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 朋友圈与轻社交

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户动态、朋友圈与内容互动（朋友圈与轻社交）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-077 · 客户企业组织、团队空间和成员管理

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 组织与工作区

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户企业组织、团队空间和成员管理（组织与工作区）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户企业组织、团队空间和成员管理（组织与工作区）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户企业组织、团队空间和成员管理（组织与工作区）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-078 · 客户项目、任务、里程碑和活动流

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 项目协作

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户项目、任务、里程碑和活动流（项目协作）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户项目、任务、里程碑和活动流（项目协作）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户项目、任务、里程碑和活动流（项目协作）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-079 · 客户网站在线客服、排队和坐席工作台

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 客服中心与访客挂件

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户网站在线客服、排队和坐席工作台（客服中心与访客挂件）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-080 · 客户联系人、企业会话、资产和离岗交接

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 企业客户经营 CRM

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户联系人、企业会话、资产和离岗交接（企业客户经营 CRM）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-081 · 客户知识助手、群内助手和受控自动化

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** AI 助手与 Agent

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户知识助手、群内助手和受控自动化（AI 助手与 Agent）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-082 · 客户第三方系统机器人和事件集成

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 开发者 Bot 与 Webhook

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户第三方系统机器人和事件集成（开发者 Bot 与 Webhook）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-083 · 客户私密聊天、群组加密与密钥恢复

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 端到端加密通信

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户私密聊天、群组加密与密钥恢复（端到端加密通信）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-084 · 客户文件共享、图片视频和安全下载

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 附件与文件中心

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户文件共享、图片视频和安全下载（附件与文件中心）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户文件共享、图片视频和安全下载（附件与文件中心）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户文件共享、图片视频和安全下载（附件与文件中心）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-085 · 客户一对一通话、群通话边界与直播互动

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 音视频通话与直播

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户一对一通话、群通话边界与直播互动（音视频通话与直播）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先核对 Flutter docs/reference/feature-status.md 与后端当前编译 profile、运行时 feature gate：当前一期组合只开放已登录 App 用户的一对一 rtc；群通话必须同时选入并启用 rtc_group 与父功能 rtc，而当前组合未选入 rtc_group；访客和 Web 坐席音视频也不随一对一开放。不得把 LiveKit/RTC handler、群通话代码或测试存在等同于群通话已开放。分别核验编译选择、运行开关、好友/黑名单授权、房间类型、呼叫状态机、媒体转发和真实设备验收；未执行真实双端媒体验收不得标 PASS。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户一对一通话、群通话边界与直播互动（音视频通话与直播）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先核对 Flutter docs/reference/feature-status.md 与后端当前编译 profile、运行时 feature gate：当前一期组合只开放已登录 App 用户的一对一 rtc；群通话必须同时选入并启用 rtc_group 与父功能 rtc，而当前组合未选入 rtc_group；访客和 Web 坐席音视频也不随一对一开放。不得把 LiveKit/RTC handler、群通话代码或测试存在等同于群通话已开放。分别核验编译选择、运行开关、好友/黑名单授权、房间类型、呼叫状态机、媒体转发和真实设备验收；未执行真实双端媒体验收不得标 PASS。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户一对一通话、群通话边界与直播互动（音视频通话与直播）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先核对 Flutter docs/reference/feature-status.md 与后端当前编译 profile、运行时 feature gate：当前一期组合只开放已登录 App 用户的一对一 rtc；群通话必须同时选入并启用 rtc_group 与父功能 rtc，而当前组合未选入 rtc_group；访客和 Web 坐席音视频也不随一对一开放。不得把 LiveKit/RTC handler、群通话代码或测试存在等同于群通话已开放。分别核验编译选择、运行开关、好友/黑名单授权、房间类型、呼叫状态机、媒体转发和真实设备验收；未执行真实双端媒体验收不得标 PASS。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-086 · 账号注册、登录与联系人关系

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 账号认证、设备会话与社交关系

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户账号注册、登录与联系人关系（账号、好友与社交关系）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

认证链路入口与测试锚点：src/api/passport_handler.erl、auth_oidc_handler.erl、qr_login_handler.erl、qr_login_sse_handler.erl、wechat_mini_auth_handler.erl、user_device_handler.erl；src/logic/passport_logic.erl、auth_oidc_logic.erl、qr_login_logic.erl、wechat_mini_auth_logic.erl、user_device_logic.erl、login_security_logic.erl；test/api/passport_handler_tests.erl、qr_login_handler_tests.erl、qr_login_sse_handler_tests.erl、wechat_mini_auth_handler_tests.erl、user_device_handler_tests.erl、test/ds/auth_session_ds_tests.erl。以上仅为当前代码入口，执行时重新核对 HEAD。不要把注册/登录误写成实名核验；区分密码登录、OIDC、扫码登录、微信小程序认证、设备会话和联系人关系的真实状态。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户账号注册、登录与联系人关系（账号、好友与社交关系）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

认证链路入口与测试锚点：src/api/passport_handler.erl、auth_oidc_handler.erl、qr_login_handler.erl、qr_login_sse_handler.erl、wechat_mini_auth_handler.erl、user_device_handler.erl；src/logic/passport_logic.erl、auth_oidc_logic.erl、qr_login_logic.erl、wechat_mini_auth_logic.erl、user_device_logic.erl、login_security_logic.erl；test/api/passport_handler_tests.erl、qr_login_handler_tests.erl、qr_login_sse_handler_tests.erl、wechat_mini_auth_handler_tests.erl、user_device_handler_tests.erl、test/ds/auth_session_ds_tests.erl。以上仅为当前代码入口，执行时重新核对 HEAD。不要把注册/登录误写成实名核验；区分密码登录、OIDC、扫码登录、微信小程序认证、设备会话和联系人关系的真实状态。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户账号注册、登录与联系人关系（账号、好友与社交关系）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

认证链路入口与测试锚点：src/api/passport_handler.erl、auth_oidc_handler.erl、qr_login_handler.erl、qr_login_sse_handler.erl、wechat_mini_auth_handler.erl、user_device_handler.erl；src/logic/passport_logic.erl、auth_oidc_logic.erl、qr_login_logic.erl、wechat_mini_auth_logic.erl、user_device_logic.erl、login_security_logic.erl；test/api/passport_handler_tests.erl、qr_login_handler_tests.erl、qr_login_sse_handler_tests.erl、wechat_mini_auth_handler_tests.erl、user_device_handler_tests.erl、test/ds/auth_session_ds_tests.erl。以上仅为当前代码入口，执行时重新核对 HEAD。不要把注册/登录误写成实名核验；区分密码登录、OIDC、扫码登录、微信小程序认证、设备会话和联系人关系的真实状态。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-087 · 客户充值、支付、账单和付费权益

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 钱包、支付与订阅

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户充值、支付、账单和付费权益（钱包、支付与订阅）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-088 · 客户用户、群组、频道和内容发现

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 搜索与发现

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户用户、群组、频道和内容发现（搜索与发现）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户用户、群组、频道和内容发现（搜索与发现）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户用户、群组、频道和内容发现（搜索与发现）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-089 · 客户白标入口、功能开关和版本升级

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 应用配置与版本运营

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：客户白标入口、功能开关和版本升级（应用配置与版本运营）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-110 · 新一代 AI 模型复审与改进旧 AI 生成代码

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

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

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 多设备在线、推送送达和通知偏好

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：设备、推送与通知中心（多设备在线、推送送达和通知偏好）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：设备、推送与通知中心（多设备在线、推送送达和通知偏好）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：设备、推送与通知中心（多设备在线、推送送达和通知偏好）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-112 · 用户关系扩展与位置服务

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 收藏、标签、黑名单、好友分类和附近的人

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：用户关系扩展与位置服务（收藏、标签、黑名单、好友分类和附近的人）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：用户关系扩展与位置服务（收藏、标签、黑名单、好友分类和附近的人）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：用户关系扩展与位置服务（收藏、标签、黑名单、好友分类和附近的人）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-113 · 举报、内容审核与申诉

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 社区内容审核、用户举报和申诉工作流

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：举报、内容审核与申诉（社区内容审核、用户举报和申诉工作流）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：举报、内容审核与申诉（社区内容审核、用户举报和申诉工作流）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：举报、内容审核与申诉（社区内容审核、用户举报和申诉工作流）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-114 · 直播间、通话记录与录制治理

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 直播、RTC 房间、通话记录和录制回放

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：直播间、通话记录与录制治理（直播、RTC 房间、通话记录和录制回放）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：直播间、通话记录与录制治理（直播、RTC 房间、通话记录和录制回放）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：直播间、通话记录与录制治理（直播、RTC 房间、通话记录和录制回放）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-115 · 插件、企业应用与单点登录

**预计执行时间（单选一种模式）：A 新增功能 180 分钟 · B 优化现有功能 120 分钟 · C 测试现有功能 90 分钟**

**场景：** 客户插件扩展、OA 集成和 OIDC/SSO

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：插件、企业应用与单点登录（客户插件扩展、OA 集成和 OIDC/SSO）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：插件、企业应用与单点登录（客户插件扩展、OA 集成和 OIDC/SSO）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：插件、企业应用与单点登录（客户插件扩展、OA 集成和 OIDC/SSO）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

#### ECC-116 · 运营通知、反馈与可观测性

**预计执行时间（单选一种模式）：A 新增功能 120 分钟 · B 优化现有功能 90 分钟 · C 测试现有功能 60 分钟**

**场景：** 公告、用户反馈、平台统计和运营审计

**A · 新增功能 · `product-capability` + `orch-add-feature` + `tdd-workflow`**

```text
$product-capability $orch-add-feature $tdd-workflow

业务场景：运营通知、反馈与可观测性（公告、用户反馈、平台统计和运营审计）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：为该场景新增功能。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

读取该场景对应的领域概念、路由/接口、数据模型、Flutter/Admin 消费入口和现有测试，先做能力盘点：哪些可直接复用、哪些可通过配置组合、哪些确实缺失。不得重复造轮子或把未开放能力当作已交付。

为真正缺失的能力形成 capability contract：角色与权限、用户旅程、状态机、输入输出、数据归属、API/WS、迁移兼容、失败恢复、观测和验收标准。客户输入不完整时列出阻塞问题并停在方案阶段；需求明确时按 orch-add-feature 的计划门禁执行，先写失败测试再实现最小差异。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

运行相关定向测试和契约/架构门禁。只修改批准范围，不 commit、不 push、不合并、不部署。
```

**B · 优化现有功能 / 新模型复审旧 AI 代码 · `brownfield-retrospective` + `tdd-workflow` + `verification-loop`**

```text
$brownfield-retrospective $tdd-workflow $verification-loop

业务场景：运营通知、反馈与可观测性（公告、用户反馈、平台统计和运营审计）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：优化现有功能，重点用于新一代 AI 模型复审和改进旧 AI 模型生成的代码。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

按 brownfield-retrospective 的 slice-first 方法，把范围限制在约 15 个文件或 2000 行以内。不要信任旧模型自评、注释或现有测试完整性；独立重建真实行为、调用链、数据流、权限边界、失败路径、并发/竞态、资源生命周期和跨端契约，建立改动前测试/性能/错误行为基线。

重点检查：逻辑正确性、边界条件、异常处理、幂等与重试、安全、性能热点、重复/过度抽象、可维护性和跨端一致性；特别排查只改响应未改查询/持久化、只修主路径漏掉其他分支、字段/可空性漂移、异常吞掉、UI-only 权限、mock 通过但真实路径失败、测试与实现共享错误假设等 AI 常见盲点。

每项发现提供文件/行号、证据、影响、风险和最小修复方案。优先行为保持型优化；先新增/修正测试证明问题，再做最小改动。涉及公共 API/WS、数据库结构、E2EE、权限语义或用户可见行为变化时先请求确认。Erlang/OTP、Cowboy、PostgreSQL、Handler → Logic → DS → Repo、API/WS 契约、数据库迁移和跨端兼容。

先在会话中输出基线和差距清单；不要自动创建 docs/retro 或重复报告。对范围内明确且行为保持的改进可以实施；完成后运行相关测试、静态检查和 diff review，报告改动前后证据与未验证项。不 commit、不 push、不合并、不部署。
```

**C · 测试现有功能 · `test-collaboration` + `ai-regression-testing` + `verification-loop`**

```text
$test-collaboration $ai-regression-testing $verification-loop

业务场景：运营通知、反馈与可观测性（公告、用户反馈、平台统计和运营审计）。客户信息：[填写客户行业、用户角色、业务目标、关键流程、特殊业务规则、数据规模和验收指标]。本次范围：[填写具体端、模块、功能切片]。

任务：测试现有功能，独立验证旧 AI 模型生成的实现。

先核对当前仓库 HEAD、工作区状态、AGENTS.md/CLAUDE.md、架构/产品文档、API/WS/数据契约、真实代码和测试。以代码与可执行证据为准，标记 CURRENT、TARGET、LOCKED、UNKNOWN、NOT_VERIFIED；不能把文档/API 存在等同于功能已经端到端交付。

先不要修改业务实现。盘点该场景已有单元、集成、契约、E2E、平台/设备测试和 CI 门禁，建立“需求/不变量 → 测试资产或 TEST-ID → 命令 → 证据”矩阵。覆盖正常路径、权限拒绝、非法输入、边界值、重复请求、并发、超时、故障恢复、旧数据兼容和跨端行为。

针对 AI 生成代码特别检查 mock/sandbox 与真实路径不一致、数据库查询/写入字段遗漏、序列化遗漏、仅测 happy path、异常吞掉、重试重复副作用和测试 fixture 迎合实现。只新增有区分力的测试，不复制重复用例、不降低阈值、不扩大 skip。EUnit、Common Test、API/WS contract gates、架构边界脚本和仓库权威测试入口。

逐项报告 PASS/FAIL/BLOCKED/NOT RUN、真实命令、退出码、测试数量和证据位置。发现缺陷先提供复现和最小修复建议；未经授权不修改业务代码。不得把未执行或环境缺失标记为 PASS。
```

## 维护说明

- 本文只维护本仓库职责范围内的提示词；跨仓案例的统一索引位于 [imboy 后端主仓 docs/prompts/README.md](https://github.com/imboy-pub/imboy/blob/main/docs/prompts/README.md)。
- 修改案例时保持 ECC 编号不变；新增案例先在后端主仓索引登记编号和归属，避免重复。
- Skill 名称必须与 ECC `skills/<name>/SKILL.md` 中的目录名一致；ECC 升级后应重新核对可用性。
- 提示词是任务入口，不是执行结果或质量证明；验收以真实代码、测试和 CI 证据为准。
