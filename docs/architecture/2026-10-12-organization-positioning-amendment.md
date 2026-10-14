# Organization 定位修订：业务对象，非租户（ADR）

> STATUS: `ACCEPTED`（产品 owner 裁决，2026-10-12；M0-M3 已实施，见 §5）
>
> AUTHORITY: 本文修订 `2026-09-16-enterprise-organization-v1-core-contract.md` 的
> Organization 语义定位，并约束所有把 Organization 当作**授权边界**的实现与合同
> （首当其冲：`2026-09-21-epgz05-oa-sso-contract.md` OA 免登全链）。
> 与既有冻结合同冲突时，**本文效力更高**；实施按 §5 迁移计划推进，禁止静默改约。
>
> 裁决原文（owner，2026-10-12）：
> 「organization 在 IMBoy 里不是租户，是业务对象。如果系统不是这么设计的，需要修改为这样。」

## 1. 决策（不可再误解）

1. **Organization 不是租户（tenant）**。它是 IMBoy 的**业务对象**：实例内对企业/组织
   单元的业务建模，服务于资源归属、目录、工作台分组等业务场景。
2. **授权与隔离边界是部署实例（instance）**。私有化部署下，实例即客户环境；实例内
   不存在第二层「安全租户」边界。
3. **对外协议的授权主体是应用与凭证本身**（实例级）：OA 免登与
   `/api/internal/v1/*` Application Credential 面的授权判定不依赖 Organization。

任何后续设计文档、代码注释、AI 上下文不得再把 Organization 表述为「租户」或
「安全/授权边界」。

## 2. 三个概念的钉死

**术语澄清（2026-10-12，owner 追问确认）**：`enterprise` 前缀 ≠ `organization`。
`enterprise_*`（enterprise_oa_sso_logic / enterprise_application / /api/internal/v1 等）
是「企业集成域」的**域命名空间**（IMBoy 对接企业外部系统的能力面），不是业务实体；
`organization` 是域内/产品内的一个**业务对象**。两者不统一、不互改：把
`enterprise_oa_sso_logic` 改名 `organization_*` 会把已拆除的 org 授权边界重新焊回
模块名，且表名/路由/凭证前缀改名是零收益的破坏面。需要修正的是注释与文档中把
enterprise 域表说成「归 Organization 租户所有」的**语义**（如
`enterprise_application.organization_id` 列注释「资源 owner 租户」→「业务归属」），
不是标识符本身。

| 概念 | 语义 | Organization 是否参与 |
|---|---|---|
| 寻址 addressing | 定位业务资源（这个群/应用属于哪个组织单元） | 参与（作为业务标识符） |
| 归属 ownership | 资源的业务归属与展示分组 | 参与（业务归属，非授权） |
| 授权 authorization | 谁能调什么、能换到谁的身份 | **不参与**；边界 = 实例 + 凭证 scope + 用户身份 |

## 3. 被本修订推翻 / 降级的既有表述

| 出处 | 原表述 | 修订后 |
|---|---|---|
| core-contract C02 | 「Organization 是企业租户与企业资源 owner」 | Organization 是业务对象；「资源 owner」降级为**业务归属**，非授权边界 |
| core-contract C02 | 「跨 Org FK/访问 fail closed」 | 降级为**归属完整性约束**（FK 一致性），不再是安全授权边界 |
| EPGZ-05 §3.4 | 签发需「请求者是该 org 的 active Human member」+ org active | 移除 org 环节；仅需 active Human 账号 |
| EPGZ-05 §2 | code 绑定五元组 `org/app/user/redirect_uri/nonce` | 变**四元组** `app/user/redirect_uri/nonce` |
| EPGZ-05 NEG-05 | 跨 org credential 换 org A code | **删除**（org 不再是绑定维度） |
| EPGZ-05 `organization_id` 字段 | 「多企业客户端必须传」 | 兼容期容忍并忽略，标记 deprecated，观测归零后移除 |
| `enterprise_application` | 归属 org；`uq_ea_org_key` org 内唯一 | **实例级实体**；`application_key` 实例内全局唯一；`organization_id` 保留为可空业务归属字段 |
| `enterprise_external_identity` | `(org, app, external_user_id) → user_id` 三元组 | **`(app, external_user_id) → user_id`** 二元组；org 列保留为业务冗余，不参与判定 |
| EPGZ-05 §4.4 | mapping 缺失 → `identity_not_mapped`（整个事务回滚） | **mapping 可选化**：无映射时 `external_user_id` 回退为 `user_id` 字符串形态（响应本就含 `user_id`，OA 侧直接以 `user_id` 关联员工）；`identity_not_mapped` **收窄**为仅「账号非 active Human」触发（账号级 kill-switch，撤销等待在途交换的行锁语义保留）；mapping 降级为**可选别名机制**（OA 员工号），由 OA 团队按需登记 |
| EPGZ-05 §3 / 工作台发现 | 应用需人工 SQL 登记，prod 发布后缺数据即 `app_not_configured` | **部署自动登记默认 OA 应用**（M0）：标准部署开箱即用，`getAuthCode` 在标准部署下不再报 `app_not_configured` |

## 4. 目标设计（OA 免登链）

### 4.1 签发（HUMAN-SSO-01 `POST /api/v1/oa/sso/code`）

```text
JWT（active Human，account_type=0）
  -> 请求字段语法（application_key 全局解析 / redirect_uri / nonce）
  -> application active（实例内全局唯一 key）
  -> redirect_uri 对 allowed_redirect_uris 逐字节 exact match（空 allowlist 仍 fail-closed）
  -> 签发 60s one-time code，绑定四元组 app/user/redirect_uri/nonce
```

请求中的 `organization_id`：兼容期**容忍并忽略**（旧客户端/OA 侧可能在传）。

### 4.2 工作台发现（`GET /api/v1/workbench/entries`）

返回实例内全部 `status='active'` 且 redirect allowlist 非空的 OA 应用。
`organization_id` 查询参数兼容期容忍并忽略。
**不做** identity mapping 过滤（mapping 已可选化，见 4.4）；标准部署下因 M0
自动登记，此列表永不为空。实例内确实无可用应用（如管理员停用全部应用）时，
客户端按既有入口可见性规则**静默隐藏** OA 入口，不向用户抛错误。

### 4.3 兑换（INT-14 `POST /api/internal/v1/oa/sso/exchange`）

```text
A2 认证链：credential active/expiry -> application active -> scope=sso:exchange -> rate
  -> code_digest 等值查找 -> TTL -> consumed_at IS NULL
  -> 绑定校验（app/user/redirect_uri/nonce 四元组；常时比较）
  -> CAS 单次消费 -> 同事务解析 (application, user) identity mapping
     （可选；缺失回退 user_id 字符串；账号须仍为 active Human——
     非 active → identity_not_mapped，账号级 kill-switch）
```

A2 链中原 org 环节（`row.organization_id == credential.org`）移除。

### 4.4 开箱即用（zero-config）目标

OA 免登是**平台内置能力**：标准部署发布完成后即可用，`getAuthCode` 不得因
「未登记」报 `app_not_configured`。达成条件：

1. **部署自动登记默认 OA 应用（M0）**：部署流程自动创建实例级默认 OA 应用并
   签发 Application Credential（credential 产物交运维转交 OA 团队）。这是
   「登记」从运维动作变为部署默认行为的工程化——Application 实体与 credential
   不能取消（exchange 必须认证调用方，否则一次性票据退化为口令），但用户对
   它无感。
2. **redirect_uri 经部署配置注入**：OA 回调地址是 IMBoy 必须知道的外部事实
   （把人送去哪），从「手工多表登记」降级为**一个部署配置项**（deploy .env /
   sys.config）。未配置时 OA 入口静默隐藏（fail-closed 隐藏，不报错）。
3. **identity mapping 可选化**：无映射时 `external_user_id` 回退为 `user_id`
   字符串（§4.3 响应本就含 `user_id`）。OA 侧直接以 `user_id` 关联自己的员工；
   需要员工号语义时再经 identity-mappings API 登记别名。新用户首次免登不再被
   `identity_not_mapped` 阻断。

错误分支保留语义：`app_not_configured` / `ambiguous_application` 仅在**故障态**
（部署异常、全部应用被停用、多应用未指定 key）出现，正常运营不触达。

## 5. 迁移计划（expand / backfill / verify / switch / cleanup，对齐 C18）

| # | 步骤 | 内容 | 状态 |
|---|---|---|---|
| M0 | 部署默认 OA 应用 | 部署流程（deploy / 初始化脚本）自动创建实例级默认 OA 应用 + 签发 credential（产物输出给运维转交 OA 团队）；redirect_uri 经部署配置项注入 allowlist，未配置则跳过登记（OA 入口静默隐藏）。幂等：重复部署不重复创建 | ✅ `imboy/scripts/seed_default_oa_application.escript`（本地库三遍验证：新建/幂等/allowlist 追加） |
| M1 | DB 迁移（00000171） | `enterprise_application.application_key` 加实例内全局唯一（先校验存量冲突，冲突则 FAIL 并人工处置）；`enterprise_external_identity` 加 `(application_id, external_user_id)` 唯一（同样先校验）。两表 `organization_id` 列**保留**为业务归属，不删、不收缩 | ✅ `00000171_organization_positioning_deorg.{up,down}.sql`（eunit 合成库 up/down 验证） |
| M2 | 后端 | `enterprise_application_repo:workbench_entries_tx/1` 重写（去 org/member JOIN 与 mapping 过滤）；`enterprise_oa_sso_logic` 签发/兑换判定链去 org；exchange 的 identity mapping 解析**可选化**（无映射回退 `external_user_id = user_id` 字符串；账号非 active Human 保留 `identity_not_mapped` 账号级 kill-switch）；A2 中间件按路由豁免 org 状态门（INT-14 `org_status_gate => false`）；`organization_id` 参数容忍忽略；负例矩阵更新（NEG-05 改跨应用语义、NEG-H03/H04 翻转为正例 POS-H03/H04） | ✅ `enterprise_oa_sso_tests` 53/53、`enterprise_internal_wiring_http_tests` 9/9（含 expiry http checks 撤销语义）、`enterprise_internal_pg_tests` 40/40、`enterprise_oa_context_binding_tests` 通过 |
| M3 | 客户端（imboyapp） | `OaWorkbenchApi.entries` 去 organizationId 依赖；`oaWorkbenchEntriesProvider` 缓存键简化为 accountKey；`OaSsoApi.issueCode` 不再发送 organization_id；`ApiOaSsoIssueLauncher` 去 org 参数；flutter analyze 零告警 | ✅ enterprise_oa + shell + bridge 测试 190/190 |
| M4 | 文档 | EPGZ-05 合同改版（v2，org 语义移除 + mapping 可选化）；`docs/guides/oa-sso-integration-guide.md` 同步；本文勾选实施状态 | ✅ ADR/合同警示已落（本文 + core-contract + EPGZ-05 banner）；指南已改版 v2.0（开箱即用/凭证交付/多域名登记/错误码收敛）；test_oa.html 与 imboyapp 错误文案同步新语义 |
| M5 | prod 运维 | 发布 M0 后补配置 `redirect_uri` 即完成 prod 开通（见 §7）；credential 产物转交 OA 团队 | ⏳ 待 prod 发布执行 |
| M6 | cleanup | 带 `organization_id` 的旧请求观测归零后，字段与冗余列正式移除（另行迁移） | ⏳ 观测期 |

**兼容性**：OA 对接方请求形态兼容（多传字段被忽略）；exchange 响应不变；
现有登记数据在 M1 后依然有效（org 列降级为业务归属，不破坏数据）。

## 6. 保留项（明确不改，避免过度反应）

以下场景的 org 维度是**寻址/归属/内部业务治理**，不属于本修订范围，保留现状：

- `organization_member` 与组织治理（邀请/恢复/归档）；
- 工作台按组织分组的展示形态（若产品保留多组织 UI）；
- Workspace / Department / 客服 Session / Agent Grant 中的 org 范围（内部授权治理，
  边界在实例内由产品自行定义，与对外协议无关）；
- internal API 中 org 作为**目标资源寻址参数**（如指定向某组织的群组投递）。

同时修订一处此前的判断（2026-10-12 上一版 ADR 曾认为「identity mapping 是不因去
租户化消失的本质复杂度」）：exchange 响应本就返回 `user_id`，mapping 可选化（§4.4）
后它不再是无配置免登的阻断项。仍不可去除的只有两项——**Application Credential**
（exchange 的调用方认证，防一次性票据退化为口令）与 **redirect_uri 这个外部事实**
（IMBoy 必须知道把人送去哪）——前者由 M0 自动化，后者降级为一个部署配置项。

## 7. 与当前 prod `app_not_configured` 的关系

目标形态下，标准部署的 `getAuthCode` 不再报 `app_not_configured`：M0 自动登记
默认应用（列表非空）+ mapping 可选化（无映射不阻断）+ org 门槛移除（M2）。
prod 开通动作收敛为：部署时配置 OA 回调地址一项 + credential 转交 OA 团队；
不再需要手工多表 SQL 登记。迁移完成前，现状形态登记的数据（含 org 字段）在
M1-M3 落地后依然有效，可作过渡。

## 8. 应用归属与资源授权分离（2026-10-14 owner 决定）

用户选择方案一：应用可不归属组织，同一应用可由管理员分别授予多个组织或
Workspace 的权限。应用的业务归属不限制可授权目标，也不自动产生权限。

- `application.organization_id` 仅为可空业务归属；应用与凭据以实例内 AppId 绑定。
- Application Grant 的组织字段表示明确的资源目标，不能继续要求与应用归属相同。
  同一有效 Grant 必须同时覆盖操作 scope 和目标资源；跨 Grant 拼接权限仍拒绝。
- 无组织归属不表示实例全域权限。没有目标或没有对应有效 Grant 时拒绝资源操作，
  不自动选择首个组织，也不把空组织解释为通配符。
- 管理端必须分别表达应用归属和资源授权，保留旧组织路径的归属校验与兼容行为。
- 凭据撤销、Grant 撤销/到期、应用停用、幂等、审计失败回滚与 Workspace 归属约束保留。

调用链分别保留 `application_id`、可空 `application_organization_id` 和资源请求的
`target_organization_id`，不得覆盖同一字段混用。资源目标通过
`X-Imboy-Organization-Id` 显式指定；旧客户端未传时，可用当前应用归属作为默认寻址，
但仍须通过同样的目标 Grant 校验。无归属且未传目标时拒绝，不猜测目标；目标与
路径或正文资源归属不一致时拒绝。self/SSO 不使用资源默认目标。

幂等键以应用为范围，并将解析后的目标绑定到请求摘要、快照和审计。同一 key 在
应用归属改变后落到另一目标时必须冲突，不能把网络重试变成另一个组织的新写入。
重放之前仍须验证当前权限。新格式与存量幂等记录的兼容必须单独验证后启用。

实施状态：**进行中**。00000173 仅将 Grant→Application 外键改为 AppId 引用，保留
Grant→目标组织及显式 Workspace 的完整性。00000174 补充可空应用归属、凭据/SSO
的独立 AppId 外键、仅允许 self scope 的无目标 Grant 与审计约束；两项迁移已有
隔离 PostgreSQL 定向回归。可空应用运行链、实例级管理入口、跨目标调用和完整
回归尚未完成，不能把 expand 迁移视作方案一整体交付。

### 8.1 一期交互简化（2026-10-14 owner 追加要求）

> 一期产品范围已由下方 §8.2 的进一步澄清收敛。本节保留此前设计和本地实施事实，
> 不再作为一期必须交付应用授权管理的依据。

本节 Application 指 OA、商城等外部系统的接入配置和凭据，不是新增用户账号或应用市场。
用户要求一期交互简单，授权可暂不单独配置或放宽。实施采用组织级粗粒度配置：
创建应用时仅选择「可访问组织」（可多选），后台为所选组织自动建立整组织 Grant；
scope 使用服务端统一预设，不展示 scope、Workspace、Grant 表单。不选择组织时
仍可配置 OA 免登及应用自身能力，其 application-self 授权也由后台自动建立。

「应用归属」不作为必填接入步骤。组织选择只表达资源访问范围，不自动修改应用归属。
一期不要求管理员逐条签发 Grant；移除某个组织访问范围时同步撤销对应自动授权，
停用应用或撤销凭据继续立即阻止调用。后端保留实际授权记录、能力上限、有效期、
审计与幂等检查，不增加零 Grant 自动放行或未登录调用的旁路。

创建应用、建立默认授权与审计须同事务成功；任何一步失败整体回滚。默认能力与
实际有效期由服务端统一管理并在详情中说明，不引入用户必须理解的授权术语。
已有人工配置的细粒度 Grant 不得因隐藏表单而被自动扩权或覆盖。

此节是一期交互范围修订，优先于要求用户逐项配置 scope/Workspace 的旧交互要求；
细粒度授权管理暂缓，不能据此将原细粒度交互验收项标为通过。新建应用的后端
原子创建逻辑已实现并通过隔离数据库回归：自动建立 self 和所选组织 Grant，
回显默认有效期（365 天）；目标授权或审计失败时全部回滚。无组织应用的凭据
签发/轮换/撤销、应用自检及 self Grant 降级/撤销/到期已通过真实 HTTP 和数据库
定向回归；应用归属与凭据可空标记不同的情况也可正确治理。实例级 Admin HTTP
已接通创建、列表、详情、状态、凭据与审计，沿用管理员会话及读写权限；旧组织入口
仍校验应用归属。00000175 为新默认授权持久化来源，旧记录保留人工来源，详情读取
实际范围、有效期及状态，不按幂等键猜测来源。管理页面已改为实例列表、可选组织
多选及凭据获取，隐藏细粒度权限表单；组织范围编辑只维护自动授权，保留人工配置
和历史，使用版本校验并记录变更前后内容。页面尚待真实浏览器联调；无组织
OA/SSO 和跨目标资源运行链仍待实施。当前仅为本地候选，未部署。

### 8.2 一期收敛为组织通讯录与用户聊天（2026-10-14 owner 最终澄清）

用户明确选择：**只同步人员和组织架构，用户自己在 IMBoy 聊天**。

- 外部 OA、商城等系统管理自身业务权限；IMBoy 不复制审批、订单或商城角色权限。
- OA 系统主动通过 `/api/internal/v1/*` 同步组织、部门、人员及成员关系到 IMBoy，
  IMBoy 展示组织通讯录并提供聊天能力；一期不由 IMBoy 轮询外部 OA。
- IMBoy 仍校验用户身份、群成员关系与消息访问；组织架构展示不隐含读取全组织消息。
- 一期不包含外部系统自动建群、代发消息或通知，也不要求管理员操作 Application、
  scope、Workspace Grant 或「可访问组织」授权表单。
- 数据同步连接仍须鉴别来源、限定同步能力并记录操作；这属于接入配置，不是
  外部业务权限管理。不能把当前全部 Internal API 的默认权限授予通讯录同步连接。

本节优先于 §8.1 的一期入口设计。§8 的归属与资源授权分离可保留为后续集成能力的
技术设计，但不能据此继续扩大一期产品。已经实现的应用授权管理代码仅为未部署的
本地候选，不作为一期验收入口；已有人工授权和历史数据不得删除或自动扩权。

后续实施须复用既有用户、组织及成员模型，核实外部标识映射、重复同步、人员或组织
变更以及离职后的通讯权限行为。当前尚未证明通讯录同步闭环；原计划验收项须按本节
重新对照，不能将延期的集成能力标成测试通过。

### 8.3 OA 主动同步的实施边界与验收

一期接入方向为 `OA → Internal API → IMBoy 组织通讯录 → 用户聊天`。现有
`POST /api/internal/v1/directory/users` 和 `identity-mappings/directory` 是查询接口，
不能算作同步写入能力。新增写入复用现有组织、部门和成员模型；接入凭据只具备
目录同步所需能力，不能因简化界面取得建群、代发消息或文件访问能力。

同步遵守以下约束：

- 外部人员标识按接入来源与 `external_user_id` 定位，同一来源的人员跨组织调动
  不应新建第二个 IMBoy 账号；组织、部门也须有稳定外部标识，不能按名称匹配。
- 首次关联已有账号须证明用户身份，不能仅凭 OA 上传的姓名、手机号或邮箱自动
  合并；后续同步及管理绑定入口不能覆盖已确认的账号关联，解除后也不得自动
  改绑其他账号。
- 同一请求重试不得重复建档；相同幂等键携带不同内容须明确冲突。同步写入与
  外部标识关联、审计同事务提交，失败不得留下半份成员或部门关系。
- 一期采用显式增量变更；一次请求未包含某个人或部门，不代表删除。旧版本变更
  不得覆盖较新状态；部门不能跨组织引用父节点或形成循环。
- 离职只处理对应组织关系，不删除实例用户、其他组织关系或个人聊天。组织主
  Owner 必须先交接，不能用同步请求绕过现有守卫。暂停/移除后，组织资源访问
  与现有会话的权限变化须实测，不以成员表状态变化代替撤权验收。
- OA 负责来源目录字段；IMBoy 用户个人资料与本地治理角色不能被目录同步顺带
  覆盖。新增组织的初始 Owner 必须是经确认的 Human 用户，不能暗建占位管理员。

账号首次进入 IMBoy 的两种方式均纳入一期（owner 已确认）：

1. 用户先注册或登录 IMBoy，再经 OA 身份验证，确认将该 OA 身份关联到当前账号。
2. 用户从 OA 单点登录进入 IMBoy；先解析同一来源的已确认身份关联，存在则登录
   原账号，未关联则允许用户登录已有 IMBoy 账号完成关联，或明确选择新建账号。

两种入口共用同一关联事实，目录同步本身不签发用户会话。先同步目录、先关联账号、
先从 OA 登录三种顺序都不能为同一外部身份产生多个 IMBoy 账号。关联需要同时证明
IMBoy 账号与 OA 身份，不能凭客户端提交 `external_user_id` 就绑定。外部身份已经
关联其他 IMBoy 账号时须返回冲突，不能由同步或普通登录自动换绑或合并两份聊天数据。

现有 `enterprise_oa_sso_logic` 是 IMBoy→OA 方向，不能作为反向登录已完成的证据。
现有 OIDC 登录链可作为接入基础，但 OA 实际协议、issuer/subject 与同步人员标识的
对应关系须在接入配置中明确；不能假定所有 OA 已支持 OIDC。两种入口均须执行正常
账号状态及设备会话校验；现有 OIDC 无设备标识的签发路径不能直接作为完成实现。

验收至少覆盖：首次同步、同请求重试、并发重复提交、改名/调部门、跨组织任职、
显式离职、恢复、乱序更新、非法部门树、来源串用、凭据撤销及事务失败回滚。
最终须证明 OA 写入后客户端实际可见相应目录，已关联用户可正常聊天，离职后的
组织权限按既有规则失效且其他组织不受影响。当前这些是待实施要求，尚未验收。
双入口另须覆盖三种首次使用顺序、并发首次关联、重复登录、关联冲突、OA 身份
伪造/过期/重放、停用账号拒绝登录及设备会话吊销，不能只验证跳转成功。
