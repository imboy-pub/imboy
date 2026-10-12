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
