# Plan-Orchestrate Result

**Plan**: `/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md`
**Plan SHA256**: `7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599`
**Lang**: `unknown`（多语言，无语言超过 60%）
**ECC mode**: `plugin`
**Steps**: 10
**Scope**: all

English summary: Scoped sequential prompts bound to the V2 contract; generation does not execute chains.

## Steps overview

| # | Title | Tags | Chain |
|---|---|---|---|
| 1 | 冻结缺口与运行证据 | design,plan | `ecc:planner,ecc:architect` |
| 2 | 客服完整候选 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:typescript-reviewer,ecc:security-reviewer` |
| 3 | 组织加入、退出与治理 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 4 | 企业资料归属与授权 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 5 | 简洁企业 UX 与功能关闭 | impl,security | `ecc:tdd-guide,ecc:flutter-reviewer,ecc:typescript-reviewer,ecc:security-reviewer` |
| 6 | OA 正确上下文与协议交付 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:flutter-reviewer,ecc:security-reviewer` |
| 7 | Internal 身份、群和只读域 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 8 | Internal 文件、消息、Webhook 与交换 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 9 | 候选集成、文档与裁剪 | refactor,security | `ecc:architect,ecc:refactor-cleaner,ecc:code-reviewer,ecc:security-reviewer` |
| 10 | 设备与生产准备审查 | test | `ecc:tdd-guide,ecc:e2e-runner` |

---

## Step 1 — 冻结缺口与运行证据

**Intent**: 用当前代码建立每个目标旅程和每个 INT 端点的可执行验证清单，区分已实现、可复用、需要补齐、外部条件。记录三个 HEAD、本计划 SHA、工作区差异及实际测试环境；复用现有测试设施。

**Tags**: design,plan

**Chain rationale**: 规划与架构审查冻结范围。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-1] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；冻结三端HEAD、工作区占用与完整验收矩阵，逐项定位调用链；不改业务代码。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: G0-01全32端点与四认证域；G0-02缺口绑定调用链；G0-03失败绑定命令日志。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 2 — 客服完整候选

**Intent**: 复用独立 Seat 面与现有接待流程，修复实际链路中影响接单、转接、结束、附件、流重连、退出及授权撤销的问题；移动端若需要接线列入 step-5，不在此改共享 App shell。

**Tags**: impl,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:typescript-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-2] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；复用独立Seat实现接单、转接、结束、附件与断流恢复，补齐应用侧坐席管理接口。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: CS-01真实DB完整接待；CS-02并发唯一接单与撤权；CS-03凭据隔离及追加合同一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 3 — 组织加入、退出与治理

**Intent**: 复用邀请、默认工作区、部门与成员治理。复用已经合入的自助退出并闭环验证：本人可退出，Owner 必须完成合法移交；存在企业资源或坐席交接依赖时返回具体影响，走现有离岗 / 交接能力。退出收回访问，企业资料和群历史按归属保留；通知验证用本地接收端。

**Tags**: impl,db,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-3] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；闭环邀请、部门树、Owner移交与成员退出，撤销所有企业访问并保留企业资产。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: ORG-01并发邀请一致；ORG-02离岗撤权保留资产；ORG-03部门无环与权限正确。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 4 — 企业资料归属与授权

**Intent**: 追踪 Human 附件、企业应用附件、托管资产从 presign 到确认、消息绑定、读取 / 下载和治理的全调用链。保留原有模型；仅对确实缺少的归属 / 授权投影做最小追加。新资料的企业 / 工作区 / 来源绑定不能在确认后被任意改写；上传人离岗不改变企业所有权。

**Tags**: impl,db,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-4] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；统一Human/Application/托管资料归属，修复签名上传确认绑定与授权下载。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: FILE-01跨域及退出拒绝；FILE-02事务幂等审计；FILE-03旧数据迁移可恢复。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 5 — 简洁企业 UX 与功能关闭

**Intent**: 按最新定稿收敛个人四项「消息、通讯录、频道、我」、企业四 / 三项，统一切换面板、手机部门树 / 宽屏关系图、同事发消息、群 / 公告与资料入口；管理收进「我」，按真实资格隐藏动作。关闭 E2EE 展示并验证关闭配置下的新消息路径，继续兼容历史密文。

**Tags**: impl,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:flutter-reviewer,ecc:typescript-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-5] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；实现个人四栏消息/通讯录/频道/我、企业三或四栏、统一切换、组织树与治理；关闭本期E2EE入口。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: UX-01切换失败无串域；UX-02服务端归属及缓存隔离；UX-03角色/暗色/大字号通过。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 6 — OA 正确上下文与协议交付

**Intent**: 复用已经合入的当前企业绑定与入口发现，验证真实签发/交换链并补齐 Cookie 会话隔离。复用已有 SSO，不新造反向登录接口；换配置 / 换企业 / 登出按企业身份清会话。对方资料缺失时使用本地合成 OA 验证接收、state、交换及会话，真实对接保持待验证。

**Tags**: impl,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:flutter-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-6] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；复用当前企业入口发现与SSO，补齐同origin会话隔离及IMBoy到OA协议文档。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: OA-01企业身份不串；OA-02单次交换与Cookie退出正确；OA-03文档字段和方向一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 7 — Internal 身份、群和只读域

**Intent**: 覆盖 INT-01..06、15..21、24..31；逐项验证身份映射、合法发送人基础、群与成员治理和 Grant 约束只读目录。冻结错误码 / 游标，不创建旁路授权。endpoints.md 待补 Workspace / 项目 / 频道写合同需明确授予的范围、不可变归属、幂等与审计；工作区与企业频道写面必须补齐；项目新增写面按上文明确延期，不用 CRUD 数量充当最优解。

**Tags**: impl,db,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-7] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；验证INT-01..06、15..21、24..31，追加工作区与企业频道写接口，禁止应用自授权。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: INT-A01逐端点正负例；INT-A02签名分页隔离；INT-A03新增写面Grant幂等审计一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 8 — Internal 文件、消息、Webhook 与交换

**Intent**: 覆盖 INT-07..14、22..23、32，补齐真实对象存储验证、发送人 / 来源、幂等审计、Webhook 签名 / 重试 / 死信 / 重放和 SSO 交换。测试回调只指向受控本地接收端，不对真实商户发送事件。

**Tags**: impl,db,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-8] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；验证INT-07..14、22..23、32，闭环文件、消息、Webhook和SSO交换。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: INT-B01逐端点事务和幂等；INT-B02身份附件范围正确；INT-B03签名重试SSRF及状态正确。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 9 — 候选集成、文档与裁剪

**Intent**: 先完成各卡候选和领域检查，再冻结一个三端候选运行完整旅程。把 docs、OpenAPI、Postman、功能配置与实际实现统一；旧计划保持历史事实，不批量替换成 PASS。按独立功能本地提交。

**Tags**: refactor,security

**Chain rationale**: 复用既有设施；通用审查覆盖 Erlang，语言审查覆盖前端，数据库审查核对事务，安全审查收口信任边界。

```bash
/ecc:orchestrate custom "ecc:architect,ecc:refactor-cleaner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-9] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；冻结三端候选并集成，统一接口文档与配置，按调用和历史兼容证据裁剪旧代码。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: INTG-01冻结SHA保护WIP；INTG-02必需检查通过；INTG-03裁剪兼容且可回退。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

---

## Step 10 — 设备与生产准备审查

**Intent**: 交付目标中的 Android / iOS 真机完成广州员工、老板、退出 / 失权、资料与 OA 旅程；桌面核实组织树、平台治理和 Seat。生产准备核验迁移、存储、回调、流重连、监控与回退。缺设备、OA、存储或生产授权时单独标注，不用截图或 HTTP 200 替代。

**Tags**: test

**Chain rationale**: 真实旅程测试负责验收收口。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-10] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；只读审查真实设备与生产准备证据，分别判定本地/设备/外部/生产状态。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: QA-01每ID有SHA和真实oracle；QA-02四类结果分离；QA-03缺证据不可判可投产。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```

## Batch execution

依合同顺序执行，失败停止依赖步骤。命令仅供复制，不表示已启动或已通过。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-1] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；冻结三端HEAD、工作区占用与完整验收矩阵，逐项定位调用链；不改业务代码。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: G0-01全32端点与四认证域；G0-02缺口绑定调用链；G0-03失败绑定命令日志。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:typescript-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-2] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；复用独立Seat实现接单、转接、结束、附件与断流恢复，补齐应用侧坐席管理接口。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: CS-01真实DB完整接待；CS-02并发唯一接单与撤权；CS-03凭据隔离及追加合同一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-3] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；闭环邀请、部门树、Owner移交与成员退出，撤销所有企业访问并保留企业资产。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: ORG-01并发邀请一致；ORG-02离岗撤权保留资产；ORG-03部门无环与权限正确。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-4] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；统一Human/Application/托管资料归属，修复签名上传确认绑定与授权下载。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: FILE-01跨域及退出拒绝；FILE-02事务幂等审计；FILE-03旧数据迁移可恢复。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:flutter-reviewer,ecc:typescript-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-5] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；实现个人四栏消息/通讯录/频道/我、企业三或四栏、统一切换、组织树与治理；关闭本期E2EE入口。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: UX-01切换失败无串域；UX-02服务端归属及缓存隔离；UX-03角色/暗色/大字号通过。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:flutter-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-6] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；复用当前企业入口发现与SSO，补齐同origin会话隔离及IMBoy到OA协议文档。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: OA-01企业身份不串；OA-02单次交换与Cookie退出正确；OA-03文档字段和方向一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-7] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；验证INT-01..06、15..21、24..31，追加工作区与企业频道写接口，禁止应用自授权。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: INT-A01逐端点正负例；INT-A02签名分页隔离；INT-A03新增写面Grant幂等审计一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-8] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；验证INT-07..14、22..23、32，闭环文件、消息、Webhook和SSO交换。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: INT-B01逐端点事务和幂等；INT-B02身份附件范围正确；INT-B03签名重试SSRF及状态正确。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:architect,ecc:refactor-cleaner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-9] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；冻结三端候选并集成，统一接口文档与配置，按调用和历史兼容证据裁剪旧代码。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: INTG-01冻结SHA保护WIP；INTG-02必需检查通过；INTG-03裁剪兼容且可回退。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v2.md#step-10] SHA256=7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599；只读审查真实设备与生产准备证据，分别判定本地/设备/外部/生产状态。依合同Owned及依赖执行，保护用户未提交内容与历史密文；真实证据绑定冻结源码。 Acceptance: QA-01每ID有SHA和真实oracle；QA-02四类结果分离；QA-03缺证据不可判可投产。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。"
```
