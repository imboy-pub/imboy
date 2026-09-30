# Plan-Orchestrate Result

**Plan**: `/Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md`
**Expected plan SHA256**: `87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252`
**Lang**: unknown（Erlang / Flutter / TypeScript 混合）
**ECC mode**: plugin
**Steps**: 10
**Scope**: all

每个链开始先完整读冻结合同并验证 SHA；SHA 不等停止为 `BLOCKED_PLAN_DRIFT`。步骤遵循合同 DAG；本技能只生成命令，不启动代理或执行 `/orchestrate`。

| # | Title | Tags | Chain |
|---|---|---|---|
| 1 | 冻结缺口与运行证据 | design | `ecc:planner,ecc:architect` |
| 2 | 客服完整候选 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:typescript-reviewer,ecc:security-reviewer` |
| 3 | 组织加入退出与治理 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 4 | 企业资料归属与授权 | impl,db,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 5 | 简洁企业 UX 与功能关闭 | impl,security | `ecc:tdd-guide,ecc:flutter-reviewer,ecc:security-reviewer` |
| 6 | OA 上下文与协议交付 | impl,security | `ecc:tdd-guide,ecc:code-reviewer,ecc:flutter-reviewer,ecc:security-reviewer` |
| 7 | Internal 身份群与只读域 | impl,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 8 | Internal 文件消息Webhook交换 | impl,security | `ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer` |
| 9 | 候选集成文档与裁剪 | refactor,security | `ecc:architect,ecc:refactor-cleaner,ecc:code-reviewer,ecc:security-reviewer` |
| 10 | 设备与生产准备审查 | test | `ecc:tdd-guide,ecc:e2e-runner` |

## Step 1 — 冻结缺口与运行证据

**Intent**: 按三个当前 Git 根重新采样 HEAD、WIP 与本地环境，覆盖所有 32 个 Internal 端点、客服旅程、组织加入退出、企业资料和 UX V2，复用现有测试与数据模型。不得把历史 PASS 搬到新 run；先给出实际文件、调用链、缺口和最小实施位置。
**Tags**: design
**Chain rationale**: 设计步骤由 planner 与 architect 收敛；设备验收由测试与 e2e 验证，不自动外向发布。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-1] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；按三个当前 Git 根重新采样 HEAD、WIP 与本地环境，覆盖所有 32 个 Internal 端点、客服旅程、组织加入退出、企业资料和 UX V2，复用现有测试与数据模型。不得把历史 PASS 搬到新 run；先给出实际文件、调用链、缺口和最小实施位置。 Acceptance: G0-01 覆盖 32 端点和四认证域；G0-02 每个缺口有源码依据；G0-03 测试基线绑定 HEAD、exit 和日志。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 2 — 客服完整候选

**Intent**: 只在隔离候选修改客服独占模块与 Seat 页面，复用独立入口、队列、接单、转接、结束、附件、事件流和退出授权。保留 foreign WIP；接线共享文件由集成者串行。先真实隔离 DB 测试，再最小修复，不能把 HTTP 200 或静态页面算链路通过。
**Tags**: impl,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:typescript-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-2] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；只在隔离候选修改客服独占模块与 Seat 页面，复用独立入口、队列、接单、转接、结束、附件、事件流和退出授权。保留 foreign WIP；接线共享文件由集成者串行。先真实隔离 DB 测试，再最小修复，不能把 HTTP 200 或静态页面算链路通过。 Acceptance: CS-01 完整接待旅程通过；CS-02 并发接单单赢家、断流与撤权正确；CS-03 四认证域互拒且产物无 Admin 凭据。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 3 — 组织加入退出与治理

**Intent**: 复用组织邀请、默认工作区、部门、成员与离岗治理，在组织独占模块补自助退出和 Owner 合法移交，Admin 页只用平台权限。退出后收回目录、群、文件与 OA 权限；企业资产不随上传人离岗自动删除，交接依赖在 UI 具体呈现。
**Tags**: impl,db,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-3] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；复用组织邀请、默认工作区、部门、成员与离岗治理，在组织独占模块补自助退出和 Owner 合法移交，Admin 页只用平台权限。退出后收回目录、群、文件与 OA 权限；企业资产不随上传人离岗自动删除，交接依赖在 UI 具体呈现。 Acceptance: ORG-01 重复并发加入一致；ORG-02 退出、移交与依赖正确；ORG-03 真实 DB 验证无环、CAS 冲突和角色边界。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 4 — 企业资料归属与授权

**Intent**: 追踪 Human 附件、Application 企业附件及托管资产所有调用方，复用现有 presign、确认、绑定、下载与治理，只补确缺归属。组织和工作区绑定不可被任意改写，上传人仅审计身份；生产历史资料不猜测回填，迁移编号由集成者分配。
**Tags**: impl,db,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-4] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；追踪 Human 附件、Application 企业附件及托管资产所有调用方，复用现有 presign、确认、绑定、下载与治理，只补确缺归属。组织和工作区绑定不可被任意改写，上传人仅审计身份；生产历史资料不猜测回填，迁移编号由集成者分配。 Acceptance: FILE-01 跨 Org、失权和退出读写拒绝；FILE-02 上传绑定、重放与治理一致；FILE-03 合成旧数据迁移及恢复方案通过。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 5 — 简洁企业 UX 与功能关闭

**Intent**: 在 App 独占 UI 范围实施 UX V2：个人三项、企业四或三项、唯一切换面板、群与公告、工作区资料、部门树和宽屏关系图，管理收进我。复用现有 token、页面和账号偏好；E2EE 入口关闭，验证新消息配置和历史密文兼容，不以静默明文回退掩盖失败。
**Tags**: impl,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:flutter-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-5] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；在 App 独占 UI 范围实施 UX V2：个人三项、企业四或三项、唯一切换面板、群与公告、工作区资料、部门树和宽屏关系图，管理收进我。复用现有 token、页面和账号偏好；E2EE 入口关闭，验证新消息配置和历史密文兼容，不以静默明文回退掩盖失败。 Acceptance: UX-01 切换与默认读取失败不串企业；UX-02 会话、未读、草稿、附件目标正确；UX-03 角色、直达链接与大小屏通过。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 6 — OA 上下文与协议交付

**Intent**: 复用 Human 签发和 Application 交换，按当前企业发现 OA 条目并服务端重验，修正所有企业条目取第一项。补配置变化、切企业同 origin、保活和退出会话；当前协议是 IMBoy 到 OA，真实对方条件缺失时用本地合成 OA，不伪造生产联调。
**Tags**: impl,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:flutter-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-6] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；复用 Human 签发和 Application 交换，按当前企业发现 OA 条目并服务端重验，修正所有企业条目取第一项。补配置变化、切企业同 origin、保活和退出会话；当前协议是 IMBoy 到 OA，真实对方条件缺失时用本地合成 OA，不伪造生产联调。 Acceptance: OA-01 Android/iOS 有配置可达且企业不串；OA-02 60s 单次消费、绑定、退出正确；OA-03 文档与字段一致，外部缺口明确。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 7 — Internal 身份群与只读域

**Intent**: 覆盖 INT-01..06、15..21、24..31，以当前注册表逐端点写真实行为验证，复用身份、群、Grant 和签名分页；不让应用自授工作区或父组织权限。对待补写合同逐项做有依据的实现或范围决定，有必要的追加同步路由、scope CHECK、审计、OpenAPI 和 Postman。
**Tags**: impl,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-7] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；覆盖 INT-01..06、15..21、24..31，以当前注册表逐端点写真实行为验证，复用身份、群、Grant 和签名分页；不让应用自授工作区或父组织权限。对待补写合同逐项做有依据的实现或范围决定，有必要的追加同步路由、scope CHECK、审计、OpenAPI 和 Postman。 Acceptance: INT-A01 各端点成功及权限负例有证据；INT-A02 游标域与过滤绑定正确；INT-A03 新增合同全链一致且不自授权。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 8 — Internal 文件消息Webhook交换

**Intent**: 覆盖 INT-07..14、22..23、32，复用对象存储、消息来源、SSO 和 Webhook 管线，验证幂等、事务回滚、审计、签名重试、死信重放、SSRF 和 INT-23 游标。与资料及 OA 卡同文件串行，回调只发受控本地接收端，不访问真实商户或生产服务。
**Tags**: impl,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-8] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；覆盖 INT-07..14、22..23、32，复用对象存储、消息来源、SSO 和 Webhook 管线，验证幂等、事务回滚、审计、签名重试、死信重放、SSRF 和 INT-23 游标。与资料及 OA 卡同文件串行，回调只发受控本地接收端，不访问真实商户或生产服务。 Acceptance: INT-B01 成功负例、幂等与审计证据完整；INT-B02 发送人及附件范围正确；INT-B03 投递签名、重试与状态一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 9 — 候选集成文档与裁剪

**Intent**: 集成者在一个冻结三端候选串行接线共享文件、分配迁移编号，统一实际源码、OpenAPI、Postman、文档与 feature 配置。只删除已查清调用、路由、角色和历史读取不再需要的代码；用户 WIP 不进提交。各领域检查后仅跑一次必需的冻结候选集成验收。
**Tags**: refactor,security
**Chain rationale**: 按混合语言采用通用 reviewer，涉及前端步骤补对应 reviewer；安全链末尾执行安全审查。

```bash
/ecc:orchestrate custom "ecc:architect,ecc:refactor-cleaner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-9] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；集成者在一个冻结三端候选串行接线共享文件、分配迁移编号，统一实际源码、OpenAPI、Postman、文档与 feature 配置。只删除已查清调用、路由、角色和历史读取不再需要的代码；用户 WIP 不进提交。各领域检查后仅跑一次必需的冻结候选集成验收。 Acceptance: INTG-01 三 HEAD 和 diff、证据完整；INTG-02 必需本地检查全过；INTG-03 删除有依据且有回退，独立功能本地提交。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Step 10 — 设备与生产准备审查

**Intent**: 只读验证冻结候选，按广州要求跑 Android/iOS 真机员工、老板、加入退出、失权、资料与 OA 旅程，桌面核实组织治理与独立 Seat。核验存储、回调、流恢复、监控、迁移与回退；缺设备或真实 OA 条件单独标记，不能用截图、mock 或 HTTP 200 升级可投产结论。
**Tags**: test
**Chain rationale**: 设计步骤由 planner 与 architect 收敛；设备验收由测试与 e2e 验证，不自动外向发布。

```bash
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-10] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；只读验证冻结候选，按广州要求跑 Android/iOS 真机员工、老板、加入退出、失权、资料与 OA 旅程，桌面核实组织治理与独立 Seat。核验存储、回调、流恢复、监控、迁移与回退；缺设备或真实 OA 条件单独标记，不能用截图、mock 或 HTTP 200 升级可投产结论。 Acceptance: QA-01 每个 ID 绑定 HEAD、设备或命令和真实 oracle；QA-02 本地、设备、外部、生产分开；QA-03 外向未授权保持未执行。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```

## Batch execution

按合同 DAG 和共享文件串行规则运行；前序失败不得执行依赖链。逐条命令的 SHA 不匹配即停止。

```bash
/ecc:orchestrate custom "ecc:planner,ecc:architect" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-1] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；按三个当前 Git 根重新采样 HEAD、WIP 与本地环境，覆盖所有 32 个 Internal 端点、客服旅程、组织加入退出、企业资料和 UX V2，复用现有测试与数据模型。不得把历史 PASS 搬到新 run；先给出实际文件、调用链、缺口和最小实施位置。 Acceptance: G0-01 覆盖 32 端点和四认证域；G0-02 每个缺口有源码依据；G0-03 测试基线绑定 HEAD、exit 和日志。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:typescript-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-2] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；只在隔离候选修改客服独占模块与 Seat 页面，复用独立入口、队列、接单、转接、结束、附件、事件流和退出授权。保留 foreign WIP；接线共享文件由集成者串行。先真实隔离 DB 测试，再最小修复，不能把 HTTP 200 或静态页面算链路通过。 Acceptance: CS-01 完整接待旅程通过；CS-02 并发接单单赢家、断流与撤权正确；CS-03 四认证域互拒且产物无 Admin 凭据。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-3] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；复用组织邀请、默认工作区、部门、成员与离岗治理，在组织独占模块补自助退出和 Owner 合法移交，Admin 页只用平台权限。退出后收回目录、群、文件与 OA 权限；企业资产不随上传人离岗自动删除，交接依赖在 UI 具体呈现。 Acceptance: ORG-01 重复并发加入一致；ORG-02 退出、移交与依赖正确；ORG-03 真实 DB 验证无环、CAS 冲突和角色边界。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-4] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；追踪 Human 附件、Application 企业附件及托管资产所有调用方，复用现有 presign、确认、绑定、下载与治理，只补确缺归属。组织和工作区绑定不可被任意改写，上传人仅审计身份；生产历史资料不猜测回填，迁移编号由集成者分配。 Acceptance: FILE-01 跨 Org、失权和退出读写拒绝；FILE-02 上传绑定、重放与治理一致；FILE-03 合成旧数据迁移及恢复方案通过。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:flutter-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-5] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；在 App 独占 UI 范围实施 UX V2：个人三项、企业四或三项、唯一切换面板、群与公告、工作区资料、部门树和宽屏关系图，管理收进我。复用现有 token、页面和账号偏好；E2EE 入口关闭，验证新消息配置和历史密文兼容，不以静默明文回退掩盖失败。 Acceptance: UX-01 切换与默认读取失败不串企业；UX-02 会话、未读、草稿、附件目标正确；UX-03 角色、直达链接与大小屏通过。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:code-reviewer,ecc:flutter-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-6] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；复用 Human 签发和 Application 交换，按当前企业发现 OA 条目并服务端重验，修正所有企业条目取第一项。补配置变化、切企业同 origin、保活和退出会话；当前协议是 IMBoy 到 OA，真实对方条件缺失时用本地合成 OA，不伪造生产联调。 Acceptance: OA-01 Android/iOS 有配置可达且企业不串；OA-02 60s 单次消费、绑定、退出正确；OA-03 文档与字段一致，外部缺口明确。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-7] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；覆盖 INT-01..06、15..21、24..31，以当前注册表逐端点写真实行为验证，复用身份、群、Grant 和签名分页；不让应用自授工作区或父组织权限。对待补写合同逐项做有依据的实现或范围决定，有必要的追加同步路由、scope CHECK、审计、OpenAPI 和 Postman。 Acceptance: INT-A01 各端点成功及权限负例有证据；INT-A02 游标域与过滤绑定正确；INT-A03 新增合同全链一致且不自授权。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:database-reviewer,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-8] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；覆盖 INT-07..14、22..23、32，复用对象存储、消息来源、SSO 和 Webhook 管线，验证幂等、事务回滚、审计、签名重试、死信重放、SSRF 和 INT-23 游标。与资料及 OA 卡同文件串行，回调只发受控本地接收端，不访问真实商户或生产服务。 Acceptance: INT-B01 成功负例、幂等与审计证据完整；INT-B02 发送人及附件范围正确；INT-B03 投递签名、重试与状态一致。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:architect,ecc:refactor-cleaner,ecc:code-reviewer,ecc:security-reviewer" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-9] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；集成者在一个冻结三端候选串行接线共享文件、分配迁移编号，统一实际源码、OpenAPI、Postman、文档与 feature 配置。只删除已查清调用、路由、角色和历史读取不再需要的代码；用户 WIP 不进提交。各领域检查后仅跑一次必需的冻结候选集成验收。 Acceptance: INTG-01 三 HEAD 和 diff、证据完整；INTG-02 必需本地检查全过；INTG-03 删除有依据且有回退，独立功能本地提交。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
/ecc:orchestrate custom "ecc:tdd-guide,ecc:e2e-runner" "[Plan: /Users/leeyi/project/imboy.pub/imboy/docs/design/2026-09-30-guangzhou-enterprise-ux/implementation-contract-v1.md#step-10] 先验证计划 SHA=87630e683df4acd04ea5920009d542a9f723365572ffab1aa07734def1733252；只读验证冻结候选，按广州要求跑 Android/iOS 真机员工、老板、加入退出、失权、资料与 OA 旅程，桌面核实组织治理与独立 Seat。核验存储、回调、流恢复、监控、迁移与回退；缺设备或真实 OA 条件单独标记，不能用截图、mock 或 HTTP 200 升级可投产结论。 Acceptance: QA-01 每个 ID 绑定 HEAD、设备或命令和真实 oracle；QA-02 本地、设备、外部、生产分开；QA-03 外向未授权保持未执行。 Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料。"
```
