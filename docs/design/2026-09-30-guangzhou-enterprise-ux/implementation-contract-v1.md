# 客服、企业协作与 OA / Internal API 实施合同 V1

日期：2026-10-01。状态：`PLAN_READY / IMPLEMENTATION_NOT_COMPLETE / RELEASE_NOT_EVALUATED`。
用户最新目标：基于现有三端实现可投产客服、企业组织架构及加入退出 / 工作区 / 群 / 频道 / 文档归属、OA 协议及全部 Internal API。本期 E2EE 功能关闭；允许删除确实不合理的代码。

UI 唯一依据：[广州企业 UX V2](./ux-decision-v2.md)。可视预览：[V2 页面](./ux-decision-v2.html)。本合同由本轮当前源码分析形成，不把历史执行计划的 PASS 或授权迁移到新 run。

## 范围与原则

- 复用现有客服、组织、附件、目录、身份映射、Grant、SSO、Webhook 与聊天实现。删除优先针对重复入口、失效路由和无调用代码；保存历史数据与仍有调用的兼容读取。
- Human JWT、Application Credential、Seat JWT、Admin Cookie 分开。布局统一不扩大角色权限，不让应用签发自己的 Grant 或生命周期管理父组织。
- 「全部 Internal API」以当前注册表 **INT-01..INT-32、26 path** 为完整基线，并对 `endpoints.md` 的待补合同逐项给出实现 / 有依据的范围决定。不能把路由存在当可投产，也不能把未来 CRUD 草案塞进 Postman 当已实现。
- Enterprise 文档一期提供文件 / 资料归属与授权读取，不新造在线协同编辑器。OA H5 承载办公业务；以 IMBoy → OA SSO 为当前真实协议。
- E2EE 入口关闭与消息协议关闭分别核验；关闭后的新消息遵循明确配置，存量密文仍可正确读取。不得靠解密失败时静默发送明文来实现关闭功能。
- Out of scope: push、发布、部署、生产迁移、真实客户数据、真实对外 Webhook / 通知、联系方式设置、客户包名与签名材料；本地可投产候选先做成可审阅结果，外部步骤另按目标取得授权。

## 基线、所有权和共享文件

当前独立 Git 根：

| 仓库 | 分析基线 HEAD | 当前情况 |
|---|---|---|
| imboy | `037621f5124ededfe5465069342faec84e2685a4` | 存在客服、TSID、部署及文档 WIP |
| imboyapp | `54576b776f4691c88f4c42726c1b9aa20d9ede26` | 存在已暂存通话、工作区选择器及依赖 WIP |
| imboyadmin | `5598b7ef45b06670f185bf17100fb1155d8e0376` | 分析起点无工作区修改 |

执行开始重新采样 HEAD、工作区差异与占用情况。基线变化先解释来源，不强行回到历史 SHA。源文件实现使用每仓隔离工作区，从明确选定的当前提交开始；用户 WIP 不 reset、stash、覆盖或自动吸收。有意依赖用户 WIP 时先形成来源和最小依赖说明。

`src/imboy_router.erl`、Internal routes / boundary / manifest / OpenAPI、数据库迁移编号、App router / token / i18n / feature manifest、Admin App / sidebar / shared API client 属集成所有者串行写入；业务卡提供明确接线需求，不能互抢。迁移编号重新核对当前目录，不复用旧计划示例编号。

授权实施的改动验证后按独立功能本地提交，命令级 author/committer 使用 `leeyi <leeyisoft@qq.com>`。不修改全局配置，不 stage 无关改动。文档 / 原型本身不代表业务实施提交。

## 当前已确认与尚未验证

| 项目 | 已确认 | 尚未验证 / 缺口 |
|---|---|---|
| 客服 | Seat 页面、队列 / 附件 / 上下文组件、独立静态入口及 `/seat/:public_seat_console_id` 存在 | 当前候选真实 DB、事件流断连恢复、授权撤销、媒体与外部存储、真实设备旅程 |
| 组织 | 邀请加入、默认工作区、目录、角色、成员停用 / 移除、部门、归档 / 恢复 / 移交实现存在 | 自助退出端到端覆盖、退出依赖处理与企业资产保留；不能等同成员移除已完整 |
| 企业 scope | Organization 1:N 工作区；App 工作区选择能按企业过滤 | 个人显示偏好、目标企业落位失败恢复、账号全局会话与工作区资源关联 |
| 文档资料 | Human 附件、Application 企业附件、托管资产链路存在 | 三种来源是否一致投影归属、普通员工工作区资料读面、存量无归属资料与离岗处理 |
| OA | Human 签发 + Application 交换、60s code、身份映射 / exact redirect 存在 | 标准包按当前企业发现入口；切企业同 origin 会话；对方 OA 实现与联调条件 |
| Internal | 32 路由 / 26 path；12 项 manifest 一致性检查 PASS；Postman 32 请求计数匹配 | 全部端点真实 positive / negative / 幂等 / 审计 / Grant 行为与分页；待补 CRUD 语义 |
| UX | V2 明确三 / 四项导航与角色分层，静态页面预览 | App / Admin 尚未按 V2 改版，未作真机验收 |

## step-1 — 冻结缺口与运行证据

Owned: 运行证据、基线 / 验收矩阵和本合同；不写业务源文件。
用当前代码建立每个目标旅程和每个 INT 端点的可执行验证清单，区分已实现、可复用、需要补齐、外部条件。记录三个 HEAD、本计划 SHA、工作区差异及实际测试环境；复用现有测试设施。

Acceptance: G0-01 清单覆盖 32 端点、四认证域和广州 UX 全部旅程；G0-02 每个缺口有文件 / 调用链与最小修复位置；G0-03 既有失败有命令、exit、日志、原因，不能写成无基线 PASS。

## step-2 — 客服完整候选

Owned: `imboy/src/features/customer_service/`、其专属 tests；`imboyadmin/src/modules/customer_service/` 与 `src/seat/`、专属 tests。不碰 foreign dirty `cs_widget_handler.erl`；所需修改在隔离候选中形成可比较补丁。
复用独立 Seat 面与现有接待流程，修复实际链路中影响接单、转接、结束、附件、流重连、退出及授权撤销的问题；移动端若需要接线列入 step-5，不在此改共享 App shell。

Acceptance: CS-01 真实隔离 DB 完成访客入队 → 原子接单 → 文本 / 附件 → 转接 → 结束；CS-02 双 Seat 并发接单只有一个成功、断流恢复及撤销后无写权限；CS-03 Widget / Seat / Human / Admin 凭据不可交叉使用，产物不泄漏 Admin 接口或 secret。

## step-3 — 组织加入、退出与治理

Owned: `imboy/src/lib/organization/`、相应 `organization_*` handler / logic / repo 与专属 tests；Admin `src/modules/organization/`。
复用邀请、默认工作区、部门与成员治理。补齐自助退出：本人可提出退出，Owner 必须完成合法移交；存在企业资源或坐席交接依赖时返回具体影响，走现有离岗 / 交接能力。退出收回访问，企业资料和群历史按归属保留；通知验证用本地接收端。

Acceptance: ORG-01 邀请预览 / 过期 / 重复加入 / 并发加入产生一致有效 membership；ORG-02 成员退出、Owner 移交、坐席及资源依赖正确，退出后目录 / 群 / 文件 / OA 访问失效；ORG-03 部门树无环、版本冲突可恢复、普通成员和工作区 Owner 的权限边界通过真实 DB 验证。

## step-4 — 企业资料归属与授权

Owned: 当前 `attach_*` / `enterprise_asset_*` / `enterprise_business/application/asset` 的必要文件、对应 repo / tests；新迁移只由集成者分配。
追踪 Human 附件、企业应用附件、托管资产从 presign 到确认、消息绑定、读取 / 下载和治理的全调用链。保留原有模型；仅对确实缺少的归属 / 授权投影做最小追加。新资料的企业 / 工作区 / 来源绑定不能在确认后被任意改写；上传人离岗不改变企业所有权。

Acceptance: FILE-01 本企业获授权成员可读，跨企业 / 未加入工作区 / 已退出用户的列表与签名链接均拒绝；FILE-02 上传 / confirm / 消息绑定 / 重放 / 删除或归档一致且有审计；FILE-03 存量无法可靠判断归属的记录不自动猜测回填，迁移在空库与合成旧数据上通过并提供恢复方案。

## step-5 — 简洁企业 UX 与功能关闭

Owned: App `modules/organization/presentation`、`page/bottom_navigation`、`page/workspace_shell`、企业群 / 频道 / 资料呈现、必要的账号展示偏好及专属 tests；Admin 企业聚焦导航。router、i18n、sidebar 变更由集成者接线。
按 UX V2 收敛个人三项、企业四 / 三项，统一切换面板、手机部门树 / 宽屏关系图、同事发消息、群 / 公告与资料入口；管理收进「我」，按真实资格隐藏动作。关闭 E2EE 展示并验证关闭配置下的新消息路径，继续兼容历史密文。

Acceptance: UX-01 一 / 多 / 无工作区、切企业失败 / 默认读取失败均无半切换和跨域残留；UX-02 群范围由服务端资源归属决定，C2C 不伪装企业消息，未读 / 草稿 / 附件不串目标；UX-03 普通成员 / Org 管理员 / WS Owner、直达链接、大字号 / 暗色 / 大小屏通过，广州项目与社交发现关闭不丢个人能力。

## step-6 — OA 正确上下文与协议交付

Owned: `enterprise_oa_sso_*` 必要文件 / tests、App `modules/enterprise_oa/` 与最小工作台入口发现、`api/internal/v1` OA 文档。
修正 all-org 取第一条的语义，明确当前企业绑定并在服务端重验。复用已有 SSO，不新造反向登录接口；换配置 / 换企业 / 登出按企业身份清会话。对方资料缺失时使用本地合成 OA 验证接收、state、交换及会话，真实对接保持待验证。

Acceptance: OA-01 Android / iOS 标准包有配置可进、无配置隐藏，当前企业 A/B 与同 origin 身份不串；OA-02 code 60s / 单次并发消费 / 绑定不符 / 映射撤销 / Cookie 退出正确，无 JWT / secret 进入 H5；OA-03 文档、样例、签发 / 交换方向和真实返回字段一致，真实 OA 条件缺失明确 `BLOCKED_EXTERNAL`。

## step-7 — Internal 身份、群和只读域

Owned: `enterprise_identity_*`、`enterprise_group_*`、Internal read handler / page、必要 repo、相关专属测试 / OpenAPI path。
覆盖 INT-01..06、15..21、24..31；逐项验证身份映射、合法发送人基础、群与成员治理和 Grant 约束只读目录。冻结错误码 / 游标，不创建旁路授权。endpoints.md 待补 Workspace / 项目 / 频道写合同需明确授予的范围、不可变归属、幂等与审计；需求未证明的草案先记范围决定，不用完整 CRUD 数量充当最优解。

Acceptance: INT-A01 列举端点全部有真实行为证据及缺 scope / 跨 Org / 失效 Grant 拒绝；INT-A02 分页签名绑定、人类 / 应用域隔离及非法 / 旧游标正确；INT-A03 有必要的新增写能力采用追加合同，routes / boundary / scope CHECK / manifest / OpenAPI / Postman 同步，应用不得自授权限。

## step-8 — Internal 文件、消息、Webhook 与交换

Owned: `enterprise_asset_*`、`enterprise_message_*`、`enterprise_webhook_*`、`enterprise_oa_sso_exchange_*` 的必要文件 / repo / tests / path；与 step-4/6 同文件串行执行。
覆盖 INT-07..14、22..23、32，补齐真实对象存储验证、发送人 / 来源、幂等审计、Webhook 签名 / 重试 / 死信 / 重放和 SSO 交换。测试回调只指向受控本地接收端，不对真实商户发送事件。

Acceptance: INT-B01 端点均有成功、负例、幂等 / 一次消费、事务回滚和必需审计证据；INT-B02 两发送模式的来源、身份有效状态与附件范围正确；INT-B03 回调签名、重试、SSRF 防护、INT-23 游标和 INT-32 入箱 / 投递状态对应，无凭证 / PII 泄漏。

## step-9 — 候选集成、文档与裁剪

Owned: 集成者串行拥有共享接线、必要迁移、文档；代码删除必须有引用 / 路由 / feature / 角色 / 历史读取验证依据。
先完成各卡候选和领域检查，再冻结一个三端候选运行完整旅程。把 docs、OpenAPI、Postman、功能配置与实际实现统一；旧计划保持历史事实，不批量替换成 PASS。按独立功能本地提交。

Acceptance: INTG-01 冻结三个候选 HEAD 与 diff、各领域 evidence，foreign WIP 不在提交中；INTG-02 全套要求的本地检查通过且未知 / 跳过不算 PASS；INTG-03 仅删除已证明不需要的代码，支持旧链接 / 历史数据并给出回退提交。

## step-10 — 设备与生产准备审查

Owned: 只读验收报告及合成证据，不直接改源码或外部系统。
Android / iOS 真机完成广州员工、老板、退出 / 失权、资料与 OA 旅程；桌面核实组织树、平台治理和 Seat。生产准备核验迁移、存储、回调、流重连、监控与回退。缺设备、OA、存储或生产授权时单独标注，不用截图或 HTTP 200 替代。

Acceptance: QA-01 每个验收 ID 绑定冻结 HEAD、命令 / 设备、真实 oracle 和结果；QA-02 `LOCAL_CANDIDATE` / `DEVICE` / `EXTERNAL` / `PRODUCTION` 独立判定；QA-03 可投产结论只在必需条件全部满足时成立，外向动作未执行保持对应待授权状态。

## 依赖、验证与恢复

```text
1 → 2、3（仅不同独占文件可并行）
3 → 4 → 5
3 → 6
3、4 → 7 → 8（6/8 的 SSO 文件串行）
2、5、6、7、8 → 9 → 10
```

优先验证受影响路径，再领域组合，再冻结三端候选的一次集成验收。不要每个业务卡都跑所有全局检查。当前可复用命令入口：

| 层级 | 入口 | 注意 |
|---|---|---|
| 静态合同 | `python3 scripts/check_enterprise_release_manifest.py` | 当前 12/12 通过，只证明一致性 |
| 后端受影响套件 | `make eunit-local t=<实际测试模块>` | 先核对当前 Makefile 与环境；有 DB oracle 的用隔离 DB |
| 后端候选 | `make compile`、适用领域 / 集成套件 | 不覆盖常驻服务的共享编译产物 |
| App | `flutter analyze`、受影响 `flutter test <实际路径>` | 真机旅程独立，禁模拟器当功能验收 |
| Admin / Seat | `bun run test <实际文件>`、`bun run build`、`bun run build:widget`、现有产物验证脚本 | 单测沿用仓内隔离入口，Seat / Widget 产物逐个验证 |

每条 evidence 至少保存 `acceptance_id, repo, head, command, exit, result, oracle, log`。有硬失败→停止依赖卡→修复最小根因→只重跑失效证据→下一状态。超时 / 条件缺失写 `BLOCKED_ENVIRONMENT` / `BLOCKED_DEVICE` / `BLOCKED_EXTERNAL`；当前行为错误写 `FAIL_*`，不能吞错改 PASS。

被中断后先核对提交、文件、迁移与证据，不盲重放外部写；仅本地合成验证可重试。不确定外部结果必须先查询状态。最终报告不得把本地候选等同已部署；发布与生产迁移须拿到具体可审阅结果后再处理授权。

## Plan-Orchestrate 输出

按命名技能生成顺序链；不在本技能内执行 `/orchestrate`。安装模式 plugin；整体 Erlang / Flutter / TypeScript，`lang=unknown`，按步骤使用通用与对应前端 reviewer。命令与所有 agent 名均使用 `ecc:`。

可复制链、步骤说明和文档 SHA 绑定见 [orchestrate-prompts-v1.md](./orchestrate-prompts-v1.md)。
