# 企业组织三端增量完善计划 V1 / Three-platform organization closure plan

日期：2026-10-06。状态：PLAN_READY / NOT_EXECUTED。本轮只做只读调查及计划，不修改业务源码、不启动验收环境。范围：Backend、Admin、Flutter Android 真机及 macOS 原生 App。

## 1. 当前基线与判断 / Baseline and findings

三仓 HEAD：Backend `79cbd2e7597fe9eea6bff80eb0b025724ffe7509`；Admin `f3e1ca5b03e21a83d61ee017ec7389ffb10e5b25`；App `ab306ddf19a13250ced5deff8f75536cac3d4c2a`。Backend 存在 group_ds/elib_pg_sql/ct.cover.spec 及商业化/客服文档 WIP；App 有七个 provider 生成文件 WIP。禁止吸收这些改动；执行启动重新采样及哈希，不能用本记录替代现场状态。

已有组织生命周期、邀请、Owner 转移、部门树、成员停用/恢复/离场、默认 Workspace、企业群频道、资料归属、离岗交接、Internal 应用授权及 Admin 页面。当前计划是闭环与缺陷修复，不是重新开发这些域。

调查依据：
- Backend `src/lib/organization/application/organization_lifecycle.erl`：归档/恢复、角色与 pending/rejected 门已有；归档不自动恢复子资源独立状态。
- App `lib/modules/organization/infrastructure/organization_api.dart`：成员生命周期、部门治理、邀请、Owner 转移及 Directory 已有 API 接线。
- `docs/design/2026-10-04-enterprise-workspace-acceptance/evidence/ews-20261004T065500Z/final/report.md`：旧候选 Admin 真实浏览器32项通过、Internal合同通过，App设备阻断，全量EUnit失败。是历史输入，不是当前结论。
- 同run `post-fix-regression-record.md` 及当前 `imboy_app.erl`/`enterprise_oa_sso_code_repo.erl`：trust_audit 注册与 SSO 双时钟缺陷已修复；列回归项，不列未修功能。
- 同run `app-case-manifest.md` 与非OA `20261003-corrective-review/FINAL.md`：原生跨账号、迟到响应、故障恢复、角色直达与触控板证据曾未闭合。
- 当前 App 已有 `enterprise_scope_native_test.dart` 等原生用例，不能因旧报告说缺spec而断言没有测试；需核覆盖与实际执行证据。自动化 specs 仍只发现 organization-home-device-preparation.json，准备用例不等于业务旅程完成。
- 当前 Makefile 已有 TEST_HTTP_PORT=0 和 run级relay配置改善，历史端口失败需先验证是否仍存在，不能直接照搬旧根因。

缺口分类：GAP_IMPLEMENTATION（复现确认缺实现）、GAP_ACCEPTANCE（有实现缺当前证据）、CONTRACT_CONFLICT（历史要求与后续决定冲突）、BLOCKED_EXTERNAL。此调查未运行三端完整门，不给当前PASS。

## 2. 执行范围与授权 / Scope

复用10月4日 EWS-00…12及继承required IDs，逐条建立“继承/替代/已执行待重验/不适用”的对应表。以下EO卡只索引增量重点，不缩减原有已承诺企业功能。

默认可做只读盘点、独立worktree本地源码修复、测试与本地提交。联系方式设置/对外使用、外向通知、真实客户OA/Webhook、生产/共享数据库写入及不可逆动作，必须按根AGENTS人工确认。先形成具体环境与动作审阅表，再申请所缺授权；已授权的精确范围不重复询问。隔离fixture消息不能外发，不能默认把现有4323库当本run数据库。无push/发布/部署。

不新增SCIM/SAML、考勤、审批流、在线文档、企业计费等扩展产品域。先把组织治理→成员进入→协作→撤权离岗的现有闭环做完。个人Human E2EE与企业托管消息边界保持原合同，/api/open/v1保持暂停。

## 3. 任务卡 / Executable cards

每卡负责人唯一，默认顺序执行；多代理须另有授权及路径/资源租约。共享router、i18n、tokens、Makefile、测试配置只允许单owner修改。

| ID/优先级 | 责任路径与依赖 | 工作与测试 | 验收oracle |
|---|---|---|---|
| EO-00/P0 | 协调者；三仓只读；无依赖 | 记录SHA/WIP/worktree、扫描路由/页面/API/worker；继承EWS及旧required集合；逐项搜索当前证据；出隔离环境/fixture/设备/端口审阅表 | required集合全部映射；每项有平台/角色/状态/证据或NEXT_UNLOCK；无重复计划覆盖、无外来WIP吸收 |
| EO-01/P0 | Backend测试owner；Makefile/test fixture；EO-00 | 核当前端口0/run级relay实现；独立DB与完整app boot；解决套件依赖TSID初始化的测试setup，禁止放宽生产fail-closed；先单域后完整门 | 相同冻结候选全EUnit连续两次RC=0；无cancel/required skip；资源污染测试能拒绝误用共享库；失败有原始日志与归因 |
| EO-02/P1 | Backend组织域、Admin组织页、App organization；EO-00/01 | 串起创建/审核→邀请→加入→默认Workspace→部门→角色→Owner转移→归档恢复；多次重放、过期撤销、最后Owner、并发交接及失败回滚 | DB恰一个有效Owner；pending/rejected不能restore绕审核；邀请一次性；默认WS创建失败无半成品；归档拒写且不自动复活已停用成员/Grant |
| EO-03/P0 | Backend resolver/guard及群频道；Admin scope页；App scope/cache；EO-02 | Org A含WS A1/A2，Org B含B1；单独篡改org/ws/member/gid/channel/asset/message；跨账号、跨企业、撤权时让旧请求晚到 | 无越权数据、写入/审计副作用为0；A1成员不能读A2；撤权旧UI清退，缓存和晚到响应不能恢复旧数据；后端拒绝直达请求 |
| EO-04/P1 | App organization图及原生tests；EO-02/03 | 完整分页加载、搜索、部门成员、展开折叠、拖动/缩放/适应、弱网重试、部分分页失败、暗色/大字号；Android真机与macOS真实窗口 | 图节点/连线/人数与授权API数据集一致；部分失败不丢已加载数据；旧finally不污染新状态；实际触控板和触控证据；44逻辑像素与无溢出 |
| EO-05/P1 | Admin organization/enterprise_business；App成员面；Backend offboarding；EO-02/03 | 停用→立即撤权→交接联系人/会话/群频道/项目/资产→离场→重试；失败中断恢复、目标无权限、重复请求 | 交接对象的归属/权限DB前后准确；失败不丢资产、不误移成员；移除终态不能restore，新邀请才重入；旧token/WS及下载入口拒绝未授权操作 |
| EO-06/P1 | Backend企业消息/归档/保留/资产；Admin对应页面；App消费；EO-03/05 | 真实消息→ACK→重连/重启→历史→附件→hold→到期purge；复用EWS-04…09，先证缺口再修 | ACK不删归档；有hold不可purge；跨scope附件签名/下载拒绝；origin_application_id准确；项目done不等于archive/delete；删除边界按策略及授权 |
| EO-07/P1 | Backend Internal/OA及Admin applications；EO-03 | 凭据轮换/撤销、零Grant、deny优先、写幂等、游标、审计；重验trust_audit与SSO单次消费/过期/双时钟修复 | runtime/OpenAPI/Postman method/path/schema/header一致；相同key不同payload冲突，重放无重复副作用；撤权生效；SSO并发仅一消费成功；客户联调另记BLOCKED_EXTERNAL |
| EO-08/P1 | Admin端到端与App native owner；EO-02…07 | Admin操作后App消费；企业四栏、切企业/个人、成员管理与成员直达拒绝、错误恢复；两平台各两轮；继承旧16旅程并查新增入口 | 每个required case有真实UI操作+API/DB oracle、平台/build/SHA；mock/准备spec/裸HTTP不计原生通过；个人企业入口按后续“我→切换企业”决定映射，不能复活过时入口 |
| EO-09/P0 | 只读终审；全部卡 | 冻结三仓candidate、依赖/config/fixture/runner；复算exact-set与artifact hash；核WIP及最终门 | 全部required格PASS、无P0/P1、无未解释skip/cancel、终审通过才LOCAL_CANDIDATE_PASS；任何缺项为PARTIAL或BLOCKED，不写生产可交付 |

EO-08继承冲突裁决：10月2日明确个人通讯录不放企业加入入口、路径为“我→切换企业”，因此旧ContactPage入口不能自动当缺陷；在继承表保留后续决定出处。macOS不展示移动OA应按平台支持合同说明，不虚构OA PASS。iOS暂不计本轮强制平台，但如原required合同强制iOS则保留待解锁，不擅删除。

## 4. 命令与测试入口 / Commands

以下从对应独立候选仓执行。先审runner/config确认隔离库、fixture与端口已绑定；未通过EO-00环境门不要直接跑DB/设备命令。

Backend：`make compile`；`python3 api/flatten_internal.py --check`；`make eunit-local t=organization_owner_invariant_tests`、`t=organization_lifecycle_tests`、`t=organization_invitation_app_tests`、`t=organization_department_app_gate_tests`、`t=workspace_guard_tests`（分别运行，先确认该套件setup完整boot/TSID条件）；最终 `make eunit-local` 连续两次。沿现有Makefile参数显式绑定本run PG/config/端口，先读recipe，不能只设HTTP变量而漏掉数据库。TSID VM-global专项独立VM运行，不能从显式专项任务排除。

Admin：`bun run test src/modules/organization`；`bun run lint`；`bun run typecheck`；`bun run build`；最后`bun run test`全量。真实浏览器复用 `tests/e2e/admin-organization-governance.spec.ts`、`enterprise-app-governance-real.spec.ts`、`enterprise-ent-int01/`、`enterprise-business.spec.ts`；建立本run隔离config后先`bunx playwright test --config=<run-config> --list`核清单，再执行。浏览器操作遵守根AGENTS先尝试autoglm-browser-agent，不可用记录后使用既有Playwright runner。

App：`flutter analyze`；`flutter test test/unit_test/page/workspace_shell`；组织域测试先`rg --files test | rg 'organization|human_directory'`冻结实际路径再运行；复用`integration_test/enterprise/enterprise_scope_native_test.dart`、`organization_chart_nonoa_macos_test.dart`、`enterprise_nav_perm_nonoa_macos_test.dart`及相关workspace用例。`flutter devices`采集真实设备；按用例现有参数绑定批准的隔离fixture后`flutter test <native-case> -d <实际设备ID> --dart-define=API_BASE_URL_OVERRIDE=<run-url> ...`，省略号必须在EO-00生成的commands.sh替换为完整参数，不得当可运行命令交付。禁止模拟器替代真机。

命令模板不是执行记录；每个最终case必须在commands.sh记录可重放完整命令（秘密用外部环境变量，不写入证据），实际exit与执行数量必须保存。纯静态/UI mock仅证明对应层。

## 5. 证据、检查点与恢复 / Acceptance and recovery

RUN_ROOT=`docs/design/2026-10-06-enterprise-organization-gap/evidence/<run_id>/`。保存baseline.json、protected-wip.sha256、requirements.json、leases.json、commands.sh、acceptance.tsv、recovery.jsonl、candidate-manifest.json、artifacts.sha256、final/verdict.json及report.md。禁止生产PII落仓。

每格最少：acceptance_id、source_requirement、card、case/platform/role/org/ws、三仓SHA、plan_sha256、command、exit、executed/skipped、UI/API/DB oracle、artifact path/hash、attempt、status、reason、NEXT_UNLOCK。一个父卡通过不能替代子格。NOT_APPLICABLE要有合同来源，不能靠关闭功能变绿。

检查点：C0输出实际缺口清单/授权与资源表；C1环境门及组织治理；C2跨scope与离岗闭环；C3原生两轮及三端最终门；C4冻结终审。每次向用户报告完成ID/新增缺陷/阻断/下一动作。

失败处理：保存原失败→只读核查→根因卡→最小修复→独立代码审查→受影响回归→独立功能本地提交→重新冻结。基础设施同指纹最多2次重试；无法归因停该路径并继续独立卡。不杀foreign进程、不DROP foreign库、不自动重放未知结果写操作。不改erlang.mk、ios/*、macos/*、plugin/r_upgrade。生成文件按工具生成，不手改。

状态分开：BACKEND/ADMIN/ANDROID_NATIVE/MACOS_NATIVE/INTERNAL/EXTERNAL/USER_VISUAL/PRODUCTION。当前旧日志仅REFERENCE。整体本地PASS须所有强制本地项闭合；外部/视觉未完成仍明确保留。生产状态固定NOT_EXECUTED。本地提交身份命令级使用leeyi <leeyisoft@qq.com>，不得混入无关WIP。
