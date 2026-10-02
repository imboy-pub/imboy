# 企业功能非 OA 增量收尾与验收计划 V1

日期：2026-10-02。状态：`READY_FOR_EXECUTION`。执行者：GLM-5.3。

## 1. 目标与完成边界

对现有实现做增量收尾，完成最新组织架构图及企业导航的真实 macOS App 验收，将历史非 OA 验收证据重新绑定到最终候选。发现真实缺陷才修复，不重新实现已通过的客服、组织治理、资料归属或 Internal API。

本计划完成表示 `NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS`，不表示上线完成，也不表示用户已认可视觉效果。只做计划，本文件创建不表示执行已经开始。

排除：全部 OA 对接、OA 协议修订、SSO/nonce/浏览器会话安全修订、OA Cookie 联调、客户 OA、官网替代入口的网络联调；生产部署/迁移、push、发布、对外 Webhook/消息/通知、真实客户数据；iOS 与 Android 新验收；重新启用 E2EE；在线协同文档编辑器。企业工作台四栏存在性仍属于导航验收，但不打开 OA。

## 2. 当前事实与输入

执行前采样基线：

| 仓库 | 本计划调查 HEAD | 后续变化 |
|---|---|---|
| imboy | `4d9ffba21aa731a6e4a04e6d82635ff6164b0590` | 创建本计划后的文档提交允许，不构成业务漂移 |
| imboyapp | `b6bb4dc578acd2bae07b000c97d954a476c2170d` | 已含图形组织架构及紧凑工具栏 |
| imboyadmin | `2b9015b4ab62ef39c5df234fdb724b2309b8bd5d` | 相对旧验收候选仅 ADR 文档变化 |

既有证据目录：`docs/design/2026-09-30-guangzhou-enterprise-ux/evidence/phase-local-final-2026-10-02/`。其 `verify.py` 本次通过；绑定 Backend `083901a6`、App `d868815b`、Admin `9a808bec`，含30父项、42 Internal 子项，不能直接改成当前 SHA 的 PASS。

原始范围见同目录上级 `implementation-contract-v2.md` 与 `goal-completion-audit-2026-10-02.md`。旧证据包含真实隔离 PG/Garage/客服浏览器及 macOS 旅程；App 跳过/真实 API 排除不得计 PASS。

本次调查：后端 OA 业务逻辑未改变；非 OA 后端增量主要是测试环境修正及 `imboy_app:ensure_rsa_keys/0` 导出。Admin 业务源码未改变。App 增量含通讯录、组织图、企业「我」、选择器及 P2P 预览变化，不能仅看 HEAD 祖先关系就复用全量结论。

## 3. 产品验收口径

- 个人固定「消息、通讯录、频道、我」；无顶部“个人”下拉，个人通讯录不出现“加入企业、创建组织”。企业切换沿用「我 → 切换企业」。
- 企业固定「消息、通讯录、工作台、我」，没有 OA 也保留工作台。
- 企业通讯录内「成员｜组织架构」切换；组织架构是节点和连线组成的图，可拖动、缩放、适应视图、展开/折叠，点击部门进入该部门成员。
- 企业名称、部门名称和成员数量来自已授权数据，不用虚构人员或汇总人数；管理员动作按真实角色展示，直达路径仍由服务端授权。
- 图形缓存随账号/企业及权限变化失效；晚到请求不能恢复旧企业图或已撤权数据。
- 根节点突出，部门卡片紧凑；工具栏与页面内容对齐，暗色及大字号下文字/操作可见、无溢出；交互目标至少44逻辑像素。

## 4. 执行规则、所有权和证据

协调执行者串行完成 N0→N1→N2→N3→N4→N5，不要求建立多个代理。若使用代理，必须独占路径，不得覆盖他人改动；独立终审只读。

- App 实施所有权：`lib/modules/organization/presentation/`、`lib/page/workspace_shell/`、`lib/page/mine/mine/`、`lib/page/contact/contact/`、`lib/page/workspace/` 与受影响专属测试。P2P 预览只做增量回归，除非发现确定缺陷。
- Backend/Admin 默认只读。发现新增业务缺陷，先形成含调用链、复现、最小受影响路径的缺陷卡，再局部修复；范围升级不能悄悄扩展本计划。
- router、公共 tokens、i18n 接线串行；颜色/字号/间距复用 AppColors/AppSpacing/FontSizeType。禁止改 `erlang.mk`、`ios/*`、`macos/*`、`plugin/r_upgrade`。
- 执行前重新确认各仓 Git 根、HEAD、未提交内容。禁止 reset/clean/stash/重写历史/整体暂存。验证后的独立功能或缺陷本地提交，身份 `leeyi <leeyisoft@qq.com>`，不 push。

每次执行创建独立 `RUN_ROOT=imboy/docs/design/2026-10-02-enterprise-non-oa-closure/evidence/<UTC时间>/`。复用已有证据与运行设施，不搭建新的通用测试平台。新增记录至少：

1. `baseline.json`：三仓 HEAD、状态、变化清单、工具版本、环境类型。
2. `acceptance.tsv`：`id,status,repo,source_sha,plan_sha256,command,exit_code,oracle,evidence_path,evidence_sha256,reason,next_action`。
3. `state.json`：当前卡、已完成卡、重试次数、阻断原因、三仓候选。
4. `recovery.tsv`：失败→原因→恢复动作→重试结果→下一状态。
5. `FINAL.md`：逐ID结论、原生运行候选、源码变化、缺陷及修复、本地/设备/外部/生产各自状态。

日志、截图仅含合成测试数据；凭证不写进命令示例、提交、报告。含认证信息原始日志放仓外私人路径，报告保留脱敏摘要/指纹。无需读或打印现有 test.env 的秘密。

## 5. 分步任务卡

### N0 — 基线与证据资格核对

前置：无。只读，预计30–60分钟（估算，不是截止承诺）。

动作：确认三仓基线；执行旧记录完整性检查；对旧候选至当前的差异逐文件分类为业务、测试、生成物或文档。提取旧验收的 source binding/命令/oracle；未变化领域可有条件复用，变化的消费者链必须列出影响测试。先读取使用到的 AGENTS/CLAUDE 文件。

命令（对应仓执行，保存退出码和完整输出）：

```bash
git rev-parse --show-toplevel
git rev-parse HEAD
git status --porcelain=v1
git diff --name-status <旧候选完整SHA> HEAD
python3 docs/design/2026-09-30-guangzhou-enterprise-ux/evidence/phase-local-final-2026-10-02/verify.py
python3 api/flatten_internal.py --check
```

Acceptance `N0-A01`：旧证据指纹完整；当前差异全部归类；确定真实 macOS 和隔离测试后端可用。源码不同/证据缺失列为待验证，不写 PASS。环境不可用时只阻断原生卡，继续离线卡。

### N1 — 当前增量离线回归与有依据的修复

前置：N0。预计1–2小时，无缺陷则更短。

先复用现有测试，检查是否覆盖暗色、大字号、长名称、空组织、多层宽树、加载失败/重试、分页、撤权、切账号/企业、晚到响应。已有覆盖不重复添加；缺失且涉及实际风险时补最小行为测试。禁止以测试专用数据覆盖生产权限判断。

App 精确回归命令：

```bash
flutter test --no-pub test/organization/organization_chart_widget_test.dart test/organization/human_directory_page_widget_test.dart test/organization/human_directory_controller_test.dart test/organization/human_directory_access_revocation_test.dart test/organization/organization_detail_hub_widget_test.dart test/unit_test/page/workspace_shell/enterprise_shell_page_test.dart test/unit_test/page/workspace/workspace_picker_page_test.dart test/unit_test/page/chat/p2p_call_preview_test.dart
flutter analyze --no-pub lib/modules/organization/presentation lib/page/workspace_shell lib/page/workspace lib/page/contact/contact lib/page/mine/mine
git diff --check
```

新增 touched 路径加入分析清单；格式化仅 touched 文件。禁止使用不存在的 `test/unit_test/modules/workspace_shell`（正确位置是 `test/unit_test/page/workspace_shell`）。测试依赖未准备好先正常恢复，不关闭检查或隐瞒失败。分析已有问题保留基线/差异，不能将非零命令写 PASS。

Acceptance `N1-A01`：组织图布局/分页/展开/拖动/真实双指缩放/适应视图等行为测试通过。

Acceptance `N1-A02`：撤权与上下文切换清图、晚到响应隔离通过。

Acceptance `N1-A03`：个人/企业四栏、成员/图切换、角色入口、选择器与增量 P2P 预览不回退；分析无新增问题。

### N2 — 最新图形组织架构真实 macOS 验收

前置：N1；隔离服务及合成测试账号就绪。预计1–2小时。

复用仓内真机测试框架，先查看 `python3 scripts/auto_test.py --help` 与现有规格；没有对应图形旅程时，仅补一个专属原生测试/规格，或用可复现的人工原生步骤记录。不以纯策略函数、Widget截图、静态HTML或浏览器页面代替原生运行证据。

```bash
flutter devices
flutter build macos --debug
```

记录实际启动的 `.app` 路径、构建来源 SHA/脏状态、设备、窗口大小、后端隔离环境、运行时间。改用 `flutter run -d macos` 时同样绑定来源，不调用全量集成脚本、不启动旧 binary 冒充新构建。启动参数复用已批准本地配置，不硬编码/公开测试凭证。不修改 macos 工程文件。

原生步骤及 oracle：

| 验收ID | 可执行步骤 | 必须观察到的事实 |
|---|---|---|
| N2-A01 | 企业通讯录→组织架构→展开两层部门→点击子部门 | 真实企业名、正确连线/父子关系；只有授权节点；成员页部门路径正确 |
| N2-A02 | 拖拽横纵方向→触控板缩放或可用原生缩放手势→按钮缩放→适应视图→折叠 | 视图实际改变；比例同步；可找回根节点；工具栏保持可操作 |
| N2-A03 | 窄窗口约400逻辑像素及宽窗口；浅/暗色；系统/应用可用最大字号 | 无文字/按钮溢出；长名称可识别；不重叠、无不可点击控件。尺寸/字号/主题写入记录 |
| N2-A04 | 空企业、部门加载失败及恢复；多页兄弟部门和足够宽/深的合成组织 | 明确空/失败/重试状态；不漏后续页；大图仍可导航。无法制造某场景就标未验证 |

截图用于视觉佐证；N2-A01另需接口/数据对应关系，N2-A02需操作前后状态或短录屏。合成数据必须明确标记，不能造预期返回代替真实后端读取。

### N3 — 企业权限和导航原生闭环

前置：N2；复用同一隔离环境。预计1–2小时。

| 验收ID | 步骤 | 预期 |
|---|---|---|
| N3-A01 | 个人通讯录、个人我→切企业→企业四栏→切回个人 | 菜单符合§3；企业工作台始终存在；顶部与内容对齐；个人无加入企业/创建组织；不打开 OA |
| N3-A02 | 账号A/企业A查看图→切企业B/账号B；延迟A请求后返回 | 不展示A名称/节点/成员；晚到请求不恢复A数据；刷新仍为B |
| N3-A03 | 同一隔离企业普通成员与管理员登录，尝试管理入口和既有直达路径 | UI权限与服务端资格一致；普通成员无法绕过权限，管理员保留既有治理功能 |
| N3-A04 | 查看组织图→在隔离环境撤销成员权限→刷新/切成员→切图；测试待返回请求 | 403/404后旧图卸载；旧数据不再次出现；恢复权限需重新读取 |

撤权只操作本run合成账号，不影响真实用户。生产数据不复制到本地。角色/直达校验复用已有服务器旅程，避免新建管理接口。

### N4 — 非 OA 历史领域证据重新绑定

前置：N0和N3；未变化领域不重跑全套。预计30–90分钟，取决于漂移。

Acceptance `N4-A01`：客服 CS-01..03 的访客入队、唯一接单、文本附件、转接结束、重连撤权的历史实际运行证据及源码消费者链与最终候选一致；变化或缺失的那项重跑隔离真实旅程。

Acceptance `N4-A02`：ORG-01..03、FILE-01..03、UX-01..03、INTG-03 的加入退出、Owner、工作区/群/频道/资料归属、签名访问撤权、合理裁剪及本期E2EE关闭证据可资格复用或局部补验。保留历史密文读取，不重启E2EE任务。

Acceptance `N4-A03`：Internal注册表/manifest实际导出全部操作，按路由引用过滤 OA SSO exchange及独立OA身份协议测试（当前INT-14）；剩余操作逐项保留旧真实HTTP/Grant/审计证据绑定。与OA无关的Application凭证、scope、Grant安全检查保留。不能简单沿用42/42全通过作为本计划结果；输出确切 included/excluded ID清单及原因，预期当前非OA为41项，实际注册表不同先说明。

每项复用必须有：旧候选和历史command/exit/oracle、证据SHA、直接源文件与被调用链的当前指纹对照、生成产物/依赖/运行配置变化说明；记录为 `PASS_REQUALIFIED`，不是 `PASS_EXECUTED`。仅HEAD祖先关系、状态JSON、接口HTTP200、构建成功不足以复用行为结论。

Backend新增缺陷才运行受影响EUnit/真实HTTP模块；仅测试环境变化需要验证对应环境入口。Admin业务未变则保留源一致资格，不强制重跑所有浏览器用例。若修复引起广泛领域变化，写清影响后扩大验证；需要全量后端门时冻结候选并按现有框架独立连续两轮exit0，VM全局TSID专用套件保持隔离。不得调用生产数据库或对外Webhook。

### N5 — 冻结候选、终审与交付

前置：N1..N4。预计30–60分钟。

局部修复按独立功能本地提交；最后一份功能提交确定三仓最终SHA后进行受影响的最终检查。最终测试后业务或生成物变化，只失效受影响的验收ID，重新执行对应卡；不把新的SHA替换进旧日志。

Acceptance `N5-A01`：N0-A01、N1-A01..03、N2-A01..04、N3-A01..04、N4-A01..03、N5-A01共16个ID完整且无FAIL/BLOCKED/PENDING；每个PASS有source SHA、命令exit0（人工原生步骤明确标manual而非虚构进程exit）、可判定oracle、证据指纹。人工记录须包含步骤/实际结果/观察者，最终审查接受才能PASS。

终审者只读核对16个ID精确集合、原生构建来源、三仓Git状态、source资格、证据哈希与排除项；不得由执行者自行编一个通过JSON代替终审。用户最终视觉满意度保持 `USER_VISUAL_ACCEPTANCE_PENDING`，不阻断“技术验收通过”，也不冒充用户确认。

交付 `FINAL.md` 和简短用户报告：最新效果图/录屏链接、缺陷修复提交、实际测试范围、16项验收表、非OA Internal精确计数、后续独立事项。状态分别列：LOCAL、MACOS、OA=`OUT_OF_SCOPE`、PRODUCTION=`NOT_DEPLOYED_NOT_VERIFIED`。

## 6. Failure → Recovery → Retry → Next State

| 失败 | 恢复动作 | 重试与下一状态 |
|---|---|---|
| 路径/工具/依赖错误 | 查真实路径/版本并恢复本地依赖；不得改预期掩盖失败 | 重新运行原卡，保留原失败记录 |
| 产品断言失败 | 最小复现→定位共享调用链根因→局部修复→对应评审 | 每卡最多2轮修复重试；仍失败为BLOCKED且继续无依赖卡 |
| 原生设备或隔离环境不可用 | 记录具体缺项；完成离线和只读资格卡 | 原生ID为BLOCKED_ENV；不得以Widget图冒充原生PASS |
| 候选漂移/他人WIP出现 | 记录来源并重新计算影响；不回退/吸收WIP | 未受影响ID继续，受影响ID回PENDING |
| 权限/跨企业数据泄露 | 保存脱敏复现并阻断该领域通过，先修复再复验 | 禁止绕过403/放宽授权；无法局部修复则BLOCKED_SCOPE |
| 需要OA/生产/第三方动作 | 不执行；标记排除边界 | 不阻断本计划其他卡；不自动等待/重试外向操作 |

每卡结束立即保存state、acceptance、recovery。恢复执行先核对现有产物/进程/候选，不重复创建或重新播报已完成步骤，不重放结果不明的写入。只清理本run明确拥有的合成夹具和进程。

状态：`READY→PREFLIGHT→TEST→FIX→RETEST→NATIVE_ACCEPTANCE→REQUALIFY→READ_ONLY_REVIEW→NON_OA_LOCAL_AND_MACOS_ACCEPTANCE_PASS`；不能满足必需ID则`PARTIAL/BLOCKED`。工作困难或预算不足不是PASS。

## 7. 成本与执行节奏

无重大缺陷且macOS/隔离环境现成时，预计4–8小时；环境不可用或出现权限缺陷时重新估算，不能为了赶时间省略原生oracle。每卡结束报告完成ID、失败原因、下一卡及耗时；30分钟无进展给出具体阻断与恢复动作。禁止反复跑已通过的全量测试，禁止另造架构、重写客服系统或扩展OA工作。
