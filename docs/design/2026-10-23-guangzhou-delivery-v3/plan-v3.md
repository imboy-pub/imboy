# 广州交付 V3：合同重对齐与剩余验收 / Guangzhou delivery V3

日期：2026-10-23。状态：**PLAN_READY_FOR_REVIEW / IMPLEMENTATION_PARTIAL / NOT_ACCEPTED**。

本轮仅重新盘点、制定计划。没有实施、运行全量、启动服务、调用供应商、创建 worktree、提交或推送。V3 以清理后的本地 main 为事实起点，不继续旧广州或 talk_record 执行器。V2 原合同及验收门保留；V3 替代旧调度与运行目录，不修改原始失败、暂停和预算记录。

## 1. 文件与事实起点 / Authority and baseline

- 本文是唯一任务顺序、依赖、验收和恢复规则入口。
- [baseline.json](baseline.json)：三仓 commit/tree、既存 WIP、17 个直接来源指纹及旧 materials-index 逐路径复核。
- [requirements.json](requirements.json)：252 条原文义务索引，含 P1 35、EO 10、MC 26、CI 8、CX 55、继承坐席 67、广州卡 26、面聊任务 25。来源行号与完整 oracle 保留；任务容器、父项、来源记录不可相加为独立功能数或完成率。
- [evidence-index.json](evidence-index.json)：实际保留文件的 SHA256、适用边界；不是通过证明。
- [resume-prompt.md](resume-prompt.md)：后续会话使用同一份提示词恢复。计划版本不等于进度版本。

| 仓库 | 本地 main | 本轮结论 |
|---|---|---|
| Backend | `9f96b45ff5f864320585d1c74dac514add241893` | GZ 成果、可选豆包恢复与类型/登录修复已合入 |
| App | `f1086336e1c316c6189101e26b2f3f071709c3ee` | 保留 MiniMax/BYOK 防护，豆包按开关接入 |
| Admin | `2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44` | 相关历史提交已在 main，不能推出当前端到端已验收 |

上述是本地观察，不是远端 HEAD。未来执行先复采；不强制 fetch 后丢弃本地整合成果，不 reset/clean/stash 外来 WIP。确需读远端时，单列来源差异与授权；不能静默更换候选。

### 已定位的来源缺口

17 个直接来源均存在。但旧 materials-index 的六个原路径已不存在：`ews-plan`、`v21-closure`、`nonoa-plan`、`ews-input-manifest.json`、`ews-inherited-requirements.json`、`ews-ews-coverage-map.json`。已检查主仓 docs/plans 同名位置也不存在。V01 先尝试 materials-index 给定 Git blob、现有归档和原仓路径，核对 expected digest；恢复不成功则保留 `BLOCKED_SOURCE` 及其所有后代，不能按冻结摘要假装递归合同全集已闭合。

本版已经形成可执行的补证路线，**尚未完成全部递归叶项的 exact-set 证明**。252 条是来源索引，不是最终叶集合。原合同数字/required/平台要求优先于本版概括。未找到正文的继承项不能默认为 N/A；其他已知独立项可以先行。

## 2. 证据重新定级 / Evidence admission

| 保留证据 | 可支持的结论 | 不能支持的结论与下一步 |
|---|---|---|
| Backend 177 用例 receipt，绑定当前 SHA，exit=0 | 离线协议、授权、恢复及 mocked handler 定向检查 | 真实 HTTP/PG/供应商/整个后端通过；V09/V12 补齐 |
| App 最终全量日志：10223 passed、313 skipped | 曾在配套原生库环境完成测试日志 | 每个 Required 均通过、真机通过；V12 核命令退出、依赖、候选、skip 逐项归属 |
| App analyze 日志无问题 | 静态检查支持 | 原生交互、Android 后台录音或 macOS 安全存储通过 |
| Dialyzer 基线 GREEN、205 存量告警 | 无新增基线告警 | V2 C22 要求的 raw RC0；V12 明确保留此差额，不能改基线洗白 |
| SQL 223 模板上事务内执行 224→225 后 rollback，exit=0 | 两份 SQL 在该合成上下文可执行 | 迁移 ledger、空库/非空历史升级和恢复通过；V03 重验 |
| security-gate、Moya 迁移 8 用例日志 | 各自限定的安全/迁移检查线索 | 整体终验；V12 重新绑定实际命令与候选 |
| GZ checkpoint/candidate-manifest | 历史失败、暂停、余额及源码线索 | 新候选 PASS；checkpoint 多个候选字段互相过时，禁止执行其 NEXT_UNLOCK |
| talk_record run4 terminal | 历史运行失败及调查入口 | 失败已修好、run5 已执行；V09 先复现当前 HTTP 创建重放/恢复链 |

证据接纳顺序：原文件存在且 hash 一致 → 原命令/退出码/执行数/skip 可核验 → 候选与环境匹配 → 正负业务 oracle 真实成立 → 才能挂接叶项 PASS。缺一项为待补证；旧候选可作回归设计输入，不能直接转换为 V3 通过。日志结尾“All tests passed”不能独自代替 receipt。

## 3. 范围与不可变边界 / Scope

主线继承：Core IM、企业目录与双向 OA 身份入口、App/Admin 交互、人工客服、附件、推送、安装升级恢复、完整质量门、容量/连续24h、独立复核与交付。朋友圈/附近的人依原一期合同保留。Cache、附件 V5、微社群 V1+CI V2、CX、一期原合同余额分别记账。

talk_record 作为独立产品验收轨道 V09，与广州仅共享附件、身份、数据库和构建依赖，不能混算完成率；若它未被广州 profile 启用且已证明不会影响共享能力，可以不阻塞广州业务叶，仍不得签面聊完成。若共享迁移/附件/权限受影响，相关广州叶也阻断。

Human-Human E2EE 和历史密文保持；企业 delegated-Human 仅限已授权企业操作。旧一期 E2EE OFF 的 profile 文字与当前保留 E2EE 要求按适用场景显式对齐，不扩大 OFF 范围，不重启暂停的 E2EE 专项，也不自动迁移密文。organization 是业务对象，不能当租户授权边界；workspace 与资源 ACL 逐请求复验。企业接入使用 `/api/internal/v1/*`，Open Platform 暂停。

不动 `erlang.mk`、App `ios/*`、`macos/*`、`plugin/r_upgrade`；若验收发现确需改保留区，列具体缺口等待该项授权，独立工作继续。不重写群文件、群任务、成员管理、企业治理业务界面；已批准列表/详情/空间/工作台 UX 需回归，不再次整仓换设计系统。

个人/企业频道链接、QR/扫码落地、失效邀请、订阅/撤权闭环是已有 UI 与业务链路的盘点内容。个人/企业面聊入口、录音恢复、共享/撤销、导出/移交、注销预检须覆盖。源码有页面不能代实际入口闭环。

## 4. 实施与验收卡 / Work cards

下列卡各有准备依赖与签发依赖。默认唯一执行器；准备可交叉，涉及共享路径/资源的运行串行。不为每卡复制 SDK/依赖/数据库。下表的路径是调查与复用入口，未来动手前缩小到精确 owned_paths；不是授予整个目录写权限。

每卡通用出口：原来源 oracle 的正例、负例、失败恢复均有实际值；受影响模块及直接调用方验证；无未闭 P0/P1；产物/证据绑定同一输入；Required 的 skip/cancel、未知费用/副作用不能记 PASS。未复现代码缺陷时只补证，不预设必须改实现。

| 卡 | 准备依赖 / 签发依赖 | 已有实现、剩余动作与可观察出口 | 原合同映射 |
|---|---|---|---|
| V01 合同全集与基线 | 无 / 来源逐叶可核实 | 复采三仓/WIP；核六条失联来源与修订优先级；展开角色×平台×scope×故障子叶；无孤儿父项、无重复叶/静默减范围。输出 requirements 冻结集与来源差集 | C01/02；全部来源根 |
| V02 资源与恢复准入 | V01 部分 / 对应叶合同 | 单控制器、资源身份与独占租约、命令目标、限额与预算；演练控制器中断接管、旧 epoch 回写拒绝及污染证据失效，恢复不重复写/提交供应商任务 | C03；T01、A09 |
| V03 PG/SQLite 迁移 | V01/02 / 独占合成库 | 复用 check_migrations、drill_migrate、App SQLite gate；224 保留 Moya 退役、225 为 provider_task。空库、明确旧版本非空多FK fixture、二次up、故障恢复、ledger 与关系/字节逐项对拍；不把 Down 当无损回退 | C04；MC02/03/24；T04 |
| V04 Cache/附件/撤权 | V01/02 / 相关 V03 | 复用 cache-v2.2.1 和附件 V5 runner；COMMIT/ROLLBACK、QR并发/重启、旧票据撤权；真实 PUT/confirm/Range/hash、引用/hold/retention/queue/lease。只证所用 profile 闭包；未启用 snapshot 不能擅删专项义务 | C05/06；Cache、ATT G0–G6；EO03/05/06 |
| V05 Core IM/profile | V01/02 / V03/04 对应叶 | `msg_ack_logic`、`msg_c2g_timeline_repo`、群事件及 feature/policy；双设备送达≠已读、逆序游标不回退、SERVER_ACK前后重启无丢重；群转让/解散通知不双发；停用功能 HTTP/WS/worker/深链均拒绝且合法OA可用 | C07/13；P1 AC01–14、朋友圈/附近原叶 |
| V06 目录/入站/出站身份 | V01/02 / V03/04 | `enterprise_directory_sync_logic:sync/3`、`claim_login_tx/4`、OA login/SSO；同步/关联/登录三种顺序不重复或错绑；intent/PKCE/DID/epoch与发行权限复核；出站code单消费。退A不连带禁用B/个人；拒绝后无新增会话/映射/用户 | C08–10；EO02/07；ORG §8.2–8.3.1 |
| V07 App/Admin/推送 UX | API 固定即可准备 / V05/06 与相关 V04 | App organization、workspace、channel 路由及 Admin directory；Android真机/macOS原生分别验入口、分页、搜索、部门下钻、切账号/空间与迟到回调；大字号/键盘/顶部标题不遮挡；频道链接→QR→受控订阅。推送前后台/杀进程/弱网/DND/token/撤权逐平台真实核验 | C11/12/14；EO04/08；P1 AC05/06/22/23 |
| V08 客服闭环 | 接口固定即可准备 / V03/04/05/06 相关叶 | Admin 现有 queue/hooks/API 与 backend customer_service slice；首次开通、搜索/待回复、IME、多会话草稿、附件未知发送、长历史/ACK/SSE、双坐席CAS认领/转接/关闭、两种snippet、真实扫码、Widget/hosted、评价/运营口径按SQL核对 | C15–19；CX55 + SC67 原记录逐条 |
| V09 面聊剩余闭环 | V01/02、已有API / V03/04、授权平台/供应商 | `talk_record_*` handler/provider/recovery；App store/api、录音条与routes。先复现HTTP创建重放500、旧T05 fixture污染；个人/企业CRUD→长录音→附件补传→ASR→摘要→校对/回听→共享/撤销→导出/移交→删除/注销。并发单任务、未知计费先查询，拒绝旧账号/输入版本/撤权结果；具体8组场景见下节 | T01–T25、A01–A09；DOUBAO-API、UI-SPEC、QUALITY |
| V10 微社群及专项余额 | V01/02 / 对应 V03–07 | 保留 MC01–26 全子场景、付费仅合成订单；绑定/权限持续撤回/真实WS拒投/中文正文检索/真机讨论返回/Admin管理。CI V2 为增量，不替换V1；未启用功能分类依原合同而非删叶 | C26；MC/CI/Cache/ATT/一期余额 |
| V11 集成/安装/恢复 | 先准备清单 / 所用业务叶通过 | 冻结三仓向量、profile、runner、fixture、工具、正式产物；UI→API/WS/SSE→PG/对象→另一端可见。新装、非空升级、备份→另一隔离目标恢复；消息ID/成员关系/附件hash和实际可读一致 | C20/21；P1 AC29/30；MC24 |
| V12 最终全门 | 先列全集 / V11、稳定候选 | 主集成者统一执行后端两轮全EUnit及排除套件、原始Dialyzer/安全/分层；App analyze/fulltest/SQLite；Admin test/lint/typecheck/deadcode/build；Widget配对与适用CI全矩阵。205条告警差额、313 skip逐项解决或保持阻断，不自改Required | C22；P1 AC28；MC25；CI01–08；T24 |
| V13 容量/24h | 可先准备采样 / V11/12、容量确认 | 沿V2测量合同热身30min+完整连续24h；正常/故障/恢复原始分母、p95/p99、每组件内存与恢复时限可复算；不得与构建/全量抢资源 | C23；CX00-A05/CX08；P1 AC31–34适用修订 |
| V14 独立终审 | 可先审计划 / V01 exact-set、V11–13 | 从原来源复算叶全集、hash/candidate/exit/skip/oracle；缺ID、多ID、错候选、缺raw、mock冒真机、非零假PASS等变异必须拒绝。无独立reviewer则BLOCKED_REVIEWER；实施者不能换名自签 | C24；MC26；T23/25 |
| V15 交付与UAT | 可先整理草包 / V14、用户/客户所需确认 | tracked白名单与许可证、部署/升级/恢复说明；用户五项亲验、客户OA、真实通道分别确认。技术PASS≠UAT≠发布资格≠生产；不自动发邀请、推送或部署 | C25；CX09/10；P1 EXT/PROD |
| V16 总余额与交接 | V01起持续 / V14 | 每个原Required有唯一去向、证据、缺口、owner、下一解锁条件；广州、面聊与五专项分别结论；不重新开已暂停产品域 | C26；T25 |

### V09 的八组最小验收场景

1. 个人/企业创建与幂等：同请求重放只一条记录；串workspace/角色拒绝且无写入。当前历史500需先复现定位，不认定源码仍必错。
2. 真机长录音与持久恢复：前后台、断网、进程结束、恢复同一capture；AAC实际可解码、时长/段序/引用一致；不靠内存假恢复。
3. 附件：统一pending/confirm/purpose/授权票据/续签，撤权与过期拒绝；hold不GC、重复删除不丢他人数据。
4. 豆包：显式启用后prepare→assembly→提交→query→结果；响应丢失/429/401/402/超时/杀进程保留任务ID，不自动重提计费任务；默认关闭兼容路径继续回归。
5. GLM：`glm-5.3-flash` 通用API；引用绑定同记录/输入版本，旧结果不得覆盖；事实支持≥95%、关键行动≥90%、引用有效100%，虚构人物/金额/日期为0。
6. 分享与编辑：双自有账号授权/回听/编辑冲突/撤销，旧票据和迟到响应均不能越权；导出只含当前有权内容。
7. 治理：企业移交、退出、删除、注销预检、hold/retention/恢复/清理逐种终态；供应商副本留存未知须明示，不冒称本地GC已删云副本。
8. 本机Key：按账号/供应商安全存储，配置保存零调用，换设备不迁移；业务HTTP/日志/备份中无Key。仅公开/合成样本，先核累计费用和剩余额度；原5元是累计目标而非每次重试5元，不能因新run清零。原声学质量线CER干净≤8%/噪声≤15%、DER普通≤15%/重叠≤25%及其他原指标保留。

## 5. 命令绑定和测试策略 / Commands

实际入口沿 V2 K-*，不是新造CLI。以下是经文件存在性确认的复用命令，**本轮未执行**。执行前读取脚本/配置并写入命令绑定：真实 cwd/完整 argv、工具/runner hash、只含安全引用的环境、timeout、预期exit/执行数、fixture、资源身份、oracle。未填完禁止执行；不能采用脚本默认共享库。

| 入口 | cwd | 准备/定向与最终用途 |
|---|---|---|
| 静态迁移 | B | `bash scripts/check_migrations.sh`；只证明配对，不证明升级 |
| PG升级 | B | `escript scripts/drill_migrate.escript version` / `up`，核编译与全部PG参数；客服补 `bash scripts/customer_service_migration_gate.sh` |
| 定向EUnit | B | `IMBOYENV=local make eunit t=<已核实非空模块>`；检查setup是否启应用/DB，BEAM/ETS/meck共享资源串行 |
| 最终后端 | B | `make compile`；`IMBOYENV=local make eunit`两轮分别取证；`make dialyze`；`make arch-check`；`make security-gate`；`bash scripts/check_server_zero_crypto.sh` |
| SQLite | A | `dart run tool/generate_sqlite_migrations.dart --check`；`bash scripts/run_sqlite_migration_gate.sh --local` |
| App | A | `flutter analyze`；`flutter test`；FRB库与Dart包必须同版、hash绑定；`flutter devices`采真实设备后绑定适用 integration_test 完整argv，不能用模拟器 |
| Admin | D | `bun run test`（isolate）；`bun run lint`；`bun run typecheck`；`bun run deadcode`；`bun run build` |
| Widget | D | `bun run build:widget`；`bun run verify:widget`；`bun run verify:widget-pairing` |
| 真实客服 | D | `bun run test:e2e:customer-service:real`、`:p2`；Seat用 `SC153_E2E_EXECUTE=1 bunx playwright test --config=playwright.customer-service-seat-embed.config.ts`；先审目标/启动副作用 |

B/A/D 是未来冻结的独立后端/App/Admin工作树，不是本机main可变目录。现有 `scripts/test/phase1_gates/*` 可以复用，但先检查默认目标和过滤器，不能一键跑 wrapper 后忽略其排除集合。CI按当前 `.github/workflows` 完整枚举；远端证据缺失保持缺口，不能以本地门代替远端同候选检查。

开发期只验受影响模块与直接调用方；最终统一跑适用全量，不给每个worker各建全量环境。现有Makefile排除集、需要真实PG/Gateway/T20/governance的专项单列，执行数为0或提前退出拒收；TSID与全局fixture污染在独立VM及隔离DB复现。代码/依赖/schema/profile/runner变化按影响闭包使证据失效，必要时重跑，不能把“只跑一次”当豁免。

## 6. 资源、磁盘与中断恢复 / Runtime custody

未来授权执行后使用**一个**用户可定位的新根：`/Users/leeyi/.codex/imboy-delivery-v3/`，不重建已删除 `.codex-runs`、`.codex/task-runs`。不在本轮创建该根。根下仅 `control/`、`worktrees/`、`artifacts/`、`logs/`、`scratch/`；默认每仓一个工作树、单套工具链安全复用、一个Android构建槽。不再每次attempt克隆SDK/Gradle/Rust/Python。

启动前盘点容量、实际可回收量、依赖和现有进程；磁盘准入按本次工具链历史高水位+安全余量冻结，不把旧23.3GiB当永久常数。只清本run明确可重建scratch，先核活跃引用；源码、补丁、唯一证据不得自动删除。归档大小上限/保留周期待V02实际测量确定，超阈值先暂停重构建并报告，不无限复制。

资源登记包含 owner/run_id/epoch、PID+启动时间、设备serial、PG实例/物理data目录/数据库/schema、Garage实例/bucket/prefix、Docker CID/volume标签、端口、构建路径、租约期限。合作锁不能替代目标身份；连接握手和每个破坏操作前复核真实资源。资源被其他会话占用时等待或选独立资源；不kill外部进程，不借共享PG/真机，不全局Docker prune/restart。

默认单执行器。此前“最多8并行”是上限意愿，不要求造8份环境；本会话当前最多4活动角色且未获启动子agent授权。未来如获明确授权并行，取用户上限/平台上限/实测资源最小值；控制器、reviewer都计入。允许源码准备与只读审查并行，迁移号/公共schema、AUTH/ATT/MSG、CS共享hooks、App生成物分别单写者；PG故障、设备、Android构建、全量/soak互斥。

### 最小持久状态

执行时只维护一份 `control/state.json` 与 append-only `control/events.jsonl`，以及原始日志/必要receipts；不再生成数百份彼此矛盾的进度摘要。state至少含：plan/source hash、schema_version/state_version、run_id/controller_epoch、候选向量、授权引用、逐叶状态、当前owner/attempt、资源租约、累计重试/费用、evidence refs、blocked_reason、NEXT_UNLOCK。

状态写入用同目录临时文件→fsync→原子替换并记录事件；epoch/state_version compare-and-swap，唯一控制器锁。实现此机制前不能声称可自动接管。接手先验证锁持有者PID+启动时间/心跳和资源，不凭租约过期杀进程；旧owner未停止只能只读盘点。暂停为USER_PAUSED，只有明确恢复指令才启动执行。

叶状态：TODO→READY→RUNNING→PASS/FAIL/BLOCKED/INVALID/USER_PAUSED；实现组件另记IMPLEMENTED，不与PASS混用。RUNNING失联先查进程、输出与真实业务状态，记INTERRUPTED；外部已提交但响应未知记RECONCILE_REQUIRED，先查询/对账，不重放支付、供应商提交、迁移或删除。

每个失败先留首因，普通可逆步骤建议首次+最多2次有根据修复重试；这是V3建议预算，待执行授权冻结，不能覆盖旧耗尽/未知授权。付费、故障、破坏性步骤无自动重试。历史预算UNKNOWN不改0；隔离无副作用检查可准备，复用旧危险操作必须有新明确预算。可解锁独立卡继续，不让某个外部等待阻塞所有准备。

### 接续验收

V02实际演练：①原子状态写入中断，恢复前一完整版本；②RUNNING进程存活时不重复启动；③进程已死而业务提交未知先查询；④旧epoch结果拒收；⑤篡改hash/换设备/换DB使相关receipt无效；⑥恢复后外来WIP hash不变。六项有运行证据才签“可中断接续已验证”。

## 7. 最终通过、外部阻断与时间 / Verdict

- `GZ_V3_LOCAL_CANDIDATE_PASS`：递归LOCAL_REQUIRED全集闭合、同候选V11–14通过、无未闭P0/P1，包括适用原生平台/真实推送/CX本地24h。不能仅以卡数百分比签发。
- `TALK_RECORD_LOCAL_PASS`：独立T01–T25/A01–A09全部适用叶、真机和真实供应商质量通过；不由广州状态推出。
- `CUSTOMER_UAT_PENDING/ACCEPTED`、`RELEASE_NOT_QUALIFIED/QUALIFIED`、`PRODUCTION_NOT_EXECUTED` 独立保留。本地24h不代CX10生产24h。
- 容量确认、真实OA/推送凭据、平台签名/安全存储、独立reviewer、剩余付费额度缺失只阻断对应叶；不编造值、不降Required、不代用户亲验。不应重复问已经明确且适用的授权，但不将旧特定run的准入自动移植到新资源。

工期不按历史测试数推算。V01/02完成后，用可重现缺陷数、资源与fixture准备成本、全量实际耗时重估；至少保留热身30min+连续24h的自然时间以及外部等待。不能承诺同一短会话完成全部本地/客户/生产验收。会话内优先完成所有已解锁卡并落状态，阻断时写最小NEXT_UNLOCK。

本计划静态自检：直接来源17/17存在、索引252条无重复ID、旧C01–26/T01–25均有去向、已知失联来源6条显式列出。没有业务测试、独立终审或全部递归合同闭合证据，因此不打9.9分、不声明PLAN_ADMITTED或交付通过。提交、合并、推送、外部通知、生产操作均需各自授权；本轮未提交。
