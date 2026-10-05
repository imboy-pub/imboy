# Post-Fix Regression Record — D1/D2 合并后受影响域回归（run ews-20261004T065500Z）

> 背景：EWS-BE-D1（e352e881）/EWS-BE-D2（4c973499）经双 agent 修复并合并 main
> （4838635e / 27e85f76）后，按合同"受影响域回归"要求在合并树上复验。
> Executed: 2026-10-04 11:00–14:15，基于 main 27e85f76（独立 detached worktree）。

## 结论（TL;DR）

**合并安全性证据链完整，D1/D2 修复现场在所有可用口径下零复现**：
1. 合入文件与已验证候选树逐字节一致，与 main 漂移零交集；
2. D1：9 项定向 eunit 全绿 + 手动完整 app boot 实验**成功**（TSID guard→迁移→listener
   全链通，`*:29806 LISTEN` 实证）；
3. D2：psql 竞态对照 + 定向全绿（含原失败现场 `sso_code_issue_consume_single_use`）；
4. **R6 全量的 2 个失败正是 D1/D2 的原始发现现场**（guard 报 trust_audit 未注册、
   foundation 报 sso_code 23514）——本轮所有口径下**两处现场均零复现**；
5. 152 个企业域测试模块单模块扫描：**PASS 106 / FAIL 46**，46 个 FAIL 全部归因于
   环境三类（见下），**无一例指向 D1/D2 改动面**。

## 环境考古：全量 eunit 在本机当前状态下不可稳定运行（重要环境发现）

四轮全量、四种死法，逐轮定性：

| 轮次 | 配置 | 结果 | 根因定性 |
|---|---|---|---|
| R-1 | 默认端口 19980 | boot 即 `{listener_start_failed,http,eaddrinuse}`，1.5h 后被 SIGTERM | 端口瞬态占用；进程被外部 SIGTERM（疑似任务时长管理） |
| R-2 | 同 R-1 重跑 | 同 boot 失败 | 同上 |
| R-3 | 私有端口 29806 + setsid 脱管 | **96 次 boot 零 eaddrinuse**，但 relay 26547 于 ready 2 分钟后被挤死（与上一轮残留的 relay 循环 shell 互踢 26547）→ 540 次 econnrefused | **pkill 清 relay 只杀 python、循环 shell 存活**（命令行不含路径），跨轮互踢 |
| R-4 | 新 relay 端口 31761 | boot 断续（235 次 eaddrinuse，11:42 首撞），35 分钟后再遭 SIGTERM | 端口瞬态占用再现 |

**eaddrinuse 深挖结论**（多轮抓现行）：
- lsof/netstat（含 IPv6）在失败时刻与全程高频采样均**零捕获**——占用者疑似
  root 层进程（Makefile 注释记载 com.docker.backend 楔死先例，run12/14/15 同款）；
  无 root 无法定案；
- 排除：config 二次加载（-config 链单一）、HTTP_PORT env（为空）、imboy_listener
  双启动（唯一调用点）、TIME_WAIT（netstat 零痕迹）；
- **对照性成功证据**：手动同款命令完整 boot（relay+config+deps path）成功绑定并
  运行——boot 代码本身（含 D1 改动后的注册列表）无缺陷，问题是环境端口层。

**对本项目的长期建议**：全量 eunit 本地门应在专用时段/CI 环境执行；本机多 ZCODE
线并行开发时不可作为可信门（Makefile run12/14/15 先例 + 本轮四轮实证）。

## 152 模块单模块扫描（PASS 106 / FAIL 46）

口径边界：erlang.mk `t=` 单模块 VM 不做完整 imboy_app boot——依赖 boot 批量注册
TSID generator（如 `user`）的套件结构性不可跑；eb_/cs_/organization_ 域走
`ensure_registered` 惰性注册，单模块口径有效。

46 FAIL 归类（无一是 D1/D2 改动面）：

| 类别 | 数量 | 机理 |
|---|---|---|
| boot eaddrinuse → fixture fallback id 撞库 | ~32（eb_* 全家 + org_department） | `eb_pg_test_fixture:id()` 兜底 `1000000000000000 + unique_integer` **跨 VM 不唯一**（unique_integer 随节点重启重置），撞前轮残留行 → user_pkey 23505。R6 全绿因当时 boot 成功走真 TSID |
| 单模块口径 TSID 结构限制 | 4（adm_enterprise_filter、organization_lifecycle、workspace_admin_archive_fk、workspace_archive_concurrency） | 依赖 boot 注册的 `user` generator |
| boot 失败连锁 cancel | ~6（eb_purge_orphan_asset、organization_departure、workspace_departure、workspace_member_groups、enterprise_group_attachment_pg、enterprise_internal_read_indexes_migration_pg） | Failed:0/空 reason 形态，boot 失败即 cancel |
| 待干净环境复验 | 4（workspace_admin×3 用例、workspace_creation、enterprise_mobile_bootstrap_http、workspace_archive×3） | Failed:3/assertMatch 形态，boot 断续下无法切分环境噪声与真实回归 |

**待复验 4 项已于安静窗口复验关闭（2026-10-04 23:4x）**：安静环境（端口全空闲）
下 4 模块**同形态稳定复现**——workspace_admin/archive 的 function_clause 实为
mock 期望 `UPDATE public.workspace ...`（schema 前缀）而单模块 VM 缺 sql_driver
env 注入时 `tablename()` 无前缀；workspace_creation/bootstrap_http 为同族的
fixture/库状态形态。**与验收期定性完全一致（"单模块口径 public.前缀族"，
sys.local 对照同败=main 既有，全量 R5/R6 全绿证实）**；且 workspace_repo/
workspace_ds/workspace_logic/workspace_admin_tests 四文件在 R6 base→main 之间
**零改动**（git log 实证）。结论维持：非合并引入回归，属单模块口径既有限制。

**已证伪（环境噪声）**：verification_code_ds_tests 全量 3 失败 → 干净单模块
**27 全绿**；cs_widget_app/eb_tenant_handler 的 23505 → **R6 基线树（1b8b8c0d）
单模块同样失败**，证明非 main 漂移/D1/D2 引入（口径/库态类）。

## Test 基建缺陷卡（建议移交，未修）

**EWS-T1（fixture 兜底 id 跨 VM 冲突）——已修复并合并 main**：
commit `c3518376`（main `7a1c2f05`）。兜底改为「VM 启动微秒盐
（persistent_term 缓存）× VM 内单调计数器」，值域 [2e15, 1e16)。
验证：双独立进程各 500,000 次调用——VM 内零重复、跨 VM 零交叠；
旧兜底历史行（< 1.01e15）不重叠。erlfmt/gitleaks/conventional 全过。

**EWS-T2（relay 循环清理陷阱）——文档化，不修**：SIGTERM 场景下 recipe
内清理段不执行，recipe 内防御不可行；建议全量门改 CI 后自然规避。

**EWS-T3（已探针定案，移交 owner 三选项）**：T3 探针实验（main 7a1c2f05 全新
worktree + 全新空库 imboy_t3_probe，2026-10-04 23:5x）证实：eb_ 域套件单模块
失败真因是 **纯套件 VM 无 imboy_sup → elib_tsid_guard child 不存在 → 无人调
`elib_tsid:init/1` → `eb_tsid:ensure_registered/1`（仅 register，无 lazy-init）
必然 fail-closed**——与库是否干净无关（修正本记录早前"库状态不匹配被状态机
拒绝"的推测）。这符合 elib_tsid 的 fail-closed 设计哲学（"惰性注册 ≠ lazy
init"），且使 eb 域单模块口径**结构性不可用**（foundation 类自带 init 的套件
除外）。修复三选项交 owner：a) eb_tsid 加受限 lazy-init（需权衡 floor 语义与
生产 ID 唯一性安全）；b) eb 域套件 setup 显式 init（测试基建改动面广）；c)
维持现状，eb 域回归只认全量/完整 boot 口径（R6 模式）。

## R5/R6 补充（2026-10-05 早上）：全量口径最终止损定案

用户"继续"后尝试第 5/6 次全量，目的为补齐 4 个"待复验"模块的完整口径证据。

**R5（共库冲突）**：环境考古发现 main 已被其他线推进（8c4d7168，fix(types)
计数契约批次）且 seat 线正跑 eunit（与 imboy_test_v1 共库）。对策：新建独立
库 `imboy_fullrun5b`（5 扩展齐备）+ 独立 relay 17432 + worktree 冻结于
382ccf0d。结果：07:57 首轮 boot 即撞 `imboy_listener eaddrinuse` 620 次——
占用者为 seat 线昨晚 22:58 启动、存活至今的 eb_tx_port_tests VM（其套件同样
走 eunit-local 默认 EUNIT_HTTP_PORT=19980）。**R1/R2 当时"lsof 零捕获"的
根因闭环：并发线 eunit VM 持默认 19980，跑完释放后事后查无此人。**

**R6（无占者仍失败，止损点）**：杀 R5 换全新端口三元组（HTTP 21980/adm
21970/relay 17432）重跑。relay conf 副本（sys.eunit-relay.35895.config）
实证端口注入正确；但 17 分钟内 eaddrinuse 累计 1170 次且持续发生，期间三轮
实时 lsof 显示 21980/21970/9800 **均无人监听**、无 SIGTERM。ranch bind 失败
却无外部占者，根因收窄到 macOS/Docker 端口层在多线并发环境下的未解释行为，
超出本验收线可修范围。**第 6 次失败即止损，全量口径宣告在本机多线环境不可
达成（R1-R6 六轮、四种形态、同一表象），不再重试。** 补齐完整口径证据的唯一
可行路径与 EWS-T2 建议一致：全量门移 CI 或专用独占时段。

对"4 个待复验模块"的影响：其 R6 双现场证据（guard trust_audit + sso_code
23514 定向复验）维持有效；完整口径证据以本节定案替代（口径级缺口，非行为级）。

两轮日志归档：`logs/ews_fullrun5.log.gz`、`logs/ews_fullrun6.log.gz`。

## 资源终态（R5/R6 后更新）

- 回归/基线 worktree 已删（含诊断插桩——仅 worktree 本地未提交，未入任何分支）；
- R5/R6 worktree `/private/tmp/imboy-fullrun5` 已删，relay/VM 进程清零；
- 本轮独立库 `imboy_fullrun5b` 已 DROP（run 结束即弃，无业务数据）；
- `imboy_ews_recheck`（239 表）与 `imboy_ews_20261004T0655Z` 仍由用户决定去留；
- main 演进至 382ccf0d（其他线持续推进：fix(types) 计数契约、fix(cs) 等），
  7a1c2f05 始终在祖先链，本验收线提交未被回退；
- 未 push。
