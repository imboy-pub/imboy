# EUnit 全量预检盘点（C12 最终候选验证 · 只读预检）

> 任务：CACHE_V221_BACKEND_CLOSEOUT_V1 / C12 全量 eunit-local 窗口排障预检。
> 本文性质：**只读盘点产出，未运行任何测试/服务，未做任何 git 操作**。
> 盘点时间基准：2026-10-12（ Asia/Shanghai）；工作树含 C09/C10 在途修改
> （`src/lib/imboy_cache_sync*.erl`、`test/lib/imboy_cache_sync_tests.erl` 等，未提交）。
> 上游文档：`c12-window-plan.md` §6.1（全量 PASS 判据）、`w1-w5-report-20261011.md`
> W5-010（历史全量实跑数据）、Makefile:554-677（eunit-local 机制）。
> 证据日志：`~/.codex/cache-closeout-runs/zcode-20261011T130725Z/logs/`。

---

## 1. 套件清单（test/ 全量 *_tests.erl）

### 1.1 总量

| 指标 | 值 | 口径 |
|---|---|---|
| 测试文件数 | **898** | `find test -name "*_tests.erl"`（另有 10 个 CT `_SUITE.erl`，不参与 eunit） |
| 生成器函数 | 6698 | `^[a-z_0-9]+_test_\(\) ->`（每个展开 ≥1 用例） |
| 简单测试函数 | 1765 | `^[a-z_0-9]+_test\(\) ->`（每个 = 1 用例） |
| 测试入口函数合计 | 8463 | 上述两项之和 |
| **预计用例总数** | **≈11080–11150** | 历史全量实跑 Passed 11078 + Failed 4（W5-010 第二轮，2026-10-11）为基数；当前树含 C09/C10 套件改动小幅浮动 |

### 1.2 按目录分布（文件数 / 测试入口函数数）

| 目录 | 文件 | 入口函数 | 主要风格 |
|---|---|---|---|
| test/logic | 223 | 2435 | generator 为主（2097 gen + 338 simple） |
| test/lib | 138 | 1849 | generator 为主（1434 + 415） |
| test/repo | 126 | 1103 | generator 为主（967 + 136） |
| test/api | 123 | 866 | generator 为主（745 + 121） |
| test/features | 110 | 540 | simple 为主（437 simple + 103 gen） |
| test/ds | 81 | 997 | generator 为主（903 + 94） |
| test/adm | 37 | 385 | generator 为主（367 + 18） |
| test/integration | 30 | 82 | 混合 |
| test/domain | 9 | 100 | simple 为主（99 + 1，零 mock 值对象测试） |
| test/mcp | 6 | 33 | 混合 |
| test/stress / spike / rest / performance | 8 | 24 | 少量 |
| 顶层（test/ 根） | 6 | — | e2ee_safety_contract、enterprise_oa_sso 等 |

最大套件（入口函数 ≥40）：`channel_logic_tests`（204）、`channel_handler_tests`（91）、
`elib_oss_tests`（71）、`elib_tsid_tests`（67，**已 Excl 排除出全量**）、
`eb_conversation_tests`（63）、`imboy_policy_tests`（59）、`elib_pg_tests`（56）、
`msg_store_repo_tests`（53）、`adm_group_handler_tests`（52）。

### 1.3 依赖特征分类（grep 全量 898 文件）

| 特征 | 文件数 | 机制（只读理解） |
|---|---|---|
| `?TEST_WITH_APP` | 27 | `include/eunit_setup.hrl` → `eunit_runner:eunit_setup/0` 真启动 imboy app：**需绑定 HTTP/adm 端口（经 eunit-local 替换为私有端口）+ PG 可达 + TLS 密钥文件**。app 起不来即整段连锁 cancel |
| `?TEST_WITH_DB` / `?TEST_WITH_CONN` / `?TEST_WITH_DB_TIMEOUT` | 63 | `eunit_setup_with_db/0` 真连 PG（经 relay）；DB 不可用时 skip（返回 `[]`），可用时真跑 |
| meck（含 meck_helper 系宏） | 422 | mock 型，环境依赖最低；`WITH_MECKS` 已补幂等 boot（eunit_setup.hrl:225-258） |
| `inttest_marker_db` | 46 | 集成测试 PG 标记 fixture（test/common/inttest_marker_db.erl），`<PREFIX>_PG_*` env 指向 relay 端口 |
| `ewh_http_fixture` | 1 | `test/logic/enterprise_webhook_governance_pg_tests.erl`（唯一显式 HTTP fixture 用户） |
| 直接 `TEST_HTTP_PORT` env | 0 | 仅 `eunit_runner.erl:819` 内部读取（回落逻辑）；测试文件不直接用 |

### 1.4 全量轨道排除项（Makefile:150 Excl，仅无 t= 全量生效）

`elib_tsid`（VM 级 TSID 全局状态独占）、`agent_grant_pg` / `agent_run_pg` /
`agent_grant_pg_tests` / `agent_recovery_pg_tests` / `agent_run_pg_tests` /
`agent_tool_authorizer_pg_tests`（AG31 隔离库验收口径，无 env 必红）、
`cs_pg_widget` / `cs_seat_console_pg_tests`（对共享库跑 migrate down/up 往返，
与全量并发互踩）。这 9 项不参与全量，由专项单跑承载；**t= 单跑不受此过滤影响**。

---

## 2. 既有日志提取（最近一轮全量/分套件）

### 2.1 历史全量实跑（W5-010，2026-10-11，w1-w5-report-20261011.md）

| 轮次 | 树 | 结果 | 时长 | 失败定性 |
|---|---|---|---|---|
| 第一轮 | `f3b2bd9f` | Failed 6 / Passed 11076 | **33 分钟**（正常负载） | 6 例均为 W5-01 迁移波及面（mock/断言指旧 flush），修复 `5b304cba` |
| 第二轮 | `5b304cba` | Failed 4 / Passed 11078，**make exit 非零** | **2h10m**（并行会话极端负载） | 4 例全在 `eb_purge_port_tests`（SQLSTATE 23514 `trg_enterprise_message_purge_guard` 时间敏感断言跨边界）；隔离复跑 8/8 绿 → 负载型环境噪声 |

> 历史教训（review-ledger-v1.md #5）：Failed 4/Passed 11078/exit 非零 ≠ 全量 PASS，
> 必须新候选 exit=0 重跑——即本次 C12 的动机。

### 2.2 本会话分套件日志（~/.codex/cache-closeout-runs/.../logs/）

| 日志系列 | 套件 | 结果演变 | 定性 |
|---|---|---|---|
| sync_tests r1–r7 | `imboy_cache_sync_tests`（29 gens，26 WITH_APP） | r1 编译失败（`route_action/5 undefined`，当时 WIP）→ r2 F8/P15 → r3 F10/P15 → r4/r5 F3/P22 → r6 F1/P24 → **r7 F0/P25 仍伴 cancelled** | **已知阻断项**：`c04_round_requires_done_and_pages_test_/0`（test/lib/imboy_cache_sync_tests.erl:772）generator 在 eunit 迭代时 `{badmatch, <随机binary>}` 崩溃（eunit_data:iter_next 栈，r2–r7 全部出现）→ 计入 cancelled 而非 Failed。C09 修复在途；**只要它还崩，全量必有 cancel，而 C12 PASS 判据要求 cancel 归零** |
| account_ds_tests r1 | `account_ds_tests`（6 用例，3 WITH_APP） | **All 6 tests passed**（14:59 UTC） | 绿 |
| cache_adjacent r1 | t= 逗号列表误用 | `**'imboy_cache_tests,imboy_cache_policy_tests,imboy_cache_v2_tests,account_ds_tests'` → Failed 0 / Passed 0 / 全 cancel | **t= 不接受逗号列表的铁证**（见 §3.2） |
| qr_repo r1–r6 | `qr_login_session_repo_tests`（7 用例，全部 TEST_WITH_DB 走 relay 真 PG） | r1 F6（`{badmatch,{error,not_transitioned}}` ×6，C08 修复前）→ r2 F3 → r3/r4 F1 → r5 被外部 kill → **r6 All 7 passed** | 失败史属**候选缺陷型**（状态机断言），非环境 flaky；当前树绿 |
| dcheck r1–r17 | C09 三节点分布式检查（**非 eunit**，cache_v21_dcheck） | r11 17P/5F 383s；r17 19P/3F 102s | C09 未全绿；其节点名/cookie/负载与全量窗口**互斥**（resources.json 串行租约） |
| imboy_cache*_tests r1 | cache 三件套 | 日志截断无摘要行（22:58，伴随 cancel 现象） | 以 r7 sync 套件状态为准 |

### 2.3 单套件轮次时长参考

qr_repo 系列 r1→r6 文件 mtime 23:26→23:40（6 轮 / ~14 分钟）≈ **每轮 3 分钟**
（含增量编译 + relay 起停 + 7 用例 + 清理）。全量重编单独约 4–8 分钟
（Makefile:170 自述"一次约 4 分钟"，实测 800+ 模块）。

---

## 3. eunit-local 机制确认（Makefile:554-677 只读核实）

### 3.1 全量命令（确认无误）

```bash
cd /Users/leeyi/project/imboy.pub/imboy
EUNIT_CONFIG=config/sys.closeout-local \
EUNIT_RELAY_PORT=<p1> EUNIT_HTTP_PORT=<p2> EUNIT_HTTP_ADM_PORT=<p3> \
make eunit-local          # 不带 t= 即全量
```

机制链（Makefile:585-646）：
1. 入口硬复位：删 `.erlang.mk/imboy.test` 标记 + beam-cache + `ebin/imboy.app`
   → **每次强制全量 -DTEST=1 重编**（4–8 分钟）；
2. 生成 `$(EUNIT_CONFIG).config` 的端口替换副本（文件名含 $$PID 唯一，防并发覆写）：
   `sed s/$(EUNIT_RELAY_TARGET)/$(EUNIT_RELAY_PORT)/ + s/{http_port, 9800}/$(EUNIT_HTTP_PORT)/ + s/{http_port_adm, 9706}/$(EUNIT_HTTP_ADM_PORT)/`。
   **已核实 `config/sys.closeout-local.config` 内正是 `{http_port, 9800}` /
   `{http_port_adm, 9706}` / pg_conf→127.0.0.1:4323（库 `imboy_cache_closeout_v1`），
   sed 字面量全部命中**；
3. 起 PG relay：`python3 test/common/pg_relay.py <relay_port> 4323`（fork-per-connection，
   规避 macOS Docker backend 按进程楔死长命 VM 新建 TCP 的缺陷；父进程 accept 循环
   永不退出、子进程清握手 timeout 防 sock_closed 风暴）；
4. `ln -sfn . imboy`（worktree lib 位解析）→ 内层 `make eunit IMBOY_EUNIT_INNER=1`，
   `EUNIT_ERL_OPTS="-config <副本> -pa imboy/ebin -pa ebin -pa test"`；
5. rc 捕获后无条件清理：`rm -f imboy`、杀 relay 三连、删配置副本。

### 3.2 t= 机制与逗号列表禁令

- erlang.mk:3477 `ifdef t` 分支：`eunit.erl(['$(t)'])` —— **t 值整体作为单个 atom**。
  `t=a,b,c` 会拼出不存在的模块 atom `'a,b,c'` → test module not found →
  **Failed 0 / Passed 0 / 全部 cancelled 的静默假失败**（cache_adjacent_r1.log 实证，
  见 §2.2）。多套件只能逐个 t= 单跑，或走无 t= 全量轨道。
- `t=mod:fun` 可跑单函数（erlang.mk:3481）。
- t= 单跑**不经过 Excl 过滤**（Makefile:147 `ifdef t` 分流），被排除套件单跑照常。

### 3.3 端口互斥约束

- 默认 `EUNIT_RELAY_PORT=15432`、`EUNIT_HTTP_PORT=19980`、`EUNIT_HTTP_ADM_PORT=19970`
  （Makefile:651-658）。并发 eunit-local 若共用端口：后写方覆写 → 先写方 VM 读到错端口
  → eaddrinuse → app 起不来 → elib_tsid 未注册连锁失败 19 例（2026-10-04 实证）。
- 本 closeout 会话惯例已用 25xxx（relay）/ 19xxx+偏移（http/adm）段；
  全量窗口必须三端口全独占，且确认无其他会话在跑 eunit/dcheck/主树 imboy 实例
  （主树会间歇性绑 9800/9706，私有端口已规避，但需避开他人已占的私有端口）。
- 前置硬条件：本地 `imboy_cache_closeout_v1` 库（127.0.0.1:4323）schema 已应用最新迁移，
  否则 `imboy_migrate` 报 `{out_of_order, ...}` → app 启动失败 → TEST_WITH_APP 大面积 cancel。

---

## 4. 高风险套件与预检建议

| # | 风险项 | 类型 | 预检/处置建议 | 失败时区分方法 |
|---|---|---|---|---|
| R1 | `imboy_cache_sync_tests` c04 generator badmatch（:772） | **候选缺陷（已知在途）** | 全量前先 `t=imboy_cache_sync_tests` 单跑确认 r7 形态之上无新增 cancel；c04 崩溃未修复则全量必不满足"cancel 归零"，先修后跑 | badmatch 出现在 `eunit_data:iter_next` 栈 + "One or more tests were cancelled" 而 Failed=0 → generator 崩溃型 cancel，定位到具体 `_test_/0` 函数名即候选缺陷 |
| R2 | `eb_purge_port_tests` 时间敏感断言 | 环境（负载型） | 保持窗口静默（无并行会话满载）；失败时先隔离复跑 | SQLSTATE 23514 且隔离复跑 8/8 绿 → 负载型环境噪声（W5-010 先例），归外部归属；隔离复跑仍红才归候选 |
| R3 | TEST_WITH_APP 类 27 文件（含 cache_v2 11 用例、cache_sync 26 用例、account_ds 3 用例） | 环境（端口/PG） | 跑前 `lsof -i :<三端口>` 确认空闲；`pg_isready -p 4323`；确认 python3 可用（relay 缺失则回落直连，长跑有楔死风险） | eaddrinuse / econnrefused / `listener_start_failed` / `no_pool` / `missing_config` / TSID not_initialized 连锁 → 环境型（端口或 PG 断）；确定性断言 assert 失败 → 候选型 |
| R4 | 并发冲突（其他会话 eunit / C09 dcheck 三节点 / 主树 imboy） | 环境 | 全量与 `full_eunit`/`heavy_fault_stress` 租约互斥（resources.json）；跑前 `ps aux | grep -E "pg_relay\|cache_v21\|beam.smp"` 抽查；dcheck_r17 今晨 09:21 仍在跑，须协调 C09 停手 | 轮次时长异常拉长（33min→2h10m 先例）+ 时间敏感断言批量失败 → 并行负载型 |
| R5 | PG relay 链路（python3 fork 中继 / Docker fd / SSH 隧道占端口） | 环境 | 确认 4323 无隧道占用（`lsof -i :4323`）；历史 fd 危机（Docker VM 独占 51k fd）曾致全套件 badarg 假失败 | 全套件突发 badarg / invalid_password 而单套件复跑绿 → 环境症状（w1-w5-report 环境事件先例） |
| R6 | `qr_login_session_repo_tests`（TEST_WITH_DB 走 relay 真 PG） | 候选缺陷史（已绿） | r6 All 7 passed 为当前基线；全量中若再红，先 `t=` 单跑复现 | `{error,not_transitioned}` badmatch 复现 → 候选缺陷（C08 面回归）；连接类错误 → 环境 |
| R7 | 编译期失败（sync_tests_r1 `route_action/5 undefined` 教训） | 环境/树状态 | 全量前必跑 `IMBOYENV=local make compile`（c12-window-plan §6.1 步骤①已含）；冻结时工作树须干净 | ERLC 报 undefined function → 树未闭合，禁止进入全量 |
| R8 | Excl 9 套件误入（AG31 隔离库 env 检查 / migrate 往返互踩） | 轨道保证 | 全量轨道已由 Makefile:150 自动过滤，无需人工干预；**不要**为"补覆盖"手动加回 | 全量日志出现 agent_grant_pg_tests 等 cancelled → 检查是否误改 Excl |
| R9 | 机器态（uptime 长、load 5+，W5-07 已证同字节吞吐 -74%） | 环境 | 窗口前记录 uptime/load（c12-window-plan §6.0）；极端退化时性能窗口记 PARTIAL，但全量门禁以 exit/Failed 为准，时长拉长可容忍 | 与负载相关的只有时长与 R2 类断言；机械失败不受此影响 |

**外部归属 vs 候选缺陷通用判据**：
- 外部归属特征——端口/econnrefused/eaddrinuse、relay 断连、大面积 setup cancel
  （missing_config / no_pool / not_initialized 连锁）、时间敏感断言在长轮次失败且
  隔离复跑绿、fd/隧道型 badarg。
- 候选缺陷特征——确定性断言失败（badmatch 固定值如 not_transitioned）、同一用例
  隔离复跑（`t=<suite>`）仍红、cache/QR 域语义断言。
- 处置顺序（c12-window-plan §6.1）：全量失败 → 记 FAIL + 定向复跑取证，**隔离复跑
  绿不等于全量 PASS**；无关失败不擅自修复，保留证据链。

---

## 5. 全量窗口预估

| 项 | 预估 |
|---|---|
| 全量 -DTEST=1 重编（eunit-local 入口强制） | 4–8 分钟 |
| 全量测试运行（正常负载 / 静默窗口） | 30–40 分钟（W5-010 第一轮 33 分钟） |
| 全量测试运行（并行负载，需避免） | 可达 2h10m（W5-010 第二轮先例） |
| **建议窗口预算** | **1.5–2.5 小时**（含重编 + 正常轮次 + 日志归档余量）；超 45 分钟未出测试进度即查并行负载 |
| 预计参与套件 | 898 - 6（Excl 中 test 文件）= **892 个测试文件**（伴随约 800+ src 模块入口） |
| 预计用例总数 | **≈11080–11150**（基数：W5-010 Passed 11078 + Failed 4，加 C09/C10 套件在途增删浮动） |
| PASS 判据 | exit=0 且 `Failed: 0` 且 cancel=0 且 Skipped 与预期一致（c12-window-plan §6.1） |

---

## 6. 预检结论（一句话清单）

1. 全量命令形态确认无误：`EUNIT_CONFIG=config/sys.closeout-local EUNIT_RELAY_PORT=<p> EUNIT_HTTP_PORT=<p> EUNIT_HTTP_ADM_PORT=<p> make eunit-local`（无 t=）；三端口配置与 sed 替换字面量已逐项核实命中。
2. **最大单点风险是 R1**：`imboy_cache_sync_tests` c04 generator badmatch 未修复前，全量不可能满足 cancel 归零判据——建议 C12 窗口开启的硬前置之一。
3. t= 严禁逗号列表（静默假失败实证在案）；多套件定向只能逐个跑。
4. 正常负载下 40–50 分钟可完成；窗口预算 1.5–2.5 小时；与 C09 dcheck / 其他 eunit 会话严格互斥。
5. 失败分流有据可依：负载型（eb_purge_port_tests 先例）隔离复跑绿归环境；确定性 badmatch 归候选缺陷；两类都不得用隔离复跑替代全量 PASS。
