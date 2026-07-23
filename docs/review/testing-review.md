# IMBoy 三仓测试评审（Fact-based）

- **日期**：2026-07-22
- **方法**：只读评审。全部结论基于磁盘文件与 `文件:行号` 证据，不跑测试、不改源代码。
- **范围**：`imboy/`（Erlang 后端）、`imboyapp/`（Flutter）、`imboyadmin/`（React）+ 三仓 CI/lefthook 门禁。

---

## 1. imboy 后端（Erlang/OTP + EUnit/meck/CT）

### 1.1 覆盖现状（按测试文件计数，非行覆盖）

`find imboy/test -name "*_tests.erl"` 共 **404 个** EUnit 测试文件，另有 9+ 个 Common Test SUITE（`test/*_SUITE.erl`）与 `test/common/` 基础设施（`eunit_runner.erl`、`meck_helper.erl`、`cowboy_req_h.erl`）。

| 层 | 测试文件数 | src 模块数 | 文件级配比 |
|---|---|---|---|
| logic | 101 | 105 | ~96% |
| api (handler) | 60 | 66 | ~91% |
| repo | 63 | 91 | ~69% |
| ds | 50 | 89 | ~56% |
| lib | 73 | 70 | ~104% |
| adm | 27 | 33 | ~82% |
| domain / mcp | 9 / 5 | 9 / 8 | — |
| integration / performance / stress | 12 / 2 / 2 | — | — |

**P1 关键事实：目标覆盖率（Repo 80% / Logic 70% / Handler 60% / 整体 65%，见根 `CLAUDE.md` 测试策略表）目前无任何度量手段。** `imboy/Makefile` 中不存在任何 `cover`/`COVER` 目标（`grep -n "cover\|COVER" imboy/Makefile` 为空），CI（`.github/workflows/backend-ci.yml`）也未收集行覆盖。上表"文件级配比"不是行覆盖，目标达成与否**不可验证**——这是宣称与事实之间的缺口。

### 1.2 死测试/幻影测试问题（证据：`docs/analysis/dead-tests-census.md`）

普查结论（census:5）：**132 处死调用点、14 个 test 文件、约 16 个已被重构删除/改名的生产函数**，根因是生产重构后测试未同步。分类现状：

- **A 类（6 个 CT SUITE，旧 API 整体失效）**：已全部重写完成（census:10-23），且重写过程抓出 **5 个真生产 bug**（census:25-32）：群主转让恒失败（`group_logic:do_transfer/5`）、`group_log_repo:batch_add` INSERT 缺 VALUES 从未跑通、`group_ds:dissolve_group` 吞 `with_tx` 返回值恒报成功、离线 C2C 撤回必 case_clause 崩溃（`msg_c2c_ds:revoke_offline_msg/9` spec 与实现不符）、改密自 E2EE-013 起 undef 崩溃。
- **B 类（原 7 个 eunit 死测试文件，冻结基线复核后仅 3/7 仍残留，其余 4 个已删）**：仍存在的 3 个 = `test/ds/adm_user_ds_tests.erl`、`test/performance/db_query_performance_tests.erl`、`test/performance/msg_send_performance_tests.erl`；已删除的 4 个 = `fts_logic_tests_simple.erl`、`imboy_cache_sync_tests_simple.erl`、`group_notice_integration_tests.erl`、`websocket_performance_tests.erl`（census:34-46 名单已部分清理）。注意：**performance 目录实为 4 个文件**（`channel_perf_benchmark.erl`、`channel_ws_push_benchmark.erl` 两个 benchmark **非死**，另加 `db_query_performance_tests`、`msg_send_performance_tests` 两个死测试），并非"3 个全死"。**风险 P2**。
- **C 类（160 个 handler 层 mock 漂移失败）**：DB 环境门解除后暴露（此前 `?TEST_WITH_APP` 用例一律 setup cancelled 从未真跑，census:57-66），已 160/160 全清（census:70-79）。
- **D 类（4 个预存断言漂移）**：已修（census:84-95）。

### 1.3 反模式

1. **mock 掉协议/存储边界 → 抓不住契约错误（P1）**。census:25-32 的 5 个真 bug 均未被 404 个单测中的任何一个发现——"离线/转让/改密路径要么被 mock 走在线分支跳过，要么无对应单测"（census:32）。典型如 `msg_c2c_ds:revoke_offline_msg/9` spec 声明 `ok|{error}` 实返 `{ok,N}`：单测 mock 了 DS 层，只有对真 PG 的 CT 才暴露。经验教训与 `docs/analysis/`（messaging_flow 轨道）一致：**mock 掉协议边界的测试验证的是 mock 自身**。
2. **`?TEST_WITH_APP` 环境门静默取消（P1，已缓解但机制仍在）**。`include/eunit_setup.hrl:27` 定义的 DB 门宏，在无 DB 环境下 setup cancelled 而非 fail，导致 160 个失败被掩盖数月（census:57-66）。当前 CI full-eunit 有真 DB 前基线仍靠 continue-on-error（见 §4），同类掩盖可再发生。
3. **meck history 三元组坑**。`meck:history/1` 返回 `{Pid, {M,F,Args}, Result}` 三元组；正确写法见 `test/logic/e2ee_trust_logic_tests.erl:104`（`[{_, {_, _, Args}, _}] = meck:history(...)`）。写成四元组会 badmatch，属团队已知踩坑点，建议收进测试规范。
4. **模块重名陷阱**：`test/common/eunit_runner.erl` 与 `src/lib/eunit_runner.erl` 曾重名（census:51），快 harness 增量编译时 helper beam 缺失会造成大面积 `context setup failed` 假象（census:80-82）。CI 已加 `scripts/check_duplicate_modules.sh` 门（backend-ci.yml "Check duplicate module names" step）。

### 1.4 缺口（后端）

| 缺口 | 证据 | 风险 |
|---|---|---|
| 行覆盖率零度量，目标不可验证 | Makefile 无 cover 目标；CI 无覆盖收集 | P1 |
| ds 层文件级配比最低（50/89） | test/ds vs src/ds 计数 | P2 |
| 性能测试 4 文件中 2 个为死测试（db_query/msg_send；2 个 benchmark 非死） | census:42-44 | P2 |
| B 类死测试文件 3/7 仍残留污染 `make eunit` 全量（4/7 已删） | census:34-46（"授权后可批量清理"部分执行） | P2 |
| liveRoom 功能四层齐全但**零测试**（`grep -rl live_room imboy/test` 为空），且已挂在生产路由 `imboy_router.erl` | src/{api,logic,ds,repo}/live_room_* 存在 | P2 |

---

## 2. imboyapp（Flutter）

### 2.1 覆盖现状

- `test/` 下 **433 个** `*_test.dart`（按 api/component/page/service/store/widget/integration/smoke 等分目录）；`integration_test/` 下 **23 个** dart 文件（auth/channel/chat/contact/mine/flows/smoke + `e2e_chat_test.dart`、`sqlcipher_migration_test.dart`）。
- 无头 widget 与异步页不兼容问题已有工程化解法：`test/smoke/` 从 CI 单测 job 排除（`imboyapp/.github/workflows/ci.yml:57-59` 注释明示"共享 isolate 会级联"），由**进程隔离 runner** 专职运行（`imboyapp/.github/workflows/ci.yml:277-298` `route-smoke-test` job → `test/smoke/run_smoke_isolated.sh`，脚本在盘确认存在）。这是对"路由烟雾测试固有限制"的正确落地。

### 2.2 缺口与问题

1. **覆盖率只查文件存在、无阈值（P1）**：imboyapp 的 ci.yml analyze job 中 "Check code coverage" 步骤实际内容是 `flutter test --coverage && test -f coverage/lcov.info`——只验证 lcov 文件生成，**不设任何百分比门**；integration 覆盖只上传 Codecov（`imboyapp/.github/workflows/ci.yml:120-126`），同样无阈值门。80% 目标同后端一样不可验证。
2. **`integration_test.yml` 是坏死工作流（P2）**：该 workflow 的 paths 过滤为 `imboyapp/**`、`imboy/**`、`script/test.env`，job 设 `working-directory: imboyapp`（integration_test.yml:5-9,32-34）——但它位于 imboyapp 仓库**内部**，仓内不存在 `imboyapp/` 子目录（`ls imboyapp/imboyapp` 不存在），`script/test.env` 也不存在；且 `FLUTTER_VERSION: '3.29.3'` 与 ci.yml 的 `3.41.0` 不一致。此文件疑似从 umbrella 工作区拷贝而来，push 触发永不匹配 paths、手动触发必失败。应删除或改写。
3. **模块边界检查是软门**：`imboyapp/.github/workflows/ci.yml:48-50` 的 `dart scripts/check_boundaries.dart` 带 `continue-on-error: true`（注释"基线期，收集数据；归零后删此行"）。
4. **真机验收依赖（P2，结构性）**：项目规范要求功能验证用真机（根 CLAUDE.md"Flutter 调试必须使用真机"），E2EE/WebRTC/附件上传等关键流的最终验收无法进 CI，历史上多个真 bug（撤回 ack UI、图片渲染链）只在真机暴露。CI 能给的只是 widget/route 级信心。

### 2.3 门禁（lefthook）

`imboyapp/lefthook.yml`：dart-fmt（--set-exit-if-changed）、dart-analyze、gitleaks、**design-tokens 硬门**（对 staged diff 的新增行 grep 禁 `Color(0x`/`fontSize:`/`Colors.*`，Colors.transparent 豁免）。design-tokens 门只拦增量，存量 2099 处硬编码颜色不受管（见代码质量评审）。

---

## 3. imboyadmin（React + bun）

### 3.1 覆盖现状

- 单测：`src/**` 下 **119 个** `*.test.ts(x)`（bun test）。最大测试文件 `src/components/shared/shared.test.tsx`（932 行）、`src/services/api/payloadServices.test.ts`（657 行）。
- E2E：`tests/e2e/` 下 **9 个** Playwright spec（login-and-dashboard / user-management / admin-rbac / channel-messages / group-task / report-center / setup-flow / prod-health-check / login-comprehensive），`playwright.config.ts` 在仓。

### 3.2 缺口

1. **Playwright E2E 完全不在 CI（P1）**：`grep -n "playwright\|e2e" imboyadmin/.github/workflows/*.yml` 零命中。`imboyadmin/.github/workflows/ci.yml` 的 lint-and-build job 只跑 `bun run lint` + `bun run test` + 两个回归 gate（channel/moment 定点 bun test，`imboyadmin/.github/workflows/ci.yml:35-48`）+ `bun run build`。9 个 E2E spec 只能手动 `bun run test:e2e`，等价于无回归保障。
2. **typecheck 与 knip 在 quality.yml（非 ci.yml）**：更正——imboyadmin 有独立 `quality.yml` 质量门，跑 `typecheck`（ratchet baseline 31 errors）+ `deadcode`/knip（ratchet baseline 35 findings）+ ESLint（37）+ gitleaks（40）；`ci.yml` 只跑 lint/test/build。原"typecheck/knip 完全不跑"系只 grep ci.yml 漏看 quality.yml 所致。
3. **后端就绪门是条件跳过**：`IMBOY_ADMIN_BASE_URL` 未配置时直接 skip（`imboyadmin/.github/workflows/ci.yml:50-52`），默认分支上该门形同虚设。

---

## 4. CI 门禁综合评估

### imboy（backend-ci.yml + quality.yml）

| 门 | 状态 | 证据 |
|---|---|---|
| Compile（OTP 28）+ 重名模块检查 + zero-crypto 检查 | **阻塞** | backend-ci.yml compile job |
| Moment 定点 EUnit | **阻塞** | moment-eunit job |
| 全量 EUnit | `continue-on-error: true`（基线收集，注释明示"收紧计划：改 ratchet"） | full-eunit job |
| Dialyzer | `continue-on-error: true` | dialyze job |
| Xref undefined-calls | **阻塞 ratchet=0**（2026-06-13 从 42 全修） | xref job "Enforce zero undefined calls" |
| elvis lint / gitleaks / redocly 契约 | ratchet（elvis 8824 上限 / gitleaks 0 / redocly 4E+9W） | quality.yml 头部注释 |
| DCO | 仅 PR | dco-check job |

**评价**：xref=0 硬门 + 重名模块门是对"死测试/幻影"类问题的正确结构性防御（xref 管 src，census 管 test）。最大剩余风险是**全量 eunit 仍非阻塞**——census C/D 类已全清、13 模块全绿（census:79），基线已具备，`continue-on-error` 应按注释中的计划收紧为 ratchet，否则新的 mock 漂移会重新静默积累。**P1**。

### imboyapp / imboyadmin

- app：单测/回归 gate/analyze/format 均阻塞，覆盖率无阈值（见 §2.2.1），边界检查软门，integration_test.yml 坏死。
- admin：ci.yml lint+单测+build 阻塞；typecheck/knip 在独立 quality.yml（ratchet 31/35）；仅 Playwright E2E 缺席 CI（见 §3.2）。

### lefthook（三仓）

- imboy：erlfmt --check + gitleaks + Conventional Commits（纯 shell）。**无本地 DCO 检查**（`scripts/check_dco.sh` 存在但未挂钩），DCO 只在 CI PR 层拦，push 到分支后才发现要重写历史。P3。
- imboyapp：fmt/analyze/gitleaks/design-tokens（见 §2.3）。
- imboyadmin：eslint --max-warnings=0 + gitleaks。无 commit-msg 规范门（与另两仓不一致）。P3。

---

## 5. 测试策略缺口（跨仓）

1. **契约测试单薄（P1）**：census 5 个真 bug 全部是"层间契约"错误（spec vs 实现、meck 期望 vs 生产参数）。现有防御=redocly OpenAPI 门（HTTP 层）+ 重写后的 6 个 CT SUITE（真 PG）。WS 协议契约（`docs/analysis/ws-protocol-contract.md`）无自动化契约测试对齐三端；SDK（imboy-sdk-js，vitest）与后端之间无契约验证。
2. **E2E 关键流**：app 侧 `integration_test/e2e_chat_test.dart` 等 23 个文件在 ci.yml test job 跑（headless），但真正端到端（真后端+真机）流程只有手动冒烟（`imboy/scripts/smoke/`、`make ctl ARGS="smoke all"`）；admin 侧 E2E 不进 CI。
3. **覆盖率度量三仓均缺阈值门**：后端零度量、app 只查文件存在、admin bun test 无 coverage 配置进 CI。所有书面覆盖率目标当前均为无据宣称。

---

## 6. 汇总表

| # | 发现 | 仓 | 风险 | 证据 |
|---|---|---|---|---|
| 1 | 覆盖率目标（80/70/60/65）零度量手段，不可验证 | imboy | P1 | Makefile 无 cover；backend-ci.yml 无覆盖收集 |
| 2 | 全量 EUnit 仍 continue-on-error，mock 漂移可再静默积累 | imboy | P1 | backend-ci.yml full-eunit job |
| 3 | Playwright 9 个 E2E spec 完全不进 CI | imboyadmin | P1 | tests/e2e/*.spec.ts；workflows 零 playwright 引用 |
| 4 | mock 协议/存储边界反模式：5 个真生产 bug 未被 404 个单测发现 | imboy | P1 | docs/analysis/dead-tests-census.md:25-32 |
| 5 | Flutter 覆盖率门只验证 lcov 文件存在，无阈值 | imboyapp | P1 | .github/workflows/ci.yml analyze job |
| 6 | B 类死测试文件 3/7 仍残留（4/7 已删；performance 目录 4 文件中 2 死 2 benchmark 非死） | imboy | P2 | dead-tests-census.md:34-46 |
| 7 | integration_test.yml 坏死工作流（paths/working-directory 指向不存在目录） | imboyapp | P2 | integration_test.yml:5-9,32-34 |
| 8 | liveRoom 四层实现挂生产路由但零测试 | imboy | P2 | src/*/live_room_*; test 零引用 |
| 9 | ds 层测试文件配比最低（50/89） | imboy | P2 | test/ds vs src/ds 计数 |
| 10 | 真机验收流程无法自动化，关键流最终信心靠手动 | imboyapp | P2 | 根 CLAUDE.md 真机规范；ci.yml headless 限制 |
| 11 | 模块边界检查 continue-on-error 软门 | imboyapp | P3 | imboyapp/.github/workflows/ci.yml:48-50 |
| 12 | admin typecheck/knip 在 quality.yml（ratchet 31/35），Playwright E2E 与后端就绪门不在 CI 主路径 | imboyadmin | P3 | quality.yml；ci.yml |
| 13 | DCO 无本地钩子（imboy）；admin 无 commit-msg 门 | imboy/admin | P3 | lefthook.yml 三仓对比 |
