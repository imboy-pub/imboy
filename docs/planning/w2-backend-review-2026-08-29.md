# W2 后端全量与安全审查报告（ZC-09 独立 Review）

> **Reviewer**：ZC-09 独立 Review Agent（未参与 W2 实现，全新视角）
> **审查日期**：2026-08-29
> **Base SHA**：`db41789aed09dc9edaf14ad015c35fa800229579`（main）
> **审查对象**：工作树全部 W2 产物（git status 39 项 = 5 M + 34 ??）
> **约束遵守**：除本报告文件外**零修改**（未改代码/测试，未 commit/reset/clean；仅删除 gitignored 构建产物 `ebin/`、`.erlang.mk/beam-cache` 以恢复可信构建态，并在 `/tmp` 建过一次性 main worktree 用于基线验证，已清理；`git status --short` 前后恒为 39 项）

---

## 0. 总判定：**FAIL**（CRITICAL=0，HIGH=1）

| 级别 | 数量 | 摘要 |
|---|---|---|
| CRITICAL | 0 | — |
| **HIGH** | **1** | **H-1：W2 ZC-03/04 三个 meck 测试套件空转判绿（67 用例从未执行），交付证据失真，台账"三卡均已规避"的声明与事实不符** |
| MEDIUM | 7 | M-1 ~ M-7（含既有空转基建隐患 M-6，只审计不修复） |
| LOW | 8 | L-1 ~ L-8 |

按验收标准"CRITICAL/HIGH = 0 否则 FAIL"，本轮总判 **FAIL**，阻断 Release 卡。
**精确缺口**：将 H-1 所列 3 个套件（67 用例）改造为 ZC-02 的 `?WITH_MECK_TESTS` 规范形态并真实重跑（修复范式现成，见 M-6 之外的可直接派卡项）；MEDIUM 各项可由总控排卡或书面豁免。

---

## 1. 分级发现清单

### HIGH

#### H-1：ZC-03/04 meck 测试套件空转判绿（67 用例从未执行）

- **文件**：
  - `test/logic/project_channel_logic_tests.erl`（34 用例，如 `write_permission_test_/0` L85 起全部元素）
  - `test/logic/project_milestone_logic_tests.erl`（19 用例，如 `create_validation_test_/0` L20 起）
  - `test/ds/project_milestone_ds_tests.erl`（14 用例，如 `create_ok_writes_event_same_conn_test_/0` L133 起）
- **问题**：三套件采用 `{"desc", fun() -> ?WITH_MECKS(...) end}` 包装形态。`?WITH_MECKS`（`include/eunit_setup.hrl:225`）展开为 EUnit `{setup, S, C, I}` **描述结构**；该结构出现在普通测试 fun 的**返回值**位置时，EUnit 不再将其作为测试描述解释——mock 不建立、断言不执行，用例直接判绿。
- **实证**（独立最小探针，`/tmp` 内完成，未触碰仓库）：generator 返回 `[{Desc, fun() -> Setup结构 end}]` 时，即使内层断言为 `?assert(false)` 也输出 `ok`；对照形态（generator 直接返回 setup 结构）则正确报 failed。铁证空转。
- **影响**：
  1. ZC-03/04 验收证据"55/47 用例全绿"中 67 个用例是空转数字；权限矩阵（guest 403/非成员 403/owner 判定）、"无 ds 调用"断言（`meck:num_calls`）、事件原子性 mock 断言等从未真实运行；
  2. 执行台账（`channel-firstclass-w2-execution-ledger.md` ZC-02 节）声称"本轮三卡改用规范 context 形式规避"，与事实不符——**仅 ZC-02 规避**（member 三套件 46 用例用本地 `?WITH_MECK_TESTS` 宏，verbose 实测描述名逐个出现、真实执行）；
  3. 全量 6481 绿灯中混入无效用例，回归保护网存在已知空洞。
- **缓冲因素**（不降级，仅说明影响面）：ZC-03/04 的核心行为有真库测试真实兜底——milestone integration 7 + repo 5 + schema contract 11、channel rel 8 + agg 5（`?TEST_WITH_DB_TIMEOUT`/`?TEST_WITH_CONN` 形式，全部真实执行并通过）；handler 测试 10 用例为 direct generator 形态（探针证明真实执行）。
- **修复建议**：三套件改造为 ZC-02 `?WITH_MECK_TESTS(MockConfigs, Tests)` 形态（generator 尾部直接返回 setup 结构）；改造后先用注入必败断言的探针验证用例可失败，再恢复断言重跑。可由总控直接派一张测试修复卡。

### MEDIUM

#### M-1：channel 权限 owner 分支不查 workspace active（跨模块不一致 + 防御纵深缺口）

- **文件**：`src/logic/project_channel_logic.erl:232-249`（`ensure_can_read/2` 的 `OwnerId =:= Uid -> {ok, Project}` 直通分支）
- **问题**：project owner 读关联列表/四聚合时**不校验其 workspace_member 是否 active**。对照：
  - `project_member_logic.erl:168-186`：经 `workspace_logic:my_role/2`（只放行 `status='active'`）；
  - `project_milestone_logic.erl:144-154` → `project_logic:detail/2` → `workspace_logic:ensure_member/2`（同上）；
  - 同模块写路径 `ensure_can_write/2`（L255-269）owner 也要求 active（注释自述"Owner 的 ws 身份失效时 fail-closed"）。
- **现实可利用性**：低。DB 三重防线使"失效 owner"在正常 API 流程下不可达：`trg_workspace_member_remove_guard_pm`（有 active pm 不得移除 wm）+ `remove()` 对 owner 409 + `fk_project_owner_membership ON DELETE CASCADE`（物理删 wm 连带删 project）。需人工 DB 治理（软删 wm 且先直改 pm）才能构造。
- **建议**：owner 分支补 active ws member 检查（或统一走 `my_role`）；并与台账已上报的"ws_owner 直通读差异"合并为一个权限语义统一决策项由用户拍板。

#### M-2：milestone 并发 reach 双写竞态（reached_at 覆盖 + 重复事件）

- **文件**：`src/ds/project_milestone_ds.erl:184-240`；`src/repo/project_milestone_repo.erl:102-106`（`update_fields_tx` 的 WHERE 仅 `id = $1`）
- **问题**：并发 reach 在 READ COMMITTED 下：T1、T2 都读到 `planned`；T1 提交后 T2 的 UPDATE 仍执行（无 `status='planned'` 守卫）→ `reached_at` 被覆盖为第二时间、`milestone_reached` 事件写两条。"重复 reach 幂等、不重复写事件"契约仅在串行下成立。
- **建议**：UPDATE 增加 `AND status = 'planned'`，`{ok, 0}` 分支按 `already_reached` 处理（不写事件）。

#### M-3：member invite/remove 缺事务内 actor 复检（与 transfer_owner 防御标准不一致）

- **文件**：`src/ds/project_member_ds.erl:50-79`（invite）、`:85-108`（remove）对照 `:220-226`（transfer 有 `ensure_transfer_actor_tx/2`）
- **问题**：actor 权限仅 logic 层前置，DS 事务内不复检，存在理论 check-act 窗口（owner 被转移/治理方身份变化后，旧身份的写仍可落库）。transfer_owner 已做事务内复检，同模块标准不一。台账 ZC-02"残余风险②"已自认该窗口，未消除。
- **建议**：事务内补 actor 身份复检（owner 判定读 `project.owner_id`，治理权判定读 ws role/status），成本极低。

#### M-4：repo 层查询错误被吞为"无记录/0"（静默失败，语义漂移）

- **文件**：
  - `src/repo/project_member_repo.erl:80-83`（`find/3` 的 `_ -> #{}`）、`:95-101`（`find_tx/4` 同）、`:128-132`（COUNT 失败当 0）
  - `src/repo/project_milestone_repo.erl:70-77`（`find_tx/3` 同）
  - `src/repo/project_channel_rel_repo.erl:69-76`（`find_channel_tx/3` 同）
- **影响**：DB 故障时权限判定退化为 403（方向 fail-closed，安全）、分页 total=0、`find_tx` 空 map 使调用方误走 INSERT 分支（由 FK/触发器兜底）。均无告警日志，排障困难。
- **建议**：区分 `{error, Reason}` 与空结果，error 路径至少 `?ERROR_LOG` 并上抛 500 语义。

#### M-5：DS 写操作"提交后读失败"将成功写报成失败（create 非幂等，重试致重复）

- **文件**：`src/ds/project_milestone_ds.erl:82-88`（create 提交后 `find_by_id/1`）、`:173-178`（update）、`:234-240`（reach）
- **问题**：`with_tx` 提交后的事务外回读失败（连接池抖动）会令 create/update/reach 返回 `{error,…}` → logic 归一 500；实际行已提交。create 不幂等，客户端按 500 重试将产生重复里程碑（name 无唯一约束）。
- **建议**：回读数据改在事务内取（member ds 同款风格），或 create 的失败路径显式区分"已创建"。

#### M-6：空转模式既有波及面（W0/W1 存量，只审计不修复）

- **清单**（wrapped 空转形态用例数，启发式扫描）：`test/logic/friend_logic_tests.erl` 30、`test/lib/workspace_resolver_tests.erl` 17、`test/logic/project_logic_tests.erl` 16（W0）、`test/logic/project_task_logic_tests.erl` 13（W0）、`test/lib/workspace_guard_tests.erl` 12、`test/logic/websocket_logic_tests.erl` 11、`test/logic/workspace_archive_tests.erl` 10、`test/lib/imboy_cache_sync_tests.erl` 7、`test/ds/workspace_archive_closure_tests.erl` 4、`test/logic/e2ee_logic_tests.erl` 4、`test/lib/imboy_cluster_tests.erl` 2 —— **合计约 126 用例**。
- 加上 H-1 的 W2 三套件 67 用例，全仓 wrapped 空转约 **193 用例**。
- **建议**：独立基建卡，统一迁移到 `?WITH_MECK_TESTS` 形态；迁移后以必败探针抽样验证。

#### M-7：milestone admin_page 的 total 语义失真

- **文件**：`src/logic/project_milestone_logic.erl:46-63`（`Total = length(Rows)` 即当前页行数，`total_page` 恒 ≤1）
- **影响**：admin 分页控件按 total 计算页数会失真（注释已声明妥协）。`project_channel_logic:admin_aggregation/4` 对 resources/related_posts 无分页语义的同型包装可接受。
- **建议**：补独立 COUNT 查询（表小，代价可忽略）。

### LOW

| # | 位置 | 问题 | 建议 |
|---|---|---|---|
| L-1 | logic/repo 各分页入口 | Page 无上限（`elib_param:pase_page_size` 只钳 Size），OFFSET 可被放大；因 WHERE 均走 project_id 前缀索引，实际代价有界 | Page 加上限（如 ≤1000） |
| L-2 | `src/ds/project_channel_ds.erl:178-190` | resources 对不存在 project 返回 `{ok, []}` 而非 404（admin 直连路径语义漂移；logic 前置已 404） | 未命中返回 404 或维持现状并注释 |
| L-3 | `src/repo/project_channel_agg_repo.erl:46-51/84-93` | pinned/related_posts 不过滤 `channel.status`，禁用频道的存量置顶/帖子仍出现在聚合 | 视产品语义补 JOIN 过滤 |
| L-4 | `src/ds/project_member_ds.erl:138-141` | owner 已失效的存量 project 执行 transfer_owner 时旧 owner upsert 触发 23514 → 500（应为业务错误码）；此类 project 属"留待人工治理"数据 | 前置校验 owner 的 ws 有效性，400/409 明示 |
| L-5 | `src/ds/project_channel_ds.erl:140-142` | `update_fields_tx` 的 `{ok, 0}` 空写未处理（project 并发删除窗口；事件 INSERT 由 FK 23503 兜底 abort，无孤儿事件，但错误语义为 500） | `{ok,0}` → 404 abort |
| L-6 | `project_member_ds:transfer_owner`（56 行）、`project_milestone_ds:update`（70）、`reach`（64） | 函数超 50 行规范 | 拆分内部步骤 |
| L-7 | 各 page_by_project/agg 分页 | COUNT 与 DATA 两条 SQL 非同一快照，并发写下 total 短暂偏差 | 接受（惯例）或同事务 |
| L-8 | `src/logic/project_channel_logic.erl:333-336` | links 的 url 无 scheme 白名单（`javascript:` 等可入库，渲染面自担） | 视需要限定 http/https |

---

## 2. 九个审查维度逐项结论

1. **授权专项**：权限链完整度良好——全部 19 条新路由位于 `/api/v1/*` JWT 默认门（`auth_middleware` 免鉴权白名单不含任何新路径），Admin 4 条走 `adm_acl:ensure_permission(<<"workspaces:read">>)` fail-closed（`src/adm/adm_workspace_handler.erl` 4 个 action 均先 ACL 后业务）。水平越权（直接 ID 打资源）：非成员读/写均 403、guest 写 403（三 logic + DS `ensure_writer_tx` 双层）、归档 980 全写面收口。垂直越权：非 active ws member 全部 403（`my_role` 校验 status）。**发现 M-1**（channel owner 分支不查 ws active，正常流程被 DB 三重防线封死，现实可利用性低）与**已知差异**（member logic 允许 ws_owner 直通读成员列表、milestone/channel 不允许——台账已上报用户拍板；评估为语义一致性问题而非越权，因 ws_owner 是现任 active 治理者，无数据泄露给无权方）。
2. **SQL 专项**：全部值参数化（`$n`）；`find/3` 的 Column 参数调用链核查为**代码内字面量常量**（logic/ds/handler 均无用户输入汇入）；表名经 `elib_pg_sql:public_tablename/1` 常量；NOTICE 黑名单为字面量内联（注释声明）。分页 Size 双层钳制（repo 100/DS 50），Page 无上限（L-1）。EXPLAIN 抽查五类模式全部走索引（详见 §5），pinned 聚合空表统计下暂走 Seq Scan、真实规模验证走 `i_channel_message_pinned` 部分索引。**注入面：未发现。**
3. **事务专项**：业务写与 `project_event` 同 Conn 同事务（member/milestone/channel 三 DS 一致，集成测试含回滚无孤儿断言，真库通过）；Owner 自动入项目 `ensure_owner_member_tx/4` 幂等 upsert、失败 `abort_tx` 连带建项目回滚（接线于 `project_ds:create/4` diff 确认）；归档守卫 `workspace_guard:ensure_writable_tx` 覆盖全部写端点（member 2 / milestone 6 / channel 2 处，台账核查与代码抽查一致）；可延迟触发器 COMMIT 校验由 `SET CONSTRAINTS ALL IMMEDIATE` 测试实证（w2 schema contract 11/11）。**发现 M-2（并发 reach 竞态）、M-5（提交后读非原子返回）、M-3（invite/remove 无事务内复检）。**
4. **静默失败专项**：**发现 M-4**（repo `_ -> #{}`/COUNT→0 吞错）、M-5、L-2、L-5。`{ok,0}` 类空写在 member remove/unlink 为有意幂等语义（注释明确），channel link `{ok,0}`=existing 幂等正确；map 空匹配 fallback（`#{}`）在所有 DS 调用点均有 `maps:get(..., undefined)` + 404 兜底，无误判为成功的路径（除 M-4 所述错误被吞为空 map 的场景）。错误码透传/归一约定三卡一致（`{Code,Msg} when is_integer(Code)` 透传，其余 500 + ERROR_LOG）。
5. **代码质量**：四层边界 Handler→Logic→DS→Repo 单向合规（logic→repo 直调仅 admin 只读面，与 `workspace_logic` 既有惯例一致，`scripts/check_module_boundaries.sh` PASS）；13 个新文件全部 <800 行（最大 361），3 个函数略超 50 行（L-6）；TSID：member/rel 无 id 列不产 TSID、milestone 由应用层 `elib_tsid:generate()` 产 bigint、JSON integer 下发符合仓库规范（前端 `safeParseBigIntJson` 责任面）；文案为中文硬编码字面量，与仓内既有风格一致（仓库无 i18n 机制，非 W2 引入问题）。
6. **迁移质量**：up/down 对称可逆（Phase 1-7 与逆序一一对应，`IF EXISTS/IF NOT EXISTS` 保证可重复执行）；回填正确（owner 为 active ws member 才回填 + ON CONFLICT DO NOTHING，34/34 孤儿 0）；与 00000077 触发器共存（`trg_workspace_member_remove_guard` 与 `..._pm` 同表同事件、独立命名互为互补，按字母序执行均 fail-closed）；锁预算：`channel ADD CONSTRAINT UNIQUE` 与 `project_event` CHECK 重建需 ACCESS EXCLUSIVE 全表扫描，alpha 规模毫秒级，生产执行需发布窗口（up 文件头注释"全部为空表/新增列"表述不完全准确——Phase 1/6 作用于存量表，已在台账 ZC-01 锁预算节正确披露）。
7. **测试空转专项**：见 H-1（W2 新增 3 套件 67 用例）与 M-6（W0/W1 存量 11 套件约 126 用例）。机制经独立最小探针实证；波及面逐文件扫描（14 文件 / 约 193 用例，含 W2）。健康对照组：member 家族（`?WITH_MECK_TESTS`）、全部真库套件（`?TEST_WITH_CONN`/`?TEST_WITH_DB_TIMEOUT`）、milestone_handler（direct generator）。
8. **全量测试**：见 §4 对账表。最终可信全量：**All 6481 tests passed**（exit 0，0 failed / 0 cancelled）。
9. **迁移演练复核**：**独立重做**（未复用 ZC-01 证据），见 §5。

---

## 3. 确认项（审查中验证为正确的关键点）

- 权限语义三处已知决策（台账 §4 H1 Scope）：unlink 404、link 幂等吸收、Guest 只读优先于 Project 所有权——代码与测试一致。
- 事件类型契约：三 DS 只写 `chk_project_event_type` 14 值内的 9 个 W2 值，无自造值（真库 CHECK 测试通过）。
- Activity payload 正文键清洗（content/message/body/text）双保险落地（logic 层 `strip_content_keys`，DS 列白名单不含 content）。
- Related Posts 有界（每频道 5/总量 50，单条窗口 SQL 无 N+1）；links ≤20 条/名称≤200/url≤2048 应用层防线 + DB 形状触发器兜底。
- `.contract/api_contract.json` +111 行（40 处 W2 端点/枚举相关），由 `make contract-export` 再生（台账 endpoints=630）。
- 禁区核对：`erlang.mk`、`ios/`、`macos/`、`plugin/r_upgrade` 零改动（git status 佐证）。
- ZC-08 演练产物（`scripts/demo/dual_exp_demo_b_w2.sh` + 两份文档）齐备，台账记录连续两遍 66/66 ALL PASS；本轮抽查脚本头部约定（唯一前缀 teardown、PGPASSWORD 注入）符合安全要求。

---

## 4. 全量测试数字与基线对账

**最终可信全量（本审查第六轮，干净构建态）**：`All 6481 tests passed.`，exit 0，无 failed / 无 cancelled。

| 轮次 | 环境 | 结果 | 说明 |
|---|---|---|---|
| 基线（给定） | main 终态 | 6262 pass / 0 failed / 62 cancelled | 62 cancelled 为独占命名套件互踩，已知非失败 |
| 第 1 轮 | 主树，`make eunit-local`（ebin 含历史 release beam 污染） | 6470 pass / **11 failed** / 总 6481 | 11 个失败见下归因 |
| 第 6 轮 | 主树，清理 `ebin/` + beam-cache 后干净重建 | **6481 pass / 0 failed / exit 0** | 与第 1 轮总数恒等（6470+11=6481），交叉验证 |

**第 1 轮 11 个失败的归因（闭环）**：
- `adm_plugin_handler_tests` 8 个 + `read_raw_body_limit_tests` 3 个，全部 `error:undef`。
- 根因：两套件所测函数（`safe_plugin_name/1`、`is_lifecycle_mutation/1`、`read_raw_body/2`）均位于源码 `-ifdef(TEST).` 块；主树 `ebin/` 中这 3 个模块的 beam 曾被 `make app`（台账 ZC-05 验证含 `make app`）以**无 TEST 宏**编译覆盖，且 erlang.mk beam-cache 增量机制未重编。
- 验证：在 `/tmp` 以 `git worktree`（同 SHA db41789a）全新构建单跑两套件 → **8/8 与 3/3 全部通过**；主树清理构建缓存重建后全量 6481 全绿。
- **与 W2 代码零交集，非代码缺陷**。

**基线对账**：

| 项 | 基线 | 本轮 | 差 | 归因 |
|---|---|---|---|---|
| failed | 0 | 0 | 0 | 一致 |
| passed | 6262 | 6481 | +219 | W2 新增真实通过 **168**（逐套件实测，见下表）+ 口径差 **51**（基线口径含独占套件运行并产生 62 cancelled；当前口径 `EUNIT_TEST_SPEC` 排除 15 个独占套件，其用例不再计入） |
| cancelled | 62 | 0 | -62 | 同上（排除清单生效；本轮日志无任何 cancelled 测试） |

**W2 新增套件逐个实测（15 套件 168 用例）**：

| 套件 | 用例 | 形态 | 真实执行 |
|---|---|---|---|
| project_member_handler_tests | 9 | WITH_MECK_TESTS | ✅ |
| project_member_logic_tests | 23 | WITH_MECK_TESTS | ✅ |
| project_member_ds_tests | 14 | WITH_MECK_TESTS | ✅ |
| project_member_repo_tests | 5 | 真库 | ✅ |
| project_member_concurrency_tests | 4 | 真库并发 | ✅ |
| project_milestone_handler_tests | 10 | direct generator | ✅ |
| project_milestone_logic_tests | 19 | **wrapped** | ❌ 空转（H-1） |
| project_milestone_ds_tests | 14 | **wrapped** | ❌ 空转（H-1） |
| project_milestone_repo_tests | 5 | 真库 | ✅ |
| project_milestone_integration_tests | 7 | 真库 | ✅ |
| project_channel_logic_tests | 34 | **wrapped** | ❌ 空转（H-1） |
| project_channel_rel_integration_tests | 8 | 真库 | ✅ |
| project_channel_agg_integration_tests | 5 | 真库 | ✅ |
| w2_schema_contract_tests | 11 | 真库 | ✅ |
| w0_schema_contract_tests（换档） | 4 | 真库 | ✅ |
| **合计** | **168** | 真实执行 101 / 空转 67 | |

（"约 168"与计划口径完全吻合；其中 67 个绿灯为空转，即 **有效新增绿灯 101**。）

---

## 5. 迁移演练复核：独立重做（选择重做，未复用 ZC-01 证据）

- **方式**：确认 `imboy_v1` 无活动连接（除本审查 1 个）后，`CREATE DATABASE imboy_w2review TEMPLATE imboy_v1` 克隆；以 `psql -v ON_ERROR_STOP=1` 在显式 `BEGIN/COMMIT`（模拟 erlang_migrate 外层单事务）内执行 `00000081.down.sql` → 断言 W0 形态 → 同法执行 `00000081.up.sql` → 断言 W2 形态 + EXPLAIN 抽查 → `DROP DATABASE`。
- **down 结果**：三表删除 ✅、`project.links` 列删除 ✅、`uk_channel_id_workspace` 删除 ✅、`chk_project_event_type` 恢复 W0 五值 ✅、**project 34 行数据无损** ✅。
- **up 重放结果**：三表就位 ✅、links 列 ✅、8 个关键约束（4 PK/UNIQUE + 3 CHECK + 事件 CHECK）✅、3 个触发器 ✅、**回填不变式孤儿=0 且 owner 成员行 34/34** ✅。
- **EXPLAIN 抽查**（5 条代表查询）：
  1. 成员分页 → pkey 前缀 Bitmap/Index Scan ✅
  2. 成员身份校验 → `project_member_pkey` Index Scan ✅
  3. 里程碑分页 → `i_project_milestone_project_status` Index Only Scan ✅
  4. 频道反查 → `i_project_channel_rel_channel` Bitmap ✅
  5. pinned 聚合 JOIN `channel_message`：空库统计下暂为 Seq Scan（35 行表 planner 理性选择）；`SET enable_seqscan=off` 模拟真实规模 → 走 `i_channel_message_pinned` 部分索引 + rel 侧 Index Only probe ✅（生产大表无全表扫描风险；建议上线后对聚合路径做一次真实统计复核）。
- **结论**：down→up 双向演练通过，与 ZC-01 台账记录一致；锁预算结论（alpha 毫秒级、生产需发布窗口）维持台账判断。

---

## 6. 结论

- **产品代码**：W2 四层实现、SQL 参数化、事务原子性、DB 约束、迁移质量均达到放行水准；未发现可现实利用的越权（M-1 为低可及性一致性问题）。
- **测试有效性**：H-1 空转使 ZC-03/04 的 mock 层验收证据失真（67 用例空转），且台账声明与事实不符——按验收标准（CRITICAL/HIGH=0）本轮总判 **FAIL**。
- **放行路径建议**：派一张小卡将 3 个套件改造为 `?WITH_MECK_TESTS` 形态并真实重跑（预计改动局限测试文件，参照 ZC-02 范式），复核通过后即可转 PASS；MEDIUM 7 项由总控排卡或书面豁免（M-2 并发 reach 建议优先）。

---

## 附：审查过程凭证索引

- 全量日志：`/tmp/w2-review-eunit-full.log`（第 1 轮，11 failed 明细）、`/tmp/w2-review-eunit-full-6.log`（第 6 轮，All 6481 passed）
- 基线验证 worktree：`/tmp/imboy-review-main-wt`（已 remove + prune）
- 空转探针：`/tmp/eunit-probe/probe{,2,3,4}_tests.erl`（probe4 为必败断言判决实验）
- 迁移演练：克隆库 `imboy_w2review`（已 DROP），psql 输出见本报告 §5
- 单套件计数：两轮后台日志（member/handler/meck 家族 14 项 + 真库家族 8 项）
