# STEP-16 设计决策、给 B 的 handoff — 学员账号绑定

## 设计决策

1. **守卫（deny-by-default）**：Operator 必须是 learner 所属 Org 的 owner，或该 learner 所在任一 active 班的 class **manager**（teacher/assistant 无绑定权——绑定是管理动作不是教学动作）。守卫 SQL 在 bind_repo:operator_role_tx（事务内，_tx 变体），logic 只做映射与审计。未复用 B 的 teaching_acl 导出（其 resolve_staff 面向教学角色而非管理动作），自建 12 行守卫查询并保持同 deny-by-default 语义。
2. **审计三层**（不建表、不碰 migrations）：
   - 行级：learner.account_bound_at/by（00000096 §6.4 既有列）；**解绑保留这两个字段作为"最近一次绑定"痕迹**，user_id=NULL 即当前未绑定——查询语义不变且痕迹可审计；
   - 日志级：`[teaching_learner_bind] event/role/operator_uid/learner_id/target/why`（无 display_name/PII）；
   - DB 审计表需求（含 C 的 sentinel uid 0 匿名化建议）登记为 DB 缺口（下方 handoff #2），本波不自行建表。
3. **解绑语义**：仅 `UPDATE learner SET user_id=NULL`——与 user 注销时 FK SET NULL 同一物理形态，任何下游按 user_id 判定入口的逻辑天然立即失效；learner/submission/review 零删除（BIND-02 集成断言）。
4. **错误分类在 repo 层**（classify_bind_error）：23505+uk_learner_org_user → duplicate_bind_in_org；23503 → invalid_target_user；logic 层 reason_atom 收敛为业务原子（其余 db_error）。
5. **测试组织**：照 B 的真库 4323 scratch + BEGIN/ROLLBACK 模式；种子含双 Org（甲=两 Workspace 两班 / 乙=一班）支撑跨 Org 矩阵；ID 段 99 前缀与 B 的 98 段错开。
6. **HISTORY-01 复核方式**：不调用 B 的 history（其 repo 函数走全局连接池，直连测试不可达）；用与 teaching_submission_repo:history 同构的 join SQL 在同事务断言（learner 锚定聚合跨 ws 连续 + 跨 Org 关系为 0 行 fail-closed）。这是对 B 实现的**独立复核**而非替代。

## Handoff（给 Coordinator 转 B）

### #1 路由段（B 正在挂路由，此为所需段——我没有改 imboy_router.erl）

```erlang
%% 教学学员账号绑定（Step 16：管理侧最小动作；JWT；禁改 B 文件故未自行挂载）
{"/api/v1/teaching/learners/:id/bind",   teaching_learner_bind_handler, #{action => bind}},
{"/api/v1/teaching/learners/:id/unbind", teaching_learner_bind_handler, #{action => unbind}},
```

handler 骨架建议（B 可直接落）：bind → `teaching_learner_bind_logic:bind_learner(CurrentUid, LearnerId, BodyTargetUid)`；错误映射：not_authorized→403、learner_not_found→404、duplicate_bind_in_org→5462 段新码（见 #3）、invalid_target_user→422、db_error→500。**本波未建 handler 文件**（属 B 的 api/ 域，避免越界；logic/repo 已就绪可直接调用）。

### #2 DB 缺口登记（转 C）

- 教学管理审计表（bind/unbind/删除等管理动作）：建议 `teaching_admin_audit(id, action, operator_uid, learner_id, target_uid, detail jsonb, created_at)`；**审计用户匿名化遵循 sentinel uid 0 而非 NULL**（STEP-08-DB 建议，避免 FK SET NULL 抹掉操作人）；operator_uid FK→"user"(id) 需预插 uid=0 占位行或取消 FK 改裸列（C 决策）。

### #3 错误码建议（Step 9 号段管理，B 定夺）

- duplicate_bind_in_org / invalid_target_user / not_authorized 若走 HTTP 403/404/422 通用码可复用现有；若需域码建议 5427-5429 段（5420-5426 已占）。

### #4 B 的 WIP 挡板提醒

- `test/logic/teaching_attach_logic_tests.erl`（未跟踪）当前编译失败（?WITH_MECKS badly formed）→ `make eunit-local`/test-build 被挡；修复前各泳道定向测试需用 erl 直跑（本波 commands.md 有可复现命令）。

## 已知风险

1. 账号本人"历史入口"当前不存在于 B 的 history_access（只有 guardian/staff 两分支）——**绑定后账号本人要能看自己历史，需 B 在 history_access 补 learner.user_id == Uid 分支**（BIND-02 的"解绑后失效"以两分支关系不存在已验证；补第三分支后该测试追加正向用例即可）。这是有意的预留（不越界改 B 的文件）。
2. 路由/handler 未挂（#1）——logic 就绪但无 HTTP 入口，Step 17 联调前需 B 接线。
3. 审计目前依赖日志 + 行字段，DB 审计表缺（#2）——满足试点最低要求，正式运营前补。

## B 接线完成（R6）

Owner: Agent B | 日期: 2026-09-09 | 承接上文 handoff #1/#3 与已知风险 #1。

### 1. 路由落点（handoff #1 原文照挂）

`src/imboy_router.erl` 教学段（`/api/v1/teaching/learners/:id/history` 之后）：

```erlang
{"/api/v1/teaching/learners/:id/bind",   teaching_learner_bind_handler, #{action => bind}},
{"/api/v1/teaching/learners/:id/unbind", teaching_learner_bind_handler, #{action => unbind}},
```

### 2. 错误码决定及理由（handoff #3 定夺）

新建 `src/api/teaching_learner_bind_handler.erl`（内联映射，未动 teaching_error.erl——不在我可写清单）。
`include/error_code.hrl` 的 5427-5429 宏与 ERROR_MSG_MAP 中文条目**已在前轮就位，本轮零新增码**，未复用 5300-5399。

| logic reason | HTTP status | envelope code | 理由 |
|---|---|---|---|
| not_authorized | 403 | 5429 | 域码信息量大于通用 403（区分"绑定守卫拒绝"） |
| learner_not_found | 404 | 404 | 资源不存在，通用码足够 |
| learner_inactive | 404 | 404 | 学员档案停用=目标资源当前不可用，与 not_found 同族（不新增域码） |
| duplicate_bind_in_org | 409 | 5427 | 状态冲突标准 409 + 域码精确提示 |
| **invalid_target_user** | **422** | **5428** | **定夺理由**：这是域业务错误（目标账号本身不存在/不可绑定），非通用参数格式错误；5428 已预定义且客户端需精确提示"换目标账号"而非"改参数格式"；HTTP 422 与 handoff #1 建议一致 |
| not_bound | 409 | 409 | 解绑时无绑定，通用冲突码 |
| db_error | 500 | 1 | 通用 |

body 契约：bind 取 `user_id`（兼容整数与数字字符串）；unbind 无 body。均走 `elib_response:error_with_status/4`（envelope + 真实 HTTP status，项目先例 passport_handler/throttle_middleware）。

### 3. history_access 第三分支落点（已知风险 #1 的预留兑现）

`src/logic/teaching_review_logic.erl` 的 `history_access/2`：guardian 分支之后、staff 分支之前插入 `self_bound(Uid, LearnerId)`——`SELECT id FROM learner WHERE id=$1 AND user_id=$2 AND status='active' LIMIT 1`（新增内部函数）。绑定后账号本人可查自己历史；解绑仅置 user_id=NULL 即立即失效（无额外清理）。

### 4. 【重要】teaching_learner_bind_logic:tx_run 真 bug 修复（handler 契约测试暴露）

原实现 `case elib_pg:with_tx(Tx, ...) of {ok, Result} -> Result` 存在双重错误：`epgsql:with_transaction` **透传 Tx fun 的原始返回值**（`{ok, Row}` 或业务 `{error, ReasonAtom}`），原 case 会把 `{ok, Row}` 再解一层（调用方拿到**裸 Row** 而非 `{ok, Row}`），且把业务错误原子（not_authorized/duplicate_bind_in_org 等）误折叠为 `db_error`。**生产上 D 的 logic 从未被 HTTP 层调用过，此 bug 未曾暴露**；Worker（run_once）无此问题（解包后重包 `{processed, _}`）。

修复：仅 `{rollback, Reason}` 折叠 `db_error`，其余透传。**给 Coordinator 的风险提示**：若 D 泳道有其他调用方依赖旧（错误）返回形态（裸 Row / db_error 折叠），需同步知悉；bind 集成测试（直调 repo）不受影响，全量回归已绿。

### 5. 审计接线（Coordinator R6 通报 → 已实现）

`teaching_learner_bind_logic` 的 bind/unbind **成功路径在同一事务** `INSERT teaching_admin_audit`（新内部函数 `audit_admin_tx/6`）：
- 列按 migration-99 实际：`target_user_id`（非 handoff 草案的 target_uid）；action 取枚举值 `bind_learner`/`unbind_learner`（非 bind/unbind）
- detail jsonb 含 `role`（owner/manager）；target 仅 bind 有（unbind 为 NULL=本无目标用户）
- 被拒动作不落行（日志级 audit_rejected 已覆盖）；审计写失败 `error({audit_insert_failed, _})` 抛出 → 整事务回滚（fail-closed）
- 注意 epgsql 参数不接受原子：action/role 经 atom_to_binary 传参

### 6. 测试结果

- 新建 `test/api/teaching_learner_bind_handler_tests.erl`（19 例全绿）：bind/unbind 参数解析（path :id、user_id 整数/字符串/缺失/非法）、错误映射全矩阵（含 invalid_target_user 422/5428）、logic 审计接线 4 例（bind INSERT 参数形态、unbind target NULL、被拒不调、审计失败 fail-closed 抛出）。
- `test/repo/teaching_learner_bind_integration_tests.erl` 追加 2 例（全绿）：
  - `history_self_branch_positive_test_`：绑定前后 self_bound 判定依据（同构 SQL：user_id=本人 count 1 / 陌生人 count 0）；
  - `audit_rows_after_bind_test_`：00000099 表契约复核（SQL 同构手法，与 HISTORY-01 先例一致——logic 走全局池直连不可达；"logic 确实调 INSERT"由 handler 测试 meck 断言覆盖）。
- 曾尝试 meck `elib_pg:with_tx` 直连转发方案，在 eunit 全量跑出现 meck 版本相关诡异行为（本仓 meck 无 `is_mecked/1`；with_tx mock 单跑/全量行为差异未根因定位）——已改用 SQL 同构 + logic 层 meck 断言的稳妥组合，不再依赖。

### 7. 验证（全部通过）

```bash
make compile   # EXIT=0，零源码警告（Makefile/erlang.mk recipe 警告为既有环境噪音）
erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'R = eunit:test([teaching_flow_integration_tests, teaching_acl_tests, teaching_auth_logic_tests, teaching_learner_bind_integration_tests, teaching_ai_provider_tests, teaching_ai_worker_tests, teaching_learner_bind_handler_tests], [no_tty]), io:format("RESULT: ~p~n", [R]), case R of ok -> halt(0); _ -> halt(1) end.'
# RESULT: ok（七模块；attach 两套属 C 泳道不在本清单）
```
