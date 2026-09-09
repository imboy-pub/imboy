# STEP-09 备注 — 实现说明与设计决策

## 交付文件（Step 9 新增）

| 文件 | 层 | 说明 |
|---|---|---|
| `src/repo/teaching_submission_repo.erl` | Repo | 配方①幂等 CTE（ON CONFLICT 带部分索引谓词 + digest 判 5460）、配方②FOR UPDATE 取号、配方③lock-first 撤回；附件归属校验（creator_user_id）；AI 草稿入队占位；家长作业列表/老师队列（withdrawn 恒过滤、ai_status LATERAL join、total 计数）/学员历史 |
| `src/repo/teaching_review_repo.erl` | Repo | 草稿 upsert（per submission+reviewer）、配方③条件发布（0 行回读判 already_published/withdrawn/no_draft）、AI 草稿读取 |
| `src/logic/teaching_assignment_logic.erl` | Logic | 家长列表（learner 显式拒绝 5422）、详情（guardian|staff 双路径）、幂等提交（守卫链：assignment 教学形态→learner 匹配→can_submit→task 开放 status=1→附件规则 1 video+≤3 photo+归属→digest sha256→事务体） |
| `src/logic/teaching_review_logic.erl` | Logic | 队列（staff 班级集 + group_id 过滤归属校验）、工作台（staff-only，家长 5424=T6）、草稿（写角色 manager/teacher，assistant 5425；保留字段 5484）、发布（确认学员 5483→内容非空 5485→lock-first 事务）、撤回（仅 can_submit 监护人；老师/Owner 代撤 403）、视角感知详情（guardian payload 结构性无 ai_draft 键）、历史（guardian can_view_review | 同机构班 staff） |
| `src/api/teaching_error.erl` | Handler 公共 | reason 原子→错误码统一映射（5401-5485 全段） |
| `src/api/teaching_assignment_handler.erl` | Handler | list/detail/create_submission（Idempotency-Key 头 8..128）/submission_detail/withdraw/history；:id 路由绑定（:token/:gateway 先例） |
| `src/api/teaching_review_handler.erl` | Handler | queue/workbench/save_draft/publish |
| `src/imboy_router.erl` | M | +10 路由（assignments、assignments/:id、:id/submissions、submissions/:id、:id/withdraw、:id/review-workbench、:id/review-draft、:id/reviews/publish、review-queue、learners/:id/history） |
| `test/repo/teaching_flow_integration_tests.erl` | Test | FLOW-01/IDEMP-01/STATE-01 真库集成（直连 4323，BEGIN/ROLLBACK） |

## 关键设计决策

1. **严格照抄 C 配方**：幂等 CTE（谓词不可省）、assignment 行 FOR UPDATE 取号、
   lock-first 撤回/发布互斥；DB 触发器只作兜底，应用层两语句配方为主防线。
2. **TSID 表达**：所有入参 `:id` 路径段 binary→integer（服务端解析），所有出参
   payload integer→binary 字符串（API-01 契约）。
3. **ai_status 语义（无 Worker 本波）**：提交即入队 queued 占位行
   （uk_crd_active_per_submission 幂等）；队列/工作台读 LATERAL 最新有效草稿行
   （queued/running/succeeded/failed），无行=none。AI failed 不影响任何人工路径。
4. **撤回端点形态**：定为 `POST /teaching/submissions/:id/withdraw`（契约 Step 4
   冻结时留给 Step 9 的决定）；守卫=can_submit 监护人 + 无已发布回评 +
   lock-first；withdrawn_at/withdrawn_by 审计由 00000098 CHECK 强制。
5. **家长详情双 schema 隔离**：parent_view 构造函数结构性不含 ai_draft 键；
   teacher_view = parent_view 超集 + ai_draft + my_review_draft（T6 实现层落点）。
6. **发布幂等**：0 行条件更新回读——已存在 published → {ok, already_published}
   （HTTP 200 幂等重放）；submission withdrawn → 5482；无草稿 → 5480。
7. **附件归属**：attachment.creator_user_id == 提交人（视频/照片须家长本人 confirm
   过的）；MIME 白名单细化留给 Step 10（此处校验存在性+归属+数量规则）。

## PARTIAL 项（如实标注）

- `assignments/:id` 详情 payload 为概要版（未含 submissions 时间线数组）——
  FLOW-01 主链不受影响（家长经列表 latest_submission + `submissions/:id` 详情取时间线）。
- `SubmissionCreated.submitted_at` / `learner_id` 字段未回填（契约可选完整度项）。
- Handler 层无 Cowboy 级单测（薄适配；路由注册经 billing_route_tests 类比 +
  compile 验证，HTTP 行为 Step 17）。
- queue 的 ai_status=failed 筛选分支经 SQL 实现但未单测（worker 不存在，
  仅集成层验证了 failed 不阻塞发布）。

## 给后续泳道的输入

- Step 10（附件授权）：assets_tx 已返回 path（object_key）；view_url 授权挂
  submission_access 守卫即可。
- Step 11（AI Worker）：enqueue_ai_draft_tx 为入队点；worker 取任务后原子改
  queued→running（uk_crd_active_per_submission 防重复）。
- Step 13（moya）：错误判定双通道（HTTP 401→envelope code）；TSID 全程 string；
  Idempotency-Key 客户端 UUID ≥8 字符。
- Step 16：history 已按 org 内多 workspace 聚合（w.id 输出），learner.user_id
  绑定后 staff/guardian 判定无需改（守卫在 ACL 层）。

## 撞车修复补记（R4，B/B2 并行污染）

**事件**：B2 在 19:51-19:52 交叠期并行写入 4 个教学文件后，工作区呈 B/B2 混合态，
`make compile` 红（`teaching_assignment_logic.erl:363` 语法错误：函数调用结果上
直接 `#{` 更新——Erlang 语法不允许）。B2 快照基于本人早期版本（含已被本人修复的
`elib_tsid:generate(homework_submission)` 旧调用）。

**逐文件处置**（以本人 handoff 全绿终态为权威）：

| 文件 | B2 改动 | 处置 |
|---|---|---|
| `teaching_assignment_logic.erl` | list 拆分 list_for_guarded+guardian_has_any_scope、task 开放语义改 status≠3、新增 deadline_passed/mime_matches、详情加 submissions 时间线（半成品断句致语法错）、回退到旧 tsid 调用 | **全量重写为本人终态**（status=1 开放；无 deadline/MIME 校验——留 Step 10；详情概要版；`elib_tsid:generate()`） |
| `teaching_submission_repo.erl` | 新增 `submissions_for_assignment/1`（仅被 B2 半成品详情引用）；history SQL 加 published review 明细列（加法） | 删除孤儿函数；保留 history 加列（加法无害，history_item 已消费 published_review_id） |
| `teaching_context_repo.erl` | assignment_scope 增选 task_title/task_deadline/group_title（加法） | 保留（本人 logic 不读，Step 10+ 可用；无行为变化） |
| `teaching_review_logic.erl` | save_draft 加 lock-first（完成且正确）；bundle 增载 published + parent_view 回填 published_review/submitted_at/withdrawn_at/learner_id（补全 FLOW-01 家长视图 published-only——方向正确且完整） | **保留增强**，修两处缺陷：① build_workbench `maps:get(draft, Bundle)` 缺默认值（badmap 运行时崩溃，本人在自己版本同样埋了此雷，B2 未修）→ 加 `undefined` 默认；② `submission_not_found` 原子未在 teaching_error 映射（会落到通用 ERR_ERROR）→ 统一回 `not_found`（404） |

**净效果**：家长视图 `published_review` 由 B2 补全为真实数据（原本人版本硬编码
null，是 notes「PARTIAL 项」之外的隐性缺口）——该增强经本次复验全绿后收编。

**复验**（同 STEP-09/commands.md 命令）：

```
$ make compile                          # COMPILE_EXIT=0（零告警）
$ erlc -I include -o test/ <三个教学测试文件>   # ERLC_EXIT=0
$ erl ... eunit:test([teaching_flow_integration_tests, teaching_acl_tests,
    teaching_auth_logic_tests, auth_middleware_tests, billing_route_tests,
    auth_ds_tests], [no_tty])           # ALL_RESULT: ok
$ psql residue check                    # 0
```
