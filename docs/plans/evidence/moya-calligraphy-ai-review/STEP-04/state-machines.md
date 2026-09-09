# 墨芽教学域状态机 — Step 4 冻结

> 来源：执行计划 §6.3、§7.2；实现泳道 Step 7（DB 约束）/ Step 9（API 状态机）。
> 语义：所有转换必须在服务端事务内完成条件更新（compare-and-set）；
> 客户端提交的 status / reviewer_uid / published_at / attempt_no 一律忽略或拒绝。

## 1. homework_submission

```
                 +---------------------+
      create     |                     |  withdrawn 仅当：
   (idempotent)  |     submitted       |   1) 发起人 = 该 learner 的
   ────────────▶ |  (attempt_no = N)   |      can_submit=true 有效监护人
                 |                     |   2) 该 submission 无已发布回评
                 +----------+----------+   3) 老师与 Owner 不可代撤
                            │              （7.2 硬约束）
                            │ 监护人发起
                            │ POST withdraw（首版通过 submissions/:id
                            │ 语义扩展或独立端点，由 Step 9 定；本文冻结守卫）
                            ▼
                 +---------------------+
                 |      withdrawn      |  撤回效果：
                 +---------------------+   - 老师队列即时移除（队列查询恒过滤）
                                           - 附件证据保留不删除（审计/合规）
                                           - 不可再变为 submitted（新提交=新 submission）
                                           - 家长历史仍可见（标记 withdrawn）
```

| 转换 | 触发者 | 守卫条件 | 拒绝码 |
|---|---|---|---|
| ∅ → submitted | 监护人（createSubmission） | can_submit=true；assignment 开放；assets 校验通过；幂等键唯一 | 5423 / 5441 / 5442 / 5460 |
| submitted → withdrawn | 同一 submission 的 can_submit 监护人 | 无已发布回评（5481 若已发布）；非老师/Owner 代撤（403） | 5481 / 403 |
| withdrawn → submitted | 禁止 | —— | 409 |
| 重练 | 监护人（新 createSubmission） | 新 submission/attempt_no+1；旧记录不动 | —— |

- `attempt_no = max(assignment 下既有 attempt) + 1`，服务端计算，UNIQUE(assignment_id, attempt_no)。
- 幂等：`(uid, assignment_id, idempotency_key)` 命中 → 返回既有 submission（`idempotent_replayed=true`），不新增 attempt。

## 2. teacher_review

```
   save draft (PUT review-draft, upsert per (submission, reviewer))
        │
        ▼
   +---------+   publish（条件更新）     +-----------+
   │  draft  │ ──────────────────────▶ │ published │ （终态）
   +---------+   WHERE status='draft'   +-----------+
        │        AND reviewer_uid=JWT uid
        │
        │ discard（老师主动废弃 / 老师被移任教职后的清理，Step 9+）
        ▼
   +-----------+
   │ discarded │ （终态；可另存新草稿）
   +-----------+
```

| 转换 | 触发者 | 守卫条件 | 语义/拒绝 |
|---|---|---|---|
| ∅ → draft | 任课老师（manager/teacher） | class_staff active；assistant 无写权 | 5425 |
| draft → draft | 同一 reviewer | upsert 覆盖草稿体，不产生新行 | —— |
| draft → published | 该草稿的 reviewer（JWT uid 一致） | 条件更新 `UPDATE teacher_review SET status='published', published_at=now() WHERE id=? AND status='draft' AND reviewer_uid=?`；至少一种有效反馈内容；submission 未 withdrawn | 影响行=0：已发布→200 幂等重放（already_published=true）；无草稿→5480；submission 已撤→5482；内容空→5485 |
| published → 任意 | 禁止 | published 为终态，published_at/reviewer_uid 不可变 | 409 |
| draft → discarded | 草稿 owner 老师 | 发布前主动废弃 | —— |

- **一次性质**：draft→published 全局仅允许一次；重复 publish 请求永远返回已发布结果（不报错，幂等友好）。
- `published_at` 在条件更新同一事务内由服务端写入；`reviewer_uid` 在草稿创建时固化。

## 3. calligraphy_review_draft（AI 草稿）

```
   submission created
        │ enqueue（只带业务 ID + 版本）
        ▼
   +---------+  worker 取任务（原子）  +---------+
   │ queued  │ ─────────────────────▶ │ running │
   +---------+                        +---------+
      │    │                             │   │
      │    │ 取任务即失败（附件缺失等）      │   │ Schema 校验通过
      │    ▼                             │   ▼
      │  +---------+  超时/provider 错误 │ +-----------+
      │  │ failed  │ ◀───────────────── │ │ succeeded │（终态）
      │  +---------+                    │ +-----------+
      │    ▲    │                        │
      │    │    └─ 重试 ≤ N 次（Ste11 定 N）│
      └────┘── 超过重试上限 → failed（终态）
```

| 转换 | 触发者 | 守卫条件 |
|---|---|---|
| ∅ → queued | submission 创建事务后的入队（服务端内部） | submission 状态 = submitted；载荷只含 Org/Ws/assignment/submission/attachment ID + rubric/prompt 版本，不含短时 URL |
| queued → running | AI Worker | 原子取任务（`DELETE … RETURNING` 或事务内锁）；超时上限 |
| running → succeeded | AI Worker | LLM 输出通过 JSON Schema 校验；写 result_json（不含思维链）+ input_digest + completed_at |
| running → failed | AI Worker | 超时 / provider 错误 / 非法 JSON / 附件缺失；重试上限后终态；写 error_code |
| succeeded/failed → 任意 | 禁止 | 同一 submission 至多一个有效草稿；重练的新 submission 生成新草稿 |

- AI 状态独立于 assignment 状态；`failed` 仍进入老师人工队列（老师工作台 ai_status=failed 时走纯人工，不阻塞）。
- AI 任何状态都**不能**触发 published；发布永远只能由老师完成（D-10）。

## 4. group_task_assignment（教学/普通双形态）

```
   普通群作业（既有路径，不改语义）:
     create → assigned → (submit/review 既有 group_task 流程)
     learner_id IS NULL；唯一性 (task_id, user_id) WHERE learner_id IS NULL

   教学作业（新增形态）:
     create（teacher 发布 task + 建 assignment）
        │
        ▼
     +-----------+   监护人提交（幂等）   +---------------------+
     │ assigned  │ ──────────────────▶ │ submission(s) 挂载  │
     │ (待完成)   │                     │ 独立状态见 §1       │
     +-----------+                     +---------------------+
        │  作业关闭/删除（task status）          │
        ▼                                     │ 任一 submission 有 published review
     closed（终态）                            ▼
        │                              reviewed（家长视角推导态：
        └─ 关闭后拒绝新提交（5442）          assignment 本身无 published 字段，
                                           由最新 submission 的回评推导）
```

| 转换 | 触发者 | 守卫条件 |
|---|---|---|
| ∅ → assigned（教学） | 任课老师发布/指派 | (task_id, learner_id) 部分唯一；learner.org == group.workspace.org（跨机构拒绝） |
| ∅ → assigned（普通） | 既有 group_task assign 流程 | (task_id, user_id) WHERE learner_id IS NULL 部分唯一（兼容一家长两孩） |
| assigned → 有提交 | 监护人 createSubmission | 见 §1；assignment/task 未关闭 |
| 任一形态 → closed | task status 变更（老师/群管理，既有流程） | 关闭后拒绝新 submission（5442），已建 submission/回评保留 |

- 推导态（服务端计算，不落库）：`pending`（无 submission）/ `submitted`（有 submission 无已发布回评）/ `reviewing`（老师草稿存在）/ `reviewed`（存在已发布回评）。
- **assignment 永不因重练回到 pending**：重练 = 新 attempt submission 挂载同一 assignment。

## 5. 跨状态机不变量（Step 7 DB 约束 + Step 9 逻辑共同保证）

1. submission.submitted → teacher_review.published 存在 ⇒ submission 不可 withdrawn。
2. teacher_review.published 存在 ⇒ reviewer_uid / published_at 不可变、不可删。
3. 每个 submission 同时至多 1 个有效 AI 草稿 + 1 个已发布回评（其余为 discarded/被覆盖 draft）。
4. withdrawn submission 不出现在任何老师队列/工作台入口；但其附件、AI 草稿行、审计记录保留。
5. assignment → learner → group → workspace → organization 链上任一跳不匹配 ⇒ 请求拒绝（deny-by-default）。
