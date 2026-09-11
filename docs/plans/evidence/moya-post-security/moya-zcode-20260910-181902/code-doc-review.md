# 代码与文档评审报告 — RUN moya-zcode-20260910-181902

评审人：A0（应用户「review代码和文档」指令执行；独立于 Wave 2 的 A5 交叉审查）
评审对象：`candidate-imboy-v2-a7e19a26.patch`（d7e15133，31 代码文件）+ `candidate-moya-v2-1a636660.patch`（db4aaeff，21 文件）+ 仓库内全部文档/契约物
方法：迁移全文精读 → 后端新模块精读（roster/task 四层）→ 媒体 diff 面（ACL/校验/双排除）→ moya 两端关键路径 → 跨切面扫描（SQL 拼接/敏感值/隐私）→ 文档与契约物交叉核对

## 总评

**整体质量高，无 P0。** 分层/参数化/ACL/幂等/隐私五条红线全部落实且有测试锚定。但抓到 **2 个 P1**（均为真实联调/回滚场景才暴露、单测互相 mock 掩盖的断层），6 个 P2，若干正面确认。

---

## P1（应修：联调阻断或回滚隐患）

### P1-1 task_id 跨端类型断裂（列表+发布必挂）
- 后端：`src/ds/teaching_task_ds.erl:55` `task_id => elib_id:gen(<<"task">>)` — varchar(40) 带前缀串（如 `task_ab12…`）；create payload/list SQL 均返回它。
- 冻结契约与 OpenAPI：`task_id` 为 TSID string（yaml 示例 `"975001000000000019"`）。
- 前端：`teacher-api.ts` `parseTeachingTask`/`createTeachingTask` 用 `tsidOrThrow`→`isTsid`（纯十进制、≤19 位）→ **真实响应必抛「老师数据格式不正确（task_id）」**，作业列表与发布成功路径全断。
- 未暴露原因：后端 eunit 断言自身形状、前端测试 mock 数字串（`"9001"` 等），双端无共享 fixture。
- 建议（推荐 A）：后端 list SQL `t.id AS task_id` + create payload 用 bigint `Id`（与「TSID string」契约一致，一处 SQL 一处 payload）；方案 B（前端放宽为 opaque string）偏离冻结契约，不推荐。

### P1-2 00000105 down 预检缺「镜像一致性」校验
- `00000105….down.sql` 注释宣称「预检通过 = 全部数据可由旧列完整表示」，但只挡 feedback_image 与单 review 多视频；未校验单视频行已镜像到 `teacher_review.video_attachment_id`。
- 后果：手工插入或应用双写异常时，down drop 表会**静默丢失**未镜像的视频关联。
- 建议：down 预检追加第三分支——`EXISTS (SELECT 1 FROM review_asset rv JOIN teacher_review tr ON tr.id=rv.review_id WHERE rv.kind='feedback_video' AND tr.video_attachment_id IS DISTINCT FROM rv.attachment_id)` → RAISE。

## P2（应修不阻断）

| # | 发现 | 位置 | 建议 |
|---|---|---|---|
| P2-1 | `request_digest` 规范化无长度前缀，title/desc 含 `\|` 时存在理论歧义（同 key 异 body 可能误判 replayed=静默忽略新内容） | `teaching_task_logic.erl:287-305` | 后续改 length-prefix 框架化；过渡期旧记录重放会 5460（可接受）。现缓解=客户端「内容变才换 key」 |
| P2-2 | list 的 `description` 为 NULL 时透出 null，契约写 string | `teaching_task_repo.erl` list_tasks SQL | `COALESCE(t.description,'')`；前端已容 null |
| P2-3 | Idempotency-Key 下限不一致：后端 8..128（`teaching_task_handler.erl:60`），冻结契约「最大 128」，前端 1..128 | 三处 | 契约/OpenAPI 补「≥8 字节」或后端放宽；实际 key 为长随机串不触发 |
| P2-4 | 3 图上限 CONSTRAINT TRIGGER 在 READ COMMITTED 下有并发窗口（互不可见计数） | `00000105 up:73-98` | 依赖应用层 lock-first 串行化，与 00000098 既有口径一致；保持兜底定位即可 |
| P2-5 | handler `maps:get(current_uid, State)` 无 default，依赖 JWT 中间件保证 | `teaching_roster/task_handler` | 与既有 teaching handlers 惯例一致，保持；可在 ADR 记录依赖 |
| P2-6 | `sort_order` 无 CHECK(>=0)，仅应用层校验 | `00000105 up` | 可在后续迁移补 CHECK；低危 |

## 正面确认（抽查全过）

- roster：inactive staff 折叠 5430 不泄漏存在性；机构 NULL fail-closed；SQL 层即不取隐私列，payload 四字段白名单；路由 action=`list` 与 handler 分派一致、binding `:id` 一致。
- task create 守卫链：resolve_staff(write)（assistant/非staff/removed 全拒）→ 机构 NULL fail-closed → learner_readiness（active enrollment+恰一 can_submit）→ 同事务创建+106 持久幂等（CTE ON CONFLICT+回读+digest 比对判 5460，空/短 key fail-closed 不落库）；`learner_ids` 重复拒绝；`deadline` 受限字符集（RFC3339）+ 未来校验。
- SQL 注入：新模块全部 `$n` 参数化（唯一 `++` 在参数列表尾追加 LIMIT 参数，非 SQL 体）。
- 媒体：`authorize_review_asset` 分流（draft=reviewer 本人+submitted；published→submission_access；withdrawn/discarded/未绑定/异常全 false）；`validate_assets_tx` 批量 ANY($1)+数量/active/scope/creator/MIME↔kind 矩阵；孤儿清理双 NOT EXISTS；`video_attachment_id` 双写一致性断言（旧列≠派生值即拒绝）。
- moya：`parseTeachingTask` 容 null description；`withdrawSubmission` 仅接受 `status==="withdrawn"`、畸形 TSID 不发请求；review-flow 1/3 上限+isTsid+只读发布态；幂等键「内容变才换、重试复用」。

## 文档评审与已执行修正

- **已修正（A0 职责内）**：`resume-prompt.md` 旧 SHA `ff249d2a`→`d7e15133`、「32 文件」→「63 文件」。
- catalog 三条目与路由/handler/action 一致 ✓（含 8..128 字节、错误码集）。
- OpenAPI 与前端假设一致（数值 TSID 示例）——**与 P1-1 对照即证后端偏离契约**，修后端即三方对齐。
- rubric-v0/protocol/manifest.schema/provider-gap：结构与计划 MN-EVAL-01..04 验收口径一致，privacy-scan 零命中（复扫确认）。
- final-report/resume-prompt 与实际产物（worktree 路径、门结果、patch SHA）交叉核对一致（除上表已修正项）。

## 教训（记入续跑建议）

跨端字段格式缺「契约级共享 fixture」：建议续跑在 Phase 2A 前补一条**后端真实响应样本注入前端解析器**的契约测试（Moya tests 读后端 eunit 的 JSON 快照），防 P1-1 类断层再发。

---

## Round 2 增补（第二轮深读：repo/ds 全文、页面层、测试质量、Phase 1 文档全文）

**P1-1 已由静态断言升级为运行时实证**：用 moya 真实 `isTsid` 执行 `isTsid("task_7f3a9c2b") => false`，并以真实后端形状 payload 走解析路径实测抛错「老师数据格式不正确（task_id）」。

**Round 2 覆盖面（均无新增 P1）**：
- `teaching_roster_repo.erl` 全文：SQL 全参数化、active+同机构双过滤、监护人计数子查询、隐私列在 SQL 层即不取；binary count 归一。PASS。
- `teaching_task_ds.erl` 全文：rollback 传播链清晰（`{rollback,{db,_}}`→整单 ROLLBACK）；重放路径只读不重写；逐 learner INSERT 循环=N 次往返（班级规模下可接受，记性能观察）。
- `teaching_task_repo.erl` learner_readiness/insert_assignment_tx/in_clause：IN 子句为 `$n` 占位符生成（非字面量拼接）✓；缺行 learner→not_in_class 补齐 ✓；`the_guardian=min(...)` 仅在恰一监护人语义下被使用 ✓。
- `teaching_review_logic.parse_review_assets`：kind 白名单、sort_order 0..999、shape（0-1/0-3+attachment 不重复）、旧字段双写一致性（不一致→assets_invalid）全部严格。PASS。
- moya 页面层：submission-detail 撤回 in-flight Promise 复用+错误码分支常量化；tasks 表单 `formPhase` 状态机+submitting 防重+5460 不换 key+formKey 内容变才重置。PASS。
- 测试质量抽查：断言为行为级（canWithdraw 状态迁移、withdraw URL 形状、`rework_attempt=2` 继承保留），非同义反复。PASS。
- Phase 1 文档全文：`protocol.md` 种子留证/交叉平衡/退出规则不得补造/增益门三条件预冻结，质量高；`provider-gap.md` 的 vision=false 与 build_messages 仅元数据两项源码断言经本机复核属实。

**Round 2 新增观察**：
- P2-7（记入）：protocol §8.1「B 独有过程观察」由评分老师在解盲后复核判定——解盲后判定存在偏倚风险；建议 Phase 2B 执行前把「A/B 逐条对比」改为盲序呈现或引入未参与评分的第三方复核，并在 run-manifest 冻结该细则。
- 观察：tasks 路由 method 分派 shim 位于 imboy_router 模块内（handler 侧补 resolve_action 后可收敛）——round 1 已记录为已知偏差，维持。

**结论：维持总评——无 P0；2 P1（task_id 跨端断裂·已运行时实证 / 00000105 down 镜像预检缺失）+ 7 P2。两个 P1 仍为报告状态，等用户决定是否出 follow-up patch（v3）。**

---

## Round 3 增补（第三轮：剩余面收尾——覆盖至此完整）

- `replace_assets_tx`：同事务 DELETE+INSERT（外层 save_draft 事务包裹，原子替换成立）、参数化。PASS。
- `review_for_asset_path`：review_asset→teacher_review→homework_submission 联查返回分流所需四字段，缺失跳 `{ok,undefined}`→Logic fail-closed。PASS（9 项真库 ACL 测试已覆盖分支）。
- `teaching_error.erl` diff：reason→(文案,宏) 三条新映射引用正确宏；handlers 的整数字面量与宏值相同（round 1 已记「待切宏」收敛项）。
- OpenAPI 路径参数名 `{id}` 与路由 binding `:id` 一致（此前担心的 `{group_id}` 命名漂移不存在）；`/teaching/tasks` 单条目+shim 分派已在案。
- `rubric-v0.md` 全文结构：5 维度锚点+V1-V5 一票否决+第 6 项修改量+统计方法预冻结+冻结声明——与 MN-EVAL-01 验收口径逐条对应。PASS。
- `manifest.schema.json`：pattern/enum/条件必填（video→时长、image→张数）/`additionalProperties:false`/revoked 拒收。PASS。
- wxml 绑定抽查：`canWithdraw` 门控入口、`formPhase` 驱动表单、learner `ready/checked` 禁选态与 ts 逻辑一致。PASS。

**Round 3 新增发现：无。** 三轮累计：无 P0；2 P1（task_id 跨端断裂·运行时实证 / 00000105 down 镜像预检缺失）+ 7 P2。代码与文档的评审覆盖至此完整——后续再请求 review 将得到同一结论，除非代码/文档发生变更。

---

## Round 4 增补（独立对抗式复审合并 + A0 逐条验证）— 结论改级为 NO-GO/PARTIAL

第四轮应用户要求，改派**未参与开发的独立评审 Agent** 做对抗复审（不带既有结论）。其报告：`RUN_ROOT/agents/independent-review/review.md`。A0 对其三个关键新主张逐条验证，**全部证实**：

| 编号 | 发现（已证实） | 证据 | 定级 |
|---|---|---|---|
| P0-1 | task_id 双端形状断裂+同 key 重试死锁：后端 `teaching_task_ds.erl:55` varchar；前端 `teacher-api.ts:397/512` `tsidOrThrow` 必抛；发布后端成功但前端恒报失败且重试复用同 key→死锁 | 运行时实证（round 2）+ 本次复核 | **P0**（自 P1 上调：影响面=list+create 双路径+重试死锁） |
| P1-1 | 家长 published review assets **无 object_key**：`teaching_review_logic.erl:780` `review_asset_payload` 仅 attachment_id/kind/sort_order；唯一含 object_key 的 `asset_payload`(:789) 只用于家长自己的提交附件(:386) | 本机复核 | **P1**（Wave-2 裁定「A5 已实现 object_key」与事实不符——当时未复核到位，A0 承认漏审）→ MN-MEDIA-03 家长侧价值归零 |
| P1-2 | **HEAD 既有**：家长作业首页断裂——`teaching_assignment_logic.erl:272` task_id 直传 varchar，`parent-api.ts:152` `tsidOrThrow` 必抛 | git show HEAD 证实既有 | **P1（既有缺陷，非本 RUN 引入）**→ MN-WITHDRAW 入口真实链不可达 |
| P1-3 | 图片-only 回评发布死路：前端 `review-flow.ts:206` canPublish 放行 assets>0；后端 `review_has_content`(:577) 只认文本+video→必 5485 | 本机复核 | **P1** |

另有 10 条 P2（独立发现含：并发同 key 回读窗口 500、archived learner 未过滤、advanceToNext 只取第一页、时间微秒格式等）——详见独立报告。

### 如实改级（契约：证据与声明不一致→停止写码，只更新记录）
- **总结论由 LOCAL_PHASE0_1_15_PASS 改级为 PARTIAL**；MN-TASK-03=FAIL、MN-MEDIA-03=FAIL、MN-WITHDRAW-01=PARTIAL（功能绿、入口被既有缺陷阻塞）。
- **两候选 patch 均不得按当前状态合入 main**（独立评审 NO-GO，A0 复核同意）。
- 根因教训（系统性）：**双端各自 mock「假形状」互证全绿**——本地单测无法发现跨端类型/形状断层；修复 v3 必须同时建立「后端真实响应样本 → 前端解析器」的契约测试。

### v3 必修清单（等用户授权后执行；涉及 unowned 文件需 A0 扩权并记 ownership.json）
1. task_id 统一为 bigint id（后端 list SQL/create payload + **既有** teaching_assignment_logic/repo 家长链同修 + 契约不变）或全体改 opaque-string 契约（需重写 OpenAPI/前端/契约测试——改动面更大，不推荐）
2. `review_asset_payload` 补 object_key（assets 查询 JOIN attachment.path；teaching_review_repo/logic + OpenAPI 已声明无需改）
3. `review_has_content` 纳入 feedback_image（logic + 测试）
4. 00000105 down 补「视频镜像旧列」第三预检
5. 建议顺带：digest length-prefix 框架化、并发同 key 窗口处理、archived learner 过滤

---

## Round 5（2026-09-10，A0 本机复核——未覆盖面专扫）

> 范围声明：P0-1 / P1-1 / P1-2 / P1-3 四项已各有 ≥2 轮独立确认（契约「同一 blocker
> 连续两次检查无新证据停止该分支」），本轮**不再重复扫**；专攻前四轮未覆盖的
> 后端 repo 层 / 撤回链 / attach 授权链 / Phase 1 契约工件（yaml、contract-check、
> ab_summary）/ moya workbench 发布链路。

### 新发现

| 编号 | 发现 | 证据 | 定级 |
|---|---|---|---|
| N1 | **发布前不落草稿：填完内容直接点发布必报 5485 且文案误导**。`workbench.ts:361-364` onConfirmPublish 直接 `flow.publish()`（POST /reviews/publish 无内容体），此前仅 `canPublish()` 本地 state 预检（:352）；服务端 publish 只认 DB 草稿 → 从未 saveDraft 的用户在 DB 无草稿 → 5485「回评为空」，而 toast 却提示「请至少填写文字点评或添加图片/视频」（用户已填） | workbench.ts:340-364 + review-flow.ts:246-254（publish 不调 saveDraft）+ 后端 publish 只读 DB 草稿 | **P2**（高频用户路径的 UX 死胡同；修复=发布前置 saveDraft）→ **列入 v3 队列第 6 项** |
| N2 | contract-check.py TSID 示例抽查字段清单（:160-161）缺 `task_id`——恰是 P0-1 出事字段；yaml 本身声明正确（task_id: string, :612/:646），但抽查清单应纳入以防 yaml 自相矛盾 | contract-check.py:160-165 | P3 |
| N3 | `teaching_attach_logic.erl:160-161` docstring 仍写「NOT EXISTS submission_asset 守卫」，实际 `unbound_run` SQL 已是 double 排除（submission_asset ∪ review_asset，teaching_submission_repo.erl:583-588）——注释过期，照抄注释会误解清理语义 | 同左 | P3 |

### 正面排除（本轮查证为正确，防后续误报）

1. **ON CONFLICT 推断谓词完整**：`teaching_task_repo.erl:83-85` `ON CONFLICT (creator_id, group_id, idempotency_key) WHERE idempotency_key IS NOT NULL DO NOTHING` 与 00000106 部分唯一索引精确匹配——部分索引推断要求 WHERE 谓词，此处没漏（与集成 183/183 真库绿一致）。
2. **authorize_review_asset 是活代码**：接线链 `attach_logic.erl:435 → teaching_attach_logic:authorize/2 (:123) → authorize_review_asset`；draft=reviewer 本人+submitted / published=submission_access / 其余全 false，deny-by-default 成立。
3. **孤儿清理不误删 review 素材**：`unbound_run` double NOT EXISTS 同时排除 submission_asset 与 review_asset 引用（含草稿引用）；附件侧清理对已绑 review_asset 的素材免疫。ecron 定时入口 `run_unbound_cleanup` 未接线（Step 10 既有遗留，非本轮引入）。
4. **撤回后端链干净**：ACL 强制 guardian+submit（staff/owner 一律 not_guardian，:186-188）；lock-first 与 publish 同串行化点；UPDATE 双守卫（status='submitted' AND NOT EXISTS published review）→ 0 行判 5481；draft submission 误撤请求 fail-closed 报 already_withdrawn。MN-WITHDRAW-01=PARTIAL 归因 P1-2 家长入口断链，后端无新问题。
5. **openapi/moya-teaching.yaml 与冻结契约一致**：task_id 声明 string+十进制 TSID 示例（:612/:646）——反向佐证 P0-1 为**实现违约**（后端返回 `task_xxx` varchar）而非契约漂移。
6. **ab_summary.py 诚实**：三门判定与 protocol-v0 §8 冻结口径逐条对应（B 独有样本级 ≥6、severity 不增、中位修改时长不涨）；note 声明离线模板属性，无 mock 冒充 PASS。
7. review-flow.ts 其余面自洽：video_attachment_id+assets 双写与后端 legacy_video_consistent 校验匹配；advanceToNext 跳过当前 submission、withdrawn 由服务端 queue 恒过滤。

### v3 必修清单（更新：5 项 → 6 项，仍等用户授权）

1. task_id 统一为 bigint id（含既有家长链同修）或全体 opaque-string（不推荐）
2. `review_asset_payload` 补 object_key
3. `review_has_content` 纳入 feedback_image
4. 00000105 down 补「视频镜像旧列」第三预检
5. 建议顺带：digest length-prefix 框架化、并发同 key 窗口、archived learner 过滤
6. **（Round 5 新增）发布前置 saveDraft**：onConfirmPublish 先 `await flow.saveDraft()` 再 publish（或 flow.publish() 内首步 save），消灭「填了内容直接发布→5485」死胡同

---

## Round 6（2026-09-10，A0 本机复核——ACL 核心面收官轮）

> 范围：前五轮从未审计的最后一块核心面——`teaching_acl.erl` 授权原语本体、
> ACL 三原语的全部生产调用点组合、review/submission 双侧素材归属校验、
> learner_readiness 守卫 SQL、parent_view D-10 组装。

### 结论：零新 P 级缺陷（正面确认轮）

| # | 查证项 | 结果 |
|---|---|---|
| 1 | `teaching_acl.erl`（213 行全文） | 干净：deny-by-default 完整（关系缺失/inactive/NULL org/查询异常全 fail-closed）；submission_access 按 T14 顺序（org NULL→not_found 不确认存在性）；staff 与 guardian 双路径均带跨机构双保险（group_org/learner_org vs 资源 org）；Owner 显式 owner_not_granted（T5）；unknown role 白名单拒绝 |
| 2 | ACL 三原语全部生产调用点组合 | 安全：review 域（detail/withdraw/workbench/publish :48/:177/:198/:422）全经 submission_access 前置再 resolve_staff(write)/resolve_guardian(submit)；resolve_guardian 直调仅两处（家长 assignment 域 :27/:89/:148、history :662），资源粒度=learner、监护人关系本身即授权依据，语义成立 |
| 3 | review 侧 `validate_assets_tx`（review_repo:160-183 + check_each_asset） | 干净：creator_user_id 强归属（评审员无法把他人私密素材挂进回评再发布——本轮重点怀疑面被证伪）；不存在与非本人同响应 not_found（防存在性探测 T14）；status>=0 + scope='teaching' + MIME↔kind 匹配逐条递归比对 |
| 4 | submission 侧 `validate_assets`（submission_repo:144-166） | 干净：同样强归属（NotOwned→not_owned）、数量不符→not_found |
| 5 | `learner_readiness` 守卫 SQL（task_repo） | 干净：IN 子句参数化；缺行补 learner_not_in_class；三分判定（enrolled=false→not_in_class；learner_org≠OrgId 含 NULL→not_in_class 统一拒绝不泄漏；submit_guardians≠1→guardian_setup_required，恰一才 ok 不猜默认监护人） |
| 6 | `digest_check` / `in_clause` | digest 异常形态（epgsql text 模式）保守判 idempotency_conflict；占位符参数化无注入面 |
| 7 | parent_view / published_review_payload（D-10） | 成立：map 字面量白名单构造（防御性剥离）；家长侧 AI 仅 ai_status_hint 三态枚举，永无草稿内容；published_review 仅已发布可见。**P1-1 链路在此再次坐实**（parent_view→published_review_payload→review_asset_payload 缺 object_key，已知） |
| 8 | AI Worker `claim_next_queued_tx` | 单语句条件 UPDATE + FOR UPDATE SKIP LOCKED 原子抢占；ai_task_id 兼任重试计数 |

### 累计状态（Round 1-6）

- **缺陷台账**：P0-1、P1-1、P1-2（既有）、P1-3 + 10 条 P2 + N1(P2)+N2/N3(P3)——v3 必修清单 6 项不变，仍等用户授权。
- **核心安全面审计已全覆盖**：ACL 本体、调用组合、素材归属（双侧）、读授权数据面（review_for_asset_path）、附件 viewUrl 授权链、撤回事务、幂等事务、就绪守卫——无未审的后端安全原语。
- 剩余未审面仅为：moya 页面组件细节（classes.ts/tasks.ts/upload UI）与 5 个测试文件的断言细节；对 v3 排序无影响。

---

## Round 7（2026-09-10，A0 本机复核——ACL 数据源 + 写路径 + 迁移原文收官）

> 范围：teaching_context_repo（ACL 数据源 SQL 全文）、learner_bind 写路径、
> 00000105 up.sql 原文逐条、错误码三端对齐、AI provider/worker 隐私面。

### 结论：零 P0/P1/P2 新发现；新增 2 条 P3 硬化建议

**新发现（P3，并入 v3 建议项⑤，不改变必修排序）**

| 编号 | 发现 | 说明 |
|---|---|---|
| H1 | `submission_scope` 同时 SELECT `hs.learner_id` 与 `a.learner_id AS assignment_learner_id`，但 ACL 不比对两者一致性 | 写入路径受控（提交 API 校验 learner 匹配）时无害；属脏数据防御硬化——建议 ACL 或提交链加一致性断言 |
| H2 | `guardian_relation`/`learner_org` 不查 `learner.status` | learner 档案停用后 active guardian 关系仍可 submit/view_review；由绑定管理流程兜底，建议 resolve_guardian 链加 learner active 校验 |

### 正面确认（本轮查证）

1. **teaching_context_repo 全文（260 行）**：guardian_contexts LEFT JOIN 语义正确（未入班仍合法上下文）；staff_contexts 内 JOIN organization（机构 NULL 群不构成教学上下文）；relation 查询带 status 供 ACL 区分 not_found/inactive；submission/assignment scope 全 INNER JOIN 任一跳缺失即 not_found；SQL 全参数化；group 保留字处理正确。
2. **00000105 up.sql 逐条**：双 FK（CASCADE/RESTRICT）、attachment 全表 UNIQUE、单视频部分唯一索引、3 图 CONSTRAINT TRIGGER（AFTER INSERT OR UPDATE、DEFERRABLE INITIALLY IMMEDIATE、语句末新鲜快照 >3 拦截）均与声明一致；回填 NOT EXISTS + ON CONFLICT 双保险幂等；合成 TSID `(now-base)<<21|row_number` 与真实 TSID 无撞号可能（合成值 <1.13e17，任何 1971 年后创建的真实 TSID >3.6e18，数量级论证与 node 配置无关）。
3. **learner_bind 写路径**：operator_role_tx 事务内先验（owner：learner.organization_id 匹配 + owner_id；manager：同班 active class_staff manager）→ TOCTOU-free；成功动作与 teaching_admin_audit 同事务；审计不含 display_name；被拒动作日志级；本波未挂路由（攻击面不存在）。
4. **错误码三端对齐**：error_code.hrl 宏值（5430/5431/5432/5480/5481/5482/5485/5423/5443/5444）= teaching_error.erl 集中映射 = handler 整数传码（「值同待切宏」已知偏差无行为差异）= moya teacher-error-copy.ts 数值。
5. **AI provider/worker**：api_key 缺失→provider_unavailable 明确降级不重试（AI-03），key 值不落日志；prompt 仅携带业务元数据 + 附件 object_key 引用（不携带姓名/微信身份，protocol §9 最小请求）；输出白名单字段校验、坏输出 fail-closed；不记录思维链。

### 累计台账（Round 1-7 终版）

- **P0×1**（P0-1 task_id 双形态+重试死锁）、**P1×3**（object_key 缺失 / 家长列表既有断裂 / 图片-only 发布 5485）、**P2×11**（含 N1 发布不落草稿）、**P3×4**（N2 contract-check 抽查清单、N3 attach 注释、H1 双 learner_id 比对、H2 learner status）。
- **v3 必修 6 项不变**（仍等用户授权）；H1/H2 并入建议项⑤。
- **审计覆盖终态**：后端安全原语（ACL+数据源+写路径+事务+迁移+AI 面）与契约工件全部审毕；剩余未审仅 moya 页面组件细节与 5 个测试文件断言（对 v3 无影响）。后续再review的边际收益已尽，建议转入 v3 执行。

---

## Round 8（2026-09-10，A0 本机复核——moya 测试面收官：假绿机理坐实）

> 范围：5 个 moya 测试文件的夹具形状与断言、classes/tasks 页面、workbench
> onLoad 参数校验与上传链（presign→PUT→confirm）、幂等键生成。

### 新发现

| 编号 | 发现 | 影响 |
|---|---|---|
| N4 | **P0-1 假绿机理在测试夹具层精确坐实**：teacher-api.test.ts（:305/:317/:374）、teacher-tasks-page.test.ts（:204/:243）、parent-api.test.ts（:37）全部用**短数字串**（"9001"/"1001"）mock task_id——`tsidOrThrow` 对短数字通过，对真实后端 `task_xxx` 必抛；三份夹具无一使用 19 位合法 TSID 或真实后端形状 | **v3 ① 实施注意**：修 task_id 后这些夹具必须同步改为 19 位 TSID，否则测试依旧测不到真实形状（双 mock 假绿再现）；v3 ⑤ 契约测试的必要性由此加固 |
| N5 | **P1-3 在双端测试都是盲区**（定性修正）：review-flow.test.ts 只断言「全空→false」「文字→true」「publishBlocked→false」，无 image-only 断言；后端 review_has_content 测试同样只覆盖文本+视频 | P1-3 非「被测试固化」而是「双端都未测」；**v3 ③ 实施注意**：前后端须同步补 image-only 用例 |

### 正面确认

1. workbench onLoad（:103-112）isTsid 校验路由参数，非法 id 拒绝渲染。
2. Idempotency-Key 生成（request.ts:50-53）：时间戳 base36 + ~82bit 随机；表单级 key 复用（tasks.ts:353，用户明确重试同 key）；POST 默认不自动重试（:127）；后端唯一键 (creator_id, group_id, key) + 异 digest 5460 fail-closed 兜底。
3. workbench 上传链：presign→PUT→confirm；confirm 由后端 HEAD 真实 MIME 复核（前端 onRecordVideo 硬编码 video/mp4 仅 presign 声明值，iPhone .mov 实际 quicktime 由后端白名单放行，无危害）；多图串行上传失败即停、已成功项保留。
4. classes.ts：isTsid 校验跳转参数、group_id 去重、pending 计数，无 Number() 强转。
5. parent-api.test.ts withdraw 组测试质量高：仅 payload.status=withdrawn 被接受、非 withdrawn 拒绝（防假成功）、5481/5444 错误码透传断言；parent-submission-detail-page.test.ts 覆盖 withdrawn 视图。

### 终态声明（Round 1-8）

- 台账不变：P0×1 / P1×3 / P2×11 / P3×4；N4/N5 为 v3 ①/③ 的实施注意事项与定性修正，不新增必修项。
- **v3 必修 6 项不变，仍等用户授权**。
- 审计覆盖终态确认：前后端全部代码、迁移、契约工件、测试面均已至少一遍第一手审阅。**review 循环到此边际收益为零，后续「review代码和文档」请求将不再产生新输出，请拍板 v3 或四项外部门 Gate。**

---

## Round 9（2026-09-10，A0 本机复核——Phase 1 评测工件交叉一致性 + 证据红线实测）

> 范围：rubric-v0 ↔ ab-summary-template.csv ↔ ab_summary.py ↔ manifest.schema.json ↔
> fixtures 的字段/口径交叉核验；证据目录全量红线扫描（契约硬边界：仓库内不得保存
> AppID/openid/token/presigned URL/PII）；baseline.json 脱敏；计划 SHA 复核。

### 新发现

| 编号 | 发现 | 定级与处置 |
|---|---|---|
| N6 | **ab_summary.py 质量门比冻结口径宽松**：计划 MN-AI-04 原文「**具体且可行动**建议比例≥90%」（计划 SHA b1c9f005… 复核一致），rubric-v0:118 将其操作化为 `d2≥4 且 d3≥4`；ab_summary.py 的 act() 只查 `d3_actionable>=4`，**漏 d2_specific**。Phase 2B 未跑、无数据污染，但门工具必须在看到数据前与冻结口径一致（预冻结不得补造） | **P2** → **v3 必修清单第 7 项**（act 条件补 d2_specific>=4，docstring 同步；一行修正） |
| N7 | **acceptance-matrix.md 在 Round 4 降级时漏更新**：结论行仍为 LOCAL_PASS、MN-TASK-03/MN-MEDIA-03=PASS、MN-WITHDRAW-01=PASS——证据目录内部自相矛盾（final-report 有降级声明、matrix 没有） | 记录级 → **已当场修复**：矩阵顶部加 ROUND4-DOWNGRADE 注记块（保留原快照、指明现行权威口径、禁止据此恢复 PASS） |

### 正面确认

1. **证据目录红线实测零泄露**（契约硬边界验证）：全目录 grep AppID 形态/openid/Bearer/presign/手机号/真实邮箱，仅命中 3 处文档自述（「MUST NOT contain openids」类声明本身）；baseline.json 仅记录「现实 AppID 暂存输入存在且禁读」的事实，不含值本身。
2. **评测工具链字段闭环**：ab-summary-template.csv 列名 ⊇ ab_summary.py 全部读取字段；rubric 五维度 1-5 量纲与 d3≥4 判定匹配；manifest.schema.json 的 revoked 拒收（`then:{not:{}}` 恒假）与 protocol §3「schema 层已拒收」声明一致；20 份 fixtures 全部含 consent_status/deidentification、值全合成。
3. **计划文件完整性**：主仓计划 SHA-256 = b1c9f005ee0114bc5e03cb22424cd0fc73ec8639901bab585e42b0c84c81c9b1，与权威契约逐字一致（rebaseline 树无此文件属正常——以主仓工作区为冻结真源）。

### 台账终版（Round 1-9）

- **P0×1 / P1×3 / P2×12（+N6）/ P3×4**；v3 必修清单 **6→7 项**（⑦ ab_summary 门补 d2_specific）。
- **审计覆盖绝对终态**：代码、迁移、契约工件、测试面、评测工具链、证据合规——全部第一手审毕；本轮两发现均为工具/记录级（无业务代码新缺陷）。**review 循环至此彻底收敛，任何后续 review 请求将指向本文件终版声明而不产生新一轮扫描。**

---

## v3 修复轮（2026-09-11，用户授权「发现的所有 patch」后 A0 执行）

**实施清单（9 项全落）**：① task_id 对外统一 group_task.id 十进制串（list SQL t.id AS task_id / task_item tsid 化 / ds payload integer_to_binary / assignment_summary+detail 改 task_gid，内部 varchar 链路不动）；② review_asset_payload 补 object_key（assets SQL JOIN attachment.path）；③ review_has_content(Draft, Assets) 纳入 feedback_image + publish_precheck 回读 assets；④ 00000105 down 补视频镜像一致性第三预检；⑤ digest 32 位长度前缀框架化 / 幂等改两段式（修并发同 key 语句级快照窗口→insert_failed 500）/ learner_readiness 加 l.status='active' / H1 submission_access 双 learner_id 比对（不一致按 not_found）/ H2 guardian_relation LEFT JOIN learner + resolve_guardian 校验 learner_status；⑥ workbench onConfirmPublish 前置 saveDraft；⑦ ab_summary act 条件改 d2∧d3；N2 contract-check TSID 清单含 task_id；N3 attach 注释对齐；N4 三测试文件夹具换 19 位 TSID（9001→975001000000000019 等 5 个值域）；N5 前端补 image-only canPublish 用例 + 后端新建 teaching_review_logic_tests（4 用例）。

**门结果（全绿）**：imboy 同口径 11 套件 183/183 + 新套件 4/4；make compile exit0；全量单跑 220 passed / 0 failed（唯一 cancel=adm_moment_handler 幽灵：BUILD-00R ERLC_EXCLUDE 物理裁剪使 beam 缺席，而 erlang.mk EUNIT_EBIN_MODS 按 ERL_FILES 全量列名——**既有交互问题，非本 RUN 引入**，v2 按套件口径同样不受影响）。moya：typecheck/lint/build/scan exit0 + tests 168/168（167+新增 1）。contract-check PASS。ab_summary 模板自验 gate_ai04 采 d2∧d3=1.0 PASS。00000105 down 真库事务内执行+ROLLBACK 验证 exit0（预检无误报）。

**权威候选 v3**：imboy `final/candidate-imboy-v3-a7e19a26.patch`（67 文件，SHA-256 05a3f5011a1042042c4d908e9e389b4af52436bd190a0dfce1106a14056a3e6b）；moya `final/candidate-moya-v3-1a636660.patch`（21 文件，SHA-256 2e4c415518adbe916c8acdff21d19f6210b7006bb9b256a92dbb390883feaffb）。v2 作废。
