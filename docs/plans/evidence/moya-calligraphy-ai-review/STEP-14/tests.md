# STEP-14 测试证据 — 家长端

> `npm test`：77 tests / 77 pass / 0 fail（Step 13 后 52 → 77，新增 25）。
> mock 基建沿用 `tests/helpers/mock-wx.ts`（显式请求队列防假绿）；附件网关为可编程注入（成功/失败/进度序列）。

## PARENT-01 → 测试映射（五类路径全覆盖）

| 路径 | 测试 |
|---|---|
| 成功 | submit-flow：选视频→上传(1视频+2照片)→提交→done（断言 assets 请求体形态/sort_order）；重练入口 begin(reworkOfAttempt=1)→服务端 attempt=2 |
| 空态 | parent-api 无需 mock 空（DTO 层）；页面空态由 state-view 呈现（home/growth `phase==='empty'` 分支 + 引导文案）；learner-session 无绑定孩子 → 空态引导 |
| 上传失败 | submit-flow：第 2 附件 fail → uploadFailed 且选择保留（video/photos 不变）→ retryUpload 成功（网关计数 2+2）；取消上传 → 回 videoReady 保留选择（真 bug 修复后） |
| AI 失败 | parent-api：ai_status_hint 仅三态解析（processing/done/none），AI 失败对家长不可感知为错误（"整理遇到问题·不影响老师回评"由 aiHint 呈现 none 分支）；SubmissionCreated.ai_status=queued 解析 |
| 重练 | submit-flow 重练标注 + submission-detail 页"去重练"入口（onRework 直达原作业提交页，attempt 由服务端分配）；assignment-detail 时间线多 attempt 可回看 |

## PARENT-02 → 测试映射

- **篡改 learner_id 被拒**：parent-api —— fetchAssignments(他人 learner) mock 5422 → ApiError(business, 5422) + parentErrorCopy → "您未监护该孩子，无法查看"（retryable=false）；直接访问他人 submission mock 403 → "无权访问该内容"；无监护提交权限 5423 → "您没有为这个孩子提交作业的权限"。
- **不串孩子/不闪现**：learner-session —— switchLearner 落记忆 + 家长域缓存全清（A/B 孩子的 assignments/history 缓存清空断言）；缓存 key 以 learnerId 隔离（同名互不覆盖）；记忆失配（孩子被移除）回退列表第一个。
- **不渲染 AI 草稿/未发布回评（防御性过滤）**：parent-api —— payload 夹带 ai_draft/my_review_draft → parse 后字段消失（JSON.stringify 断言不含）；published_review 缺 published_at → 剥离为 null；history 嵌套泄漏 ai_draft 连同剥掉。

## 其他覆盖

- **幂等（契约 §submissions）**：createSubmission 携带 Idempotency-Key 头；同键重试（submitFailed→retrySubmit）断言两次请求同键且 idempotent_replayed=true；附件内容变化（换视频/加/删照片）换键（5460 防线）；防重复点击：submitting 中双击 → 只发一次 POST、两调用同结果。
- **≤60s 硬约束**：media-validate —— 60s 含边界通过、61s 拒绝（文案含当前时长）、0/负/NaN/∞ 拒绝；chooseMedia maxDuration 与常量一致性。
- **MOYA-ID-01 延续**：int64 上界 "9223372036854775807" 作为 assignment_id 贯穿 DTO 解析保持 string。
- **文案表对齐**：parentErrorCopy 映射 5401-5481 号段 + 兜底（未知码含"错误码 N 仅用于反馈"），ACL 拒绝类 retryable=false。
