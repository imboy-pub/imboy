# STEP-15 测试证据 — 老师端

> `npm test`：98 tests / 98 pass / 0 fail（新增 21：teacher-api 14 + review-flow 逻辑）。
> mock 基建沿用 helpers/mock-wx.ts 显式请求队列。

## TEACHER-01 → 测试映射

- **队列筛选**：fetchReviewQueue DTO 解析 + group_id/assignment_id/ai_status query 传递断言。
- **草稿保存**：saveDraft PUT 请求体不含服务端保留字段（reviewer_uid/status/published_id 断言缺失）；保存中双击只发一次 PUT、两调用同结果（幂等防重）。
- **重复发布保护**：publish already_published=true → code=0 不抛错（幂等友好），PublishResult.already_published 正确解析；confirm_learner_id 随请求体发送。
- **无 AI 人工回评**：workbench ai_draft=null → 纯人工路径可用；canPublish 文字四要素 ≥1 即 true；failed draft → result=null 但工作台可进入。
- **发布失败恢复**：5485（空内容）失败后草稿内容保留（getFields 不变）→ 重试成功；失败不丢内容。
- **连续处理**：advanceToNext 排除当前 submission 返回下一条；队列空/只剩当前 → null（完成态，页面呈现"全部回评完成"）。

## TEACHER-02 → 测试映射

- **非任课老师 5424**：队列拒绝 → "您不是这个班的任课老师"（retryable=false）。
- **assistant 无发布权 5425**：teacherErrorCopy → hint="已保存草稿，发布需任课老师" + publishBlocked=true（页面隐藏发布按钮、显示"已保存草稿，发布需任课老师操作"）；flow.publishBlocked=true → canPublish=false。
- **removed staff 5421**：workbench 拒绝 → "该身份已失效（可能已被移除），请重新登录"。
- **5483 确认不一致**：发布拒绝 → "确认信息与学员不符，请重新核对"（发布确认弹层防线）。
- **5482 withdrawn**：发布拒绝 → "家长已撤回这份提交，无需回评 / 将进入下一条"（队列推进语义）。

## AI 草稿核对（采用/修改/标错）

- 采用 → 三要素填入（positive/focus/practice）；随后编辑 → action=edited；标错 → 内容不动、action=rejected。
- AI 四态（succeeded/failed/running/none）全部"可进入工作台"（DTO 层验证）；needs_human_check → 警示标记。
- myDraft 草稿恢复回填（含 rework_required）。

## 其他

- MOYA-ID-01 延续：SUB/LEARNER/GROUP 等 TSID 极值字符串贯穿（含 dist 冒烟）。
- 老师文案表与 error-copy.md 老师列逐条对齐（5404/5421/5424/5425/5480-5485/5501）。
