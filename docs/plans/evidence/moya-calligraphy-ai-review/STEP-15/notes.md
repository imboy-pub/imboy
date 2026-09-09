# STEP-15 设计决策与风险 — 老师端

> Agent D（MOYA）。UX 依 §2.3 老师流程 + 状态矩阵老师行 + §2.6 工作台线框；文案真源 STEP-03/error-copy.md 老师列。

## 结构（对称家长域，公共壳与家长端零改动）

```
src/pages/teacher/
├── core/
│   ├── teacher-api.ts        # 契约 DTO（ReviewQueueItem/Workbench/AiDraft 四态/TeacherReview/PublishResult）+ 4 端点
│   ├── teacher-error-copy.ts # 老师文案（含 5425 publishBlocked 专属信号）
│   └── review-flow.ts        # ReviewWorkbenchFlow（AI 核对/草稿幂等防重/发布守卫）+ advanceToNext（连续处理）
├── home/        # 待评队列：班级 picker（contexts 派生）+ AI 状态 chips + 卡片（重练标记）
├── workbench/   # 工作台：学员提交占位 + AI 观察核对（采用/标错）+ 四要素 + 录制（≤60s）+ 发布确认弹层 + 连续处理
├── tasks/       # 作业视图：从队列按 assignment 分组（待评数/AI 进度）
├── classes/     # 班级视图：contexts 任教身份派生 + 每班待评数
└── profile/     # 身份切换 + 缓存清理（reLaunch 重登）+ 录制权限/隐私说明
```

## 关键决策

1. **对称家长域结构**（Step 14 notes 建议）：逻辑全部下沉 `pages/teacher/core/`，页面只做 UI 驱动；复用家长域 `media-validate`（chooseMedia 参数 + ≤60s 校验，import 跨域只读，不修改）。
2. **AI 四态不阻塞**：workbench 对 queued/running/failed/none 一律可进入并直接人工填写（契约描述同语义）；succeeded 才渲染草稿核对区（采用/标错；"修改"= 采用后继续编辑 → action=edited，计入老师反馈统计语义）。
3. **发布幂等语义对齐契约**：publish 天然幂等（already_published=true 不抛错）；页面发布确认弹层呈现学员名+可见范围+不可撤回；成功（含幂等重放）→ advanceToNext 刷新队列 → redirectTo 下一条（1.2s 成功态）或完成态。
4. **5425 专属处理链**：errorCopy.publishBlocked → flow.publishBlocked → 页面隐藏发布按钮 + 显示"已保存草稿，发布需任课老师操作"（error-copy.md 规定动作）。
5. **草稿保存幂等防重**：PUT upsert per (submission, reviewer)；inFlight Promise 复用防双击；保存成功不通知家长（draft 状态提示）。
6. **tasks/classes 无专属契约端点**：从 review-queue/teaching-contexts 派生最小视图（提交进度/任教班级+待评数）；发布作业、学员名册属后续 Step（契约未定义端点，不发明 API）。
7. **withdrawn 即时移除**：服务端过滤 status=submitted（契约）；客户端对 5482 的呈现是"进入下一条"而非错误中断。

## 已知风险

1. **附件同家长端待联调**：学员视频/照片播放、老师点评视频上传均为占位展示（录制选择与 ≤60s 校验已实现，attachment_id 落"0"占位）；Step 17 接 attachment presign/confirm/view_url 后兑现。
2. **TEACHER-03 BLOCKED_EXTERNAL**：真机录制/选择/预览/取消/重试/发布流程未验证（无微信工具+后端）。
3. **workbench 录制后视频附件为占位 ID "0"**：真实发布前必须替换为 confirm 后的 attachment_id（否则服务端 5441 拒绝——契约守卫恰好兜底）；已在代码注释标注。
4. **queueFilters 未从路由带入 workbench**：advanceToNext 当前用空 filters（全队列推进）；若老师从筛选视图进入，下一条应保持同筛选——小改进点，Step 17 前补（页面间传参 via route）。
5. **页面 UI 组装层无自动化**（同 Step 14）：逻辑下沉 core 已测；真机走查留 Step 17。
6. **连续处理 redirect 用 navigateTo 栈**：workbench redirectTo 自身路径（替换当前页，栈不累积）；多次连续处理下页面栈稳定。
