# STEP-14 设计决策、冻结增量与风险 — 家长端

> Agent D（MOYA）。UX 依 §2.2/2.4/2.5/2.6 + 状态矩阵家长行；错误文案真源 STEP-03/error-copy.md。

## 结构（家长域自主层，公共壳零改动）

```
src/pages/parent/
├── core/                  # 家长域逻辑层（可 mock 单测）
│   ├── parent-api.ts      # 契约 DTO + 防御性过滤（ai_draft/未发布 review 剥离）
│   ├── error-copy.ts      # ApiError → 家长文案（error-copy.md 真源映射）
│   ├── media-validate.ts  # ≤60s 硬约束 + chooseMedia 参数 + 拍摄要求文案
│   ├── submit-flow.ts     # 提交状态机（幂等键生命周期/防重/取消vs失败/重试）
│   └── learner-session.ts # 多孩派生/记忆/缓存隔离（clearParentCache）
├── home/                  # 作业列表：孩子切换条 + 5 状态筛选 chips + 卡片
├── assignment-detail/     # 详情：拍摄要求卡 + attempt 时间线 + 开始提交
├── submit/                # 提交流：选择/校验/预览/进度/取消/重试/幂等提交/AI 等待
├── submission-detail/     # 提交详情：我的提交 + 已发布回评 + 去重练
├── growth/                # 成长时间线（倒序，仅已发布回评）
└── profile/              # 孩子切换 + 身份切换 + 隐私说明
```

公共壳（core/、components/、index/identity-picker/no-identity、teacher/*）零改动；仅 app.json 追加 3 个新页注册（任务书允许的必要接线）。

## 关键决策

1. **家长域逻辑放 `pages/parent/core/`** 而非顶层 core/：公共壳已冻结，家长域自主演进不污染冻结面；给 Teacher Agent 的对称建议是 `pages/teacher/core/`。
2. **幂等键生命周期**：一次"逻辑提交"（选定内容）一键；失败重试（上传/提交）复用同键；附件内容任何变化（换视频/加/删照片）换键——对应服务端 5460（同 key 不同 body 拒绝）防线。防重复点击用 inFlight Promise 复用（submitting 中双击只发一次）。
3. **取消 ≠ 失败**：uploadAll 以 cancelSignal 区分——取消回 videoReady 保留选择；失败进 uploadFailed（保留待提交态 + 显式"继续上传"按钮）。断点续传感属网关内部职责（真实现接 attachment presign/confirm 时做分片续传），状态机层重试=重新走上传流程。
4. **附件网关接口化**（AttachmentGateway）：页面注入"待联调占位网关"（上传即失败→UI 呈现上传失败+重试，诚实不虚构）；Step 17 接 imboy `/api/v1/attachment/{presign,confirm}`（契约不重定义附件端点）。测试注入可编程网关。
5. **防御性过滤在 DTO 边界**：parse 层白名单提取（published_review 必须 review_id+published_at 齐全才算已发布），ai_draft/my_review_draft 即使服务端误发也在 parse 消失——页面无需也不能看到这些字段（D-10/PARENT-02）。
6. **多孩缓存隔离**：内存缓存 Map 以 `${learnerId}:${name}` 为 key；switchLearner = 落记忆（moya.parent.active_learner）+ clearParentCache()；页面切换时先 setData 清列表再 loading（防旧孩子数据闪现）。
7. **错误文案映射在家长域 error-copy.ts**：core/errors.ts 冻结不能加码表 → 补充码（5422/5423/5426/544x/546x/5481）本地 CODE 常量，文案逐条对齐 error-copy.md。

## 已知风险

1. **附件上传未联调**（占位网关恒失败）：真机提交流程走到"上传失败+继续上传"循环属预期；Step 17 接 presign/confirm 后即通。真实分片进度/断点续传语义由网关实现兑现。
2. **PARENT-03 BLOCKED_EXTERNAL**：真机录制（chooseMedia 实调）、后台切回恢复、真上传——本波无法验证（微信工具未装 + 后端未实现），已在验收标注。
3. **视图层未做组件测试**：页面 TS 的 UI 组装（setData/wxml）无自动化（node 环境无组件渲染器）；逻辑全部下沉 core 层测试覆盖。真机走查留 Step 17。
4. **列表分页仅首页**：fetchAssignments 拉第 1 页（size 20），上拉加载未实现——单班试点数据量内可接受，Step 17/后续补 onReachBottom。
5. **视频播放/照片大图**：submission-detail 页当前展示回评文字四要素 + 重练入口；视频播放引用（video_object_key → view_url 短时授权链路）依赖附件端点，与风险 1 同批 Step 17 接入。
6. **photos 在 submission-detail 的展示**为 attachment_id 占位（未取临时 URL），联调后换成授权 viewUrl。
7. submit 页 `wx.showToast` 用于就地校验提示（≤60s 拒绝）——toast 文案含原因，符合 UX"就地提示原因，不跳走"。

## 给 Teacher Agent（Step 15）的接口参考

- 对称结构 `pages/teacher/core/`（teacher-api/error-copy/queue 状态机等）；
- 可复用（勿改签名）：`parentErrorCopy` 模式（老师文案表另建）、`media-validate`（chooseMedia 参数与 ≤60s 校验通用）、`learner-session` 的缓存模式（老师侧建议 class/queue 维度）；
- `SubmitFlow` 的幂等键/防重/取消语义可直接参考（teacher review-draft 保存幂等同理）。
