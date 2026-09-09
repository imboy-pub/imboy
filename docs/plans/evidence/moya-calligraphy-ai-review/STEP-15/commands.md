# STEP-15 命令证据 — 老师端待评队列与视频回评

> Agent D（MOYA），2026-09-09。对 STEP-04 契约（review-queue/workbench/review-draft/publish）+ STEP-03 error-copy.md 老师列编程；全 mock 驱动（真实联调 Step 17）。
> 基线：moya HEAD = `9644a2e`（无 git 写操作；PNG shasum 未变）。

## 质量门（最终态）

| 命令 | 退出码 |
|---|---|
| `npm run check`（typecheck+lint+test+scan 串联） | 0 |
| `npm run typecheck` | 0 |
| `npm run lint`（含全部老师域新代码） | 0 |
| `npm run test`（98 tests / 98 pass / 0 fail；Step 14 后 77 → 98，新增 21） | 0 |
| `npm run scan` | 0 |
| `npm run build`（teacher/{home,workbench,tasks,classes,profile,core} 全部入 dist） | 0 |

依赖清单不变：`dependencies: {}`。

## dist 冒烟（node require CJS）

```text
new ReviewWorkbenchFlow() → canPublish(empty)=false（发布守卫生效）
teacherErrorCopy(ApiError(business, 5425)) → hint="已保存草稿，发布需任课老师" | publishBlocked=true
```

## 过程中的真实失败与修复

1. type-stripping 同 Step 14 经验提前规避（无构造参数属性）；branded Tsid 测试常量统一 assertTsid 构造。
2. `?? "0"` 零值与 branded Tsid 不兼容 → teacher-api 引入 TSID_ZERO 常量。
3. review-flow.publish 返回类型初版自造 `{kind:"published"}` 包装 → 对齐契约 PublishResult 直接透传（already_published 字段名同步修正）。
4. home 队列页补 assignment_id/group_id 路由筛选（tasks/classes 页跳转目标），与契约 query 参数一一对应。
