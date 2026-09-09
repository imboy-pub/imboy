# 墨芽教学域错误码三端一致性审计（R15）

> **范围**：冻结契约（本目录 `error-codes.md`）↔ 后端实现（`imboy/include/error_code.hrl` 5400-5519 段 + 全仓发射点）↔ 小程序客户端映射（moya `src/core/errors.ts` + `src/pages/parent/core/error-copy.ts` + `src/pages/teacher/core/teacher-error-copy.ts`）。
> **方法**：三端全量取数交叉比对 + 发射点 grep 实证（R15，Coordinator）。

## 结论：三端一致，无用户可见缺口

1. **后端 27 码与契约逐一对应**：5401-5404 / 5420-5429 / 5440-5444 / 5460-5461 / 5480-5485，`ERROR_MSG_MAP` 中文文案全量；宏名与契约建议名一致。
2. **5501 未实现=符合契约**：契约原文标注「预留，老师侧」，且注明「failed 是正常态」（经 `payload.ai_draft` 三态传达，R8 实测 workbench 可见 `ai_draft.failed+error_code`）——不实现为正确行为。
3. **客户端覆盖所有当前可达码**：core 6 码（5401-5404/5420/5421）+ parent 12 码（5422/5423/5426/5440-5444/5460/5461/5481）+ teacher 10 码（5424/5425/5426/5480-5485 + 5501 防御性映射）+ 通用 400/403/404/409/410/422/429 双端齐。
4. **兜底安全网双层**：moya 请求层 `parseEnvelope` 以 `ApiError("business", env.msg, env.code)` **透传后端中文 msg**——任何未映射码用户仍看到后端文案而非裸码；两份 copy map 的 `default` 分支按设计（STEP-03/error-copy.md）仅在反馈语境显示码号。

## 登记项（非缺口）

| 项 | 状态 | 说明 |
|---|---|---|
| 5427/5428/5429（绑定三码） | 客户端未映射=**正确现态** | 客户端 v1 无绑定 UI（`grep -ri bind src/` 零命中；绑定走机构 API/admin，Step 16 预留）。后端 `teaching_learner_bind_handler:172` 发射 5427，三码中文 msg 齐。⚠ **未来做绑定 UI 时必须把三码加入 copy map（真源 STEP-03/error-copy.md）**——此为该功能的验收项之一 |
| 5426（CROSS_ORG） | 后端预留未发射 | R8/R9 实测跨 Org 走 403（资源型端点）/5423（关系型端点）；客户端已防御性映射，无害 |
| 5404 文案角色分化 | 有意设计 | parent「您还没有绑定孩子档案，请联系老师添加」/ teacher「您还不是任课老师，请联系机构管理员开通」，真源 STEP-03/error-copy.md 双列 |

## 方法备注

契约实测矩阵（`STEP-17-PREP/contract-deviations.md` §1）已实证后端实际发射码 5420/5422/5423/5424/5425/5428/5429/5460/5461/5482/5483 与客户端映射对齐；本次审计补齐其余码、5501 预留语义与兜底链路（msg 透传）核验。
