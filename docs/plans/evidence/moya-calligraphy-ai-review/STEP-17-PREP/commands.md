# STEP-17-PREP 命令与测试证据 — 附件链路真实现 + 两处改进

> Agent D（MOYA），2026-09-09。moya 侧从"占位网关"升级为"按 imboy 真实附件 API 编程 + mock 测试"；后端联调仍留 Step 17。

## 质量门（最终态）

| 命令 | 退出码 |
|---|---|
| `npm run check`（typecheck+lint+test+scan） | 0 |
| `npm run typecheck` | 0 |
| `npm run lint` | 0 |
| `npm run test`（108 tests / 108 pass / 0 fail；Step 15 后 98 → 108，新增 10） | 0 |
| `npm run scan` | 0 |
| `npm run build`（core/attachment.js 入 dist，冒烟通过） | 0 |

## 新增测试（tests/attachment.test.ts，10 例）

- **完整链路**：presign 请求形态（filename/mime_type/scope query）→ transport 进度回调 → confirm 请求体（object_key/file_hash256 空/mime_type/size/scope）→ attachmentId 落真实 ID；进度序列 [50,100] 断言。
- **fallback**：confirm 响应缺 attachment_id（当前 imboy 真实形态）→ 从 `u<Uid>/...` 键首段提取数字回退（防崩，非正确语义——gap 见 notes）。
- **上传中断重试**：传输首次失败 → 重试从 presign 重新走 → confirm 恰好 1 次（不重复落库，服务端 upsert 幂等对偶验证）；presign 恰好 2 次。
- **取消**：signal 置位 → 不上传、不 confirm（0 次 confirm 断言）。
- **presign 业务失败**（400 不支持类型）→ ApiError(business)。
- **confirm 幂等**：同 key 重复 confirm 返回同 ID。
- **view_url**：首次签发 → 缓存期内复用（请求数 1）；过期（清缓存模拟 TTL）重取（请求数 2，新签名）；鉴权拒绝 → ApiError。
- **端到端集成**：SubmitFlow + 真网关（视频+照片两段上传）→ createSubmission 请求体 assets 断言 `practice_video:7036874417766401`、`final_photo:7036874417766402`（真实 confirm 产物，非 "0" 占位）。

## 两处改进（无独立测试，行为由页面逻辑承载）

1. **queueFilters 路由保持**：teacher home → workbench 透传 `q_group_id/q_assignment_id/q_ai_status`；发布成功 advanceToNext 用同一筛选；redirectTo 下一条时继续透传。
2. **上拉分页**：parent home 与 teacher home `onReachBottom`（page+1 追加、loadingMore 防重入、末页判定 total 缺省以本页满额推测、翻页失败静默保留已加载）。

## 既有测试不回归

98 个既有用例（tsid/format/request/session-context/id-roundtrip/no-token-log/parent-api/submit-flow/learner-session/media-validate/teacher-api/review-flow）全部保持通过；家长/老师 API 层零签名改动（attachment.ts 为纯新增；teacher-api 仅 WorkbenchView assets 增加可选 objectKey 字段，向后兼容）。

## 过程修复

1. fallback 初版"object_key 整串当 TSID"——真实 Garage 键含 `/` 过不了校验 → 改为提取键首段数字（并意识到回退语义是 Uid 非附件 ID，降级为防崩手段，正确值依赖 B2 扩展，notes gap #1）。
2. 测试 mock 引用空安全（biome 禁 non-null assertion）→ calls()/mockSet() 封装。
3. 中断重试/幂等两用例的响应队列排列错位（失败用例只消费 presign 不消费 confirm）→ 修正序列。
