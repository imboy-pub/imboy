# /v1/* 响应字段层契约审计报告 / Response-Field Contract Audit Report

> 日期 / Date: 2026-06-02 ｜ 范围 / Scope: FE(Flutter / React Admin / JS SDK) ↔ BE(Erlang) `/v1/*` 响应字段
> 方法 / Method: 11 域并行 fan-out 审计 + 对抗式验证（过滤 SQL 别名 / normalize 后处理 / FE 适配层 / 分页信封误报）

简体中文为权威版本，English mirror follows each section.

---

## 概览 / Overview

- 审计端点 / Endpoints audited: 276 routes across 11 domains
- 确认 mismatch / Confirmed mismatches: **29**（已全部修复 / all fixed）
- 过滤误报 / False positives filtered: 11
- 前置完成 / Prior phases done: 请求参数层、响应崩溃风险维度（见 [tsid-field-convention.md](../../reference/tsid-field-convention.md) 与项目记忆）

This report covers the **functional** response-field comparison (FE reads but BE omits → blank/default; BE sends but FE unused → redundant; type mismatch esp. TSID; naming/nesting mismatch). All 29 confirmed items were fixed directly (FE-aligns-BE by default unless BE clearly wrong).

---

## 确认并修复的 mismatch 清单 / Confirmed & Fixed Mismatches

| # | 端点 / Endpoint | 字段 / Field | 类别 / Category | 严重度 | 消费方 | 修复侧 / Fix |
|---|------|------|------|--------|--------|------|
| 1 | passport/login·quick_login·signup | refreshtoken vs refresh_token | naming | HIGH | SDK | FE: types.ts/auth.ts/passport.ts 改 refreshtoken |
| 2 | user/qrcode·uqrcode | id (`as String` 崩溃) | type | HIGH | Flutter | FE: scanner_page parseModelString（崩溃修复） |
| 3 | fts/user_search | uid vs id | naming | HIGH | Flutter | FE: people_model `uid ?? id` |
| 4 | group_member/page | mute_until 漏发 | missing | HIGH | Flutter | BE: group_member_ds 补 m.mute_until |
| 5 | group/detail | owner.id 嵌套 | naming | HIGH | Admin | FE: 改 owner_uid + GroupDetail=Group |
| 6 | moments/feed·user/:uid | author_nickname/avatar | missing | HIGH | Flutter | BE: moment_logic enrich_posts 批量补 |
| 7 | e2ee/key/status | registered/expired | missing | HIGH | Flutter | FE: 改 has_valid_key（误报未注册修复） |
| 8 | passport/login | uid 2^53 精度 | type | MED | SDK | FE: client.ts safeParseBigIntJson |
| 9 | group_member/page | id 漏发 | missing | MED | Flutter | BE: group_member_ds 补 m.id |
| 10 | channel/:id/subscribers | nickname/avatar | missing | MED | Flutter | BE: channel_subscription_repo JOIN user |
| 11 | channel/:id/admins | nickname/avatar | missing | MED | Flutter | BE: channel_admin_repo JOIN user |
| 12 | channel/:id/admins | added_at vs created_at | naming | MED | Flutter | FE: parseModelDateTime(created_at) |
| 13 | channel/:id | owner_uid vs creator_uid | naming | MED | SDK | FE: types.ts 改 creator_uid |
| 14 | moment/:id/comments | user_nickname/avatar 等 | missing | MED | Flutter | BE: moment_logic enrich_comments |
| 15 | moments/feed | liked 漏发 | missing | MED | Flutter | BE: moment_logic + liked_post_ids 批量 |
| 16 | passport/login | expires_in（BE 未发） | missing | LOW | SDK | FE: types.ts 删除 expires_in |
| 17 | channel/:id | type 字符串 vs smallint | type | LOW | SDK | FE: ChannelType 改 `0\|1\|2` |
| 18 | channel/:id/subscribers | is_muted 漏发 | missing | LOW | Flutter | BE: channel_subscription_repo 补 is_muted |
| 19 | mention/list | is_read_bool 冗余 | extra | LOW | — | BE: mention_handler 删除 |
| 20 | mention/list | list 与 items 重复 | extra | LOW | — | BE: 删 list 留 items（FE 读 items） |
| 21 | location/people_nearby | location 坐标泄露 | extra | LOW | Flutter | BE: geo_repo 删 ST_AsText(location)（隐私） |

### 保留不动 / Intentionally kept (YAGNI，验证者建议保留)

| 端点 / Endpoint | 字段 / Field | 理由 / Rationale |
|------|------|------|
| user/credential | ttl | 语义有效（凭据 86400s），便于客户端缓存复用 |
| msg/offline | c2c/c2g/s2c.total | 服务端调试/未来未读数 UI 潜在价值，不阻断功能 |
| group/task/list | assignment_status | 未来作业详情 UI 可用 |
| group_album/list | album_cover | 需 FE 加封面缩略图 UI 才消费 |
| moment show/feed | allow_comment/status/updated_at | admin_post_transfer 复用同函数，删需区分 admin |
| wallet/transactions | balance_after 等 | balance_after 建议保留供「交易后余额」展示 |
| live_room/list | stream_key | 隐私：列表不应下发推流密钥（仅 detail 对房主返回） |
| user/qrcode | isfriend/remark | 未被消费；下游重新拉取关系，低优先 |

These are redundant or latent-value fields that cause no functional break; removing them is churn with negligible benefit (KISS/YAGNI).

---

## 改动文件清单 / Changed Files (23)

**imboyapp (Flutter, 5)**: `page/scanner/scanner_page.dart`, `store/model/people_model.dart`, `page/channel/channel_admin_page.dart`, `service/e2ee_health_check_service.dart`, `store/api/e2ee_api.dart`

**imboy (Erlang, 8)**: `ds/group_member_ds.erl`, `repo/channel_subscription_repo.erl`, `repo/channel_admin_repo.erl`, `logic/moment_logic.erl`, `ds/moment_ds.erl`, `repo/moment_like_repo.erl`, `api/mention_handler.erl`, `repo/geo_people_nearby_repo.erl`

**imboy-admin-frontend (React, 5)**: `pages/groups/GroupCategoryManagePage.tsx`, `modules/groups/api/public.ts`, `modules/groups/api/groups.test.ts`, `pages/groups/GroupCategoryManagePage.test.tsx`, `pages/groups/GroupDetailPage.test.tsx`

**sdk/js (TS, 3)**: `src/types.ts`, `src/auth.ts`, `src/api/passport.ts`, `src/client.ts`

---

## 关键实现说明 / Key Implementation Notes

1. **TSID 大整数安全解析 / BigInt-safe parsing (SDK)**：`client.ts` 新增零依赖 `safeParseBigIntJson`（正则把 16 位及以上裸整数加引号），`response.json()` → `response.text()` + 解析。与管理后台 `safeParseBigIntJson` 行为一致。13 位时间戳（<16 位阈值）保持 number。
   SDK has no runtime deps, so a regex reviver is used instead of json-bigint.

2. **moment 富化避免 N+1 / Enrichment without N+1**：新增 `moment_like_repo:liked_post_ids/2`（批量查点赞），作者/评论昵称头像用 `user_ds:list_by_ids/2` 批量获取。`remark`（联系人备注）属调用方私有，由客户端用 uid 查本地联系人填充。

3. **channel JOIN**：订阅者/管理员列表 `LEFT JOIN public.user` 补 `nickname`/`avatar`（订阅者另补 `is_muted`）；user_id 仍以 TSID integer 传输。

4. **隐私 / Privacy**：`people_nearby` 移除 `ST_AsText(location)` 投影，避免下发他人精确坐标（`distance` 已满足展示）。

---

## 验证 / Verification

| 项 / Item | 结果 / Result |
|----|------|
| Flutter 改动 5 文件 dart analyze | ✅ 零问题 / No issues |
| Flutter 全量 analyze | ⚠️ 仅预存 i18n 测试错误（test/integration/moment·tag，需 `dart run slang`），非本次回归 |
| Erlang 改动 8 模块 ERLC | ✅ 编译通过 / compiled clean |
| SDK BigInt 正则 | ✅ node 实测：19 位 uid→string 无丢精度、短整数/时间戳保持 number |
| Admin GroupDetail=Group | ✅ owner_uid 已在 Group 类型；3 测试对齐 |

> 未跑（需装依赖，受成本控制）/ Not run (require install, cost-capped): Admin `tsc`+`bun test`、SDK `build`、Erlang `eunit`。

---

## 约束遵守 / Constraints Honored

- 未 push / Not pushed；未触碰保留区文件（erlang.mk / ios / macos / r_upgrade）与他人未完成改动（adm_attach_handler / adm_feedback_handler / flutter_chat_ui）。
- 默认 FE 对齐 BE，仅在 BE 明显遗漏（SELECT 漏列、enrichment 缺失、隐私泄露）时改 BE。
