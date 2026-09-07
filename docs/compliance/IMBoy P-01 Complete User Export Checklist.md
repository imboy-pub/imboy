# IMBoy P-01 Complete User Export Checklist

> 任务：完整用户导出（Overseas Compliance Implementation Plan Task P-01）
> 日期：2026-09-08 | 状态：**DONE（受限同步导出强化；异步大表归档为后续任务）**
> 提交：imboy（本轮，未 push）

## Goal 对照

导出用户本人的便携数据，不暴露秘密、不暴露他人数据。

| 项 | 结果 |
|---|---|
| 不暴露秘密 | ✅ 双层防线：DS 层显式列 allowlist + logic 层黑名单兜底（sanitize/sensitive_key，覆盖 password/token/secret/salt/private/credential/api_key 等，大小写不敏感、递归 map/list、含 atom 键） |
| 不暴露他人数据 | ✅ Uid 仅来自 auth 中间件 current_uid，不接受请求参数；friends 仅取 from_user_id=$1 的关系行（对端只有 to_user_id/remark/created_at，无他人资料列）；groups 仅含本人所在群（id/title/joined_at） |
| 导出审计 | ✅ user_log type=130 不可变追加；审计失败不阻断导出但留 ERROR 日志 |

## Implementation 条款对照

| 计划要求 | 落地 |
|---|---|
| explicit field allowlists and categories | ✅ user_info/friends/groups 保持显式列；settings 弃 SELECT * 改显式列 `setting, updated_at`（user_setting.setting 是 jsonb 自由键，列 allowlist 根治"新增列静默进导出"，jsonb 内部键由黑名单兜底=双层）；分类=响应顶层四键 + `scope.categories` 声明 |
| bounded | ✅ friends LIMIT 5000 / groups LIMIT 1000（export_friends_limit/export_groups_limit 可配）；命中上限 truncated=true 诚实截断标记（静默截断会被误读为全量）。注意：本条计划原文为 "bounded **async** archive"——异步归档见下方"后续" |
| encryption/expiry | ➖ N/A（本轮）：同步 JSON 响应直返本人端，不落对象存储、无分享链接 → 无过期攻击面；异步归档交付时一并实现加密 zip + 短时效链接 |
| ownership filtering | ✅ 见 Goal 对照；`export_rejects_invalid_uid` 断言非法 uid 一律拒绝不回退 |
| export audit | ✅ type=130。注：迁移 00000051 曾修复真 bug——chk_user_log_type 不含 130 导致审计行被 CHECK 拦下、GDPR 导出长期无审计留痕（本次勘察确认该修复已在） |
| scope by legal review（不声称 GDPR 完整） | ✅ 响应新增 `scope`：categories/friends_limit/groups_limit/excluded（messages/moments/attachments/payments 逐项点名）/disclaimer 明示"不声称满足完整的数据可携带权，扩展范围由法务评审决定"。静默缺失会被用户与审计方误读为已覆盖，故 excluded 逐项点名 |

## Tests 条款对照（18/18 全绿）

| 计划测试项 | 落地 |
|---|---|
| every category | ✅ `export_returns_schema_and_audits`（四分类 + scope + legal_hold 键齐备 + 审计行 type=130 断言） |
| other-user redaction | ✅ `sanitize_recurses_into_nested_test`（他人行内 secret 剥离）+ DS 层 friends SQL 最小列（代码审查保证，真库行为由 SQL 显式列限定） |
| large dataset | ✅ `export_passes_truncation_markers`（friends_truncated/groups_truncated 透传 + 行数上界）；SQL LIMIT 硬上界在 DS 层，真库规模由 LIMIT 保证 |
| expired link | ➖ N/A：无链接同步导出设计下不存在过期链接场景；异步归档任务交付时补 |
| concurrent/rate-limited request | ✅ 冷却门 4 用例：窗口内拒绝（且断言不执行导出、不写审计行）/ 窗口外放行 / 配置 user_export_cool_down_ms=0 关闭 / 冷却检查自身故障 fail-open。**诚实记录**：两并发请求可同时通过冷却检查（无锁），后果=多导出一次自己的数据，危害可接受，未引入分布式锁 |
| schema drift | ✅ `export_strips_sensitive_from_user_info_schema_drift`（user_info 新增 password_hash/smtp_credential 被剥）+ `export_strips_sensitive_from_ds_payload`（settings jsonb 自由键如 push_token 被剥）+ DS 层显式列根治 |

## Acceptance 条款对照

| 验收项 | 结果 |
|---|---|
| D-02 categories marked export are present | ⚠️ 诚实记录：data-disposition.yml（D-02 交付物）是**删除链**处置清单（action ∈ delete/anonymize/retain），无 export 维度标记，无法机械核对。本轮以 `scope.categories` 声明导出分类（user_info/friends/groups/settings），对应表（user/user_setting/friend/group+group_member）均在 D-02 清单内被映射。是否给 data-disposition.yml 增加 export 维度属受限 YAML schema 变更（有解析器校验测试），**待 owner/legal review 拍板，不擅自改** |
| secret/token/password/private keys absent | ✅ 双层防线 + 测试断言 |
| user can retrieve on real device | ✅ app 注销页入口既有（logout_account_page → POST /v1/user/export_data → 本地 JSON + 系统分享）；429 冷却响应落 app 既有"操作失败请稍后再试"错误区块（exportUserData null 路径），无需改 app；真机人工走查按惯例留用户 |

## 变更清单（imboy）

- `src/ds/user_ds.erl`：新增 export_data_bounded/1（导出链专用，与 export_data/1 注销快照语义分离：LIMIT + truncated + settings 显式列）
- `src/ds/user_log_ds.erl`：新增 last_export_at/1（最近 type=130 审计行 created_at；冷却真源，不另建表）
- `src/logic/user_export_logic.erl`：冷却门（默认 24h，cool_down_ms/0 + check_cool_down + judge_cool_down，检查故障 fail-open）+ scope/0 范围声明；数据源切 export_data_bounded；sanitize 黑名单不变
- `src/api/user_handler.erl`：{error, {cool_down, _}} → 429「导出过于频繁，请 24 小时后再试」；注释更新（异步归档唯一遗留）
- `test/logic/user_export_logic_tests.erl`：9 → 18 用例

## 后续（不在本轮）

1. **异步大表归档导出**（计划 "async job/object delivery if needed"）：消息/动态/附件/支付的加密 zip + 对象存储 + 短时效过期链接 + worker——等 legal review 圈定范围后另立任务；这是 P-01 与 GDPR 完整导出的主要距离
2. data-disposition.yml 增加 export 维度标记 → 待 owner/legal review（受限 YAML schema 变更）
3. 冷却默认 24h 的产品口径确认（当前为工程默认值）

## 验证记录

- `make eunit-local t=user_export_logic_tests ERLC_EXCLUDE=agent_task_repo`（并行会话 WIP 排除）：**All 18 tests passed**（erlfmt -w 后复跑仍 18/18）
- 改动文件均经 erlfmt -w
