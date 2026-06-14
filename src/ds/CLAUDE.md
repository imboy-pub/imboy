# DS 层 - 数据服务层

[根目录](../CLAUDE.md) > **src/ds** | 77 个模块 | 职责：封装数据操作，调用 Repo 层，提供缓存支持

---

## 模块清单

### 用户

| 模块 | 说明 |
|------|------|
| `user_ds` | 用户数据服务 |
| `user_setting_ds` | 用户设置 |
| `user_dnd_rule_ds` | 用户免打扰(DND)规则（缓存键 `{user_dnd_rule, Uid}`） |
| `user_device_ds` | 设备管理 |
| `user_log_ds` | 操作日志 |
| `user_presence_ds` | 在线状态 |
| `user_collect_ds` | 收藏 |
| `user_denylist_ds` | 黑名单 |
| `user_tag_ds` | 用户标签 |
| `user_tag_relation_ds` | 标签关系 |
| `account_ds` | 账户服务 |
| `adm_user_ds` | 管理员用户 |

### 认证

| 模块 | 说明 |
|------|------|
| `auth_ds` | 认证数据服务 |
| `token_ds` | Token 管理 |
| `login_attempt_ds` | 登录尝试记录 |
| `verification_code_ds` | 验证码服务 |

### 好友 & 社交

| 模块 | 说明 |
|------|------|
| `friend_ds` | 好友数据服务 |
| `friend_category_ds` | 好友分组 |
| `mention_ds` | @提及服务 |

### 群组

| 模块 | 说明 |
|------|------|
| `group_ds` | 群组数据服务 |
| `group_member_ds` | 群成员 |
| `group_category_ds` | 群分类 |
| `group_notice_ds` | 群公告 |
| `group_tag_ds` | 群标签 |
| `group_file_ds` | 群文件 |
| `group_album_ds` | 群相册 |
| `group_log_ds` | 群操作日志 |
| `group_random_code_ds` | 入群码 |
| `group_schedule_ds` | 群日程 |
| `group_task_ds` | 群任务 |
| `group_vote_ds` | 群投票 |

### 消息

| 模块 | 说明 |
|------|------|
| `msg_c2c_ds` | 单聊消息（含已读回执） |
| `msg_c2g_ds` | 群聊消息 |
| `msg_c2s_ds` | 客户端请求 |
| `msg_s2c_ds` | 系统消息 |
| `message_ds` | 消息投递服务 |
| `msg_store_ds` | 消息存储服务 |
| `msg_store_worker` | 批量写入 Worker |
| `msg_store_sup` | 消息 Worker Supervisor |
| `msg_archive_ds` | 永久消息历史查询（conv_seq 游标） |
| `msg_operation_ds` | 消息操作（撤回等） |
| `msg_forward_ds` | 消息转发 |
| `msg_reaction_ds` | 消息表情反应 |
| `msg_read_ds` | 消息已读状态 |

### E2EE

| 模块 | 说明 |
|------|------|
| `e2ee_social_ds` | E2EE 社交恢复 |
| `e2ee_transfer_ds` | E2EE 设备传输 |
| `e2ee_local_backup_ds` | E2EE 本地备份 |
| `e2ee_shard_transmission_log_ds` | E2EE 分片传输日志 |
| `compliance_key_ds` | 合规密钥管理 |

### 频道

| 模块 | 说明 |
|------|------|
| `channel_ds` | 频道基础服务 |
| `channel_admin_ds` | 频道管理员 |
| `channel_subscription_ds` | 频道订阅（is_subscribed 唯一路径）|
| `channel_message_ds` | 频道消息 |
| `channel_order_ds` | 付费频道订单（含 has_purchased/create_order/pay/get_price）|
| `channel_invitation_ds` | 频道邀请（含 create/accept/reject/is_invited）|

### 应用 & 系统

| 模块 | 说明 |
|------|------|
| `app_version_ds` | 应用版本管理 |
| `app_version_policy_ds` | 版本升级策略 |
| `app_upgrade_log_ds` | 升级日志 |
| `app_ddl_ds` | DDL 配置 |
| `config_ds` | 配置管理 |
| `announcement_ds` | 公告服务 |

### 对话 & 内容 & 其他

| 模块 | 说明 |
|------|------|
| `conversation_delete_ds` | 对话删除 |
| `conversation_mute_ds` | 对话静音 |
| `conversation_pin_ds` | 对话置顶 |
| `attachment_ds` | 附件管理 |
| `moment_ds` | 朋友圈 |
| `fts_user_ds` | 全文搜索用户 |
| `geo_people_nearby_ds` | 附近人 |
| `push_token_ds` | 推送令牌 |
| `push_notification_ds` | 推送通知 |
| `feedback_ds` | 用户反馈 |
| `report_ds` | 举报服务 |
| `report_ticket_ds` | 举报工单 |
| `report_action_log_ds` | 举报处理日志 |
| `live_room_ds` | 直播间 |
| `wallet_ds` | 钱包服务 |
| `payment_transaction_ds` | 统一支付流水数据服务（薄封装 payment_transaction_repo + 回调幂等记录） |
| `billing_plan_ds` | SaaS 套餐数据服务（薄封装 billing_plan_repo） |
| `billing_subscription_ds` | SaaS 订阅数据服务（薄封装 billing_subscription_repo） |
| `billing_usage_ds` | SaaS 用量数据服务（薄封装 billing_usage_repo） |
| `billing_invoice_ds` | SaaS 账单数据服务（薄封装 billing_invoice_repo） |
| `websocket_ds` | WebSocket 状态管理 |

---

## 依赖关系

| DS | 依赖 Repo | 依赖 Lib |
|----|-----------|---------|
| `user_ds` | `user_repo`, `user_setting_repo` | `imboy_cache` |
| `friend_ds` | `friend_repo`, `friend_category_repo` | `imboy_cache` |
| `group_ds` | `group_repo` | `imboy_cache` |
| `msg_c2c_ds` | `msg_c2c_repo` | — |
| `auth_ds` | `user_repo` | — |
| 所有 DS | — | `elib_pg`, `imboy_syn` |

---

## 关键约束

- 调用链：Logic → DS → Repo → PostgreSQL
- DS 层**不直接**调用 `elib_pg`，所有 DB 操作通过 Repo 层
- 缓存键格式：`{Table, Id}` 或 `{Uid, Did}`
- `msg_store_ds` 写入触发 `msg_store_worker` 批量处理
- 消息严格排序依赖 `conv_seq`，不依赖 TSID

---

## Bug 修复记录

- **2026-05-03** `user_ds:insert_and_get_id/1`：BIGSERIAL→TSID 迁移后未调用 `elib_tsid:generate(user)`，导致 `not_null_violation`；修复：INSERT 前添加 `Id = elib_tsid:generate(user)` 并注入 `<<"id">>` 字段

---

**文档维护**: 添加新 DS 模块时同步更新此文档。
