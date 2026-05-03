# DS 层文档 - 数据服务层

[根目录](../CLAUDE.md) > **src/ds**

> **最后更新**: 2026-04-15 CST
> **模块数量**: 77 个（含 Worker/Supervisor）
> **职责**: 封装数据操作，调用 Repo 层访问数据库，提供缓存支持

---

## 模块职责

DS 层是 Imboy 系统的数据服务层，负责：
- 封装数据访问逻辑
- 调用 Repo 层进行数据库操作
- 提供缓存支持
- 消息投递服务
- 配置管理
- WebSocket 状态管理

---

## 入口与启动

DS 模块由 Logic 层调用：

```erlang
% Logic 调用 DS
{ok, User} = user_ds:find_by_uid(Uid).

% DS 调用 Repo
{ok, User} = user_repo:find_by_uid(Uid).
```

---

## 对外接口

### 用户相关 DS

| DS | 说明 |
|----|------|
| `user_ds.erl` | 用户数据服务 |
| `user_setting_ds.erl` | 用户设置 |
| `user_device_ds.erl` | 用户设备管理 |
| `user_log_ds.erl` | 用户操作日志 |
| `user_presence_ds.erl` | 用户在线状态 |
| `user_collect_ds.erl` | 用户收藏 |
| `user_denylist_ds.erl` | 用户黑名单 |
| `user_tag_ds.erl` | 用户标签 |
| `user_tag_relation_ds.erl` | 用户标签关系 |
| `account_ds.erl` | 账户服务 |
| `adm_user_ds.erl` | 管理员用户服务 |

### 认证与身份验证 DS

| DS | 说明 |
|----|------|
| `auth_ds.erl` | 认证数据服务 |
| `token_ds.erl` | Token 管理 |
| `login_attempt_ds.erl` | 登录尝试记录 |
| `verification_code_ds.erl` | 验证码服务 |

### 好友与社交 DS

| DS | 说明 |
|----|------|
| `friend_ds.erl` | 好友数据服务 |
| `friend_category_ds.erl` | 好友分组 |
| `mention_ds.erl` | @提及服务 |
| `user_denylist_ds.erl` | 黑名单（用户屏蔽） |

### 群组相关 DS

| DS | 说明 |
|----|------|
| `group_ds.erl` | 群组数据服务 |
| `group_member_ds.erl` | 群组成员 |
| `group_category_ds.erl` | 群组分类 |
| `group_notice_ds.erl` | 群组公告 |
| `group_tag_ds.erl` | 群组标签 |
| `group_file_ds.erl` | 群组文件 |
| `group_album_ds.erl` | 群组相册 |
| `group_log_ds.erl` | 群组操作日志 |
| `group_random_code_ds.erl` | 群组二维码（入群码） |
| `group_schedule_ds.erl` | 群组日程 |
| `group_task_ds.erl` | 群组任务 |
| `group_vote_ds.erl` | 群组投票 |

### 消息相关 DS

| DS | 说明 |
|----|------|
| `msg_c2c_ds.erl` | 单聊消息服务（含已读回执） |
| `msg_c2g_ds.erl` | 群聊消息服务 |
| `msg_c2s_ds.erl` | 客户端请求服务 |
| `msg_s2c_ds.erl` | 系统消息服务 |
| `message_ds.erl` | 消息投递服务 |
| `msg_store_ds.erl` | 消息存储服务 |
| `msg_store_worker.erl` | 批量写入 Worker |
| `msg_store_sup.erl` | 消息 Worker Supervisor |
| `msg_archive_ds.erl` | 永久消息历史查询 |
| `msg_operation_ds.erl` | 消息操作（撤回等） |
| `msg_forward_ds.erl` | 消息转发 |
| `msg_reaction_ds.erl` | 消息表情反应 |
| `msg_read_ds.erl` | 消息已读状态 |

### 端到端加密 (E2EE) DS

| DS | 说明 |
|----|------|
| `e2ee_social_ds.erl` | E2EE 社交恢复 |
| `e2ee_transfer_ds.erl` | E2EE 设备传输 |
| `e2ee_local_backup_ds.erl` | E2EE 本地备份 |
| `e2ee_shard_transmission_log_ds.erl` | E2EE 分片传输日志 |
| `compliance_key_ds.erl` | 合规密钥管理 |

### 频道相关 DS

| DS | 说明 |
|----|------|
| `channel_ds.erl` | 频道基础服务 |
| `channel_admin_ds.erl` | 频道管理员 |
| `channel_subscription_ds.erl` | 频道订阅 |
| `channel_subscribe_ds.erl` | 频道订阅状态 |
| `channel_message_ds.erl` | 频道消息 |
| `channel_order_ds.erl` | 频道订单（付费频道） |
| `channel_invitation_ds.erl` | 频道邀请 |

### 应用与系统 DS

| DS | 说明 |
|----|------|
| `app_version_ds.erl` | 应用版本管理 |
| `app_version_policy_ds.erl` | 应用版本策略（升级规则） |
| `app_upgrade_log_ds.erl` | 应用升级日志 |
| `app_ddl_ds.erl` | DDL 配置 |
| `config_ds.erl` | 配置管理 |
| `announcement_ds.erl` | 公告服务 |

### 对话与消息流 DS

| DS | 说明 |
|----|------|
| `conversation_delete_ds.erl` | 对话删除 |
| `conversation_mute_ds.erl` | 对话静音 |
| `conversation_pin_ds.erl` | 对话置顶 |

### 内容与媒体 DS

| DS | 说明 |
|----|------|
| `attachment_ds.erl` | 附件管理 |
| `moment_ds.erl` | 朋友圈/矩（Moment） |
| `fts_user_ds.erl` | 全文搜索用户 |
| `geo_people_nearby_ds.erl` | 地理位置附近人 |

### 推送与通知 DS

| DS | 说明 |
|----|------|
| `push_token_ds.erl` | 推送令牌管理 |
| `push_notification_ds.erl` | 推送通知 |

### 反馈与举报 DS

| DS | 说明 |
|----|------|
| `feedback_ds.erl` | 用户反馈 |
| `report_ds.erl` | 举报服务 |
| `report_ticket_ds.erl` | 举报工单 |
| `report_action_log_ds.erl` | 举报处理日志 |

### 直播与房间 DS

| DS | 说明 |
|----|------|
| `live_room_ds.erl` | 直播间 |

### 钱包与支付 DS

| DS | 说明 |
|----|------|
| `wallet_ds.erl` | 钱包服务 |

### WebSocket 与实时 DS

| DS | 说明 |
|----|------|
| `websocket_ds.erl` | WebSocket 状态管理 |

---

## 关键依赖与配置

### 依赖的 Repo 模块

| DS | 依赖的 Repo |
|----|-------------|
| `user_ds` | `user_repo`, `user_setting_repo` |
| `friend_ds` | `friend_repo`, `friend_category_repo` |
| `group_ds` | `group_repo` |
| `msg_c2c_ds` | `msg_c2c_repo` |
| `auth_ds` | `user_repo` |

### 依赖的 Lib 模块

- `imboy_cache.erl`: 缓存操作
- `elib_pg.erl`: 数据库连接
- `imboy_syn.erl`: 分布式进程注册

---

## 数据模型

### 消息存储服务

`msg_store_ds.erl` 和 `msg_store_worker.erl` 实现消息批量写入：

```erlang
% 存储消息
ok = msg_store_ds:store(Msg).

% 批量写入
{ok, _} = msg_store_worker:write_batch(Msgs).
```

### 消息投递服务

`message_ds.erl` 实现消息投递：

```erlang
% 投递消息
ok = message_ds:send_next(Uid, Msg).
```

---

## 测试与质量

### 测试文件位置

```
test/ds/
├── account_ds_tests.erl
├── app_version_ds_tests.erl
├── auth_ds_tests.erl
├── feedback_ds_tests.erl
├── friend_category_ds_tests.erl
├── friend_ds_tests.erl
├── msg_c2c_ds_tests.erl
├── msg_c2g_ds_tests.erl
├── msg_c2s_ds_tests.erl
├── msg_s2c_ds_tests.erl
└── user_setting_ds_tests.erl
```

---

## 常见问题 (FAQ)

### Q: 如何添加新的数据服务?

1. 在 `src/ds/` 创建新的 DS 文件
2. 调用 Repo 层进行数据库操作
3. 编写测试

### Q: 如何实现缓存?

使用 `imboy_cache:get/1,2` 和 `imboy_cache:set/3,4`。

---

## 相关文件清单

### DS 文件 (77 个)

```
src/ds/
├── account_ds.erl
├── adm_user_ds.erl
├── announcement_ds.erl
├── app_ddl_ds.erl
├── app_upgrade_log_ds.erl
├── app_version_ds.erl
├── app_version_policy_ds.erl
├── attachment_ds.erl
├── auth_ds.erl
├── channel_admin_ds.erl
├── channel_ds.erl
├── channel_invitation_ds.erl
├── channel_message_ds.erl
├── channel_order_ds.erl
├── channel_subscribe_ds.erl
├── channel_subscription_ds.erl
├── compliance_key_ds.erl
├── config_ds.erl
├── conversation_delete_ds.erl
├── conversation_mute_ds.erl
├── conversation_pin_ds.erl
├── e2ee_local_backup_ds.erl
├── e2ee_shard_transmission_log_ds.erl
├── e2ee_social_ds.erl
├── e2ee_transfer_ds.erl
├── feedback_ds.erl
├── friend_category_ds.erl
├── friend_ds.erl
├── fts_user_ds.erl
├── geo_people_nearby_ds.erl
├── group_album_ds.erl
├── group_category_ds.erl
├── group_ds.erl
├── group_file_ds.erl
├── group_log_ds.erl
├── group_member_ds.erl
├── group_notice_ds.erl
├── group_random_code_ds.erl
├── group_schedule_ds.erl
├── group_tag_ds.erl
├── group_task_ds.erl
├── group_vote_ds.erl
├── live_room_ds.erl
├── login_attempt_ds.erl
├── mention_ds.erl
├── message_ds.erl
├── moment_ds.erl
├── msg_archive_ds.erl
├── msg_c2c_ds.erl
├── msg_c2g_ds.erl
├── msg_c2s_ds.erl
├── msg_forward_ds.erl
├── msg_operation_ds.erl
├── msg_read_ds.erl
├── msg_reaction_ds.erl
├── msg_s2c_ds.erl
├── msg_store_ds.erl
├── msg_store_sup.erl
├── msg_store_worker.erl
├── push_notification_ds.erl
├── push_token_ds.erl
├── report_action_log_ds.erl
├── report_ds.erl
├── report_ticket_ds.erl
├── token_ds.erl
├── user_collect_ds.erl
├── user_denylist_ds.erl
├── user_device_ds.erl
├── user_ds.erl
├── user_log_ds.erl
├── user_presence_ds.erl
├── user_setting_ds.erl
├── user_tag_ds.erl
├── user_tag_relation_ds.erl
├── verification_code_ds.erl
├── wallet_ds.erl
└── websocket_ds.erl
```

---

## 变更记录 (Changelog)

### 2026-05-03
- 修复 `user_ds.erl` `insert_and_get_id/1` 注册流程 TSID 遗漏 bug
  - **根因**：BIGSERIAL→TSID 迁移后，DB 序列默认值已移除，但 `insert_and_get_id/1` 未调用 `elib_tsid:generate(user)` 生成 `id` 字段
  - **现象**：PostgreSQL 23502 `not_null_violation`（`null value in column "id" of relation "user" violates not-null constraint`）
  - **影响范围**：`passport_logic.erl` 4 处调用（lines 50, 106, 376, 408）全部受影响，注册功能完全不可用
  - **修复**：在 `insert_and_get_id/1` 中 INSERT 前添加 `Id = elib_tsid:generate(user)` + `Data2 = Data#{<<"id">> => Id}`，与 `user_repo:save/1` 保持一致
  - **排查**：全库 INSERT 路径 TSID 审计完成，确认注册相关 `user` 表路径已全部修复；其余 repo 层 INSERT 均已有 TSID

### 2026-04-15
- 完整同步 DS 层文档，实际 77 个文件统计（从 13 个）
- 新增多个 G3 治理薄封装 DS 模块和功能服务模块
  - 应用升级：app_upgrade_log_ds、app_version_policy_ds
  - 频道系统：channel_admin_ds、channel_ds、channel_invitation_ds、channel_message_ds、channel_order_ds、channel_subscribe_ds、channel_subscription_ds
  - 端到端加密：e2ee_local_backup_ds、e2ee_shard_transmission_log_ds、e2ee_social_ds、e2ee_transfer_ds、compliance_key_ds
  - 群组功能：group_album_ds、group_category_ds、group_file_ds、group_log_ds、group_member_ds、group_notice_ds、group_random_code_ds、group_schedule_ds、group_tag_ds、group_task_ds、group_vote_ds
  - 消息功能：msg_archive_ds、msg_c2g_ds、msg_forward_ds、msg_operation_ds、msg_read_ds、msg_reaction_ds
  - 用户功能：user_device_ds、user_log_ds、user_presence_ds、user_tag_ds、user_tag_relation_ds、adm_user_ds
  - 其他服务：announcement_ds、attachment_ds、conversation_delete_ds、conversation_mute_ds、conversation_pin_ds、feedback_ds、fts_user_ds、geo_people_nearby_ds、live_room_ds、mention_ds、moment_ds、push_notification_ds、push_token_ds、report_action_log_ds、report_ds、report_ticket_ds、user_collect_ds、verification_code_ds、wallet_ds
- 重新分类对外接口，按功能领域组织（用户、认证、好友、群组、消息、E2EE、频道、应用、对话、内容、推送、反馈、直播、钱包等）

### 2026-04-04
- 新增 `msg_archive_ds.erl` 永久消息历史查询服务（方案 B：conv_seq 游标）

### 2026-01-31
- 新增 `e2ee_social_ds.erl` E2EE 社交恢复数据服务
- 新增 `e2ee_transfer_ds.erl` E2EE 设备传输数据服务
- 完善 DS 层文档

### 2026-01-20
- 新增 `msg_store_ds.erl` 消息存储服务
- 新增 `msg_store_worker.erl` 批量写入 Worker
- 完善 DS 层文档

---

**文档维护**: 请在添加新的数据服务时同步更新此文档。
