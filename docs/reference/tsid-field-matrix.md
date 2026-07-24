# TSID 字段真值矩阵（首版）

> Last Updated: 2026-04-11  
> Status: Working draft  
> Purpose: 用一份矩阵拆开 TSID 的“生成方式、字段语义、物理类型、API 表示、客户端表示”，作为后续批次修复的单一参考。

---

## 1. 判定规则

### 字段角色

- `TSID primary key`
  - 本表主键 `id` 由应用层 `elib_tsid:generate/1` 生成。
- `TSID foreign/semantic field`
  - 字段值承载其他实体的 TSID，但不是本表内新生成。
- `Local surrogate key`
  - 仅用于本地缓存表或中间表的自增主键，不代表后端实体 ID。
- `Business string key`
  - 业务主键是字符串，不属于 TSID。

### 本矩阵当前范围

- 优先覆盖已经确认冲突或跨端耦合最强的字段。
- 不是最终全量字段清单，后续会继续扩充。

---

## 2. 由应用层直接生成 TSID 的主键（高置信度）

| Domain | Table | Field | Role | Generator Name | PostgreSQL Type | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| User | `user` | `id` | TSID primary key | `user` | `BIGINT` | 用户主键 |
| User | `user_device` | `id` | TSID primary key | `user_device` | `BIGINT` | 设备记录主键 |
| User | `user_collect` | `id` | TSID primary key | `user_collect` | `BIGINT` | 收藏记录主键 |
| User | `user_denylist` | `id` | TSID primary key | `user_denylist` | `BIGINT` | 黑名单记录主键 |
| User | `user_tag` | `id` | TSID primary key | `user_tag` | `BIGINT` | 标签主键 |
| User | `user_tag_relation` | `id` | TSID primary key | `user_tag_relation` | `BIGINT` | 标签关系主键 |
| Friend | `user_friend` | `id` | TSID primary key | `friend` | `BIGINT` | 生成器名和表名不一致 |
| Friend | `user_friend_category` | `id` | TSID primary key | `friend_category` | `BIGINT` | 生成器名和表名不一致 |
| Group | `group` | `id` | TSID primary key | `group_info` | `BIGINT` | 生成器名和表名不一致 |
| Group | `group_member` | `id` | TSID primary key | `group_member` | `BIGINT` | 群成员关系主键 |
| Group | `group_notice` | `id` | TSID primary key | `group_notice` | `BIGINT` | 公告主键 |
| Group | `group_log` | `id` | TSID primary key | `group_log` | `BIGINT` | 治理日志主键 |
| Group | `group_random_code` | `id` | TSID primary key | `group_random_code` | `BIGINT` | 入群码记录主键 |
| Group | `group_category` | `id` | TSID primary key | `group_category` | `BIGINT` | 分类主键 |
| Group | `group_tag` | `id` | TSID primary key | `group_tag` | `BIGINT` | 标签主键 |
| Group | `group_vote` | `id` | TSID primary key | `group_vote` | `BIGINT` | 投票主键 |
| Group | `group_vote_option` | `id` | TSID primary key | `group_vote_option` | `BIGINT` | 选项主键 |
| Group | `group_schedule` | `id` | TSID primary key | `group_schedule` | `BIGINT` | 日程主键 |
| Group | `group_schedule_participant` | `id` | TSID primary key | `group_schedule` | `BIGINT` | 代码当前复用 `group_schedule` 生成器，待后续单独决策 |
| Group | `group_schedule_remind` | `id` | TSID primary key | `group_schedule_reminder` | `BIGINT` | 生成器名和表名不一致 |
| Group | `group_album` | `id` | TSID primary key | `group_album` | `BIGINT` | 相册主键 |
| Group | `group_album_photo` | `id` | TSID primary key | `group_album_photo` | `BIGINT` | 图片主键 |
| Group | `group_album_photo_comment` | `id` | TSID primary key | `group_album_comment` | `BIGINT` | 生成器名和表名不一致 |
| Group | `group_file` | `id` | TSID primary key | `group_file` | `BIGINT` | 文件主键 |
| Group | `group_task` | `id` | TSID primary key | `group_task` | `BIGINT` | 任务主键 |
| Group | `group_task_assignment` | `id` | TSID primary key | `group_task_assignment` | `BIGINT` | 作业提交主键 |
| Message | `msg_c2c.id` | `id` | TSID primary key | `msg_c2c` | `BIGINT` | 消息存储行主键，不等于 `msg_id` |
| Message | `msg_c2g.id` | `id` | TSID primary key | `msg_c2g` | `BIGINT` | 同上 |
| Message | `msg_c2s.id` | `id` | TSID primary key | `msg_c2s` | `BIGINT` | 同上 |
| Message | `msg_s2c.id` | `id` | TSID primary key | `msg_s2c` | `BIGINT` | 同上 |
| Message | `msg_store` | `id` | TSID primary key | `msg_store` | `BIGINT` | 归档消息行主键 |
| Message | `msg_read` | `id` | TSID primary key | `msg_read` | `BIGINT` | 已读回执主键 |
| Message | `msg_mention` | `id` | TSID primary key | `msg_mention` | `BIGINT` | @提醒主键 |
| Message | `msg_forward` | `id` | TSID primary key | `msg_forward` | `BIGINT` | 转发记录主键 |
| Message | `msg_reaction` | `id` | TSID primary key | `msg_reaction` | `BIGINT` | 反应主键 |
| Conversation | `conversation_delete` | `id` | TSID primary key | `conversation_delete` | `BIGINT` | 删除标记主键 |
| Conversation | `conversation_pin` | `id` | TSID primary key | `conversation_pin` | `BIGINT` | 置顶记录主键 |
| Attachment | `attachment` | `id` | TSID primary key | `attachment` | `BIGINT` | 附件主键 |
| Moment | `moment_post` | `id` | TSID primary key | `moment_post` | `BIGINT` | 动态主键 |
| Moment | `moment_comment` | `id` | TSID primary key | `moment_comment` | `BIGINT` | 评论主键 |
| Moment | `moment_like` | `id` | TSID primary key | `moment_like` | `BIGINT` | 点赞主键 |
| Moment | `moment_timeline` | `id` | TSID primary key | `moment_timeline` | `BIGINT` | 时间线行主键 |
| Moment | `moment_post_acl` | `id` | TSID primary key | `moment_post_acl` | `BIGINT` | ACL 行主键 |
| Moment | `moment_report` | `id` | TSID primary key | `moment_report` | `BIGINT` | 举报主键 |
| Channel | `channel` | `id` | TSID primary key | `channel` | `BIGINT` | 频道主键 |
| Channel | `channel_message` | `id` | TSID primary key | `channel_message` | `BIGINT` | 频道消息主键 |
| Channel | `channel_subscription` | `id` | TSID primary key | `channel_subscription` | `BIGINT` | 订阅关系主键 |
| Channel | `channel_admin` | `id` | TSID primary key | `channel_admin` | `BIGINT` | 管理员关系主键 |
| Channel | `channel_message_view` | `id` | TSID primary key | `channel_message_view` | `BIGINT` | 消息阅读视图表主键 |
| Channel | `channel_order` | `id` | TSID primary key | `channel_order` | `BIGINT` | 订单主键 |
| Channel | `channel_invitation` | `id` | TSID primary key | `channel_invitation` | `BIGINT` | 邀请主键 |
| Feedback | `feedback` | `id` | TSID primary key | `feedback` | `BIGINT` | 反馈主键 |
| Report | `report_ticket` | `id` | TSID primary key | `report_ticket` | `BIGINT` | 工单主键 |
| Report | `report_action_log` | `id` | TSID primary key | `report_action_log` | `BIGINT` | 处理动作主键 |
| Wallet | `wallet` | `id` | TSID primary key | `wallet` | `BIGINT` | 钱包主键 |
| Wallet | `wallet_transaction` | `id` | TSID primary key | `wallet_transaction` | `BIGINT` | 流水主键 |
| Live | `live_room` | `id` | TSID primary key | `live_room` | `BIGINT` | 直播间主键 |
| App | `app_version` | `id` | TSID primary key | `app_version` | `BIGINT` | 版本主键 |
| App | `app_ddl` | `id` | TSID primary key | `app_ddl` | `BIGINT` | DDL 版本记录主键 |
| App | `app_upgrade_log` | `id` | TSID primary key | `app_upgrade_log` | `BIGINT` | 升级日志主键 |
| App | `app_version_policy` | `id` | TSID primary key | `app_version_policy` | `BIGINT` | 升级策略主键 |

---

## 3. 承载 TSID 语义，但不是本表新生成

| Table | Field | Role | PostgreSQL Type | Current API / Client Notes |
| --- | --- | --- | --- | --- |
| `msg_c2c` | `from_id`, `to_id` | TSID foreign/semantic field | `BIGINT` | 消息发送方/接收方 TSID |
| `msg_c2g` | `from_id`, `to_id` | TSID foreign/semantic field | `BIGINT` | `to_id` 语义是群 TSID |
| `msg_c2s` | `from_id`, `to_id` | TSID foreign/semantic field | `BIGINT` | 同上 |
| `msg_s2c` | `from_id`, `to_id` | TSID foreign/semantic field | `BIGINT` | S2C 发送方可为 0 |
| `channel` | `creator_uid` | TSID foreign/semantic field | `BIGINT` | 管理端有时以 `owner_id` 暴露 |
| `channel_message` | `channel_id`, `author_id` | TSID foreign/semantic field | `BIGINT` | 管理端消息列表已 string 化 |
| `channel_subscription` | `channel_id`, `user_id`, `last_message_id` | TSID foreign/semantic field | `BIGINT` | 本地 SQLite 中部分列为远端 TSID，`id` 不是 |
| `channel_admin` | `channel_id`, `user_id` | TSID foreign/semantic field | `BIGINT` | 同上 |
| `group` | `owner_uid`, `creator_uid` | TSID foreign/semantic field | `BIGINT` | 管理端已 string 化 |
| `group_member` | `group_id`, `user_id` | TSID foreign/semantic field | `BIGINT` | 管理端已 string 化 |
| `group_notice` | `group_id`, `user_id`, `edit_user_id` | TSID foreign/semantic field | `BIGINT` | Flutter SQLite 已迁为 `INTEGER` |
| `moment_post` | `author_uid` | TSID foreign/semantic field | `BIGINT` | 动态作者 TSID |
| `moment_comment` | `post_id`, `user_id`, `reply_to_uid` | TSID foreign/semantic field | `BIGINT` | `post_id` 是动态 TSID |
| `moment_like` | `post_id`, `user_id` | TSID foreign/semantic field | `BIGINT` | 同上 |
| `moment_timeline` | `recipient_uid`, `post_id`, `author_uid` | TSID foreign/semantic field | `BIGINT` | 时间线归属/动态/作者 |
| `moment_report` | `post_id`, `reporter_uid`, `handled_by` | TSID foreign/semantic field | `BIGINT` | 举报相关 TSID |
| `conversation` | `user_id`, `peer_id`, `last_msg_id` | TSID foreign/semantic field | `VARCHAR(40)` | 语义已偏向 TSID，但物理类型仍是字符串 |
| `msg_c2c` | `msg_id` | TSID foreign/semantic field | `VARCHAR(40)` | 业务消息 ID；API 和文档仍未完全统一为 string |
| `msg_c2g` | `msg_id` | TSID foreign/semantic field | `VARCHAR(40)` | 同上 |

---

## 4. 明确不是 TSID 主键的对象

| Table | Field | Role | PostgreSQL Type | Notes |
| --- | --- | --- | --- | --- |
| `verification_code` | `id` | Business string key | `VARCHAR(80)` | 邮箱/手机号等业务键，不应注册 TSID 生成器 |
| `geo_people_nearby` | `user_id` | TSID foreign/semantic field | `BIGINT` | 主键就是用户 TSID，但不是本表独立生成的新 `id` |
| `user_setting` | `user_id` | TSID foreign/semantic field | `BIGINT` | 以 `user_id` 为主键，不应额外写入 `id` |
| `user_log` | `uid` | TSID foreign/semantic field | `BIGINT` | 日志表没有 `id` 列，不应注入 TSID 主键 |

---

## 5. Flutter / SQLite 的关键边界

| SQLite Table | Field | Role | SQLite Type | Notes |
| --- | --- | --- | --- | --- |
| `conversation` | `id` | Local surrogate key | `INTEGER PRIMARY KEY AUTOINCREMENT` | 本地会话行号，不等于后端会话 TSID |
| `conversation` | `peer_id`, `last_msg_id` | TSID foreign/semantic field | `INTEGER` | 远端实体 TSID |
| `channel_subscription` | `id` | Local surrogate key | `INTEGER PRIMARY KEY AUTOINCREMENT` | 本地缓存主键 |
| `channel_subscription` | `channel_id`, `last_message_id` | TSID foreign/semantic field | `INTEGER` | 远端频道/消息 TSID |
| `channel_admin` | `id` | Local surrogate key | `INTEGER PRIMARY KEY AUTOINCREMENT` | 本地缓存主键 |
| `channel_admin` | `channel_id`, `user_id` | TSID foreign/semantic field | `INTEGER` | 远端 TSID |
| `msg_c2c` | `id`, `from_id`, `to_id` | TSID foreign/semantic field | `INTEGER` | 本地消息表直接存远端 TSID |
| `group_notice` | `id`, `group_id`, `user_id`, `edit_user_id` | TSID foreign/semantic field | `INTEGER` | 预置库 `example10.db` 已是 v16 结构 |

---

## 6. 已确认的冲突点

1. `user_log_repo` 曾向 `user_log` 写入 `id`，但表无此列。
2. `user_setting_repo` 曾向 `user_setting` 写入 `id`，但表无此列。
3. `imboy_app:tsid_generator_names/0` 已清理以下误注册对象：
   - `user_log`
   - `user_setting`
   - `verification_code`
   - `geo_people_nearby`
4. 文档对 JSON TSID 的描述仍未和 Admin API 现实行为对齐。
5. `elib_pg_sql` 仍需要兼容导出 `insert/2`，否则大量 Repo 会在运行时触发 `undef`；这不是 TSID 语义问题，但会直接掩盖本轮修复验证。

---

## 7. 后续扩展方向

- 补全全部 Repo 和 migration 的字段矩阵
- 明确 App REST、Admin REST、WebSocket 三条协议边界是否统一使用 string
- 补一份“生成器名 <-> 表名”映射清单，消除命名错位
