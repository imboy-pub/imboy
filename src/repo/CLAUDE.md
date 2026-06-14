# Repo 层文档 - 数据仓库层

[根目录](../CLAUDE.md) > **src/repo**

> **最后更新**: 2026-06-10 | **模块数量**: 72 个
> **职责**: 封装数据库操作，使用 elib_pg 访问 PostgreSQL，提供 CRUD 接口

以 `find src/repo -maxdepth 1 -name '*.erl' | wc -l` 为准（截至 2026-06）。

---

## 模块职责

Repo 层负责：封装所有 PostgreSQL 操作、参数化 SQL 查询（防注入）、TSID 生成（insert 时）、数据验证与转换。

**强制规则**：所有数据库操作必须通过 `elib_pg` 模块，禁止直接调用 epgsql。

---

## 调用模式

```erlang
% DS 调用 Repo
{ok, User} = user_repo:find_by_uid(Uid).

% Repo 使用 elib_pg（参数化查询）
{ok, _, [{Res}]} = elib_pg:query(<<"SELECT * FROM user WHERE uid = $1">>, [Uid]).

% 分页查询
{Sql, Params} = elib_pg_sql:select(<<"user">>, [<<"uid">>, <<"nickname">>],
    [{<<"status">>, 1}], #{limit => 20, offset => 0}).
```

---

## Repo 模块清单

### 用户管理（11 个）

| Repo | 说明 |
|------|------|
| `user_repo` | 用户信息 |
| `user_setting_repo` | 用户设置 |
| `user_device_repo` | 用户设备（E2EE 公钥） |
| `user_dnd_rule_repo` | 用户免打扰(DND)规则 |
| `user_collect_repo` | 用户收藏 |
| `user_denylist_repo` | 黑名单 |
| `user_tag_repo` | 用户标签 |
| `user_tag_relation_repo` | 用户标签关系 |
| `user_log_repo` | 用户操作日志 |
| `adm_user_repo` | 管理员用户 |
| `account_ds` | 账户数据服务（虚拟模块，见 DS 层） |

### 认证与安全（3 个）

| Repo | 说明 |
|------|------|
| `verification_code_repo` | 验证码管理 |
| `compliance_key_repo` | 合规密钥管理 |
| `fts_user_repo` | 全文搜索用户索引 |

### 好友与社交（4 个）

| Repo | 说明 |
|------|------|
| `friend_repo` | 好友关系 |
| `friend_category_repo` | 好友分组 |
| `mention_repo` | @提及数据访问 |
| `geo_people_nearby_repo` | 附近的人 |

### 群组管理（13 个）

| Repo | 说明 |
|------|------|
| `group_repo` | 群组信息 |
| `group_member_repo` | 群成员 |
| `group_category_repo` | 群分类 |
| `group_notice_repo` | 群公告 |
| `group_log_repo` | 群操作日志 |
| `group_random_code_repo` | 入群码 |
| `group_album_repo` | 群相册 |
| `group_file_repo` | 群文件 |
| `group_tag_repo` | 群标签 |
| `group_schedule_repo` | 群日程 |
| `group_task_repo` | 群任务 |
| `group_task_assignment_repo` | 群任务分配 |
| `group_vote_repo` | 群投票 |

### 消息处理（11 个）

| Repo | 说明 |
|------|------|
| `msg_c2c_repo` | 单聊消息（投递队列） |
| `msg_c2g_repo` | 群聊消息（投递队列） |
| `msg_c2g_timeline_repo` | 群聊时间线索引 |
| `msg_c2s_repo` | 客户端请求消息 |
| `msg_s2c_repo` | 系统消息 |
| `msg_store_repo` | 消息暂存（staging） |
| `msg_archive_repo` | 永久消息存储（conv_seq 游标） |
| `msg_read_repo` | 消息已读回执 |
| `msg_forward_repo` | 消息转发 |
| `msg_reaction_repo` | 消息表情反应 |

`msg_archive_repo` 关键接口：`conv_key/3`、`next_conv_seq/1`（原子递增）、`archive/1`（幂等写入）、`get_history/3,4`（基于 conv_seq 游标）

### E2EE（4 个）

| Repo | 说明 |
|------|------|
| `e2ee_transfer_repo` | E2EE 设备间传输 |
| `e2ee_social_repo` | E2EE 社交恢复 |
| `e2ee_local_backup_repo` | E2EE 本地备份 |
| `e2ee_shard_transmission_log_repo` | E2EE 分片传输日志 |

### 频道系统（5 个）

| Repo | 说明 |
|------|------|
| `channel_repo` | 频道信息 |
| `channel_admin_repo` | 频道管理员 |
| `channel_subscription_repo` | 频道订阅 |
| `channel_message_repo` | 频道消息 |
| `channel_order_repo` | 付费频道订单 |
| `channel_invitation_repo` | 频道邀请 |

### 朋友圈（7 个）

| Repo | 说明 |
|------|------|
| `moment_post_repo` | 朋友圈动态 |
| `moment_post_acl_repo` | 朋友圈权限控制 |
| `moment_comment_repo` | 朋友圈评论 |
| `moment_like_repo` | 朋友圈点赞 |
| `moment_timeline_repo` | 朋友圈时间线 |
| `moment_report_repo` | 朋友圈举报 |

### 对话与附件（3 个）

| Repo | 说明 |
|------|------|
| `conversation_delete_repo` | 对话删除记录 |
| `conversation_pin_repo` | 对话置顶 |
| `attachment_repo` | 附件信息（Garage S3） |

### 应用与版本（4 个）

| Repo | 说明 |
|------|------|
| `app_version_repo` | 应用版本 |
| `app_version_policy_repo` | 版本升级策略 |
| `app_upgrade_log_repo` | 应用升级日志 |
| `app_ddl_repo` | DDL 配置 |

### 公告与反馈（5 个）

| Repo | 说明 |
|------|------|
| `announcement_repo` | 公告信息 |
| `feedback_repo` | 用户反馈 |
| `feedback_reply_repo` | 反馈回复 |
| `report_ticket_repo` | 举报工单 |
| `report_action_log_repo` | 举报处理日志 |

### 其他功能（3 个）

| Repo | 说明 |
|------|------|
| `push_token_repo` | 推送令牌 |
| `live_room_repo` | 直播间 |
| `wallet_repo` | 钱包及流水 |
| `payment_transaction_repo` | 统一支付流水（对账+回调幂等，UNIQUE(gateway,gateway_payment_no)/UNIQUE(trade_no)） |
| `billing_plan_repo` | SaaS 套餐定义（code UNIQUE，price/quota_config，CRUD+分页） |
| `billing_subscription_repo` | SaaS 租户订阅（tenant_id 逻辑字段，uniq_active 单活订阅，续费/到期扫描） |
| `billing_usage_repo` | SaaS 用量记录（按 sub/metric/period upsert 累加） |
| `billing_invoice_repo` | SaaS 账单（invoice_no UNIQUE，(sub,period) 幂等，mark_paid/mark_overdue） |
| `imboy_plugin_audit_repo` | 插件审计日志 |

---

## 依赖的基础库

- `elib_pg`：所有 SQL 操作的唯一入口
- `imboy_cache`：缓存操作
- `elib_tsid`：insert 时调用 `elib_tsid:generate(table_name)` 生成 TSID

## 数据库配置（config/sys.config）

```erlang
{pg_conf, #{name => pgsql, max_count => 80, init_count => 5,
    start_mfa => {epgsql, connect, [...]}}}
```

---

## 测试文件

`test/repo/` 目录包含主要 repo 的 `_tests.erl` 文件，覆盖约 40+ 个模块的单元测试。

---

## 测试配置

- 框架：EUnit；超时：30s；环境：`application:set_env(imboy, env, test)`；覆盖率目标：80%

## 操作指南

- **添加新 Repo**：`src/repo/` 建文件 → 使用 `elib_pg:query/2` → 写测试
- **TSID 生成**：insert 时调用 `elib_tsid:generate(table_name)`
- **严格顺序查询**：使用 `conv_seq` 游标，不用 `msg_id`/`TSID` 作为顺序依据

