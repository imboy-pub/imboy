# Repo 层文档 - 数据仓库层

[根目录](../CLAUDE.md) > **src/repo**

> **最后更新**: 2026-02-01 04:35:00 CST
> **模块数量**: 35 个
> **职责**: 封装数据库操作，使用 elib_pg 访问 PostgreSQL

---

## 模块职责

Repo 层是 Imboy 系统的数据访问层，负责：
- 封装数据库操作
- 使用 `elib_pg` 模块访问 PostgreSQL
- 提供 CRUD 操作接口
- 数据验证与转换
- SQL 查询构建

---

## 入口与启动

Repo 模块由 DS 层调用：

```erlang
% DS 调用 Repo
{ok, User} = user_repo:find_by_uid(Uid).

% Repo 使用 elib_pg
{ok, _, [{Res}]} = elib_pg:query(<<"SELECT * FROM user WHERE uid = $1">>, [Uid]).
```

---

## 对外接口

### 用户相关 Repo

| Repo | 说明 |
|------|------|
| `user_repo.erl` | 用户信息 |
| `user_setting_repo.erl` | 用户设置 |
| `user_device_repo.erl` | 用户设备 |
| `user_collect_repo.erl` | 用户收藏 |
| `user_denylist_repo.erl` | 黑名单 |
| `user_tag_repo.erl` | 用户标签 |
| `user_tag_relation_repo.erl` | 标签关系 |
| `user_log_repo.erl` | 用户日志 |

### 好友相关 Repo

| Repo | 说明 |
|------|------|
| `friend_repo.erl` | 好友关系 |
| `friend_category_repo.erl` | 好友分组 |

### 群组相关 Repo

| Repo | 说明 |
|------|------|
| `group_repo.erl` | 群组信息 |
| `group_member_repo.erl` | 群成员 |
| `group_notice_repo.erl` | 群公告 |
| `group_log_repo.erl` | 群日志 |
| `group_random_code_repo.erl` | 群随机码 |

### 消息相关 Repo

| Repo | 说明 |
|------|------|
| `msg_c2c_repo.erl` | 单聊消息 |
| `msg_c2g_repo.erl` | 群聊消息 |
| `msg_c2g_timeline_repo.erl` | 群聊时间线 |
| `msg_c2s_repo.erl` | 客户端请求 |
| `msg_s2c_repo.erl` | 系统消息 |
| `msg_store_repo.erl` | 消息存储（staging 暂存） |
| `msg_archive_repo.erl` | 永久消息归档（conv_seq 游标） |
| `msg_read_repo.erl` | 消息已读回执 |

### E2EE 相关 Repo

| Repo | 说明 |
|------|------|
| `e2ee_transfer_repo.erl` | E2EE 设备间传输 |
| `e2ee_social_repo.erl` | E2EE 社交恢复 |
| `e2ee_local_backup_repo.erl` | E2EE 本地备份 |

### 其他 Repo

| Repo | 说明 |
|------|------|
| `adm_user_repo.erl` | 管理员用户 |
| `app_version_repo.erl` | 应用版本 |
| `app_ddl_repo.erl` | DDL 配置 |
| `attachment_repo.erl` | 附件信息 |
| `feedback_repo.erl` | 用户反馈 |
| `feedback_reply_repo.erl` | 反馈回复 |
| `verification_code_repo.erl` | 验证码 |
| `fts_user_repo.erl` | 全文搜索用户 |
| `geo_people_nearby_repo.erl` | 附近的人 |

---

## 关键依赖与配置

### 依赖的基础库

- **`elib_pg.erl`**: 所有数据库操作必须使用此模块
- **`imboy_cache.erl`**: 缓存操作
- **`elib_tsid.erl`**: TSID 分布式 ID 生成（insert 时调用 `elib_tsid:generate(table_name)`）

### 数据库配置

在 `config/sys.config` 中配置：

```erlang
{pg_conf, #{name => pgsql,
    max_count => 80,
    init_count => 5,
    start_mfa => {epgsql, connect, [...]}}}
```

---

## 数据模型

### SQL 查询示例

```erlang
% 使用参数化查询（防止 SQL 注入）
{ok, _, [{Res}]} = elib_pg:query(
    <<"SELECT * FROM user WHERE uid = $1">>,
    [Uid]
).

% 批量插入
{ok, _} = elib_pg:query(
    <<"INSERT INTO msg_c2c (from_id, to_id, payload) VALUES ",
      "($1, $2, $3), ($4, $5, $6)">>,
    [Uid1, Uid2, Payload1, Uid3, Uid4, Payload2]
).
```

---

## 测试与质量

### 测试文件位置

```
test/repo/
├── adm_user_repo_tests.erl
├── app_ddl_repo_tests.erl
├── app_version_repo_tests.erl
├── attachment_repo_tests.erl
├── feedback_reply_repo_tests.erl
├── feedback_repo_tests.erl
├── friend_category_repo_tests.erl
├── friend_repo_tests.erl
├── fts_user_repo_tests.erl
├── geo_people_nearby_repo_tests.erl
├── group_log_repo_tests.erl
├── group_member_repo_tests.erl
├── group_notice_repo_tests.erl
├── group_random_code_repo_tests.erl
├── group_repo_tests.erl
├── msg_c2c_repo_tests.erl
├── msg_c2g_repo_tests.erl
├── msg_c2g_timeline_repo_tests.erl
├── msg_c2s_repo_tests.erl
├── msg_s2c_repo_tests.erl
├── user_collect_repo_tests.erl
├── user_denylist_repo_tests.erl
├── user_device_repo_tests.erl
├── user_log_repo_tests.erl
├── user_repo_tests.erl
├── user_setting_repo_tests.erl
├── user_tag_relation_repo_tests.erl
├── user_tag_repo_tests.erl
└── verification_code_repo_tests.erl
```

---

## 常见问题 (FAQ)

### Q: 如何添加新的数据仓库?

1. 在 `src/repo/` 创建新的 Repo 文件
2. 使用 `elib_pg:query/2` 进行数据库操作
3. 编写测试

### Q: 如何处理分页查询?

使用 `elib_pg_sql:select/4` 构建分页查询：

```erlang
{Sql, Params} = elib_pg_sql:select(
    <<"user">>,
    [<<"uid">>, <<"nickname">>],
    [{<<"status">>, 1}],
    #{limit => 20, offset => 0}
).
```

---

## 相关文件清单

### Repo 文件 (36 个)

```
src/repo/
├── adm_user_repo.erl
├── app_ddl_repo.erl
├── app_version_repo.erl
├── attachment_repo.erl
├── e2ee_local_backup_repo.erl
├── e2ee_social_repo.erl
├── e2ee_transfer_repo.erl
├── feedback_reply_repo.erl
├── feedback_repo.erl
├── friend_category_repo.erl
├── friend_repo.erl
├── fts_user_repo.erl
├── geo_people_nearby_repo.erl
├── group_log_repo.erl
├── group_member_repo.erl
├── group_notice_repo.erl
├── group_random_code_repo.erl
├── group_repo.erl
├── msg_c2c_repo.erl
├── msg_c2g_repo.erl
├── msg_c2g_timeline_repo.erl
├── msg_c2s_repo.erl
├── msg_s2c_repo.erl
├── msg_store_repo.erl
├── msg_read_repo.erl
├── user_collect_repo.erl
├── user_denylist_repo.erl
├── user_device_repo.erl
├── user_log_repo.erl
├── user_repo.erl
├── user_setting_repo.erl
├── user_tag_relation_repo.erl
├── user_tag_repo.erl
└── verification_code_repo.erl
```

---

## 变更记录 (Changelog)

### 2026-04-04
- 新增 `msg_archive_repo.erl` 永久消息归档仓库层
  - `conv_key/3`：生成 per-conversation 唯一键（c2c/c2g）
  - `next_conv_seq/1`：原子递增 per-conversation 序列号
  - `archive/1`：写入 msg_store 永久表（幂等）
  - `get_history/3,4`：基于 conv_seq 游标的历史查询

### 2026-02-01
- 新增 `e2ee_transfer_repo.erl` E2EE 设备间传输
- 新增 `e2ee_social_repo.erl` E2EE 社交恢复
- 新增 `e2ee_local_backup_repo.erl` E2EE 本地备份
- 更新模块数量：32 → 35

### 2026-01-20
- 完善 Repo 层文档
- 新增 `msg_store_repo.erl` 消息存储仓库

---

**文档维护**: 请在添加新的数据仓库时同步更新此文档。
