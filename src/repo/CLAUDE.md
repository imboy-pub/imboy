# Repo 层文档 - 数据仓库层

[根目录](../CLAUDE.md) > **src/repo**

> **最后更新**: 2026-04-04 | **模块数量**: 35 个
> **职责**: 封装数据库操作，使用 elib_pg 访问 PostgreSQL，提供 CRUD 接口

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

### 用户相关（9 个）

| Repo | 说明 |
|------|------|
| `user_repo` | 用户信息 |
| `user_setting_repo` | 用户设置 |
| `user_dnd_rule_repo` | 用户免打扰(DND)规则 |
| `user_device_repo` | 用户设备（E2EE 公钥） |
| `user_collect_repo` | 用户收藏 |
| `user_denylist_repo` | 黑名单 |
| `user_tag_repo` | 用户标签 |
| `user_tag_relation_repo` | 标签关系 |
| `user_log_repo` | 用户日志 |

### 好友 / 群组（7 个）

| Repo | 说明 |
|------|------|
| `friend_repo` | 好友关系 |
| `friend_category_repo` | 好友分组 |
| `group_repo` | 群组信息 |
| `group_member_repo` | 群成员 |
| `group_notice_repo` | 群公告 |
| `group_log_repo` | 群日志 |
| `group_random_code_repo` | 群随机码 |

### 消息相关（8 个）

| Repo | 说明 |
|------|------|
| `msg_c2c_repo` | 单聊消息（投递队列） |
| `msg_c2g_repo` | 群聊消息（投递队列） |
| `msg_c2g_timeline_repo` | 群聊时间线 |
| `msg_c2s_repo` | 客户端请求 |
| `msg_s2c_repo` | 系统消息 |
| `msg_store_repo` | 消息暂存（staging） |
| `msg_archive_repo` | 永久消息归档（conv_seq 游标） |
| `msg_read_repo` | 消息已读回执 |

`msg_archive_repo` 关键接口：`conv_key/3`、`next_conv_seq/1`（原子递增）、`archive/1`（幂等写入）、`get_history/3,4`（基于 conv_seq 游标）

### E2EE 相关（3 个）

| Repo | 说明 |
|------|------|
| `e2ee_transfer_repo` | E2EE 设备间传输 |
| `e2ee_social_repo` | E2EE 社交恢复 |
| `e2ee_local_backup_repo` | E2EE 本地备份 |

### 其他（9 个）

| Repo | 说明 |
|------|------|
| `adm_user_repo` | 管理员用户 |
| `app_version_repo` | 应用版本 |
| `app_ddl_repo` | DDL 配置 |
| `attachment_repo` | 附件信息 |
| `feedback_repo` | 用户反馈 |
| `feedback_reply_repo` | 反馈回复 |
| `verification_code_repo` | 验证码 |
| `fts_user_repo` | 全文搜索用户 |
| `geo_people_nearby_repo` | 附近的人 |

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

## 测试文件（29 个）

`test/repo/` 目录包含主要 repo 的 `_tests.erl` 文件，覆盖：
`user`, `user_device`, `user_setting`, `user_collect`, `user_denylist`, `user_tag`, `friend`, `friend_category`, `group`, `group_member`, `group_notice`, `group_log`, `group_random_code`, `msg_c2c`, `msg_c2g`, `msg_c2g_timeline`, `msg_c2s`, `msg_s2c`, `adm_user`, `app_ddl`, `app_version`, `attachment`, `feedback`, `feedback_reply`, `fts_user`, `geo_people_nearby`, `verification_code` 等。

---

## 测试配置

- 框架：EUnit；超时：30s；环境：`application:set_env(imboy, env, test)`；覆盖率目标：80%

## 操作指南

- **添加新 Repo**：`src/repo/` 建文件 → 使用 `elib_pg:query/2` → 写测试
- **TSID 生成**：insert 时调用 `elib_tsid:generate(table_name)`
- **严格顺序查询**：使用 `conv_seq` 游标，不用 `msg_id`/`TSID` 作为顺序依据
