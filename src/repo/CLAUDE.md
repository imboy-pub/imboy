[根目录](../CLAUDE.md) > **src/repo**

---

# Repo 层 (src/repo/)

> **最后更新**: 2026-01-07 10:05:54 CST
> **模块数量**: 32 个 | **覆盖率**: 80%

## 模块职责

Repo 层是 Imboy 系统的 **数据访问层 (Repository Layer)**，负责：

1. **数据库操作**: 所有 SQL 查询和执行
2. **参数化查询**: 防止 SQL 注入
3. **数据映射**: 将数据库行转换为 Erlang 数据结构
4. **连接管理**: 通过 `imboy_pg` 使用连接池

## 模块列表

### 用户相关

| 模块 | 说明 |
|------|------|
| `user_repo.erl` | 用户基础信息 |
| `user_device_repo.erl` | 用户设备 |
| `user_setting_repo.erl` | 用户设置 |
| `user_collect_repo.erl` | 用户收藏 |
| `user_denylist_repo.erl` | 用户黑名单 |
| `user_tag_repo.erl` | 用户标签 |
| `user_tag_relation_repo.erl` | 标签关联 |
| `user_log_repo.erl` | 用户日志 |

### 关系相关

| 模块 | 说明 |
|------|------|
| `friend_repo.erl` | 好友关系 |
| `friend_category_repo.erl` | 好友分组 |
| `group_repo.erl` | 群组信息 |
| `group_member_repo.erl` | 群成员 |
| `group_notice_repo.erl` | 群公告 |
| `group_log_repo.erl` | 群日志 |
| `group_random_code_repo.erl` | 群随机码 |

### 消息相关

| 模块 | 说明 |
|------|------|
| `msg_c2c_repo.erl` | 单聊消息 |
| `msg_c2g_repo.erl` | 群聊消息 |
| `msg_c2g_timeline_repo.erl` | 群消息时间线 |
| `msg_c2s_repo.erl` | C2S 消息 |
| `msg_s2c_repo.erl` | S2C 消息 |
| `msg_store_repo.erl` | 消息存储（staging 表） |

### 系统相关

| 模块 | 说明 |
|------|------|
| `verification_code_repo.erl` | 验证码 |
| `feedback_repo.erl` | 用户反馈 |
| `feedback_reply_repo.erl` | 反馈回复 |
| `attachment_repo.erl` | 附件信息 |
| `app_version_repo.erl` | APP 版本 |
| `app_ddl_repo.erl` | DDL 管理 |
| `adm_user_repo.erl` | 管理员用户 |

### 功能扩展

| 模块 | 说明 |
|------|------|
| `fts_user_repo.erl` | 全文搜索用户 |
| `geo_people_nearby_repo.erl` | 附近的人 |

## 对外接口

### 函数分类

#### SELECT 类 - 查询多行

```erlang
% 返回: {ok, Columns, Rows}
user_repo:all() -> {ok, [<<"id">>, ...], [{1, ...}, ...]}
```

#### FIND 类 - 查询单行

```erlang
% 返回: Map 或 #{}
user_repo:find_by_id(Id) -> #{<<"id">> => 1, ...} | #{}
```

#### EXECUTE 类 - 执行操作

```erlang
% 返回: {ok, Affected}
user_repo:save(Data) -> {ok, 1}
user_repo:update(Id, Data) -> {ok, 1}
user_repo:delete(Id) -> {ok, 1}
```

## 核心模块

### 用户仓库 (`user_repo.erl`)

```erlang
% 根据 ID 查找
find_by_id(Uid) -> Map | #{}

% 根据账号查找
find_by_account(Account) -> Map | #{}

% 保存用户
save(Data) -> {ok, Uid} | {error, Reason}

% 更新用户
update(Uid, Data) -> {ok, 1} | {error, Reason}

% 用户列表
page(Page, Size) -> {ok, Total, List}
```

### 好友仓库 (`friend_repo.erl`)

```erlang
% 查找好友关系
find(Uid, FriendUid) -> Map | #{}

% 添加好友
insert(Uid, FriendUid, Remark) -> {ok, 1} | {error, Reason}

% 删除好友
delete(Uid, FriendUid) -> {ok, 1}

% 好友列表
list(Uid, Page, Size) -> {ok, Total, List}
```

### 消息仓库

#### 单聊消息 (`msg_c2c_repo.erl`)

```erlang
% 保存消息
save(Msg) -> {ok, 1} | {error, Reason}

% 消息历史
history(Uid, ToUid, Page, Size) -> {ok, Total, List}

% 未读消息计数
unread_count(Uid, ToUid) -> integer()
```

#### 群聊消息 (`msg_c2g_repo.erl`)

```erlang
% 保存群消息
save(Msg) -> {ok, 1}

% 群消息历史
history(GroupId, Page, Size) -> {ok, Total, List}
```

## 数据库访问规范

### 必须使用参数化查询

```erlang
% ✅ 正确
imboy_pg:query("SELECT * FROM users WHERE id = $1", [Uid])

% ❌ 错误 - SQL 注入风险
imboy_pg:query("SELECT * FROM users WHERE id = " ++ integer_to_list(Uid))
```

### 使用 SQL 构建器

```erlang
% 动态条件查询
Sql = imboy_pg_sql:select("users")
    |> imboy_pg_sql:where(#{<<"account">> => Account})
    |> imboy_pg_sql:limit(Size)
    |> imboy_pg_sql:offset((Page - 1) * Size)
    |> imboy_pg_sql:build(),
imboy_pg:query(Sql, []).
```

### 返回值约定

| 类型 | 返回值 | 说明 |
|------|--------|------|
| SELECT | `{ok, Cols, Rows}` | Cols 是列名列表，Rows 是行列表 |
| FIND | `Map` 或 `#{}` | 找到返回 Map，没找到返回空 Map |
| EXECUTE | `{ok, Affected}` | Affected 是影响行数 |

## 关键依赖

- `src/lib/imboy_pg.erl`: 数据库连接
- `src/lib/imboy_pg_sql.erl`: SQL 构建器
- `epgsql`: PostgreSQL 驱动
- `pooler`: 连接池管理

## 数据模型

### 数据库行到 Map 的转换

```erlang
row_to_map(Row) ->
    Columns = [<<"id">>, <<"name">>, <<"email">>],
    maps:from_lists(lists:zip(Columns, Row)).
```

### Map 到数据库行的转换

```erlang
map_to_row(Map) ->
    {ok, Name} = maps:find(<<"name">>, Map),
    {ok, Email} = maps:find(<<"email">>, Map),
    [Name, Email].
```

## 测试覆盖

### 测试文件

```
test/repo/
├── user_repo_tests.erl
├── friend_repo_tests.erl
├── group_repo_tests.erl
├── msg_c2c_repo_tests.erl
└── ...
```

### 覆盖情况

- **覆盖率**: 约 80%
- **已测试**: 基本 CRUD 操作
- **待补充**: 复杂查询、边界情况

## 常见问题

### Q: 如何处理查询没找到?

A:
```erlang
case user_repo:find_by_id(Uid) of
    #{}} -> {error, not_found};
    User -> {ok, User}
end.
```

### Q: 如何处理事务?

A:
```erlang
imboy_pg:transaction(fun() ->
    {ok, _} = user_repo:save(UserData),
    {ok, _} = friend_repo:insert(Uid, FriendUid),
    {ok, success}
end).
```

### Q: 如何批量操作?

A: 使用 PostgreSQL 的 `IN` 子句：
```erlang
Ids = [1, 2, 3],
Sql = "SELECT * FROM users WHERE id = ANY($1)",
imboy_pg:query(Sql, [Ids]).
```

## 相关文件

- `src/lib/imboy_pg.erl`: 数据库连接模块
- `src/lib/imboy_pg_sql.erl`: SQL 构建工具
- `doc/architecture/database-access.md`: 详细规范文档
- `test/repo/`: 测试文件

## 变更记录

### 2026-01-07
- 更新模块列表
- 更新覆盖率统计

### 2026-01-03
- 初始化 Repo 层文档
- 整理数据访问规范
