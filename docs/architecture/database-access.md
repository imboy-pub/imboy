# Imboy 数据库访问层规范

> Last Updated: 2026-03-09  
> Status: 长期架构文档  
> Scope: `src/repo/`、`src/ds/` 与 `elib_pg` / `elib_pg_sql` 的数据库访问约束  
> Source of truth: `src/repo/`, `src/ds/`, `src/lib/elib_pg.erl`, `src/lib/elib_pg_sql.erl`  
> Related docs: `docs/architecture/overview.md`, `docs/reference/engineering/migration-naming.md`

## 1. 文档定位

本文档说明在当前 `Handler -> Logic -> DS -> Repo` 架构下，数据库访问应如何落到 `Repo` / `DS` 与公共库。

核心目标：

1. 数据访问入口统一；
2. SQL 参数化约束统一；
3. 返回值语义统一；
4. 安全与可维护性优先。

## 2. 核心模块

- `elib_pg`：数据库连接、查询执行、事务封装，返回结果会规整为 map 列表或单个 map；
- `elib_pg_sql`：SQL 构建工具，输出 `{Sql, Params}`，用于参数化查询与动态条件拼装。

## 3. 强制要求

### 所有数据库操作必须使用 `elib_pg` 模块


**原则**：
- ✅ **必须**使用 `elib_pg` 模块进行所有数据库操作
- ❌ **禁止**直接使用 `epgsql` 模块
- ❌ **禁止**绕过 `elib_pg` 的连接池管理

**正确示例**：
```erlang
% ✅ 查询操作
{ok, Rows} = elib_pg:query("SELECT * FROM users WHERE id = $1", [UserId])

% ✅ 插入操作
{ok, Count} = elib_pg:insert(<<"users">>, #{<<"name">> => <<"Alice">>})

% ✅ 更新操作
{ok, Count} = elib_pg:update(<<"users">>, #{<<"name">> => <<"Bob">>}, <<"id = $1">>, [UserId])

% ✅ 删除操作
{ok, Count} = elib_pg:execute("DELETE FROM users WHERE id = $1", [UserId])

% ✅ 事务操作
elib_pg:with_tx(fun(Conn) ->
    {ok, _} = elib_pg:insert(Conn, <<"users">>, UserData),
    {ok, _} = elib_pg:insert(Conn, <<"user_logs">>, LogData),
    {ok, Result}
end)
```

**错误示例**：
```erlang
% ❌ 直接使用 epgsql（禁止）
{ok, Cols, Rows} = epgsql:equery(Conn, "SELECT * FROM users")

% ❌ 字符串拼接 SQL（禁止）
Sql = "SELECT * FROM users WHERE id = " ++ integer_to_list(UserId),
elib_pg:query(Sql, [])

% ❌ 绕过连接池（禁止）
{ok, Conn} = pooler:take_connection(pool_name),
epgsql:equery(Conn, Sql, Params),
pooler:return_connection(pool_name, Conn)
```

**原因**：
1. **安全性**：`elib_pg` 强制使用参数化查询，防止 SQL 注入
2. **统一性**：统一的接口便于维护、测试和优化
3. **连接池管理**：自动获取和释放数据库连接
4. **事务支持**：提供 `with_tx` 等高级事务封装
5. **错误处理**：统一的错误处理、重试和日志记录
6. **性能监控**：便于性能分析和慢查询监控

---

## 4. 三类函数

### 1. SELECT 类 - 查询多行
```erlang
% 返回：{ok, Rows} | {error, Reason}
% Rows: [#{<<"id">> := 1, <<"name">> := <<"Alice">>}, ...]

user_repo:all() -> {ok, Rows}
```

### 2. FIND 类 - 查询单行
```erlang
% 返回：Map 或 #{}
% 找到：#{<<"id">> => 1, <<"name">> => <<"Alice">>}
% 找不到：#{}

user_repo:find_by_id(1) -> #{...}
user_repo:find_by_id(999) -> #{}
```

### 3. EXECUTE 类 - 执行操作（INSERT/UPDATE/DELETE）
```erlang
% 返回：{ok, Affected} | {ok, Affected, Returning} | {error, Reason}
% Affected: 受影响行数（0, 1, 2...）

user_repo:save(Data) -> {ok, 1}
user_repo:update(1, Data) -> {ok, 1}
user_repo:delete(1) -> {ok, 1}
```

---

## 5. 重要规则

### 1. 永远用参数化查询
```erlang
% ✅ 正确 - 使用参数化
elib_pg:query("SELECT * FROM users WHERE id = $1", [UserId])

% ❌ 错误 - 拼接 SQL（SQL 注入风险）
elib_pg:query("SELECT * FROM users WHERE id = " ++ integer_to_list(UserId))
```

### 2. 区分"没找到"和"错误"
```erlang
% FIND 类：没找到返回空 Map，不是 error
find_by_id(999) -> #{}
find_by_id(abc) -> {error, syntax_error}  % 这才是真正的错误
```

### 3. 返回值要一致
同一个函数不要有时返回 `{ok, 1}`，有时返回 `{ok, Rows}`。

---

## 6. 代码示例

### SELECT 类
```erlang
%% @doc 获取所有用户
all() ->
    Sql = "SELECT id, name, email FROM users ORDER BY id",
    elib_pg:query(Sql, []).

%% 返回
% {ok, [#{<<"id">> := 1, <<"name">> := <<"Alice">>, <<"email">> := <<"a@b.com">>}]}
```

### FIND 类
```erlang
%% @doc 根据 ID 查找用户
find_by_id(Id) ->
    Sql = "SELECT id, name, email FROM users WHERE id = $1",
    elib_pg_sql:value_or_empty(elib_pg:one(Sql, [Id])).

%% 返回
% 找到：#{<<"id">> => 1, <<"name">> => <<"Alice">>}
% 找不到：#{}
% 错误：#{}
```

### EXECUTE 类
```erlang
%% @doc 新增用户
save(Data) ->
    Sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id",
    Name = maps:get(<<"name">>, Data),
    Email = maps:get(<<"email">>, Data),
    elib_pg:execute(Sql, [Name, Email]).

%% @doc 更新用户
update(Id, Data) ->
    Sql = "UPDATE users SET name = $1, email = $2 WHERE id = $3",
    Name = maps:get(<<"name">>, Data),
    Email = maps:get(<<"email">>, Data),
    elib_pg:execute(Sql, [Name, Email, Id]).

%% @doc 删除用户
delete(Id) ->
    Sql = "DELETE FROM users WHERE id = $1",
    elib_pg:execute(Sql, [Id]).
```

### 动态条件（使用 elib_pg_sql）
```erlang
%% @doc 条件查询
page_by_account(Account, Page, Size) ->
    Tb = elib_pg_sql:public_tablename(<<"user">>),
    Opts = #{
        limit => Size,
        offset => (Page - 1) * Size,
        order_by => [{id, desc}]
    },
    {Sql, Params} =
        elib_pg_sql:build_select(
            Tb,
            <<"id,account,nickname,status,created_at">>,
            #{account => Account, status => 1},
            Opts
        ),
    elib_pg:query(Sql, Params).
```

---

## 7. 安全规则

### 必须参数化的情况
- 用户输入
- 外部数据
- 配置值
- 上层传入参数

### 允许拼接的情况（仅限以下）
- 编译期固定的 SQL 片段
- 白名单化的表名/列名
```erlang
% ✅ 允许 - 排序字段来自白名单
ValidFields = [<<"id">>, <<"name">>, <<"created_at">>],
Field = <<"name">>,
lists:member(Field, ValidFields),
Sql = "SELECT * FROM users ORDER BY " ++ Field.

% ❌ 禁止 - 直接使用外部输入
Sql = "SELECT * FROM users ORDER BY " ++ UserInput.
```

---

## 8. 错误处理

### 真正的错误（返回 {error, Reason}）
- 连接失败
- SQL 语法错误
- 类型不匹配
- 权限不足

### 不是错误（返回空结果）
- SELECT 返回 0 行
- UPDATE 影响 0 行
- DELETE 影响 0 行
- FIND 找不到记录

---

## 9. 快速参考

| 类型 | 函数命名 | 返回值 | 示例 |
|------|----------|--------|------|
| SELECT | `all/0`, `list/0`, `page/3` | `{ok, [map()]}` | `user_repo:all()` |
| FIND | `find_by_id/1`, `find_by_xxx/1` | `Map` 或 `#{}` | `user_repo:find_by_id(1)` |
| EXECUTE | `save/1`, `update/2`, `delete/1` | `{ok, Affected}` | `user_repo:save(Data)` |

---

## 10. 检查清单

写完 repo 函数后，问自己：

1. **是否使用参数化？** 所有外部参数都用 `$1, $2...`
2. **返回值一致吗？** 同一函数不会返回不同形态
3. **区分了"没找到"和"错误"吗？** FIND 类返回 `#{}` 而不是 error
4. **错误信息保留了吗？** `{error, Reason}` 包含足够信息

---

## 11. 相关文件

- `src/lib/elib_pg.erl` - 数据库连接模块
- `src/lib/elib_pg_sql.erl` - SQL 构建工具
- `src/repo/*.erl` - 所有 Repository 实现
