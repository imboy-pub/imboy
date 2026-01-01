# Imboy 数据库访问层规范

## 📋 概述

Imboy 使用 4 层架构，Repository 层（`src/repo/`）负责所有数据库操作。

### 核心模块
- `imboy_pg` - 数据库连接和查询执行
- `imboy_pg_sql` - SQL 构建工具（参数化查询，防注入）

---

## 🎯 三类函数

### 1. SELECT 类 - 查询多行
```erlang
% 返回：{ok, Columns, Rows}
% Columns: [<<"id">>, <<"name">>]
% Rows: [{1, <<"Alice">>}, {2, <<"Bob">>}]

user_repo:all() -> {ok, Columns, Rows}
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
% 返回：{ok, Affected} 或 {error, Reason}
% Affected: 受影响行数（0, 1, 2...）

user_repo:save(Data) -> {ok, 1}
user_repo:update(1, Data) -> {ok, 1}
user_repo:delete(1) -> {ok, 1}
```

---

## ⚠️ 重要规则

### 1. 永远用参数化查询
```erlang
% ✅ 正确 - 使用参数化
imboy_pg:query("SELECT * FROM users WHERE id = $1", [UserId])

% ❌ 错误 - 拼接 SQL（SQL 注入风险）
imboy_pg:query("SELECT * FROM users WHERE id = " ++ integer_to_list(UserId))
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

## 📝 代码示例

### SELECT 类
```erlang
%% @doc 获取所有用户
all() ->
    Sql = "SELECT id, name, email FROM users ORDER BY id",
    imboy_pg:query(Sql, []).

%% 返回
% {ok, [<<"id">>, <<"name">>, <<"email">>], [{1, <<"Alice">>, <<"a@b.com">>}]}
```

### FIND 类
```erlang
%% @doc 根据 ID 查找用户
find_by_id(Id) ->
    Sql = "SELECT id, name, email FROM users WHERE id = $1",
    case imboy_pg:query(Sql, [Id]) of
        {ok, _Cols, [Row | _]} -> row_to_map(Row);
        {ok, _Cols, []} -> #{};
        {error, Reason} -> {error, Reason}
    end.

%% 返回
% 找到：#{<<"id">> => 1, <<"name">> => <<"Alice">>}
% 找不到：#{}
% 错误：{error,Reason}
```

### EXECUTE 类
```erlang
%% @doc 新增用户
save(Data) ->
    Sql = "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id",
    Name = maps:get(<<"name">>, Data),
    Email = maps:get(<<"email">>, Data),
    imboy_pg:query(Sql, [Name, Email]).

%% @doc 更新用户
update(Id, Data) ->
    Sql = "UPDATE users SET name = $1, email = $2 WHERE id = $3",
    Name = maps:get(<<"name">>, Data),
    Email = maps:get(<<"email">>, Data),
    imboy_pg:query(Sql, [Name, Email, Id]).

%% @doc 删除用户
delete(Id) ->
    Sql = "DELETE FROM users WHERE id = $1",
    imboy_pg:query(Sql, [Id]).
```

### 动态条件（使用 imboy_pg_sql）
```erlang
%% @doc 条件查询
page_by_account(Account, Page, Size) ->
    Sql = imboy_pg_sql:select("users")
        |> imboy_pg_sql:where(#{
            <<"account">> => Account,
            <<"status">> => 1
        })
        |> imboy_pg_sql:limit(Size)
        |> imboy_pg_sql:offset((Page - 1) * Size)
        |> imboy_pg_sql:order_by(<<"id DESC">>)
        |> imboy_pg_sql:build(),
    imboy_pg:query(Sql, []).
```

---

## 🔒 安全规则

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

## 📦 错误处理

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

## 🎓 快速参考

| 类型 | 函数命名 | 返回值 | 示例 |
|------|----------|--------|------|
| SELECT | `all/0`, `list/0`, `page/3` | `{ok, Cols, Rows}` | `user_repo:all()` |
| FIND | `find_by_id/1`, `find_by_xxx/1` | `Map` 或 `#{}` | `user_repo:find_by_id(1)` |
| EXECUTE | `save/1`, `update/2`, `delete/1` | `{ok, Affected}` | `user_repo:save(Data)` |

---

## ✅ 检查清单

写完 repo 函数后，问自己：

1. **是否使用参数化？** 所有外部参数都用 `$1, $2...`
2. **返回值一致吗？** 同一函数不会返回不同形态
3. **区分了"没找到"和"错误"吗？** FIND 类返回 `#{}` 而不是 error
4. **错误信息保留了吗？** `{error, Reason}` 包含足够信息

---

## 📚 相关文件

- `src/lib/imboy_pg.erl` - 数据库连接模块
- `src/lib/imboy_pg_sql.erl` - SQL 构建工具
- `src/repo/*.erl` - 所有 Repository 实现
