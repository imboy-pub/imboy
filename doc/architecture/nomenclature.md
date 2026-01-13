# 命名约定与代码规范

> **版本**: 0.7.3
> **架构**: 单应用 4 层架构 (Handler → Logic → DS → Repo)
> **更新时间**: 2026-01-06

---

## 目录结构

Imboy 采用**单应用架构**，所有代码都在 `imboy` 应用下，按照**DDD 分层**组织。

```
imboy/
├── src/
│   ├── api/              # API 处理器层 (27 个)
│   ├── adm/              # 管理后台 API (7 个)
│   ├── logic/            # 业务逻辑层 (26 个)
│   ├── ds/               # 数据服务层 (13 个)
│   ├── repo/             # 数据仓库层 (32 个)
│   └── lib/              # 基础库函数 (29 个)
├── include/              # 头文件
│   ├── *.hrl             # 通用头文件
│   └── log.hrl           # 日志宏定义
├── priv/                 # 私有数据
│   ├── migrations/       # 数据库迁移脚本
│   ├── static/           # 静态文件
│   └── template/         # HTML 模板
└── config/               # 配置文件
```

---

## 模块命名规范

### 文件命名

| 层级 | 命名模式 | 示例 |
|------|---------|------|
| **Handler** | `{功能}_handler.erl` | `user_handler.erl`, `group_handler.erl` |
| **Logic** | `{功能}_logic.erl` | `user_logic.erl`, `friend_logic.erl` |
| **DS** | `{实体}_ds.erl` | `user_ds.erl`, `auth_ds.erl` |
| **Repo** | `{表名}_repo.erl` | `user_repo.erl`, `friend_repo.erl` |
| **Lib** | `imboy_{功能}.erl` | `imboy_cache.erl`, `elib_cipher.erl` |

### 函数命名

```erlang
% 导出函数
-export([function_name/1, function_name/2]).

% 私有函数
internal_function(Args) -> ...
```

---

## API 层命名规范

### REST API Handler

**文件**: `src/api/{功能}_handler.erl`

```erlang
-module(user_handler).
-behavior(cowboy_rest).

-export([init/2]).
-export([show/2, update/2, list/2]).

%% Cowboy REST 回调
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
```

### 路由命名

```
GET  /api/user/show           # 查看用户资料
POST /api/user/update         # 更新用户资料
GET  /api/friend/list        # 好友列表
POST /api/group/add           # 创建群组
```

---

## Logic 层命名规范

### 业务逻辑模块

**文件**: `src/logic/{功能}_logic.erl`

```erlang
-module(user_logic).
-export([info/1, update/2, online/3]).

%% 获取用户信息
-spec info(pos_integer()) -> map().
```

### 函数分类

| 函数类型 | 命名模式 | 示例 |
|---------|---------|------|
| 查询单个 | `{实体}_{字段}` | `user_info/1`, `group_detail/1` |
| 查询列表 | `{实体}_list` 或 `{entity}_page` | `friend_list/1`, `user_page/3` |
| 创建 | `{实体}_add` 或 `add_{实体}` | `group_add/1`, `add_friend/2` |
| 更新 | `{实体}_update` 或 `update_{实体}` | `user_update/2` |
| 删除 | `{实体}_delete` 或 `delete_{实体}` | `group_delete/1` |
| 检查 | `is_{状态}` 或 `can_{操作}` | `is_friend/2`, `can_join/2` |

---

## DS 层命名规范

### 数据服务模块

**文件**: `src/ds/{实体}_ds.erl`

```erlang
-module(user_ds).
-export([user/1, update/2]).

%% 获取用户信息（带缓存）
-spec user(pos_integer()) -> map() | undefined.
```

### 缓存键格式

```erlang
% 格式：{实体, ID}
{user, Uid}
{token, Token}
{config, Key}
```

---

## Repo 层命名规范

### 数据仓库模块

**文件**: `src/repo/{表名}_repo.erl`

```erlang
-module(user_repo).
-export([find_by_id/1, save/1, update/2, delete/1]).

%% 根据ID查找
-spec find_by_id(pos_integer()) -> map() | #{}.

%% 保存
-spec save(map()) -> {ok, pos_integer()} | {error, term()}.
```

### SQL 函数命名

| 操作 | 函数名 | 示例 |
|------|-------|------|
| 查询单行 | `find_by_{字段}` | `find_by_id/1`, `find_by_account/1` |
| 查询多行 | `list_by_{字段}` | `list_by_uid/1`, `list_by_gid/1` |
| 分页查询 | `page` | `page/3` |
| 插入 | `save` 或 `insert` | `save/1` |
| 更新 | `update` | `update/2` |
| 删除 | `delete` | `delete/1` |
| 计数 | `count_by_{字段}` | `count_by_uid/1` |

---

## Lib 层命名规范

### 基础库模块

**文件**: `src/lib/imboy_{功能}.erl`

| 模块 | 功能 |
|------|------|
| `imboy_cache` | 缓存操作 |
| `elib_pg` | 数据库连接 |
| `elib_pg_sql` | SQL 构建 |
| `elib_cipher` | 加解密 |
| `elib_password` | 密码哈希 |
| `elib_hashids` | ID 混淆 |
| `elib_req` | HTTP 请求 |
| `elib_response` | 响应格式化 |
| `imboy_syn` | 进程注册 |
| `elib_log` | 日志记录 |

---

## 数据库表命名规范

### 表名格式

```
{业务前缀}_{表名}
```

**示例**:
- `user` - 用户表
- `user_device` - 用户设备表
- `friend_category` - 好友分类表
- `msg_c2c` - 单聊消息表
- `msg_c2g` - 群聊消息表
- `group_member` - 群组成员表

### 字段命名

```sql
-- 用户表
CREATE TABLE user (
    id BIGSERIAL PRIMARY KEY,
    account VARCHAR(255) NOT NULL,
    nickname VARCHAR(255),
    created_at BIGINT NOT NULL,
    updated_at BIGINT NOT NULL
);
```

---

## 术语规范更新

### 旧术语 → 新术语

| 旧术语（已废弃） | 新术语 |
|----------------|--------|
| `apps/imlib` | `src/lib/` |
| `apps/imrepo` | `src/repo/` |
| `apps/imds` | `src/ds/` |
| `apps/imadm` | `src/adm/` |
| `apps/imapi` | `src/api/` |

### 避免使用的术语

| 避免 | 使用 |
|------|------|
| master/slave | main/subordinate |
| blacklist | denylist |
| whitelist | allowlist |

---

## API 响应格式

### 成功响应

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {}
}
```

### 错误响应

```json
{
  "code": 1001,
  "msg": "用户不存在"
}
```

### 分页响应

```json
{
  "code": 0,
  "msg": "success.",
  "payload": {
    "total": 100,
    "page": 1,
    "size": 10,
    "list": []
  }
}
```

---

## 错误码规范

详见：[error-codes.md](../standards/error-codes.md)

| 范围 | 说明 |
|------|------|
| 0 | 成功 |
| 1000-1999 | 用户相关错误 |
| 2000-2999 | 好友相关错误 |
| 3000-3999 | 群组相关错误 |
| 4000-4999 | 消息相关错误 |
| 5000-5999 | 系统错误 |
| 7000-7999 | 认证授权错误 |

---

## 参考文档

- **架构概览**: [overview.md](./overview.md)
- **设计思考**: [design-thinking.md](./design-thinking.md)
- **数据库访问**: [database-access.md](./database-access.md)
- **API 格式规范**: [api-format.md](../standards/api-format.md)
- **错误码规范**: [error-codes.md](../standards/error-codes.md)
- **主文档**: [CLAUDE.md](../../CLAUDE.md)
