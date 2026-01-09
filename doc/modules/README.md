# Imboy 模块索引

> **架构**: 单应用 4 层架构 (Handler → Logic → DS → Repo)

---

## 架构总览

```
┌─────────────────────────────────────────────────────────────┐
│                    Handler 层 (API)                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   HTTP REST  │  │   WebSocket  │  │   Admin API  │      │
│  │    Handler   │  │    Handler   │  │    Handler   │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Logic 层 (业务逻辑)                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ Friend Logic │  │  Group Logic │  │   Msg Logic  │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    DS 层 (数据服务)                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   User DS    │  │   Auth DS    │  │   Config DS  │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Repo 层 (数据访问)                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  User Repo   │  │  Friend Repo │  │   Msg Repo   │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
```

## 模块分层

| 层级 | 目录 | 数量 | 说明 |
|------|------|------|------|
| **Handler** | `src/api/` | 27 个 | HTTP REST API 处理器 |
| **Admin** | `src/adm/` | 7 个 | 管理后台 API 处理器 |
| **Logic** | `src/logic/` | 26 个 | 业务逻辑层 |
| **DS** | `src/ds/` | 13 个 | 数据服务层 |
| **Repo** | `src/repo/` | 32 个 | 数据仓库层 |
| **Lib** | `src/lib/` | 29 个 | 基础库函数 |

## 各层详细索引

- **[API 层](./api.md)** - HTTP REST API 处理器 (27 个)
- **[Logic 层](./logic.md)** - 业务逻辑层 (26 个)
- **[DS 层](./ds.md)** - 数据服务层 (13 个)
- **[Repo 层](./repo.md)** - 数据仓库层 (32 个)
- **[Lib 层](./lib.md)** - 基础库函数 (29 个)
- **[Admin 层](./adm.md)** - 管理后台 API (7 个)

## 快速查找

### 按功能查找

| 功能 | Handler | Logic | DS | Repo |
|------|---------|-------|-----|------|
| 用户管理 | `user_handler` | `user_logic` | `user_ds` | `user_repo` |
| 认证授权 | `passport_handler` | `passport_logic` | `auth_ds` | `token_repo` |
| 好友管理 | `friend_handler` | `friend_logic` | - | `friend_repo` |
| 群组管理 | `group_handler` | `group_logic` | - | `group_repo` |
| 消息处理 | `msg_handler` | `msg_c2c_logic` | `message_ds` | `msg_c2c_repo` |
| WebSocket | `websocket_handler` | `websocket_logic` | `websocket_ds` | - |

### 按路由查找

| 路由前缀 | Handler | 说明 |
|---------|---------|------|
| `/user/*` | `user_handler` | 用户管理 |
| `/passport/*` | `passport_handler` | 认证授权 |
| `/friend/*` | `friend_handler` | 好友管理 |
| `/group/*` | `group_handler` | 群组管理 |
| `/msg/*` | `msg_handler` | 消息处理 |
| `/ws` | `websocket_handler` | WebSocket 连接 |
| `/adm/*` | `adm_*_handler` | 管理后台 |

## 相关文档

- **主文档**: [CLAUDE.md](../../CLAUDE.md)
- **编码规范**: [standards/](../standards/)
- **WebSocket API**: [api/websocket-api.md](../api/websocket-api.md)
- **架构设计**: [architecture/](../architecture/)
