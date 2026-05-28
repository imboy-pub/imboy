> [imboy.pub 根目录](../CLAUDE.md) > **imboy（Erlang/OTP 后端）**

# Imboy - AI 上下文文档 / AI Context Document

> **最后更新 / Last updated**: 2026-04-15 CST | **版本**: 1.0.0-rc.2
> **架构**: 单应用 4 层架构 Handler -> Logic -> DS -> Repo | **语言**: Erlang/OTP 28+ + PostgreSQL 18+

---

## 双语文档规则 / Bilingual Documentation Rule (MANDATORY)

所有面向用户/贡献者/运维的 Markdown 文档必须同时提供**简体中文 + English**。
All user/contributor/ops-facing Markdown docs MUST provide **Simplified Chinese + English**.

- **方式 A**（短文档）：同节并排。**方式 B**（长文档）：`README.md`（中文权威）+ `README.en.md`
- **Pattern A** (short): side-by-side sections. **Pattern B** (long): `README.md` (CN authoritative) + `README.en.md`
- 简体中文为权威版本，中文先改，英文同 PR 内同步。/ Simplified Chinese is source of truth; sync English in same PR.
- 代码、命令、模块名不翻译。/ Code, commands, module names are NOT translated.
- 例外（仅中文）：`.claude/plan/*`、`.claude/memory/*`、内部会议纪要
- AI Agent 契约：收到"写/改/新建 .md"任务时默认双语输出，commit message 加 `docs(bilingual):` 前缀。
- AI Agent contract: default bilingual output; prefix commits with `docs(bilingual):`.

---

## 构建系统规则 / Build System Rules

- **禁止修改 `erlang.mk`**（vendored 第三方工具）。自定义逻辑只能在 `Makefile` 中实现。
- **Do NOT modify `erlang.mk`**. All custom build logic goes in `Makefile` only.
- `IMBOYENV=local make run` 自动加载 `config/sys.local.config` → 复制为 `config/sys.runtime.config`
- 非 local 环境使用 `config/sys.config`；`IMBOY_*` 环境变量运行时优先级最高。
- Admin 前端分页：统一用 `DataTablePagination`，默认 `size:10`，分页/筛选变化时重置 `page=1`。

---

## 技术栈

| 层级 | 技术 |
|------|------|
| 语言 | Erlang/OTP 28+ |
| Web 框架 | Cowboy 2.10 (HTTP/WS) |
| 数据库 | PostgreSQL 18+ (pg_jieba, postgis, timescaledb, pgcrypto) |
| 缓存 | depcache (Erlang 内存缓存) |
| 连接池 | epgsql + pooler |
| 日志 | lager |

---

## 4 层架构

调用链：`Handler → Logic → DS → Repo → PostgreSQL`，横向 `Lib` 层提供基础设施。

| 层级 | 目录 | 数量 | 职责 | 文档 |
|------|------|------|------|------|
| Handler (API+ADM) | `src/api/` + `src/adm/` | 27+7=34 个 | HTTP/WS 入口、参数验证 | [api](./src/api/CLAUDE.md) / [adm](./src/adm/CLAUDE.md) |
| Logic | `src/logic/` | 26 个 | 业务逻辑 | [logic](./src/logic/CLAUDE.md) |
| DS | `src/ds/` | 77 个 | 数据服务、缓存封装 | [ds](./src/ds/CLAUDE.md) |
| Repo | `src/repo/` | 32 个 | SQL/PostgreSQL 访问 | [repo](./src/repo/CLAUDE.md) |
| Lib | `src/lib/` | 29 个 | 基础库 (elib_pg, elib_async, elib_retry…) | [lib](./src/lib/CLAUDE.md) |

---

## 按功能快查

| 功能 | Handler | Logic | DS | Repo |
|------|---------|-------|-----|------|
| 用户管理 | `user_handler` | `user_logic` | `user_ds` | `user_repo` |
| 认证授权 | `passport_handler` | `passport_logic` | `auth_ds` | `token_repo` |
| 好友管理 | `friend_handler` | `friend_logic` | `friend_ds` | `friend_repo` |
| 群组管理 | `group_handler` | `group_logic` | `group_ds` | `group_repo` |
| 消息处理 | `msg_handler` | `msg_c2c_logic` | `message_ds` | `msg_c2c_repo` |
| WebSocket | `websocket_handler` | `websocket_logic` | `websocket_ds` | - |
| E2EE | `e2ee_handler` | `e2ee_logic` | - | `user_device_repo` |

---

## 运行命令速查

```bash
make compile                          # 编译
IMBOYENV=local make run               # 本地运行
IMBOYENV=local make run HTTP_PORT=9800
IMBOYENV=local make rel               # 构建发布
make eunit                            # 所有测试
erl -noshell -eval "eunit:test([user_repo_tests],[verbose])" -s init stop
make dialyze                          # 类型检查
make ctl ARGS="node status"           # CLI 工具
make ctl ARGS="smoke all"             # 冒烟测试
make ctl ARGS="db ping"
make ctl ARGS="plugin list"
_rel/imboy/bin/imboy remote_console   # 远程调试 shell
lm()                                  # 热加载（在 shell 中）
config_ds:local_reload()              # 重新加载配置
observer_cli:start()                  # 节点监控
```

CLI 环境变量：`IMBOY_CTL_NODE`, `IMBOY_CTL_COOKIE`, `IMBOY_CTL_TIMEOUT`

代码生成模板：
```bash
make new t=imboy.rest_handler n=demo_handler
make new t=imboy.logic n=demo_logic
make new t=imboy.repository n=demo_repo
make new t=imboy.ds n=demo_ds
```

---

## 编码规范速查

| 规范 | 要点 |
|------|------|
| **UTF-8** | 中文字符串加 `/utf8` 后缀：`<<"操作成功"/utf8>>` |
| **错误码** | `?ERR_OK`, `?ERR_USER_NOT_FOUND` 等宏，include `error_code.hrl` |
| **数据库** | 所有 SQL 必须通过 `elib_pg` 模块，参数化查询防注入 |
| **TSID** | 分布式 ID，JSON 以 integer 传输；前端用 `safeParseBigIntJson` 转 string |
| **安全** | 所有 SQL 参数化；API 必须 JWT 认证（除 open 路由）；WebSocket 必须验证 token |

关键文件：`include/error_code.hrl`（错误码），`include/imboy_const.hrl`（常量），`src/imboy_router.erl`（路由），`priv/migrations/*.sql`（迁移）

---

## 测试策略

- 框架：EUnit；Mock：meck；超时：30s；环境标记：`application:set_env(imboy, env, test)`
- 目录：`test/api/`(50+)、`test/repo/`(40+)、`test/lib/`(30+)、`test/logic/`(20+)、`test/ds/`(10+)
- 覆盖率目标：Repo 80%、Logic 70%、Handler 60%、整体 65%

---

## 关键特性

**消息 QoS 投递**：在线立即投递 → 未确认重试 2s/5s/7s/11s → 4 次失败转离线消息 → 客户端 ACK 后清理。

**消息永久存储（conv_seq 游标）**：配置 `{msg_archive_enabled, true}`（默认 false）。严格顺序以 `conv_seq` 为准，不用 `msg_id`/`TSID`。conv_key 格式：C2C=`"c2c:{min_uid}:{max_uid}"`，C2G=`"c2g:{group_id}"`。相关模块：`msg_archive_repo`, `msg_archive_ds`, `msg_store_worker`。

**Token 刷新**：WS 连接时 token 过期仍响应成功 → 发 S2C 要求 8s 内刷新 → 失败则强制下线。

**E2EE**：RSA-OAEP-256 + AES-256-GCM；服务端不解密 `ciphertext`，仅路由存储。API：`/v1/e2ee/user_keys`、`/v1/e2ee/group_member_keys`。

**分布式**：基于 Erlang/OTP 分布式，`syn` 库进程注册发现，跨节点消息投递。

**缓存**：depcache 内存缓存，缓存键格式 `{Table, Id}`；`IMBOY_*` 环境变量运行时覆盖。

---

## FAQ（精选）

- **调试 WS**：`http://coolaf.com/tool/chattest`；Token：`token_ds:encrypt_token(Uid)`
- **DB 连接池**：`pooler:status()`
- **热加载**：`lm()`（shell 中）
- **重新加载配置**：`config_ds:local_reload()`
- **添加新端点**：`src/api/` 建 handler → `imboy_router.erl` 加路由 → `src/logic/` 建 logic → 写测试

---

## 相关文档

- 架构：[docs/architecture/overview.md](./docs/architecture/overview.md)
- 数据库访问：[docs/architecture/database-access.md](./docs/architecture/database-access.md)
- WebSocket API：[docs/api/websocket-api-2.md](./docs/api/websocket-api-2.md)
- UTF-8 规范：[docs/standards/utf8-encoding.md](./docs/standards/utf8-encoding.md)
- 错误码：[docs/standards/error-codes.md](./docs/standards/error-codes.md)
- TSID 规范：[docs/api/tsid-field-convention.md](./docs/api/tsid-field-convention.md)
- API 格式：[docs/standards/api-format.md](./docs/standards/api-format.md)
