# WebSocket REPL 开发速记

> **定位**：开发者本地联调笔记，不面向运营/部署。
> **生产环境请使用** `/setup` 首启向导（P0-5）创建管理员；普通用户通过 App 注册。

---

## 1. 创建本地测试账号（开发阶段）

> 注意：生产环境禁用 `erl remote_console` 直接写库的路径，统一走 `/adm/setup/init` 首启向导 + App 注册流程。

```bash
cd imboy
IMBOYENV=local make run
# 另一个终端：
_rel/imboy/bin/imboy remote_console
```

Erlang shell 中：

```erlang
user_repo:save(#{
    mobile => <<"13800000001">>,
    password => elib_password:generate(elib_hasher:md5("test123456")),
    account => <<"alice@test.com">>,
    status => 1,
    nickname => <<"Alice">>,
    reg_cosv => <<"ios">>,
    reg_ip => <<"127.0.0.1">>,
    created_at => elib_dt:now()
}).

user_repo:save(#{
    mobile => <<"13800000002">>,
    password => elib_password:generate(elib_hasher:md5("test123456")),
    account => <<"bob@test.com">>,
    status => 1,
    nickname => <<"Bob">>,
    reg_cosv => <<"ios">>,
    reg_ip => <<"127.0.0.1">>,
    created_at => elib_dt:now()
}).
```

## 2. 开两个终端联调聊天

```bash
# 前置：安装 ws 库（Node 内置 WebSocket 不支持自定义 headers）
npm i ws

# 终端 1 — Alice
node imboy/scripts/ws-repl.mjs --account alice@test.com --pwd test123456

# 终端 2 — Bob
node imboy/scripts/ws-repl.mjs --account bob@test.com --pwd test123456
```

## 3. 聊天命令

```
Alice> .c2c <bob_uid> 你好 Bob!
Bob>   .c2c <alice_uid> 收到!
```

---

## 历史

本文件内容从根 `README.md` 迁移而来（2026-04-11，README 重写为产品门面时）。
