# 本地跑通 IMBoy 后端（15 分钟）

> **类型**：教程 · **读者**：后端工程师 · **适用版本**：main 分支 · **最后验证**：2026-07-24

**你将做出什么**：一个跑在你电脑上的 IMBoy 后端节点，HTTP 接口可访问，数据库迁移自动完成，（可选）含演示数据。

**你会学到**：
- 本地开发环境的组成（Erlang 节点 + PostgreSQL 容器）
- `IMBOYENV` 环境切换机制
- 两个本地配置文件的职责（`.env` 与 `config/sys.local.config`）

**前置条件**（逐项确认）：

- [ ] [Docker Desktop](https://www.docker.com/products/docker-desktop/) 已安装且在运行
- [ ] Erlang/OTP **28 或更高**（`erl -version` 查看；macOS 用 `brew install erlang`）
- [ ] GNU Make（macOS/Linux 自带）
- [ ] 约 2 GB 磁盘空间（PostgreSQL 18 镜像 + 编译产物）

---

## 第 1 步：克隆代码并进入仓库

```bash
git clone <imboy 仓库地址> && cd imboy
```

> **说明**：后续所有命令都在 `imboy/` 仓库根目录执行。

## 第 2 步：一键初始化开发环境

初始化脚本会做四件事：检查依赖 → 生成 `.env`（含随机数据库密码）→ 用 Docker 启动 PostgreSQL 18 → 生成 `config/sys.local.config`。

```bash
bash scripts/dev_setup.sh
```

预期输出（结尾部分）：

```text
==> 完成！下一步 / Done! Next steps:
  1. 核对 config/sys.local.config 中的数据库连接（密码须与 .env 一致）
  2. IMBOYENV=local make run        # 启动后端（迁移自动执行）
  3. bash scripts/seed_demo.sh      # （可选）演示数据
  4. make eunit                     # 单元测试
```

> **常见坑**：
> - `✗ 缺少 erlang（需要 OTP 28+）` → 先装 Erlang，或你装的版本低于 28，需要升级。
> - `PG 60 秒未就绪` → 执行 `docker logs imboy_pg18` 看容器日志；最常见原因是宿主端口 **4323** 被占用（改 `.env` 里的 `IMBOY_PG_HOST_PORT` 后重跑本脚本）。

## 第 3 步：同步数据库密码（最容易踩的坑，别跳过）

脚本生成了两个文件，但**密码不会自动同步**——这是新人启动失败的第一大原因：

| 文件 | 作用 | 密码字段 |
|------|------|---------|
| `.env` | Docker Compose 创建 PG 容器时用的密码 | `IMBOY_PG_PASSWORD`（脚本已自动填入随机值） |
| `config/sys.local.config` | Erlang 应用连接 PG 时用的密码 | `password => "CHANGE_ME_PG_PASSWORD"`（**占位符，需手改**） |

先查看脚本生成的随机密码：

```bash
grep IMBOY_PG_PASSWORD .env
```

预期输出：

```text
IMBOY_PG_PASSWORD=a1b2c3d4e5f6...（一串 32 位十六进制）
```

然后打开 `config/sys.local.config`，把**两处** `"CHANGE_ME_PG_PASSWORD"` 都替换成这串密码（约在第 101 行和第 117 行，分别对应主库配置和迁移配置）。

> **提示**：`.env` 和 `sys.local.config` 都不提交 Git，放心存放本地密码。

## 第 4 步：编译并启动

```bash
make compile
IMBOYENV=local make run
```

首次编译需要拉取依赖，耗时 2-5 分钟属正常。启动后你会进入 Erlang shell，看到类似日志：

```text
==> Booting imboy (env: local) ...
[info] http listener started on port 9800
[info] database migrations applied (XX migrations)
```

> **`IMBOYENV=local` 是什么**：它告诉 Makefile 加载 `relxlocal.config` + `config/sys.local.config`。不带它则按 dev 环境构建。本地开发始终加 `IMBOYENV=local`。

**保持这个终端窗口开着**——后端节点在前台运行。

## 第 5 步：验证服务活着

**新开一个终端**，请求初始化接口：

```bash
curl http://127.0.0.1:9800/api/v1/init
```

预期输出：一个 JSON 响应，包含服务器版本与功能开关信息（字段随版本变化，HTTP 200 即成功）。

```json
{"code": 0, "data": { ... }}
```

看到这个，你的本地后端已经跑通了。

> **常见坑**：
> - `curl: (7) Failed to connect` → 回到第 4 步的终端看 Erlang 日志；多半是密码没同步（回第 3 步）或端口被改。
> - 想让手机连本机后端做联调：把 `config/sys.local.config` 里的 `127.0.0.1` 相关项改成你电脑的局域网 IP，重启节点。

## 第 6 步（可选）：填充演示数据

不想面对一个空数据库？注入演示用户和会话：

```bash
bash scripts/seed_demo.sh
```

## 第 7 步（可选）：跑单元测试确认环境健康

```bash
make eunit-local
```

`eunit-local` 会用你的本地 PostgreSQL 跑测试套件。全绿说明环境完全就绪。

---

## 你建成了什么

一个完整的前台运行 Erlang 节点：

```text
你的终端 ──► Erlang 节点（:9800 HTTP / :9806 管理） ──► PostgreSQL 容器（宿主端口 4323）
```

- 数据库迁移在启动时自动执行，无需手动跑迁移
- 停止节点：在 Erlang shell 里按 `Ctrl+C` 两次，或执行 `q().`
- PG 容器停启：`docker stop imboy_pg18` / `docker start imboy_pg18`

## 下一步

- [运维脚本与命令总览](../../scripts/)（backup / smoke / ctl 等，见 scripts 目录）
- [参考：工程约定 CONVENTIONS](../CONVENTIONS.md)（TSID、分层边界等不可妥协规则）
- [解释：四层架构设计](../explanation/README.md)（ADR-0001，待迁移补全）
- [教程：私有化部署一套生产环境](./quickstart-deploy.md)（待补）
