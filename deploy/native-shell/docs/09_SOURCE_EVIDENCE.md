# 固定源码与部署合同证据

历史读取日期：2026-11-05（北京时间）。初始材料读取时 `main` 为 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，后端 `VERSION=1.0.0-alpha.83`。以下链接均固定到该提交，不引用会移动的默认分支作为发行身份。

## 1. 核对结果与安装器决定

| 主题 | 核对到的事实 | 本包如何处理 | 直接来源 |
| --- | --- | --- | --- |
| 自包含发布 | relx `dev_mode=false`、`include_erts=true`、扩展启动脚本开启 | 客户取得含 ERTS 的原生 release，目标机不另编译 Erlang | [relx.config](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config) |
| 配置准备 | Makefile 将选定配置复制为 `config/sys.runtime.config` | 发布方只从固定源码无秘密模板构建；运行时配置在 `/etc/imboy` 单独生成 | [Makefile](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/Makefile) |
| HTTP 端口 | 启动时读取 `HTTP_PORT` | 使用 `HTTP_PORT=9800`；不使用无读取方的 `IMBOY_HTTP_PORT` | [imboy_app.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| WebSocket | 实际路由 `/api/v1/ws` | 公网地址固定为 `wss://API_DOMAIN/api/v1/ws` | [imboy_router.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_router.erl) |
| 监听地址 | 由 `listen_ip` 配置决定 | 后端只监听回环，外部访问经 Nginx | [imboy_app.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| 严格秘密校验 | pro 环境需要 JWT、PG AES、管理员 Cookie 等正确格式且关键值独立 | 现场分别生成，保持原值续跑/升级；丢失后拒绝重新生成替代数据密钥 | [imboy_env.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl)、[sys.config.example](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/config/sys.config.example) |
| S3 与公开读 | Garage 内网 S3 API、公网 S3 endpoint、公开桶 Website 是不同角色 | 签名 API 用 `https://s3.example.com`；公开 Website 用独立 `https://files.example.com`；保留签名 Host、URI、查询串 | [Garage 原生脚本](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/garage-install.sh)、[环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl) |
| Garage 固定根 | 严格启动要求附件 registry；可从 Garage 配置派生 | 设置 `IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE=true`，首次写入后不擅改根的桶和端点 | [启动校验](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| PostgreSQL 依赖 | 原生 CI 的同版本迁移准备包含12项扩展 | 随包提供完整 PG prefix，检查扩展/动态库/词典，不能替换成裸 PostgreSQL | [native-eunit-pins.json](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-pins.json)、[native-eunit-run.sh](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-run.sh) |
| 自动迁移 | strict 迁移失败/dirty 会抛异常，在 HTTP 监听前阻止启动 | 停写期间执行；不伪造成功标记；失败保留现场 | [imboy_migrate.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_migrate.erl)、[应用启动](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl) |
| 健康端点 | `/healthz` 检查 PG 与 TSID readiness，不检查 S3 或真实业务 | 同时检查版本、DB、TSID；对象读写和业务 UAT 分开记录 | [healthz_handler.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/api/healthz_handler.erl) |
| 首次管理员 | 本机 setup 接口使用 RSA-OAEP，传入客户端 MD5 后的口令；事务内防重复 | 隐藏读入口令→MD5文本→OAEP/SHA256；仅回环调用，公网禁止 setup 路由 | [adm_setup_handler.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/adm/adm_setup_handler.erl)、[adm_setup_logic.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/logic/adm_setup_logic.erl)、[elib_cipher.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/elib_cipher.erl) |
| 产品范围 | 当前 `phase1_delivery` 清单及运行时交付锁限定功能 | 不安装 RTC/AI 额外服务，不把关闭的入口当成交付能力，不把常驻模块误称为完全裁剪 | [product-feature-manifest.json](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/config/product-feature-manifest.json)、[imboy_profile_preset.erl](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_profile_preset.erl) |

12项扩展：`pgcrypto`、`pg_jieba`、`timescaledb`、`vector`、`postgis`、`pg_trgm`、`btree_gin`、`btree_gist`、`citext`、`unaccent`、`intarray`、`pg_stat_statements`。

## 2. 原生启动脚本的额外合同

IMBoy 固定依赖是 relx 4.10.0。本包核对了该版本的[扩展启动模板](https://github.com/erlware/relx/blob/v4.10.0/priv/templates/extended_bin)：VM 参数文件由 `VMARGS_PATH` 指定；`RELX_REPLACE_OS_VARS` 使用非空判断，所以写 `false` 仍会启用替换。本包将其设为空并使用独立配置；不使用无效的 `RELX_VM_ARGS` 或 `RELX_READ_ONLY`。

`start_erl.data` 可以没有结尾换行。本包配置读取处理了这种 EOF，不把已读到的有效版本误判为失败。配置渲染器绑定原始模板 SHA-256：

```text
cf111ef0af8d713fdd6c80e4fcfbfcaf01fd3161b048eb31dc1a1bb5a2da4616
```

新 release 的上游配置发生变化时，发布方必须适配和重新验证渲染器，不能仅换版本号就继续安装。升级先验证目标候选，再执行目标候选自己的升级入口、渲染器和 systemd 配置。

## 3. 初始材料与本次集成的验证边界

2026-11-05 初始材料整理时访问 [GitHub Releases API](https://api.github.com/repos/imboy-pub/imboy/releases?per_page=3) 返回空数组，因此没有把一个猜测的二进制下载地址写成已供应资产。公开构建文件与原生 CI 配方只用于确认构建合同；真实编译产物、完整安装与业务结果仍需发布方生成。

主仓固定 SHA 不会自动冻结所有外部依赖；部分依赖仍指向移动分支。组装器记录实际依赖提交并校验最终产物，不能据此宣称构建天然逐字节可复现。

本包备份采用停止数据库后的完整文件系统快照，并同时停止后端、入口和对象存储以保持同一写入窗口。PostgreSQL 对普通文件级备份的完整集群与停机要求见[官方文档](https://www.postgresql.org/docs/18/backup-file.html)。这一设计仍需要真实恢复演练证明可用，哈希正确不等于业务已恢复。

2026-11-05 的初始交付只形成材料包。2026-11-08 的仓库集成将材料纳入版本管理和独立合成测试工作流，没有因此完成真实构建、部署或业务验收，也没有完成 main 的 alpha.84／迁移 275 适配。权威验证范围与可重复命令见 [10_VALIDATION.md](10_VALIDATION.md)；当前执行状态由对应 PR/CI 报告。
