# Make 命令参考 / Make Command Reference

> **Last Updated:** 2026-09-30
> **真源 / Source of Truth:** `Makefile`、`include/cli.mk`、`include/tpl.mk`、`erlang.mk`（vendored，勿改）
> 部署与运维命令不在本文范围，见 `deploy/README.md`。

## 环境选择（IMBOYENV）

`IMBOYENV` 决定构建与运行加载哪套配置（`Makefile` 头部逻辑）：

| IMBOYENV | relx 配置 | 运行时配置 |
|----------|-----------|------------|
| `local` | `relxlocal.config` | `config/sys.local.config` |
| `dev` | `relxdev.config` | `config/sys.dev.config` |
| `pro` | `relxpro.config` | `config/sys.pro.config` |
| （未设置） | `relx.config` | `config/sys.config`（不存在时回退 `sys.config.example`） |

`IMBOY_*` 环境变量在**运行时**覆盖配置值（`src/lib/imboy_env.erl`），优先级最高。环境变量参考见 [env-vars.md](./env-vars.md)。

<!-- AUTO-GENERATED:make-targets BEGIN（由 Makefile / include/*.mk 提取；手工内容请写在本区间外） -->

## 构建与运行

| 命令 | 作用 |
|------|------|
| `make compile` | 编译（erlang.mk 无原生 `compile`，此处为 `app` 的别名） |
| `IMBOYENV=local make run` | 本地启动（relx console，自动加载 `config/sys.local.config`） |
| `IMBOYENV=local make run HTTP_PORT=9800` | 指定 HTTP 端口启动 |
| `IMBOYENV=pro make rel` | 构建生产 release（`scripts/imboy-deploy.sh` 使用该口径） |
| `make clear_beam` | 删除全部 `.beam`（deps 除外） |
| `make clean-beam` | 递归删除全部 `.beam`（`include/cli.mk`，比 `clear_beam` 更彻底） |
| `make help` | erlang.mk 内建帮助 |

## 测试

| 命令 | 作用 |
|------|------|
| `IMBOYENV=local make eunit` | 全量 EUnit（`IMBOYENV=local` 下自动委派 `eunit-local`） |
| `IMBOYENV=local make eunit t=<模块名>` | 单模块 EUnit（含 DB 用例的模块走 `eunit-local` 路径，自动注入 `-config`） |
| `make eunit-local` | 显式入口：`-config` 注入 + PG 中继 + 私有 HTTP 端口；`EUNIT_CONFIG` 可覆盖（默认 `config/sys.local`） |
| `make e2ee-verify` | E2EE 全套验证（安全门禁 + 专项 EUnit 套件） |
| `make ct` | Common Test（`CT_CONFIG ?= config/sys.config`，`TEST_HTTP_PORT ?= 0`） |
| `make rest-api-test` | REST API 黑盒测试（runner 自持隔离 scratch DB，容器 `imboy_pg18` 127.0.0.1:4323；`REST_PG_PORT` 可覆盖） |
| `make rest-contract-check` | REST 契约覆盖检查（`scripts/check_rest_contract_coverage.sh`） |

> 单模块跑 DB 注入用例须给 EUnit VM 传 `EUNIT_ERL_OPTS`（含 `-config`），`make eunit-local t=<模块>` 已封装该口径，无需手工拼参。

## 代码质量与门禁

| 命令 | 作用 |
|------|------|
| `make lint-erlang` | Elvis 静态检查（`elvis rock`） |
| `make format` | erlfmt 写入格式化（`src/**/*.erl`、`include/*.hrl` 等） |
| `make format-check` | erlfmt 只检查（pre-commit 由 lefthook 强制） |
| `make dialyze` | Dialyzer 全量分析（存量告警 exit 2） |
| `make dialyze-check` | Dialyzer 递减基线门：对照 `dialyzer.baseline`，基线外新增 1 条即红，只准减不准增 |
| `make dialyze-local` | `dialyze-check` 的 L3 gate 别名 |
| `make xref-strict` | xref 严格模式 |
| `make security-gate` | 三重门禁：服务端零密码学守护 + 模块边界（Handler→Logic→DS→Repo）+ 纵切架构九铁律 |
| `make arch-check` | Feature Slice 九铁律（ADR-0007）；`arch-check-self-test` 为门禁自检 |
| `make migrations-check` | 迁移文件门禁：命名格式 / up-down 成对 / 版本号唯一（ADR-0002） |
| `make cron-check` | ecron 定时作业配置校验（发布前自查加 `--strict` 口径见脚本） |
| `make moya-ai-check` | 墨芽 AI 回课配置前置校验 |
| `make moya-ai-key-check` | 墨芽 AI 密钥真实可用性体检（独立临时节点 + 纯文本 ping；`--no-ping` 离线） |
| `make terminology-check` | 术语表校验（`priv/terminology/*.json`） |

## 数据库 / 跨仓专项门禁

| 命令 | 作用 |
|------|------|
| `make cs-migration-gate PGDATABASE=...` | 客服域迁移是否已全部落库（连真库） |
| `make pg-harness-check PGDATABASE=... PGPORT=...` | 三支真 PostgreSQL 行为 harness |
| `make widget-asset-pairing-check` | CS widget 资产名后端↔admin 配对一致（`ADMIN_REPO_DIR=...`） |
| `make feature-cross-repo-check` | 三仓特性产物与 `config/product-feature-manifest.json` 一致（`APP_DIR`/`ADMIN_DIR`） |

## API 契约

| 命令 | 作用 |
|------|------|
| `make contract-export` | 导出契约产物 `.contract/api_contract.json`（确定性输出） |
| `make contract-check` | 契约校验：落仓产物 vs 真源 + admin/flutter 枚举 diff + EntityId 规则（`ADMIN_DIR`/`FLUTTER_DIR` 可覆盖） |
| `make contract-regen` | 一行重生成全部契约物（= contract-export + imboyapp error_code.dart；`IMBOYAPP_DIR ?= ../imboyapp`） |

## 冒烟与金安装门禁

| 命令 | 作用 |
|------|------|
| `make smoke` | Tier-0 冒烟全集（= smoke-c2c + smoke-ws + smoke-ctl；`SMOKE_FROM`/`SMOKE_TO` 指定 uid 段） |
| `make smoke-c2c` / `make smoke-ws` / `make smoke-ctl` | 单项冒烟 |
| `make smoke-8step` | 8 步应用层冒烟链（Golden Gates §4.3） |
| `make feature-smoke FEATURE_SMOKE_BASE_URL=...` | 特性开关冒烟（必填 `FEATURE_SMOKE_BASE_URL`；`FEATURE_SMOKE_EXPECTS='core=true moment=false'` 等） |
| `make golden-install GOLDEN_ARGS="..."` | Golden Install 金安装门禁（cleanroom 全流程 + 计时 + restart 幂等） |
| `make golden-upgrade GOLDEN_ARGS="..."` | Golden Upgrade 升级门禁（vN → vN+1 + 数据保留断言） |

## 静态类型检查

| 命令 | 作用 |
|------|------|
| `make gradualizer-setup` | 拉取并构建 Gradualizer escript（pin 版本，首次必跑） |
| `make gradualize FILE=src/lib/elib_cnv.erl` | 单文件 Gradualizer 快检 |
| `make gradualize-layer LAYER=lib` | 分层检查（`LAYER=lib\|repo\|ds\|logic\|api`） |
| `make gradualize-audit` | 全仓逐模块审计（预算制 `GRADUALIZE_BUDGET`） |
| `make gradualize-baseline` | 从最近 audit 日志重建 pre-push 棘轮基线（只准减不准增） |
| `make elp-setup` | 校验 elp + JVM，拉取 eqwalizer_support（首次必跑） |
| `make eqwalize MOD=<模块>` | 单模块 eqWAlizer 检查 |
| `make eqwalize-layer LAYER=lib` | 分层检查（预算制 `EQWALIZE_BUDGET`） |
| `make eqwalize-all` | 全量检查（CI 用） |

## 节点控制 / 代码生成 / 依赖 / 文档

| 命令 | 作用 |
|------|------|
| `make ctl ARGS="node status"` | 节点 CLI（`CTL_NODE ?= imboy@127.0.0.1`；亦支持 `smoke all`、`db ping`、`plugin list` 等） |
| `make new t=imboy.rest_handler n=demo_handler` | 代码生成模板（`t=` 可选 `imboy.rest_handler` / `imboy.logic` / `imboy.repository` / `imboy.ds`，模板定义在 `include/tpl.mk`） |
| `make mod-check` | 审计 `include/deps.mk` 全部依赖的上游最新版本 |
| `make mod-up MOD="cowboy cowlib=2.17.0"` | 定点升级依赖（`MOD=all` 升全部；升级后需清 `deps/<name>` 与 `.erlang.mk/dep_built/<name>` 再 make） |
| `make docs-serve` / `make docs-stop` | 启停 API 文档服务器（Docker，http://localhost:8080） |

## erlang.mk 内建目标（vendored，勿改）

`all`、`app`、`deps`、`eunit`（原生）、`ct`、`dialyze`、`xref`、`edoc`、`rel`、`run`、`clean`、`distclean`、`new`（模板系统，`tpl_<t>` 有定义时优先用项目模板）、`help`。

## Legacy 目标（include/cli.mk，谨慎使用）

| 命令 | 状态 |
|------|------|
| `make efmt` | 旧格式化入口（`./efmt -w src/*.erl`），已被 `make format`（erlfmt）取代 |
| `make clean-beam` | 可用（见上表） |
| ~~`make gen-appup`~~ | 已删除目标（2026-09-30）：全仓无 `gen_appup.sh` 脚本可引；将来需要 appup 生成时须先补脚本再恢复目标 |
| `make start` / `make stop` | 启动/停止单个节点，走 `scripts/start_node.sh` / `scripts/stop_node.sh`（2026-09-30 修复 `script/` 少 s 笔误） |

<!-- AUTO-GENERATED:make-targets END -->

## 相关文档

- 环境变量与配置：[env-vars.md](./env-vars.md)
- 工程笔记（CI/依赖/发布现状）：[engineering/engineering-overview.md](./engineering/engineering-overview.md)
- 静态类型检查选型与落地：[static-typechecking/](./static-typechecking/)
