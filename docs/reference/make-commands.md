# Make 命令参考 / Make Command Reference

> **Last Updated:** 2026-11-05
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

环境专用文件缺失时分别回退 `relx.config`、`config/sys.config`（后者缺失再用 example）。仅发布/运行目标通过 `runtime-config` 生成 `config/sys.runtime.config`；`help`、`mod-check` 和纯编译不复制运行配置，也不读取 LLM dotenv。发布/运行缺失或无效的 relx 配置会明确失败。

Environment-specific files fall back independently. Release/run targets explicitly prepare runtime configuration and reject missing/invalid release configuration; audit/help/compilation do not prepare it or load LLM dotenv files.

本仓 vendored erlang.mk 为 `efb0d04`。该版本解析期立即求 relx 元数据，Makefile 在 include 期间阻止 Erlang 命令执行，执行期恢复原生命令格式并按需缓存元数据。不要删除这一兼容屏障或以审计成功推断发布配置有效。`run` 的三个 LLM 变量优先级为：Make 命令行 > `.env.local`（首个定义非空时） > `.env`（同前） > 继承环境；保留 `$`、`#`、空格，不执行 dotenv 内容。

The pinned erlang.mk eagerly evaluates release metadata. The project defers Erlang execution until parsing finishes and caches metadata on demand. LLM values are loaded only for the run process tree, preserving literal characters and existing precedence.

裸 `make` 保留原有默认目标 `test-build`，并不等于官方原生默认 `all`。测试批量编译需要 GNU Make 4+；macOS 自带 Make 3.81 可执行帮助/审计，编译测试请使用 `gmake`。隔离构建回归：`python3 scripts/test/test_makefile.py`，仅使用临时假配置和纯函数测试，不连接数据库。

Bare make retains the existing test-build default. Test compilation requires GNU Make 4+ (gmake on macOS). The build regression uses temporary configurations and pure tests without a database.

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
| `IMBOYENV=local make eunit` | 委派 `quality-full`，由构建收据决定复用/重建，不重复运行原生 EUnit |
| `IMBOYENV=local make eunit t=<模块名>` | 原生定向入口；支持 `t=模块:函数`，需要 DB 时使用 `eunit-local t=...` 或显式注入配置 |
| `IMBOYENV=local make eunit-local` | 无显式测试参数时走 `quality-full`；显式 `t` / `EUNIT_CONFIG` / `EUNIT_MODS` / `EUNIT_USE_RELAY`（含空值）走 legacy |
| `make eunit-legacy` | 强制 legacy：配置注入 + 可选 PG 中继 + 私有 HTTP 端口；`EUNIT_CONFIG` 默认 `config/sys.local` |
| `make eunit-all` / `make quality-full` | 质量全量入口；保留许可、资源门和完整性判定 |
| `make quality-group g=模块1,模块2` | 质量诊断分组，只产生部分执行结果 |
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
| `make docs-serve` / `make docs-stop` | 启停 API 文档服务器（Docker，`http://localhost:8080`） |

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

非 local 环境的 `eunit` 和 `IMBOY_EUNIT_INNER` 非空的内层调用保留原生路由。全量原生列表保留 `talk_record_owned_eunit_fixture` 包装与独占套件排除；TSID 全局状态、AG31/HIRD 专属 PG、迁移 DDL、附件协议等排除项仍由各自专项门执行。定向 `t=` 不过滤；显式空 `EUNIT_MODS` 不能产生零用例成功。Make 路由与 `imboy_quality_build:entry_route/1` 的 origin 判定保持一致。

Non-local and inner EUnit calls retain native routing. Excluded exclusive suites still belong to their specialized gates; targeted selections are unfiltered, while an empty explicit module selection fails. The Make routing follows the authoritative Erlang origin table.

## 相关文档

- 环境变量与配置：[env-vars.md](./env-vars.md)
- 工程笔记（CI/依赖/发布现状）：[engineering/engineering-overview.md](./engineering/engineering-overview.md)
- 静态类型检查选型与落地：[static-typechecking/](./static-typechecking/)
