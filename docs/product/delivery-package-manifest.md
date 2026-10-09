# 一期交付包清单（Delivery Package Manifest）

> 状态：CURRENT ｜ 版本：v1.2 ｜ 最后更新：2026-10-09
> 适用：一期交付（`phase1_delivery` profile，能力面与交付锁见 [packaging-contract.md](./packaging-contract.md) §9）
> 本文定义产品交付内容；任务计划、执行记录和验收证据保存在仓库外。

---

## 1. 打包原则（不可违反）

1. **由 tracked 白名单打包，不是复制工作区**：只从三仓 `git ls-files` 的白名单子集生成交付包；未跟踪文件、ignored 文件、本地缓存一律不进包。
2. **不含**：真实配置、token、PII、crash dump、临时 evidence、缓存、非交付媒体。
3. **新机器可上手**：干净目录依照 README 能找到**配置、启动、验收、恢复**四个入口。
4. **供应链可证**：许可证与第三方依赖清单随包保留（§5）。
5. 历史证据可追溯：运行材料不入库、不进包；仓外证据记录对应提交 SHA 和产物哈希。
6. 本清单是**类别级**白名单（目录/文件族），不是逐字节快照；具体文件以打包时三仓 tracked 状态为准，打包产物须附 `git ls-files` 快照与 SHA。

## 2. imboy（后端主仓）

**包含**

- 源码/构建：`src/`、`include/`、`priv/`（迁移 + 静态）、`proto/`、`api/`（openapi/asyncapi；**`api/internal/v1/` 整目录**——集成方交付物，自含 README/endpoints/manifest/openapi）、`erlang.mk`、`Makefile`、`relx.config`、`elvis.config`、`erlang_ls.config`、`rebar.lock`、`dialyzer.baseline`、`Dockerfile`、`docker/`
- 配置模板：`config/sys.config.example`、`config/vm.args`、`config/cron.config`、`config/nginx-imboy.conf`、`config/product-feature-manifest.json`、`config/product-feature-manifests/`（含 `phase1_delivery.json`）、`deploy/.env.example`、仓根 `.env.example` / `.env.local.example`
- deploy：`install.sh`、`preflight.sh`、`docker-compose.community.yml` / `.demo.yml` / `.observability.yml` / `.widget.yml` / `.healthcheck.yml` / `docker-compose.secrets.example.yml`、`nginx/`（模板 + init-letsencrypt，**prod-vhosts 除外**，见排除）、`prometheus/` `loki/` `grafana/` `alertmanager/` `promtail/` `uptrace/`、`cron/`、`widget/`、`ops.env.example`；`docker-compose.prod.yml` 与 sales-policy 不在开源仓（商务渠道，保持）
- scripts/：源码维护、构建、运维与自动化回归脚本及配套 fixture；运行日志和生成材料不包含。`agent_hub_*` 为二期域，保留源码不代表一期允许执行。
- docs 白名单：仓根 `README(.en)` / `RELEASES` / `CHANGELOG` / `SECURITY` / `SUPPORT` / `CONTRIBUTING` / `CODE_OF_CONDUCT` / `ROADMAP` / `LICENSE` / `VERSION`；docs 内 `reference/`、`concepts/`、`guides/operations|release|security|migrations`、`api-contracts/`（一期域）、`legal/`（含 third-party-licenses*.md）、`glossary.md`、`README.md`、`CONVENTIONS.md`、`adr/`、`tutorials/`、`explanation/`、`architecture/`（现行篇）、`compliance/`（产品必需清单）、`product/`、`ops/`、`deliver/`、`runbooks/bot-webhook-aead-key.md`（一期保留；agent-hub 两篇排除）。`deploy/README.md` 随部署入口交付。
- 测试：`test/`（自动化回归源码；不包含执行报告、运行日志和生成材料）

**排除**

- 任务计划与执行材料目录：`docs/qa/`、`docs/design/`、`docs/plans/`、`docs/archive/design/`；这些材料在仓外管理，不属于产品操作文档。
- 任意层级的 `evidence/`、测试结果、审查报告、运行日志和一次性生成材料；可追溯性通过仓外证据记录保证。
- `docs-site/`（官网 CI）、`.github/`、`.githooks/`、`.contract/`、`.ecc/`、`.mcp.json`、`.qa/`、`.gitcode`、`.elp.toml`、`.gradualizer-baseline.txt`（可选）、`lefthook.yml`、`sonar-project.properties`、`index.html`
- 任意层级 `AGENTS.md`、`CLAUDE.md`：AI 工作区上下文，依赖仓库与聚合工作区上下文，不属于客户安装与操作文档；源码仓保留，客户包排除。
- `deploy/nginx/prod-vhosts/`：生产环境快照默认排除；可交付的 vhost 配置必须以经过审查的脱敏模板提供。

## 3. imboyadmin（管理后台仓）

**包含**：`src/`、`seat/`、`widget/`、`vendor/`、`public/`、`docker/`、`Dockerfile`、`Dockerfile.widget`、`package.json`、`bun.lock`、`vite.config.ts`、`tsconfig.*`、`eslint.config.js`、`bunfig.toml`、`playwright*.config.ts`（seat-embed/real/p2/hosted）、`scripts/`（widget 相关）；docs 白名单：`README` / `RUNBOOK(.en)` / `DESIGN` / `CHANGELOG` / `CONTRIBUTING(.en)` / `LICENSE` / `docs/env-vars(.en)` / `docs/module_map.md` / `docs/api-contracts/`（4 篇）/ `docs/adr/`（2 篇）；`tests/e2e/`（一期相关）

**排除**：`.claude/`、`docs/qa/`、`docs/plans/`（含 evidence/）、`tests/auto_test/`（页面操作脚本型资产，非交付文档，可选）、`sonar-project.properties`、`.gitcode`

## 4. imboyapp（Flutter 客户端仓）

**包含**：`lib/`、`assets/`、`android/`、`ios/`、`macos/`、`plugin/`（含 r_upgrade 保护区，原样）、`integration_test/`、`test/`、`test_driver/`、`tool/`、`scripts/`、`config/`、`pubspec.yaml`、`pubspec.lock`、`slang.yaml`、`build.yaml`、`analysis_options.yaml`、`flutter_*.yaml`、`brand.yaml`、`LICENSE`、`NOTICE.md`、`README` / `DESIGN` / `SECURITY` / `CONTRIBUTING`；docs 白名单：`docs/README.md`、`docs/reference/`、`docs/codemaps/`、`docs/module-map.md`、`docs/build-and-release.md`、`docs/FAQ.md`、`docs/privacy-policy.md`、`docs/data-safety-declaration.md`、`docs/about.md`、`docs/changelog.md`、`docs/adr/`、`docs/third-party-config-guide.md`

**排除**：任务计划、审查和测试执行报告、页面执行台账、一次性测试材料、`coverage/`、运行日志和缓存；一期平台矩阵不含 web 时排除 `web/`。所有仓库的 AI 上下文文件均按 §2 的统一规则排除。

## 5. 许可证与依赖清单（存在性矩阵）

| 仓 | LICENSE | 第三方清单 | lockfile |
|---|---|---|---|
| imboy | BSL 1.1（+ `docs/legal/MulanPSL-2.0.txt` 历史文本） | `docs/legal/third-party-licenses.generated.md` + `third-party-licenses.md` | `rebar.lock`（erlang.mk 体系，无 mix.lock） |
| imboyadmin | BSL 1.1 | 交付前必须提供第三方许可证清单；缺少时不得通过供应链验收 | `bun.lock` |
| imboyapp | 木兰宽松许可证 v2（MulanPSL-2.0） | `NOTICE.md` | `pubspec.lock` |

**工作区层排除**（不在任何仓内，打包天然不含，仅流程申明）：`imboy-commercial-hub/`（商业敏感）、`.Codex/`（AI 工具）、`www.imboy.pub/`（官网 draft）、`imboy.wiki/`（wiki 推送产物）。

## 6. 交付检查

- 后端 README 的单个品牌标识 `docs/brand/logo/imboy_logo_2474E5_0512.png` 随入口交付，其余品牌媒体不纳入此例外。可选 Uptrace 的 `deploy/docker-compose.uptrace.yml` 与 `deploy/uptrace/` 一起交付，保持安装脚本引用完整；不因此纳入 RTC 编排。
- 后端正向类别选择由 `scripts/deliver/backend_allowlist.regex` 实现；新增 tracked 类别默认不交付，不能用排除列表代替白名单。
- 后端源包默认要求已提交 `phase1_delivery` manifest；显式指定其它 profile 仅生成对应源包，不取得一期资格。后端单仓包不能代替三仓装配与供应链验收。
- 三仓冻结提交、一期 profile、配置模板及产物哈希有明确对应关系。
- 干净目录可通过 README 找到配置、启动、验收和恢复入口。
- 各仓许可证、依赖锁文件和第三方许可证清单齐全。
- 生产环境快照默认排除；允许交付的配置必须经过脱敏并以模板提供。
- 交付包使用固定提交生成；本地未提交修改和生成材料不得混入。
- 验收结果记录在仓外，不能引用历史状态或仅以文件存在宣称交付通过。

## 7. 变更规则

- 白名单增删须由交付 owner 审核并更新本文（带版本号与日期），不允许在打包脚本里临时加减。
- 与 `packaging-contract.md`（构建 profile 契约）分工：本文管**交付包含哪些文件**，契约管**产物内哪些 feature 被编译/开启**。
