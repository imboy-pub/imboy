# 一期交付包清单（Delivery Package Manifest）

> 状态：CURRENT ｜ 版本：v1.0 ｜ 最后更新：2026-10-07 ｜ Owner：交付 owner（Q11）
> 适用：一期交付（`phase1_delivery` profile，能力面与交付锁见 [packaging-contract.md](./packaging-contract.md) §9）
> 来源：一期交付质量主计划 Q11 / AC26–AC27（`docs/design/2026-10-07-phase1-delivery-quality-plan.md`）与 Q11 文档盘点设计 §5 草案的正式化。

---

## 1. 打包原则（不可违反）

1. **由 tracked 白名单打包，不是复制工作区**：只从三仓 `git ls-files` 的白名单子集生成交付包；未跟踪文件、ignored 文件、本地缓存一律不进包。
2. **不含**：真实配置、token、PII、crash dump、临时 evidence、缓存、非交付媒体（AC27）。
3. **新机器可上手**：干净目录依照 README 能找到**配置、启动、验收、恢复**四个入口。
4. **供应链可证**：许可证与第三方依赖清单随包保留（§5）。
5. 历史证据可追溯：归档与历史 run 不进包，但 git 历史与 `docs/archive/` 保留出处。
6. 本清单是**类别级**白名单（目录/文件族），不是逐字节快照；具体文件以打包时三仓 tracked 状态为准，打包产物须附 `git ls-files` 快照与 SHA。

## 2. imboy（后端主仓）

**包含**

- 源码/构建：`src/`、`include/`、`priv/`（迁移 + 静态）、`proto/`、`api/`（openapi/asyncapi；**`api/internal/v1/` 整目录**——集成方交付物，自含 README/endpoints/manifest/openapi）、`erlang.mk`、`Makefile`、`relx.config`、`elvis.config`、`erlang_ls.config`、`rebar.lock`、`dialyzer.baseline`、`Dockerfile`、`docker/`
- 配置模板：`config/sys.config.example`、`config/vm.args`、`config/cron.config`、`config/nginx-imboy.conf`、`config/product-feature-manifest.json`、`config/product-feature-manifests/`（含 `phase1_delivery.json`）、`deploy/.env.example`、仓根 `.env.example` / `.env.local.example`
- deploy：`install.sh`、`preflight.sh`、`docker-compose.community.yml` / `.demo.yml` / `.observability.yml` / `.widget.yml` / `.healthcheck.yml` / `docker-compose.secrets.example.yml`、`nginx/`（模板 + init-letsencrypt，**prod-vhosts 除外**，见排除）、`prometheus/` `loki/` `grafana/` `alertmanager/` `promtail/` `uptrace/`、`cron/`、`widget/`、`ops.env.example`；`docker-compose.prod.yml` 与 sales-policy 不在开源仓（商务渠道，保持）
- scripts/：交付相关保留（`backup_pg` / 恢复 / `backup_garage` / `dev_setup.sh` / smoke 系列 / `imboy_ctl`）；agent_hub_* 脚本为二期域，标注二期后保留
- docs 白名单：仓根 `README(.en)` / `RELEASES` / `CHANGELOG` / `SECURITY` / `SUPPORT` / `CONTRIBUTING` / `CODE_OF_CONDUCT` / `ROADMAP` / `LICENSE` / `VERSION`；docs 内 `reference/`、`concepts/`、`guides/operations|release|security|migrations`、`api-contracts/`（一期域）、`legal/`（含 third-party-licenses*.md）、`glossary.md`、`README.md`、`CONVENTIONS.md`、`adr/`、`tutorials/`、`explanation/`、`architecture/`（现行篇）、`compliance/`（交付必需清单）、`product/`、`ops/`、`runbooks/bot-webhook-aead-key.md`（一期保留；agent-hub 两篇排除）、`docs/README.md`
- 测试：`test/`（Q12 按冻结 run 裁定 required 子集或全量）

**排除**

- `docs/qa/`（HISTORICAL）、`docs/design/`（进行中计划与 run；已归档部分在 `docs/archive/design/`，亦不进包）、`docs/plans/`（gitignored 历史悬留）
- 仓根 `evidence/`（历史 run 证据，无导航引用；可追溯性走 git 历史与 Q12 证据台账）
- `docs-site/`（官网 CI）、`.github/`、`.githooks/`、`.contract/`、`.ecc/`、`.mcp.json`、`.qa/`、`.gitcode`、`.elp.toml`、`.gradualizer-baseline.txt`（可选）、`lefthook.yml`、`sonar-project.properties`、`index.html`
- 仓根 `AGENTS.md`、`CLAUDE.md`：AI 工作区上下文，引用父级聚合工作区，不属于客户安装与操作文档；源码仓保留，客户包排除。
- `deploy/nginx/prod-vhosts/`：tracked 的生产 vhost 快照，**含真实域名等环境信息**；存在疑似环境敏感内容待治理，未经 AC27 逐条审查前一律不得进入交付包（审查通过前按排除处理）

## 3. imboyadmin（管理后台仓）

**包含**：`src/`、`seat/`、`widget/`、`vendor/`、`public/`、`docker/`、`Dockerfile`、`Dockerfile.widget`、`package.json`、`bun.lock`、`vite.config.ts`、`tsconfig.*`、`eslint.config.js`、`bunfig.toml`、`playwright*.config.ts`（seat-embed/real/p2/hosted）、`scripts/`（widget 相关）；docs 白名单：`README` / `RUNBOOK(.en)` / `DESIGN` / `CHANGELOG` / `CONTRIBUTING(.en)` / `LICENSE` / `docs/env-vars(.en)` / `docs/module_map.md` / `docs/api-contracts/`（4 篇）/ `docs/adr/`（2 篇）/ `docs/prototype-delta-ledger`；`tests/e2e/`（一期相关）

**排除**：`.claude/`、`docs/qa/`、`docs/plans/`（含 evidence/）、`tests/auto_test/`（页面操作脚本型资产，非交付文档，可选）、`sonar-project.properties`、`.gitcode`

## 4. imboyapp（Flutter 客户端仓）

**包含**：`lib/`、`assets/`、`android/`、`ios/`、`macos/`、`plugin/`（含 r_upgrade 保护区，原样）、`integration_test/`、`test/`、`test_driver/`、`tool/`、`scripts/`、`config/`、`pubspec.yaml`、`pubspec.lock`、`slang.yaml`、`build.yaml`、`analysis_options.yaml`、`flutter_*.yaml`、`brand.yaml`、`LICENSE`、`NOTICE.md`、`README` / `DESIGN` / `CLAUDE` / `SECURITY` / `CONTRIBUTING` / `AGENTS`；docs 白名单：`docs/README.md`、`docs/reference/`、`docs/codemaps/`、`docs/module-map.md`、`docs/build-and-release.md`、`docs/FAQ.md`、`docs/privacy-policy.md`、`docs/data-safety-declaration.md`、`docs/about.md`、`docs/changelog.md`、`docs/adr/`、`docs/third-party-config-guide.md`

**排除**：`docs/qa/`（evidence + 带日期篇；`testing-charter.md` 与 `full-app-test-checklist.md` 两篇稳定篇保留）、仓根 `I18N_*.md` 13 篇与 `overview.md`（一次性 i18n 评审记录，归档候选）、`web/`（一期平台矩阵不含 web 时）、`coverage/`、`.claude-loop-anchor.txt`、`rebar.lock`（Flutter 仓中异物，疑误入——Q10 核实后处置）

## 5. 许可证与依赖清单（存在性矩阵）

| 仓 | LICENSE | 第三方清单 | lockfile |
|---|---|---|---|
| imboy | BSL 1.1（+ `docs/legal/MulanPSL-2.0.txt` 历史文本） | `docs/legal/third-party-licenses.generated.md` + `third-party-licenses.md` | `rebar.lock`（erlang.mk 体系，无 mix.lock） |
| imboyadmin | BSL 1.1 | **缺失**（见 §6 待办 T2） | `bun.lock` |
| imboyapp | 木兰宽松许可证 v2（MulanPSL-2.0） | `NOTICE.md` | `pubspec.lock` |

**工作区层排除**（不在任何仓内，打包天然不含，仅流程申明）：`imboy-commercial-hub/`（商业敏感）、`.Codex/`（AI 工具）、`www.imboy.pub/`（官网 draft）、`imboy.wiki/`（wiki 推送产物）。

## 6. 待办登记（Q11 交接）

| # | 事项 | 责任 | 备注 |
|---|---|---|---|
| T1 | `deploy/nginx/prod-vhosts/` 逐条敏感性审查（AC27）后决定入包/脱敏/排除 | 交付 owner + 安全 owner | 现按排除处理；审查时不得将环境值复制进任何文档 |
| T2 | imboyadmin 第三方许可证清单补齐（`bun pm licenses` 产出，入库其 docs） | imboyadmin owner | 跨仓动作，Q11 未执行 |
| T3 | `imboyapp/rebar.lock` 异物核实与处置 | Q10 / imboyapp owner | 见 §4 排除 |
| T4 | imboyapp `I18N_*.md` 13 篇 + `overview.md`、`docs/qa/`、imboyadmin `docs/qa/`、`docs/plans/evidence/` 的跨仓归档 | 各仓 owner | imboy 仓内对应处置（docs/design 5 目录归档、docs/qa README）已于 2026-10-07 完成 |
| T5 | Q12 冻结验收 run 产出后，回填 `docs/qa/README.md` 与本文验收入口 | Q12 owner | — |

## 7. 变更规则

- 白名单增删须由交付 owner 审核并更新本文（带版本号与日期），不允许在打包脚本里临时加减。
- 与 `packaging-contract.md`（构建 profile 契约）分工：本文管**交付包含哪些文件**，契约管**产物内哪些 feature 被编译/开启**。
