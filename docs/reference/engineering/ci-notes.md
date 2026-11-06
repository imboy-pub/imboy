# CI 流程笔记（CI Notes）

> 工程视角 · 描述现状 + 增量改进 · 与 `docs/guides/testing/ci-testing.md` 互补(那份偏测试编排,本份偏工程门禁现状)

## 现状

**后端**(`imboy/.github/workflows/`,12 个):`backend-ci.yml`(Compile Gate + Moment EUnit Gate + Full EUnit baseline + 零加密检查 + 重复模块检查 + DCO)、`contract-gate.yml`(API/枚举/EntityId 契约门)、`quality.yml`、`sbom-diff.yml`、`codemap.yml`、`docs.yml`、`release.yml`、`nightly.yml`、`native-eunit-acceptance.yml`、`mirror.yml`、`widget-pairing.yml`、`dependabot-auto-merge.yml`。

**Flutter**(`imboyapp/.github/workflows/`):`ci.yml`(analyze + 模块边界 + new code guard + unit/widget + Moment regression gate + 独立 integration job)、`contract.yml`(错误码契约门)、`integration_test.yml`(评审记录路径坏死)、`quality.yml`、`mirror.yml`、`dependabot-auto-merge.yml`。

**Admin**:`ci.yml`(lint + bun test + Channel/Moment regression gate)、`quality.yml`、`admin-e2e.yml`、`widget-pairing.yml`、`mirror.yml`、`dependabot-auto-merge.yml`。Playwright E2E 由 `admin-e2e.yml` 在 CI 覆盖。

**本地钩子**(三仓 `lefthook.yml`):后端 pre-commit erlfmt --check + gitleaks protect,commit-msg Conventional Commits;前端类似。

## 优点

- **ratchet 框架已成型**:`backend-ci.yml` 注释明确"全量 eunit 先建失败基线,基线明确后改 ratchet(失败数只减不增)、移除 continue-on-error"——收紧路线自带计划。
- 关键回归有针对性硬门(Moment/Channel regression gate 三仓一致)。
- 安全门到位:零加密检查(E2EE 不变量)、gitleaks、重复模块检查、SBOM diff、DCO。
- 本地钩子 + CI 双层,格式/规范在提交即拦。
- matrix(OTP 版本)+ 缓存(deps/PLT),CI 效率有优化。

## 潜在改进

1. **软门收紧为 ratchet 硬门**(优先级高,增量,已有计划):Full EUnit 与 dialyzer 当前 continue-on-error;按 `backend-ci.yml` 自带计划,清存量红项后翻硬门。这是工程效能最高杠杆。
2. **覆盖率门**(高):三仓仅 `test -f lcov.info` 存在性检查,补真实阈值门(见 `docs/guides/testing/coverage-plan.md`)。
3. ~~**admin E2E 进 CI**(高)~~ 已完成:`admin-e2e.yml` 已在 CI 覆盖 Playwright E2E。
4. **排查坏死工作流**(中):`imboyapp/integration_test.yml` 的触发配置(paths/working-directory)在 umbrella 工作区下失效,可能从未运行;注意 `ci.yml` 另有在跑的 integration job,该 yml 可能是冗余坏死副本。
5. **契约门禁**(高):proto/OpenAPI/ws_url diff 进 CI,拦协议漂移(roadmap ARCH-03)。
6. **"有约定无 lint"收口**(中):autoDispose/裸URL/800行/token 等约定补 custom_lint(roadmap ENG-01),让 CI 强制而非靠自觉。

## 相关模块

`imboy/.github/workflows/backend-ci.yml`、`imboyapp/.github/workflows/{ci,integration_test}.yml`、`imboyadmin/.github/workflows/ci.yml`、三仓 `lefthook.yml`、`imboy/scripts/check_*.sh`

## 优先级

| 建议 | 优先级 |
|---|---|
| Full EUnit/dialyzer 收紧 ratchet 硬门 | 高 |
| 覆盖率阈值门 | 高 |
| 契约 diff 门(proto/OpenAPI/ws_url) | 高 |
| custom_lint 收口"有约定无机制" | 中 |
| 修坏死 integration_test.yml | 中 |
