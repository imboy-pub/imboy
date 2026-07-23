# 发布流程笔记（Release Notes）

> 工程视角 · 描述现状 + 增量改进 · 补充 `docs/testing/ci-testing.md`(CD 侧)

## 现状

**版本管理**:后端版本双源 `imboy/VERSION`(当前 `1.0.0-alpha.15`)+ `relx.config`;三仓同版本线(admin `package.json` 亦 `1.0.0-alpha.15`)。有 `CHANGELOG.md`。commit 经 lefthook `commit-msg` 强制 Conventional Commits(type 白名单 feat/fix/refactor/docs/test/chore/perf/ci/style/build/revert)。

**构建**:后端 `make rel`(relx 组装,`dev_mode=false` 自包含 ERTS);Flutter `build_play_aab.sh`;Admin `vite build`。三仓 Dockerfile 多阶段。

**CD**(`imboy/deploy/`):`scripts/deploy.sh`(蓝绿,`-l` HOST=生产)、`preflight.sh`(部署前校验)、`certbot`(TLS)。生产 systemd nginx(非 docker)。

**制品**:`sbom-diff.yml`(SBOM 追踪);工作区根有 `releases/` 目录。

## 优点

- 蓝绿部署 + preflight 前置校验,降低发布风险。
- Conventional Commits 强制,历史可机读、可生成 CHANGELOG。
- 多阶段 Docker 自包含,runtime 无需装 Erlang。
- SBOM diff 让发布制品变更可追溯。
- 生产 fail-fast 配置校验(见 configuration-notes)作为发布安全网。

## 潜在改进

1. **版本双源同步机制**(优先级中,增量):`VERSION` 与 `relx.config` 需都 bump(评审记录 alpha.14 踩坑,漏一个致迁移 dirty/版本不一致)。建议单一真源生成或 CI 校验二者一致(类比 Flutter DDL 单一真源思路)。
2. **发布后自动冒烟 + 回滚**(中):蓝绿切换后自动跑 `smoke`(c2c/ws/ctl)+ admin `prod-health-check`,失败自动回滚(见 ci-testing CD 段)。当前冒烟脚本存在但发布链自动联动待确认。
3. **三仓版本对齐校验**(低):三仓同版本线,建议发布门校验版本一致,避免 app/后端/admin 版本漂移。
4. **回滚演练常态化**(中):迁移 down + 蓝绿回退定期演练(见 `docs/testing/chaos-testing.md`),确保回滚路径可用。
5. **CHANGELOG 自动化**(低):基于 Conventional Commits 自动生成,减少手工维护漂移。

## 相关模块

`imboy/VERSION`、`imboy/relx.config`、`imboy/scripts/deploy.sh`、`imboy/deploy/preflight.sh`、`imboy/lefthook.yml`、`imboy/CHANGELOG.md`、`imboyapp/scripts/build_play_aab.sh`、`.github/workflows/sbom-diff.yml`

## 优先级

| 建议 | 优先级 |
|---|---|
| 版本双源同步机制 | 中 |
| 发布后自动冒烟 + 回滚联动 | 中 |
| 回滚演练常态化 | 中 |
| 三仓版本对齐校验 | 低 |
| CHANGELOG 自动化 | 低 |
