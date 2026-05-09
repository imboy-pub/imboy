# Pull Request — imboy (Erlang Backend)

> 关联：`.claude/plans/quality-loop.md` v1.3 T4.5 / v1.1 §304 三仓各一份

## 摘要 / Summary

<!-- 一句话说明本 PR 解决什么问题 -->

## 改动类型 / Type of Change

- [ ] 🐛 Bug fix（非破坏性）
- [ ] ✨ Feature（新增功能）
- [ ] 💥 Breaking change（API 契约变更，需升 major version）
- [ ] 📝 Docs（文档/codemap 更新）
- [ ] 🔧 Refactor（不改行为）
- [ ] ⚡ Performance
- [ ] ✅ Tests / 🧪 CI

## 自检清单 / Self-Review

### 编码规范

- [ ] 已遵守 [CONVENTIONS.md](docs/CONVENTIONS.md) 6 条不可妥协规则（ID/时间/字段命名/错误响应/分页/命名）
- [ ] UTF-8 字符串带 `/utf8` 后缀（如 `<<"操作成功"/utf8>>`）
- [ ] SQL 全部 `elib_pg` 参数化（防注入）
- [ ] 无新 `behavior` 写法（统一英式 `behaviour`）
- [ ] 无新 `if` 表达式（用 `case`）

### 质量门（自动跑，但请提前自查）

- [ ] `make elvis` 通过 / 无新增违规（ratchet 上限 8824）
- [ ] `make dialyze` 通过 / 无新警告
- [ ] `make xref-strict` 通过
- [ ] `make eunit` 全绿
- [ ] `make format-check` 通过

### 契约变更（如适用）

- [ ] 改动了 `api/openapi.yaml` → PR 描述中说明影响 + oasdiff PR check 已 review
- [ ] 改动了 `api/proto/imboy.proto`（实际 src/imboy.proto） → 已跑 `make compile` + 通知 imboyapp/admin codegen 同步
- [ ] **Breaking change** → 已升 major version + 在 description 中详细说明客户端迁移路径

### 文档

- [ ] 改动 src 后超 14 天未跑 codemap → 跑过 `/update-codemaps`（doc-updater agent）
- [ ] 改动 README/CHANGELOG → 已遵循 imboy/CLAUDE.md 双语规则（中文权威 + English 同 PR）

### 安全

- [ ] 无 hardcoded credentials（gitleaks ratchet 8）
- [ ] 用户输入已验证（参数化 SQL + 业务规则校验）
- [ ] 端点 JWT 认证（除 open 路由）

## 关联 / Related

- Issue: #
- 主计划任务: <!-- 如 T3.6 / T5.1 -->
- 相关 PR (imboyapp / admin): <!-- 跨仓改动需链接 -->

## 测试计划 / Test Plan

<!-- 复现步骤 / 影响范围 / 回归测试覆盖 -->

## CI 触发的检查

本 PR 会自动跑（详见 `.github/workflows/`）：
- `quality.yml` → erlang-lint (elvis) + contract-lint (redocly) + oasdiff-breaking + secrets-scan
- `sonar.yml` → SonarCloud 扫描（含 coverage trend）
- `codemap.yml` → codemap freshness check（PR paths 触发）
- `backend-ci.yml` → build + eunit + dialyze（业务 CI）

合并前所有上述 status check 必须 ✅。
