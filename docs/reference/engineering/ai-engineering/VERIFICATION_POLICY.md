# 独立验证政策（Verification Policy）

> 版本 1.0 · 2026-11-18 · 隶属 [ai-engineering](./README.md)
> 原则：**Worker 不得自证任务通过**。本政策把三仓已在运行的验证设施映射为统一三级体系，并规定证据标准与独立性规则。

---

## 1. 三级验证定义与三仓现状映射

| 级别 | 内容 | imboy 现役设施 | imboyapp 现役设施 | imboyadmin 现役设施 |
|---|---|---|---|---|
| **L1 静态** | 格式/lint/编译/类型/静态分析/安全依赖扫描 | erlfmt、elvis、compile、dialyzer 基线门、xref-strict、semgrep/trivy、sbom-diff、p5-manifest | dart format、flutter analyze ratchet、gitleaks、secrets-scan | eslint --max-warnings=0、typecheck、knip、gitleaks |
| **L2 动态** | 单元/集成/API/WS/DB/跨端契约测试 | quality_full 路由（`IMBOYENV=local make eunit`）、CT 19 套件、contract-gate（OpenAPI）、nightly 慢门 | `run_test_suite.sh --profile pr\|full`、SQLite 迁移门、contract.yml（error_code 逐字+SHA 锁） | `bun run test`（--isolate）、admin-e2e（setup-flow）、widget-pairing |
| **L3 真实业务验收** | Playwright/Flutter 集成/真机/多端链路 | smoke 脚本（`make ctl ARGS="smoke all"`）、release golden-install/upgrade | integration_test 8 profiles（device-single/device-pair/native-ui/backend/golden/routes/web/web-shell，手动+真机优先）、Patrol | Playwright 专属 8 配置（customer-service-real/p2/hosted 等，需真实后端） |

**选择规则**：按变更影响面选级——docs-only → L1；代码 → L1+L2 起步；跨端协议/DB/权限 → 必须含跨端契约证据；UI/链路行为 → L3 采样。**每次验证必须记录选择依据与未覆盖风险**，未覆盖不等于 PASS。

## 2. 证据标准（每次验证的最低记录）

```
Task ID / PR / Git SHA
环境声明（是否干净 worktree、env、依赖版本关键项）
实际执行命令（可复制，含 env 前缀）
原始退出码
测试数量：total / pass / fail / skipped（skipped 必须可解释）
原始日志/报告路径（evidence/ 内相对路径）
未覆盖范围与理由
最终判定：PASS | FAIL | NEEDS_MANUAL_REVIEW
```

**反假通过清单**（来自三仓真实事故教训）：
- SKIP / 空测试集 / 测试未执行 / 环境不完整 → 一律不得判 PASS。
- bun 必须带 `--isolate`（共享全局对象曾致 2360/5 vs 2365/0 假差异）。
- 批次连坐：批跑中前序失败会污染后续 → 判读 counts.failed 与个案日志双确认。
- 契约性测试失败先判环境（PG/网络）再判代码；REJECT 瞬态（noproc/cancel）≠断言失败。
- 陈旧 beam/pcm 缓存：指纹不符必须现编译，不引用旧产物。
- 收据/验证禁止并发跑同一环境；双 env（local/test）口径不得混。

## 3. 独立性规则

1. **Verification Agent 不得复用 Worker 的工作目录**：在独立 worktree（或至少 `git stash` 隔离后的干净检出）执行验证。
2. **验证者不得修改被验证代码**：发现问题退回 Worker 修复后重验，验证者只产出判定与证据。
3. **CI 是第三验证者**：PR 必须过三仓 CI 快门（backend-ci / ci.yml / Admin Frontend CI）才算 PR_CREATED→VERIFYING 完成；CI 红即 FAIL，无人工豁免。
4. `VERIFIED` 状态由 Verification 角色写入 state.json（引用 evidence 路径），Worker 只能推进到 PR_CREATED。

## 4. 与现有体系的衔接

- 任务包 `evidence/` 目录是证据第一归宿；长任务沿用 receipts 机制（AUTOTEST 模式）。
- CI 结论以 GitHub Checks 为准（strict 分支保护下 merge API 服务端强制）。
- 本地等价命令必须与 CI 同口径（lefthook pre-push 与 CI 对齐是既成实践，勿绕过）。

## 5. 判定出口

- PASS → 任务状态机推进 VERIFIED。
- FAIL → 回 Worker，附失败证据路径；重试上限 3 次。
- NEEDS_MANUAL_REVIEW → 附待决问题清单，进属主决策队列（见 AGENT_OPERATING_MODEL.md §5）。
