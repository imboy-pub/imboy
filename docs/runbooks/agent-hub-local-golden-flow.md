# Agent Hub 本地诊断 / Agent Hub local diagnostics

本手册介绍本地诊断脚本的运行条件，不包含历史运行结果或候选验收结论。Agent Hub、Bot、AI 助手与 MCP 属于第二期，一期交付保持入口关闭。

This guide describes local diagnostic prerequisites without certifying a candidate or recording historical results. Agent Hub, Bot, AI assistants, and MCP belong to phase two and remain disabled in phase-one deliveries.

## 隔离与配置 / Isolation and configuration

- 使用脚本创建并标记的 scratch 数据库及 loopback 端点，不连接生产或真实用户数据。
- 依据目标候选配置数据库、迁移及所需扩展；不要沿用历史固定迁移号或扩展数量。
- 本地 fixture provider 和 webhook receiver 用于诊断，不证明真实外部服务可用。
- 凭证及原始产物写入仓库外的受控目录，避免泄露到日志和提交。

- Use marked scratch databases and loopback endpoints, excluding production and real-user data.
- Configure database prerequisites, migrations, and extensions for the target candidate rather than historical fixed numbers.
- Local fixture providers and webhook receivers diagnose behavior; they do not qualify external services.
- Keep credentials and raw artifacts in controlled directories outside Git.

## 运行脚本 / Running the script

先阅读 `scripts/agent_hub_golden_flow.sh` 的参数及资源创建、清理逻辑。输出目录必须位于工作区外且末级目录为 `E2E-01`：

Read script arguments and resource creation and cleanup behavior first. The output directory must be outside the workspace and end with `E2E-01`:

```bash
bash scripts/agent_hub_golden_flow.sh --profile local-fixture \
  --evidence-dir "$IMBOY_EVIDENCE_ROOT/E2E-01"
```

根据实际退出码、日志和业务断言判断结果；部分通过、跳过或缺失的检查不能解释为完整通过。最终集成模式额外要求脚本规定的分支和候选文件一致性，应在冻结候选上运行。

Assess actual exits, logs, and behavioral assertions. Partial, skipped, and missing checks do not establish full completion. Final integration mode adds branch and candidate consistency requirements and must run on a frozen candidate.

## 密钥生命周期 / Key lifecycle

长期操作规范见 [Bot webhook AEAD 密钥 / Bot webhook AEAD keys](./bot-webhook-aead-key.md)。密钥轮换及退役须遵循对应环境的授权和恢复要求。

Follow the linked key lifecycle guide for rotation and retirement, including target-environment authorization and recovery requirements.

## 记录与清理 / Records and cleanup

执行计划、验收卡、截图、测试日志和报告保存在仓库外。记录当前候选与配置哈希、实际命令及行为断言。清理限定于本次创建且具有归属标记的资源，核对数据库、端口、进程和凭证文件的实际终态。

Keep plans, acceptance cards, screenshots, test logs, and reports outside Git. Record current candidate and configuration hashes, commands, and behavioral assertions. Clean only resources created and marked by the current run, and verify final database, port, process, and credential-file state.
