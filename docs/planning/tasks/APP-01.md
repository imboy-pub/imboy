# APP-01 — Flutter 真实 Agent Task 路径与真机前置验收

> 所属主计划：[IMBoy Agent Hub 产品收敛执行计划](../ai-agent-mcp-bot-webhook-product-convergence-plan-2026-09.md)
> 本文件是 APP-01 的权威执行卡。跨任务冻结决策、全局协议、依赖拓扑和独占边界以主计划 §1-§7 为准。

执行前必须先完成：

- 执行主计划 §4.1 的路径探测，使用 `$IMBOY_WORKSPACE_ROOT` 与 `$IMBOY_EVIDENCE_ROOT`，不得写死个人路径。
- 核对依赖状态和 §7 独占边界；只修改本卡声明的范围。
- 后端测试模块名以 G0-01 的 `test-module-map.tsv` 为准；卡片计划名变化时由 Coordinator 同步。
- 证据写入 `$IMBOY_EVIDENCE_ROOT/APP-01/evidence.json` 并经 EVID-00 verifier 判定。
- 不 reset/clean/stash，不覆盖共享改动；未经人工确认不 commit，不 push，不执行外部或生产动作。

- Status：`blocked`
- Owner：Flutter-Agent-Hub
- Dependencies：MCP-02、TRACE-00；AGT-02 若执行则一并接入
- Repo：`imboyapp`
- Files：现有 agent task progress/card/API/event state、相关 unit/integration tests；禁止 iOS/macOS 保留区。
- Actions：
  1. 删除对 demo driver 的产品依赖，使用 MCP-02 创建的真实 task/event/decision，并透传可信 correlation_id。
  2. 保持过渡态不落库、不增加未读；终态/审批卡片落库并可重进会话回读。
  3. 审批按钮处理 loading、重复决定、过期、无权限、网络失败；不会因重建 widget 重复提交。
  4. E2EE 会话不显示服务端 task 入口；收到违规 server frame 时丢弃并记录非敏感诊断。
  5. 加最小双账号真机脚本/清单，但本任务只跑 unit/widget；设备执行留 EXT-01。
- Verify：

```bash
cd "$IMBOY_WORKSPACE_ROOT/imboyapp"
flutter test test/unit_test/component/chat/message_agent_task_builder_test.dart
flutter test test/unit_test/page/chat/agent_task_ephemeral_state_notifier_test.dart
flutter test test/unit_test/store/api/agent_task_api_test.dart
flutter analyze lib test
git diff --check
```

- Acceptance IDs：`APP-01-A01` FSM-00 的客户端可见状态可渲染；`APP-01-A02` 重进会话保留 durable 状态/correlation；`APP-01-A03` 重复点击不重复决策；`APP-01-A04` 非 E2EE 约束有正反测试。
- Stop：没有已授权真机时不得启动模拟器替代功能验收，也不得自动安装 App。
