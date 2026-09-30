# 失败事件事务返回契约修复

根因：enterprise_webhook_logic:emit_event_failed/4 期望 `{ok,ok}`，但 elib_pg:with_tx/1 返回回调的裸值 `ok`。原真实 HTTP 门禁日志出现四次 `{badmatch,ok}`，属于成功路径错误报告。

修复：共享入口按真实契约匹配 `ok`；保留异常捕获和错误日志。两个调用方 enterprise_message_handler / enterprise_asset_handler 均复用此入口。未扩展通知协议、重试策略，也未宣称外部 Webhook 投递完成。

验证：当前全部源码重新编译；独立合成 PostgreSQL 全量迁移 + 36 条路由真实 HTTP conformance + 工作空间并发流程，8/8 PASS。门禁新增失败事件 crash 日志检查，旧日志会触发失败，新日志无该错误。定向 erlfmt、脚本语法、git diff --check 通过。

证据：[HTTP](evidence/webhook-failure-emission-2026-10-01/http.txt)、[源码哈希](evidence/webhook-failure-emission-2026-10-01/sha256.json)。基线 1e67bf0b；旧日志保留在 workspace-creation-2026-10-01/http.txt。

人工复查事务实际返回、两个调用方及异常路径。没有执行子代理审查。完整六项需求、Workspace/Channel 写接口和设备/投产门禁仍未完成。

English summary: The shared failed-event emitter now matches the actual bare transaction return value, eliminating false crash logs on successful completion. The real database and HTTP gate passes all eight tests and rejects recurrence of these crash logs. This does not establish external webhook delivery or full product readiness.
