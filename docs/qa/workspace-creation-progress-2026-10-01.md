# 工作空间创建：配额和并发验证

范围：复用现有 workspace_ds:create_template/4，修复共享创建流程；尚未实现新增 Internal Workspace 写接口，也未完成全部六项需求。

- 同 Owner 的事务锁覆盖幂等查询、配额统计和模板创建；配额统计使用调用方 Conn。统计失败拒绝创建，不再当作零。
- 已有工作空间先于配额检查返回，因此配额达到 100 后，同请求或同名重试仍成功。删除无调用者且吞错的旧 count_by_owner/1。
- 组织资格校验仍先执行，Owner/Admin 边界与企业默认工作空间初始化关系不变；未增加自动 Grant。

## 验证

- workspace_template_tests + workspace_org_default_relation_tests：15/15，通过当前源码独立编译到 /tmp 后运行。覆盖配额统计失败、模板故障、资格拒绝、个人归属和默认关系。
- 真实 PostgreSQL 全量迁移和 Cowboy 门禁：8/8。两个并发相同请求只创建一个；99 个工作空间时两请求争抢最后名额恰好一个成功；达到 100 后重试已有空间成功。
- 实际验证 owner 成员、默认群和默认频道归属；用事务内无权限角色确认真实统计错误原样返回，回滚后计数仍是 100。
- 全部当前产品源码编译成功；存在既有 deprecated catch 警告。erlfmt 定向检查、脚本语法和 git diff --check 通过。
- 测试启动真实 imboy_cache，未 mock 缓存或数据库。仅原有对象 HEAD / DNS 外部替身保留。测试自建容器自动清理，既有容器和数据不变。

复跑：`IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`。

证据：[HTTP 日志](evidence/workspace-creation-2026-10-01/http.txt)、[源码与证据哈希](evidence/workspace-creation-2026-10-01/sha256.json)。基线 769a552b；源码哈希绑定本次验证。

人工复查了共享创建调用者、锁顺序、事务返回、默认关系和旧计数调用集，没有执行子代理审查。明确待处理：日志暴露原有 enterprise_webhook_logic:emit_event_failed/4 错误匹配 `{ok,ok}`，真实 with_tx 返回裸 `ok`，因此产生错误日志；此问题未在本批混入修复。测试通过不代表事件失败通知链已可投产。

English summary: Local progress only. Owner-scoped transaction locking now protects idempotency and quota checks on the same connection. Existing resources can be retried at the quota limit; count failures reject creation. Fifteen focused tests and eight real HTTP/database tests pass, including concurrent creation, final-slot contention and complete default resources. Source hashes bind the evidence. Workspace write APIs, the webhook failure-emission contract issue and the full six-part production objective remain unfinished.
