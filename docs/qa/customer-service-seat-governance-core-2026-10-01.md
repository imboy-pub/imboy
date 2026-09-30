# 客服坐席应用治理底层验收（2026-10-01）

基线：`ed548e5ac8282c2be1da8fff4fa44716a3df214f`。状态为底层用例通过，Internal HTTP 接口尚未接线；完整投产目标未完成。

## 已实现

- customer_service_facade:govern_seat/2 暴露给持有事务连接的可信适配层，由 application 验证形状、store 端口委派 PostgreSQL 实现；支持 list/detail/create/update。
- 列表按 business_identity_id 键集分页，包含停用席位；详情同 SQL 绑定企业。返回组织、业务身份、enabled、max_concurrent、version，不猜测工作区归属、不签发坐席 JWT、不创建成员或换绑身份。
- 工作区仅作为显式审计落点，必须是当前企业的 active 工作区。企业 active、身份同企业且职能 customer_service；开通和启用要求身份 active，显式停用允许收敛失效身份的遗留席位。
- 调整 enabled/max_concurrent 要求 expected_version；组织限额锁在席位行锁之前，与开通使用同一组织锁顺序。复用现有容量检查、启停和事件写函数，保留当前接待会话，容量下降限制后续新接单。
- 变更和 application 审计使用调用方的同一连接与事务。记录 application_id、correlation_id、before/after；不采信 Params 中的 actor_user_id 来冒充人类。
- 新增两个模块登记到客服物理裁剪清单；新增目录与裁剪清单精确相等的测试。

## 重要调用契约

适配层必须先完成 Application Credential、scope 和企业级显式 Grant 校验，再提供 connection、组织 ID、服务端时钟及从认证上下文派生的 application_id/correlation_id。该 facade 本身不是 HTTP 认证面。

写失败抛出 `{rollback, {error, Reason}}`，交由调用方事务回滚。形状校验失败返回 `{error, {invalid_argument, govern_seat}}`；若适配层已写入幂等预约，同样必须回滚。列表 primitive 允许最多 101 条，供公共分页上限 100 的 lookahead；公共游标签名、分页包装属于后续 Internal 适配层。

## 实际验证

- `/tmp/gz-seat-govern-beams` 编译实际候选的 feature 模块和测试，复用主仓依赖，不覆盖主仓构建产物。
- 新建仅 Unix socket 的隔离 PostgreSQL 18，合成用户 departure_test；测试结束已停机。运行信息 `/tmp/gz-seat-govern-runtime.json`。
- cs_seat_transaction_pg_tests:run(Socket)：16/16，日志 `/tmp/gz-seat-govern-tests.log`。包括原有十项连接和审计测试，以及六项治理验证：停用席位分页／详情隔离、版本检查、两个并发修改同一版本仅一个成功、审计及外层完成失败全部回滚、父资源状态和失效身份停用、限额与输入拒绝。
- 审计 oracle 核对 application actor、空 actor_user_id、应用 ID 和关联 ID；真实 CHECK 拒绝事件写入，不 mock SQL/事务。连接池入口限制每进程一次持有一条真实连接，TSID 采用合成单调 ID。
- cs_application_tests + cs_closure_tests：26/26，日志 `/tmp/gz-seat-govern-regression.log`；冻结端口、装配和 feature 引用一致。
- `python3 -m unittest discover -s test/scripts -p test_generate_product_features.py`：25/25，日志 `/tmp/gz-seat-govern-feature-tests.log`。
- 架构门最初发现新模块未登记裁剪清单；补齐生成器登记后 `env -u MANPATH bash scripts/check_feature_architecture.sh` exit 0，日志 `/tmp/gz-seat-govern-architecture.log`。未修改生成物或其他仓库。
- 既有 Internal manifest 检查 12/12；仍为 32 端点、26 路径，新 Seat HTTP 端点并未注册。
- 手工审查所有新增调用方、锁顺序、边界和错误传播；未运行独立审查代理。最小测试表不代替完整迁移、FK、触发器和 REST 验收。

## 后续仍需完成

新增 Seat Internal handler/logic、企业级 scopes 与 Grant 接线、幂等请求摘要与响应快照、读 usage、错误码映射、追加路由、OpenAPI/Postman/manifest，以及真实 HTTP + 完整数据库旅程。该底层通过不代表接口交付或投产通过。

English summary: the trusted transaction-based seat governance use case supports paged reads, creation and versioned settings updates, with parent checks, capacity enforcement and atomic application audit. Sixteen focused PostgreSQL checks, twenty-six feature regressions, twenty-five build contract tests and the architecture gate passed. Internal HTTP integration and full production acceptance remain pending.
