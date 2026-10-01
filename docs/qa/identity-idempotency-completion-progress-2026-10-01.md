# 身份映射契约与幂等完成回滚

基线 448b9551。修正 PUT identity-mappings 的实际响应描述：首次与重放正文均为 external_user_id、user_id、status=active，重放只通过 Idempotent-Replayed 响应头标识。删除两个重叠且字段非必需的 oneOf，改为必需字段的闭合响应 schema；DELETE 响应同样闭合。重放是首次提交的历史快照，查询当前状态应使用 resolve。平台 user_id 拒绝超过正 int64 的值。

发现九个调用点忽略 complete_tx 的返回值：身份绑定/解绑、附件确认、消息、好友请求、群变更及三项 Webhook 写操作。统一调用 must_complete_tx，保存快照失败则抛事务回滚，既有 HTTP 壳返回 internal_error，不会提交业务成功却留下 pending 幂等占位。Seat、Workspace、Channel 原有显式检查不变。

## 验证与边界

- 先用真实 PostgreSQL 的 BEFORE UPDATE RETURN NULL 复现：原代码错误地返回 HTTP 200，预期 500 的测试失败，见 baseline-failure.txt。
- 修复后分别对绑定和解绑注入 UPDATE 零行与数据库异常，四次真实 HTTP 都返回 500/internal_error，映射状态、审计数量不变且无幂等占位残留。故障注入使用临时合成数据库，未接触生产或真实用户数据。
- 两项正常首次响应和重放均 HTTP 200、响应原字节一致、重放 header 为 true；捕获的四份真实正文通过 source OpenAPI schema 校验。超 int64 user_id 返回 400。
- 完整当前代码编译、全量实际迁移和 8/8 PostgreSQL/Cowboy 测试通过；原有覆盖断言要求全部 42 个 Internal 操作 / 28 条路径。九个变更调用点的正常流程在此门禁覆盖；没有声称逐一注入九个调用点故障。
- 12/12 发布契约断言、bundle/aggregate 确定性检查、320 迁移文件/160 组、erlfmt 与 whitespace 检查通过。OpenAPI lint 无告警。
- 人工复查全部九个调用点的事务范围、回滚分支及提交后推送，未执行子代理审查。未新增依赖、迁移或授权自动授予。

证据均在 [evidence/identity-idempotency-completion-2026-10-01](evidence/identity-idempotency-completion-2026-10-01/sha256.json)，包含绑定源文件摘要、真实响应和测试日志。隔离测试容器已由脚本清理。

本轮是局部可靠性修复，完整六项目标仍未交付。App 坐席与企业 UX、OA/全部 API 整体资格、资料存储、真机及生产验收继续推进。未部署或进行生产迁移。

English summary: Identity response contracts now describe actual closed response bodies and replay headers. Nine ignored idempotency completion results use a shared mandatory guard, rolling back on failed snapshot persistence. Real HTTP fault injection verifies binding/revocation rollback for zero-row updates and SQL errors. All 42 operations pass the current real HTTP conformance gate; four captured identity bodies match OpenAPI, and lint is warning-free. The complete six-part production objective remains unfinished.
