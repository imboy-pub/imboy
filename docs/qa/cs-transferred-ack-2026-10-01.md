# 客服转接后的投递回执权限

状态：局部本地验收通过；六项目标整体仍为 PARTIAL。

App 客服消息流使用 identity:<当前坐席业务身份> 确认投递。旧 ACK 经办门却比较 enterprise_conversation.business_identity_id，该字段保存稳定的入口身份，不随客服领取/转接变化。真实 HTTP 复现 /tmp/imboy-seat-http.2oIksa exit 1：转接后当前坐席 ACK 返回 403 forbidden.not_assignee，原坐席 ACK 返回 200。原检查 25 项通过、1 项失败；红检查源码与原始响应一起归档。

现在 ACK 与附件 ACL 复用 current_conversation_identity：普通销售会话使用原经办身份；托管客服会话通过现有客服 facade 查询当前 session 经办，且必须有有效坐席。无托管客服 session 的既有企业会话保留原经办规则；查询失败、未领取、无坐席或停用均拒绝。入口身份、消息本体与个人 ACK 清理链没有被改写。收件人必须仍是调用者自己的身份，来自认证事实，不采信客户端申报身份。

原生 Garage + PostgreSQL 检查 /tmp/imboy-seat-http.IP2HNE、/tmp/imboy-asset-garage.CIzYbC exit 0，原有消息应用 13 项、留存 11 项、孤儿清理 10 项及显式附件替身 6 项通过。新增复用消息套件时发现静态源码查找把 code:lib_dir 的 {error,bad_name} 当成路径，已在原测试帮助函数中按返回类型过滤；静态检查重新执行通过，不跳过或删除断言。

最终 HTTP /tmp/imboy-seat-http.RR4gek exit 0：26 个顶层检查及四种身份响应 schema 通过。转接后当前坐席 ACK 200、原坐席 403；再次 ACK 200 且同一投递 ID；通过现有应用 facade 停用后 ACK 403，恢复后 ACK 200。前后完整 canonical 行 JSON 的哈希相同。停用/恢复验证的是应用 facade 与真实数据库，不宣称治理 HTTP 或管理页面已验收。

复现修复：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_CS_ACK_OWNER_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ASSET_GARAGE_PG_CHECK=1 bash scripts/test/enterprise_asset_garage_gate.sh
```

首次修复 HTTP 运行在全部 26 项检查后因运行期间修改了测试脚本而收尾报错，不计为完整通过；修复静态源码路径前的 11/13 消息检查也不计套件通过。最终重新执行时固定测试源码。实际证据目录为 evidence/cs-transferred-ack-2026-10-01；历史红证据为 evidence/cs-transferred-ack-red-2026-10-01。归档只含合成响应与日志，不含 JWT 或生成凭证。

人工检查共享调用链及主体限制，不宣称独立代理审查。没有验证真实 App 设备、整个客服生产资格或转接/停用与 ACK 落库之间的并发窗口；总体验收仍需继续。没有部署、生产迁移或通知第三方。
