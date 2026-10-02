# Internal v1 必需审计的事务验收

冻结政策 required_ids() 全部 25 个操作逐项通过。复用既有成功请求，在独立 marker PostgreSQL 对 enterprise_audit_event / customer_service_event 加真实 CHECK(false) NOT VALID：原请求返回 HTTP 500 / internal_error，所有 public 表行摘要与失败前一致，包含幂等记录。仅忽略真实认证合法更新的 enterprise_application_credential.last_used_at；序列号消耗不是业务行。

解除约束后原请求返回 200，对应企业、政策 action 的审计严格增加一条。Seat35/36 落 customer_service_event，其余落 enterprise_audit_event。即时重放全部 public 表行不变，响应与首次逐字一致并包含重放标记；SSO14 以同 code 的不透明 404 拒绝代替幂等重放，未重复审计。

新增矩阵已接入既有 conformance 的成功链、工作区和频道成功写请求；完整 conformance 及42路由176条授权负例同时通过。生产源码未改。对象 HEAD、公网 DNS 仍沿用已有隔离服务替身；不作为真实存储、真实Webhook接收方、客户OA或iOS证据。没有 push、部署或生产迁移。

复现：把附带 .sh.txt / .py.txt 恢复到 /tmp 下同名 .sh / .py，设置 IMBOY_DEPS_ROOT 到构建依赖目录，按需修改 shell REPO 到当前独立仓，运行 bash /tmp/gz-internal-audit-conformance.sh；需要 Docker 的 imboy/pg18:3.6.1-2 镜像。默认重新编译全部当前源码。最终复跑仅复用逐项源码/依赖摘要相同且 beam 摘要一致的产物，变更的三个测试模块重新编译；冻结输入前后摘要一致。
