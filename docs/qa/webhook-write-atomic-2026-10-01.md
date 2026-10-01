# Webhook 配置与密钥写入结果校验

状态：本地专项 PASS；整体六项目标仍为 PARTIAL，未执行生产操作。

历史真实 HTTP / PostgreSQL 红证据 `sKkQg7` exit 1：原生 BEFORE UPDATE 触发器阻止 verify_token_enc 更新，接口仍返回 200 和新 secret，新 secret 与实际持久化密钥不同，配置状态改变、审计 +1。原因是 enterprise_webhook_repo:set_secret_tx 忽略 UPDATE 结果，固定返回 updated。相邻 upsert_config_tx 也把 INSERT RETURNING 的空结果当作 updated，端点配置可能没有生效却记录成功。

两处共享仓储函数现在验证实际写入结果：密钥更新必须恰有 1 行；空结果或零行返回错误，SQL 错误向上传递。既有配置逻辑/HTTP Handler 复用同一事务将错误映射为 internal_error，并回滚端点配置、密钥、代际、审计与幂等占位。不新增错误码、迁移或依赖。

`PCLuwz` exit 0：独立 PostgreSQL 全量迁移、当前全部产品 fresh compile、真实 INT-12 HTTP/鉴权。密钥和配置分别注入原生 RETURN NULL / RAISE EXCEPTION，共 4 个场景：

- 失败请求均 HTTP 500 internal_error；完整 bot 行状态保持不变、审计增量 0，无 secret 返回。
- 移除故障后同一个 Idempotency-Key 重试均成功、审计恰增加 1。
- 初次配置与成功轮换的响应密钥必须逐字等于仓储实际解密密钥（断言为布尔，不打印密钥）；不轮换的配置响应无 secret。
- 既有治理审计故障、并发轮换及 6 项凭证/Grant 生命周期 HTTP 检查同轮通过。

现有完整 Internal HTTP 门禁 `HHv8PC` exit 0，28 项通过，42 操作覆盖集合与路由一致。外部 HEAD/DNS 的替身边界仍按原门禁说明保留。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ADMIN_GOVERNANCE_PG_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
```

安全运行数据、日志和源码 SHA 归档于 `evidence/webhook-write-atomic-2026-10-01`；历史失败及原始检查保留在 `evidence/webhook-write-red-2026-10-01`。不归档凭证、密钥、配置或崩溃转储。

此专项验证本地配置/签名密钥事务与 HTTP 回应；SSRF 校验使用合成公共 DNS 窗口，没有向外部服务投递。完整真实回调、真实 OA、App 真机与整体投产资格仍待验收。
