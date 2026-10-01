# Webhook 密钥与幂等响应快照

状态：本地专项验证通过；整体六项需求仍为 PARTIAL。

历史版本 c380f68a 的真实 HTTP / PostgreSQL 检查 `d6oSBC` exit 1：INT-12 首次配置与相同键重试均返回 200、响应逐字节一致，但数据库 response_body 包含返回的签名密钥。失败断言只记录布尔值，不打印密钥。第一次检查 `3fPJCZ` 使用错误列名并读到默认空值，不能作为通过证据；最终检查明确要求读取到非空快照。

共享幂等中间件现在对 enterprise_webhook_config 的完整响应使用既有 AES-256-GCM 加密，合法 JSON 信封保存在原 text 列。HMAC-SHA256 从既有主密钥和固定域、组织、应用、幂等键、资源类型、资源 ID、HTTP 状态派生加密键，因此复制密文到其他键不能重放。无需新增依赖、迁移或改变 HTTP 响应格式；解密后保留原始响应字节。

`iKHPuM` exit 0：全量迁移、全部当前产品源码重新编译，真实 HTTP / 鉴权验证：

- 原始数据库快照不包含签名密钥；首次与重试响应逐字节一致。
- 密文损坏、移至其他幂等键、主密钥缺失或错误，均 HTTP 500 internal_error；不重新执行业务，不增加审计，不返回密钥。
- 缺少主密钥时，新的不轮换配置写入也失败，配置状态与审计保持不变。
- 配置 postgre_aes_key_old 后，可解密旧密钥加密的快照，重放字节一致。
- 历史明文快照在其原幂等有效期内仍能精确重放；旧密文写入异常检查及凭证生命周期检查同轮通过。

历史明文行不会被本修改批量改写或清除；TTL 到期使其不能继续重放，但不等于物理删除。上线前应核实既有数据并安排清理，生产数据处理仍需单独授权。本地合成数据兼容性检查不证明生产已经不存在历史明文。

`ODm7ZE` exit 0：现有完整 Internal HTTP 门禁 28 项通过，覆盖集合仍为 42 个操作 / 28 个路径；源码与上述专项运行一致。此门禁不替代全仓 EUnit 或真机验收。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ADMIN_GOVERNANCE_PG_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
IMBOY_DEPS_ROOT=/path/to/independent/deps bash scripts/test/customer_service_internal_http_gate.sh
```

证据目录：`evidence/webhook-secret-snapshot-2026-10-01`。只归档安全布尔、状态、源码摘要和测试日志，不归档密钥、数据库原始响应、配置或崩溃转储。本地测试使用合成公共 DNS，不向真实 OA 投递；真实回调、真机及整体投产资格仍待验收。
