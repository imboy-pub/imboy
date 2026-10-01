# 全部 Internal 操作覆盖与集成合同校正

状态：本地合同及 HTTP 回归 PASS；整体六项目标仍为 PARTIAL。

当前 runtime 注册表是 INT-01..42，28 条路径，18 个 scope，16 个稳定错误码。本轮复核已有 conformance 链：各操作在真实 HTTP 预期响应及字段检查后登记，最终覆盖集合必须逐 ID 等于 runtime routes；任何缺项或多项均失败。新增安全覆盖文件只在全部行为、幂等及边界检查完成后写出，不再依靠文字描述 42/28。

`i8ImXk`：完整当前产品编译、一次性 marker PostgreSQL 全量实际迁移、真实 Cowboy/认证链，exit 0，28 项 EUnit 通过；42 操作集合等于注册表，28 条路径一致。新增两个回归检查：

- 错误信封须是封闭 JSON error/code/message，code 必须逐字等于预期，message 非空。伪造响应把正确错误码放在 message、实际 code 错误时必须被拒绝；不再用字符串出现作为证据。
- 集成 README 的权限表必须与 runtime 固定 scope 枚举集合一致，并使用服务端提供的 credential_prefix 表述 Bearer 格式。

同步修正对接文档与 OpenAPI：

- 前缀是服务端提供的完整 ib_int_ 前缀；不能用 application_id 或返回的 credential_id 重建。签发/轮换的 secret 字段实际返回完整可使用凭证。
- 单份凭证属于一个应用，同一应用可以有多份凭证。有效权限是 allowed_scopes 与生效 Grant scopes 的交集；零 Grant、全部撤销或过期不回退。
- 补齐 workspaces:write（INT-37..39）；权限表完整包含 18 枚举。
- Webhook HMAC secret 独立于 API 凭证，经 INT-12 rotate:true 轮换；API 凭证轮换不会自动更换回调签名密钥。
- 错误表补正排版；版本冲突用于资源，不只用于坐席。

OpenAPI 编辑真源、Internal bundle 和重新生成的 aggregate 格式同步。三份 YAML 可解析；aggregate --check 与 12/12 manifest 检查通过。文档语义由当前认证、签发与 webhook signing 调用链交叉核对。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps bash scripts/test/customer_service_internal_http_gate.sh
python3 scripts/check_enterprise_release_manifest.py
python3 api/gen_aggregate.py --check
```

证据：`evidence/internal-contract-coverage-2026-10-01`，含实际覆盖集合、源码 SHA、编译/HTTP/manifest 日志和 YAML/aggregate 校验。

此门禁的外部边界必须保留：INT-08 的对象 HEAD 使用替身，Webhook 使用公共 DNS 测试窗口且没有对真实外部服务投递；它证明已有本地 HTTP 合同、幂等/边界及检查链覆盖，不证明真实 Garage 的每个端点旅程、完整回调交付、真实 OA、Admin Cookie 页面、App 真机或全量投产资格。此前对象存储专项不能自动升级为全 API 外部验收。本轮没有部署、发布、生产迁移或第三方通知。
