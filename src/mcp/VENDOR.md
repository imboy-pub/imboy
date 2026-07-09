# Vendored: barrel_mcp 协议引擎（Phase 3 T3.1）

来源 / Source: https://github.com/barrel-platform/barrel_mcp （Apache License 2.0，见 LICENSE.barrel_mcp）
仅取**传输无关的协议引擎**（decode/handle/encode + registry + schema + session/tasks/uri_template）；
裁掉 h1/h2/hackney 传输层与 client/http/oauth/auth 模块（imboy 用自己的 cowboy + JWT）。

vendored 模块（6）：
- barrel_mcp_protocol   JSON-RPC 2.0 编解码 + MCP 方法派发
- barrel_mcp_registry   tool/resource/prompt 注册表
- barrel_mcp_schema     JSON Schema 校验
- barrel_mcp_session    会话状态（gen_server，传输无关）
- barrel_mcp_tasks      异步任务（gen_server）
- barrel_mcp_uri_template  URI 模板

依赖：仅 OTP 内建 `json`（OTP27+）+ gen_server，无第三方 JSON/HTTP 依赖。
⚠️ barrel 跟的 MCP spec 2025-11-25 为 draft，仍在 RC 演进，升级需回归。
