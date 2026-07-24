# 0003 — 动态插件路由命名空间对齐为 `/api/v{n}/` 前缀

- 状态：Accepted
- 日期：2026-07-09
- 关联文件：`src/lib/imboy_router_registry.erl`、`src/lib/imboy_plugin_toml.erl`、`docs/reference/plugin/contract.md`
- 关联测试：`test/lib/imboy_router_registry_tests.erl`、`test/lib/imboy_router_plugin_routes_tests.erl`、`test/lib/imboy_plugin_toml_tests.erl`

## 背景

全站 REST 路由统一以 `/api/v1/*` 为前缀，静态定义在 `imboy_router.erl` 的 `ApiV1Routes`（含现有 channel/moment/location/group_collab 等功能，对外均 `/api/v1/...`）。

而**冻结的动态插件平台**（见 `docs/reference/plugin/contract.md`，标注为 roadmap-only、v2 代码冻结）原契约（§3.3）要求插件路由 path 必须以 `/v{n}/<plugin_name>/` 开头——**不含 `/api`**，与全站不一致。两处校验同源：`imboy_router_registry:validate_routes/2` 与 `imboy_plugin_toml:validate_routes/1`，正则均为 `^/v[0-9]+/<name>/`。

该平台当前**未启用**（生产插件走静态 `/api/v1/`，不经此注册表），因此不一致暂无运行时危害。但：

1. **误导**：落地"把 imboy 暴露为 MCP server"时，该正则差点使 MCP 端点被写成 `/v1/mcp/`，与全站脱节。
2. **测试实为红**：`imboy_router_plugin_routes_tests` 的注册**输入**已用 `/api/v1/channel/...`，但正则与断言残留 `/v1/`——输入撞正则被拒，`ok = register(...)` badmatch，相关测试实际失败。作者的心智模型已是 `/api/v1/`，只是契约层未跟进。

## 决定

将插件路由命名空间契约**对齐为 `/api/v{n}/<plugin_name>/`**，与全站 REST 前缀一致：

- 两处校验正则改为 `^/api/v[0-9]+/<name>/`。
- `contract.md` §3.3 约束条款、TOML 示例（`/v1/channel/*` → `/api/v1/channel/*`）、§manifest 字段表同步更新。
- 相关测试断言由 `/v1/...` 更正为 `/api/v1/...`（同时修复上述残留红测试）。
- `route_namespace_override + ADR` 的覆盖机制保留不变。

## 后果

- ✅ 未来若启用动态第三方插件，其路由与全站 `/api/v1/` 天然一致，无双前缀混淆。
- ✅ 顺带修复了 `imboy_router_plugin_routes_tests` 的残留失败（输入/正则/断言现三者一致）。
- ✅ 消除了"MCP 等新端点该用什么前缀"的误导源（记忆 `project_imboy_route_prefix_convention` 已同步）。
- ⚠️ 这是插件契约（contract semver）的 breaking change，但平台**冻结未发布、无任何外部插件依赖**，实际影响面为零。
- ⚠️ 将来第三方插件 manifest 的 `routes[].path` 必须写 `/api/v{n}/<plugin>/`，否则 loader 以 `invalid_route_namespace` 拒绝。

## 备注

MCP server（AI Agent 载体路线图 Phase 3，`docs/planning/ai-agent-platform-roadmap.md`）作为**核心固定端点**，直接进 `imboy_router.erl` 静态 `ApiV1Routes`（`/api/v1/mcp/`），不走本注册表——本 ADR 仅对齐"未来动态插件"的契约前缀，不改变"核心端点走静态路由"的做法。
