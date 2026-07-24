# 参考（Reference）

> **本目录定位**：信息导向。工作中查阅的事实：API、协议、错误码、配置项、约定。

**写作要求**：
- 只陈述事实，不教学、不劝诫
- 完整 > 精炼：参数全列、默认值全标、错误码全覆盖
- 每个接口至少一个可运行的请求/响应示例
- 「边界与限制」（速率、分页、长度上限）单独成节——最容易被遗漏

**判断标准**：如果文档在讲「为什么这样设计」，它该去 `explanation/`。

## API 与协议

| 文档 | 内容 |
|------|------|
| [rest-api.md](./rest-api.md) | REST 通用入口与基础契约（envelope、TSID 字段约定） |
| [rest-api-v1-catalog.md](./rest-api-v1-catalog.md) | `/api/v1` 全量端点目录 |
| [api-format.md](./api-format.md) | 请求、响应和分页约定 |
| [error-codes.md](./error-codes.md) | 错误码定义与使用 |
| [utf8-encoding.md](./utf8-encoding.md) | UTF-8 编码约定 |
| [ws-protocol-contract.md](./ws-protocol-contract.md) | WebSocket 消息信封与事件约定 |
| [websocket-api-2.md](./websocket-api-2.md) | WebSocket API 详细协议（全量参考） |
| [tsid-field-convention.md](./tsid-field-convention.md) | TSID 跨端字段约定 |
| [tsid-field-matrix.md](./tsid-field-matrix.md) | TSID 字段矩阵 |
| [ws-repl-cheatsheet.md](./ws-repl-cheatsheet.md) | WebSocket REPL 开发速记 |
| [contracts/](./contracts/) | 频道/朋友圈/E2EE 分片契约 v1 |

## 插件规范

| 文档 | 内容 |
|------|------|
| [plugin/contract.md](./plugin/contract.md) | 插件契约（imboy_plugin behaviour 权威定义） |
| [plugin/lifecycle.md](./plugin/lifecycle.md) | 生命周期 gen_statem 精确规范 |
| [plugin/frontend-integration.md](./plugin/frontend-integration.md) | manifest + WS push API 协议规范 |

## 工程笔记

| 文档 | 内容 |
|------|------|
| [engineering/](./engineering/) | CI/配置/依赖/Docker/日志/可观测/发布/技术债笔记 + 迁移命名规范 |

## 静态类型检查

| 文档 | 内容 |
|------|------|
| [static-typechecking/](./static-typechecking/) | Gradualizer + eqWAlizer 双引擎：选型分析、落地规划、误报决策日志、CI 集成验证 |

## 待生成

- `api/`：REST API 参考站点，由 `imboy/api/openapi.yaml` 经 Redoc CI 自动生成，**禁止手写**

模板：见 [documentation-system/templates/REFERENCE_TEMPLATE.md](../documentation-system/templates/REFERENCE_TEMPLATE.md)
