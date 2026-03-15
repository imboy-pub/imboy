# ADR: Backend Modular Monolith Boundaries And Plugin Scope

- Status: Accepted
- Date: 2026-03-15
- Context:
  `imboy` 后端已经形成 `router -> handler -> logic -> ds -> repo` 的分层结构，但跨领域调用边界和扩展点约束仍然分散在实现细节里。随着 Flutter 客户端和管理后台同步推进领域模块化，如果后端继续以“按文件堆叠能力”的方式演进，领域边界会持续变弱，调用路径也更难稳定。与此同时，消息类型、后台面板等高变化能力需要扩展点，但核心一致性链路不能被泛插件化侵蚀。
- Decision:
  后端继续采用 modular monolith，不拆分为微服务。领域边界以稳定的 public entry / facade 为先，优先通过薄封装和兼容层收敛调用入口，再逐步迁移内部实现。插件化只用于高变化扩展点，例如功能清单、消息类型或后台面板配置，不用于消息一致性、权限判定、事务状态流转等核心链路。
- Consequences:
  后端领域迁移可以在单仓内渐进推进，保持现有路由、接口与部署模型稳定。新代码需要优先依赖领域公开入口，而不是直接跨领域调用内部 `logic/ds/repo`。插件注册点需要采用窄接口和显式 manifest，避免演变成通用脚手架式平台。
- Non-Goals:
  本 ADR 不引入微服务拆分，不改变现有 HTTP / WebSocket 路由契约，不在本阶段删除所有兼容层，也不把核心消息主链路改造成插件系统。
