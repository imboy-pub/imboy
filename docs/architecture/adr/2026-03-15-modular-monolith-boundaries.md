# ADR: Modular Monolith Boundaries And Lightweight Plugin Extension Points

- Status: Accepted
- Date: 2026-03-15
- Context:
  `imboy` 当前已经具备 `handler -> logic -> ds -> repo` 的分层雏形，但不同业务域的入口、调用边界和扩展点定义仍然分散。随着消息、频道、群组、朋友圈、加密与后台能力继续演进，如果继续直接在全局目录中横向加文件，会让跨域调用、兼容层治理和后续门禁收口越来越困难。
- Decision:
  后端继续采用 modular monolith，不拆微服务。领域边界以仓内模块和公开入口组织，优先建立稳定的 domain facade、public contract 和兼容层，再逐步收敛内部实现。Flutter 和管理后台将同步按领域模块化推进，不再继续膨胀全局 `service/pages/components` 入口。插件化仅用于高变化扩展点，例如消息类型、媒体处理、后台面板扩展，不用于消息一致性、权限校验、核心路由等主链路。
- Consequences:
  后端的演进重点从“拆服务”转为“收敛边界”。`src/api/`、`src/adm/` 和跨域调用将逐步通过稳定的领域公开入口接入；旧入口会以薄封装和兼容层形式暂时保留，直到调用点完成迁移并有验证证明可收口。自动化门禁会在后续任务中补上，防止新增代码继续绕过模块边界。
- Non-Goals:
  本次决策不引入微服务拆分、不修改现有对外路由协议、不把插件系统扩展为通用远程加载平台，也不在单个提交内完成大规模文件搬迁。
