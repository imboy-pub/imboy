# 企业频道列表数量限制修复

基线 3434bd73。企业频道入口已经由 EnterpriseShell 消息页分段切换复用 WorkspaceChannelsPage，本轮未增加导航。

workspace_handler 错把 elib_param:int/3 的 {ok, Integer} 当成数值，Erlang term 比较使 limit 恒为 200。改为解包，保留既有默认 100 与 1..200 边界。没有修改成员校验、scope/status 分区或个人频道接口。

先通过真实客户端 HMAC 签名和 Human JWT、完整中间件/Cowboy/PostgreSQL 复现 limit=1 返回四条合成频道；修复后 limit=1/2/0/-9 分别返回 1/2/1/1 条，所有行属于当前 Workspace，外企业用户返回业务 403。没有绕过设备签名或 JWT。初次测试缺少客户端签名而被 902 正确拒绝，补足真实签名后才复现数量缺陷。

8/8 真实 HTTP/PostgreSQL 门禁通过，仍要求 42 个 Internal 操作完整覆盖；8/8 Workspace boundary EUnit 通过。新增数量断言，补齐旧测试对 member_only/cursor/preview 参数默认值的 mock 支持，旧 mock 因接口扩展产生的 function_clause 已修正。当前产品源码全量编译、实际全量迁移、erlfmt 和 whitespace 检查通过。人工复查函数调用与参数流，未执行子代理审查。

[证据来源与摘要](evidence/workspace-channel-limit-2026-10-01/sha256.json)。临时合成数据库和容器已清理；主仓无关改动须保持。完整六项目标仍未交付；频道全量分页、搜索刷新及整体 App UX 和设备验收仍未完成，本轮不作完成声明。未部署或生产迁移。

English summary: Fix unpacking of the channel-list limit parameter. Real signed Human HTTP requests reproduce and verify the fix, including lower-bound normalization and cross-organization refusal. Eight real HTTP/PG checks and eight Workspace boundary tests pass. The existing enterprise channel entry is reused; full pagination and complete product delivery remain unfinished.
