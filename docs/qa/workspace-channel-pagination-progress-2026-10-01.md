# 企业频道完整分页、搜索与刷新

基线 Backend b6867896、App 8bf29ceb。复用企业「消息」里的频道切换及既有详情页面，不增加一级导航。个人导航保持消息、通讯录、频道、我。

## 交付行为

- Human GET /api/v1/workspaces/:workspace_id/channels 新增 paged=1，与现有群目录同款可选分页。旧请求保持创建时间排序及 list 信封。新模式按不可变 ID 倒序，cursor=0 首次查询；排他上界与 limit+1 判定 has_more，最后一页 next_cursor=0。删除游标指向的旧行仍能继续读取；刷新才纳入翻页期间新增的更大 ID。
- 仍校验当前工作区成员，SQL 限定 scope=workspace、workspace_id 及 active/archived/all。新模式拒绝负数、非整数、超正 int64 游标及未知状态；不放宽个人频道或附件访问权限。
- App 完整读取当前工作区所有页；任一页业务失败、跨空间/个人资源、无效状态/ID、重复/不递减 ID、超长页、缺分页标志或错误游标，整份列表失败，不返回前几页假装完整。
- 页面按名称、数字 ID、自定义 ID 本地搜索。提供下拉和按钮刷新、忙碌指示及禁用重复按钮；成功刷新保留搜索。失败显示错误和重试并撤下旧列表；空目录与搜索空结果仍可刷新。账号/空间作为搜索状态键，切换时清空旧搜索并重新读取资源。
- 新 App 要求分页协议，须先部署支持 paged=1 的后端；旧 App 行为保留，新 App 对旧服务器的缺失分页响应显示错误，不伪装完整结果。此处仅本地代码集成，未进行任何部署。
- 使用现有 i18n、Cupertino、AppSpacing、Riverpod；未新增依赖。当前空间全目录放在内存以支持完整本地搜索，大规模目录应改为服务端搜索与分页 UI。

## 实证

8/8 全当前代码真实 PostgreSQL/Cowboy 门禁，覆盖全部 42 个 Internal 操作。新增 Human 请求使用真实客户端 HMAC 与 JWT：两页读取、分页期间新增及删除游标行、归档和全部过滤、五项无效参数与跨企业拒绝；205 个有效合成频道完整翻页，limit=500 被限制为 200，下一页五项，所有 ID 唯一且最后游标归零。8/8 Workspace boundary EUnit 通过，legacy handler 数量及授权保持。

48/48 App API/Widget/邻近群与会话测试通过，含完整三页、失败不返回部分数据、全部分页拒绝项、archived/all、搜索清空、大小写、自定义ID、空目录恢复、忙碌反馈和保留搜索、刷新错误/重试、账号与工作区切换。定向 flutter analyze 零问题。未把 Widget 测试声称为真机或三端 E2E。

新 Human 接口挂载至 API root 和生成式聚合。修正既有群目录三处 OpenAPI 3.1 nullable 为联合类型，保留 null 语义。API root lint exit 0，仍有 87 项其他已有文档告警；新增频道文件无诊断。Internal bundle、aggregate 确定性及 12/12 发布契约断言通过，erlfmt/whitespace 通过。初次客户端测试误用了超 64-bit 的 Dart 整数字面量，改为实际 JSON 字符串表示；初次 analyzer 提示 refresh 结果使用问题，改用既有 invalidate + read 模式并重新验证，未压制告警。

[日志与两仓来源摘要](evidence/workspace-channel-pagination-2026-10-01/sha256.json)。临时合成数据库及容器已清理。人工审查 SQL 参数化、scope/状态/游标边界、分页进度、失败与 UI 隔离，未执行子代理审查。

完整六项目标仍未交付；坐席完整旅程、企业组织与资料全流程、OA 全面资格、真机/外部/生产证据仍需继续。未推送、部署或进行生产迁移，E2EE 历史数据未改动。

English summary: Enterprise channel directories gain opt-in descending-ID keyset pagination, complete fail-closed App loading, local name/ID search and explicit refresh states. Real signed Human requests verify 205 channels, insert/delete stability, filters, invalid parameters and cross-organization refusal. Forty-eight App checks and eight Workspace boundary checks pass; all 42 Internal operations remain covered. API lint succeeds with 87 existing unrelated warnings. Device and complete six-part production delivery remain unproven.
