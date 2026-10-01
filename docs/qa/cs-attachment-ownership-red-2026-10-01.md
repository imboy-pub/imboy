# 客服附件当前经办权限缺陷

此页保留修复前失败证据；后续修复与当前验证见 [当前经办权限验证](cs-attachment-authority-2026-10-01.md)。

原失败状态：PARTIAL，当时尚未提交或合入代码，不能作为投产通过证据。

本轮已修精确的 presign/confirm/content Web 坐席设备签名豁免，仍走 JWT、职能、企业及经办授权；Loader 添加附件下载 sandbox 权限；Seat 与 Widget 内容通道拒绝 HTTP 200 JSON 错误信封。72 项相关测试、类型检查、针对产品改动的 lint、新 Widget/Seat 构建及显式跨仓资产配对均通过。静态宿主原生检查 3/3 通过。

实际 Garage + PostgreSQL + Chromium 链 /tmp/imboy-seat-http.uH8mmN 在坐席 A 获胜时通过：双向下载 29/26 字节完全一致、6 条唯一消息、2 个 active 且绑定消息的附件，以及断流重连、停用恢复、转接、结束、评分。该成功不证明不同坐席接管后的附件权限。

再次运行 /tmp/imboy-seat-http.r8Z2vN，实际并发抢单结果 [409,200]，坐席 B 已接管并能发送正文，但访客附件下载返回 403 forbidden.not_assignee。代码追踪发现附件 eb_asset_scope 读取 enterprise_conversation.business_identity_id，而 claim/transfer 只更新 customer_service_session.business_identity_id。二者授权事实不一致；不能固定 A 获胜或放宽权限掩盖问题。

新增转接后附件权限断言：旧坐席 403、新坐席 200。修复须统一客服当前经办授权真源，同时保留 sales、企业/工作区、停用/离职及跨会话拒绝。修改后重跑真实链路。当前新增该断言的运行在 B 初次下载即失败，未到达转接断言，不能宣称后者已执行。

证据目录：evidence/cs-attachment-ownership-red-2026-10-01。仅保存合成结果和脱敏日志，不保存浏览器 fixture JWT、生成存储凭证、HTTPS 私钥或上传凭证。此前成功运行尚未冻结新版构建，因此不当作不可变交付证据。未部署、未生产迁移。
