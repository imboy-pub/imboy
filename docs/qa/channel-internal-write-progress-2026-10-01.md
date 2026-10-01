# 企业频道应用写管理

基线 a2147772。新增 INT-40 POST /channels、INT-41 PATCH /channels/{channel_id}、INT-42 DELETE /channels/{channel_id}，独立 channels:write，全部要求目标 Workspace Grant 与幂等键。固定企业非公开免费邀请制（visibility=1/access_type=0/join_policy=1）；归属、创建者、策略及未知字段拒绝。POST 校验正 int64 Workspace/creator ID、非空名称、字段长度/Unicode/NUL；PATCH 必需 expected_version 及至少一个资料字段；DELETE 使用 expected_version 软归档。

创建者资格按 Organization → Workspace → 成员/账号顺序锁定：企业 Owner 或有效在册成员，并且是有效工作空间 Owner/Member，账号 active；Guest、非成员、退出、停用账号拒绝，不自动加入成员或伪装 Human。复用 channel_ds:create_channel_tx/4 与现有 20 个有效管理频道配额，创建者订阅和管理员关系仍原子写入。

应用 Grant、业务资源、actor_role=application / actor_user_id=NULL 且 detail 含 application_id/correlation_id 的审计，以及幂等快照共用事务。请求沿用规范化 JSON 指纹；响应按首个成功响应原字节重放，且重放前验证当前 Grant。频道归档后仍可由有效 Grant 重放既有结果；撤权后拒绝。父 Workspace 先锁，再锁频道行；版本冲突返回 409。归档 status=0，不删除消息、资料归属或身份关系。

迁移 161 添加 channels:write 枚举及频道治理 version 触发器，覆盖旧用户/Admin 的资料、访问策略、归属、验证标志、状态更新；订阅计数与更新时间等派生更新不增加版本，直接指定 version 也不能覆盖。存在新 scope Grant、Application allowed_scopes 或 version>1 时 down 拒绝，不删除数据。GET 频道列表/详情追加 version。Workspace/Channel 两个实际消费者复用 enterprise_internal_write_handler:write/5，删除旧 Workspace 专属 HTTP 壳；Logic 模块只由代码常量指定。

## 验证

- 8/8 真实 PostgreSQL/Cowboy 门禁，覆盖当前全部 42 个操作 / 28 条路径，重新编译全部当前产品代码，全部实际扩展与全量迁移通过。
- 新路由完整正例：create v1 → update v2 → 两个并发 PATCH 相同 v2 仅一个成功、一个 version_conflict → archive v4。创建与修改时间戳更新，三项同 key 重放原响应，三项缺 scope/key、超长 key、未知字段拒绝，POST/PATCH/DELETE 不同规范化内容冲突。窄 Workspace Grant 三项写拒绝。
- 两个真实并发 POST 使用同 key，仅一个频道、一条审计和一个新执行，另一个携带重放 header，响应完全一致。Guest、removed 企业成员、非工作空间成员及跨企业 creator 拒绝；Organization Owner 创建通过。个人与跨企业频道 PATCH/DELETE 返回同体 404；父空间归档拒绝新建；创建不允许公开 visibility。
- 填满 20 个合成管理频道后应用 POST 返回 resource_conflict，不留下幂等预留；归档保留真实历史消息、创建者订阅及 role=3 管理员。注入 channel 审计 CHECK 拒绝时名称/版本和幂等预留一起回滚；成功资源审计共四条（create/update/并发胜者/archive），重放不重复审计。撤销 channels:write 后原归档 key 重放拒绝。
- 279/279 专项 EUnit，验证既有共享创建、订阅、频道 Logic、scope 及归档写守卫；本轮新增应用写流程由上述真实 HTTP 证明。
- 1/1 PostgreSQL 迁移往返/保留测试；12/12 契约门禁、42 份 Postman 请求、确定性 bundle / aggregate、320 迁移文件 / 160 up-down 组、erlfmt、git diff --check 通过。
- Admin 枚举精确对齐 18 scopes，11/11 Bun 合同测试、typecheck、定向 ESLint 通过；未新增自动授予。
- OpenAPI lint exit 0，保留一项本轮之前已有 identity/mappings PUT 200 的 oneOf 重叠告警；不声称零告警。

初次新 HTTP 用例使用了错误的 removed 状态拼写 left，以及与 Workspace 用例相撞的通用幂等 key；已改为实际枚举和独立业务 key。修复后最终真库/HTTP 日志为 http.txt，未改动服务端来绕过这些失败。单元测试取全部当前共享产品编译产物，新增写逻辑拆分短函数后再次完整编译及 HTTP 验证。

证据：[HTTP / PostgreSQL](evidence/channel-internal-write-2026-10-01/http.txt)、[专项 EUnit](evidence/channel-internal-write-2026-10-01/unit.txt)、[迁移](evidence/channel-internal-write-2026-10-01/migration.txt)、[契约](evidence/channel-internal-write-2026-10-01/contracts.txt)、[来源绑定](evidence/channel-internal-write-2026-10-01/sha256.json)。隔离容器及测试数据库已清理。人工复查认证、授权、SQL 参数化、成员资格、事务/锁顺序、版本与派生更新、错误/回滚、重放和审计；未执行子代理审查。

完整六项目标仍未交付；App 坐席与企业 UX、OA/全部 API 的整体资格及资料存储实证、设备/外部/生产证据等仍需后续验收。未部署、未进行生产迁移，个人导航及历史 E2EE 密文/密钥未改动。

English summary: Internal channel create, profile update and soft archive are implemented with explicit Workspace-scoped channels:write, current Organization/Workspace creator eligibility, existing quota, governance versions, atomic application audit and authorized canonical-request replay. Archive retains history and identity relations. All 42 operations pass real HTTP conformance, 279 focused tests pass, migration preservation and contract checks pass. One pre-existing identity schema warning remains; the complete six-part production objective and device/external proof are unfinished.
