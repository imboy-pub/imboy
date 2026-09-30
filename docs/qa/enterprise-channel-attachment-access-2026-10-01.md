# 企业频道附件撤权 / Enterprise Channel attachment revocation

日期 / Date: 2026-10-01。状态 / Status: LOCAL_FOCUSED_PASS，整体目标尚未完成。

## 修复 / Change

`attach_logic` 的频道附件共享授权函数先读取真实父级范围资格，再按原频道角色、订阅或付费订单裁决。上传 presign、confirm 和下载 view_url 均调用此函数；暂停、移除或缺少企业资格不能借旧频道管理员或订阅关系绕过。查询经 DS → Repo，不缓存资格，也不改变频道的订阅/付费模型。
The shared Channel attachment predicate now checks current parent eligibility before existing role, subscription or purchase entitlement. Upload presign, confirmation and download authorization all use it. Stale Channel roles or subscriptions cannot bypass suspended, removed or missing Organization membership. Parent eligibility follows DS → Repo and is not cached.

群和频道复用上轮同一段范围 SQL：个人群/频道不需要企业身份；个人工作区需要工作区成员；企业工作区要求有效企业成员与有效企业，再验证工作区成员或企业治理资格。频道本身须 active。此父级检查不授予任何订阅、订单或频道角色。
Group and Channel attachment checks reuse one scope SQL predicate. Personal resources retain their identity model. Personal Workspaces require Workspace membership; enterprise Workspaces additionally require an active Organization and active Organization membership. Channel status must be active. Parent eligibility alone grants no Channel entitlement.

## 验证 / Validation

- 修复前的新回归实际得到上传 URL，本应返回 forbidden；日志 `/tmp/gz-channel-attachment-red.log`。
- `eunit:test([attachment_repo_tests,attach_logic_tests,enterprise_channel_attachment_tests],[verbose])`：58/58 PASS，退出码 0；日志 `/tmp/gz-channel-attachment-unit.log`。覆盖失权后三条附件路径拒绝、零签名/零 HEAD/零角色查询、有效频道管理员仍可读、订阅和付费权益、查询失败与非法 ID 拒绝。
- `enterprise_group_attachment_pg_tests:run(SocketPath)`：2/2 PostgreSQL 场景 PASS；日志 `/tmp/gz-channel-attachment-pg.log`。实际执行生产 SQL，覆盖群附件历史边界和资料保留，以及频道暂停/移除/无企业身份/工作区失权/其他企业/个人域/治理者/企业归档/无效频道。
- 所有变更源码和测试编译通过；正常提交 hooks 校验格式与 secrets。

仅使用新建的本地 Unix socket 合成库，无真实用户、生产数据库、外部 OA 或对象存储请求。最小表和 mock 签名不代表完整迁移、HTTP/JWT、存储签名、真机与生产验收。
Only fresh task-local synthetic PostgreSQL data and mocked signing are used. This does not prove full migrations, HTTP/JWT, storage signing, device or production acceptance.

## 剩余范围 / Remaining scope

本次拒绝资格失效后的新请求；父级查询与后续权益查询/签名不是同一事务，不能宣称已证明并发暂停与签名线性一致。confirm 的最终写事务仍需检查父级资格，尤其外部 HEAD 在预检后返回的竞态。已签发的 GET/PUT 链接仍按原到期时间有效，尚未完成实时撤销；群上传/确认也需要同样核对。频道正文、评论等共享入口的企业授权与缓存仍须继续检查。
This patch rejects subsequent requests after qualification changes. Parent checks, entitlement reads and signing are not one transaction, so linearizable revocation is not established. Confirmation still needs parent eligibility in its final write transaction, including a HEAD-after-precheck race. Existing signed URLs remain valid until expiry. Group upload/confirmation and Channel content/comment boundaries remain to be verified.
