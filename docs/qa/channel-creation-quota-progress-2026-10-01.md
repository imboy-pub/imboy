# 企业频道创建配额并发收口

基线 2466d165。复查频道创建入口发现旧逻辑在事务外取 list_managed 后判断配额，并发可同时通过。删除该完整列表读取及事务外配额判断；在共用 channel_ds:create_channel_tx/4 内，父空间归档守卫之后取得创建者维度 PostgreSQL 事务 advisory 锁，再通过同一连接计数、插入频道及订阅/管理员。channel_repo:count_managed_tx/2 与原 list_managed/1 保持同一集合：所有 scope 下 status=1 且本人有管理员关系的频道；不改为仅本人创建的频道，不计已归档频道。负 UID 锁键与工作空间创建的正 UID 锁键区分。

用户入口继续使用现有 20 上限，Logic 的可信 MaxChannels 传给事务核，直接调用默认 20；该参数不来自用户请求选项，也不持久化到 channel 表。非法非正整数上限拒绝；锁或计数失败回滚，异常查询结果不会作为零。外层保留原中文上限提示。工作空间模板创建仍走其现有模板初始化流程，本次不改变模板配额，也不宣称所有管理员任命均受频道创建配额限制。

同时修复自定义 ID 查询失败后仍尝试创建的问题：查询失败返回原错误，不继续写入；既有数据库唯一约束仍负责并发 ID 唯一性。

## 验证

- 真实数据库，两条不同连接同步发起 personal 频道创建，剩余一个名额时恰好一个成功、一个上限错误；管理频道计数仅加一，成功者的订阅、计数及管理员身份一致。使用 personal 避免父空间行锁替 advisory 锁掩盖并发问题。
- 同事务接口达到上限时稳定返回 channel_creation_limit；非法上限回滚；切换为无 channel SELECT 权限的临时角色后计数失败回滚，没有留下频道或孤儿身份。测试角色与隔离数据库均清理。
- 279/279 专项 EUnit：频道 DS、归档写守卫、频道消息 Logic、scope、频道 Logic。错误路径阻止后续读取，custom ID 查错阻止创建。
- 8/8 PostgreSQL / Cowboy 门禁；全部产品源码当前编译、全部扩展及真实迁移、39 个现有 Internal 操作覆盖通过。
- erlfmt、git diff --check 通过。人工复查所有创建调用方、计数集合、同连接/事务、错误与配额参数来源。未执行子代理审查。

证据：[专项 EUnit](evidence/channel-creation-quota-2026-10-01/unit.txt)、[真实数据库与 HTTP](evidence/channel-creation-quota-2026-10-01/http.txt)、[源码绑定](evidence/channel-creation-quota-2026-10-01/sha256.json)。

本次为企业频道写管理的基础修复；应用创建、修改和归档路由仍需独立 channels:write、创建者成员资格、版本冲突、幂等和应用审计接线。完整六项目标尚未完成；没有部署或生产迁移。

English summary: Channel creation quota is now checked inside the shared transaction after a per-creator advisory lock, using the same active managed-channel set as the old list. Two real concurrent connections with one slot remaining produce exactly one success. Count failure and invalid limits roll back; custom ID lookup errors no longer proceed to creation. All 279 focused tests and eight real database/HTTP tests pass. Internal channel write routes and full production delivery remain unfinished.
