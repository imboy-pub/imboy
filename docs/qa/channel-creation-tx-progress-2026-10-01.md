# 企业频道创建事务核进度

基线 95115377。频道创建仍经过既有用户端成员授权及配额检查。channel_ds:create_channel/3 复用新增 create_channel_tx/4；后者使用调用方连接，不另开事务，方便后续应用审计与幂等结果一起提交。已删除重复内嵌事务代码；保留工作空间归档行锁、可选字段处理、创建者订阅及计数与 role=3 管理员身份、稳定错误码。

真实 PostgreSQL 检查：成功提交后三项身份/计数一致；同事务创建成功后上层 abort_tx 使频道、订阅及管理员关系全部回滚；管理员 CHECK 拒绝时无半成品或孤儿关系；父工作空间归档时返回 980，拒绝创建且该测试的父状态修改也回滚。已接入现有一次性数据库门禁，所有扩展和全量迁移正常，39 个现有 Internal 操作的真实 HTTP 检查保持通过，8/8 测试通过。

专项 EUnit 59/59 通过，覆盖频道订阅及创建和归档写守卫。首次运行借用了 main 的旧编译产物，两个未修改的群文件上传测试失败；使用本轮全部当前源码编译产物重新运行后 59/59，通过结果保存为 unit.txt。没有以失败结果声称通过，也没有为绕过失败改动群文件逻辑。erlfmt 与 git diff --check 通过。人工复查调用路径及事务行为，未执行子代理审查。

证据：[HTTP / PostgreSQL](evidence/channel-creation-tx-2026-10-01/http.txt)、[当前源码 EUnit](evidence/channel-creation-tx-2026-10-01/unit.txt)、[来源哈希](evidence/channel-creation-tx-2026-10-01/sha256.json)。隔离容器已自动清理，原有数据库未修改。

企业频道应用写路由尚未接线；仍需 channels:write、创建者资格与配额、版本冲突、幂等及同事务应用审计。该事务核只供已授权入口调用。六项目标尚未全部交付；未部署、未执行生产迁移。

English summary: Channel creation now offers a caller-owned transaction core reused by the existing entry point. Creator subscription, subscriber count and administrator identity remain atomic. Real PostgreSQL tests prove outer rollback, administrator failure rollback and archived-parent rejection; eight database/HTTP tests and 59 focused tests pass against current source. Internal channel write routes and full production delivery remain unfinished.
