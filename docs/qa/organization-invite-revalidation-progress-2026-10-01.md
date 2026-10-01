# 企业邀请码持锁重核验 — 2026-10-01

English summary: Joining and previewing an organization now revalidate an invite after obtaining the organization lock and retain a shared invite-row lock through the transaction. Revoked or newly expired invites cannot reuse stale authorization; concurrent joins converge to one membership.

## 范围与根因

属于六项合同 ORG-01 的本地局部闭环；计划 SHA256 `7331846159fc0ab61a1a8be248c52e9a8d81581a3fa7a01c9fb0a49723a98599`。源码基础 HEAD `835cb67bf40bbf94bc0cfe09b132e6d5ab219bf3`，最终变更源码与日志 SHA256 见 [证据清单](./evidence/organization-invite-revalidation-2026-10-01/sha256.json)。不据此将 ORG-01 或六项目标标为整体完成。

原实现先读有效邀请码，再等待组织锁，锁获得后使用旧角色与有效状态。合成数据库复现：控制事务持组织排他锁；加入请求读取有效码并确实等待该锁；控制事务撤销码后提交，旧加入请求仍成功落成员。预览与单码加入有同样顺序。

复用已有组织共享锁和加入编排；三入口统一重读并共享锁定邀请码行，保持组织 → 码 → 工作空间 / 成员的锁序。无效码 981、过期码 982、原有组织生命周期和角色冲突语义保留。过期读取使用实际 clock_timestamp，而非事务开始时固定的 CURRENT_TIMESTAMP。预览 INFO 日志不再包含可换取加入权限的邀请码。

## 检查与恢复

- 真实 PG：三个入口分别验证撤销与自然过期（6 个场景）。用 pg_blocking_pids 确认正在等待组织锁；过期点在请求事务开始之后、释放锁之前，不能用事务起始时间蒙混。每次拒绝后确认目标没有成员关系。
- 并发正常加入：两个入口同时等待同一组织锁，释放后均成功；仅一条 active membership；再重放返回 unchanged。无默认工作空间的合成组织，不声称本轮证明全员群、公告订阅等全部落地。
- `IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`：退出 0，当前全部产品源码重编、完整真实迁移与 HTTP 检查通过；8 个顶层 EUnit 检查，不等同 8 个端点。
- 当前源码领域套件 `eunit:test([organization_invite_code_app_tests,organization_join_orchestrator_tests],[verbose])`：退出 0，56 项通过。对应 mock 补齐持锁重读；角色预览的两次读取使用一致合成行。独立临时编译与日志目录 `/var/folders/8m/wbjj0qmn4ml0mgn56wm56j2m0000gn/T/gz-invite-unit-szgbbzb4`。
- 初期测试夹具清理误删 Owner 行触发治理保护，修正为删除本轮合成组织并依赖级联；不得用该夹具失败当业务基线。一次并发观察占用池连接引发 no_connection，改为用控制事务连接观察锁，不放宽产品连接池。
- 最终 PG 证据 `/tmp/imboy-seat-http.f9qw3c`；已在仓内保存实际业务失败基线、最终 PG HTTP 与领域日志。一次性测试容器由退出清理移除，未操作共享数据库。
- 手工检查全部邀请码读取调用、治理锁序、返回角色与日志；未启动独立审查代理。格式与 diff 检查通过。

## 状态

本地邀请码并发与撤销缺口关闭；UI 加入/退出全旅程、真实 OA 与设备/生产证明仍按完整合同推进。未推送、部署、生产迁移或对外通知。
