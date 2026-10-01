# 定向邀请接受的锁等待过期与并发 — 2026-10-01

English summary: Both token and targeted invitation acceptance now lock organization before invitation, re-read locked invitation rows, and enforce expiry using the actual database clock when consuming. Concurrent acceptance converges to one membership and an accepted replay.

本轮属于完整六项合同 ORG-01 的局部修复，不将该验收或项目标为完成。源码基础 `e5494ed3c7af06d960832b7779ec5d670f4ed913`；计划及最终源码/日志 SHA256 见 [证据清单](./evidence/organization-targeted-invitation-2026-10-01/sha256.json)。

## 根因与变更

真实合成 PG 基线：请求在邀请有效时进入接受事务，等待邀请行锁；控制事务将截止时间移到请求事务开始之后，等待其过期再释放锁。旧实现仍消费成功并写入成员关系。原过期扫描使用固定的事务起始时间，消费语句也没有期限条件。

复用既有数据库锁和事务：接受及拒绝先共享锁组织，再锁邀请；撤销复用原组织排他锁。邀请查询持行锁，过期扫描与接受 CAS 使用 clock_timestamp()。成员编排仍在同一事务内，失败整体回滚。接受重放仍返回 accepted，不重复触发成员 hook；历史重放不额外限制组织状态。没有新增依赖或迁移。

## 可运行检查与证据

- `IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh` 退出 0；全部当前产品源码编译，完整真实迁移、42 项 Internal 操作及 8 个顶层检查通过。最终目录 `/tmp/imboy-seat-http.avxiAz`。
- 真实 PG 中 token / targeted 两入口分别验证锁等待期间过期，返回错误且无 accepted 状态、无成员落地；用 pg_blocking_pids 确认请求确实等待控制事务。
- 两入口同时接受同一有效邀请：恰一个首次接受、一个 accepted 重放，仅一条 active 成员；两个入口再次重放均返回 already_accepted。测试仅用合成邀请 SQL，不调用会发送通知的创建流程。
- 保留上轮邀请码六个撤销/过期场景及两个入口并发正常加入。定向邀请并发测试清理目标普通成员后再跑邀请码，避免已经加入掩盖其验证。
- `eunit:test([organization_invitation_app_tests,organization_invitation_tests,organization_invite_code_app_tests,organization_join_orchestrator_tests],[verbose])` 退出 0，136 项通过。当前产品来自本轮完整编译，领域测试单独编译；通知替身阻止任何外向触达。领域日志目录见证据文件。
- 手工检查所有受改读取/消费调用、治理锁顺序、accepted 重放、hook 回滚及并发夹具；未运行独立审查代理。erlfmt 与 git diff 检查通过。旧测试文件已超过 800 行，本轮只补齐共享锁 mock，不扩展既有巨型结构。

## 状态与边界

本地定向邀请过期竞态及并发收敛验证通过。合成组织没有默认工作空间，不用本轮结果证明全员群、公告订阅、真实设备或 OA 全旅程。企业完整加入退出 UI、六项验收与投产证据继续推进；未推送、部署、生产迁移或对外通知。
