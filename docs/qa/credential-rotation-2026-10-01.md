# 凭证轮换的单次使用与并发闭环

状态：本地专项 PASS；六项目标仍为 PARTIAL，未执行生产操作。

历史真实红证据 `lG157Y` exit 1：第一次轮换撤销旧凭证后，同一个旧凭证仍能再次轮换；第二次返回成功，持久化记录改变，审计从 1 变为 2。原因在共享 enterprise_internal_ops：只读取旧凭证的 application_id；创建新凭证后把撤销旧凭证的 not_active 当作成功。Admin 与运维复用该路径。

现在沿现有 Application → Credential 锁顺序读取当前状态，只有 active 旧凭证可进入签新撤旧流程。重复请求返回 not_active；签新后撤旧发生任何错误均触发整个事务回滚，不返回新凭证。复用现有应用锁和凭证仓储，没有依赖、迁移或客户端契约新增。

真实独立 PostgreSQL / 完整迁移 / fresh compile `nqPHoJ` exit 0：

- Admin 与运维均验证第一次轮换成功，第二次明确拒绝，不产生新行；Admin 只留一条审计。
- 用 marker 独立连接持有 Application 行锁，数据库活动查询实际观察到两名轮换调用者均在等待锁，再放行；Admin 和运维两类各恰有一个成功、一个 not_active。数据库共旧/新两条凭证；Admin 为一条审计。
- 原生 BEFORE UPDATE 触发器阻止旧凭证撤销，实际运维轮换返回错误，完整行状态与之前相同，未留下新凭证；移除故障后重试成功。
- 既有 10 类治理写入 × 2 类审计故障、组合修改回滚、并发 CAS、租户拒绝检查仍通过。
- 相同产品源码的现有 Internal HTTP 门禁 `Ia87kV` exit 0，26 项通过；之后仅调整上述 native 专项检查，不据此宣称新增专项由 HTTP 门禁执行。

验证过程中 `Z8Mlsf` 的退出码虽为 0，但活动快照未刷新且池化包装吞掉观察失败，明确不作为通过证据。`Qui1AH` 已使观察失败导致 exit 1；持锁改用独立 marker 连接，避免占用调用者池名额，并刷新数据库活动快照后才获得 `nqPHoJ` 的严格通过。一次中间编译失败也不作为通过证据。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ADMIN_GOVERNANCE_PG_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
```

归档：`evidence/credential-rotation-2026-10-01` 为最终源码 SHA、实际断言结果及日志；`evidence/credential-rotation-red-2026-10-01` 为原始红检查与安全响应标志。原红断言日志可能序列化返回的合成明文，故不归档该日志或崩溃转储；持续检查改为布尔断言避免打印明文。

未验证本专项的 Admin HTTP/页面、真实 OA 与 App 真机；整体投产资格仍需完整验收。
