# P0 决策文档：group.user_id_sum int8 溢出

> 状态：待拍板 | 提出日期：2026-08-29 | 来源：套件隔离治理（eunit 全量）+ 生产快照实证
> 关联：stress ×3 + db_query_performance_tests（cancelled 形态）；生产"88 人群已冻结、加群 500"

---

## 1. 问题

`"group".user_id_sum`（**bigint**，DEFAULT 0 NOT NULL）= 群成员 `user_id`（TSID）的算术和。
成员 TSID ≈ 1.09e17，int8 上限 9.223e18 → **约 85 个成员即溢出**。

溢出点不在 PG 的 `SUM(bigint)`（返回 numeric，不溢），而在**写回**：
`group_member_ds:update_statistics/2`（join/leave 都会执行）把 SUM 结果作为
**绑定参数**写回 `UPDATE "group" SET user_id_sum = $huge` → epgsql 绑定阶段
`{integer_overflow,int8,...}` → **连接进程崩溃** → 事务 ROLLBACK 报 noproc → 加群/退群 500。

生产快照实证：88 人群的 user_id_sum 已无法更新，该群"冻结"（无法加减成员）。

## 2. user_id_sum 的真实用途（决定修法的关键）

| 场景 | 位置 | 语义 |
|---|---|---|
| **建群幂等签名** | `group_logic:add/4` → `group_ds:find_by_creator_and_sum/2`：`WHERE creator_uid=$1 AND user_id_sum=$2` | 客户端"选人建群"重试去重：同一创建者 + 同一成员集合 → 返回已有 Gid |
| 维护写入 | `update_statistics/2`（join/leave）、建群时=creator、workspace 默认群 | 保持签名与成员集合同步 |
| **客户端载荷** | 群成员 join/leave 系统通知、入群 API 响应、face2face payload | **imboyapp 6 处消费**：存入本地 SQLite + 日志；未发现按它做逻辑查询（弱依赖，读端有 `?? 0` 兜底） |
| 索引 | `i_creatorid_memberidsum (creator_uid, user_id_sum)` | 服务幂等查询 |

**数学缺陷**：SUM 作集合签名本身就不严谨（不同成员集合可碰撞），溢出只是它的急性发作。

## 3. 三个修法

### A. numeric 迁移
`ALTER TABLE "group" ALTER COLUMN user_id_sum TYPE numeric;`
- ✅ 语义/客户端契约零变化；改动面最小。
- ❌ 表重写锁窗口（生产 group 表规模待查）；epgsql numeric 解码回读值变为
  decimal/binary，`ec_cnv:to_integer` 路径需回归；索引重建；**保留了数学上就弱的签名**。

### B. 弃用写路径 + 幂等改集合等价查询（目标态，推荐）
1. `group_logic:add/4` 幂等查询改为集合等价（对 group_member 做
   `GROUP BY group_id HAVING COUNT(*)=N AND COUNT(*) FILTER (WHERE user_id = ALL($uids))=N`
   类查询，或建群时写入成员集 hash 列）。
2. `update_statistics` **停止写 user_id_sum**（保留 member_count）；
   建群/workspace 默认群停写。
3. 通知/响应 payload 的 `user_id_sum` 字段：客户端为弱依赖（`?? 0` 兜底），
   可置 0 过渡、下个版本随客户端移除。
4. 观察一个版本后 `DROP COLUMN user_id_sum` + 删索引。
- ✅ 根治溢出（不再写 SUM）；幂等语义变正确（集合等价无碰撞）；无表重写；
  88 人群自动解冻。❌ 改动面最大，需服务端+客户端两个发布周期。

### C. 饱和和（立即止血）
所有写点与 `find_by_creator_and_sum` 的查询参数统一 `min(Sum, 9_207_000_000_000_000_000)`。
- ✅ 一处小改立即消除 500/冻结；无迁移。❌ 大群幂等签名失真（同创建者多个
  大群互相碰撞 → 幂等误判返回错误 Gid）；payload 值失真。

## 4. 建议

**C 先止血（当天可上）+ B 分两个版本做目标态**；A 不推荐（重写代价 + 保留弱签名）。
止血后 4 个受影响测试（stress ×3 + db_query_perf）自动转绿；B 完成后测试断言
需同步把 101 成员群的溢出场景改为集合语义断言。

## 5. 拍板问题

1. 是否授权 C 止血（改 `group_member_ds:update_statistics/2` + `group_logic:add/4`，产品代码）？
2. 目标态选 B 还是 A？
3. 若选 B：payload 字段过渡策略（置 0 / 保留读旧值）需要与移动端确认发布顺序。
