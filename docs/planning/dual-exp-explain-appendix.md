# 附录：R2 迁移草稿 EXPLAIN (ANALYZE, BUFFERS) 原始输出

> 归属：`dual-exp-decision-brief.md` R2 节。执行时间 2026-08-26。
> 环境：本地 Docker PG 18.1（aarch64 Debian，端口 4323，库 `imboy_v1`）。
> 方法：**会话级 TEMP 表沙箱**——`CREATE TEMP TABLE *_sim AS SELECT * FROM <原表>`，在临时表上加草稿列/约束/索引并注入合成数据后 EXPLAIN。全程零持久化写入（会话结束临时表自动销毁），真实业务表未被修改。
> 规模声明：本地真实数据 channel=26 行 / "group"=515 行；合成放大至 ~10 万行以验证索引选择。**本地规模非生产规模**，生产规模结论 BLOCKED（见 R2 主文）。

## 1. 沙箱 DDL（T3 草稿等价物）

channel_sim 与 group_sim 同款：

```sql
CREATE TEMP TABLE channel_sim AS SELECT * FROM channel;
ALTER TABLE channel_sim ADD COLUMN scope text NOT NULL DEFAULT 'personal';
ALTER TABLE channel_sim ADD COLUMN workspace_id bigint;
ALTER TABLE channel_sim ADD CONSTRAINT chk_channel_scope_xor CHECK (
  (scope='personal' AND workspace_id IS NULL) OR
  (scope='workspace' AND workspace_id IS NOT NULL));
-- I2 回填等价：DEFAULT 'personal' + workspace_id 可空 → 存量行自动满足 personal/NULL
CREATE INDEX i_channel_sim_personal ON channel_sim (creator_uid, created_at DESC)
  WHERE scope='personal' AND status=1;
CREATE INDEX i_channel_sim_ws ON channel_sim (workspace_id, created_at DESC)
  WHERE scope='workspace' AND status=1;
CREATE UNIQUE INDEX i_channel_sim_pkey ON channel_sim (id);
-- 合成数据：channel 100_000 行（1/4 workspace、workspace_id∈[500,519]）；
-- group 100_000 行（1/3 workspace）。ANALYZE 后执行。
```

## 2. channel_sim 结果

### Q1 个人频道列表（creator 视角）

```text
EXPLAIN (ANALYZE, BUFFERS) SELECT id, name, created_at FROM channel_sim
WHERE creator_uid=1001 AND status=1 AND scope='personal'
ORDER BY created_at DESC LIMIT 20;

 Limit  (cost=0.42..49.51 rows=20 width=20) (actual time=0.181..0.299 rows=20.00 loops=1)
   Buffers: shared hit=3, local read=15
   ->  Index Scan using i_channel_sim_personal on channel_sim
         (cost=0.42..3746.11 rows=1526 width=20) (actual time=0.181..0.297 rows=20.00 loops=1)
         Index Cond: (creator_uid = 1001)
         Buffers: shared hit=3, local read=15
 Planning Time: 0.284 ms
 Execution Time: 0.312 ms
```

### Q2 Workspace 频道列表（workspace_id=500，含数据）

```text
EXPLAIN (ANALYZE, BUFFERS) SELECT id, name, created_at FROM channel_sim
WHERE scope='workspace' AND workspace_id=500 AND status=1
ORDER BY created_at DESC LIMIT 50;

 Limit  (cost=0.29..129.05 rows=50 width=20) (actual time=0.606..2.294 rows=50.00 loops=1)
   Buffers: shared hit=3, local hit=1 read=13 written=13
   ->  Index Scan using i_channel_sim_ws on channel_sim
         (cost=0.29..3242.54 rows=1259 width=20) (actual time=0.605..2.288 rows=50.00 loops=1)
         Index Cond: (workspace_id = 500)
         Buffers: shared hit=3, local hit=1 read=13 written=13
 Planning Time: 3.945 ms
 Execution Time: 2.310 ms
```

（首轮以 workspace_id=501 试跑因合成取模分布恰好无行，返回 0 行仍走索引：`Index Scan using i_channel_sim_ws ... Execution Time: 0.076 ms`。）

### Q3 按 ID 直访

```text
EXPLAIN (ANALYZE, BUFFERS) SELECT id, name, scope, workspace_id FROM channel_sim WHERE id=99999;

 Index Scan using i_channel_sim_pkey on channel_sim
   (cost=0.29..8.31 rows=1 width=29) (actual time=0.077..0.077 rows=1.00 loops=1)
   Index Cond: (id = 99999)
   Buffers: local hit=1 read=2
 Planning Time: 0.029 ms
 Execution Time: 0.081 ms
```

### Q4 XOR CHECK 正向违规（workspace 且 NULL）被拒绝

```text
INSERT INTO channel_sim (..., scope, workspace_id) VALUES (..., 'workspace', NULL);
ERROR:  new row for relation "channel_sim" violates check constraint "chk_channel_scope_xor"
```

## 3. group_sim 结果（100_416 行）

### Q1 个人群列表

```text
 Index Scan using i_group_sim_personal on group_sim
   (cost=0.42..3571.74 rows=1339 width=20) (actual time=0.112..0.164 rows=20.00 loops=1)
   Index Cond: (owner_uid = 1001)
   Buffers: local read=19
 Planning Time: 0.713 ms
 Execution Time: 0.177 ms
```

### Q2 Workspace 群列表

```text
 Index Scan using i_group_sim_ws on group_sim
   (cost=0.29..1839.12 rows=552 width=20) (actual time=0.085..0.203 rows=50.00 loops=1)
   Index Cond: (workspace_id = 500)
   Buffers: local hit=16 read=24
 Planning Time: 0.144 ms
 Execution Time: 0.216 ms
```

### Q3 按 ID 直访

```text
 Index Scan using i_group_sim_pkey on group_sim
   (cost=0.42..8.44 rows=1 width=29) (actual time=0.022..0.022 rows=1.00 loops=1)
   Index Cond: (id = 99999)
   Buffers: local hit=1 read=3
 Planning Time: 0.066 ms
 Execution Time: 0.030 ms
```

### Q4 双向 XOR 反向违规（personal 带 workspace_id）被拒绝

```text
INSERT INTO group_sim (..., scope, workspace_id) VALUES (..., 'personal', 500);
ERROR:  new row for relation "group_sim" violates check constraint "chk_group_scope_xor"
```

## 4. 原始执行记录

- 第一次运行（channel Q1–Q4）：`/tmp/wp0_explain_run1.log`（含 psql 回显，未归档）
- 第二次运行（channel Q2 补跑 + group Q1–Q4）：`/tmp/wp0_explain_run2.log`（同上）
