# T03 剩余 P0-1 落地设计：多端 ACK 按设备送达

> 设计日期：2026-07-02 ｜ 状态：**已实施**（见文末第 9 节实施记录）｜ 关联：00_action_plan 根因 R2、[MSG-P0-1]
> 前置：T03/P0-2（ACK 与 staging 解耦）已完成（commit 78d29ccd）。本文档处理 R2 的另一半。
> **拍板（2026-07-02 用户）：V-A = (b) per-device 各自送达**，按方案 A 实施（实现采用"ACK 标记"变体，见第 9 节）。

## 1. 问题复述（P0-1）

一条 C2C/S2C 消息在 `msg_c2c`/`msg_s2c` 表中按 `(msg_id, to_uid)` 存**单行**代表"待投递给该用户"。
客户端 ACK 走 `msg_operation_ds:ack_c2c_msg/ack_s2c_msg` → `delete_by_msg_id_and_to_id(MsgId, Uid)`
**按用户维度删整行，不分设备（DID）**。

**丢消息路径**：用户双端登录，设备 A 在线收到并 ACK → 删行 → 设备 B 离线；B 重连拉离线消息时行已被删 → **B 永久收不到**。

C2G 侧 `msg_c2g_timeline` 已是 per-uid 行 + `client_ack` boolean 标记（不删本体），是**最安全的现成范本**——但它同样只到 uid 粒度，不分 DID（V7 待核实：是否造成多端未读数串扰）。

## 2. 现状事实（已核对）

| 事实 | 位置 |
|---|---|
| C2C ACK = 按 (msg_id, to_id) 删行 | `msg_operation_ds.erl:82-86` → `msg_c2c_repo:delete_by_msg_id_and_to_id/2` |
| S2C ACK = 同上 | `msg_operation_ds.erl:106-110` → `msg_s2c_repo:delete_by_msg_id_and_to_id/2` |
| C2G ACK = per-uid 标记 client_ack=true（不删本体） | `msg_operation_ds.erl:94-98` → `msg_c2g_timeline_repo:client_ack/2` |
| C2G 离线拉取按 to_uid + client_ack=false | `msg_c2g_timeline_repo.erl:38`（本轮已补 ORDER BY） |
| ACK 已不再触碰 staging（P0-2 已修） | `msg_ack_logic.erl`（78d29ccd） |
| 重试定时器挂在接收方 WS 进程、按 (Uid,DID,MsgId) 缓存 | `message_ds.erl:140-156` |

**关键观察**：投递重试与 ACK 关联已是 **(Uid, DID, MsgId)** 三元组（`websocket_logic:cancel_timer/3`、`imboy_cache` 键），
即**投递层已有 DID 维度**，唯独**离线存储层（msg_c2c/msg_s2c 删行）退化到 uid 粒度**。这是不一致的根源。

## 3. 待核实假设（实施前必须确认）

- **V-A**：多端未读语义——B 设备离线期间 A 已读，B 上线是否应仍收到该消息？（决定"送达"是 per-device 还是 per-user 一次性）
- **V7**：C2G timeline 不分 DID 是否已在生产造成多端未读串扰（消息本体不删，影响限于未读计数）。
- **V-B**：新设备**首次登录**能否拉取到登录前的历史未读？现有 msg_c2c 按 to_id 存，新设备可拉到未删行；若改 per-device 送达表，需定义新设备的"应收集合"起点（否则新设备拉不到任何历史，或拉到全部历史）。
- **V-C**：`msg_c2c`/`msg_s2c` 表精确 DDL（列、唯一约束、索引），迁移前 `\d msg_c2c` 核对。

## 4. 方案对比

### 方案 A（推荐）：主体不删 + per-device 送达标记表

新增 `msg_delivery` 表跟踪"每消息每设备"送达状态；`msg_c2c`/`msg_s2c` 主行 ACK 时**不再删**，改由归档/TTL 清理。

```sql
-- 草案，DDL 以 V-C 核对后为准
CREATE TABLE public.msg_delivery (
    msg_id      varchar(40) NOT NULL,
    to_uid      bigint      NOT NULL,
    to_did      varchar(64) NOT NULL,
    msg_kind    varchar(8)  NOT NULL,   -- 'c2c' | 's2c'
    acked_at    timestamptz,            -- NULL=未确认
    created_at  timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (msg_id, to_uid, to_did, msg_kind)
);
CREATE INDEX idx_msg_delivery_pending
    ON public.msg_delivery (to_uid, to_did) WHERE acked_at IS NULL;
```

- 投递时：对接收方**当前活跃设备集**各插一行（acked_at=NULL）；
- ACK 时：`UPDATE ... SET acked_at=now() WHERE msg_id=$1 AND to_uid=$2 AND to_did=$3`（per-device）；
- 离线拉取：`msg_c2c JOIN msg_delivery` 查 `(to_uid,to_did) 且 acked_at IS NULL`；
- 主行清理：`msg_delivery` 全设备 acked 后（或 TTL）由 worker/归档清理，不在 ACK 路径删。

**优点**：与投递层已有的 (Uid,DID,MsgId) 三元组一致；C2C/S2C 统一；与 C2G timeline 模型同构（可后续收敛）。
**代价**：新表 + 迁移；投递路径要拿到"当前活跃设备集"（`user_device_logic:online_dids/1` 已有，但离线设备/未来设备需 V-B 决策）；离线拉取 SQL 改 JOIN。

### 方案 B（最小侵入）：主表加 to_did 列 + 复合删除键

`msg_c2c`/`msg_s2c` 加 `to_did` 列，投递时按设备各存一行，ACK 按 `(msg_id, to_uid, to_did)` 删。

**优点**：不新增表，改动集中。
**缺点**：主表行数 ×设备数膨胀（payload 冗余存储）；E2EE payload 已较大，冗余成本高；与 C2G timeline 模型不一致。

### 方案 C（伪修复，不推荐）：删行前确认所有活跃设备已收

ACK 时查该 uid 所有在线设备是否都 ACK，全 ACK 才删。**离线设备**永远不在"在线集"里 → 仍丢，且并发复杂。仅列作反面。

## 5. 推荐

**方案 A**，但**强前置 V-A/V-B 产品语义拍板**：
- 若 V-A =「per-user 一次性」（任一设备读即所有设备视为已读）→ 现状 uid 粒度删行其实符合，P0-1 降级为"多端体验问题"而非丢消息，可只补 C2G 一致性；
- 若 V-A =「per-device 各自送达」→ 方案 A 是正解。

> 这是**产品语义决策**，不是纯技术问题。实施 T03 P0-1 前必须先定 V-A。

## 6. 改造点清单（方案 A，供实施会话）

| 层 | 文件 | 改动 |
|---|---|---|
| 迁移 | `priv/migrations/` 新增时间戳迁移 | 建 `msg_delivery` 表 + 索引（8 位序号规则见记忆） |
| Repo | 新增 `msg_delivery_repo` | insert_batch / mark_acked(msg_id,uid,did,kind) / list_pending(uid,did) / cleanup |
| DS | `msg_operation_ds:ack_c2c_msg/ack_s2c_msg` | 改签名带 DID → mark_acked（不删主行）；调用方 `msg_ack_logic` 传 DID（现签名已有 `_DID`，去掉下划线接入） |
| DS | `msg_c2c_ds:read_msg` / S2C 离线读取 | 改 JOIN msg_delivery 过滤该设备未 acked |
| Logic | C2C/S2C 投递路径 | 投递时对接收方设备集写 msg_delivery 行 |
| 清理 | worker 或独立清理 | 全设备 acked / TTL 后清主行与 delivery 行 |
| 测试 | 新增 | 双端场景：A ACK 后 B 仍能拉到；B ACK 后主行可清 |

## 7. 兼容性

- **协议不变**：CLIENT_ACK 线上格式 `CLIENT_ACK,type,msgid,did` 已含 did，无需改客户端。
- **迁移期**：旧 msg_c2c 存量行无 delivery 记录 → 离线读取需 fallback（无 delivery 行时按旧 uid 语义），迁移脚本可为存量行补 delivery（按 to_id + 该 uid 已知设备）或定义"迁移点之后才启用 per-device"。
- **回滚**：down 迁移删表；ack_* 恢复删行逻辑（保留旧函数分支）。

## 8. 风险

- 设备集动态性（V-B）是最大不确定点：新设备如何界定"应收历史"。建议 MVP 先只对**消息创建时刻在线 + 已注册**的设备写 delivery，历史补拉另设机制。
- 行数膨胀：msg_delivery 行数 = 消息数 × 设备数，需 TTL/归档控制（C2G timeline 已有 30 天 retention 可参照）。

## 9. 实施记录（2026-07-02，方案 A 的"ACK 标记"变体）

### 9.1 与第 4 节草案的差异及理由

实施保留了方案 A 的核心（新建 `msg_delivery` 表、per-device 语义、主行延迟清理、读路径反连接），
但把行的写入时机从**投递时**反转为 **ACK 时**：

- **行语义反转**：`msg_delivery` 一行 = "该设备已确认"（草案是 "待投递 + acked_at 标记"）。
  **无行 = 未确认**。
- **为什么**：草案在投递时只给"当前活跃设备集"写行，发送时刻离线的设备**没有行**，
  按 `acked_at IS NULL` JOIN 拉取会拉不到 → V-B 的洞仍在（草案第 8 节自认"最大不确定点"）。
  反转后离线设备/新设备天然无标记 → 仍能拉到未清理的主行，V-B 消解；且投递路径零改动（省掉草案第 6 节最大改造项）。

### 9.2 核实结论

- **V-C**：`msg_c2c`/`msg_s2c` 均为 TimescaleDB hypertable，PK `(id, created_at)`，
  接收人列均为 `to_id`，压缩 3 天 / 保留 1 年（`00000005`/`00000008` 迁移）。
- **V-B（新设备语义，实施后）**：新设备注册后可拉到**尚未被全端确认清理**的存量主行（无标记即未确认）；
  已清理的消息走 `msg_archive` history API（生产归档已开启）。新设备注册后即加入活跃设备集，
  后续消息清理需等它确认（受活跃窗口约束）。
- **活跃窗口**：`msg_delivery_active_days`（默认 30 天）。超窗未活跃设备不阻塞主行清理，
  即按设备离线消息保留期 = 30 天（与 C2G timeline retention 对齐）。

### 9.3 实际改造点

| 层 | 文件 | 改动 |
|---|---|---|
| 迁移 | `priv/migrations/00000019_msg_delivery.{up,down}.sql` | 建表 PK `(msg_kind,msg_id,to_uid,to_did)` + `(to_uid,to_did)` 索引 |
| Repo | `msg_delivery_repo`（新） | `mark_acked[_batch]`（幂等 upsert）/ `delete_delivered[_batch]`（全端确认→删主行+清标记，`make_interval` 活跃窗口）/ `pending_filter`（反连接片段） |
| Repo | `msg_c2c_repo` | `count_unread_since/3`（带 DID 反连接，保证 has_more 与读一致，防空拉循环） |
| DS | `msg_operation_ds` | `ack_c2c_msg/3`、`ack_s2c_msg/3`、`ack_{c2c,s2c}_batch/3`、`maybe_clean_delivered/3`；DID 空 → legacy 按 uid 删行 |
| DS | `msg_c2c_ds`/`msg_s2c_ds` | `read_msg_for_device/4`、`count_unread_since/3`/`count_since/3` |
| DS | `message_ds` | `check_and_notify_offline_msgs/2`（按设备读 + `send_next/6` 白名单定向推送） |
| DS | `msg_store_worker` | c2c/s2c 落库后 `maybe_clean_delivered`（关掉"ACK 先于落库→主行永不清理"竞态） |
| Logic | `msg_ack_logic` | `client_ack/4` 的 DID 接入（原 `_DID` 忽略） |
| Logic | `messaging_logic` | REST `/offline`、`offline_ack` 可选 `did` 参数（带 did 走按设备；缺省 legacy） |
| Logic | `user_server`/`passport_logic` | 3 处 reconnect 调用点透传 DID |

### 9.4 兼容性与遗留

- 线上 CLIENT_ACK 格式不变（`CLIENT_ACK,type,msgid,did` 本就带 did），WS 路径立即生效。
- **跨仓跟进（imboyapp）**：REST `/offline` 与 `offline_ack` 需带 `did` 参数才获得按设备语义；
  未带时保持旧 per-uid 删行（存在多端丢消息风险，与现状相同、不更差）。
- **C2G 未动**：timeline 仍 per-uid `client_ack`（V7 多端未读串扰随后续任务处理，可复用本表 kind='c2g'）。
- 孤儿标记（ACK 后主行始终未落库的错误路径）体量 ≈ staging 失败率，暂不专设清理；
  `delete_delivered_batch` 清标记时会顺带清除同批消息的历史遗留孤儿标记。
