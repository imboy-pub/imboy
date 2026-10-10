# G-QR：QR 登录一次性消费原子迁移独立方案（BLOCKED）

> 状态：**G-QR_BLOCKED**（须 Schema 迁移，超出 CACHE_V2_2_1_W1_W5_LOCAL_IMPLEMENTATION 写入范围）
> 本文档为计划 19.2 要求的独立方案，不是已完成实现。QR 两类 Key（`{qr_login,_}`、`{qr_login_qr,_}`）
> 在 `imboy_cache_policy` 中标记 `blocked`，永不进入 V2 replicate；全局 ENABLE_V2 与整体安全 PASS
> 被 G-QR 阻塞（本文件不解除）。

## 1. 现状取证（2026-10-11，候选 6a28632f 的基线代码）

- 会话与 QR 映射为纯 depcache 内存态，**无 PostgreSQL 权威表**：
  - `src/api/qr_login_handler.erl:438` `{qr_login_qr, QRToken}` → SessionToken（60s）
  - `src/api/qr_login_handler.erl:461` `{qr_login, SessionToken}` → 会话 map（含 confirm 后的 `login_token`）
- **status 一次性消费非原子**（W1-06 竞态）：`handle_status`（:123-166）为
  `get_session`（depcache:get）→ 读 token → `delete_session`（depcache:flush/1）三步两次独立 ETS 操作，
  并发 status 请求可同读 confirmed 会话、各自取得 `login_token` 后才互删（第二次 delete 为 no-op）。
- **SSE 与 status 消费语义不一致**：confirm 成功路径（:351-368）写回 confirmed 会话后经
  `qr_login_event_ds:notify/2`（syn:publish，`src/ds/qr_login_event_ds.erl:123`）把 `login_token`
  推送给 SSE 订阅者——SSE 订阅者取得 token 不经过 status 的删除；SSE 已收走 token 而会话仍在，
  status 轮询者可再次取得同一 token。
- **敏感值经广播复制**：confirm 后 `cache_session` → `imboy_cache:set/4` 在 `dsync_enabled=true`
  时对全集群广播含 `login_token` 的会话值（本轮 W2 已将 `qr_login` 归类 blocked，set 广播已停；
  但这只是缓存层的止血，不是原子消费修复）。
- 扫码/确认的 get→改→set 亦无版本/CAS（多窗口竞态），过期与重试语义分散在 handler 各分支。

## 2. 满足 G-QR 的目标合同（计划 19.2）

同一 PostgreSQL authority、原子 claim/consume、token 可见性边界、过期、重试幂等、
status 与 SSE 同一授权/消费语义、双读竞争 oracle。

## 3. 独立方案（需另行授权的 Schema 迁移）

新增表（示意，最终以迁移评审为准）：

```sql
CREATE TABLE public.qr_login_session (
    session_token  bytea PRIMARY KEY,           -- 随机 32B，不做主键索引以外的索引
    qr_token       bytea NOT NULL UNIQUE,       -- 反查映射（替代 {qr_login_qr,_}）
    status         text NOT NULL DEFAULT 'waiting',   -- waiting|scanned|confirmed|cancelled
    scanned_by     bigint,
    device_id      text, device_name text, platform text,
    login_token    bytea,                       -- 仅 confirmed 后写入；取出即置 NULL
    consumed_at    timestamptz,                 -- 原子消费时间
    expires_at     timestamptz NOT NULL,
    created_at     timestamptz NOT NULL DEFAULT now()
);
```

关键原子操作（epgsql，`DELETE ... RETURNING` / `UPDATE ... WHERE` 单语句原子性）：

1. **status 一次性领取**（双读竞争 oracle：两个并发请求只有一个拿到 token）：

```sql
UPDATE public.qr_login_session
   SET login_token = NULL, consumed_at = now()
 WHERE session_token = $1 AND status = 'confirmed'
   AND login_token IS NOT NULL AND consumed_at IS NULL
   AND expires_at > now()
RETURNING login_token, scanned_by;
```

   0 行返回 = 已消费/未确认/过期；非竞争窗口内幂等重试返回一致的"已使用"错误码（现有
   `?ERR_QR_LOGIN_ALREADY_USED` 语义保持）。

2. **SSE 改为"可领取"信号**：confirm 成功后 syn 推送不含 token 的 `confirmed_ready` 事件；
   Web 端落地 token 一律走上述原子领取（SSE/status 同一消费语义）。
3. **scan/confirm 状态机**：`UPDATE ... SET status='scanned', scanned_by=$u WHERE session_token=$1
   AND status='waiting' AND expires_at>now() RETURNING ...`——CAS 语义替代 get→改→set；
   权限分支（scanned_by=0/=uid）保持现错误码。
4. 过期清理：领取/状态机语句内联 `expires_at > now()` 判断；另加定时清扫兜底（复用现有
   清理 worker 机制另评审）。

## 4. 为什么必须 Schema（不能只改缓存层）

原子 claim/consume 需要跨请求的持久判定点。depcache 的 `get`/`flush` 是两次独立 ETS 操作，
不提供原子取出；在缓存上自建锁/代次只是把竞态窗口缩小而不是消除（计划 19.3 亦明确
generation 读取+验证≠原子 claim，安全最终决策要权威校验）。会话若必须跨节点/重启存活，
内存态本身也不满足。

## 5. 迁移与兼容

- 迁移文件走 `priv/migrations/`（erlang_migrate，strict）；旧缓存路径在迁移期内保留读兼容、
  新写入全走表；上线后删除 handler 内 `cache_session/get_session/delete_session` 缓存调用。
- 客户端协议不变（三端 API 形状与错误码保持），仅服务端内部权威替换——**因此本方案不触碰
  客户端协议**，但仍然属于 Schema 变更，按执行边界须独立授权轮次实施。
- 回滚：迁移 down 脚本删表；表存在期间的新会话随回滚失效（60s 生命周期，业务可接受窗口）。

## 6. 验收 oracle（实施轮须全部通过）

- 并发 2×N status 领取：恰好 1 个拿到 token（N≥100 轮）。
- SSE 先收事件、status 后领取：同一 token 恰好被领取一次。
- scan/confirm/cancel 状态机 CAS：乱序与并发下状态转移唯一。
- 过期：领取与状态机对 `expires_at` 过期会话一律拒绝。
- 表内无 token 残留：消费后 `login_token IS NULL`；清扫后无过期行。
