# Imboy 消息确认机制/消息投递机制

> **版本**: 0.7.3 | **更新时间**: 2026-01-10
> **变更**: 统一 ACK 处理、配置化重试间隔

---

## 一、消息投递流程

### 1.1 流程图

```
发送方 →           服务器 →                   接收方
   ↓                          ↓                 ↓
发送                     立即响应            推送
存储备份表             等待ACK            确认(ACK)
入队处理                重试                  完成
```

### 1.2 重试时间线

| 时间 | 事件 |
|------|------|
| **0ms** | **立即投递**（先写入备份表，确保零丢失） |
| 5s | 第1次重试（未收到 ACK） |
| 7s | 第2次重试 |
| 11s | 第3次重试 |
| 17s | 第4次重试（停止在线重试） |

> **说明**：
> - 消息在投递前已写入数据库（`msg_store_ds:stage`）
> - 17秒后停止在线重试，消息仍在数据库中
> - 用户上线后通过离线消息接口拉取

### 1.3 重试间隔配置

**配置位置**：`include/chat.hrl`

```erlang
% 单聊消息
-define(MSG_RETRY_DELAYS_C2C, [0, 5000, 7000, 11000, 17000]).

% 群聊消息
-define(MSG_RETRY_DELAYS_C2G, [0, 3500, 7000, 11000, 17000]).

% 系统消息
-define(MSG_RETRY_DELAYS_C2S, [0, 5000, 7000, 11000]).

% 离线消息 pull
-define(MSG_RETRY_DELAYS_PULL, [0, 10000, 20000]).

% 用户通知
-define(MSG_RETRY_DELAYS_NOTICE, [0, 5000, 10000]).
```

**使用方式**：
```erlang
MsLi = elib_retry_config:intervals(<<"c2c">>),
message_ds:send_next(ToUid, MsgId, MsgJson, MsLi).
```

---

## 二、ACK 机制

### 2.1 两种 ACK

| 类型 | 方向 | 作用 |
|------|------|------|
| **SERVER_ACK** | 发送方→服务器 | 告知发送方"消息已收到" |
| **CLIENT_ACK** | 接收方→服务器 | 告知服务器"消息已送达"，停止重试 |

### 2.2 CLIENT_ACK 格式

```
CLIENT_ACK,C2C,msg_12345,device_id
    │         │      │         └─ 设备ID
    │         │      └────────────── 消息ID
    │         └────────────────────── 消息类型
    └─────────────────────────────── 固定前缀
```

**示例**: `CLIENT_ACK,C2C,cdsgrbgppoodp0gvpb60,ios-1234567890`

### 2.3 ACK 粒度

- ACK 以"设备 DID"为粒度：某个 DID 发送 CLIENT_ACK，只停止该 DID 的在线重试
- 系统允许重复投递，客户端必须基于 msg_id 去重

### 2.4 统一 ACK 处理

**实现模块**：`src/logic/msg_ack_logic.erl`

**处理流程**：
```
1. 客户端发送: CLIENT_ACK,C2C,msg_id,did

2. websocket_handler 接收并转发到 msg_ack_logic

3. msg_ack_logic:client_ack/4 统一处理:
   ├─ C2C: 删除离线消息 + unstage
   ├─ C2G: 标记 timeline + unstage
   ├─ S2C: 删除离线消息 + unstage
   └─ C2S: 删除消息（参数化查询） + unstage
```

**代码示例**：
```erlang
% 各 Logic 模块调用统一接口
c2c_client_ack(MsgId, CurrentUid, DID) ->
    msg_ack_logic:client_ack(<<"c2c">>, MsgId, CurrentUid, DID).
```

### 2.5 多设备 ACK 同步

#### 核心机制

```
1. 所有设备独立投递，独立 ACK
2. 每个设备独立的定时器：{Uid, DID, MsgId}
3. ACK 标志按设备粒度：{ack_received, Uid, DID, MsgId}
4. 跨节点同步：使用 syn 库广播到所有节点
```

#### ACK 处理流程

```
1. 客户端发送: CLIENT_ACK,C2C,msg_id,did

2. 服务器处理:
   ├─ websocket_logic:cancel_timer(Uid, did, MsgId)
   │    └─→ imboy_syn:broadcast_ack_cancel(Uid, did, MsgId)
   │         └─→ syn:members(?CHAT_SCOPE, Uid) 查询所有设备
   │              └─→ 跨节点广播 {ack_cancel, Uid, did, MsgId}
   │
   └─ 每个节点的 websocket_handler 接收:
        ├─→ 取消定时器: {Uid, did, MsgId}
        ├─→ 设置 ACK 标志: {ack_received, Uid, did, MsgId} = true (40秒)
        └─→ 其他设备不受影响，继续重试
```

#### 数据结构

```erlang
% 设备注册（syn 库）
syn:join(?CHAT_SCOPE, Uid, Pid, {DType, DID}).

% 查询用户所有在线设备
syn:members(?CHAT_SCOPE, Uid).

% 定时器 Key
TimerKey = {Uid, DID, MsgId}.

% ACK 标志 Key（按设备粒度）
AckReceivedKey = {ack_received, Uid, DID, MsgId}.
```

#### 关键特性

| 特性 | 实现方式 |
|------|----------|
| **独立重试** | 每个设备独立的定时器 Key `{Uid, DID, MsgId}` |
| **ACK 粒度** | 只停止发送 ACK 的设备 |
| **跨节点同步** | 使用 `syn:members(?CHAT_SCOPE, Uid)` + 进程消息 |
| **防重机制** | `{ack_received, Uid, DID, MsgId}` 缓存标志（40秒） |
| **故障隔离** | 单个设备故障不影响其他设备 |
| **SQL 安全** | C2S 使用参数化查询，防止注入 |

---

## 三、存储机制

### 3.1 备份表流程

```
备份表 (PostgreSQL)  ← 先写入，防止丢失
        ↓
批量写入 Worker    ← 100条/批 或 1秒触发
        ↓
正式表 (msg_c2c等)  ← 批量写入
```

> **说明**：写入链路以 staging 表为唯一事实源。

### 3.2 备份表结构

| 字段 | 说明 |
|------|------|
| msg_type | c2c/c2g/s2c/c2s |
| msg_id | 消息唯一ID（UNIQUE约束） |
| payload | 消息内容（JSONB） |
| from_id | 发送者ID |
| to_id / to_id_list | 接收者（单聊/群聊） |
| created_at | 创建时间 |
| retry_count | 重试次数 |
| processed_at | 处理时间（NULL=未处理） |

---

## 四、防重复投递

| 措施 | 实现方式 |
|------|----------|
| ACK标志 | 收到ACK后设置标志（按 DID），超时检查时停止重试 |
| 唯一约束 | 备份表 `(msg_type, msg_id)` UNIQUE |
| 客户端去重 | 基于 msg_id 过滤重复消息 |

---

## 五、故障恢复

| 故障 | 恢复机制 |
|------|----------|
| 网络抖动 | 自动重试（0ms、5s、7s、11s、17s） |
| 客户端掉线 | 重试5次（17秒）后停止在线投递；消息已在数据库，用户上线后通过离线消息接口拉取 |
| 服务器重启 | 启动时从备份表恢复未处理消息 |
| 数据库故障 | 自动重试，失败重新入队 |

---

## 六、监控与调试

### 6.1 关键指标

| 指标 | 告警阈值 |
|------|----------|
| 写入待处理（staging pending） | > 1000 |
| ACK超时 | > 30/分钟 |

### 6.2 日志关键词

```
📥 [CLIENT_ACK]         ← 收到确认
📥 [UNIFIED_ACK]        ← 统一ACK处理
✅ [ACK_CANCEL]         ← 取消重试定时器
⏰ [TIMEOUT_CHECK]      ← 超时检查
📥 [ACK_CANCEL_FROM_REMOTE] ← 收到远程 ACK 取消
```

### 6.3 调试命令

```erlang
% 查看队列状态
msg_store_ds:status().

% 查看备份表记录
% SELECT * FROM msg_store_staging WHERE processed_at IS NULL;

% 清空测试数据
elib_pg:execute(<<"TRUNCATE TABLE public.msg_store_staging CASCADE">>, []).

% 查看某个 UID 的所有设备
syn:members(?CHAT_SCOPE, Uid).

% 获取重试间隔配置
elib_retry_config:intervals(<<"c2c">>).
```

---

## 七、常见问题

**Q: 消息会丢失吗？**
A: 不会。消息先写入 staging 表，启动时自动恢复未处理消息。

**Q: 消息会重复吗？**
A: 允许重复投递，但客户端必须按 msg_id 去重；服务端收到 ACK（按 DID）会停止该 DID 的重试。

**Q: 用户离线怎么办？**
A: 消息在投递前已写入数据库；在线投递重试5次（17秒）后停止；用户上线后通过离线消息接口拉取。

**Q: 如何验证已送达？**
A: 查看日志 `[CLIENT_ACK]` 和 `[ACK_CANCEL]`。

**Q: 队列堆积？**
A: 检查数据库连接池、Worker状态、错误日志。

**Q: 如何调整重试策略？**
A: 修改 `include/chat.hrl` 中的 `MSG_RETRY_DELAYS_*` 宏定义。

---

## 八、相关文件

### 核心模块

| 文件 | 职责 |
|------|------|
| `src/logic/msg_ack_logic.erl` | **统一 ACK 处理**（新增） |
| `src/lib/elib_retry_config.erl` | **重试间隔配置**（新增） |
| `include/chat.hrl` | 重试间隔宏定义 |

### 投递相关

| 文件 | 职责 |
|------|------|
| `src/api/websocket_handler.erl` | 处理 CLIENT_ACK |
| `src/logic/msg_c2c_logic.erl` | 单聊消息发送 |
| `src/logic/msg_c2g_logic.erl` | 群聊消息发送 |
| `src/logic/msg_c2s_logic.erl` | C2S 消息处理 |
| `src/logic/msg_s2c_logic.erl` | S2C 消息处理 |
| `src/logic/websocket_logic.erl` | ACK 定时器管理 |
| `src/logic/user_server.erl` | 用户通知 |
| `src/ds/message_ds.erl` | 消息投递重试 |
| `src/lib/imboy_syn.erl` | ACK 同步广播（syn 库） |
| `src/ds/msg_store_ds.erl` | 队列管理 |
| `src/ds/msg_store_worker.erl` | 批量写入 |
| `src/repo/msg_store_repo.erl` | 备份表操作 |
| `src/repo/msg_c2s_repo.erl` | C2S 消息仓库（安全接口） |

---

**文档维护**: 更新消息确认机制时同步更新此文档。
