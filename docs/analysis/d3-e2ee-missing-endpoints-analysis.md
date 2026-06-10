# D3 E2EE 缺失接口分析

> 协议一致性报告问题编号：D3
> 分析日期：2026-05-27
> 状态：**分析完成，待集成**

---

## 涉及接口

| 接口 | 路由 | 后端状态 |
|------|------|---------|
| 检查密钥状态 | `GET /v1/e2ee/key/status?device_id=xxx` | 已实现（`e2ee_handler:key_status/2`） |
| 拉取密钥变更通知 | `GET /v1/e2ee/notifications/pull?since=timestamp&limit=50` | 已实现（`e2ee_handler:pull_notifications/2`） |

路由注册位置：`imboy/src/imboy_router.erl` 第 247–248 行。

---

## 后端接口能力

### GET /v1/e2ee/key/status

调用链：`e2ee_handler:key_status` → `e2ee_recovery_logic:check_key_status(Uid, DeviceId)`

**请求参数**：
- `device_id`（必填）：当前设备 ID

**返回**：设备密钥状态及可用恢复方式列表。

**用途**：客户端在登录、密钥轮换、或检测到解密异常时，通过此接口确认服务端记录的密钥状态，判断是否需要触发密钥恢复流程。

---

### GET /v1/e2ee/notifications/pull

调用链：`e2ee_handler:pull_notifications` → `e2ee_logic:pull_key_notifications(Uid, Since, Limit)`

**请求参数**：
- `since`：上次拉取时间戳，支持增量拉取
- `limit`：最多返回条数（默认 50）

**返回**：好友的密钥变更记录列表（`notifications` 数组 + `count`）。

**用途**：当好友更换设备或重新安装 App 时，其公钥会发生变化。客户端需定期（或在 WebSocket 连接建立后）拉取此通知，清除旧的本地公钥缓存并重新获取，确保下次加密消息时使用最新公钥。

---

## 客户端现状分析

当前 `E2EEHealthCheckService`（`imboyapp/lib/service/e2ee_health_check_service.dart`）的实现仅依赖本地数据源：

| 能力 | 当前实现 | 问题 |
|------|---------|------|
| 密钥版本检查 | 调用 `E2EEService.getUserDevicePublicKeys(forceRefresh: true)` 本地缓存 + 现有公钥接口 | 无法获取服务端对本设备密钥的权威状态，无法判断是否需要恢复 |
| 密钥变更感知 | 无 | 好友换设备后，客户端无法得知其公钥已失效，继续使用旧公钥加密消息会导致对方无法解密 |

---

## 缺失的影响

1. **好友公钥失效无感知**：好友重装 App 后公钥变更，本端继续用旧公钥加密，对方解密失败，消息变为「解密失败」状态。此问题只能靠用户手动重建会话触发公钥刷新，体验差。

2. **密钥恢复时机缺失**：换机或重装场景下，客户端无法主动查询服务端是否有可用的密钥恢复路径（设备传输、社交恢复），只能等待用户手动操作。

3. **解密失败率升高**：`retryFailedMessages` 本地重试可以处理短暂密钥不同步，但无法解决公钥已彻底变更的场景。

---

## 推荐集成方案

### 阶段 1：集成 `/v1/e2ee/notifications/pull`（优先级：高）

**触发时机**：
- WebSocket 连接建立后（每次重连）
- 拉取离线消息完成后（`MessageOfflineService` 成功回调）
- 应用从后台切回前台时（`AppLifecycleState.resumed`）

**实现要点**：
1. 在 `E2EEHealthCheckService` 中新增 `pullKeyNotifications` 方法，调用 `GET /v1/e2ee/notifications/pull?since=<last_ts>`。
2. 本地保存上次拉取时间戳（`StorageService`，key：`e2ee_notification_last_since`）。
3. 遍历返回的通知，对每个发生密钥变更的好友调用 `syncFriendPublicKey(uid)`（已有实现）。

**参考代码结构**：
```dart
Future<void> pullKeyNotifications() async {
  final since = StorageService.to.getInt('e2ee_notification_last_since') ?? 0;
  final resp = await HttpClient.client.get(
    '/v1/e2ee/notifications/pull',
    queryParameters: {'since': since, 'limit': 50},
  );
  // 解析 resp.payload['notifications']，对变更 uid 调用 syncFriendPublicKey
  // 更新 last_since
}
```

### 阶段 2：集成 `/v1/e2ee/key/status`（优先级：中）

**触发时机**：
- 首次启动或重装后的 E2EE 初始化流程
- 检测到本地密钥不存在或损坏时

**实现要点**：
1. 在 `E2EEHealthCheckService.checkUserKeyVersion` 中，补充对 `/v1/e2ee/key/status?device_id=<did>` 的调用，将服务端返回的权威状态与本地状态对比。
2. 若服务端返回 `has_recovery_options: true`，引导用户进入密钥恢复流程（调用 `/v1/e2ee/recovery/start`，已有后端实现）。

---

## 依赖条件

- 后端两个接口均已实现并注册路由，无需后端改动。
- 前端仅需新增 HTTP 调用与本地游标持久化逻辑，不涉及架构改动。
- 建议与 D4 游标修复一同测试，确保离线拉取完成事件可用于触发通知拉取。
