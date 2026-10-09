# JPush 推送通道合同

本页说明当前源码的注册、投递和错误处理边界，不记录某次执行结果。服务端受理通知不等于手机收到通知；实际交付需要匹配客户端渠道、凭据、设备和运行环境验证。

## 注册与注销

`POST /api/v1/push/register` 的四个字段为 `device_id`、`device_type`、`platform`、`token`，均须非空。`device_type` 是设备类型（`android`、`ios`、`web`）；`platform` 是推送通道（`fcm`、`apns`、`web_push`、`jpush`），不能把 `android` 填入 `platform`。入口校验见 `src/api/user_device_handler.erl`；允许值不代表每种设备与通道组合均可投递。

JPush 当前适配 Android，token 使用 SDK 的 RegistrationID。同一用户、设备的活跃 token 由 `push_token_repo` 管理；注销调用 `POST /api/v1/push/unregister`。迁移 `00000136_enterprise_internal_foundation` 扩展数据库通道约束；其回滚恢复旧约束，已有 JPush 记录时不能直接回滚成功。

Flutter 对应 `lib/store/api/push_api.dart`、`lib/service/push_notification_service.dart` 与 `lib/store/repository/user_repo_local.dart`。SDK token 获取与向后端注册是不同步骤；仅有默认通道标识不能证明 SDK 已配置或 token 已取得。

## 适配器与配置

`src/push_provider_jpush.erl` 提供 `provider/0`、`device_type/0`、`send/3`、`send/4`、`classify/2`。`send/3` 不携带路由数据；`send/4` 在非空 Data 时加入 `notification.android.extras`。

HTTP 调用由 `src/push_provider_jpush_http.erl:post/3` 执行，返回 HTTP 状态与响应正文或网络错误。默认地址为 `https://api.jpush.cn/v3/push`；配置在 `{imboy, push}` 下：

```erlang
{jpush_app_key, "your-jpush-appkey"},
{jpush_master_secret, "your-jpush-secret"},
{jpush_push_url, "https://api.jpush.cn/v3/push"}
```

实际密钥应由运行环境注入，不能写入版本库、测试材料或日志。缺少或空凭据返回 `{error, not_configured}`，不发送请求。Basic 鉴权头由 AppKey 与 MasterSecret 构造；请求的 `platform` 固定为 Android，`audience.registration_id` 为目标 token。

## 隐私与路由数据

普通消息推送使用固定通知文案，不包含消息正文、密文片段或发送者身份。业务通知只应携带固定路由标识，例如 `notify_type=org_invite`。

`send/4` 本身会转发调用方提供的 Data，并未强制过滤字段；调用方必须守住上述数据边界。不能把“不应携带动态内容”写成适配器已经执行了字段白名单过滤。相关调用入口见 `src/logic/push_notification_logic.erl`。

## 响应与重试

| 响应分类 | 适配器结果 | 分派层处理 |
|---|---|---|
| HTTP 200 | `ok` | 结束；仅表示推送服务受理 |
| 缺凭据 | `not_configured` | 跳过，不重试 |
| 业务码 1003 | `invalid_token` | 下线 token，不重试 |
| HTTP 401 或业务码 1004 | `unauthorized` | 不重试，不下线 token |
| HTTP 429 或业务码 1011 | `rate_limited` | 进入异步重试 |
| 其他状态或网络错误 | 状态/错误原因 | 进入异步重试 |

`src/ds/push_notification_ds.erl` 按 `platform` 分派，多设备复用现有 fan-out；重试过程重新检查传入 Guard。未知通道记录调试信息后跳过，不表示投递成功。不要把未经支持的通道注册成功解释为支持该通道发送。

## 维护验证

`test/push_provider_jpush_tests.erl` 覆盖适配器与注册合同；`test/ds/push_notification_ds_tests.erl` 和推送逻辑测试覆盖分派、撤权及隐私边界。HTTP 替身测试验证请求构造和错误分类，不能证明真实推送到达。真实渠道测试还需单独验证 token 获取、注册、失效处理、点击路由及后台到达，并遵守第三方通知授权边界。
