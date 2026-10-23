# API 格式规范 / API formats

本文说明当前公共传输规则。机器契约入口为 [`api/README.md`](../../api/README.md)；
具体参数、类型和例外以对应端点及最终响应实现为准。
This guide describes current shared wire conventions; endpoint contracts define exceptions.

## 三种接口面 / Audiences

| 接口面 | 路径 | 认证 | JSON 响应 |
|---|---|---|---|
| 人类业务客户端 | `/api/v1/*` | Access JWT；公开入口或专用凭证见端点 | 通常为业务信封 |
| 管理后台 | `/api/adm/*` | `adm_user_id` 与 `adm_user_sig` Cookie；登录及 setup 等例外见端点 | 通常为业务信封 |
| 企业集成 | `/api/internal/v1/*` | HMAC-SHA256 四头联合认证 | 通常为裸成功对象及稳定 error 对象 |

HTTP 页面、下载／导出流、受控上传 receipt、握手错误及 OA 登录票据等存在专用格式。
详见 [Internal 集成说明](../../api/internal/v1/README.md) 和各受众 OpenAPI。

## 业务响应信封 / Business envelope

[`elib_response.erl`](../../src/lib/elib_response.erl) 生成的标准 JSON：

```json
{
  "code": 0,
  "msg": "success",
  "sv_ts": 1792713600000,
  "payload": {}
}
```

| 字段 | 类型 | 说明 |
|---|---|---|
| `code` | integer | 0 成功，非零为业务失败；普通 error/2 的缺省码为 1 |
| `msg` | string | UTF-8 人类可读说明，不作为机器判断依据 |
| `sv_ts` | integer | 服务端 Unix 毫秒 |
| `payload` | 端点定义 | 可以是对象或数组；具体成功／失败形状见 schema |

四个字段在公共信封中必需。普通 `success/error` helper 默认 HTTP 200，
`error_with_status` 及专用 handler 可返回实际 HTTP 错误码。
调用方应先处理 HTTP／网络状态，再检查信封 `code`；不能把 HTTP 200 等同于业务成功。
Check transport status and the business code separately. This envelope does not apply to every HTTP response.

业务错误码见 [`error_code.hrl`](../../include/error_code.hrl) 和
[错误码说明](error-codes.md)。Internal 的字符串错误码是另一套模型。

## ID 与时间 / IDs and time

- 实体 TSID 通常以 JSON integer 传输，部分端点显式返回十进制字符串。
  JavaScript 使用无损解析后以 `EntityId` 持有；不能经 `Number` 损失精度。
- 普通 IM 消息 `id` 是不透明字符串，保留原值；消息表行主键等实体 ID 仍是 TSID。
- 消息顺序与历史增量以 `conv_seq` 为准，不能从业务消息 ID 推断。
- 新业务端点的用户字段优先采用 `user_id`；已发布的 `uid` 等字段按端点兼容规则保留。

详见 [TSID 字段约定](tsid-field-convention.md) 和
[WebSocket 消息身份](ws-protocol-contract.md#41-业务消息-id--business-message-identity)。

| 最终输出路径 | 时间表示 |
|---|---|
| `elib_response:success/2,3,4` | 递归处理 `_at`／`_ts`：字符串／PG 时间 tuple 转 Unix 毫秒；已有整数原样保留（部分端点为秒） |
| `elib_response:success_rfc3339/2,3` | 保持调用方时间值，端点通常声明 RFC3339 字符串 |
| 直接 `cowboy_req:reply`／`jsone:encode` | 不经过上述转换，按端点实际编码 |
| WebSocket | `server_ts` 通常为 Unix 毫秒；业务 payload 的日期字段可能为 RFC3339 |

数据库字段类型本身不能决定最终 JSON 类型。没有统一“所有时间字段均为毫秒”的规则。
Follow the final serializer, not only the database column type.

## 分页 / Pagination

| 模式 | 参数 | 当前公共行为 |
|---|---|---|
| `elib_param:page/1` | `page`、`size` | 默认 1／20；通用 size 上限 1000；业务逻辑可进一步限制 |
| `elib_param:page_qs/2` | `page`、`size` | 页大小默认值由调用方提供，通用上限 100 |
| Internal cursor | `cursor` 加 `limit` 或 `page_size` | 默认值、上限及错误规则按端点；游标原样回传 |
| 消息历史 | `after_seq` 等 | 按会话序列拉取，具体参数见消息契约 |

Offset 响应常包含 `list/page/size/total/total_page`，cursor 响应常包含
`items/has_more/next_cursor`；只有端点实际提供 `total` 时才能按总数计算翻页。
不假设所有列表的参数、容器键、空游标或越界行为相同。

## WebSocket 信封 / WebSocket envelope

握手、协议选择和帧格式见 [WebSocket 契约](ws-protocol-contract.md)；
[AsyncAPI](../../api/asyncapi.yaml) 从客户端视角描述 send／receive。
JSON 业务信封使用大写 `C2C/C2G/C2S/S2C`，内容类型 `msg_type` 和
动作 `action` 位于顶层，业务载荷放在 `payload`。例如：

```json
{
  "id": "019f0000-0000-7000-8000-000000000001",
  "type": "C2C",
  "from": "84442613760000001",
  "to": "84442613760000002",
  "msg_type": "text",
  "payload": {"text": "hello"}
}
```

示例 ID 为合成值。E2EE 载荷不可套用明文 payload；JSON 与 Protobuf 的类型及字段能力
存在差异，按 [`imboy_codec.erl`](../../src/lib/imboy_codec.erl) 和具体协议定义处理。
