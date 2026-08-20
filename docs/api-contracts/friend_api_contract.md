# 好友链路 API 契约（GET /api/v1/friend/list）

> 状态：存量契约固化（2026-08-20 以 backend 代码为权威源整理）。前缀 `/api/v1`，需要用户登录态。

## 1. 端点表

| 端点 | 方法 | 说明 | 权威源 |
|---|---|---|---|
| `/api/v1/friend/list` | GET | 当前用户好友列表（含 `mine` 自身信息 + `friend[]`） | `src/api/friend_handler.erl:125-156`、路由 `src/imboy_router.erl:213` |

## 2. 响应结构

```json
{
  "mine": { "id": 100, "...": "DEF_USER_COLUMN + status" },
  "friend": [ { "id": 200, "is_from": "1", "source": "qrcode", "is_friend": 1, "...": "..." } ]
}
```

信封字段（`src/api/friend_handler.erl:154-156` list_transfer/2）：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `mine` | object | 否 | 当前用户信息（DEF_USER_COLUMN + `status` 在线态） | `friend_handler.erl:136-139` |
| `friend` | array | 否（可为空数组） | 好友行数组 | `friend_handler.erl:140-143` |

## 3. `mine` 字段

`user_logic:find_by_id/1` 默认列 = `?DEF_USER_COLUMN`（`src/logic/user_logic.erl:214-216`），定义在 `include/common.hrl:13-14`；另合并 `mine_state` 的 `status`（`src/logic/user_logic.erl:165-172`）。

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `id` | int | 否 | 用户 TSID（bigint，JSON number 直出） | `common.hrl:13` |
| `account` | string | 否 | 账号 | `common.hrl:13` |
| `nickname` | string | 否 | 昵称 | `common.hrl:14` |
| `avatar` | string | 否 | 头像 URL（空时服务端补默认头像，`user_logic.erl:227-228` check_avatar） | `common.hrl:14` |
| `background` | string | 是 | 个人背景图 | `common.hrl:14` |
| `sign` | string | 是 | 个性签名 | `common.hrl:14` |
| `gender` | int | 否 | 1 男 2 女 3 保密 0 未知 | `common.hrl:14` |
| `region` | string | 是 | 地区 | `common.hrl:14` |
| `birthday` | string/date | 是 | 生日 | `common.hrl:14` |
| `profession` | string | 是 | 职业 | `common.hrl:14` |
| `school` | string | 是 | 学校 | `common.hrl:14` |
| `interests` | string | 是 | 兴趣 | `common.hrl:14` |
| `account_type` | int | 否 | 0=真人 1=AI 助手 2=官方机器人 | `common.hrl:14` |
| `status` | string | 否 | `online` / `hide`（自身视角：隐身设置直接给 `hide`，`user_logic.erl:165-172`） | `friend_handler.erl:137-139` |

## 4. `friend[]` 行字段

SQL 字段拼接权威源 `src/ds/friend_ds.erl:348-361`（fields/1）：`u.<DEF_USER_COLUMN>` + 关系列；随后整批过 `user_ds:batch_online_state`（`src/ds/user_ds.erl:511-531`）注入 `status`/`last_seen_at`。查询条件 `f.status = 1 AND f.from_user_id = $1`，上限 1000 条（`friend_ds.erl:149-165`）。

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `id` | int | 否 | 好友用户 TSID（JSON number 直出；`convert_friend_ids` 实为 no-op） | `friend_ds.erl:360-361、363-375` |
| `account` / `nickname` / `avatar` / `background` / `sign` / `gender` / `region` / `birthday` / `profession` / `school` / `interests` / `account_type` | 同 mine | — | 好友的用户表公开列（DEF_USER_COLUMN） | `friend_ds.erl:360-361` + `common.hrl:13-14` |
| `is_from` | string | 是 | 好友关系发起人标记，取自 `f.setting::jsonb->>'is_from'`（jsonb 文本抽出，**字符串** `'0'`/`'1'`） | `friend_ds.erl:355` |
| `source` | string | 是 | 添加来源：visitcard / qrcode / people_nearby / recently_user / user_search 等（同上，字符串） | `friend_ds.erl:356` |
| `is_friend` | int | 否 | 0=对方把我拉黑（denylist 命中），1=正常好友。SQL CASE：`d.user_id=<我> AND d.denied_user_id=u.id 则 0 否则 1` | `friend_ds.erl:352-354` |
| `remark` | string | 是 | 我给对方的备注 | `friend_ds.erl:359` |
| `tag` | string | 是 | 好友标签，半角逗号分隔多值 | `friend_ds.erl:359` |
| `category_id` | int | 否 | 好友分组 ID（0=未分组） | `friend_ds.erl:359` |
| `created_at` | int | 否 | 好友关系建立时间（f.created_at；REST 信封转**毫秒 int**） | `friend_ds.erl:359` + `elib_response.erl:30-32` |
| `status` | string | 否 | 实时在线态 `online`/`offline`（`imboy_syn:count_user` + 隐身设置；对方隐身时我看到的仍是 `offline`） | `user_ds.erl:523-543` |
| `last_seen_at` | null | 是 | **恒为 null**：SELECT 不含该列，兜底 `<<>>` 经信封 `rfc3339_to(<<>>) → null` | `user_ds.erl:526-528` + `elib_dt.erl:262-263` |

## 5. 前端消费方

| 端 | 文件 | 说明 |
|---|---|---|
| Flutter | `imboyapp/lib/store/api/contact_api.dart`（`API.friendList`，取 `payload['friend']`） | 请求与数组提取 |
| Flutter | `imboyapp/lib/store/model/contact_model.dart:128-175`（ContactModel.fromMap） | 逐字段消费：`id`/`status`/`last_seen_at`/`is_from`/`source`/`remark`/`tag`/`category_id`/`account_type` 等 |
| admin | — | C 端接口，管理后台不消费（后台用 `/api/adm/user*`） |

## 6. 已知双轨与注意事项

1. **`updated_at` 缺失回退 `created_at`（已知行为，非 bug）**：后端 SELECT 只返回 `f.created_at`，不返回 `updated_at`；Flutter `ContactModel.fromMap`（`contact_model.dart:149-154`）以 `json[updated_at] ?? json['created_at']` 兜底，两者都缺才回退当前时间。文档化目的：UI 显示的「更新时间」实为好友关系建立时间。
2. **`last_seen_at` 恒 null**：user 表无该列（属 user_friend 侧），batch_online_state 兜底空串、REST 信封转 null。消费方按「不可用」处理。
3. **`is_from`/`source` 是字符串**：jsonb `->>` 抽出恒为 text；Flutter 用 `parseModelInt(json[isFrom])` 做了字符串→int 容错。新消费方注意不要按 int 断言。
4. **`is_friend` 的语义是黑名单视角**：仅当**对方**在 `user_denylist` 拉黑了**我**时为 0；单向删除好友后行直接从列表消失（`f.status=1` 过滤），不会出现 is_friend=0 的“僵尸行”。
5. **`id` 为 JSON number**：`convert_user_id`/`convert_friend_ids` 名义上“转 binary”但实际原样放回 int（`friend_handler.erl:248-255`、`friend_ds.erl:363-375`）。Dart int64 消费无碍；JS 消费方有精度风险——这是与「TSID 出站转 string」约定的**既有漂移**，固化文档时不改代码，仅登记。
6. **列表上限 1000**：`page_by_uid/1` 硬编码 LIMIT 1000 OFFSET 0（`friend_ds.erl:149-151`），无分页参数；超 1000 好友会截断。
7. **`mine.status` 与 `friend[].status` 语义不同**：mine 是 `online|hide`（自身隐身）；friend 是 `online|offline`（对方视角永远看不到 hide）。
