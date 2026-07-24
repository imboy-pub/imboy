# /api/v1/* REST API 端点总目录 / REST API v1 Endpoint Catalog

> 日期 / Date: 2026-06-02 ｜ 状态 / Status: 长期协议契约文档 / Long-lived contract doc
> 范围 / Scope: 全部 `/api/v1/*` REST 端点（不含 `/api/v1/ws` WebSocket 与 `/api/v1/test/*` 测试路由）
> 权威来源 / Source of truth: `src/imboy_router.erl` + 各 `src/api/*_handler.erl`（逐一阅读真实源码提取，非概括推断）
> 方法 / Method: 按域并行审计 30 个 handler，逐端点核对请求/响应字段

简体中文为权威版本；English mirrors are inline per row. 代码、命令、模块名不翻译。

相关文档 / Related docs：
- 通用入口与 envelope：[rest-api.md](./rest-api.md)
- API 格式规范：[../reference/api-format.md](../reference/api-format.md)
- 错误码：[../reference/error-codes.md](../reference/error-codes.md)
- TSID 字段约定：[tsid-field-convention.md](./tsid-field-convention.md) ｜ [tsid-field-matrix.md](./tsid-field-matrix.md)
- 频道详细契约：[channel_api_contract_v1.md](./contracts/channel_api_contract_v1.md)
- 朋友圈详细契约：[moment_api_contract_v1.md](./contracts/moment_api_contract_v1.md)
- E2EE 社交恢复分片：[e2ee_server_persisted_shard_contract_v1.md](./contracts/e2ee_server_persisted_shard_contract_v1.md)
- WebSocket 协议：[websocket-api-2.md](./websocket-api-2.md)

---

## 概览 / Overview

- 端点总数 / Total endpoints: **约 130+** 个 `/api/v1/*` REST 端点，分布于 **30** 个 handler。
- 鉴权模型 / Auth model: 三类 —— `公开 Open`（无需 token）、`可选 Optional`（有 token 才校验）、`JWT`（默认，必须 Authorization）。
- 响应信封 / Response envelope: 绝大多数返回 `{code, msg, payload}`；少数特殊端点返回裸 body（已在对应行注明）。
- TSID: 实体 ID（`id`/`uid`/`gid`/`channel_id` 等）为 **64 位 TSID**，以 **JSON integer** 传输；前端用 `safeParseBigIntJson` 转 string（详见 [tsid-field-convention.md](./tsid-field-convention.md)）。

### 鉴权与信封约定 / Auth & Envelope Conventions

```json
{ "code": 0, "msg": "success.", "payload": {} }
```

- `code = 0` 成功；`code != 0` 失败。`payload` 结构由各端点定义。
- 鉴权基线取自 `imboy_router:open/0`（公开）与 `option/0`（可选）；其余路由经 `auth_middleware_api_v1` 强制 JWT。
- 表中“响应载荷 Response payload”列仅列 `payload` 顶层字段，不重复 `{code,msg}`。

### 不走标准信封的端点 / Non-envelope endpoints

| 路径 Path | 说明 / Note |
|---|---|
| `/api/v1/init` | `payload.res` 为 AES-256-CBC 加密的初始化数据（裸 payload 含 `test`/`res`） |
| `/api/v1/app/manifest` | 直返裸 JSON（`features`/`policy`/`plugins`/`generated_at`），带 ETag/304 |
| `/api/v1/metrics` | `accept: text/plain` 时返回 Prometheus 文本，否则 JSON |
| `/api/v1/passport/qr_login/subscribe` | SSE `text/event-stream` 长连接 |
| `/api/v1/group/file/download` | HTTP 302 重定向到文件 URL |
| `/api/v1/uqrcode`、`/api/v1/group/qrcode` | 无 token / 校验失败时 302 重定向 |

---

# A. 系统与认证 / System & Auth

## 系统与配置 / System & Config

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/init | 公开 Open | index_handler#init | 客户端初始化配置（加密下发）/ Encrypted client init config | Header: `vsn`,`cos`,`pkg`,`sk` | `test`、`res`（AES 加密 JSON：`ws_url`/`upload_url`/`attach_presign_endpoint`/`login_rsa_pub_key` 等） |
| GET | /api/v1/app/features | 公开 Open | app_feature_handler#features | 功能特性开关表 / Feature flags | 无 | feature map（键→bool） |
| GET | /api/v1/app/manifest | 公开 Open | app_manifest_handler#manifest | 应用清单（带 ETag/304）/ App manifest | Header: `if-none-match`（可选） | `features`、`policy`、`app_entries`、`admin_entries`、`plugins`、`generated_at`（裸 JSON） |
| GET | /api/v1/app/policy | 公开 Open | app_feature_handler#policy | 生效策略视图 / Effective policy | 无 | policy map |
| GET | /api/v1/app/ice_servers | JWT | app_feature_handler#ice_servers | WebRTC STUN/TURN 配置 / ICE servers | 无 | `ice_servers`(array) |
| GET | /api/v1/app_version/check | 可选 Optional | app_version_handler#check | 版本升级检查 / Version check | Header: `cos`,`did`；Query: `vsn`,`region_code` | `updatable`、`upgrade_type`(none/force/recommend/silent)、`check_interval_hours` |
| POST | /api/v1/app_upgrade/report | 可选 Optional | app_upgrade_log_handler#report | 上报升级事件 / Report upgrade event | Body: `event`*,`client_vsn`*,`target_vsn`,`upgrade_type`,`extra`,`uid` | `ok`=true（缺必填返回 400） |
| GET | /api/v1/metrics | 公开 Open | metrics_handler | 运行时指标 / Runtime metrics | Header: `accept` | JSON `counters`/`histograms` 或 Prometheus 文本 |

## 认证与登录 / Auth & Login

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/refreshtoken | 公开 Open | passport_handler#refreshtoken | 刷新 access token / Refresh token | Header: `imboy-refreshtoken` | `token`（限流超额 429） |
| POST | /api/v1/passport/quick_login | 公开 Open | passport_handler#quick_login | 运营商一键登录 / Carrier one-tap | Body: `service`,`operator`,`token`,`sys_version` | 登录 payload：`uid` integer(TSID)、`token`、`refreshtoken`、`email`、`nickname`、`avatar`、`account`、`gender`、`region`、`sign`、`status`、`role`、`setting` |
| POST | /api/v1/passport/login | 公开 Open | passport_handler#login | 密码/验证码登录 / Login | Body: `type`(email/mobile/account),`account`,`pwd`(RSA),`code`；Header: `cos`,`did` | 同上登录 payload；设备冲突 code=5100；验证码登录可带 `action=need_set_password` |
| POST | /api/v1/passport/signup | 公开 Open | passport_handler#signup | 注册账号 / Sign up | Body: `type`,`account`,`pwd`(RSA),`code`,`nickname`,`avatar` | `{}`（语义成功） |
| POST | /api/v1/passport/getcode | 公开 Open | passport_handler#getcode | 发送验证码 / Send code | Body: `type`(email/sms),`scene`,`account` | `{}`（手机号已存在返回 paramAlreadyExist） |
| POST | /api/v1/passport/findpassword | 公开 Open | passport_handler#find_password | 验证码重置密码 / Reset password | Body: `type`,`account`,`pwd`(RSA),`code` | `{}` |
| GET | /api/v1/passport/bind_mail | 公开 Open | passport_handler#bind_mail | 邮件链接确认绑定邮箱 / Confirm email bind | Query: `ts`,`tk`(HMAC),`uin`,`mail` | `{}` |
| POST | /api/v1/passport/qr_login/create | 公开 Open | qr_login_handler#create | 创建扫码登录会话（60s）/ Create QR session | Body: `device_id`*,`device_name`,`platform` | `qr_token`、`session_token`、`expires_in`=60 |
| GET | /api/v1/passport/qr_login/status | 公开 Open | qr_login_handler#status | 轮询扫码状态 / Poll QR status | Query: `session_token` | `status`(waiting/scanned/confirmed/cancelled)；confirmed 附 `token` |
| POST | /api/v1/passport/qr_login/scan | 公开*（实需登录）| qr_login_handler#scan | 手机端扫码 / Phone scans | Body: `qr_token`；State: `current_uid` | `status`=scanned、`device_name`、`platform` |
| POST | /api/v1/passport/qr_login/confirm | 公开*（实需登录）| qr_login_handler#confirm | 手机端确认登录 / Phone confirms | Body: `qr_token`；State: `current_uid` | `status`=confirmed |
| POST | /api/v1/passport/qr_login/cancel | 公开 Open | qr_login_handler#cancel | 取消扫码会话 / Cancel QR | Body: `session_token` | `status`=cancelled |
| GET | /api/v1/passport/qr_login/subscribe | 公开 Open | qr_login_sse_handler | SSE 推送扫码状态 / SSE status push | Query: `session_token`* | SSE 帧 `data:{status[,token]}`（30s 心跳） |
> 注：`qr_login/scan`、`qr_login/confirm` 路由列为公开，但 handler 依赖 `State.current_uid`，手机端须携带 JWT 才能取到非 0 uid。

---

# B. 会话与消息 / Conversation & Messaging

## 会话 / Conversation

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/conversation/online | 公开 Open | conversation_handler#online | 在线统计与列表 / Online stats & list | Query: `type`(list 时返回列表),`limit`(默认10) | 数组（list 元素含 `uid`,`pid`,`dtype`,`did`,`time`,`ref`,`node`） |
| GET | /api/v1/conversation/mine | JWT | conversation_handler#mine | 我的会话列表 / My conversations | Query: `last_server_ts`(可选) | 会话列表（聚合 c2c+c2g，limit=1000） |
| POST | /api/v1/conversation/pin | JWT | conversation_handler#pin_conversation | 置顶会话 / Pin | Body(JSON): `conversation_id`,`type`(默认 c2c) | `{}` |
| POST | /api/v1/conversation/unpin | JWT | conversation_handler#unpin_conversation | 取消置顶 / Unpin | Body(JSON): `conversation_id`,`type` | `{updated:true}` |
| GET | /api/v1/conversation/pinned | JWT | conversation_handler#pinned_list | 置顶列表 / Pinned list | 无 | `{items:[...]}` |
| POST | /api/v1/conversation/delete | JWT | conversation_handler#delete_conversation | 删除会话（软删）/ Soft-delete | Body(JSON): `conversation_id`,`type` | `{}` |
| POST | /api/v1/conversation/restore | JWT | conversation_handler#restore_conversation | 恢复会话 / Restore | Body(JSON): `conversation_id`,`type` | `{restored:true}` |

## 消息 / Message

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/msg/offline | JWT | msg_handler#offline | 拉取离线消息 / Fetch offline | Query: `limit`(默认1000),`c2c_last_msg_at`,`c2g_last_msg_at`,`s2c_last_msg_at`(ms) | `{c2c:{has_more,next_last_msg_at,total,list},c2g:{...},s2c:{...}}` |
| POST | /api/v1/msg/offline_ack | JWT | msg_handler#offline_ack | 确认离线消息 / Ack offline | Body: `type`(c2c/c2g/s2c),`msg_ids`(list) | `{type,processed_count,msg_ids_count}` |
| GET | /api/v1/msg/read_stats | JWT | msg_handler#read_stats | 群消息已读统计 / Read stats | Query: `msg_id`* | `{read_count,total_count}` |
| POST | /api/v1/msg/pin | JWT | msg_handler#pin | 置顶/取消置顶消息 / Pin message | Body: `msg_id`*,`pinned`(bool)* | `{msg_id,pinned}` |
| POST | /api/v1/msg/forward | JWT | msg_handler#forward | 转发消息 / Forward | Body: `msg_ids`(list)*,`to`(TSID)*,`to_type`* | `{forward_msg_ids,forward_count}` |
| POST | /api/v1/msg/reaction/add | JWT | msg_handler#reaction_add | 添加表情回应 / Add reaction | Body: `msg_id`*,`msg_type`(默认 c2c),`emoji`* | `{msg_id,emoji,user_id,created_at}` |
| POST | /api/v1/msg/reaction/remove | JWT | msg_handler#reaction_remove | 移除表情回应 / Remove reaction | Body: `msg_id`*,`msg_type`,`emoji`* | `{msg_id,emoji}` |
| GET | /api/v1/msg/reaction/list | JWT | msg_handler#reaction_list | 表情列表 / List reactions | Query: `msg_id`*,`msg_type`(默认 c2c) | reaction 列表（见源码） |
| GET | /api/v1/msg/history | JWT | msg_handler#history | 消息历史（conv_seq 游标）/ History | Query: `chat_type`(c2c/c2g)*,`peer_id`(TSID)*,`after_seq`(默认0),`limit`(默认50,≤100) | `{messages,next_seq,has_more,conv_key}`；message 含 `conv_seq`/`from`/可选 `to`/`group_id` |

## @提及 / Mention

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET/POST | /api/v1/mention/list | JWT | mention_handler#list | @我列表（可按群）/ List @-me | `page`,`size`,`is_read`,`gid`/`group_id`(可选) | `{total,page,size,items[]}`；item 含 `id`,`msg_id`,`group_id`,`from_uid`,`mentioned_uid`,`is_read`,`created_at` |
| GET/POST | /api/v1/mention/unread | JWT | mention_handler#unread | 未读@计数 / Unread count | `gid`/`group_id`(可选) | `{count}` |
| POST | /api/v1/mention/mark_read | JWT | mention_handler#mark_read | 标记已读 / Mark read | `all`(bool) 或 `msg_id` 或 `mention_id`；`all=true` 可带 `gid`/`group_id` | `{msg_id}`/`{mention_id,msg_id}`/`{all:true[,group_id]}` |
| GET/POST | /api/v1/mention/suggest | JWT | mention_handler#suggest | @输入成员建议 / @-suggest | `gid`/`group_id`*,`keyword` | `{members[],items[]}`（两键同列表） |

---

# C. 用户与社交 / User & Social

## 用户 / User

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/uqrcode | 可选 Optional | user_handler#qrcode | 扫用户二维码 / Scan user QR | Query: `id`(TSID) | 无 token→302；有 token→`type`,`id`(TSID),`nickname`,`gender`,`avatar`,`sign`,`region`,`isfriend`,`remark` |
| GET | /api/v1/user/qrcode | JWT | user_handler#qrcode | 扫用户二维码 / Scan user QR | Query: `id`(TSID) | 同 /api/v1/uqrcode（不会 302） |
| POST | /api/v1/user/update | JWT | user_handler#update | 修改个人信息单字段 / Update field | `field`,`value`（email/gender/allow_search 等白名单） | `{}` |
| GET | /api/v1/user/show | 公开 Open | user_handler#show | 获取公开信息 / Public info | Query: `id`(TSID) | `id`(TSID),`nickname`,`avatar`,`account`,`sign` |
| POST | /api/v1/user/change_state | JWT | user_handler#change_state | 切换在线/隐身 / Toggle state | `state`(默认 hide) | `{}` |
| POST | /api/v1/user/setting | JWT | user_handler#setting | 批量保存设置 / Save settings | `setting`(键值对列表) | `{}` |
| GET | /api/v1/user/credential | JWT | user_handler#credential | WebRTC TURN/STUN 凭证 / Credential | 无 | `ttl`=86400,`turn_urls`,`stun_urls`,`username`,`credential` |
| POST | /api/v1/user/change_password | JWT | user_handler#change_password | 修改密码 / Change password | 旧/新密码（见源码） | `{}` |
| POST | /api/v1/user/set_password | JWT | user_handler#set_password | 设置密码 / Set password | 新密码（见源码） | `{}` |
| POST | /api/v1/user/apply_logout | JWT | user_handler#apply_logout | 申请注销 / Apply logout | 见源码 | `{}`（恒成功） |
| POST | /api/v1/user/cancel_logout | JWT | user_handler#cancel_logout | 撤销注销 / Cancel logout | 见源码 | `{}` |
| POST | /api/v1/user/export_data | JWT | user_handler#export_data | 个人数据导出（占位）/ Export (placeholder) | 无 | 恒 501 ?ERR_NOT_IMPLEMENTED |
| GET | /api/v1/user/search | JWT | user_handler#search | 精确搜索用户 / Search user | Query: `keyword`；分页 `page`,`size` | `total`,`page`,`size`,`list`（命中项含用户列+`is_friend`+`remark`，`id` TSID） |

## 设备与推送 / Device & Push

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/user_device/page | JWT | user_device_handler#page | 设备档案分页 / Paged devices | 分页 `page`,`size` | 标准分页 `total`/`page`/`size`/`list` |
| POST | /api/v1/user_device/change_name | JWT | user_device_handler#change_name | 改设备名 / Rename device | `did`,`name` | `{}` |
| POST | /api/v1/user_device/delete | JWT | user_device_handler#delete | 删除设备 / Delete device | `did` | `{}` |
| GET | /api/v1/user_device/sessions | JWT | user_device_handler#sessions | 活跃会话（内存）/ Active sessions | 无 | `devices`(array),`count` |
| POST | /api/v1/user_device/check_login | JWT | user_device_handler#check_login | 检查登录冲突 / Check conflict | Body(JSON): `device_type` | `{conflict:bool[,conflict_device],message}` |
| POST | /api/v1/user_device/kick | JWT | user_device_handler#kick | 踢出设备 / Kick device | Body(JSON): `device_type`*,`device_id`* | `{message}` |
| POST | /api/v1/user_device/kick-others | JWT | user_device_handler#kick_others | 踢出其他设备 / Kick others | Body(JSON): `device_type`*,`device_id`* | `{message}` |
| POST | /api/v1/push/register | JWT | user_device_handler#push_register | 注册推送 Token / Register push | `device_id`*,`device_type`*,`platform`(fcm/apns)*,`token`* | `{}` |
| POST | /api/v1/push/unregister | JWT | user_device_handler#push_unregister | 注销推送 Token / Unregister push | `device_id`* | `{}` |

## 收藏 / Collect

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/user_collect/page | JWT | user_collect_handler#page | 收藏分页（可筛选）/ Paged collect | 分页；`kind`(0-7),`order`,`kwd`,`tag` | `total`/`page`/`size`/`list`（项含 `kind`,`kind_id`,`source`,`created_at`,`updated_at`,`tag`,`info`） |
| POST | /api/v1/user_collect/add | JWT | user_collect_handler#add | 添加收藏 / Add | `kind`,`kind_id`,`source`,`remark`,`info` | `{}` |
| POST | /api/v1/user_collect/remove | JWT | user_collect_handler#remove | 删除收藏 / Remove | `kind_id` | `{}`（恒成功） |
| POST | /api/v1/user_collect/change | JWT | user_collect_handler#change | 修改收藏 / Change | `action`,`kind_id`,... | `{}`（恒成功） |

## 用户标签 / User Tag

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/user_tag/page | JWT | user_tag_handler#page | 标签分页 / Tag page | `page`,`size`,`kwd`,`scene`(collect/friend) | 标签分页（见源码）；token 无效返回 ?ERR_TOKEN_INVALID |
| POST | /api/v1/user_tag/add | JWT | user_tag_handler#add | 新建标签（≤14字）/ Add tag | `scene`,`tag` | `tagId`(TSID) |
| POST | /api/v1/user_tag/change_name | JWT | user_tag_handler#change_name | 改标签名 / Rename tag | `scene`,`tagName`(≤14字),`tagId`(≥1) | `{}` |
| POST | /api/v1/user_tag/delete | JWT | user_tag_handler#delete | 删除标签 / Delete tag | `scene`,`tag` | `{}` |

## 标签关系 / User Tag Relation

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/user_tag_relation/collect_page | JWT | user_tag_relation_handler#collect_page | 收藏标签对象分页 / Collect-scene page | `page`,`size`,`kwd`,`tag_id`(≥1)* | `total`,`page`,`size`,`list`（项含 `kind`,`kind_id`,`source`,`created_at`,`updated_at`,`tag`,`info`） |
| GET | /api/v1/user_tag_relation/friend_page | JWT | user_tag_relation_handler#friend_page | 好友标签对象分页 / Friend-scene page | `page`,`size`,`kwd`,`tag_id`(≥1)* | 分页结构（见源码） |
| POST | /api/v1/user_tag_relation/add | JWT | user_tag_relation_handler#add | 给对象打标签 / Tag object | `scene`,`tag`(array,每项≤14字),`objectId` | `{}` |
| POST | /api/v1/user_tag_relation/set | JWT | user_tag_relation_handler#set | 批量设置对象标签 / Batch set | `scene`,`tagName`(≤14字),`tagId`(≥1),`objectIds`(array) | `{}` |
| POST | /api/v1/user_tag_relation/remove | JWT | user_tag_relation_handler#remove | 从标签移除对象 / Remove from tag | `scene`,`tagId`(≥1),`objectId` | `{}` |

## 好友 / Friend

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/friend/add | JWT | friend_handler#add_friend | 申请加好友 / Send request | `to`(TSID),`payload`,`created_at` | `{}` |
| POST | /api/v1/friend/confirm | JWT | friend_handler#confirm | 确认申请 / Confirm | `from`(TSID),`to`(TSID),`payload` | 好友信息 + `remark` + `source`（见源码） |
| POST | /api/v1/friend/reject | JWT | friend_handler#reject | 拒绝申请 / Reject | `from`(TSID)* | `{}` |
| POST | /api/v1/friend/delete | JWT | friend_handler#delete_friend | 删除好友 / Delete | `uid`(TSID) | `{}` |
| GET | /api/v1/friend/list | JWT | friend_handler#list | 好友列表 / Friend list | 无 | `mine`(map),`friend`(array，含 `id`/`from_user_id`/`to_user_id` TSID) |
| GET | /api/v1/friend/information | JWT | friend_handler#information | 好友/群组详情 / Info | `id`(TSID),`type`(friend/group) | friend: `id`(TSID),`nickname`,`account`,`gender`,`experience`,`avatar`,`sign`,`mine_uid`,`user_setting`；group/其他: `{}` |
| POST | /api/v1/friend/change_remark | JWT | friend_handler#change_remark | 改好友备注 / Change remark | `uid`(TSID),`remark` | `{remark}` |
| POST | /api/v1/friend/move | JWT | friend_handler#move | 移动好友到分组 / Move | `uid`(TSID),`category_id`(默认0) | `{}` |

## 黑名单 / Denylist

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/friend/denylist/add | JWT | user_denylist_handler#add | 加黑名单 / Add | `denied_user_id`(TSID) | `{user_id,denied_user_id,created_at}` |
| POST | /api/v1/friend/denylist/remove | JWT | user_denylist_handler#remove | 移除黑名单 / Remove | `denied_user_id`(TSID) | `{}` |
| GET | /api/v1/friend/denylist/page | JWT | user_denylist_handler#page | 黑名单分页 / Page | `page`,`size` | 标准分页（见源码） |

## 好友分类 / Friend Category

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/friend/category/add | JWT | friend_category_handler#add | 新增分组 / Add | `name`(默认 Unnamed) | `{id(TSID),name}` |
| POST | /api/v1/friend/category/delete | JWT | friend_category_handler#delete | 删除分组 / Delete | `id`(TSID) | `{}` |
| POST | /api/v1/friend/category/rename | JWT | friend_category_handler#rename | 重命名分组 / Rename | `id`(TSID),`name` | `{}` |

## 全文搜索 / Full-Text Search

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/fts/user_search | JWT | fts_handler#user_search | 搜索用户 / Search users | `keyword`,`page`,`size` | 分页；**结果项以 `uid` 为键**（非 `id`） |
| GET | /api/v1/fts/recently_user | JWT | fts_handler#recently_user | 最近可搜索用户 / Recent users | `keyword`,`page`,`size` | 分页（见源码） |
| GET | /api/v1/fts/msg | JWT | fts_handler#msg | 消息全文搜索 / Search messages | `keyword`,`type`(默认 C2C),`page`,`size`,`start_date`,`end_date`,`msg_type`,`from_uid`,`conversation_id`,`sort_by` | 分页；功能未启用返回 ?ERR_FEATURE_DISABLED |

## 位置 / Location

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/location/makeMyselfVisible | JWT（feature 门控）| location_handler#make_myself_visible | 设为可见 / Make visible | `latitude`,`longitude` | `{}` |
| POST | /api/v1/location/makeMyselfUnvisible | JWT（feature 门控）| location_handler#make_myself_unvisible | 设为不可见 / Make invisible | 无 | `{}` |
| GET | /api/v1/location/peopleNearby | JWT（feature 门控）| location_handler#people_nearby | 附近的人 / People nearby | `longitude`,`latitude`,`unit`(默认 m),`radius`(默认500),`limit`(默认100) | `radius`,`size`,`unit`,`list`（**不含坐标**，仅 `distance`） |

> 注：location 三端点执行前经 `imboy_plugin_registry:required_feature` 门控，feature 未启用返回错误。

---

# D. 群组 / Group

## 群组核心 / Group Core

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET / POST | /api/v1/group/remark | JWT | group_handler#remark | 获取/更新群备注 / Get or set remark | GET: `gid`(TSID)；POST: `gid`,`remark` | `{remark,gid}` / `{gid}` |
| GET | /api/v1/group/qrcode | JWT | group_handler#qrcode | 扫群二维码加入 / Join via QR | `id`(TSID),`exp`,`tk` | 群对象 `id`(TSID),`title`,`avatar`,`member_count`,`member_max`,`type`,`group_member`；校验失败 302 |
| GET | /api/v1/group/face2face | JWT | group_handler#face2face | 面对面建群 / Face-to-face | `longitude`,`latitude`,`code` | `gid`(TSID),`member_list` |
| POST | /api/v1/group/face2face_save | JWT | group_handler#face2face_save | 保存面对面建群 / Save F2F | `code`,`gid`(TSID) | `group`,`member_list` |
| POST | /api/v1/group/add | JWT | group_handler#add | 创建群（type=2 私有）/ Create group | `member_uids`(list[TSID]) | `group`,`member_list` |
| POST | /api/v1/group/edit | JWT | group_handler#edit | 编辑群信息 / Edit | `gid`(TSID),`title`/`avatar`/`introduction`(可选) | `{gid}` |
| POST | /api/v1/group/dissolve | JWT | group_handler#dissolve | 解散群（仅群主）/ Dissolve | `gid`(TSID) | `{gid}` |
| GET | /api/v1/group/detail | JWT | group_handler#detail | 群详情 / Group detail | `gid`(TSID) | 群对象（全字段 group_transfer） |
| GET | /api/v1/group/page | JWT | group_handler#page | 我的群列表（owner/join/manager）/ Paged groups | `attr`,`page`,`size` | `total`,`page`,`size`,`list` |
| GET | /api/v1/group/msg_page | JWT | group_handler#msg_page | 群消息分页 / Paged group msgs | `gid`(TSID),`last_time`,`page`,`size` | `total`,`page`,`size`,`list` |
| POST | /api/v1/group/transfer | JWT | group_handler#transfer | 群转让 / Transfer ownership | `gid`(TSID),`new_owner_uid`(TSID) | `{gid}` |

## 群成员 / Group Member

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group_member/join | JWT | group_member_handler#join | 邀请成员加入 / Invite | `gid`(TSID),`member_uids`(list[TSID])*,`join_mode` | `{gid,user_id_sum,member_list}` |
| POST | /api/v1/group_member/leave | JWT | group_member_handler#leave | 离群/移除成员 / Leave or remove | `gid`(TSID),`member_uids`(list[TSID]) | `{gid}` |
| GET | /api/v1/group_member/page | JWT | group_member_handler#page | 群成员分页 / Paged members | `gid`(TSID),`page`,`size` | `total`,`page`,`size`,`list`（成员含 `id`,`role`,`alias`,`mute_until` 等） |
| POST | /api/v1/group_member/alias | JWT | group_member_handler#alias | 设置群内昵称 / Set alias | `gid`(TSID),`alias`,`description` | `{gid}` |
| GET | /api/v1/group_member/same_group | JWT | group_member_handler#same_group | 两用户共同群 / Common groups | `uid1`(TSID),`uid2`(TSID) | `{count,list}` |
| POST | /api/v1/group_member/mute | JWT | group_member_handler#mute | 禁言成员 / Mute | `gid`(TSID),`user_id`(TSID),`duration`(秒>0) | `{gid,user_id}` |
| POST | /api/v1/group_member/unmute | JWT | group_member_handler#unmute | 解除禁言 / Unmute | `gid`(TSID),`user_id`(TSID) | `{gid,user_id}` |
| POST | /api/v1/group_member/role | JWT | group_member_handler#role | 更新成员角色（1-3）/ Update role | `gid`(TSID),`user_id`(TSID),`role`(1-3) | `{gid,user_id}` |

## 群分类 / Group Category

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group/category/create | JWT | group_category_handler#create | 创建分类 / Create | `category_name` | `{id(TSID),category_name}` |
| GET | /api/v1/group/category/list | JWT | group_category_handler#list | 分类列表 / List | 无 | `{categories[]}` |
| POST | /api/v1/group/category/rename | JWT | group_category_handler#rename | 重命名 / Rename | `id`(TSID),`category_name` | `{}` |
| POST | /api/v1/group/category/delete | JWT | group_category_handler#delete | 删除分类 / Delete | `id`(TSID) | `{}` |
| POST | /api/v1/group/category/move_group | JWT | group_category_handler#move_group | 移动群到分类 / Move group | `gid`(TSID),`category_id`(TSID) | `{}` |
| POST | /api/v1/group/category/sort | JWT | group_category_handler#sort | 批量排序 / Sort | `sort_orders`(list[{id,sort_order}]) | `{}` |

## 群标签 / Group Tag

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group/tag/add | JWT | group_tag_handler#add | 添加群标签 / Add tag | `gid`(TSID),`tag_name` | `{tag_id(TSID)}` |
| POST | /api/v1/group/tag/remove | JWT | group_tag_handler#remove | 删除群标签 / Remove tag | `gid`(TSID),`tag_name` | `{}` |
| GET | /api/v1/group/tag/list | JWT | group_tag_handler#list | 群标签列表 / List tags | `gid`(TSID) | `{list[]}` |
| GET | /api/v1/group/tag/search | JWT | group_tag_handler#search | 按标签搜群 / Search by tag | `tag_name` | `{list[]}` |
| GET | /api/v1/group/tag/hot | JWT | group_tag_handler#hot | 热门标签 / Hot tags | `limit`(默认20,1-100) | `{list[]}` |

## 群公告 / Group Notice

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group_notice/add | JWT | group_notice_handler#add | 创建公告 / Create | `gid`(TSID),`title`,`body`,`status`(0-2),`expired_at` | `{notice_id(TSID)}` |
| POST | /api/v1/group_notice/edit | JWT | group_notice_handler#edit | 编辑公告 / Edit | `notice_id`(TSID),`gid`,`status`,`title`,`body`,`expired_at` | `{notice_id}` |
| POST | /api/v1/group_notice/delete | JWT | group_notice_handler#delete | 删除公告（软删）/ Delete | `notice_id`(TSID) | `{notice_id}` |
| GET | /api/v1/group_notice/page | JWT | group_notice_handler#page | 公告分页（旧）/ Page (legacy) | `gid`(TSID),`page`,`size` | `list`+分页；项含 `notice_id`,`user_id`,`body`,`status`,`expired_at`,`created_at` |
| POST | /api/v1/group_notice/publish | JWT | group_notice_handler#publish | 发布并广播 / Publish | `notice_id`(TSID),`gid` | `{notice_id}` |
| GET | /api/v1/group_notice/latest | JWT | group_notice_handler#latest | 最新已发布公告 / Latest | `gid`(TSID) | array（0/1 项） |
| GET | /api/v1/group/notice/list | JWT | group_notice_handler#list | 公告列表（置顶排序）/ List | `gid`(TSID),`page`,`size` | `{total,page,size,items[]}` |
| GET | /api/v1/group/notice/detail | JWT | group_notice_handler#detail | 公告详情 / Detail | `notice_id`(TSID) | notice map（见 DS） |
| POST | /api/v1/group/notice/pin | JWT | group_notice_handler#pin | 置顶（群主/管理员）/ Pin | `notice_id`(TSID) | `{notice_id}` |
| POST | /api/v1/group/notice/unpin | JWT | group_notice_handler#unpin | 取消置顶 / Unpin | `notice_id`(TSID) | `{notice_id}` |
| POST | /api/v1/group/notice/mark_read | JWT | group_notice_handler#mark_read | 标记已读 / Mark read | `notice_id`(TSID) | notice map（见 DS） |

## 群投票 / Group Vote（feature 门控）

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group/vote/create | JWT | group_vote_handler#create | 创建投票 / Create | `gid`(TSID),`title`,`description`,`options`(array，二进制键 `option_text`/`sort_order`),`vote_type`(1单/2多),`is_anonymous`,`end_at` | `{vote_id,group_id(TSID),creator_id(TSID),title,vote_type,is_anonymous}` |
| GET | /api/v1/group/vote/list | JWT | group_vote_handler#list | 投票列表 / List | `gid`(TSID),`page`,`size` | `{total,page,size,list[]}` |
| GET | /api/v1/group/vote/detail | JWT | group_vote_handler#detail | 投票详情 / Detail | `vote_id` | vote map + `options[]`(含 `vote_count`) + `total_votes` |
| POST | /api/v1/group/vote/cast | JWT | group_vote_handler#cast | 投票 / Cast | `vote_id`,`option_ids`(array) | `{vote_id}` |
| POST | /api/v1/group/vote/update | JWT | group_vote_handler#update | 修改已投 / Update | `vote_id`,`option_ids` | `{vote_id}` |
| POST | /api/v1/group/vote/cancel | JWT | group_vote_handler#cancel | 取消我的投票 / Cancel | `vote_id` | `{vote_id}` |
| POST | /api/v1/group/vote/close | JWT | group_vote_handler#close | 结束投票 / Close | `vote_id` | `{vote_id}` |
| GET | /api/v1/group/vote/my_vote | JWT | group_vote_handler#my_vote | 我的投票记录 / My vote | `vote_id` | `{vote_id,option_ids,created_at}` |

> 注：`vote_id` 为字符串 ID（`vote` 前缀，非 TSID integer）；`option_id` 为 `opt` 前缀字符串。

## 群日程 / Group Schedule（feature 门控）

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group_schedule/create | JWT | group_schedule_handler#create | 创建日程 / Create | `group_id`(TSID),`title`,`description`,`location`,`start_at`,`end_at`,`remind_before`(默认15),`participant_ids`(list[TSID]) | `{schedule_id,id}` |
| POST | /api/v1/group_schedule/update | JWT | group_schedule_handler#update | 修改日程（仅创建者）/ Update | `schedule_id`,`title`,`description`,`location`,`start_at`,`end_at` | `{schedule_id}` |
| POST | /api/v1/group_schedule/cancel | JWT | group_schedule_handler#cancel | 取消日程（仅创建者）/ Cancel | `schedule_id` | `{schedule_id}` |
| GET | /api/v1/group_schedule/detail | JWT | group_schedule_handler#detail | 日程详情（含参与人）/ Detail | `schedule_id` | `{schedule,participants[],participant_count}` |
| GET | /api/v1/group_schedule/list | JWT | group_schedule_handler#list | 群日程列表 / Group list | `group_id`(TSID),`start_at`,`end_at`,`page`,`size` | `{list[],total,page,size}` |
| GET | /api/v1/group_schedule/my_list | JWT | group_schedule_handler#my_list | 我的日程列表 / My list | `start_at`,`end_at`,`page`,`size` | `{list[],page,size}`（无 total） |
| POST | /api/v1/group_schedule/confirm | JWT | group_schedule_handler#confirm | 确认/拒绝参与 / Confirm | `schedule_id`,`accept`(默认 true) | `{schedule_id}` |

> 注：`schedule_id` 为 `sched_` 前缀字符串 ID（非 TSID integer）。

## 群相册 / Group Album

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group_album/create | JWT | group_album_handler#create_album | 创建相册 / Create album | `gid`*,`album_name`*,`cover_photo_id` | `{id(TSID),gid,...}` |
| GET | /api/v1/group_album/list | JWT | group_album_handler#list_albums | 相册列表 / List albums | `gid`*,`page`,`size` | `{list,total,page,size}` |
| POST | /api/v1/group_album/rename | JWT | group_album_handler#rename_album | 重命名相册 / Rename | `album_id`*,`album_name`* | `{}` |
| POST | /api/v1/group_album/delete | JWT | group_album_handler#delete_album | 删除相册 / Delete | `album_id`* | `{}` |
| POST | /api/v1/group_album/photo/upload | JWT | group_album_handler#upload_photo | 上传图片 / Upload | multipart 或 JSON: `gid`,`album_id`,`photo`,`photo_name` | `{id(TSID),gid,album_id,...}` |
| POST | /api/v1/group_album/photo/batch | JWT | group_album_handler#batch_upload | 批量上传 / Batch upload | `gid`*,`photos`(array) | `{results[]}`（每项 `{ok,PhotoData}`/`{error}`） |
| GET | /api/v1/group_album/photo/list | JWT | group_album_handler#list_photos | 图片列表 / List photos | `album_id`*,`page`,`size` | `{list,total,page,size}` |
| GET | /api/v1/group_album/photo/detail | JWT | group_album_handler#photo_detail | 图片详情 / Photo detail | `photo_id`* | photo map |
| POST | /api/v1/group_album/photo/delete | JWT | group_album_handler#delete_photo | 删除图片 / Delete photo | `photo_id`* | `{}` |
| POST | /api/v1/group_album/photo/like | JWT | group_album_handler#like_photo | 点赞图片 / Like | `photo_id`* | `{}` |
| POST | /api/v1/group_album/photo/unlike | JWT | group_album_handler#unlike_photo | 取消点赞 / Unlike | `photo_id`* | `{}` |
| POST | /api/v1/group_album/photo/comment | JWT | group_album_handler#add_comment | 添加评论 / Add comment | `photo_id`*,`content`* | `{}` |
| GET | /api/v1/group_album/photo/comments | JWT | group_album_handler#list_comments | 评论列表 / List comments | `photo_id`*,`limit`(默认20) | `{comments[]}`（含 `user_id` TSID） |
| POST | /api/v1/group_album/cover/update | JWT | group_album_handler#update_cover | 更新封面 / Update cover | `album_id`*,`photo_id`* | `{}` |

## 群文件 / Group File

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group/file/upload | JWT | group_file_handler#upload | 上传文件 / Upload | multipart: `gid`*,`file`*,`file_name`*,`file_type` | `{file_id(TSID),file_name,file_size,file_type,file_category,created_at}` |
| GET | /api/v1/group/file/download | JWT | group_file_handler#download | 下载（302）/ Download | `file_id`* | HTTP 302 `Location` |
| GET | /api/v1/group/file/list | JWT | group_file_handler#list | 文件列表 / List | `gid`*,`page`,`size`,`category` | `{items,total,page,size}` |
| POST | /api/v1/group/file/delete | JWT | group_file_handler#delete | 删除文件 / Delete | `file_id`* | `{deleted:true}` |
| GET | /api/v1/group/file/search | JWT | group_file_handler#search | 搜索文件 / Search | `gid`*,`keyword`*,`page`,`size` | `{items}`（无分页字段） |
| GET | /api/v1/group/file/categories | JWT | group_file_handler#categories | 分类统计 / Category stats | `gid`* | `{items[]}`（`category`,`count`,`total_size`） |

## 群作业 / Group Task（feature 门控）

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/group/task/create | JWT | group_task_handler#create | 创建作业 / Create | `group_id`(TSID)*,`title`*,`user_ids`(array),`description`,`deadline`,`attachment` | `{task_id(TSID)}` |
| POST | /api/v1/group/task/update | JWT | group_task_handler#update | 更新作业 / Update | `task_id`*,`status`(0-3),`title`,`description`,`deadline`,`attachment` | `{task_id}` |
| POST | /api/v1/group/task/assign | JWT | group_task_handler#assign | 分配作业 / Assign | `task_id`*,`user_ids`(array) | `{task_id}` |
| POST | /api/v1/group/task/submit | JWT | group_task_handler#submit | 提交作业 / Submit | `task_id`*,`content`,`attachment(s)` | `{task_id}`(task_uid binary) |
| POST | /api/v1/group/task/review | JWT | group_task_handler#review | 批改作业 / Review | `assignment_id`*,`score`,`comment` | `{assignment_id}` |
| GET | /api/v1/group/task/list | JWT | group_task_handler#list | 作业列表 / List | `group_id`(TSID)*,`status`,`assignee_id`(默认当前;`all`),`page`,`size` | `{list[],page,size}` |
| GET | /api/v1/group/task/detail | JWT | group_task_handler#detail | 作业详情 / Detail | `task_id`* | task map |
| GET | /api/v1/group/task/my | JWT | group_task_handler#my_tasks | 我的作业 / My tasks | `status`,`page`,`size` | `{list[],page,size}` |
| GET | /api/v1/group/task/pending | JWT | group_task_handler#pending_review | 待批改作业 / Pending review | `task_id`*,`page`,`size` | `{list[],page,size}` |

---

# E. 频道 / Channel

> 频道字段完整结构详见 [channel_api_contract_v1.md](./contracts/channel_api_contract_v1.md)。频道对象顶层含 `id`(TSID)、`name`、`type`(smallint 0\|1\|2)、`description`、`avatar`、`custom_id`、`tags`、`creator_uid`(TSID) 等。路径参数“path 优先、body 回退”。

## 频道核心 / Channel Core

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/channel/create | JWT | channel_handler#create | 创建频道（上限20）/ Create | `name`*,`type`(0\|2,默认0),`description`,`avatar`,`custom_id`,`tags` | Channel object |
| GET | /api/v1/channel/:channel_id | JWT | channel_handler#show | 频道详情 / Get | path `channel_id`* | Channel object |
| GET | /api/v1/channel/by_custom_id/:custom_id | JWT | channel_handler#by_custom_id | 按自定义ID获取 / Get by custom id | path `custom_id`* | Channel object |
| PUT/POST | /api/v1/channel/:channel_id/update | JWT | channel_handler#update | 更新频道 / Update | path `channel_id`；body 其余字段透传 | Channel object |
| POST | /api/v1/channel/:channel_id/delete | JWT | channel_handler#delete | 删除频道 / Delete | path `channel_id` | `{}` |
| POST | /api/v1/channel/:channel_id/subscribe | JWT | channel_handler#subscribe | 订阅 / Subscribe | path `channel_id` | `{}` |
| POST | /api/v1/channel/:channel_id/unsubscribe | JWT | channel_handler#unsubscribe | 取消订阅 / Unsubscribe | path `channel_id` | `{}` |
| GET | /api/v1/channels/subscribed | JWT | channel_handler#subscribed | 我订阅的频道 / Subscribed | `cursor`,`limit`(默认50) | `{list,cursor,limit}`（cursor/limit 仅回显） |
| GET | /api/v1/channels/managed | JWT | channel_handler#managed | 我管理的频道 / Managed | 无 | `{list}` |
| GET | /api/v1/channels/unread/summary | JWT | channel_handler#unread_summary | 未读聚合 / Unread summary | 无 | `{total_unread,unread_channels,channels[]}` |
| GET | /api/v1/channels/sync | JWT | channel_handler#sync | 增量同步 / Incremental sync | `since`(默认0) | `{channels,server_time}` |

## 频道消息 / Channel Messages

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/channel/:channel_id/message | JWT | channel_handler#publish_message | 发布消息 / Publish | path `channel_id`；body `content`*,`msg_type`(默认 text),`payload` | Message object（`id`/`channel_id` TSID） |
| GET | /api/v1/channel/:channel_id/messages | JWT | channel_handler#messages | 消息列表 / List | path `channel_id`*；`cursor`(默认0),`limit`(默认20,1-200) | `{list}` |
| POST | /api/v1/channel/:channel_id/read | JWT | channel_handler#mark_read | 标记已读 / Mark read | path `channel_id`；body `message_id` | `{}` |
| POST | /api/v1/channel/:channel_id/message/:message_id/view | JWT | channel_handler#record_view | 记录阅读 / Record view | path `channel_id`,`message_id`* | `{}` |
| POST | /api/v1/channel/:channel_id/message/:message_id/reaction | JWT | channel_handler#add_reaction | 添加反应 / Add reaction | path 同上；body `reaction_type`(默认 like) | `{}` |
| DELETE | /api/v1/channel/:channel_id/message/:message_id/reaction/:reaction_type | JWT | channel_handler#remove_reaction | 移除反应 / Remove reaction | path `channel_id`,`message_id`,`reaction_type` | `{}` |
| PUT/POST | /api/v1/channel/:channel_id/message/:message_id/pin | JWT | channel_handler#pin_message | 置顶/取消置顶 / Pin | path `message_id`*；body `pinned`(默认true) | Message object |
| DELETE | /api/v1/channel/:channel_id/message/:message_id/delete | JWT | channel_handler#delete_message | 删除消息 / Delete | path `message_id`* | `{}` |
| POST | /api/v1/channel/:channel_id/message/:message_id/revoke | JWT | channel_handler#revoke_message | 撤回消息 / Revoke | path `channel_id`,`message_id`* | `{}` |

## 频道管理员与订阅者 / Channel Admins & Subscribers

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/channel/:channel_id/admin | JWT | channel_handler#add_admin | 添加管理员 / Add admin | path `channel_id`；body `user_id`(TSID)*,`role`(1-3,默认1) | `{}` |
| GET | /api/v1/channel/:channel_id/admins | JWT | channel_handler#admins | 管理员列表 / Admins | path `channel_id`* | `{list}`（项含 `user_id`(TSID),`role`,`nickname`,`avatar`） |
| PUT | /api/v1/channel/:channel_id/admin/:user_id/role | JWT | channel_handler#update_admin_role | 更新管理员角色 / Update role | path `channel_id`,`user_id`；body `role`(1-3) | `{}` |
| DELETE/PUT | /api/v1/channel/:channel_id/admin/:user_id | JWT | channel_handler#remove_admin | 移除管理员（PUT 兼容转改角色）/ Remove admin | path `channel_id`,`user_id` | `{}` |
| GET | /api/v1/channel/:channel_id/subscribers | JWT | channel_handler#subscribers | 订阅者列表 / Subscribers | path `channel_id`*；`cursor`,`limit`(默认50) | `{list,cursor,limit}`（项含 `user_id`(TSID),`nickname`,`avatar`） |
| DELETE | /api/v1/channel/:channel_id/subscriber/:user_id | JWT | channel_handler#remove_subscriber | 移除订阅者 / Remove subscriber | path `channel_id`,`user_id`* | `{}` |

## 频道邀请与订单 / Channel Invitation & Order

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/channel/:channel_id/invitation | JWT | channel_handler#create_invitation | 创建邀请（私有）/ Create invitation | path `channel_id`；body `invitee_uid`(TSID)* | Invitation object |
| POST | /api/v1/channel/invitation/accept | JWT | channel_handler#accept_invitation | 接受邀请 / Accept | `invitation_id`(TSID)* | `{}` |
| POST | /api/v1/channel/invitation/reject | JWT | channel_handler#reject_invitation | 拒绝邀请 / Reject | `invitation_id`(TSID)* | `{}` |
| GET | /api/v1/channel/invitations/my | JWT | channel_handler#my_invitations | 我收到的邀请 / My invitations | 无 | `{list}` |
| GET | /api/v1/channel/invitations/sent | JWT | channel_handler#sent_invitations | 我发出的邀请 / Sent invitations | 无 | `{list}` |
| POST | /api/v1/channel/:channel_id/order | JWT | channel_handler#create_order | 创建订单（付费）/ Create order | path `channel_id` | Order object（`order_no`,`channel_id`(TSID),金额,状态） |
| POST | /api/v1/channel/order/pay | JWT | channel_handler#pay_order | 支付订单（模拟）/ Pay | `order_no`* | `{}` |
| GET | /api/v1/channel/orders/my | JWT | channel_handler#my_orders | 我的订单 / My orders | 无 | `{list}` |
| GET | /api/v1/channel/order/:order_no | JWT | channel_handler#get_order | 订单详情 / Get order | path `order_no`* | Order object |

## 频道统计与发现 / Channel Stats & Discover

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/channels/search | JWT | channel_handler#search | 搜索频道 / Search | `keyword`,`limit`(默认20) | `{list}`（keyword 空返回 `[]`） |
| GET | /api/v1/channels/discover | JWT | channel_handler#discover | 发现/推荐频道 / Discover | `limit`(默认20),`category`(未用) | `{list}` |
| GET | /api/v1/channel/:channel_id/stats | JWT | channel_handler#stats | 频道统计 / Stats | path `channel_id`* | Stats object |
| GET | /api/v1/channel/:channel_id/stats/daily | JWT | channel_handler#stats_daily | 每日统计 / Daily stats | path `channel_id`*；`days`(默认7,1-365) | `{list}` |

---

# F. 内容与互动 / Content & Interaction

## 朋友圈 / Moment

> 详见 [moment_api_contract_v1.md](./contracts/moment_api_contract_v1.md)。feed/user_posts/comments 经 `enrich_*` 批量补全作者昵称/头像/liked；`post_transfer` 将 `like_count`/`comment_count` 收敛到 `stats`。

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/moment/create | JWT | moment_handler#create | 发布动态 / Create post | `content`,`media`(≤9),`visibility`(0-4,默认1),`allow_comment`,`allow_uids`,`deny_uids` | post_transfer：`id`(TSID),`author_uid`(TSID),`content`,`media`,`visibility`,`created_at`,`stats{like_count,comment_count}` |
| POST | /api/v1/moment/:moment_id | JWT | moment_handler#show | 查看动态 / Get post | path/body `moment_id`(TSID) | post_transfer + `liked` |
| POST | /api/v1/moment/:moment_id/delete | JWT | moment_handler#delete | 删除动态 / Delete | `moment_id`(TSID) | `{}` |
| GET | /api/v1/moments/feed | JWT | moment_handler#feed | 信息流 / Feed | `cursor`(默认0),`limit`(默认20,1-100) | `{list,cursor,limit}`；项含 `author_nickname`,`author_avatar`,`liked` |
| GET | /api/v1/moments/user/:uid | JWT | moment_handler#user_posts | 用户动态列表 / User posts | path/qs `uid`(TSID),`cursor`,`limit` | 同 feed |
| POST | /api/v1/moment/:moment_id/like | JWT | moment_handler#like | 点赞 / Like | `moment_id`(TSID) | `{}` |
| POST | /api/v1/moment/:moment_id/unlike | JWT | moment_handler#unlike | 取消点赞 / Unlike | `moment_id`(TSID) | `{}` |
| POST | /api/v1/moment/:moment_id/comment | JWT | moment_handler#add_comment | 添加评论 / Add comment | `moment_id`(TSID),`content`(≤500字)*,`reply_to_uid`(TSID) | comment_transfer：`id`(TSID),`moment_id`(TSID),`user_id`(TSID),`content`,`created_at` |
| POST | /api/v1/moment/:moment_id/comments | JWT | moment_handler#comments | 评论列表 / List comments | `moment_id`(TSID),`cursor`,`limit`(默认20) | `{list,cursor,limit}`；项含 `user_nickname`,`user_avatar`,`reply_to_nickname` |
| POST | /api/v1/moment/:moment_id/comment/:comment_id/delete | JWT | moment_handler#delete_comment | 删除评论 / Delete comment | path/body `comment_id`(TSID),`moment_id` | `{}` |
| POST | /api/v1/moment/:moment_id/report | JWT | moment_handler#report | 举报动态 / Report | `moment_id`(TSID),`reason`*,`description` | `{report_id(TSID)}` |

## 举报 / Report

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/report/create | JWT | report_handler#create | 通用举报（自动识别类型）/ Create report | `target_type`(moment/group/channel/user),`target_id`(TSID)（或 `moment_id`/`group_id`/...）,`reason`(≤64)*,`description`(≤500) | `{report_id(TSID),target_type}`；moment 类型返回 `{report_id}` |
| POST | /api/v1/moment/report/create | JWT | report_handler#moment_create | 举报动态（固定 moment）/ Report moment | `target_id`/`moment_id`(TSID),`reason`(≤64)*,`description`(≤500) | `{report_id(TSID)}` |

## 直播间 / Live Room

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/live_room/list | JWT | live_room_handler#list | 直播中列表 / Active rooms | 分页 `page`,`size` | `{list[],...}`；room 含 `id`,`user_id`,`title`,`cover`,`stream_key`,`status`,`viewer_count` |
| GET | /api/v1/live_room/my_list | JWT | live_room_handler#my_list | 我的直播间 / My rooms | 分页 | 同 list |
| POST | /api/v1/live_room/create | JWT | live_room_handler#create | 创建直播间 / Create | `title`(≤100B)*,`cover`(≤255B) | room（`status`=0,`viewer_count`=0） |
| POST | /api/v1/live_room/start | JWT | live_room_handler#start | 开始直播 / Start | `room_id` | `{}`（仅房主） |
| POST | /api/v1/live_room/stop | JWT | live_room_handler#stop | 停止直播 / Stop | `room_id` | `{}`（仅房主） |
| GET | /api/v1/live_room/detail | JWT | live_room_handler#detail | 直播间详情 / Detail | `room_id` | room（非房主移除 `stream_key`） |

## 钱包 / Wallet

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/wallet/balance | JWT | wallet_handler#balance | 查询余额 / Balance | 无 | `{balance(分),balance_yuan(元),frozen(分)}` |
| GET | /api/v1/wallet/transactions | JWT | wallet_handler#transactions | 流水分页 / Transactions | 分页 `page`,`size` | 分页信封（顶层 `list`+page/total/size） |
| POST | /api/v1/wallet/topup | JWT | wallet_handler#topup | 模拟充值 / Topup | `amount`(分,100-1000000) | `{balance,balance_yuan,reference_no}` |

## 附件 / Attachment

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/attachment/presign | JWT | attach_handler#presign | 生成 S3 直传 presigned URL / Presign upload | `filename`(默认 file),`mime_type`,`expires`(60-86400,默认3600) | `{put_url,object_key,expires_at}`（非 GET 返回 405） |

## 反馈 / Feedback

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/feedback/page | JWT | feedback_handler#page | 我的反馈分页 / Feedback page | 分页 | `list`+分页；项含 `feedback_id`,`type`,`rating`,`body`,`attach`,`reply_count`,`status`,`created_at` |
| POST | /api/v1/feedback/add | 可选 Optional | feedback_handler#add | 提交反馈 / Add | Header `cos`/`vsn`/`did`；body `type`,`rating`,`contact_detail`,`content`/`description`*,`screenshot` | 成功（无 payload） |
| POST | /api/v1/feedback/remove | JWT | feedback_handler#remove | 删除反馈 / Remove | `feedback_id`(int) | 成功（无 payload） |
| GET | /api/v1/feedback/page_reply | JWT | feedback_handler#page_reply | 反馈回复分页 / Reply page | `feedback_id`*+分页 | `list`+分页；项含 `feedback_reply_id`,`feedback_id`,`replier_name`,`body`,`created_at` |

> 注：`/api/v1/feedback/change` 与 `/api/v1/feedback/reply` 路由已注册，但 handler 当前未实装（命中 `false` 分支原样返回），暂不可用。

---

# G. E2EE 端到端加密 / E2EE

> 所有 E2EE 端点先经 `imboy_policy:e2ee_enabled()` 门控，关闭时返回 ?ERR_FEATURE_DISABLED。社交恢复分片详见 [e2ee_server_persisted_shard_contract_v1.md](./contracts/e2ee_server_persisted_shard_contract_v1.md)。

## E2EE 密钥与备份 / Keys & Backup

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/e2ee/user_keys | JWT | e2ee_handler#user_keys | 查目标用户设备公钥 / User device keys | `uid`(TSID)* | 设备公钥（见源码） |
| GET | /api/v1/e2ee/group_member_keys | JWT | e2ee_handler#group_member_keys | 查群成员公钥 / Group member keys | `gid`(TSID)* | 成员公钥（见源码） |
| POST | /api/v1/e2ee/report_device_key | JWT | e2ee_handler#report_device_key | 上报设备公钥 / Report device key | `device_id`*,`device_type`*,`device_name`,`public_key`*,`key_id`* | `{success:true}` |
| GET | /api/v1/e2ee/key/status | JWT | e2ee_handler#key_status | 密钥状态与恢复方式 / Key status | `device_id`* | 状态（含 `has_valid_key` 等，见源码） |
| GET | /api/v1/e2ee/notifications/pull | JWT | e2ee_handler#pull_notifications | 增量拉取密钥变更通知 / Pull notifications | `since`(默认"0"),`limit`(默认50) | `{notifications[],count}` |
| POST | /api/v1/e2ee/recovery/start | JWT | e2ee_handler#start_recovery | 启动自动密钥恢复 / Start recovery | `device_id`*,`method` | 恢复结果（见源码） |
| GET | /api/v1/e2ee/backup/list | JWT | e2ee_handler#backup_list | 备份历史列表 / List backups | 无 | `{list[]}` |
| POST | /api/v1/e2ee/backup/delete | JWT | e2ee_handler#backup_delete | 删除备份（仅本人）/ Delete backup | `backup_id`(TSID)* | `{deleted:true}` |
| GET | /api/v1/e2ee/compliance_key | JWT | e2ee_handler#compliance_key | 获取活跃合规公钥 / Compliance key | 无 | `{key_id,public_key}` |

## E2EE 设备迁移 / Device Transfer

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| POST | /api/v1/e2ee/transfer/create | JWT | e2ee_transfer_handler#create | 创建密钥传输会话 / Create transfer | `to_uid`(TSID)* | `{session_id,expires_at}` |
| POST | /api/v1/e2ee/transfer/accept | JWT | e2ee_transfer_handler#accept | 接受并获取密钥包 / Accept | `session_id`*,`device_id` | `{session_id,from_uid(TSID),from_device_id,encrypted_key_bundle,status,expires_at}` |
| POST | /api/v1/e2ee/transfer/confirm | JWT | e2ee_transfer_handler#confirm | 确认传输完成 / Confirm | `session_id`* | `{message}` |
| POST | /api/v1/e2ee/transfer/cancel | JWT | e2ee_transfer_handler#cancel | 取消传输 / Cancel | `session_id`* | `{message}` |
| GET | /api/v1/e2ee/transfer/info | JWT | e2ee_transfer_handler#info | 查询会话信息 / Info | `session_id`* | `{session_id,from_uid(TSID),from_device_id,status,expires_at}` |
| GET | /api/v1/e2ee/transfer/pending | JWT | e2ee_transfer_handler#pending | 待处理传输列表 / Pending | 无 | `{transfers[]}`（含 `from_uid`(TSID) 等） |

## E2EE 社交恢复 / Social Recovery

| 方法 Method | 路径 Path | 鉴权 Auth | Handler#action | 用途 Purpose（中 / EN） | 请求参数 Request | 响应载荷 Response payload |
|---|---|---|---|---|---|---|
| GET | /api/v1/e2ee/social/contacts | JWT | e2ee_social_handler#contacts | 可信联系人列表 / List trusted | 无 | `{contacts[]}` |
| POST | /api/v1/e2ee/social/contacts/add | JWT | e2ee_social_handler#add_contact | 添加可信联系人（须好友）/ Add | `contact_uid`(TSID)*,`nickname` | `{message}` |
| POST | /api/v1/e2ee/social/contacts/remove | JWT | e2ee_social_handler#remove_contact | 移除可信联系人 / Remove | `contact_uid`(TSID)* | `{message}` |
| POST | /api/v1/e2ee/social/create_shards | JWT | e2ee_social_handler#create_shards | 创建社交恢复分片 / Create shards | `total_shards`(默认3),`threshold`(默认2),`proxies`(array) | `{key_version,total_shards,threshold,shards[]}` |
| GET | /api/v1/e2ee/social/shards | JWT | e2ee_social_handler#get_shards | 获取自己的分片 / Get shards | `key_version`(默认 latest) | `{shards[]}` |
| POST | /api/v1/e2ee/social/recover | JWT | e2ee_social_handler#recover_key | 用分片重组私钥 / Recover key | `decrypted_shards`(≥2) | `{message}` |
| GET | /api/v1/e2ee/social/proxy_shards | JWT | e2ee_social_handler#get_proxy_shards | 作为代理持有的分片 / Proxy shards | 无 | `{shards[]}` |
| POST | /api/v1/e2ee/social/decrypt_shard | JWT | e2ee_social_handler#decrypt_shard | 代理解密所托管分片 / Decrypt shard | `shard_id`* | `{decrypted_shard}` |

---

## 附录：标注约定 / Appendix: Annotation Conventions

- `*` 表示必填参数 / required parameter.
- `(TSID)` 表示该字段为 64 位 TSID，以 JSON integer 传输 / 64-bit TSID transmitted as JSON integer.
- `见源码` / `TBD` 表示该响应子结构由对应 logic/ds 层决定，handler 仅透传，未在本目录展开精确字段；如需逐列字段请查对应 `*_logic`/`*_ds` 或域契约文档。
- “feature 门控”表示该域端点受插件/功能开关（`imboy_plugin_registry:required_feature` 或 `imboy_policy`）约束，关闭时返回功能禁用错误。
- HTTP 方法判定依据：handler 用 `elib_param:post`/`cowboy_req:read_body`/`elib_req:body` 读取请求体 → POST；仅用 `cowboy_req:parse_qs`/`qs_val`/`elib_param:page` → GET；RESTful 资源端点结合 `cowboy_req:method` 与路由约定标注。Cowboy 路由本身不绑定方法，最终方法约束以客户端实际调用与中间件为准。

---

## 变更记录 / Changelog

| 日期 Date | 内容 Content |
|---|---|
| 2026-07-08 | 同步 43224c1f/4cc20e81 硬切换：全文档 `/v1/*` → `/api/v1/*`，与 `src/imboy_router.erl` 当前真实路由对齐 |
| 2026-06-02 | 初版：并行审计 30 个 handler 真实源码，建立完整 `/api/v1/*` 端点总目录（约 130+ 端点，按 7 大类分域），交叉引用 channel/moment/e2ee/ws 详细契约 |
