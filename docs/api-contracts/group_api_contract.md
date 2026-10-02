# 群组链路 API 契约（C 端 detail + 管理端列表/详情）

> 状态：存量契约固化（2026-08-20 以 backend 代码为权威源整理）。C 端前缀 `/api/v1`（用户登录态）；管理端前缀 `/api/adm`（管理员登录态，读权限 `groups:read`）。

## 1. 端点表

| 端点 | 方法 | 说明 | 权威源 |
|---|---|---|---|
| `/api/v1/group/detail?gid=<group_id>` | GET | 群详情，返回 group 表全列 | `src/api/group_handler.erl:65-87`、路由 `src/imboy_router.erl:245` |
| `/api/adm/group/list` | GET | 管理端群组分页（status/type/keyword 筛选） | `src/adm/adm_group_handler.erl:58-74`、路由 `imboy_router.erl:692` |
| `/api/adm/group/detail?gid=<id>` | GET | 管理端群详情（含 owner 用户对象） | `src/adm/adm_group_handler.erl:76-103` |

## 2. `GET /api/v1/group/detail`（C 端）

实现：`group_logic:find_by_id(Gid2, <<"*">>)` → `group_logic:group_transfer(G)`，`group_transfer/1` 为**恒等函数**（`src/logic/group_logic.erl:42-43`），即原样返回 group 表全列、无字段增删。列定义权威源：`priv/migrations/00000001_foundation.up.sql:4261-4282`。

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `id` | int | 否 | 群 TSID（JSON number 直出，未 string 化） | foundation.up.sql:4262 |
| `type` | int | 否 | 1 公开群组 2 私有群组 | :4263 |
| `join_limit` | int | 否 | 加入限制：1 不需审核 2 需要审核 3 只允许邀请加入 | :4264 |
| `content_limit` | int | 否 | 内部发布限制：1 圈内不需审核 2 圈内需要审核 3 圈外需要审核 | :4265 |
| `owner_uid` | int | 否 | 群主用户 TSID（JSON number） | :4267 |
| `creator_uid` | int | 否 | 创建者用户 TSID（JSON number） | :4268 |
| `member_max` | int | 否 | 最大成员数（默认 1000，CHECK >0） | :4269 |
| `member_count` | int | 否 | 当前成员数（默认 1，CHECK >=0） | :4270 |
| `introduction` | string | 否 | 简介（varchar 2000，默认 ''） | :4271 |
| `avatar` | string | 否 | 群头像（varchar 320，默认 ''） | :4272 |
| `title` | string | 否 | 群名（varchar 200，默认 ''） | :4273 |
| `chat_aes_key` | string | 否 | 群聊 AES key（varchar 2048，默认 ''）——敏感字段，见 §5.1 | :4274 |
| `status` | int | 否 | -1 删除 0 禁用 1 启用（CHECK ∈ {-1,0,1}） | :4275 |
| `updated_at` | int | 是 | **毫秒 int**（REST 信封统一转换） | :4276 + `elib_response.erl:30-32` |
| `created_at` | int | 否 | **毫秒 int** | :4277 + `elib_response.erl:30-32` |

## 3. `GET /api/adm/group/list`（管理端）

请求参数（`adm_group_handler.erl:65-70`）：`page`（默认 1）、`size`、`status`（int，-1=不过滤）、`type`（int，-1=不过滤）、`keyword`（模糊匹配 title/introduction，`:275-282`）。

响应为分页信封 `{total, page, size, list}`（`elib_pg:page_with_total`，`src/lib/elib_pg.erl:509-531`）。`list[]` 行列集权威源 `src/repo/group_repo.erl:141-146`（page/4 Column）：

```
id, title, avatar, owner_uid, creator_uid, type, join_limit, member_count, introduction, status, created_at
```

行出站前经 `normalize_group`：**TSID 键 `id`/`owner_uid`/`creator_uid` 转字符串**（`adm_group_handler.erl:295-302`，`elib_id:tsid_keys_to_bin/2`）。

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `id` | string | 否 | 群 TSID（string，防 JS 精度丢失） | group_repo.erl:143 + adm_group_handler.erl:300-302 |
| `title` | string | 否 | 群名 | group_repo.erl:143 |
| `avatar` | string | 否 | 群头像 | group_repo.erl:143 |
| `owner_uid` | string | 否 | 群主 TSID（string） | group_repo.erl:143 + adm normalize |
| `creator_uid` | string | 否 | 创建者 TSID（string） | group_repo.erl:143 + adm normalize |
| `type` | int | 否 | 1 公开 2 私有 | group_repo.erl:143 |
| `join_limit` | int | 否 | 加入限制 1/2/3 | group_repo.erl:143 |
| `member_count` | int | 否 | 当前成员数 | group_repo.erl:143 |
| `introduction` | string | 否 | 简介 | group_repo.erl:143 |
| `status` | int | 否 | -1/0/1 | group_repo.erl:143 |
| `created_at` | int | 否 | 毫秒 int（REST 信封转换） | group_repo.erl:143 + elib_response |

排序 `created_at DESC`（`adm_group_handler.erl:71`）。

## 4. `GET /api/adm/group/detail`（管理端）

列集（`adm_group_handler.erl:86-87`）：`id,title,avatar,introduction,owner_uid,creator_uid,member_count,member_max,type,join_limit,status,created_at`（比 list 多 `member_max`），TSID 同样转 string；另附加：

| 字段 | 类型 | 可空 | 语义 | 权威源 |
|---|---|---|---|---|
| `owner` | object | 否 | `{id(string TSID), nickname, avatar}`，群主用户摘要 | adm_group_handler.erl:92-95 |

## 5. 前端消费方

| 端 | 文件 | 说明 |
|---|---|---|
| Flutter | `imboyapp/lib/store/model/group_model.dart:56-81`（GroupModel.fromJson） | 消费 C 端 detail：`group_id ?? id ?? gid`、`type/join_limit/content_limit/owner_uid/creator_uid/member_max/member_count/introduction/avatar/title/status/updated_at/created_at`（`user_id_sum` 已于迁移 00000079 退役，前后端同步删除）；时间戳经 `DateTimeHelper.parseTimestamp` 双格式容错 |
| admin | `imboyadmin/src/types/group.ts`（`Group` 接口） | TSID 用 `EntityId`（string）；字段与 §3/§4 列集一致 |
| admin | `imboyadmin/src/modules/groups/api/public.ts`（`/group/list` 请求参数 page/size/status/type/keyword，类型 `Group`） | 列表/详情请求构造 |

## 6. 已知漂移与注意事项（登记，不改代码）

1. **【漂移】admin `Group.member_max` 在列表页恒 undefined**：TS 类型（`group.ts:11`）声明了 `member_max?`，但 `/api/adm/group/list` SQL 列不含 `member_max`（仅 detail 含，`group_repo.erl:143` vs `adm_group_handler.erl:87`）。列表页如展示“上限”需自行兜底。
2. **【漂移】C 端 detail 的 TSID 为 JSON number**：`group_transfer` 恒等直出，`id/owner_uid/creator_uid` 是 int（Dart int64 消费无碍；违反全局「TSID 出站转 string」约定，JS 消费方有精度风险）。与管理端 `tsid_keys_to_bin` 形成两套口径。
3. **敏感字段 `chat_aes_key` 随 `SELECT *` 下发**：任何携带 gid 的登录用户均可读取（`group_handler.erl:81`）。固化为契约时登记该事实；若 E2EE 已迁移 PFv3/Megolm，此列属历史遗留，收敛需另立变更单。
4. **C 端 detail 无权限校验**：handler 只校验 gid 合法性，不校验请求者是否群成员/群是否存在可见性（群不存在时返回错误“群组不存在”，`:82-83`）。
5. **status 枚举**：-1 删除（管理端 dissolve 即置 -1，`adm_group_handler.erl:160`）、0 禁用、1 启用；与好友列表的 `status`（online/offline 字符串）完全不同名不同义，勿混用。

## 企业群阅读游标（2026-10-02）

`POST /api/v1/groups/:group_id/read` 使用用户登录态，请求体为 `{"msg_ids":["实际已显示的消息ID"]}`，每批 1–200 个唯一消息 ID。群 ID 为正整数；服务端解析群的实际工作区，不接受客户端指定归属。

仅当前有效用户、企业成员、工作区成员、群成员及开放的成员 generation 可以推进游标。消息必须实际投递给当前用户，属于目标群及当前 generation，且未过期、未撤回；ACK 不代表阅读。重复阅读保持单调，退出再加入不继承旧 generation 的游标。

成功为 HTTP 200 / envelope code 0，数据含 `read_seq`。输入错误为 envelope 400，目标或消息不在授权范围为 envelope 404，服务错误为 envelope 500；这些沿用现有人类 API 的 HTTP 200 信封。非 POST 为 HTTP 405。

工作区群列表 `member_only=1&preview=1` 返回 `unread_count`，按当前成员 generation 的有效消息计算，排除本人发送、已过期及已撤回消息。客户端不拼接个人空间的本地计数。

## 人类企业文件审计（2026-10-02）

企业群资料上传、删除分别追加 `file.uploaded` / `file.deleted`；资源类型为 `group_file`，ID 为真实文件主键。普通群/频道附件首次确认追加 `file.confirmed`，群消息首次绑定附件追加 `file.message_bound`；资源类型为 `attachment`，ID 为真实附件主键。

审计归属由服务器群/频道→工作区→企业关系确定；人类事件包含实际用户 ID、`actor_role=human`，`origin_application_id=null`。详情仅含工作区、scope、消息及序号引用，不含文件名、URL、正文或凭据。个人资源及无企业工作区不记企业审计。

审计与业务写入同事务，故障全部回滚。已确认路径的上传者、scope、scope_ref 不可更换；确认重放、重复删除及重复绑定不产生新的审计事实。Internal 调用沿用原有应用来源审计。
