## 结论

**IMBoy 三端的时间处理基础方向合理，但当前代码还不能保证“多时区用户同时使用，所有页面与业务时间都一致”。**

具体判断：

1. **存储基础可以保留。** 后端已有 Unix 时间戳、PostgreSQL `timestamptz`，客户端也有正确的本地时间显示路径。多数消息不会仅仅因为接收者身处另一个时区就存错时间。
2. **问题主要出在跨层合同和业务边界。** 已确认存在旧消息显示 UTC、日期筛选混用时区、免打扰按服务器时间判断、HTTP 校时没有接通，以及秒/毫秒/字符串不一致等问题。其中两处已经影响频道分页和客服历史消息显示。
3. **更简洁的方案是收敛现有实现。** 继续使用现有数据库与时间戳基础，明确区分“发生时刻、纯日期、本地时间规则、持续时长”，统一三端的解析、显示和日期边界入口，再补齐必要的跨时区测试。

### 本次审查基线

我检查的是 **2026 年 10 月 23 日可取得的三端 GitHub `main` 快照**：

| 项目 | 审查提交 |
|---|---|
| Erlang 后端 `imboy` | [1834a454](https://github.com/imboy-pub/imboy/commit/1834a45487a966f6e86d5099d18556262a6da102) |
| Flutter 客户端 `imboyapp` | [f7a8fdc2](https://github.com/imboy-pub/imboy-flutter/commit/f7a8fdc24ce7e6c52827f427921c9e064bfd7046) |
| React 管理端 `imboyadmin` | [2b1a7c92](https://github.com/imboy-pub/imboy-admin-frontend/commit/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44) |

本次完成了源码调用链核对、管理端实际 TypeScript 函数的小范围复现，以及日期、夏令时边界的独立计算。**没有修改代码，没有运行 Flutter/Erlang 全量测试，也没有连接生产数据库。** 因此，下文是代码审查结论，不能替代生产环境验收；你电脑里尚未推送的修改也不在此基线内。

---

## 一、现有设计哪些是合理的？

先明确一个判断标准：

> 多时区正确，不是让所有用户看到相同的钟面时间，而是让所有用户看到同一个事件在各自时区的正确表示。

例如，同一条消息发生于 `2026-10-01T00:30:00Z`：

| 阅读者时区 | 正确显示 |
|---|---|
| 上海 | 10 月 1 日 08:30 |
| 纽约 | 9 月 30 日 20:30 |
| 加德满都 | 10 月 1 日 06:15 |

这些显示不同是正常的。它们对应的时间戳、消息身份、消息顺序应保持一致。

### 1. 后端获取当前时间的基础做法正确

`elib_dt:second/0`、`millisecond/0`、`utc/1` 最终调用的是：

```erlang
erlang:system_time(second)
erlang:system_time(millisecond)
```

没有额外加上八小时。这些 Unix 时间戳不会因为服务器从上海时区切到纽约时区就整体变化。

证据：[elib_dt.erl](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/lib/elib_dt.erl#L88-L117)。

### 2. 主要事件时间使用 `timestamptz`，适合多时区

例如消息表的 `created_at`、`server_ts` 已使用 `timestamp with time zone`。PostgreSQL 会把这种值按同一绝对时刻保存；输出时可以按会话时区表示，但不会保留输入时的原始地区时区名称。**这个存储类型应继续保留。** [postgresql.org](https://www.postgresql.org/docs/current/datatype-datetime.html?utm_source=chatgpt.com)

代码证据：[C2C 消息表](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/priv/migrations/00000005_msg_c2c.up.sql#L10-L19)。

需要区分：

- `2026-10-01T08:30:00+08:00`
- `2026-10-01T00:30:00Z`

两者可以是同一时刻。现有代码有些路径输出服务器本地 offset，**这属于表示不统一，不能直接认定成“存错八小时”**。

### 3. 部分客户端路径已经正确，而且值得复用

| 已有实现 | 判断 |
|---|---|
| Flutter `lastTimeFmt` 构造本地 `DateTime` | 正确，已修过同类时区显示问题 |
| 企业附件日期筛选使用结束日期“下一天零点”作为上界 | 正确，适合半开区间 |
| 里程碑 `due_date` 使用数据库 `date`、接口 `YYYY-MM-DD` | 正确，纯日期不应跟随时区平移 |
| 管理端客服 `messageTime` 使用浏览器本地 `Intl.DateTimeFormat` | 对合法带时区输入的转换方向正确 |
| 后端历史消息使用 `conv_seq` 游标 | 正确，消息顺序不应依赖用户时区 |

证据：[Flutter 时间显示](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/component/helper/datetime.dart#L54-L65)、[附件日期筛选](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/modules/attachments/workspace_attachment_filters.dart#L126-L136)、[里程碑日期](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/repo/project_milestone_repo.erl#L6-L19)、[管理端消息时间](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/modules/customer_service/seat/workbench/messageTime.ts#L1-L9)。

**因此，这轮优化应该围绕现有正确做法统一入口，避免继续在各页面分别修补。**

---

## 二、已经确认的具体问题

### F1：Flutter 部分旧消息、群公告、朋友圈仍按 UTC 直接显示

这是当前最直接的跨时区显示缺陷。

`DateTimeHelper.dateTimeFmt` 的行为是：

- 最近两天显示“几分钟前、几小时前”，使用时间戳差值；
- 超过两天后进入绝对日期时间显示，直接格式化传入的 `DateTime`；
- 函数内部没有保证转成本地时间。

但是 `RelativeDateFormat.format` 又主动把时间转成 UTC：

```dart
return DateTimeHelper.dateTimeFmt(date.toUtc());
```

聊天页面确实使用了这个 formatter。

所以，**新消息显示“几分钟前”时看起来正常；消息变旧之后，可能突然显示成 UTC 钟面时间。**

同类生产路径还包括：

- 群公告 `formatTime`：显式构造 `isUtc: true` 后直接显示；
- 朋友圈 `momentRelativeTime`：部分输入解析成 UTC 后直接显示。

代码证据：[时间工具](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/component/helper/datetime.dart#L5-L29)、[RelativeDateFormat](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/component/helper/datetime.dart#L177-L185)、[聊天页面实际调用](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/page/chat/chat/chat_page.dart#L1963-L1964)、[群公告调用](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/page/group/announcement/group_announcement_provider.dart#L246-L252)。

**建议：把“面向用户的绝对时间必须先转展示时区”放进统一 formatter。** 调用方只提供事件时间，不再各自决定是否 `.toLocal()`。

管理端也有类似不一致：客服队列使用 `getUTCHours()` 显示 `HH:mm`，同一会话在详情里却使用本地时间，队列还没有标注 UTC。上海用户可能在左侧看到 `00:30`，右侧看到 `08:30`。

证据：[客服队列](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/modules/customer_service/seat/workbench/queuePanel.tsx#L48-L53)、[详情显示](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/modules/customer_service/seat/workbench/customerContextPanel.tsx#L31-L34)。

---

### F2：离线免打扰按服务器本地时间判断，不能满足异地用户

后端代码明确写着：

- App 侧按用户设备本地时间判断；
- 服务端按 `calendar:local_time()` 判断。

实际调用相当于：

```erlang
user_dnd_rule_ds:is_dnd_at(Uid, H * 60 + M)
```

这里的 `H`、`M` 来自服务器，而规则没有保存用户的时区。

**服务端 DND 规则启用后，如果用户时区与服务器不同，静默推送时段就会错位。**

例如：

- 用户在纽约，希望每天 `23:00–08:00` 静默；
- 假设服务器使用上海时区；
- 纽约深夜 23:30 时，上海已是次日白天，后端可能仍允许推送；
- 反过来，纽约白天可能被错误静默。

这条判断已经接入 C2C、C2G 和复用这些入口的企业消息离线推送。

证据：[离线推送与 DND 实现](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/logic/push_notification_logic.erl#L122-L178)、[规则字段](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/repo/user_dnd_rule_repo.erl#L29-L61)。

**建议：给这类规则增加明确的时区语义。**

需要先定义产品行为是：

- 固定在用户选择的时区；
- 跟随某个设备；
- 或账号所有设备共用同一套时区规则。

多设备场景尤其不能简单地让最后一次请求覆盖账号时区，否则两个处于不同时区的设备可能反复改变规则。

---

### F3：“今天、昨天、明天”有些地方被当成固定 24 小时

这是另一类问题：**经过了多久，与属于哪一个日历日，是两种判断。**

#### 频道日期标题

频道当前使用：

```dart
now.difference(message.createdAt).inDays
```

来判断“今天”“昨天”。

如果本地现在是：

- 10 月 23 日 00:10；
- 消息发于 10 月 22 日 23:50；

只过去 20 分钟，代码会显示“今天”，正确标签应为“昨天”。

证据：[频道日期标题](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/page/channel/widgets/channel_message_feed.dart#L627-L643)。

#### 群日程与夏令时

群日程还有两种写法：

- 两个本地午夜相减，取 `.inDays`；
- 本地今天零点加 `Duration(days: 1)`，作为明天零点。

纽约 `2026-03-08` 切换夏令时，当地当天零点到次日零点只有 **23 小时**。于是：

- 两个午夜相减取整得到 `0` 天；
- 固定加 24 小时得到次日 `01:00`；
- 次日 `00:30` 的日程可能被归入“今天”。

这与 Dart 官方说明一致：本地午夜之间的间隔可能因夏令时而不是 24 小时的整数倍。[Dart API](https://api.dart.dev/dart-core/DateTime/difference.html?utm_source=chatgpt.com)

代码证据：[日程提示](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/page/group/schedule/group_schedule_ui.dart#L171-L185)、[日程分组](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/page/group/schedule/group_schedule_page.dart#L142-L162)。

**建议：**

- “几小时前”使用持续时长；
- “今天、昨天、明天”比较目标时区下的年月日；
- 下一天边界按下一天的日历日期构造，复用附件筛选已经采用的做法。

---

### F4：管理端审计日期筛选，起点按 UTC、终点按本地时间

现有代码分别使用：

```ts
new Date(params.dateStart)
new Date(params.dateEnd + 'T23:59:59')
```

日期输入是 `YYYY-MM-DD`。

JavaScript 对这两种字符串的解释不同：不带 offset 的纯日期按 UTC 解释，不带 offset 的日期时间按本地时间解释。[tc39.es](https://tc39.es/ecma262/multipage/numbers-and-dates.html?utm_source=chatgpt.com)

我按现有表达式运行了复现。选择同一天 `2026-10-22`：

| 浏览器时区 | 实际起点，按当地时间看 | 实际终点 | 实际覆盖 |
|---|---|---|---|
| 上海 | 10 月 22 日 **08:00** | 当日 23:59:59 | 约 16 小时 |
| 洛杉矶 | **10 月 21 日 17:00** | 10 月 22 日 23:59:59 | 约 31 小时 |
| UTC | 当日 00:00 | 当日 23:59:59 | 约 24 小时 |

此外，终点停在 `23:59:59.000`，还会漏掉最后一秒内的小数部分。

代码证据：[AuditLogPage.tsx](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/pages/logs/AuditLogPage.tsx#L285-L297)。

**应统一为同一时区下的半开区间：**

```text
[起始日期零点，结束日期的下一天零点)
```

也就是 `>= start`、`< endExclusive`。

---

### F5：频道分页的毫秒游标被截断，再误解释成 2038 年

**这是本次发现中应优先修复的一项，当前年份就会触发。**

完整链路是：

1. Flutter 用最后一条消息的 `millisecondsSinceEpoch` 作为 cursor。
2. 后端 handler 把 cursor 的最大值设为 `2147483647`，即有符号 32 位上限。
3. 当前毫秒时间戳约为十三位，必然超过上限，被钳成 `2147483647`。
4. Repo 调用 `elib_dt:to_rfc3339/1`，该函数按数字位数猜单位。
5. 十位数被当成“秒”，最终变成 **2038-01-19T03:14:07Z**。

例如：

```text
原始 cursor：1792627200000
钳制之后：   2147483647
解释结果：   2038-01-19T03:14:07Z
```

后续查询变成“取 2038 年以前的最新消息”，正常 2026 年数据因此可能再次返回第一页。Flutter 加载更多时又直接追加列表，会出现重复消息。

证据：[Flutter cursor 生成](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/page/channel/channel_provider.dart#L443-L466)、[后端 cursor 上限](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/api/channel_handler.erl#L477-L485)、[Repo 转换与查询](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/repo/channel_message_repo.erl#L175-L195)、[按位数猜单位](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/lib/elib_dt.erl#L133-L151)。

最小修复是明确 cursor 为毫秒、使用足够的范围，并显式按毫秒转换。

进一步要保证分页完整，建议使用稳定的组合游标，例如 `(created_at, id)`。仅用时间戳和严格 `< cursor`，还需要处理同一时间戳跨页、微秒转毫秒丢精度等情况。

---

### F6：客服历史消息返回秒整数，管理端却只接受字符串，时间会消失

这项已经通过**实际前端生产函数**复现。

真实 PostgreSQL 读链将客服消息 `created_at` 转为 Unix **秒整数**；后续解密、facade、HTTP 输出都保留这个整数。

但管理端坐席和访客 Widget 使用：

```ts
createdAt: optStr(raw.created_at)
```

`optStr` 只接受字符串，因此数字时间变成 `null`。

实际函数复现结果：

| 后端输入 | 前端结果 | 界面后果 |
|---|---|---|
| `1789600000` | `createdAt = null` | 时间不显示，日期分隔也可能缺失 |
| `"1789600000"` | 保留字符串，但 `new Date(...)` 无效 | 仍无法正常显示 |
| 合法 RFC3339 字符串 | 正常解析 | 正常显示 |

注意，后端函数叫 `success_rfc3339`，但它的实际职责是**跳过普通时间转换**，不会自动把整数变成 RFC3339。

证据：[后端时间转秒](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl#L445-L470)、[HTTP 原样出口](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/features/customer_service/interfaces/cs_http.erl#L626-L632)、[坐席消息适配](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/modules/customer_service/seat/workbench/contract.ts#L302-L317)、[Widget 消息适配](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/widget/customer_service/app/contract.ts#L191-L207)。

**建议在 API 适配层按明确合同完成一次转换。** 这个接口既然返回秒，就明确乘 1000 转内部毫秒，或者统一转成规范 ISO；不要让所有显示组件各自猜测。

---

### F7：Flutter 的 HTTP 服务器校时没有接通

后端标准响应的形状是：

```text
code
msg
sv_ts
payload
```

`sv_ts` 在最外层。

Flutter 的处理却是：

1. Transformer 从响应中取出 `payload`；
2. 合并额外字段时明确排除 `sv_ts`；
3. `IMBoyHttpResponse.success(payload)` 再去找 `payload['sv_ts']`。

所以，**正常响应的服务器时间通常不会到达校时入口。**

而 Web 平台会跳过 UDP NTP，依赖服务器时间同步，这条断链对 Web 更明显。

证据：[后端信封](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/lib/elib_response.erl#L216-L227)、[Flutter Transformer](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/component/http/http_transformer.dart#L19-L67)、[校时读取位置](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/component/http/http_response.dart#L16-L30)。

这与时区转换要分开看：

- 时区决定同一个瞬间显示成几点；
- 校时解决设备对“现在”的判断是否准确。

**建议在解析 HTTP 信封时直接读取顶层 `sv_ts`，更新统一时钟入口。** 业务 `payload` 不再负责协议元数据的校时工作。

---

### F8：消息的“客户端创建时间、服务端接收时间、排序依据”还没有完全分清

#### 群消息实时与落库的 `server_ts` 来源不一致

实时发送时：

- `created_at` 来自客户端；
- `server_ts` 来自当前服务端时间。

但进入 staging 时，代码弃用了 `NowTs`，把 `CreatedAtRfc` 同时传给创建时间和服务端时间。正式群消息表的写入路径也有 `server_ts = CreatedAt` 的行为。

如果发送设备快十分钟：

- 实时信封的服务端时间是正确的；
- 离线重拉、归档和部分会话列表却可能拿到快十分钟的“服务端时间”。

后端会话列表又使用 `server_ts` 选择最新项和排序，因此它不仅是字段名称不准确。

证据：[实时信封](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/logic/msg_c2g_logic.erl#L156-L182)、[staging 写入](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/logic/msg_c2g_logic.erl#L287-L325)、[会话排序](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/logic/conversation_logic.erl#L346-L364)。

#### 撤回、编辑等窗口也会受客户端错钟影响

C2C 撤回和编辑窗口按 `NowMs - CreatedAtMs` 判断，而 `created_at` 来自客户端。设备时间快慢会改变实际可操作窗口。

修复时也不能直接把所有引用替换为现有 `server_ts`，因为部分编辑路径会把 `server_ts` 更新为 `CURRENT_TIMESTAMP`，它目前不是稳定的“首次接收时间”。

证据：[撤回窗口](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/logic/msg_c2c_logic.erl#L495-L507)、[编辑时更新 server_ts](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/repo/msg_c2c_repo.erl#L433-L443)。

#### `conv_seq` 已有基础，但还没有贯穿 Flutter 展示顺序

后端历史同步已有 `conv_seq`，Flutter 也用 `after_seq/next_seq` 推进同步。

不过，当前 Flutter 本地会话分页按 `auto_id` 读取，主要代表本地插入顺序。不能因为后端有 `conv_seq`，就认定多个设备在乱序投递、补历史后已经保证相同展示顺序。

证据：[后端事务内序号](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/repo/personal_message_order_repo.erl#L31-L43)、[Flutter 本地分页排序](https://github.com/imboy-pub/imboy-flutter/blob/f7a8fdc24ce7e6c52827f427921c9e064bfd7046/lib/store/repository/message_query_reader.dart#L25-L71)。

**推荐明确三项职责：**

| 信息 | 职责 |
|---|---|
| 客户端创建时间 | 记录用户设备声称的创建时刻 |
| 不可变的服务端接受时间 | 在线、离线、归档保持一致；作为相应业务窗口的权威基准 |
| 会话序号 | 确定会话内顺序、同步和分页 |

排序贯通属于多设备一致性工作，应该与纯显示修复分开推进。

---

## 三、统计、基础库和文档还存在什么设计问题？

### 1. 报表的“哪一天”没有统一业务口径

| 路径 | 当前实现 | 判断 |
|---|---|---|
| 管理端总体统计 | `CURRENT_DATE`、`DATE(created_at)` | 依赖数据库会话时区；不一定等于管理员本地日 |
| 客服统计 | 明确传日期与 offset，返回 `[start,end)` | 固定偏移合同自洽，值得保留 |
| 客服自然日长度 | `end = start + 86400` | 无法表达夏令时地区的 23/25 小时自然日 |
| 运营周报 | 从当前时间减整天，没有对齐零点 | 存在明确的周边界错误 |

客服统计这里要准确评价：**它目前实现的是“固定 UTC 偏移的一天”，没有违背自己的合同；但这还不等于完整支持某个地区的自然日。**

纽约的两个典型日期：

| 日期 | 正确本地自然日长度 |
|---|---:|
| 2026-03-08 | 23 小时 |
| 2026-11-01 | 25 小时 |

此外，前端初始化使用的是“当前 offset”；夏季打开页面查询冬季历史，也可能沿用错误的一小时偏移。

周报则是另一个确定问题：配置示例每周一 09:00 运行，代码实际推导出“上周一 09:00 至本周一 09:00”，没有兑现注释里的两个周一零点。

证据：[总体统计](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/adm/adm_stats_handler.erl#L346-L376)、[客服窗口](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/features/customer_service/application/session/cs_session_app.erl#L1215-L1227)、[前端 offset](https://github.com/imboy-pub/imboy-admin-frontend/blob/2b1a7c926eb2f16c6ffbc44f0af5bcb4bfe98a44/src/modules/customer_service/pages/CsStatsPanel.tsx#L50-L56)、[周报范围](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/logic/ops_report_logic.erl#L142-L154)。

### 2. 基础库的“自动猜测和静默兜底”增加了复杂度

主要有三类：

- `to_rfc3339/1` 按整数位数猜秒、毫秒、微秒、纳秒；
- 普通 HTTP 响应递归按 `_at/_ts` 后缀猜时间字段；
- 部分非法时间被替换为 1970 年、2000 年或“当前时间”。

自定义 PostgreSQL codec 还存在编码使用 signed、解码使用 unsigned 的不对称。对于 2000 年以前的负内部时间值，会出现往返错误；非法字符串编码则返回内部零值，可能成为一个看似正常的 2000 年日期。

这些属于已确认的代码行为和基础库风险，**本次没有证据证明生产库已经因此出现了错误数据**。

证据：[自动转换](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/lib/elib_cnv.erl#L105-L145)、[PostgreSQL codec](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/src/lib/epgsql_codec_rfc3339_bin.erl#L20-L67)。

更合适的规则是：**无效时间在边界明确报错或标为未知；持久化层不编造一个日期作为成功结果。**

### 3. 时间规范与实际实现相互矛盾

`docs/CONVENTIONS.md` 规定所有时间统一为 UTC、毫秒精度、ISO 字符串，并禁止普通 epoch 整数传输。

实际代码却同时存在：

- 普通 REST 的毫秒整数；
- 部分 WebSocket payload 的 RFC3339；
- 客服域的秒整数；
- 专用 `success_rfc3339` 原样出口。

`docs/reference/api-format.md` 又描述了普通 REST 毫秒整数的事实。

证据：[工程时间规范](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/docs/CONVENTIONS.md#L33-L58)、[实际 API 时间格式说明](https://github.com/imboy-pub/imboy/blob/1834a45487a966f6e86d5099d18556262a6da102/docs/reference/api-format.md#L161-L179)。

这会让后续开发者或 Codex 按不同文档写出彼此不兼容的实现。**应先依据实际端点冻结时间合同，再统一规范。**

---

## 四、我建议采用的更简洁方案

### 1. 只保留四种明确的时间语义

| 语义 | IMBoy 中的例子 | 建议表示 | 核心规则 |
|---|---|---|---|
| **发生时刻** | 消息接收、登录、附件上传、单次会议开始 | DB 保留 `timestamptz`；应用内部统一毫秒 instant | 不因用户时区改变数值，显示时转换 |
| **纯日期** | 生日、里程碑到期日、筛选日期 | `YYYY-MM-DD` / PostgreSQL `date` | 不转换成某时区零点再传来传去 |
| **本地时间规则** | 每晚免打扰、每周一 09:00、业务日报 | 时分/日期规则 + IANA 时区 | 根据该地区在目标日期的规则计算 |
| **持续时长** | 重试间隔、超时、缓存 TTL、等待时长 | 显式秒或毫秒、`Duration` | 与事件日期分开；本机耗时使用单调时钟 |

IANA 时区如 `America/New_York` 包含历史和夏令时规则；一个 `-04:00` offset 只描述某次偏移，无法代替完整地区规则。[iana.org](https://www.iana.org/time-zones?utm_source=chatgpt.com)

**普通聊天显示不需要给每条消息保存用户时区。** 只有“每晚几点”“某地区哪一天”这类规则才需要明确时区身份。

### 2. 应用事件优先沿用毫秒，旧合同在接口边界适配

结合 IMBoy 现状，我更推荐：

- 主事件模型采用 epoch 毫秒；
- 现有明确规定 RFC3339 的接口继续遵循原合同；
- 客服秒整数在对应 API 适配器转换；
- 新代码使用显式单位函数，逐步退出按位数猜测；
- JWT 的 `exp`、持续时长配置等继续使用各自明确的单位。

**统一的是内部语义和边界合同，不必一次改掉所有已有线格式。**

也没有必要为了此次修复，把现有 `timestamptz` 全部迁移成 BIGINT。

### 3. Flutter 和管理端各收敛几个小入口即可

建议复用现有工具，统一以下职责：

| 入口职责 | 负责什么 |
|---|---|
| 获取当前时刻 | 返回统一的 `nowMs`，支持测试注入 |
| 解析 API 时间 | 根据端点合同解析秒、毫秒或带 offset 的字符串 |
| 显示事件时间 | 在最后一步转为阅读者时区 |
| 计算日历标签与范围 | 今天、昨天、明天、日期筛选的 `[start,end)` |

这样，页面只提供时间值和显示样式，减少各处散落的 `*1000`、`~/1000`、`.toUtc()`、`.toLocal()`。

对于设备运行期间切换时区，建议在 App 恢复前台、网页重新获得焦点时检测时区变化并刷新显示。管理端已有缓存的 `Intl.DateTimeFormat` 也应在时区变化时重建；这项需要补实际设备验收。

### 4. 把业务日边界集中计算

报表应该明确一个 `business_timezone`，可以是部署实例、业务对象或报表本身的属性，具体归属按产品需要确定。

数据库已有能力计算指定地区的一天：

```sql
SELECT
  $1::date::timestamp AT TIME ZONE $2 AS starts_at,
  ($1::date + 1)::timestamp AT TIME ZONE $2 AS ends_at;
```

其中：

- `$1` 是 `2026-03-08`；
- `$2` 是 `America/New_York`。

再用：

```text
created_at >= starts_at AND created_at < ends_at
```

查询。

这种做法分别计算两个日历零点，能够表达 23、24、25 小时的一天；前端只需展示服务端回显的窗口。PostgreSQL 对日期、时间戳和显式时区转换已有对应支持。[postgresql.org](https://www.postgresql.org/docs/current/functions-datetime.html?utm_source=chatgpt.com)

**不要按每个用户请求去改变连接池连接的全局时区。** 把时区作为明确输入，或直接传已经确定的起止时刻，会更容易保持并发请求之间的独立性。

### 5. 校时与服务端权威时间分别收口

Flutter 可以继续沿用现有校时组件，但建议：

- 在 HTTP 信封解析层读取 `sv_ts`；
- 业务服务器时间作为主要校时来源，NTP 按实际需要保留；
- 使用网络往返时间筛选样本，避免迟到响应直接覆盖偏移；
- 统一业务判断使用的“现在”入口；
- 服务端业务窗口仍以服务端事实为准。

本机超时和耗时测量继续使用单调时钟，避免墙上时钟被调整后影响持续时长。现有 `login_attempt_clock` 已经采用这种设计，可以复用其思路。[OTP 29.1.2 (erts 17.1.1)](https://www.erlang.org/doc/apps/erts/time_correction.html?utm_source=chatgpt.com)

### 6. 性能优化应集中在减少重复工作

当前部分路径是：

```text
数据库二进制时间 → RFC3339 字符串 → 响应递归扫描 → 毫秒整数
```

有机会减少重复字符串转换和递归扫描，但应先固定 DTO/端点合同，再按模块改成显式映射。全局更换 codec 会影响很多现有 Repo/Logic，改动范围反而扩大。

这次更确定的收益是：

- 解析与格式化逻辑减少；
- 时间单位不再各处猜测；
- 日期范围查询更明确；
- 跨端测试可以共用同一组输入和预期。

**我没有做性能基准，不能给出吞吐量提升百分比。** 此轮主要收益应是正确性、可测试性和维护成本下降。

---

## 五、建议的落地顺序

| 批次 | 内容 | 验收重点 |
|---|---|---|
| **第一批：修直接功能错误** | 频道 cursor、客服消息时间适配、顶层 `sv_ts` 校时 | 分页不重复；真实客服秒整数能显示；标准响应确实触发校时 |
| **第二批：统一界面与日期边界** | Flutter 旧消息/公告/朋友圈；客服队列；审计筛选；今天/昨天/日程分组 | 同一设备各页面一致；跨午夜与 DST 正确 |
| **第三批：明确业务时间权威** | C2G 接收时间贯通；撤回/编辑等窗口基准；DND 时区 | 在线、离线、归档事实一致；用户规则按明确时区生效 |
| **第四批：收敛统计与基础库** | 业务日/周边界、codec、单位猜测、文档合同 | 错误时间不伪装成功；统计窗口明确；规范与实现一致 |
| **独立一致性工作项** | `conv_seq` 贯通本地存储与 UI 顺序 | 乱序到达、补历史、多设备最终顺序一致 |

前两批三端可以分工并行；第三批涉及消息字段语义，需要先冻结合同。历史数据缺失的“首次服务端接受时间”不能凭空恢复，也不能简单整体平移八小时。

### 最小验收矩阵

至少覆盖以下情形，才能支持“多时区使用正确”的结论：

| 场景 | 必须验证的结果 |
|---|---|
| UTC、上海、纽约、加德满都读取同一事件 | instant 相同，本地显示正确 |
| 23:50 的消息在次日 00:10 查看 | 标签为昨天 |
| 纽约夏令时开始/结束日 | 正确处理 23/25 小时自然日 |
| 当日 23:59:59.999 | 包含在当天筛选内；次日零点排除 |
| 相同 instant 的 `Z`、`+08:00` 表示 | 显示与比较结果一致 |
| 设备时间快/慢十分钟 | 服务端业务窗口不随设备错钟改变 |
| 同一消息实时接收、离线重拉、归档读取 | 权威接收时刻一致 |
| 频道至少 25 条消息，包含相同时间戳 | 分页不重不漏 |
| 客服真实 PG 返回秒整数 | 坐席与 Widget 均有正确时间和日期分组 |
| 标准 HTTP 顶层 `sv_ts` | 确实更新统一时钟 |
| 设备运行期间切换时区 | 恢复前台后显示刷新，消息事实和顺序不变 |
| 生日、里程碑日期 | 切换时区仍保持原日期 |
| codec 的 1999/2000 年边界与非法输入 | 正常值往返一致，非法值明确失败 |

现有测试已经覆盖了部分正确路径，但仍有明显空缺：例如 Flutter 的 `lastTimeFmt` 时区反例在 UTC 环境直接返回；频道 handler 测试用小 cursor，Repo 测试又绕过 handler；客服前端 fixture 使用 ISO 字符串而真实读链返回秒。这些都说明，**各层单独通过测试，还不足以证明跨端时间合同正确。**

我建议下一轮先完成**第一批和第二批**。它们既能解决已经确认的用户可见问题，也能建立后续 DND、报表和消息顺序改造所需的统一时间入口。