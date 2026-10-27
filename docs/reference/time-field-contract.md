# 时间字段合同（Time Field Contract）

> **状态**：长期协议契约文档 · **版本**：v1.0 · **最后更新**：2026-10-26（IMBOY-TIMEZONE-REPAIR-V1 / R22-B 登记）
>
> **定位**：本文件是三端时间字段的**权威登记表**。它登记的是**已实现事实**（每条声明均附代码位置，见 §7 抽查记录），不发明新合同。与 [api-format.md](./api-format.md)（响应格式）、[message_api_contract.md](../api-contracts/message_api_contract.md)（消息 API 字段级合同）、[CONVENTIONS.md](../CONVENTIONS.md) §2 互为引用；冲突时以本文件与代码为准并回改文档。
>
> **背景**：IMBOY-TIMEZONE-REPAIR-V1 勘察确认存在两类时间污染：客户端直传时间进入服务端字段（`msg_c2g.server_ts` 被 staging 的客户端 `created_at` 污染，`msg_c2g_repo.erl:218-236`；`msg_c2c.created_at` 为客户端直传，`msg_c2c_logic.erl:192-193`）与文档声明（"全部 ISO-8601 字符串"）与生产 wire（毫秒 int 信封）不符。修复决策 D04（不可变服务端首次接受时间 `accepted_at`）与 D05（安全推进边界 `safe_head`）已冻结并实现；本文件把单位合同与恢复/回滚语义登记为长期文档。

---

## 1. 单位总合同：四轨并存（事实登记）

系统当前存在**四条时间表示轨道**，按"字段名 + 出口通道"判定，不按"所有时间一律 X"假设：

| 轨道 | 适用范围 | wire 表示 | 出口实现 |
|---|---|---|---|
| **A. 通用 REST 信封** | 经 `elib_response:success*` 返回的 payload 中一切 `_at` / `_ts` 后缀键 | **epoch 毫秒 int**（JSON number）；SQL NULL → JSON null | `elib_response.erl:32/42/54` → `elib_cnv:convert_at_timestamps/1`（`elib_cnv.erl:107-127`）→ `elib_dt:rfc3339_to/1`（`elib_dt.erl:265-295`：整数直通；PG tuple → `datetime_to(_, millisecond)`；数字字符串 → int；RFC3339 → 毫秒；空/解析失败 → null） |
| **B. EB 企业业务域** | `eb_pg_store_sql` 全部 `ts` kind 列（consent_at / retain_until / created_at / acked_at / assigned_at / ended_at / released_at 等） | **Unix 秒 int** | `src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl:447-479`（`convert(ts,..)` → `to_unix/1` → `elib_dt:rfc3339_to(Value, second)`；段头注释"R08-BE 时间单位合同"明文：EB 不得改毫秒） |
| **C. 服务端内部管线** | 消息写入路径生成的 `created_at`（落库值、兼容路径回投帧） | **RFC3339 微秒字符串**（恒 `Z`） | `elib_dt:now/0` = `now(microsecond)` → `to_rfc3339`（`elib_dt.erl:118-128`）；调用点 `msg_c2c_logic.erl:127` |
| **D. 非瞬间量** | DateOnly 纯日期、`tz_offset` 偏移分钟、`conv_seq` 序号、cursor token | 各自独立合同（见 §2 对应行） | 见 §2 |

**消费方铁律**：

- 不得按"字段名猜单位"、不得按"位数猜单位"（秒 10 位 / 毫秒 13 位判别被 R08-FL 显式废弃——判别依据是**字符串形态**（RFC3339 vs 十进制整数），不是位数，FL_W3 报告 §字段类型决策）。
- 解析必须走各端统一入口：AD `src/lib/apiInstant.ts`（`parseApiInstantMs(raw, unit)`，unit 由调用点显式声明）；FL `lib/util/api_time.dart`（`parseEpochSeconds` / `parseEpochMillis` / `parseIsoZoned`）；BE 无需解析（生成即正确）。
- 缺失/非法 → **null 或 0（=未知）**，**禁止回填 now()**（R08-5；FL/AD 三处实现均遵守）。

---

## 2. 权威字段表

逐字段登记：语义、wire 类型/单位、可空/错误行为、权威来源、时区语义、旧格式兼容、涉及端点。"可空"指 wire 层可能出现 `null`/缺键。

### 2.1 服务器时间戳与信封

| 字段 | 语义 | wire 类型/单位 | 可空/错误 | 权威来源 | 时区语义 | 旧格式兼容 | 涉及端点 |
|---|---|---|---|---|---|---|---|
| `sv_ts` | 管理面响应信封的"服务器当前时刻" | 毫秒 int | 不可空（生成时恒有值） | `src/lib/elib_response.erl:225`（`sv_ts => elib_dt:millisecond()`）、`adm_auth_middleware.erl:300` | UTC epoch 瞬间 | 无旧格式漂移；一直是毫秒 | 全部 `adm_*` 响应信封（`{code,msg,sv_ts,payload}`） |

### 2.2 消息核心时间四元组

| 字段 | 语义 | wire 类型/单位 | 可空/错误 | 权威来源 | 时区语义 | 旧格式兼容 | 涉及端点 |
|---|---|---|---|---|---|---|---|
| `created_at`（客户端→服务端） | **客户端创作时间**。仅展示/参考；**不是**服务端权威排序或游标（R01 修复后带 DID 的离线拉取已不再以它作服务端游标，`messaging_logic.erl` offline C2C 分支 DID 非空时 `C2cSince=undefined`） | 内部管线生成时为 RFC3339 微秒字符串（轨道 C）；**REST 出站一律毫秒 int**（轨道 A 归一） | 行级不可空（写入路径必生成）；非法客户端值按原样保留（不修正、不拒绝） | `msg_c2c_logic.erl:127`（`elib_dt:now()`）；REST 归一 `elib_response.erl:32` | 保留客户端钟面语义（污染源，勿再消费其精度） | legacy 行=客户端直传时间（D04 basis）；**新行不再由它派生任何服务端字段**（C2G 止污，BE_W1 R03） | WS 实时/离线帧、REST `action=history`/`history_backfill` 行 |
| `server_ts` | legacy 语义混杂字段：C2C=服务端落库时间；**C2G 旧行被客户端时间污染**；新 C2G 行止污后 `server_ts = accepted_at`（有 AcceptedAt 时，`msg_c2g_repo:write_msg_impl`） | 毫秒 int（REST；bigint 列直通） | 行级不可空；`{error, no_time_source}` 见 `accepted_at` 行 | 污染证据 `msg_c2g_repo.erl:218-236`、`msg_store_repo.erl:196-211`（staging server_ts=上游传参）；止污 BE_W1 R03 报告 §1.1#6 | 旧行不可信（客户端钟面）；新行=服务端时钟 | **不可事后区分**旧行污染与否（见 §5 不可恢复边界）；消费方对旧行应按低可信处理 | 全部消息行端点（history / offline / realtime `server_ts`，实时帧生成点 `message_ds.erl:216/305`） |
| `accepted_at`（首次接受时间，D04 新增） | 服务端时钟在**首次持久接受事务内**固定的瞬间。重试/重复/编辑**绝不覆盖**；排序权威仍是 `conv_seq`，本字段只用于窗口计算（撤回/编辑 R04）与展示 | DB：`timestamptz` 可空列；REST：毫秒 int（轨道 A）；FL 本地列 `accepted_at` INTEGER 毫秒 | 可空：v35/00000229 前旧行 NULL → 读侧降级链 `resolve_accepted_ms/1`：accepted_at(trusted) → server_ts(low_trust) → created_at(low_trust)，全缺/非法 → `{error, no_time_source}`（`msg_store_repo.erl:1289-1290`），撤回/编辑 fail-closed 返回 `message_revoke_error`/`message_edit_error`（码 `?ERR_MSG_TIME_SOURCE_MISSING`=500）；**绝不用 now() 回填** | 迁移 `priv/migrations/00000229_msg_accepted_at.up.sql`（4 表：msg_c2c/msg_c2g/msg_store/msg_store_staging，另有 expire_at 列）；写入链 BE_W1 报告 §1.1 表 1-9 | UTC epoch 瞬间 | 旧行 NULL=低可信回退（明确登记，不伪装可信）；OA 直留路径 `enterprise_message_repo:insert_direct_tx` 以 `NOW()` 同事务写入 | WS 全部消息行；REST `action=history`/`history_backfill`；撤回/编辑窗口判定 |
| `expires_at`（自毁期限，R05-A / D01 冻结双层合同） | 自毁绝对期限 = `accepted_at + expire_secs`（**服务端层唯一权威**：发送后 TTL，与是否阅读无关），随接受事务持久落库；重复请求不延长（`set_expire_at` SQL 带 `AND expire_at IS NULL`）。**D01 双层合同（2026-10-27 冻结）**：① 服务端层仅对顶层携带合法 `expire_secs` 的消息生效（FL 现不发顶层，开关关闭、合同冻结）；② 显示层保留 FL 本地语义（接收方已读起算、发送方发送起算、未读不销毁、`burn_read_at` 仅本地、永不进发送 payload、不作服务端删除依据）；③ **E2EE 边界=端上自律**：E2EE burn 元数据留在密文内，服务端不执行 E2EE 自毁（见 e2ee-protocol-specification.md §11.8）；④ 读侧过滤：history / history_backfill / 离线（C2C 三读 + C2G 时间线正文装载）不返回 `expire_at IS NOT NULL AND expire_at <= NOW()` 的正文，锚同一 PG `NOW()`，不新增物理删除（归档表 msg_store 仅读侧过滤，过期 seq 空洞合法、不进 gaps） | DB `timestamptz` 可空；REST 毫秒 int（轨道 A）；FL `retain_until`/`expire_at` 解析见 R08/R05-FL | 可空：无 expire_secs、非法 AcceptedAt（不回退 now，直接放弃期限元数据，`msg_burn_logic:calc_expire_at/2`，`msg_burn_logic.erl:62-79`）→ NULL；NULL 行不参与自毁扫描，也不进读侧过滤集（旧 NULL 不伪造，R05-7） | `msg_burn_logic.erl:7/25/62-79`；`msg_store_ds:set_expire_at`；读侧过滤 `msg_c2c_ds:read_msg_filter/3`、`msg_archive_repo:history_rows_tx/3`+`backfill_rows_tx/4`、`msg_c2g_repo:list_by_ids/2` | UTC epoch 瞬间 | 旧行（迁移前）无列 → NULL；**不可推算**（见 §5） | msg_c2c/msg_c2g 自毁扫描（`expire_at <= NOW()`，60s 物理 DELETE 仅此两表）；FL 企业消息 `retain_until`（注意该字段在 EB 域 wire=**秒**，见 2.4） |

### 2.3 会话推进与游标（D05 / R01 / R02）

| 字段 | 语义 | wire 类型/单位 | 可空/错误 | 权威来源 | 时区语义 | 旧格式兼容 | 涉及端点 |
|---|---|---|---|---|---|---|---|
| `conv_seq` | 会话内单调递增序号，**接受时分配**（staging INSERT+seq 同事务）。**仅同 conv_key 内可比较**，跨会话数值无定义；稀疏（终态失败/授权过滤/保留清理形成合法空洞）；是排序唯一权威 | int64（JSON number；Dart int64 安全，JS 需 53bit 防护同 TSID） | 行级不可空（已分配即有）；FL 本地列可空（NULL=服务端尚未给出，**绝不本地伪造**） | `personal_message_order_repo.erl:31-125`；归档 `msg_archive_repo.erl:111-114`；FL `message_order.dart:1-60` | 无时区语义（序数） | 旧客户端数值 `next_seq` 游标语义不变；FL v35 前无本地列 → 无 seq 历史组整体排前（见 §4-D） | history / history_backfill / offline（c2g marks）；FL msg_* 四表 |
| `safe_head` | D05 安全推进边界 B：`min(正式表 max(conv_seq), 非终态 staging min(seq) − 1)`；无非终态 staging 行时 = 正式表 max。客户端 `next_seq` 不得超过且只进不退 | int64 | 不可空（history 响应必有） | `msg_archive_repo.erl:490-493`（safe_head/safe_head_tx，单事务 `elib_pg:with_tx`）、`messaging_logic.erl:193/287` | 无时区语义 | 增量字段：旧客户端忽略后最坏回到旧行为（不比修复前差） | REST `action=history` 响应 |
| `blocked` | true=正式表已有高于 B 的行被非终态 staging 钳住；客户端**等待，禁止自行推断 gap 或推进** | bool | 不可空 | `messaging_logic.erl:267/288`；决策矩阵 FL `chat_archive_service.planHistorySync` | 无时区语义 | 同上（旧客户端忽略） | REST `action=history` 响应 |
| `gaps` | 合法空洞显式清单：`[{from_seq, to_seq, reason}]`，`reason ∈ terminal_failure \| authorization`；保留清理形成的 seq 不出现 | JSON array | 可为空数组（无空洞）；**不是** null 表示"未知" | `msg_archive_repo.erl:577`（terminal_gap_tx）、`messaging_logic.erl:289` | 无时区语义 | 同上 | REST `action=history` 响应 |
| `next_seq`（history/backfill 响应） | 恢复位置游标：`min(max(NextSeqRaw, AfterSeq), max(SafeHead, AfterSeq))` 钳制后返回；backfill 分批续跑时=下一批起点 | int64 | 不可空 | `messaging_logic.erl`（history）/`history_backfill/6`（`:11/303-308`） | 无时区语义 | 数值语义与旧版一致（增量兼容） | REST `action=history` / `action=history_backfill` |
| offline cursor **v1** | `{v:1, at, id}`：单游标（RFC3339 微秒字符串 `at` + msg_id `id`），同毫秒不可区分（注释明示 legacy 精度局限） | base64(JSON)，≤512B | 解码失败 → `{error, invalid_offline_cursor}`（`msg_offline_cursor.erl:36-38`）；`at` ≤64B、`id` ≤40B、格式不符即拒绝 | `msg_offline_cursor.erl:17/30-39` | UTC 瞬间（微秒字符串） | **双向兼容**：新旧代码均可解码（新代码保留 v1 分支，`:37` 注释明文"旧 token 继续可解码"） | offline 拉取（C2C/无 DID 旧路径） |
| offline cursor **v2** | `{v:2, marks:[{g,s}]}`：per-group 服务端投递进度（group_id, conv_seq），marks 数 1..50，`g>0`、`s>=0`；基于权威接受顺序而非墙钟 | base64(JSON)，≤512B | 解码失败 → `{error, invalid_offline_cursor}`；**旧代码（base 3106e6c）无 v2 分支 → v2 token 解码必失败**（`decode_data(_) → {error, invalid_offline_cursor}`，见 §6 回滚条目） | `msg_offline_cursor.erl:10-16/41-47/51-58` | 无时区语义（conv_seq 序数） | v1 token 在新代码继续有效；v2 token 仅新代码可解；客户端按账号/环境隔离保存 marks（be_fl_contract） | offline 拉取（C2G，带 DID） |

### 2.4 按域例外字段（轨道 A/B 分界实证）

| 字段 | 域/端点 | wire 单位 | 权威来源 | 备注 |
|---|---|---|---|---|
| EB 域全部 `ts` kind 列（consent_at / retain_until / created_at / acked_at / assigned_at / ended_at / released_at 等） | 企业业务（eb_pg_store_sql 出站的一切） | **秒** int | `eb_pg_store_sql.erl:447-479`（合同注释 + `rfc3339_to(Value, second)`） | 消费方：AD `toApiInstantIso(raw,'seconds')`（`pureFunctions.ts`）；FL `parseEpochSeconds`（`enterprise_models.dart:287-290`，本地统一毫秒、0=未知）。同一响应无双单位混用（W0 核对表） |
| credential 四时间字段（created_at / updated_at / last_used_at / expires_at 类） | 企业应用凭证（`/enterprise_application` credentials） | **毫秒** int | `adm_enterprise_application_handler.erl:173-174` → `elib_response:success`（轨道 A；AD_W1 报告 §R08 表格互证） | AD `toCredentialMeta` 用 `'milliseconds'` |
| moments `created_at` / `updated_at`、举报（report / moment_report / report_action 五时间字段） | 朋友圈/举报域 | **毫秒** int（SQL NULL → JSON null） | 全部走 `elib_response:success` 轨道 A；逐跳证据 AD_W3 报告 §1.1 表 | AD `normalizeMoment` / `normalizeMomentReport` / `normalizeReportAction` 归一，null 容忍（杜绝 `new Date(null)`=1970） |
| `read_at` | 已读回执（c2c/c2g `action=read`） | 毫秒 int（REST 出站轨道 A） | `msg_c2c_logic.erl:1026`、`msg_c2g_logic.erl:665`（payload 可带、缺省服务器时间 `NowMs`） | 客户端传入值不校验钟面（同 created_at 旧语义，仅展示） |
| WS 实时信封 | `server_ts`（无 `created_at` 键） | 毫秒 int | `message_ds.erl:216/305`（`elib_dt:millisecond()`） | 客户端时间基准用 `server_ts` |
| WS 离线信封 | `created_at` / `server_ts` | 毫秒 int（组帧前 `elib_cnv:convert_at_timestamps/1` 归一） | `message_ds.erl:495-497` | 内部行 RFC3339 微秒形态在此归一（轨道 C → A 的边界） |

### 2.5 非瞬间量

| 字段 | 语义 | wire 类型/单位 | 可空/错误 | 权威来源 | 时区语义 | 旧格式兼容 | 涉及端点 |
|---|---|---|---|---|---|---|---|
| DateOnly（`YYYY-MM-DD` 纯日期参数） | 审核队列等"业务日"筛选；**无时区语义的日历日**，必须配 `zone` 解释为该时区当地自然日 `[00:00, 次日00:00)` 半开区间 | query string 日期 | 非日历日 → 422；zone 缺省 `UTC`（不静默改变旧报表含义） | `src/repo/review_queue_repo.erl:31-42/99-100/155-172`（`AT TIME ZONE $zone` 构造当地日界；列不包装 `::date`，保索引）；R12-BE | **请求 zone 的当地自然日**；新参数 `start_at`/`end_at`（RFC3339 瞬间）优先于纯日期 | `start`/`end` 纯日期继续有效（同传时被 start_at/end_at 忽略，测试钉住） | `adm_moderation` 复审队列 page |
| `tz_offset` | 客服统计窗口的当地偏移 | **分钟 int**（正=UTC 以东） | 越界 → 422 `invalid_tz_offset`（`cs_http.erl:1005-1009`） | state.json R22-A units_frozen；CS-BE-07 分类 | 账号 zone 的本地墙钟分钟；旧模式=固定 24h 窗口；自然日新模式 pending **D03**（UNRESOLVED，勿提前实现） | 旧客户端固定偏移继续有效 | 客服 `cs_stats` |
| `dnd_start_min` / `dnd_end_min` | 免打扰当地墙钟分钟 | 分钟 int | 域内校验 | state.json units_frozen | 账号 zone（IANA zone 归属 pending **D02**，UNRESOLVED） | 旧值不变 | DND 设置 |
| `accepted_at`+窗口比较 | 撤回/编辑窗口：`NowMs − BaseMs > WindowMs` 严格大于才超时；`WindowMs =< 0` 不限时 | 毫秒 | `no_time_source` → fail-closed 拒绝 | `msg_c2c_logic.erl trusted_window_decision/2`、R04 报告 | 窗口为时长算术，无时区 | 旧行低可信回退（§2.2 accepted_at 行） | 撤回/编辑 |
| `user_dnd_rule.time_zone`（D02 已冻结 2026-10-27） | 账号级 IANA zone（显式设置，设备仅初值预填、不自动跟随）；判定 `user_dnd_rule_ds:is_dnd_now/1` 经 PG `NOW() AT TIME ZONE $zone` 取当地时分复用 in_window（跨午夜三分支原样）；DST 合同=instant→墙钟单向映射（NY 2026-11-01 两次 01:30 都静默、2026-03-08 缺失小时无命中）；旧行 NULL → 部署 fallback（配置 `dnd_fallback_zone`，**本部署=Asia/Shanghai，用户确认**），与修复前逐分钟等价（R15-4，部署同款 TZ 环境钉住）；多设备同账号 upsert 单行末次写入胜；**本轮 5b：仅 BE 合同+迁移（00000231），无 HTTP API，FL 未实现**；未来设置 API 非法 zone → 422 invalid_zone | DB `text` 可空（IANA 名）；未来 API wire=text | 写路径非法值拒写 `{error, invalid_zone}`（`user_dnd_rule_repo:validate_zone/1` 查 `pg_timezone_names`）；判定 DB 瞬态故障 fail-open 不静默 | 迁移 `00000231_user_dnd_rule_time_zone`（expand 阶段不回填）；判定 `user_dnd_rule_ds:is_dnd_now_at/2`（可注入瞬间）；`push_notification_logic:dnd_muted/1` 委托 | UTC instant → 规则 zone 墙钟分钟 | 旧行 NULL=按部署 zone 解释（不回填不猜测 offset→IANA） | `attachment today_uploads` / `enterprise_application_usage` 月度（R16-8 登记，D03 收尾） | 两处运营侧统计**合同登记为 UTC 运营口径**（PLAN R16 条目 5 精神：明确 UTC 月结可保留），本卡不改码。实现事实（如实登记）：today_uploads 的 `created_at >= CURRENT_DATE` 与 usage 月度 `date_trunc('month', CURRENT_TIMESTAMP)::date` 均随 **DB 会话 TimeZone** 截断（2026-10-27 实测本开发测试服务器 imboy_pg18 会话默认 = `Asia/Shanghai`，非 UTC）；日界/月界边界时刻的计数随部署会话配置漂移，属运营侧小指标可接受。若生产需严格 UTC 日界/月结：部署时固定会话 `TimeZone=UTC`，或后续小改造给两处显式 `AT TIME ZONE 'UTC'`（行为登记后另行派卡） | —（服务端运营指标，非客户端合同） | — | `attachment_repo.erl` today_uploads；`enterprise_application_usage_repo.erl` 月度计量 | 显式 UTC（随 DB 会话） | 行为不变，仅口径显式化 | 运营侧统计（AD/管理端展示） |
| 离线推送判定（4 入口，在线短路不触发） |

---

## 3. schema 演进顺序与兼容矩阵

### 3.1 五段演进顺序（expand → 兼容读写 → 能力启用 → 有界补齐 → 收紧）

| 阶段 | 动作 | 约束 | 实例 |
|---|---|---|---|
| ① expand | 加可空列/加版本化 token，**不改既有列语义、不写破坏值** | 新列无 NOT NULL/DEFAULT；旧节点对 `SELECT *` 无感知 | v35（FL msg_* 四表加 `conv_seq`/`accepted_at`）；00000229（PG 四表加 `accepted_at`，staging 另加 `expire_at`） |
| ② 兼容读写 | 新代码读旧数据走降级链；旧代码读新数据忽略增量字段 | 读侧 NULL ≠ 伪造；写侧不覆盖既有事实 | `resolve_accepted_ms/1` 三级降级；history 响应增量字段旧客户端忽略 |
| ③ 能力启用 | 新路径开始消费新事实（窗口判定、排序、安全边界） | 失败 fail-closed，不静默放行 | R04 撤回/编辑窗口用 accepted_at（`no_time_source` → 500 拒绝）；FL `ConversationOrder` 五键排序 |
| ④ 有界补齐 | 服务端提供显式补取接口；客户端幂等补写本地缺失事实 | 范围/批次/次数有界；COALESCE 仅补 NULL | `history_backfill`（range ≤1000，BE）+ `backfillServerOrderFacts`（FL，仅补 NULL） |
| ⑤ 收紧 | 事实字段覆盖率达标后才可考虑（如 accepted_at 转不可空、废弃低可信回退） | **本期未执行**；任何收紧须重新评审 §5 边界是否已被清理 | —— |

### 3.2 新旧组合兼容矩阵

| 组合 | 谁读 | 谁写 | 行为 |
|---|---|---|---|
| 旧 BE 节点 + 新 schema（00000229 已应用） | 旧节点 `SELECT *` 会带回 accepted_at 列但代码不消费 | 旧节点 INSERT 不含新列 → NULL | 新列保持 NULL，零影响（迁移头注释明文"旧行保持 NULL，旧节点/旧客户端零影响"）。**滚动部署假设：混合期旧节点只读，不得写破坏值**（D05 compat 条款） |
| 新 BE 节点 + 旧 FL 客户端 | 旧客户端读 history 响应 | 新节点写 accepted_at/新字段 | 增量字段（safe_head/blocked/gaps/conv_seq/accepted_at）被忽略，最坏回到旧行为；`next_seq` 数值语义不变 |
| 新 BE 节点 + 新 FL 客户端 | 双方读新事实 | 双方写新事实 | 全量合同生效：排序按 ConversationOrder 五键；blocked 等待；gaps → backfill |
| v1 cursor token + 新代码 | 新代码 decode v1 分支 | 旧客户端继续发 v1 | `{ok, {At, Id}}`，继续有效（`msg_offline_cursor.erl:30-39`） |
| v2 cursor token + 旧 BE 节点 | 旧节点 decode | —— | v2 必失败 → `{error, invalid_offline_cursor}`（base 无 v2 分支）。**该失败是显式错误而非静默错乱**；客户端按游标失效路径重同步（见 §6） |
| v35 FL 列 + 旧服务端行 | 新 FL 读本地 NULL 列 | 服务端 wire 携带 conv_seq/accepted_at 后由 writer 写入 | NULL=服务端尚未给出，排序回退组（无 seq 组整体排前）；经 `backfillServerOrderFacts` 幂等补齐（§4-D） |

---

## 4. 存量恢复步骤（逐类 checkpoint 与步骤）

> 总原则：**恢复靠服务端权威事实有界重放，不靠本地推断；幂等，可中断续跑，第二遍必须 no-op。**

### A. 旧游标过冲（safe_head + history_backfill 有界补扫）

- **checkpoint**：客户端持久化的 `next_seq` / v2 marks 过冲值（只进不退，不回退本地值）；服务端 `safe_head` 随 stuck seq 解决而上升。
- **步骤**：
  1. history 响应 `blocked=true` → 本轮等待，不推进、不推断 gap（`planHistorySync`）。
  2. `gaps` 非空或检测到过冲 → `action=history_backfill`（同 history 参数与授权），按 `next_seq`/`has_more` 分批续扫至 `>= safe_head`。
  3. 有界：服务端扫描跨度 ≤ `backfill_range_limit()` = 1000，超出 → 400 `backfill_range_exceeded` 强制分批（`msg_archive_repo.erl:472-473`；R02-5 实测 1001 行注入）；每批 `limit` 夹紧 1..100；FL 侧 `maxPages=40` 有界（`chat_archive_service.backfillMissing`）。
  4. 中断续跑：恢复位置=每批响应 `next_seq`，`has_more=true` 续扫；单批失败可中断、下次从 `next_seq` 继续。
  5. 去重：范围按 seq 不重叠、批间单调，仍以 **msg_id 幂等去重**（be_fl_contract：`同history参数→{messages,next_seq,has_more,conv_key,range_limit=1000}; 客户端按next_seq/has_more分批至≥safe_head, msg_id幂等去重`）。
  6. 第二遍 no-op：重复执行不再产出新行（R02-5 断言过冲恢复后集合不再变化；archive→重复 archive 行数不变）。

### B. SQLite 数字字符串时间（FL 企业消息 `enterprise_message.created_at` / `retain_until` TEXT 历史行）

- **checkpoint**：历史行 TEXT 列内容为十进制秒字符串（旧 wire 直通落库）；迁移无 schema 变更（列保持 TEXT）。
- **步骤**：
  1. 首次读出按**字符串形态**判别（RFC3339 vs 十进制整数，非位数猜测）：RFC3339 → `parseIsoZoned` 直取毫秒；十进制整数串 → `parseEpochSeconds` ×1000（旧值已知来源全部是秒，FL_W3 报告）。
  2. 归一为毫秒写回（RFC3339、毫秒精度、恒 Z；`ms<=0` 写空串）→ 被覆写后收敛到新形态。
  3. 往返不双乘：读→模型（毫秒）→再写（RFC3339）→再读走分支 1，值稳定（测试直插旧行验证）。
  4. 非法（`'0x1f'`、小数、空）→ 0=未知，**不回填 now**。

### C. 回退会话头（accepted_at NULL → server_ts / created_at 低可信）

- **checkpoint**：v35/00000229 前旧行两个新事实列均 NULL；会话头/窗口判定需要时间基准。
- **步骤**：
  1. 服务端：`resolve_accepted_ms/1` 降级链 accepted_at(trusted) → server_ts(low_trust) → created_at(low_trust)；全缺/非法 → `{error, no_time_source}`，撤回/编辑 fail-closed 拒绝（500），**绝不用 now() 补**。
  2. 客户端：排序回退键 `accepted_at ?? created_at`（`message_order.dart` 规则 3）；会话头重算（`OfflineMessageWriter.recomputeConversationHead`）对 NULL 列按低可信处理，不提升可信级别、不写入伪值。
  3. 标注纪律：凡经回退链取得的值，窗口判定结果与"新行 accepted_at"语义不同（低可信），UI/日志不得宣称服务端精确时间。

### D. 新排序字段补齐（FL `backfillServerOrderFacts` 幂等）

- **checkpoint**：本地 msg_* 行 `conv_seq`/`accepted_at` 为 NULL，而服务端已能提供权威值（history/实时/离线/ACK 响应携带）。
- **步骤**：
  1. 四路径共用写入点（`offline_message_writer.insertData`）携带两列；`parseServerOrderFacts()` 校验 conv_seq 正整数、accepted_at 毫秒/RFC3339 归一，非法拒绝（绝不伪造序号）。
  2. 去重分支（msg_id 已存在，含 SERVER_ACK 标准化）调 `MessageRepo.backfillServerOrderFacts`：SQL `COALESCE` **仅补 NULL**，重复/错乱值拒改；不覆盖 created_at/正文/密文/已有可信值（FL_W5 §2.1）。
  3. 幂等：重复执行第二次为 no-op（R07-7 archive→重复 archive→晚 ACK 实测行数不变、值稳定）。
  4. 无服务端值（旧服务端响应不含字段）→ 保持 NULL，留在无 seq 排序组，等待后续 backfill。

---

## 5. 不可恢复边界（明确不可可靠恢复，禁止掩盖）

以下三类值**在数据层已被不可逆污染或缺失**，任何事后计算都不能可靠还原。禁止用 `now()`、统一 +8h、统一锚点等手段"修复"——那会把污染从"可识别"变成"不可识别"：

1. **未知旧 `server_ts`**：C2G 旧行 `server_ts` 已被客户端 `created_at` 写入（`msg_c2g_repo.erl:218-236`，迁移 00000229 前无止污）。事后无法区分服务端时钟值与客户端钟面值，**不可批量回填**；新行已止污（server_ts = accepted_at），新旧以 `accepted_at IS NULL` 分界。
2. **NULL expiry**：迁移前旧消息 `expire_at` 无值且无 accepted_at 锚点，期限不可推算。NULL 行不参与自毁扫描（安全侧：不误删）；不得以 accepted_at 推算补写。
3. **已被伪造值污染**：历史上曾把 `new Date(null)`=1970、`new Date(undefined)`、统一 +8h 等错误值持久化（如 AD 列表 1970 显示类 bug 的落库产物、任何"补 now"写入）。此类值与真实 1970 附近时间不可区分，**不识别、不批量改写**；消费方按 null/未知处理新读路径，存量由 §4 有界恢复收敛，收敛不到的保留原值并接受其不可信。

---

## 6. 回滚策略登记

1. **新增事实字段不物理回滚**：应用代码回滚时，优先保留 v35（FL conv_seq/accepted_at 两列 + 4 索引）与 00000229（PG 四表 accepted_at + staging expire_at）列。两份迁移的 down（`DROP COLUMN`）**仅限隔离演练库执行**（00000229 `.up.sql` 文件头与 BE_W1 报告 §1.2 明文）；生产/共享库禁止物理 down——down 会丢失已积累的服务端权威事实，且 down 后需服务端重同步（FL v35 迁移元数据 `down 后 conv_seq/accepted_at 值丢失需服务端重同步` 已声明 dataLoss）。
2. **代码回滚读能力**：
   - v1 cursor token：新旧代码双向可解码（§2.3），回滚不破坏旧客户端进度。
   - v2 cursor token：回滚到 base 后 decode 必失败（`{error, invalid_offline_cursor}`，§3.2）。**后果是显式错误 + 客户端按游标失效重同步（C2G 从 marks 清零重拉），不是数据丢失**；接受该重放成本即可安全回滚，无需迁移。
   - history 增量字段（safe_head/blocked/gaps/conv_seq/accepted_at）在旧代码下不被生成，旧客户端本来就不消费——回滚零残留。
   - 回滚后新写入行不再有 accepted_at（旧代码不含该列 INSERT）→ 这些行回到低可信档，与存量旧行同类，读侧降级链已覆盖；已写入的行保留其 accepted_at 值不被清除。
3. **FL v35 回滚**：降级路径先 DROP 4 索引再 DROP 8 列（SQLite 禁止删除被索引引用的列，upgrade_edges_v35 down 边）；不降级 schema、只回滚代码时，新代码列与旧代码互不干扰（旧代码不 SELECT 这两列）。

---

## 7. 验证抽查记录（AC R22-1：doc 声明 ↔ 文件:行）

本文件每类声明抽查代码 3+ 处（2026-10-26，worktree `repair/timezone-v1/backend` @ 未提交 diff、FL worktree `repair/timezone-v1/flutter`）：

| # | 文档声明 | 抽查证据（文件:行） |
|---|---|---|
| 1 | 通用 REST `_at`/`_ts` = 毫秒 int | `src/lib/elib_response.erl:32/42/54`（success 系全量 convert）；`src/lib/elib_cnv.erl:107-127`；`src/lib/elib_dt.erl:265-295`（整数直通 / tuple→ms / 空与失败→null） |
| 2 | `sv_ts` = 毫秒 | `src/lib/elib_response.erl:225`；`src/adm/adm_auth_middleware.erl:300`；`docs/reference/api-format.md:35`（文档互证） |
| 3 | 内部管线 `created_at` = RFC3339 微秒 | `src/logic/msg_c2c_logic.erl:127`（`elib_dt:now()`）；`src/lib/elib_dt.erl:118-128`（now()=microsecond→to_rfc3339）；`docs/api-contracts/message_api_contract.md` §5（互证） |
| 4 | EB 域 = 秒、不得改毫秒 | `src/features/enterprise_business/infrastructure/eb_pg_store_sql.erl:447-454`（合同注释）、`:464/479`（`convert(ts,..)`→`rfc3339_to(Value, second)`）；`test/features/enterprise_business/infrastructure/eb_time_unit_contract_tests.erl`（BE_W4 合同测试） |
| 5 | `accepted_at` D04 合同（timestamptz 可空、首次接受固定、不覆盖、NULL 不回填） | `priv/migrations/00000229_msg_accepted_at.up.sql`（头注释 + `ADD COLUMN IF NOT EXISTS accepted_at timestamp with time zone`）；`src/repo/msg_store_repo.erl:1289-1290`（resolve_accepted_ms 签名）；`src/logic/msg_c2c_logic.erl`（c2c/3 内 `elib_dt:millisecond` 生成链，BE_W1 §1.1#1） |
| 6 | `safe_head`/`blocked`/`gaps` D05 合同 | `src/repo/msg_archive_repo.erl:43/490-493/577`（导出、safe_head_tx、terminal_gap_tx）；`src/logic/messaging_logic.erl:193/267/287-289`（钳制与响应键） |
| 7 | `history_backfill` 有界补扫 | `src/logic/messaging_logic.erl:11/303-308`；`src/repo/msg_archive_repo.erl:43/472-473`（backfill_range_limit）；R02-5 故障注入（1001 行 → 400） |
| 8 | cursor v1/v2 双向兼容与失败语义 | `src/logic/msg_offline_cursor.erl:10-17/30-47/51-58`（v1 分支保留注释、v2 marks、`invalid_offline_cursor`）；base `3106e6c:src/logic/msg_offline_cursor.erl`（git show 对比：无 v2 分支，v2 落 `decode_data(_) → error`） |
| 9 | `read_at` payload 缺省服务器时间 | `src/logic/msg_c2c_logic.erl:1026`；`src/logic/msg_c2g_logic.erl:644/665` |
| 10 | `expires_at` 不回退 now | `src/logic/msg_burn_logic.erl:25/62-79`（calc_expire_at/2：undefined/0 → null，非法 AcceptedAt 不回退）；`src/ds/msg_store_ds.erl`（set_expire_at `AND expire_at IS NULL`） |
| 11 | credential / moments / 举报 = 毫秒 | `src/adm/adm_enterprise_application_handler.erl:173-174`（success 链）；AD_W1 §R08 表格（credentials GET 毫秒互证）；AD_W3 §1.1 三端点逐跳证据链 |
| 12 | DateOnly + zone 参数化 | `src/repo/review_queue_repo.erl:31-42/99-100/155-172`（AT TIME ZONE $zone、start_at/end_at 优先级、列不包装）；BE_W4 EXPLAIN 日志（`evidence/logs/be_w4_review_queue_explain.log`：上海 16:00Z 边界、纽约 DST 日） |
| 13 | `tz_offset` 分钟 + 422 | `src/features/customer_service/interfaces/cs_http.erl:1005-1009`（invalid_tz_offset → 422）；state.json R22-A units_frozen |
| 14 | FL 排序五键与"仅同会话"边界 | FL `lib/store/model/message_order.dart:1-60`（doc 规则 1-5、orderByAsc SQL 片段）；`lib/service/sqlite.dart` v35 边（FL_W4）；`message_order_test` 双向守护 |
| 15 | FL EB 秒→毫秒、0=未知 | FL `lib/modules/enterprise/domain/enterprise_models.dart:240-306`（`parseEpochSeconds ?? 0`、`retain_until: ms ~/ 1000` 回写秒合同） |
| 16 | 污染源证据（§2.2 server_ts / created_at 行） | `src/repo/msg_c2g_repo.erl:218-236`、`src/logic/msg_c2c_logic.erl:192-193`、`src/repo/msg_store_repo.erl:196-211`（00000229 头注释引用链，与 D04 basis 一致） |

**登记与代码不一致的处理**：发现漂移时先改代码或在例外台账登记，再改本文件；本文件不得先于代码声明"未来行为"。

---

## 附：FL / AD 侧文档同步清单（owner 待办，本文档不代写）

- FL worktree（`repair/timezone-v1/flutter`）仅有 `AGENTS.md`，无独立 conventions 文档；时间解析入口合同在 `lib/util/api_time.dart` doc 与 `enterprise_models.dart` 字段注释中，建议 owner 在其仓级文档补一行指向本文件（尤其 §1 消费方铁律与 §4-B 形态判别）。
- AD worktree（`repair/timezone-v1/admin`）同上；AD 侧权威实现在 `src/lib/apiInstant.ts`（unit 显式声明），建议在其 AGENTS.md 登记本文件为后端 wire 单位真源。
- 后端仓内其他文档（`api-format.md`、`message_api_contract.md`）已与本文件一致，无需改写；`CONVENTIONS.md` §2 已由 R22-B 改写为指向本文件。
