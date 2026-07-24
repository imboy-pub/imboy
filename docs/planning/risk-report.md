# IMBoy 风险台账（Risk Report）

> Fact-based Review 汇总 · 只读评审 · 基线三仓 `1.0.0-alpha.15` · 日期 2026-07-22
> 汇总 9 份领域/主题评审的全部 P0–P3 发现。每条附 `文件:行号` 证据。分歧项由主评审读码裁决（标注"裁决"）。
> 详细上下文见各专项文档；本表是唯一的全量优先级视图。

---

## 严重度定义

| 等级 | 含义 | 处置 |
|---|---|---|
| **P0** | 阻断性：安全越权/数据丢失/核心功能生产不可用/法务一票否决 | 发布前必须修 |
| **P1** | 严重：可靠性缺口、越权、集群崩溃、可维护性重伤 | 尽快修，排入近迭代 |
| **P2** | 中等：安全债、性能天花板、约定未收口 | 计划修 |
| **P3** | 建议：文档漂移、坏味道、优化机会 | 择机 |

---

## P0 — 阻断性（4 项）

| # | 域 | 问题 | 证据 | 备注 |
|---|---|---|---|---|
| **P0-1** | 后端/安全 | `auth_middleware:execute/2` 的 `<<"/v1/",_/binary>>` 分支永不匹配真实路径 `/api/v1/*`，`auth_middleware_api_v1` 整族沦为死代码：① 第三方支付回调 `/api/v1/payment/callback/:gateway`、频道 webhook 的免签豁免写在死代码里，生产 `api_auth_switch=on` 下第三方回调 100% 被 902 拒；② passport/ws/init/refreshtoken 落默认分支，设备签名门静默失效 | `src/api/auth_middleware.erl:34-36`（分支）; `src/imboy_router.erl:41-52,518`（真实前缀 `/api/v1/`）; `src/api/auth_middleware_api_v1.erl:22-33`（豁免在此不可达） | **主评审已读码复核确认**。同源于 2026-07 路由前缀硬切，`/api/adm` 修了、`/api/v1` 漏了 |
| **P0-2** | 性能/并发 | `user_server` 单 gen_server 串行处理全站上线/下线，单次含 DB 写 + 3×5000 行离线消息读 + 好友全量 fanout；重连风暴必积压，节点级吞吐天花板 | `src/logic/user_server.erl:36,94-127,218-223` | 消息热路径穿越单进程 |
| **P0-3** | 性能/并发 | `depcache` 唯一 gen_server 被用于每消息每设备的投递定时器 `set` 与每次 ACK 的 `set+flush`（同步 call），横穿消息热路径。ACK 定时器不需要依赖失效语义，属误用（经 `imboy_cache` 封装层落到 depcache，非直接调） | `deps/depcache/src/depcache.erl:454,574`; `src/ds/message_ds.erl:154`; `src/logic/websocket_logic.erl:58,71` | 根治：换并发 ETS 表 write_concurrency（仓内 `agent_rate_limiter.erl:84` 是正确范本：单 named_table+write_concurrency，非分片） |
| **P0-4** | Flutter/法务 | `flutter_vodozemac` / `vodozemac` 为 **AGPL-3.0**，与闭源商业售卖冲突，7 个源文件直接 import；发布前未裁决（开源/购授权/换绑定三选一） | `imboyapp/pubspec.yaml:221-222` | 产品级一票否决闸门，非代码缺陷但发售即触发 |

---

## P1 — 严重（分域列出）

### 认证 / 授权

| # | 问题 | 证据 |
|---|---|---|
| P1-A1 | 计费 API 全族对象级越权（IDOR/BOLA）：9 个认证后端点忽略 `current_uid`，`tenant_id`/`subscription_id`/`invoice_no` 全取自入参无归属校验，任意 JWT 可订阅/取消/上报用量/支付**任意租户**账单，`invoice_pay` 走真实扣款 | `src/api/billing_handler.erl:70,91,117,133,147,174,201,226,246`（各端点函数头）; `src/imboy_router.erl:524-527` |
| P1-A2 | `/api/adm/setup/*` 首启向导在 `imboy_router:open/0(:930-932)` 但 `adm_auth_middleware:execute` 不查 open/0 也无 setup 分支 → 全新部署 401 不可达（路由收口回归） | `src/adm/adm_auth_middleware.erl:19-44` |
| P1-A3 **（裁决）** | 管理后台 Cookie 签名密钥硬编码默认值 `imboy-adm-cookie`；`imboy_app.erl` 有生产 fail-fast 兜底，但依赖 `is_strict_env` 判定正确，误配 `IMBOYENV` 即穿透，且与"Cookie 无过期/不可吊销"叠加放大 → **定级 P1**（Admin agent 判 P0 偏重、安全 agent 判 P2 偏轻） | `src/adm/adm_auth_middleware.erl:220`; 兜底见 `src/imboy_app.erl:318` |
| P1-A4 | 管理后台前端权限门 `useAdminPermission` 为自觉标注的 fail-open：RBAC 端点不可用时降级为角色级放行 | `imboyadmin/src/hooks/useAdminPermission.ts:93-97` |

### 集群 / 可靠性

| # | 问题 | 证据 |
|---|---|---|
| P1-C1 | `imboy_syn` 与 `message_ds` 用 `erlang:start_timer` 对 syn 可能返回的**远端 Pid** 投递；OTP 的 `erlang:start_timer/3` 要求 Dest 为本地 pid，集群模式下**假设**会 badarg（待多节点实测确认，HYPOTHESIS），与"syn 跨节点投递"宣称矛盾（单节点无影响）。严重度按证据分层：单机 P3、集群 P2/待实测（与 backend-review F-04 口径一致） | `src/lib/imboy_syn.erl:166,172`; `src/ds/message_ds.erl:121,150` |
| P1-C2 | Flutter 启动重试扫描仅取每表前 100 条，溢出的失败消息永不重试 → 积压离线发送丢失 | `imboyapp/lib/service/message_retry.dart:178` |
| P1-C3 | Flutter 本地 DB 降级脚本止于 v17，v23 降级无脚本却返回 success，schema 静默不降 | `imboyapp/lib/service/migration_service.dart:179-185` |

### 数据 / 资金完整性

| # | 问题 | 证据 |
|---|---|---|
| P1-D1 | 钱包借记守卫不扣 `frozen`：`atomic_balance_change` 既不查 `status` 也不扣 `frozen`（`:119` WHERE 仅 `balance + $1 >= 0`），`do_debit` 已查 `status=1` 但仍不扣 `frozen`（`:197`）；冻结资金可被转账/红包花掉，`frozen<=balance` 不变量可破且无表级 CHECK 兜底；同库 `recharge_order_repo.erl:271` 却是正确守卫（两套写法并存，`atomic_balance_change` 为首要风险） | `src/repo/wallet_repo.erl:119,197`; 正例 `src/repo/recharge_order_repo.erl:271` |
| P1-D2 | 全链路无 `statement_timeout`（grep 零命中），连接池耗尽走 `sleep(1s/2s/3s)` 盲重试阻塞热路径（`?DEFAULT_TIMEOUT=1000` 实为重试延迟非超时）→ 慢查询占死 80 连接池并被重试放大 | `src/lib/elib_pg.erl:64,92-95,197-199,237` **（数据库+性能两 agent 交叉命中）** |
| P1-D3 | `elib_pg_sql` 标识符拼接无校验 + `{raw}`/`__raw` 逃生门，注入防线靠约定（当前 10 处 raw 均安全但结构未封死） | `src/lib/elib_pg_sql.erl:333-334,475-477,553-557` |
| P1-D4 | hypertable 上 `msg_id` 去重唯一键含 `created_at`，跨时间戳重发不拦截，重复消息无 schema 防线 | `priv/migrations/00000005*.sql:110` |

### 协议 / 契约一致性

| # | 问题 | 证据 |
|---|---|---|
| P1-P1 | C2S_SERVER_ACK 走同步 `{reply,Map}` 路径被 protobuf 枚举编码丢 type，v2 连接上 C2S 回执失效（webrtc 已修过的同伤口复发） | `src/logic/msg_c2s_logic.erl:198-213`; `src/lib/imboy_codec.erl:255-268` |
| P1-P2 | Flutter `endsWith('_ACK')` 大小写敏感过滤，小写 `message_revoke_ack` 纯靠大小写巧合命中，后端一旦大写化撤回确认即被吞（与 P1-P1 同为 ACK 链路隐患） | `imboyapp/lib/service/websocket.dart:774,825`; `message_actions.dart:426,572` |
| P1-P3 | imboy-sdk-js 5 项契约漂移必致失败：login 发 `password` 后端读 `pwd`；quickLogin 参数不符；引用已删 e2ee 端点致 404；监听 `token_refresh_required` 后端实发 `please_refresh_token`；sendBinaryAck 发裸 8 字节从不协商子协议被静默忽略 | `imboy-sdk-js/src/api/passport.ts:9-13`; `src/api/e2ee.ts:52-84`; `src/websocket.ts:143,210-218,258` |
| P1-P4 | app protobuf 生成物含 proto 不存在的幻影枚举 C2CH=5/C2CH_SERVER_ACK=24/C2CH_DEL_EVERYONE=14，protobuf 三端不同源无 regen 门禁 | `imboyapp/lib/service/protocol/imboy.pbenum.dart:30,53,151` |
| P1-P5 | C2G_ERROR 全字段蒸发：群拒发（禁言/非成员/限流/@all 拒发）走同步回执经 `to_pb_map` 白名单转换，error/code 字段静默丢失，客户端无任何分支能收到 → 群消息被拒但用户无感 | `src/logic/msg_c2g_logic.erl:68-135`; `src/lib/imboy_codec.erl:214-228` |
| P1-P6 | 默认 `ws_url` 指向不存在的 `/ws`，真实路由仅 `/api/v1/ws`；未显式设 `IMBOY_WS_URL` 的部署三端 WS 全断（配置门控故 P1，但漏配即全站离线） | `config/sys.config:51`; `src/lib/imboy_env.erl:14`; `src/imboy_router.erl:80` |
| P1-P7 | proto `E2EEMeta` 无 `keys[].olm{type,body}`，与 room-key-over-Olm 脱节；codegen 启用即固化旧契约 | `proto/imboy.proto:128-155`; `imboyapp/lib/service/group_session_service.dart:224-240` |

### 前端 / 客户端质量

| # | 问题 | 证据 |
|---|---|---|
| P1-F1 | @riverpod codegen 默认 autoDispose 无 lint 门禁，历史两次真 bug 现仅靠注释防御，复发面覆盖全部 67 个 Notifier | `imboyapp/lib/page/chat/widget/chat_input.dart:133,194`（注释防御样本） |
| P1-F2 | `safeParseBigIntJson` 正则会误改写字符串内部 16+ 位数字（如消息正文含卡号）→ 非法 JSON → 整页静默拒服，单测无此用例 | `imboyadmin/src/lib/safeParseBigIntJson.ts:20`; `imboyadmin/src/services/api/client.ts:60-70` |
| P1-F3 | Flutter DDL 三镜像手工同步已漂移（CLAUDE.md v21 / 代码 v23）；`msg_c2c.id` 声明 INTEGER 实存 String Xid（同 QA#31 kind_id 归零隐患） | `imboyapp/lib/service/embedded_schema_scripts.dart:12-16,195`; `sqlite.dart:41` |

### 代码质量 / 错误处理

| # | 问题 | 证据 |
|---|---|---|
| P1-Q1 | Flutter 20 处 `catch (_) {}` 静默吞错，7 处集中在阅后即焚（焚毁失败无痕，安全语义敏感） | `imboyapp/lib/page/chat/chat/services/chat_burn_service.dart:150,154,202,224,245,252,324` |
| P1-Q2 | 消息主链路巨型文件群：`chat_page.dart` 2234 行（规范 2.8 倍）+ 12+ 个 >800 行手写文件，历史 bug 密度最高区 | `imboyapp/lib/page/chat/chat/chat_page.dart` |

### 性能（P1 补充，P0-2/P0-3 之外）

| # | 问题 | 证据 |
|---|---|---|
| P1-PF1 | C2G 扇出在发送者 WS 进程内同步 O(N)，撤回 O(成员×设备) | `src/logic/msg_c2g_logic.erl:386-392,611-626` |
| P1-PF2 | 投递管道 JSON 中间格式导致 protobuf/v2 客户端每消息每设备 decode+re-encode | `src/api/websocket_handler.erl:858-893` |
| P1-PF3 | 离线消息一次上线最多拉 3×5000 整行只为对比阈值 10；`msg_store_worker` 单 worker 持久化天花板（批 100/1s） | `src/ds/message_ds.erl:358-362`; `src/ds/msg_store_worker.erl:44-46` |

### 测试

| # | 问题 | 证据 |
|---|---|---|
| P1-T1 | 三仓覆盖率均无阈值门，书面目标（Repo80/Logic70/Handler60/整体65）全不可验证 | `imboy/Makefile`（无 cover）; `imboyapp` ci.yml; `imboyadmin` 无 coverage 配置 |
| P1-T2 | admin 9 个 Playwright E2E spec 在所有 workflow 零引用，E2E 纯手动无回归 | `imboyadmin/.github/workflows/*.yml` |
| P1-T3 | mock 协议/存储边界反模式：`dead-tests-census.md:25-32` 的 5 个真生产 bug（离线撤回必崩等）404 个单测无一发现，仅真 PG 的 CT 抓出 | `imboy/docs/planning/dead-tests-census.md:25-32` |
| P1-T4 | `integration_test.yml` 坏死工作流（paths/working-directory 指向不存在的 `imboyapp/` 子目录） | `imboyapp/.github/workflows/integration_test.yml` |

---

## P2 — 中等（精选）

| # | 域 | 问题 | 证据 |
|---|---|---|---|
| P2-1 | 安全 | JWT 无吊销通道，封禁用户存量 token 到期前全 API 可用 | `src/ds/token_ds.erl:55-97` |
| P2-2 | 安全 | 口令用单轮 HMAC-SHA512（快哈希非记忆硬 KDF） | `src/lib/elib_password.erl:28-31` |
| P2-3 | 安全 | `jwt_key` 与 `postgre_aes_key` 复用同值，DB/配置泄露时放大 | 安全评审文档 |
| P2-4 | 后端 | `imboy_cache:start_link` 返回 `self()` 丢弃 depcache 真实 Pid，缓存崩溃不自愈 | `src/lib/imboy_cache.erl:44-62` |
| P2-5 | 后端 | Cowboy listener 先于监督树启动，滚动发布有可用性窗口 | `src/imboy_app.erl:92-99` |
| P2-6 | 后端 | `kick_device` 非法返回值靠 crash 实现断连；`msg_store_worker` 无死信剔除的无效阻塞记录风险 | `src/api/websocket_handler.erl:599-608` |
| P2-7 | 后端 | 路由 `required_feature` 是中间件从未消费的漂移契约 | `src/imboy_router.erl:857-867` |
| P2-8 | 代码质量 | liveRoom 四层齐全挂生产路由却零测试引用（真死资产） | code-quality-review 文档 |
| P2-9 | 数据库 | history 回填无 ON CONFLICT；单事务内 `create_hypertable` 的 TSDB 版本耦合 | `erlang_migrate/src/erlang_migrate.erl:529-533` |
| P2-10 | Admin | 微秒时间坑仅公共入口根治，`MutedUsersPage.tsx:189`/`DashboardPage.tsx:186`/`NotificationPanel.tsx:110` 仍裸用 `new Date()`；zod 验证仅落地 Login/Setup 两页 | 见证据列 |
| P2-11 | 后端 | `config_ds` 直连 elib_pg 违反自定 DS 规则；6 个 logic→repo 跳层模块 | backend-review 文档 |
| P2-12 | 推送 | 推送单通道 FCM，国内设备降级后无离线推送 | `imboyapp/lib/service/push_notification_service.dart:64-79` |

---

## P3 — 建议（精选）

| # | 问题 | 证据 |
|---|---|---|
| P3-1 | 文档漂移：`msg_archive_enabled` 在 `sys.config:104` 为 true，根 CLAUDE.md 称默认 false | `imboy/config/sys.config:104` |
| P3-2 | 文档漂移：根级把 `imboyadmin` 记为 Vue，实为 React 19.2 | `imboyadmin/package.json` |
| P3-3 | 唯一分层破窗：`adm_feedback_handler` 直调 `feedback_repo:tablename()` | `src/adm/adm_feedback_handler.erl:147,189` |
| P3-4 | ADR 仅 3 条，未覆盖 E2EE/支付/LiveKit/MCP 等重决策 | `imboy/docs/adr/` |
| P3-5 | B 类 7 个死测试文件（含全部 3 个性能测试）未清 | `dead-tests-census.md` |
| P3-6 | proto 双拷贝（`proto/` vs `src/`）当前一致但无 diff 门禁 | `imboy/proto/`, `src/imboy.proto` |

---

## 统计

| 等级 | 数量 |
|---|---|
| P0 | 4 |
| P1 | 24 |
| P2 | 12（精选） |
| P3 | 6（精选） |

**发布前必清**：P0-1（支付回调死代码）、P0-4（AGPL 法务）、P1-A1（计费越权）、P1-A2（首启 401）、P1-D1（钱包冻结资金）。
