# IMBoy 三仓代码质量评审（Fact-based）

- **日期**：2026-07-22
- **方法**：只读评审。全部结论基于磁盘文件与 `文件:行号` 证据（行数来自 `wc -l`，长函数为函数头间距近似值 ±5 行）。
- **项目规范基线**：函数 <50 行、文件 <800 行（根 CLAUDE.md 编码规范）。

---

## 1. 文件规模超标清单（>800 行）

### imboy 后端（排除生成物 `imboy_pb.erl` 6018 行）

| 文件 | 行数 | 说明 |
|---|---|---|
| src/adm/adm_channel_handler.erl | 1044 | handler 层最大 |
| src/mcp/barrel_mcp_session.erl | 1027 | |
| src/mcp/barrel_mcp_protocol.erl | 1000 | |
| src/adm/adm_admin_handler.erl | 983 | |
| src/imboy_router.erl | 977 | 静态路由表，天然大，可接受但接近失控 |
| src/logic/moment_logic.erl | 962 | |
| src/logic/msg_c2c_logic.erl | 937 | 且含多个超长函数（见 §2） |
| src/api/websocket_handler.erl | 925 | |
| src/adm/adm_group_helper.erl | 921 | |
| src/lib/imboy_policy.erl | 863 | |
| src/logic/msg_c2g_logic.erl | 843 | |
| src/logic/passport_logic.erl | 829 | |
| src/ds/message_ds.erl | 801 | |

**风险 P2**：13 个手写模块超 800 行，集中在 adm/mcp/消息主链路。

### imboyapp（排除 i18n `*.g.dart`、protobuf 生成物）

| 文件 | 行数 | 说明 |
|---|---|---|
| lib/page/chat/chat/chat_page.dart | 2234 | **规范上限的 2.8 倍**，聊天主页面上帝文件 |
| lib/service/embedded_schema_scripts.dart | 2035 | 内嵌 SQLite schema 常量（半生成性质，但需与两处 sql 镜像同步，本身是漂移源） |
| lib/service/message.dart | 1576 | |
| lib/page/moment/moment_detail_page.dart | 1469 | |
| lib/page/moment/moment_feed_page.dart | 1440 | |
| lib/store/repository/message_repo_sqlite.dart | 1435 | |
| lib/service/message_s2c.dart | 1404 | |
| lib/service/message_actions.dart | 1380 | |
| lib/page/mine/user_collect/user_collect_provider.dart | 1368 | |
| lib/page/chat/widget/chat_input.dart | 1283 | |
| lib/service/websocket.dart | 1249 | |
| lib/page/chat/chat/chat_provider.dart | 1215 | |

**风险 P1**：手写 dart >800 行的文件不止 12 个，且集中在消息收发主链路（message*.dart 四件套 + chat_page/chat_provider/websocket）。该链路同时是历史 bug 密度最高区（撤回/编辑/ack/渲染），大文件直接推高回归成本。

### imboyadmin

| 文件 | 行数 | 说明 |
|---|---|---|
| src/pages/roles/RolePermissionPage.tsx | 874 | 超标 |
| src/pages/logs/AuditLogPage.tsx | 833 | 超标 |
| src/components/shared/shared.test.tsx | 932 | 测试文件，可放宽 |

**风险 P3**：admin 整体规模纪律最好，仅 2 个源文件超标。

---

## 2. 函数规模超标（>50 行规范；抽查消息主链路）

`src/logic/msg_c2c_logic.erl`（消息核心逻辑，抽查即中 4 个）：

| 函数 | 近似行数 | 位置 |
|---|---|---|
| `stage_and_send_c2c/...` | ~183 | msg_c2c_logic.erl:176 |
| `c2c_revoke/3` | ~129 | msg_c2c_logic.erl:364 |
| `handle_read_receipt/7` | ~103 | msg_c2c_logic.erl:725 |
| `do_c2c_edit/3` | ~69 | msg_c2c_logic.erl:552 |

其他：`websocket_handler.erl:25` `init/2` ~79 行、`websocket_handler.erl:404` `handle_protobuf_client_ack/3` ~69 行、`adm_channel_handler.erl:316` `update_admin_role_action/3` ~61 行。

**风险 P2**：超长函数与死测试普查抓出的真 bug 高度同区（c2c_revoke 离线分支曾必崩，census bug#4）——183 行的 `stage_and_send_c2c` 是分支覆盖最难写全的地方。

---

## 3. 分层边界（后端 Handler→Logic→DS→Repo）

**结论：非常干净（这是本次评审的正面发现）。**

- api handler 直调 `*_repo:`：**0 处**。
- ds 反向调 `*_logic:`：**0 处**；repo 反向调 logic/ds：**0 处**。
- 唯一违规：`src/adm/adm_feedback_handler.erl:147` 与 `:189` 直调 `feedback_repo:tablename()`（handler 穿两层拿表名拼查询）。**风险 P3**，但属破窗，建议经 logic/ds 转发。
- Flutter 侧有 `scripts/check_boundaries.dart` 边界检查器，但 CI 中 `continue-on-error: true`（imboyapp/.github/workflows/ci.yml:48-50），边界纪律尚未硬化。P3。

---

## 4. 死代码 / 未接线设施（对既有认知做了在盘核实，两项已翻案）

| 项 | 在盘事实 | 结论 |
|---|---|---|
| `msg_rate_logic` | `imboy_app.erl:41` init_table；`msg_c2c_logic.erl:45`、`msg_c2g_logic.erl:65` 调 `check_and_record`；`adm_admin_handler.erl:576,600` 调 `unmute` | **已接线，非死代码**（旧结论"零接线"过时） |
| `textStream` | `lib/service/message_type_constants.dart:40,92-93,130-131,200`，chat_page/chat_network_service/message_model 均引用 | **已复活接线**（Phase2 流式回复），非死代码 |
| liveRoom | `src/api/live_room_handler.erl`(200 行)、`src/logic/live_room_logic.erl`(32 行)、ds/repo 齐全，挂在 `imboy_router.erl` 与 `imboy_app.erl`；`grep -rl live_room imboy/test` **零命中** | **冻结但已暴露在生产路由、零测试**。P2：要么下路由，要么补最小 handler 测试 |
| B 类死测试 7 文件 | 见 testing-review §1.2 | 测试侧死代码，待授权清理 |
| admin 死代码工具 | package.json 有 `deadcode: knip`，但不进 CI | 工具在、门禁缺。P3 |

---

## 5. 错误处理

### 5.1 后端（总体良好，有历史教训沉淀）

- `src/lib/elib_pg.erl:200-214`：execute 路径 catch 后 `?ERROR_LOG` 记录 Class/Reason/Stacktrace 并返回 `{error,...}`——不吞错。
- `elib_pg.erl:90-120` `with_conn` 对 `throw:{abort_tx,...}`/`throw:{rollback,...}` 显式拦截并附长注释解释钱路径 case_clause 事故——错误语义有文档化。
- 历史事故（"附近的人"生产返空）：epgsql 参数带 `::float8` cast 致查询崩溃、错误被上层吞掉返空列表。教训"传 binary 参数勿加 ::cast、返空先查查询是否崩溃"值得写进 `docs/standards/`；当前 `src/ds/geo_people_nearby_ds.erl` spec 已声明 `{ok,[map()]} | {error,any()}`（geo_people_nearby_ds.erl:44-46）。
- 死测试普查证实过两处**静默吞错真 bug**（均已修）：`group_ds:dissolve_group` 忽略 with_tx 返回值恒返 ok（census:28）、`msg_c2c_ds:revoke_offline_msg/9` 返回值违 spec（census:29）。模式=「忽略下层返回值/只匹配成功分支」，建议 dialyzer 收紧后可拦一部分。

### 5.2 Flutter（P1）

`grep -rn "catch (_) {}" imboyapp/lib` 共 **20 处**空 catch（连日志都不打），高度集中：

- `lib/page/chat/chat/services/chat_burn_service.dart:150,154,202,224,245,252,324`（7 处，阅后即焚服务——**该功能吞错等于焚毁逻辑失败也无痕**）
- `lib/page/chat/chat/mixin/chat_event_subscription_manager.dart:288` 等。

违反根规范"永远不要静默吞掉错误"。历史上"真机客户端异常被 catch 静默吞"正是朋友圈拍照上传排障数日的根因，同一反模式仍在库内 20 处。**风险 P1**。

---

## 6. 不可变性 / 魔法数 / 硬编码

- **魔法数（正面）**：重试节奏收敛于 `src/lib/elib_retry_config.erl`（单一真值源，C2C `[0,3s]` 等），符合"命名常量"规范；错误码统一 `include/error_code.hrl`。
- **WS idle timeout**：真值 `config_ds:env(ws_idle_timeout_ms, 180000)`（census:90 佐证），配置化而非硬编码，正例。
- **Flutter 硬编码颜色存量（P2）**：`grep "Colors\.|Color(0x"`（排除 theme/、Colors.transparent）约 **2000+ 处**（不同过滤口径复算在 2099–2430 区间，量级稳定，精确值随排除规则浮动）。lefthook design-tokens 门只拦 staged diff 的**新增行**（imboyapp/lefthook.yml design-tokens command），存量不受任何管控——token 化迁移（AppColors/AppSpacing/FontSizeType）只完成了增量冻结，未完成存量清偿。
- **不可变性**：Erlang 天然不可变；Flutter 侧 Riverpod/Provider 状态模式为主，本次未抽查到就地 mutation 违例（未逐文件扫描，不下全称结论）。

---

## 7. 已知技术债（在盘核实）

| 债项 | 证据 | 状态/风险 |
|---|---|---|
| `erlang.mk` vendored 禁改 | imboy/CLAUDE.md 构建规则；erlang.mk 在仓根 | 已制度化（自定义逻辑只进 Makefile），P3 |
| AGPL 发布门（vodozemac） | imboyapp/pubspec.yaml:221-222（`flutter_vodozemac: ^0.5.0` + `vodozemac: ^0.5.0`，AGPL-3.0） | **未解决**。闭源发售/公开分发前必须三选一（开源/商业授权/换绑定）。P1（法务性质，非代码） |
| TS7 升级受阻 | imboyadmin/package.json devDependencies：`typescript ~6.0.3`、`typescript-eslint ^8.48.0`；typescript-eslint 全系尚不支持 TS7（PR#16 已关闭待生态） | 挂起等生态，P3 |
| 迁移乱序 | priv/migrations 现存至 `00000047_trust_event_freshness`；曾有 41 号乱序经 renumber 为 46（`00000046_compliance_key_drop_private`）排到 42-45 之后规避 strict 检测 | 已定夺处置，但说明 strict 乱序检测被"绕行"过一次，旧环境部署需按既定 UPDATE 映射操作。P2（运维债） |
| SQLite schema 三处同步 | lib/service/embedded_schema_scripts.dart（2035 行常量）+ 两处 sql 镜像 | 结构性漂移源（历史已踩），P2 |

---

## 8. 汇总表

| # | 发现 | 仓 | 风险 | 证据 |
|---|---|---|---|---|
| 1 | 20 处 `catch (_) {}` 静默吞错，7 处集中在阅后即焚服务 | imboyapp | **P1** | chat_burn_service.dart:150,154,202,224,245,252,324; chat_event_subscription_manager.dart:288 |
| 2 | 消息主链路巨型文件群：chat_page 2234 行等 12+ 个手写文件 >800 行 | imboyapp | **P1** | §1 表；chat_page.dart=规范 2.8 倍 |
| 3 | AGPL vodozemac 发布门未解 | imboyapp | **P1**(法务) | imboyapp/pubspec.yaml:221-222 |
| 4 | 后端 13 个手写模块 >800 行（adm/mcp/消息链路） | imboy | P2 | adm_channel_handler.erl 1044 等，§1 表 |
| 5 | msg_c2c_logic 单函数最长 ~183 行（stage_and_send_c2c），同文件 4 函数超标 | imboy | P2 | msg_c2c_logic.erl:176,364,552,725 |
| 6 | liveRoom 冻结功能挂生产路由、零测试 | imboy | P2 | src/*/live_room_*; imboy_router.erl; test 零引用 |
| 7 | Flutter 硬编码颜色存量约 2000+ 处，token 门只拦增量 | imboyapp | P2 | grep 计数（2099–2430 区间随口径浮动）；lefthook.yml design-tokens |
| 8 | SQLite schema 三处镜像同步（embedded_schema_scripts 2035 行） | imboyapp | P2 | lib/service/embedded_schema_scripts.dart |
| 9 | 迁移 strict 乱序检测被 renumber 绕行一次（41→46） | imboy | P2 | priv/migrations/00000046_* |
| 10 | adm_feedback_handler 直调 repo（唯一分层破窗） | imboy | P3 | adm_feedback_handler.erl:147,189 |
| 11 | admin 2 个页面文件超 800 行 | imboyadmin | P3 | RolePermissionPage.tsx 874; AuditLogPage.tsx 833 |
| 12 | knip 死代码检测不进 CI | imboyadmin | P3 | package.json scripts vs ci.yml |
| 13 | 旧认知修正：msg_rate_logic、textStream 均已接线，非死代码 | imboy/app | 信息 | imboy_app.erl:41; msg_c2c_logic.erl:45; message_type_constants.dart:40 |
