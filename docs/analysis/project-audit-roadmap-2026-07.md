# imboy 全项目巡检与升级路线图 / Project Audit & Upgrade Roadmap

> 版本 2026-07-02（下午复核版） | 巡检范围：imboy(后端 main) + imboyapp(Flutter main) + imboyadmin(React main)
> 方法：5 路并行只读审计（grep/xref/读码），主会话对 4 个最高杠杆项二次自查。全程未改代码。
> **2026-07-02 下午逐条复核**：SEC-01/03/05、FEAT-01/03、ARCH-01、PERF-01/02、CONTRACT-01/02/03、OPS-01/03、BIZ-01、r_upgrade 越界全部当场重验属实；**SEC-02 已由并发会话提交完成（commit `5cb86897`），从 Phase 1 移除**。
> 执行分工：本文档由 Fable 出方案，实现由 glm-5.2 盲执行。每条任务力求"照做即对"。

## ⚠️ 执行前必读（并发 git 危险）

- 巡检期间检测到**并发会话在操作 git stash**（`stash@{0}: On main: pre-dedup-stash` 内容会变动）。
- glm 执行任何 git 写操作前，先 `git stash list && git status --porcelain && git log --oneline -3` 确认状态，**勿擅自 stash pop / restore / commit 他会话的改动**。
- 三仓非同一 monorepo：写文件前 `cd <repo> && git rev-parse --show-toplevel` 确认。
- 后端提交陷阱：erlfmt pre-commit 会格式化；`make format` 有全量副作用（污染 160+ 文件，需 `git restore` 还原）；PostToolUse 会 auto-stage，commit 前 `git restore --staged .` 清空再精确 `git add`；DCO 必须 `-s`；sandbox 模式下 commit 不落地（commit 必须默认 sandbox 环境）。

---

## A. 现状健康度概览

| 维度 | 后端 imboy (Erlang) | Flutter imboyapp | admin imboyadmin (React) |
|---|---|---|---|
| **完成度** | 90% — 分层纪律好、SQL 全参数化、六 scope 附件鉴权全接入、E2EE 三恢复链路齐、迁移 strict 已开、DR 文档经实战演练 | 85% — lib analyze 零问题、附件 URL 纪律好、state 无混用、E2EE 客户端三链路齐 | 92% — tsc/eslint 零错、TSID 全局 transform、分页/信封契约一致、权限门控 70 路由全覆盖 |
| **主要债务** | billing 全端点零鉴权(CRIT)；WebRTC 信令可靠性；compliance key 密钥托管破坏 E2EE 语义；11 个>800 行文件；mention 无界查询 | r_upgrade 禁改区被越界改；E2EESettings 死开关与 UI 脱节；194 处 Colors 硬编码(记忆低估3x)；测试套件 8 编译错；11 文件>800 行 | 3 组前端页面调用不存在的后端端点(404)；权限 fail-open；OpenAPI 漏 finance/billing/license 全域 |
| **最高风险** | **billing 越权(CRITICAL)** — 任意登录用户可改套餐价/支付任意账单/越权读租户数据 | **r_upgrade 越界改动泄漏** + E2EE 开关名不副实 | **契约断裂 CRITICAL** — moderation/user-device/sso 三页面上线但后端未实现 |

一句话结论：**三端工程质量整体高于历史记忆预期，唯一真正的 CRITICAL 安全洞是 billing 鉴权缺失（后端）与三组前后端契约断裂（admin↔后端）。** 其余为 HIGH/MEDIUM 完善项。

---

## B. 问题清单（按 8 维度组织）

### 维度 1：安全漏洞

### [SEC-01] billing 全端点零鉴权（越权 + IDOR）  [安全] [CRITICAL]
- **根因**：`src/api/billing_handler.erl:27-49` `init/2` 分派 12 个 action 全部 `(Req0, State)`，无 `current_uid` 提取。核实命令 `grep -c current_uid src/api/billing_handler.erl` → **0**。`src/logic/billing_logic.erl` 中 `tenant_id`/`subscription_id`/`invoice_no` 全部来自客户端自报，无归属校验。路由 `/v1/billing/*`（`imboy_router.erl:570-586`）仅需合法 JWT，登录后无授权分层。
- **影响范围**：
  - 管理端 3 个 action（`plan_create`/`plan_update`/`plan_list`）：任意登录用户可创建/改价套餐 → 权限提升。
  - 租户端 9 个 action（subscribe/renew/cancel/subscription/report_usage/check_quota/invoice_generate/invoice_pay/invoice_list）：客户端自报 tenant/subscription/invoice 且无归属校验 → 水平越权 IDOR（读他人账单、支付任意 invoice_no、污染任意订阅用量）。
- **修复方案**（逐文件）：
  1. `src/imboy_router.erl`：将 `plan_create`/`plan_update`/`plan_list` 三个管理动作从 `/v1/billing/*` **迁移到 `/adm/billing/*`**，走 `adm_auth_middleware`（RBAC）。参照 `adm_finance_handler.erl` 的 admin 路由挂法。
  2. `src/api/billing_handler.erl`：租户端 9 个 action 函数体首行加 `CurrentUid = maps:get(current_uid, State)`（参照 `wallet_handler.erl` 全表用法），下传给 logic。
  3. `src/logic/billing_logic.erl`：`subscribe/renew/cancel/subscription/report_usage/check_quota/invoice_generate/invoice_pay/invoice_list` 各加 `CurrentUid` 形参，在操作前校验 `subscription`/`invoice` 归属 `CurrentUid` 对应的 tenant（查 DS 层归属，无归属返回 `{error, no_permission}`）。若 tenant 模型尚未与 uid 绑定，先补 `billing_ds:tenant_of_uid(Uid)` 映射查询。
- **边界**：不动 billing 的业务计算逻辑（套餐定价、周期换算）；不动 `/v1/billing/plan/list` 的公开读性质（若产品确认套餐列表对所有登录用户可见则保留，但仍需 JWT）。
- **验收 gate**：`make compile` 绿；新增 `test/logic/billing_logic_tests.erl`：断言（a）非 owner uid 调 `invoice_pay` 返回 `{error, no_permission}`；（b）非 admin 调 `plan_create` 路由层被 adm middleware 拒；`make eunit` 绿。
- **glm 执行陷阱**：erlfmt 会重排；提交前 `git restore --staged . && git add src/api/billing_handler.erl src/logic/billing_logic.erl src/imboy_router.erl test/logic/billing_logic_tests.erl && git commit -s`。
- **分工**：**需 Fable 先出详细方案**（tenant↔uid 归属模型是架构判断点，见 D 阻塞项 BLK-04）。
- **回滚条件**：若 tenant 归属模型未定，先只做管理端路由迁移（低风险子集），租户端 IDOR 待模型定案。

### [SEC-02] 群相册/日程/任务/投票读侧 IDOR（含作业越权读 CRITICAL 子项）  [安全] [✅ 已完成 2026-07-02]
- **状态**：已由并发会话提交落地 —— commit `5cb86897` "补齐群任务/投票/日程/相册的群成员IDOR校验"；同批还有 `2262c91f`（频道消息反应/浏览归属校验）、`dc3af70a`（群 S2C_DEL 成员+归属校验）、`8900cf0e`/`aab32d57`（撤回/编辑 from 字段信任消除）、`67ba0103`（标签 SQL 按用户限定）。**本项跳过，勿重复执行**。`stash@{0}: pre-dedup-stash` 仍存在但与本项无关，勿擅动。
- **根因（历史记录）**：早前提交只补了写侧鉴权，读端点遗漏。修复代码**已写但被并发会话 stash**（`stash@{0}`）。其中 `group_task` 的 `pending_review` 原来任意用户凭 task_id 可读其他学生已提交作业+附件（CRITICAL 子项）；相册评论/日程详情/投票统计同类越权读。
- **影响范围**：`group_album_handler+logic`、`group_schedule_handler+logic`、`group_task_handler+logic`、`group_vote_handler+logic` 及对应 3 个 tests 文件。
- **修复方案**：stash 中改动质量已达标（新增带 `ViewerUid` 高 arity 变体，先 `group_ds:is_member/2` 门控再委托原语，函数均 <50 行，分层正确）。**执行 = 恢复 stash + 编译测试 + 提交**，非重写。
- **边界**：不重写已有逻辑；保留低 arity 原语作管理端内部用途。统一 handler 取 uid 风格：确认这些端点在需鉴权区后，用 `maps:get(current_uid, State)` 与 stash 现状一致即可（勿改成 `auth_ds:current_uid/1` 引入默认 0）。
- **验收 gate**：`git stash show -p stash@{0}` 确认是这 12 个文件 → `make compile && make eunit` 绿 → 提交。
- **glm 执行陷阱**：**⚠️ 并发会话正在动 stash，栈序号会变**。执行前 `git stash list` 逐条 `git stash show --stat stash@{N}` 定位到含 group_* 12 文件的那条，勿盲信 `stash@{0}`。恢复用 `git stash apply`（非 pop，保留栈以防误判）。
- **分工**：**[MODEL] glm 可独立执行**（方案已在 stash，只需定位+验证+提交）。
- **回滚条件**：若 stash 已被并发会话提交（`git log` 出现对应 commit），则本项已完成，跳过。

### [SEC-03] red_packet_detail 越权读  [安全] [MEDIUM] [✅ 已完成 2026-07-06]
- **状态**：已修复并提交（commit `0d80be05 fix(security): [SEC-03] 红包详情越权（IDOR）修复`），勿重复执行。`test/logic/red_packet_logic_tests.erl` 已覆盖：陌生人读取返回"无权查看该红包详情"，发送者/领取者读取返回 `{ok, _}`。
- **根因**：`src/api/wallet_handler.erl:240` `_CurrentUid = auth_ds:current_uid(State)` 取了却丢弃，直接 `red_packet_logic:detail(Id)`；`src/logic/red_packet_logic.erl:90 detail(PacketId)` 只按 id 查，无查看者归属校验。任意登录用户可用红包 id 读发送者/祝福语/金额/领取名单。
- **影响范围**：`wallet_handler.erl:240`、`red_packet_logic.erl:90`。
- **修复方案**：`detail/1` → `detail/2` 加 `ViewerUid`；在返回前校验 `ViewerUid` 为发送者或已领取者之一，否则 `{error, no_permission}`。handler 传入 `CurrentUid`（去掉下划线）。
- **边界**：不改红包领取/退款逻辑。
- **验收 gate**：新增 eunit 断言非相关 uid 读 detail 返回 no_permission；`make eunit` 绿。
- **glm 执行陷阱**：同 erlfmt/DCO 常规。
- **分工**：**[MODEL] glm 可独立执行**。
- **回滚条件**：detail 若被其他在线通知链路复用（grep `red_packet_logic:detail` 调用方），确认无破坏再改签名。

### [SEC-04] compliance key 服务端密钥托管破坏 E2EE 语义  [安全] [HIGH — 需产品拍板]
- **根因**：`src/ds/compliance_key_ds.erl:26` 服务端持有加密的合规私钥；`src/logic/e2ee_logic.erl:173` 下发合规公钥供客户端 wrap。若客户端对 compliance 公钥追加 wrap，管理员解密合规私钥即可读全部密文——这是合法监听后门，与"零接触明文私钥"宣称不矛盾（是合规私钥非用户私钥）但**破坏端到端语义**。
- **影响范围**：E2EE 合规声明、隐私政策、白标合规文档。[NEEDS-VERIFY] 客户端 Dart 是否真的向 compliance key wrap（需查 imboyapp e2ee_service.dart 加密路径）。
- **修复方案**：这是**披露而非代码 bug**。（a）先核实客户端是否 wrap；（b）若 wrap，在 `docs/compliance/` 与隐私政策明示"依法留存/合规密钥托管"；（c）若不 wrap，则 compliance key 是死路径，评估移除。
- **边界**：不擅自删除 compliance 机制（可能是等保/合规要求）。
- **验收 gate**：文档明示 + 客户端 wrap 行为核实结论。
- **分工**：**[BLOCKED] 需人工拍板**（合规策略方向，见 D）。

### [SEC-05] 免鉴权路由收紧  [安全] [MEDIUM]
- **根因**：`open/0`（`imboy_router.erl:901-953`）中 `/metrics`、`/user/show`、`/conversation/online` 免鉴权。/metrics 已被 nginx 入口拦截（`deploy/nginx/templates/imboy.conf.template:43-54` 仅允许内网），但 `/user/show`（任意人拉用户资料）、`/conversation/online`（在线状态）仍公开。
- **影响范围**：`imboy_router.erl:911,927`（user/show）、conversation/online 两行。
- **修复方案**：将 `/user/show`、`/conversation/online` 移出 `open/0`，要求 JWT。若客户端登录前确需用户资料预览，收窄返回字段（仅昵称/头像，去手机号/邮箱）。
- **边界**：`/metrics` 已有内网拦截，勿重复处理；login/signup/init 等认证入口保留公开。
- **验收 gate**：`make compile` 绿 + 手工 curl 无 token 访问 `/v1/user/show` 返回 401。
- **glm 执行陷阱**：改 open/0 名单需 `make compile` 验证列表语法。
- **分工**：**需 Fable 确认字段收窄范围**后 glm 执行（产品可见性判断）。

### 维度 2：未完成功能

### [FEAT-01] admin 三组页面契约断裂（前端调用不存在的后端端点）  [未完成] [CRITICAL] [✅ 已完成 2026-07-06]
- **状态**：三个子域均已由其他会话在审计写就后完成，本次核实全部落地且与前端契约对齐，勿重复排期：
  - user-device：commit `8090e8ae`，`/adm/user/devices`+`/adm/user/device/kick` 字段与 `userDevices.ts` 完全对齐（`user_device_logic:page/3` 返回 device_id/device_name/device_type/device_vsn/last_active_at/online）。
  - moderation：commit `c557e4f1`，`/adm/moderation/sensitive-words[/:id][/import]`+`/adm/moderation/review-queue[/:id/moderate]` 路径/方法与 `moderation.ts` 完全对齐。
  - sso：commit `eb3d75d4`，`/adm/sso/config`+`/adm/sso/test` 路径/方法与 `sso.ts` 完全对齐。
- **根因**：commit `0bafd2d` 新增前端页面但后端无对应路由，任何数据请求 404：
  - `SensitiveWordPage`/`ContentReviewQueuePage` → `/moderation/sensitive-words[/import]`、`/moderation/review-queue`（`imboyadmin/src/services/api/moderation.ts:27,37`；后端 `grep moderation imboy_router.erl` → 0）
  - `UserDeviceList` → `/user/devices`、`/user/device/kick[-all]`（`imboyadmin/src/services/api/userDevices.ts:20,33,40`；后端仅 `/adm/user/force_logout`，`user_device_handler.erl` 只挂 `/v1` 未挂 `/adm`）
  - `SSOConfigPage` → `/sso/config`（`imboyadmin/src/services/api/sso.ts`；后端无 handler）
- **影响范围**：3 个 admin 功能域完全不可用。
- **修复方案**（后端补端点，逐域）：
  - **user-device（最易，后端已有客户端实现）**：`user_device_handler.erl` 已有设备查询/踢除逻辑挂在 `/v1`；在 `imboy_router.erl` 的 `/adm/` 段新增 `/adm/user/devices`（list）、`/adm/user/device/kick`、`/adm/user/device/kick-all`，dispatch 到 `adm_user_handler` 或新增 `adm_user_device_handler`，复用现有 device DS。参照 `/adm/user/force_logout`（`imboy_router.erl:670`）的挂法。
  - **moderation（新建）**：需新建 `sensitive_word` 表迁移（`priv/migrations/00000019_*`）+ `adm_moderation_handler.erl` + logic + ds + repo（敏感词 CRUD/import、review_queue 列表）。中等工作量。
  - **sso（新建，工作量最大）**：SSO 配置存储 + OIDC/SAML 对接。属商业化白标能力，见 Phase 3。
- **边界**：不改前端页面（前端已就绪，等后端）。迁移序号先 `ls priv/migrations | tail -1` 核实（当前最新 `00000018`，下一个 `00000019`）。
- **验收 gate**：user-device：admin 页面能拉到设备列表、踢除成功。moderation：敏感词 CRUD 往返。各域 handler 有 eunit。
- **glm 执行陷阱**：迁移序号 8 位连续（非时间戳）；erlang_migrate strict 已开，乱序会 `out_of_order` 报错，序号必须递增。
- **分工**：user-device **[MODEL] glm 可独立执行**（后端逻辑已存在，只挂路由）；moderation **需 Fable 出表结构+接口方案**；sso **[BLOCKED]**（白标架构决策，见 D）。
- **回滚条件**：若产品决定下线这些前端页面而非补后端，则改为在 `App.tsx` 移除对应路由（更省）。

### [FEAT-02] user export_data 端点 501 未实现（GDPR）  [未完成] [MEDIUM]
- **根因**：`src/api/user_handler.erl:339-341` export_data 已注册路由但返回 501。
- **影响范围**：GDPR/数据可携权合规缺口。
- **修复方案**：实现用户数据导出（聚合 user/message/friend/group 数据成 JSON/zip）。中等工作量。
- **分工**：**需 Fable 出导出数据范围方案**（涉及隐私边界）。
- **回滚条件**：若非目标市场合规要求，降优先级到 Phase 3。

### [FEAT-03] E2EESettings 死开关与 UI 脱节  [未完成] [HIGH]
- **根因**：`imboyapp/lib/service/e2ee_settings.dart:24-30` `isEnabled()` 硬编码 `return false`，忽略持久值；`setEnabled()` 仍写 storage。设置页开关拨动对出站加密 no-op（后端 policy 强制时仍走 e2ee，policy 优先）。属开发期 workaround（本地 RSA 私钥漂移导致"无法解密"）。
- **影响范围**：`e2ee_settings.dart`、调用点 `chat_page.dart:425`、`e2ee_service.dart:140`。
- **修复方案**：两选一（需拍板，见 D BLK-05）：（a）修复密钥漂移根因（重装后自动重新协商/拉取设备公钥）后恢复 `isEnabled()` 读持久值；（b）暂时在设置页隐藏该开关，避免名不副实。**短期建议 (b) 止血**：设置页条件隐藏 E2EE toggle，加注释说明由后端 policy 统一控制。
- **边界**：不改后端 policy 强制链路（`imboy_policy.erl` 完整可用）。颜色/间距若涉及 UI 走 AppColors/AppSpacing。
- **验收 gate**：`flutter analyze` 零问题；真机验证开关隐藏后 policy=required 仍加密。
- **glm 执行陷阱**：真机验证（禁模拟器）；改 UI 走 token。
- **分工**：短期 (b) **[MODEL] glm 可执行**；根因修复 (a) **[BLOCKED]**（需真机调试密钥漂移）。

### [FEAT-04] 其余占位/半成品清理  [未完成] [LOW]
- **根因**：`group_album_ds.erl:454` 缩略图 URL 占位（未真生成缩略图）；`e2ee_shard_validator.erl:3` 分片审计 stub；`user_setting_ds.erl:16` 按账号搜索占位；`imboyapp` textStream 死基础设施（4 个 mutator 零调用，无 `TextStreamMessage` 构造）。
- **修复方案**：按优先级补齐或明确标记；**textStream 保持不动**（历史结论：后端不下发，勿单接 UI）。
- **分工**：**[MODEL] glm 可执行**（逐个小项，缩略图生成需 Fable 定方案）。

### 维度 3：架构债

### [ARCH-01] messaging_logic 越界操作 cowboy_req  [架构] [MEDIUM] [✅ 已完成 2026-07-06]
- **状态**：已修复并提交（commit `ecbbce8d`）。8 个函数的签名映射由 glm 直接设计（沿用文件内 `pin/2`/`forward/2` 已有的 handler/logic 分工范式），HTTP 解析/认证检查/响应封装上移至 `msg_handler`，`messaging_logic` 函数改收纯参数并返回 `{ok,_}|{ok,_,_}|{error,_}|{error,_,_}` 语义化 tuple；原 `handle_rest_action/3` 收归 `msg_handler:dispatch_rest_action/3`。`grep cowboy_req src/logic/messaging_logic.erl` 仅命中说明注释；`make`/`make xref` 通过；targeted eunit（messaging_logic_tests 13/13 通过）。同步更新 `test/logic/messaging_logic_tests.erl`、`test/api/msg_handler_tests.erl` 匹配新签名。
- **根因**：`grep -rln cowboy_req src/logic/` → 仅 `src/logic/messaging_logic.erl`。该文件 handle_rest_action/offline/read_stats/history/offline_ack/reaction 全部签名 `cowboy_req:req()` 并直接 `cowboy_req:parse_qs`（`:23,45,102,148,233,274,316,352`），logic 承担了 handler 职责。
- **影响范围**：`messaging_logic.erl` 全文 + `msg_handler`（实际入口）。
- **修复方案**：把 cowboy_req 解析上移到 `msg_handler`，`messaging_logic` 函数改收纯参数（uid/gid/page 等）。逐函数重构。
- **边界**：不改消息业务语义；一次一个函数迁移+编译验证。
- **验收 gate**：`grep cowboy_req src/logic/messaging_logic.erl` → 0；`make compile && make eunit` 绿。
- **分工**：**需 Fable 出逐函数签名映射**（8 个函数的新参数列表）后 glm 执行。
- **回滚条件**：单函数迁移，任一编译失败即回滚该函数。

### [ARCH-02] 11 个后端 + 11 个 Flutter 文件超 800 行  [架构] [MEDIUM]
- **根因**：后端 `find src -name '*.erl' | xargs wc -l | sort -rn`：`adm_channel_handler.erl`(1030)、`adm_admin_handler.erl`(986)、`adm_group_helper.erl`(921)、`websocket_handler.erl`(884) 等（`imboy_pb.erl` 6018 生成豁免）。Flutter：`chat_page.dart`(2212)、`channel_detail_page.dart`(1659)、`message.dart`(1575) 等 11 个（`imboy.pb.dart` 豁免）。函数级 `msg_c2g_logic:handle_group_action/6` 单函数 ~175 行。
- **修复方案**：按职责拆分子模块（如 adm_channel_handler → admin/order/message 三子模块）。渐进式，非阻塞。
- **分工**：**需 Fable 出每个文件的拆分边界**后 glm 执行。属技术债，排 Phase 3。
- **回滚条件**：拆分后编译/测试任一失败即回滚该文件。

### 维度 4：数据完整性

### [DATA-01] 迁移与事务  [数据完整性] [INFO — 健康，无需动作]
- **核实**：迁移 8 位连续序号（`priv/migrations` 至 `00000018`），`imboy_migrate.erl:87` `strict=>true` 乱序检测已开；最近 DROP 均 `DROP CONSTRAINT IF EXISTS`+重建且有 down 脚本，无不可逆破坏性 DDL。资金 repo 全部 `elib_pg:with_tx` + `FOR UPDATE` + 版本号；commit `90bb5621` 已修事务回滚信号被吞，无同类残留。payment webhook 幂等（金额以订单为准、`(subscription_id,period)` 唯一约束防重复入账）。
- **结论**：数据完整性维度整体健康，**无 CRITICAL/HIGH 项**。

### 维度 5：性能与可扩展

### [PERF-01] 群消息扇出 >10000 人静默截断  [性能] [MEDIUM]
- **根因**：`src/repo/group_member_repo.erl:86-87` `list_by_gid/2` 默认 `LIMIT 10000`；`msg_c2g_logic.erl:311-317` 扇出基于 `member_uids`，超万人群成员被静默截断→丢投。撤回/编辑（`:513-518`）对全体成员逐个 `send_next`。
- **影响范围**：超大群消息可靠性。
- **修复方案**：`member_uids` 分页拉取或超阈值告警；online 判定与 send_next 合并一次 syn 查询（现每成员 2× ETS 查）。
- **验收 gate**：eunit 断言超 10000 成员群不截断（或显式告警日志）。
- **分工**：**需 Fable 出分页扇出方案**（涉及投递语义）。属可扩展性升级，Phase 3。

### [PERF-02] mention 等无界列表查询  [性能] [MEDIUM]
- **根因**：`src/repo/mention_repo.erl:96-103` `find_by_uid/2`、`find_by_group_and_uid/3` 无 LIMIT，@提及历史随时间无限增长全量拉取。`user_setting_repo`/`e2ee_social_repo` 类似但数据量小。
- **修复方案**：mention 列表接口加分页参数（page/size，参照 `elib_param:page`）+ repo 层强制 LIMIT。
- **验收 gate**：`grep -A2 SELECT src/repo/mention_repo.erl | grep -i limit` 有命中；eunit 分页断言。
- **分工**：**[MODEL] glm 可独立执行**（分页是既定模式）。

### 维度 6：前后端契约错位

### [CONTRACT-01] OpenAPI 漏 finance/billing/license 全域  [契约] [HIGH]
- **根因**：`imboy/api/openapi.yaml`（1241 行）滞后。抽查：`/adm/group/vote/*` 已同步；`/adm/finance/billing/*`、`/adm/finance/wallets`、`/withdrawals`、`/payment-transactions`、`/recharge-orders`、`/adm/stats/license` **全部未进 yaml**。
- **影响范围**：契约文档、CI redocly lint 门禁、SDK 生成。
- **修复方案**：将 finance/billing/license 全域端点补进 `openapi.yaml`，参照已同步的 group_vote 段格式。用后端 handler 实际 payload 字段（`adm_finance_handler.erl`、`adm_stats_handler.erl`）为准。
- **边界**：servers[] 用 `127.0.0.1:PORT`（redocly no-server-example 会拦 localhost，已知坑）；lint 忽略走 `.redocly.lint-ignore.yaml`。
- **验收 gate**：`redocly lint api/openapi.yaml` 零新增警告。
- **分工**：**[MODEL] glm 可独立执行**（对照 handler 补 yaml，机械活）。

### [CONTRACT-02] admin 消息 payload 二次 JSON.parse 绕过 TSID 保护  [契约] [MEDIUM] [✅ 已完成 2026-07-06]
- **状态**：已修复并提交（commit `98db3f7`，imboyadmin 仓）。[NEEDS-VERIFY] 已核实为真：`imboy/src/logic/msg_s2c_logic.erl:255`（`e2ee_key_changed_ack`）`payload.uid` 以裸 64-bit TSID 整数嵌入。`parsePayload` 改用既有 `safeParseBigIntJson`；补 `messageRenderingHelpers.test.ts` 回归测试覆盖大整数不失真。`tsc --noEmit` 无新增错误；`bun test src/pages/messages/` 33/33 通过。
- **根因**：`imboyadmin/src/pages/messages/messageRenderingHelpers.tsx:8` `JSON.parse(payload)` 二次解析消息 payload 字符串。外层 `safeParseBigIntJson` 正则不进引号内字符串，若 payload 内含裸整数 id（如 from_id）会丢精度。[NEEDS-VERIFY] payload 内是否真含 64-bit id（需查 `adm_message_handler` payload schema）。
- **修复方案**：先核实 payload 字段；若含 id，改用 `safeParseBigIntJson(payload)` 替代裸 `JSON.parse`。
- **验收 gate**：`tsc --noEmit` 零错；含大 id 的 payload 解析后 id 为 string 不失真。
- **分工**：**[MODEL] glm 可执行**（先 verify 再改一行）。

### [CONTRACT-03] admin 权限门控 fail-open  [契约/安全] [HIGH]
- **根因**：`imboyadmin/src/hooks/useAdminPermission.ts:88-97` 注释 `SECURITY(H11): fail-open` —— `/rbac/me` 404 时细粒度权限降级为角色级放行；`rbac.ts:markRbacUnavailable` 把不可用状态写 sessionStorage 对整会话短路。一旦 rbac 端点 404，全会话细粒度权限失效仅剩 role gate。
- **影响范围**：`useAdminPermission.ts`、`rbac.ts`。后端 `/adm/rbac/me` 存在（OK），但前端主动放行是风险。
- **修复方案**：改 fail-open 为 fail-closed（rbac 不可达时拒绝细粒度操作，仅放行明确 role 允许的基础页面），或加显式告警 + 有限重试。**需拍板**（安全性 vs 可用性权衡，见 D）。
- **验收 gate**：模拟 rbac 404，敏感操作被拒。
- **分工**：**[BLOCKED] 需人工拍板**（fail-open 是有意设计，改动影响可用性）。

### [CONTRACT-04] safeParseBigIntJson 16 位阈值偏低 + Flutter TSID int.tryParse 隐患  [契约] [MEDIUM]
- **根因**：（a）admin `src/lib/safeParseBigIntJson.ts:20`（注意在 `src/lib/` 非 `src/utils/`）阈值 16 位（正则 `\d{16,}`），1e15–9.007e15 的安全整数也被强转 string（时间戳毫秒等可能误伤）；（b）Flutter 20 处 `int.tryParse(...id) ?? 0` 绕过 safeParse，web/JS 编译 int 上限 2^53，19 位 TSID 静默截断被 `?? 0` 掩盖。
- **影响范围**：admin 阈值调整；Flutter 20 处调用点。[NEEDS-VERIFY] 是否发布 web 平台（决定 Flutter 项严重度）。
- **修复方案**：（a）admin 阈值提到 17-18 位（TSID 实际 18-19 位）；（b）Flutter 若发 web，id 全程用 string，`int.tryParse` 改 `EntityId`；若仅原生（int 为 64-bit）则安全，仅加注释。
- **分工**：admin **[MODEL] glm 可执行**；Flutter **[BLOCKED]** 先确认 web 是否目标平台。

### 维度 7：可观测性与运维

### [OPS-01] 备份无自动调度 + 告警指标未上报  [运维] [HIGH]
- **根因**：`grep cron/systemd/timer` 全仓，备份仅 `scripts/backup_pg.sh:11` 注释里一行 cron 示例，`docker-compose.prod.yml` 无 backup 服务。告警 `imboy-alerts.yml:481` `IMBoyBackupNotRunning` 依赖 `imboy_backup_last_success_timestamp` 推 Pushgateway，但 `backup_pg.sh` 末尾无推送代码。→ 生产大概率靠手工 crontab，无版本化保证，告警是死的。
- **修复方案**：（a）`docker-compose.prod.yml` 加 cron sidecar（ofelia 或 alpine+crond）跑 `backup_pg.sh`；（b）`backup_pg.sh` 末尾加 `curl` 推 Pushgateway `imboy_backup_last_success_timestamp`。
- **验收 gate**：备份定时执行；Pushgateway 有时间戳；`IMBoyBackupNotRunning` 告警从常红变绿。
- **glm 执行陷阱**：deploy 改动先查 `deploy/README.md`；compose 语法 `docker compose config` 验证。
- **分工**：**[MODEL] glm 可独立执行**（脚本+compose 补全）。

### [OPS-02] 无 TLS 证书到期告警 + 无支付失败率告警  [运维] [HIGH]
- **根因**：`grep cert|ssl|expiry` 于 rules/alertmanager → 0；prometheus.yml 无 blackbox_exporter。certbot 每 12h 续期但静默失败无监控。业务有 Stripe/支付宝/微信但 rules 无 payment 失败率指标。
- **修复方案**：（a）`docker-compose.prod.yml` 加 blackbox_exporter，`imboy-alerts.yml` 加 `probe_ssl_earliest_cert_expiry < 14d warn / 3d crit`；（b）后端暴露 `imboy_payment_*_total{status}` 指标（`metrics_handler` 加计数器），加失败率告警。
- **验收 gate**：blackbox probe 有 cert 到期指标；payment 指标在 `/metrics` 可见。
- **分工**：cert 告警 **[MODEL] glm 可执行**；payment 指标需后端埋点，**需 Fable 定指标 label 规范**。

### [OPS-03] sys.config 重复 kernel 键 + 开发弱口令入仓  [运维] [MEDIUM]
- **根因**：`config/sys.config:395` `{kernel,[...{logger_level,info}]}` 与 `:417` `{kernel,[...{logger_level,all}]}` 重复，加载行为不确定，`all` 会放开 debug。`:174,190` `password => "abc54321"` 开发弱口令入仓（生产靠 IMBOY_* env 覆盖，gitleaks 已豁免，非公网真实凭据但坏味道）。
- **修复方案**：（a）删 `:417` 重复 kernel 段，保留 `:395` `logger_level=info`；（b）`:174,190` 改 `CHANGE_ME` 占位或从 env 读。
- **验收 gate**：`grep -c "{kernel," config/sys.config` → 1；`make run` 本地启动正常。
- **分工**：**[MODEL] glm 可独立执行**。[NEEDS-VERIFY] 删哪个 kernel 段前确认本地启动依赖。

### [OPS-04] full-eunit/dialyze 仍 continue-on-error  [运维] [MEDIUM]
- **根因**：`.github/workflows/backend-ci.yml:97,150` full-eunit 与 dialyze `continue-on-error: true`（基线收集，注释明确"计划改 ratchet 未执行"）。
- **修复方案**：先跑一次 CI 收基线失败/告警数 → 改为 ratchet（只减不增）→ 移除 continue-on-error。
- **分工**：**需 Fable/人工定基线阈值**后 glm 改 workflow。Phase 3。

### 维度 8：商业化阻塞

### [BIZ-01] license 配额已接入（健康，无需动作）  [商业化] [INFO]
- **核实**：`passport_logic:quota_guard/0`（`:546`）调 `imboy_license:check_user_quota`，被 signup 三处调用（`:475,516,97`）。历史记忆"未接 signup"**已过时**。quota_guard 是 fail-open（count 查询异常静默放行），属产品权衡可接受。
- **结论**：license 规模 gate 已闭环，仅 max_nodes gate 待接（低优先）。

### [BIZ-02] 支付真实网关闭环  [商业化] [BLOCKED]
- **根因**：`payment_sign.erl` sandbox 直通、live 复用 `erlang_pay:verify_notify`；`preflight.sh:124` 对 live 模式强校验凭据。真实网关对接 blocked on 商户账号（Stripe/支付宝/微信凭证）。
- **分工**：**[BLOCKED]**（商户账号+凭证，见 D）。

### [BIZ-03] SSO/白标能力  [商业化] [BLOCKED]
- **根因**：FEAT-01(sso) 的管理端配置契约（`/adm/sso/config`/`/adm/sso/test`）已完成（commit `eb3d75d4`），但仅为 MVP：存储 provider 配置 + 字段/连通性校验，**不含真实 OIDC/SAML/LDAP 联邦登录流程**。SSO 是 ToB 私有化白标的关键能力，真实登录联邦仍缺后端实现。
- **分工**：**[BLOCKED]**（白标架构方向需拍板，见 D）。

---

## C. 升级路线图（三阶段）

### Phase 1 — 紧急止血（1-2 天，CRITICAL + 安全 + 丢数据）

| 任务 | 依赖 | 估时 | 分工 | 验收 |
|---|---|---|---|---|
| ~~SEC-02 group_* IDOR 修复~~ **✅ 已完成**（commit `5cb86897`，勿重复执行） | — | — | — | 已提交 |
| ~~SEC-03 red_packet_detail 越权读~~ **✅ 已完成**（commit `0d80be05`，勿重复执行） | — | — | — | 已提交 |
| SEC-05 收紧 /user/show /conversation/online | Fable 定字段收窄 | 1h | Fable+glm | curl 无 token 401 |
| ~~FEAT-01(user-device) 后端补 /adm/user/device* 路由~~ **✅ 已完成**（commit `8090e8ae`） | — | — | — | 已提交 |
| SEC-01 billing 管理端路由迁移（低风险子集） | 无 | 2h | [MODEL] glm | plan_create 走 adm middleware 被非 admin 拒 |
| OPS-01 备份自动调度+Pushgateway 上报 | 无 | 2h | [MODEL] glm | 告警变绿 |

> **SEC-01 租户端 IDOR 部分**依赖 tenant↔uid 归属模型（BLK-04），拍板后进 Phase 1 尾或 Phase 2 头。

### Phase 2 — 功能完善（3-4 天，HIGH + 契约对齐 + 未完成功能）

| 任务 | 依赖 | 估时 | 分工 | 验收 |
|---|---|---|---|---|
| SEC-01 billing 租户端 IDOR 完整修复 | BLK-04 拍板 | 4h | Fable+glm | eunit 越权断言 |
| ~~FEAT-01(moderation) 后端敏感词+审核队列~~ **✅ 已完成**（commit `c557e4f1`） | — | — | — | 已提交 |
| FEAT-03(b) E2EE 死开关短期隐藏 | 无 | 1h | [MODEL] glm | 真机 policy 仍加密 |
| CONTRACT-01 OpenAPI 补 finance/billing/license | 无 | 3h | [MODEL] glm | redocly lint 零警告 |
| ~~CONTRACT-02 admin payload 二次 parse 修复~~ **✅ 已完成**（commit `98db3f7`） | — | — | — | 已提交 |
| CONTRACT-04(admin) safeParseBigIntJson 阈值 | 无 | 0.5h | [MODEL] glm | tsc 绿 |
| ~~ARCH-01 messaging_logic 越界重构~~ **✅ 已完成**（commit `ecbbce8d`） | — | — | — | 已提交 |
| PERF-02 mention 无界查询加分页 | 无 | 2h | [MODEL] glm | eunit 分页断言 |
| OPS-02 cert 到期告警 | 无 | 2h | [MODEL] glm | blackbox 有 cert 指标 |
| OPS-03 sys.config 重复键+弱口令 | verify 启动 | 1h | [MODEL] glm | make run 正常 |
| FEAT-02 export_data GDPR 实现 | Fable 定范围 | 4h | Fable+glm | 导出 JSON 往返 |

### Phase 3 — 商业化升级（窗口外持续）

| 任务 | 依赖 | 分工 |
|---|---|---|
| SEC-04 compliance key E2EE 语义披露 | BLK 合规拍板 | 文档 |
| BIZ-03 白标 SSO（真实 OIDC/SAML 联邦登录，超出 FEAT-01(sso) 已完成的管理端配置契约） | BLK 白标方向 | Fable 设计 |
| BIZ-02 真实支付网关对接 | BLK 商户账号 | glm（凭证到位后） |
| CONTRACT-03 admin 权限 fail-open→fail-closed | BLK 可用性拍板 | glm |
| PERF-01 群扇出 >10000 分页 | Fable 方案 | Fable+glm |
| ARCH-02 超 800 行文件拆分（后端 11+Flutter 11） | Fable 拆分边界 | Fable+glm |
| OPS-02(payment 指标)/OPS-04(CI ratchet)/BIZ-01(max_nodes gate) | 部分需拍板 | 混合 |
| Flutter：194 处 Colors 硬编码 token 化 + 测试套件 8 编译错修复 + r_upgrade 越界还原(见 D) | — | glm（真机批次） |

**阶段间依赖**：Phase 1 的 SEC-01 租户端、Phase 2 多数 HIGH 项依赖 D 中拍板项解锁。契约对齐（CONTRACT-01/02/04）与运维（OPS-*）无外部依赖，可并行插入任意阶段。

---

## D. 风险与外部阻塞（单列，不排进执行日程）

### 需人工拍板的架构抉择

- **BLK-04 tenant↔uid 归属模型**：billing 租户端 IDOR 修复（SEC-01）需明确"一个 tenant 由哪些 uid 拥有/管理"。当前 billing_logic 把 tenant_id 当客户端自报字段，无归属表。**决策项**：tenant 是否等于群/组织？uid 与 tenant 的多对多关系表如何设计？——这是 Phase 1/2 的解锁前提。
- **BLK-05 E2EE 换设备恢复协议方向**：现有三链路（设备迁移/社交分片/本地备份文件），但**无 Garage S3/Matrix-4S 风格服务端加密密钥自动备份**——换机且无第二在线设备+无可信联系人+无本地文件 → 历史密文不可恢复（`e2ee_recovery_logic.erl:138`）。**决策项**：是否补服务端加密密钥备份（用户 recovery key 加密后存 S3）？这决定 FEAT-03(a) 死开关根因能否根治。
- **BLK-06 compliance key 合规定位**：SEC-04 —— compliance key 是等保/合规监听要求还是死路径？影响是否披露/移除。
- **BLK-07 admin 权限 fail-open**：CONTRACT-03 —— 安全性（fail-closed）vs 可用性（fail-open）权衡，需产品拍板。
- **BLK-08 白标 SSO 方向**：BIZ-03 —— 支持 OIDC/SAML 哪种？自建 IdP 还是对接企业 IdP？决定 sso 后端实现范围。

### 真机时间（Flutter，禁模拟器）

- FEAT-03 E2EE 开关真机验证；face_to_face 三件套 UI（含新引入硬编码+未 i18n 中文，`face_to_face_confirm_page.dart:206,253`，提交前须补 token+i18n）；6 个 maestro yaml 依赖华为真机；android build.gradle.kts packaging 改动（删了 `pickFirsts META-INF/*`）需真机打包确认高德重复类不复发。

### 第三方凭证 / 商户账号

- BIZ-02 真实支付网关：Stripe/支付宝/微信商户账号+API 凭证（blocked）。
- 企业提现 API：blocked on 商户账号。

### 禁改区越界（需先还原，非新功能）

- **imboyapp/plugin/r_upgrade 被越界改**（`UpgradeManager.java:112` 加 RECEIVER_NOT_EXPORTED）：r_upgrade 是明令禁改区，且无 `.gitmodules`，改动无法被主仓追踪只以 dirty 指针泄漏。**必须还原**，Android 13 receiver 合规改用上游 fork 或 gradle 层规避。
- `plugin/amap_flutter_location_plus` 同类越界（`AMapFlutterLocationPlugin.m:162` 且 language 行为收窄），需还原。

### 并发 git 状态（工作纪律）

- 巡检期间 `stash@{0}` 内容被并发会话改动（从"WIP on main"变为"pre-dedup-stash"）。任何 git 写操作前重新 `git stash list && git log --oneline -3` 确认，勿盲信栈序号，勿擅动他会话改动。
