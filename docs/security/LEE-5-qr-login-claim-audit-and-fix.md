# LEE-5：QR 登录 LoginToken 非原子领取——安全审计与修复报告

> 状态：**已实施（本地），待属主逐仓授权提交**
> 日期：2026-11-14 | 影响仓：`imboy`（后端主仓）| 不涉及数据库迁移变更、Flutter、Redis/分布式锁
> 关联文档：`docs/architecture/cache-v2.2.1/g-qr-atomic-consumption-proposal.md`（G-QR 原提案，本次落地）

---

## 0. 最终验收口径

> **任何 SSE、Polling、Cache 通知、Erlang 节点切换、网络故障和重复请求，都不能使同一个 QR Session 的 LoginToken 成功领取两次。**

实现不变式：LoginToken 的**唯一交付通道**是 `qr_login_session_repo:claim_login_token/1` 的单语句
`UPDATE ... SET login_token=NULL, consumed_at=now() WHERE ... RETURNING OLD.login_token, scanned_by`
（PostgreSQL 行级锁仲裁，全集群恰好一个消费者）；0 行返回 / PG 故障 / PG18 以下 → fail closed，
**绝不降级缓存**。Token 与 scanned_by 恒来自同一次领取结果。

---

## 1. 安全审计报告（修复前）

### 1.1 领取路径汇总（修复前）

| 入口 | 中间层 | 最终授权源 | 原子？ | 双重领取竞态窗口 |
|---|---|---|---|---|
| `GET /status`（**cache 权威=当时默认**）`cache_handle_status` | handler 直调 `imboy_cache`（depcache） | 本地 ETS | **否** | ① 并发轮询 TOCTOU：`get_session`→读 token→`delete_session` 三步两次独立 ETS 操作，并发请求同读 confirmed 会话各取 token 后互删；② 删除失败/崩溃则会话存活至 60s TTL，期间任何持有 session_token 者可再领 |
| `GET /status`（pg 权威，当时需显式配置且**无任何生产配置机制**） | `qr_login_session_ds:claim_login_token` → repo | PostgreSQL | 是 | 无（当时不可达） |
| SSE `/subscribe`（cache 默认）`info` confirmed 分支 | confirm 时 `cache_handle_confirm` → `qr_login_event_ds:notify` → syn 广播 | cache 会话 map 快照 | **否（广播零消费语义）** | 同一 session_token 的 N 个 SSE 连接**全部**收到含 token 事件；且会话仍在 cache，轮询者还能再领一次 |
| SSE `/subscribe`（pg）confirmed_ready 分支 | SSE 服务端进程 `claim_login_token` | PostgreSQL（与轮询同一 UPDATE） | 是 | 恰一胜者；败者 `: claimed_elsewhere` 关连接 |

### 1.2 缺陷裁定

- **LEE-5 指控成立，但定位于 cache 权威路径**——而 `qr_login_authority` 当时默认 `cache`
  （`application:get_env(imboy, qr_login_authority, cache)`），且 config/、deploy/（compose env 白名单、
  helm）**没有任何机制可设置 pg**：生产实际执行的就是非原子路径。
- PG 原子领取实现当时**已存在且正确**（含 repo 级 100 轮×4 路并发测试与双节点脚本），
  但迁移 `00000170_qr_login_session` 未在生产 apply（up.sql 注释自认"生产启用属部署决策"）。
- 旁路事实：`/subscribe` 与 `/cancel` 免鉴权（session_token 即持有者秘密，32B 随机）；
  含 token 会话值曾入 cache（dsync 对 `{qr_login,_}` 键已 blocked 不广播——止血非修复）。

### 1.3 根因分类

1. **一次性消费无原子原语**：depcache 无 take/get_and_delete，get+delete 是 TOCTOU。
2. **通知通道与消费通道分离**：SSE 广播含凭据、无消费语义，与轮询互不知晓。
3. **权威默认倒置**：安全的 PG 路径为 opt-in，不安全 cache 路径为默认。

## 2. 修复方案对比与最终选择

| 方案 | 要点 | 结论 |
|---|---|---|
| **A. PG 唯一权威收敛（选定）** | 默认 `qr_login_authority=pg`；删除全部 cache_* 分支；SSE/轮询统一 `claim_login_token`；cache 与凭据彻底解耦；PG 故障/版本不足 fail closed | ✅ 属主批准（2026-11-14）。复用既有生产级实现，零新基础设施 |
| B. cache 层自建原子领取 | 在 depcache 上实现 take/代次锁 | ❌ 只缩小不消除竞态窗口；跨节点仍破；等于自造分布式锁 |
| C. Redis/分布式锁 | 引入外部组件 | ❌ 违反执行边界（不新增 Redis/锁） |
| D. 保留 cache 显式模式完整行为 | 兼容最大 | ❌ 不满足验收口径（同一 token 仍可多次领取），属主明确否决 |

**属主批复关键取舍**：*宁可数据库暂时不可用时 QR 登录失败，也不能为了可用性退回非原子的 Cache 领取。*

## 3. 实际代码变更（本仓，未提交）

| 文件 | 变更 |
|---|---|
| `src/api/qr_login_handler.erl` | ① `authority/0` 默认 `pg`，仅接受 `pg\|postgres`，其他值（含显式 cache）节点级一次 WARN 后按 pg——配置无法削弱安全；② **删除全部 cache_* 分支与助手**（cache 五个 handler 分支、cache_session/get_session/delete_session、cache 版 generate_qr_token/parse_qr_token，约 −250 行）；③ `pg_create_session` 返回 `{ok,QRToken}\|{error,Reason}`，入口过 PG18 门禁；create 任何失败 → 503 `ERR_SERVICE_UNAVAILABLE` fail closed（不再 badmatch 崩溃）；④ status 的 claim 门禁失败 `{claim_gate,_}` → 503 明确拒绝 |
| `src/api/qr_login_sse_handler.erl` | `confirmed` 与 `confirmed_ready` 两分支**统一合并**：一律服务端 `claim_login_token` 代领；**事件内嵌 token/uid 永不直接交付**（含滚动升级期旧节点广播的 legacy 含 token 事件）；领取失败 → `: claimed_elsewhere` 关连接；门禁失败 → `: service_unavailable` 关连接 |
| `src/repo/qr_login_session_repo.erl` | ① 新增 `ensure_claim_compatible/0`：查 `server_version_num`（persistent_term 节点级记忆，**仅成功取得版本才缓存**；查询失败返回错误不缓存），`<180000` → `{error,{pg_version_unsupported,N}}`；② **create/claim 入口强制过闸**（repo 层兜底，任何调用方无法绕过）；③ claim 门禁失败返回可区分 `{error,{claim_gate,_}}`；④ claim/transition 的 PG 执行错误补 WARN 留痕（与"已消费"可区分，返回仍 fail closed）；⑤ `delete_expired/1` 批量限量版（LIMIT 子查询，经 `elib_pg:execute` 取影响行数） |
| `src/ds/qr_login_session_ds.erl` | 薄转发 `ensure_claim_compatible/0`、`delete_expired/0,1`（分层门保持：API 禁直调 Repo） |
| `src/lib/qr_login_cleanup_worker.erl`（新） | 过期会话行周期清扫（镜像 `olm_otk_cleanup_worker` 规范）：默认启用、间隔 5min、单批 ≤500、仅删 `expires_at <= now()`（PG 侧时钟）、失败仅 WARN、独立进程崩溃不影响 QR 登录、`{local,?MODULE}` 注册名防重复启动 |
| `src/imboy_sup.erl` | 挂载 `QrLoginCleanupWorker` 子规格（permanent worker） |
| `src/ds/qr_login_event_ds.erl` | 仅注释修正（移除指向已删 cache 键的陈旧描述） |
| `config/sys.config.example` / `config/sys.pro.config` / `config/sys.local.config` | 各加一行 `{qr_login_authority, pg}`（显式化部署决策 + 注释说明门禁/迁移前提） |
| `deploy/README.md` | 前置条件新增「PostgreSQL ≥ 18 为 QR 登录硬性要求」条目（含扩展预装清单） |

**REST/SSE 合同保持不变**：confirmed 响应/事件载荷（status/token/uid/uid_str 信封）、错误码
（5203 ALREADY_USED、404 NOT_FOUND、INVALID_QR_TOKEN、EXPIRED、CANCELLED、FORBIDDEN、NOT_SCANNED）
全部原样；新增仅 PG 故障/门禁失败的业务码 503（HTTP 200 信封 `ERR_SERVICE_UNAVAILABLE`，
与全站 5200 系错误合同同构——**监控告警须挂信封业务码，不是 HTTP 5xx**）与 SSE
`: service_unavailable` 注释帧（fail closed 显式拒绝）。

## 4. PostgreSQL 版本兼容性结论

- **正式最低版本：PostgreSQL 18**（与既有部署事实一致：compose 专用镜像
  `ghcr.io/imboy-pub/imboy-pg18`、`.env.example` 与 deploy 文档均明确 PG18；Helm 为自带数据库路径）。
- 依据：claim 的 `UPDATE ... RETURNING OLD.login_token` 依赖 PG18 起的 RETURNING OLD 能力
  （PG 官方 18 版文档确认）。**不采用**兼容旧版的 CTE/DELETE 重写（会改变"已消费"错误语义或
  残留 token 值，收益为负）。
- **运行时门禁覆盖所有领取/创建入口**（repo 层，见 §3）：`create/1` 与 `claim_login_token/1`
  入口过闸；SSE 直接领取入口与 Polling 领取入口都经由 claim，无法绕过；`RETURNING OLD` 仅 claim
  使用。首次查询后 persistent_term 记忆，同一请求链零重复查询。
- 门禁行为（mock 测试证明，见 §7）：PG18（180000+）通过；PG17（170004）明确拒绝且 claim 返回
  `{claim_gate,_}`；PG 查询失败返回明确错误**且不缓存**（恢复后重查）；已缓存后不再发版本查询。
- 不自动升级用户数据库（执行边界）：低版本库上 QR 登录 fail closed（业务码 503，HTTP 200 信封），
  其余功能不受影响。门禁版本结果按节点缓存（persistent_term）：**库原地大版本升级（如 17→18）
  后须滚动重启后端节点**，门禁不随库升级自动刷新（fail-closed 方向，非安全风险）。

## 5. 新旧节点切换方案（混合部署安全）

**威胁模型**：旧版本节点仍运行非原子 cache 领取代码时，重复领取缺口仍在——新节点拒绝旧 cache
会话**不足以**闭合缺口；**不得**把 QR Session 60 秒有效期当作混布安全保证。

**切换规程（蓝绿/维护窗口，发布门禁清单）**：

1. 前置：目标库 `server_version_num >= 180000`；迁移 `00000170_qr_login_session` 已 apply
   （社区 compose `auto_migrate=true` 随新镜像启动自动应用；蓝绿 `sys.pro.config`
   `auto_migrate=false` 须先 `scripts/imboy_ctl db migrate` 再切流）。
2. **维护窗口（推荐）**：停旧版本全部 backend → 起新版本 → 验证 `/healthz` 与一次真实扫码登录。
3. **蓝绿**：新版本起齐并验证 → **一次性切流**（避免新旧同时在线服务 QR）→ 旧版本全部下线。
4. 切换核验（逐节点）：版本号一致（镜像 tag）；`qr_login_authority` 配置为 pg/缺省；
   进程与请求路由确认旧节点不再承接 `/qr_login/*`（负载均衡摘除 + 连接耗尽）。
5. **宣布 LEE-5 修复生效的唯一时点：全部节点切换完成并核验通过之后**。混布期间发生过的一切
   QR 会话一律视为潜在暴露（60s 生命期只是影响面缩减，不是安全边界）。
6. 回滚：重新部署旧版本镜像即可（旧代码默认 cache，`qr_login_session` 表被忽略不冲突）；
   注意回滚**重新引入** LEE-5 竞态，仅作紧急手段，回滚窗口内避免宣传 QR 登录安全性。

## 6. 过期数据清理实现（LEE-5 补充 3）

- `qr_login_cleanup_worker`（gen_server + send_after 周期，规范对齐 `olm_otk_cleanup_worker`）：
  默认启用（QR 行 60s 短生命周期、无审计价值），间隔默认 5 分钟（启动延迟 60s 等 DB/迁移就绪），
  批量/间隔/开关可经 `qr_login_cleanup_batch / qr_login_cleanup_interval_ms / qr_login_cleanup_enabled` 覆盖。
- **只删 `expires_at <= now()`**（PG 侧时钟，与 claim 的 `expires_at > now()` 谓词按时间轴严格分区）；
  **单批 ≤500**（`DELETE ... WHERE session_token IN (SELECT ... ORDER BY expires_at ASC LIMIT $1)`，
  对齐 `msg_c2c_repo:delete_expired/1` 惯例，计数经 `elib_pg:execute`）。
- **过期安全从不依赖清扫**：claim/状态机全部语句内联 `expires_at` 谓词（真实 PG 竞争测试证明
  清扫与 claim 任一交错都不交付过期凭据）。
- 清理失败只 WARN（不 crash，handle_info 重排下一轮）；worker 独立进程，崩溃不影响 QR 登录
  （不在 readiness 必需清单）；`{local, ?MODULE}` 注册名使 supervisor 无法重复启动
  （二次 `start_link` 即 `{error,{already_started,_}}`，单测覆盖）。
- 落地动因：cache 删除后 PG 表为 QR 会话唯一存储，无清扫则必然膨胀（原 `delete_expired/0`
  无生产调用方且经 `elib_pg:query` 执行时计数恒 0——已修正为 execute）。

## 7. 测试结果（本地，2026-11-14）

**测试分类纪律**：真实 PG 集成 / Mock / Worker 单元 / 静态检查 四类显式标注；
Mock 仅证接线与 fail-closed，**不冒充跨节点或原子性证明**。
真实 PG 环境：本机 Docker `imboy_pg18`（server_version_num=180001），专属临时库
`imboy_cache_closeout_v1`（预装 timescaledb/postgis/pg_jieba/pgcrypto/pg_trgm 扩展，
迁移由测试启动链自动 apply；**不含任何真实用户数据**）。

### 7.1 Mock 测试（meck；证 handler/SSE 接线、合同、门禁语义）

| 套件 | 结果 | 覆盖 |
|---|---|---|
| `test/api/qr_login_handler_tests.erl`（28 用例，cache 路径全面改写为 pg） | **All 28 passed** | create 五分支（含 PG18 门禁拒绝 503、PG 不可用/迁移缺失 503 不崩溃）；status：confirmed 走 claim、**领取成功响应丢失重试→5203 不重发**、scanned 不领取、404 fail closed、claim_gate→503；scan/confirm/cancel 全错误码；confirm 广播 **confirmed_ready 无凭据**（maps:size==1 断言）；默认 authority=pg；**显式 cache 覆盖+节点级告警+imboy_cache 零触碰（throw 反证）**；postgres 别名 |
| `test/api/qr_login_sse_handler_tests.erl`（22 用例，含 3 个新用例） | **All 22 passed** | **legacy 含 token confirmed 事件：交付 claim 结果 token（事件内嵌值断言 nomatch）**；claim 失败→claimed_elsewhere；claim_gate→`: service_unavailable`；既有 init/心跳/terminate/confirmed_ready 用例保持 |
| `test/api/qr_login_uid_contract_tests.erl`（15 用例，cache 用例改写） | **All 15 passed** | 显式 cache 下 confirm 走 PG 身份+广播 confirmed_ready+cache 零写入；显式 cache 轮询经 claim 顺序消费；**claim 结果身份非法绝不发放凭据**（轮询 5203/SSE invalid_credentials）；legacy token-only 事件→统一 claim→claimed_elsewhere |
| repo 测试内 MOCK 门禁组（`pg18_gate_semantics_mock_test_`） | **随 repo 套件通过** | PG18 通过且记忆（num_calls==1）；PG17 create/claim 双入口拒绝；**查询失败不缓存**（恢复后重查成功再记忆） |

### 7.2 真实 PostgreSQL 集成（`test/repo/qr_login_session_repo_tests.erl` + 验收模块）

**`qr_login_session_repo_tests`：All 19 tests passed**（含既有 + 全部新增）。

| 用例组 | 结果 | 断言的最终状态/裁决规则 |
|---|---|---|
| 既有：roundtrip / 100 轮×4 路并发 claim / SSE×status 2 路 / 并发 scan / 重复 confirm / 过期全拒 | ✅ | 恰一赢家、无 token 残留 |
| **新增：单轮 100 路并发 claim** | ✅ | 恰 1 赢家 + 99 not_claimable；`token_claimable=false`；二次领取拒绝 |
| **竞争 1：confirm vs cancel（最终态裁决版）** | ✅ | 恰一成功：confirmed（token 恰领取一次，cancel 不可逆）xor cancelled（token 永不可领取） |
| **竞争 2：confirm vs expire** | ✅ | 双方对 `status='scanned'` CAS 恰一赢：confirm 赢→confirmed+未过期+领取一次；expire 赢→confirm 拒绝、行过期、claim 永远拒绝 |
| **竞争 3：claim vs cancel + 负向状态** | ✅ | confirmed 上 cancel 必败、claim 恰一次；**已取消会话 claim 拒绝；已过期（含过期前已 confirm）会话 claim 拒绝** |
| **竞争 4：scan vs cancel** | ✅ | 非互斥 CAS（cancel 前置集 waiting\|scanned 是 scan 后状态超集，"扫码后取消"合法）：cancel 恒成功、终态恒 cancelled、scanned_by 至多记录一次扫码赢家；**已取消会话绝不发放凭据**（实施中发现并修正了我方最初的假 XOR 断言——真不变式是终态+不可领取，非互斥性） |
| 竞争 5：重复 confirm（既有强化） | ✅ | 首 CAS 成功、二次拒绝；token 保持至 claim 恰一次 |
| **竞争 6：claim vs expire** | ✅ | 双方对 (confirmed,未消费) CAS 恰一赢；**任何交错绝不交付过期凭据**；领取窗口随后关闭 |
| **cleanup vs claim（活跃行/过期行）** | ✅ | 活跃行：清扫 0 行、claim 恰一次；过期行：claim 不论交错一律拒绝 |
| **delete_expired 只删过期 + 批量上限** | ✅ | 3 过期+2 活跃：LIMIT 2 删 2（按 expires_at ASC），活跃保留；放宽清完剩余过期；再跑 0 |
| **验收模块 `test/api/qr_login_claim_acceptance_tests.erl`（LEE5_ACCEPTANCE=1）** | **All 5 tests passed** | 迁移缺失门禁；**100 并发 Polling**（真 handle_request+真 elib_response：恰一 200+token / 99×5203 / 重试 5203）；**100 并发 SSE**（真 info(confirmed_ready)：恰一 token 帧 / 99×claimed_elsewhere）；**50+50 混合恰一**；**真实 epgsql 死连接注入**（claim/轮询/SSE 三入口 fail closed，恢复后同会话恰领取一次） |

### 7.4 汇总运行结果（全部为 2026-11-14 本机实测）

- `make compile`：0 错误 0 警告（qr 七模块）。
- `make dialyze`：exit 0，0 警告。
- `qr_login_handler_tests`：**All 28 passed**（Mock）。
- `qr_login_sse_handler_tests`：**All 22 passed**（Mock）。
- `qr_login_uid_contract_tests`：**All 15 passed**（Mock 契约）。
- `qr_login_cleanup_worker_tests`：**All 6 passed**（Worker 单元）。
- `qr_login_session_repo_tests`：**All 19 passed**（真实 PG，closeout 库 @4323，PG 18.1/180001）。
- `qr_login_claim_acceptance_tests`：**All 5 passed**（真实 PG 验收模式 LEE5_ACCEPTANCE=1）。
- 静态检查（§7.6）：全部通过。

### 7.5 双节点 dcheck 结果（2026-11-14 实测）

`erl -pa … -eval 'cache_v221_qr_entry_dcheck:main(["/tmp/lee5-dcheck.json"]),halt().'` → **PASS**：

- `C08_DEDICATED_QR_SCHEMA PASS existing_table=true`
- `C08_TWO_BACKEND_ENTRIES PASS cases=3 failed=0 skipped=0 credentials_per_case=1`
- `C08_FIXTURE_CLEANUP PASS rows_remaining=0`
- 报告 JSON（`/tmp/lee5-dcheck.json`）：`status=PASS, authority=real PG`，
  双节点 `qra…@127.0.0.1` / `qrb…@127.0.0.1`（peer 真实双 Erlang 节点，各自独立 epgsql
  连接同一 closeout 库），三场景（status×status / sse×status / sse×sse）每场景
  `synchronized_pg_waiters=2`（两入口同时阻塞在同一行锁——真并发证明），
  `credentials=1`（双节点合计恰一凭据），败者 5203 / claimed_elsewhere 兼容，fixture 清理零残留。

### 7.6 静态检查（调用链安全复扫）

| 检查 | 结果 |
|---|---|
| QR 七模块 `imboy_cache` 引用 | **0**（唯一残留为 qr_login_event_ds 陈旧注释，已修正） |
| `login_token` 全 src 引用点 | 仅 handler claim 分支/confirm 原子写入/文档注释 |
| `token_ds:encrypt_token/encrypt_seat_token` 的 QR 调用点 | **唯一**：`qr_login_handler:generate_login_token/3`（仅 confirm 原子写入时调用；交付一律经 claim） |
| `qr_login_event_ds:confirmed/2` 调用点 | 仅 `confirmed_response`（claim 结果）与 `push_confirmed`（claim 结果）——凭据构造只出自领取结果 |
| confirm 广播事件形状 | `event(confirmed_ready)` = `#{status}` 单字段，**无 token/uid** |
| cache_* 残留（cache_handle_/cache_session/get_session/delete_session/parse_qr_token_by_authority） | **0** |
| `{qr_login,_}`/`{qr_login_qr,_}` dsync 广播 | registry `blocked,true` 保持（防回归；pg 模式不写 cache，键根本不产生） |

## 8. 配置迁移说明

- 代码默认已翻转：`qr_login_authority` 缺省 = `pg`（接受 `pg|postgres`；其他值 WARN+按 pg）。
- 三个 sys 配置已显式写入 `{qr_login_authority, pg}`（example=生产镜像模板、pro=蓝绿、local=开发）。
- **部署前提**（见 §5）：迁移 `00000170` 已 apply + `server_version_num >= 180000`。
  社区 Docker（auto_migrate=true）随镜像启动自动迁移；蓝绿须先 `imboy_ctl db migrate`。
- 无 env/compose/helm 白名单键（现机制无法用环境变量设置该键；如需运维开关属后续独立决策）。
- 本地/测试库预置：新库需 `CREATE EXTENSION timescaledb, postgis, pg_jieba, pgcrypto, pg_trgm`
  （迁移假设扩展已预装；本次 closeout 临时库即按此预置）。

## 9. 部署与回滚

部署：见 §5 切换规程（维护窗口/蓝绿 + 逐节点核验 + 全量切换后才宣布生效）。
回滚：旧镜像重部署即回 cache 行为（表共存无冲突）；回滚重新引入 LEE-5 竞态，仅紧急使用。
监控点：`[lee-5]` 前缀日志——authority 覆盖告警、create/claim 门禁拒绝（ERROR）、claim/transition
PG 故障 WARN（与"已消费"可区分）、cleanup 失败 WARN。

## 10. 未通过及未执行的验收项 / 未解决风险

**未通过项：无。** 全部套件（7 项）+ 验收模式（5/5）+ 双节点 dcheck（3/3 场景 PASS）
+ dialyzer + 静态检查均通过；未执行项：无（真实 PG 验收模式已严格执行，未因环境不可用降级）。

残余风险与后续议题：

1. 混布期旧节点含 token 广播的防线是**结构性**的（新节点统一 claim、不信任事件内嵌凭据），
   未做真实新旧双版本端到端演练（需旧版本节点环境）；残余风险：旧节点自身的 cache 领取缺口，
   由 §5 规程（旧节点全部退出）闭合。
2. `delete_expired` 扫描频率 5min/批 500 为工程默认：极端 create 速率（>100/s 持续）下表仍可能
   短时增长，靠部分索引 + 多轮清扫收敛；未做容量压测。
3. claim 使 confirmed 态每次轮询多一次 UPDATE（既有 pg 路径行为），未做性能基准对比。
4. `/subscribe`、`/cancel` 免鉴权（session_token 即秘密）为既有设计，本次未改（记录为后续议题）。
5. Flutter 侧为静态核验（SSE 降级轮询/5203 停止/无 token 不写登录态均 fail-closed 兼容），
   未真机回归——服务端合同未变，预期零影响。
6. 实施期运维注记：eunit-local 首次对全新 closeout 库跑时，若库未预装 timescaledb 等扩展，
   迁移会以 `create_hypertable undefined` 失败并进入 migration_dirty 重启循环（本次已按
   §8 预建扩展解决）；中止 eunit 进程前先确认无 in-flight 迁移，否则需 DROP/重建临时库去污。
