# 回滚手册 rollback-v1（CACHE_V221_BACKEND_CLOSEOUT_V1）

> 适用范围：本次收口交付的 4 个本地提交（均未 push，已核实无任何远程分支包含）。
> 红线：本手册只描述回滚步骤，不含任何已执行的动作。执行者须对目标环境有写权限并自行确认。

## 0. 交付面事实核对（编写时以 git 只读取证）

| 提交 | 主题 | 文件面（git show --stat 取证） | 是否含迁移 | 是否含 deploy/config |
|---|---|---|---|---|
| `675b0744` | feat(qr): G-QR 原子消费（C08） | `priv/migrations/00000170_qr_login_session.{up,down}.sql`、`src/api/qr_login_handler.erl`（642 行）、`src/api/qr_login_sse_handler.erl`、`src/ds/qr_login_event_ds.erl`、`src/repo/qr_login_session_repo.erl`（新，197 行）、`test/repo/qr_login_session_repo_tests.erl`（新，274 行） | 是（00000170） | 否 |
| `e001fb8e` | fix(account): G-01 原子 ID 分配 | `src/ds/account_ds.erl`（-45）、`src/ds/imboy_account_allocator.erl`（新，138 行）、`test/ds/account_ds_tests.erl` | 否 | 否（allocator 无 supervision/app.src 改动，仅被 account_ds 与测试引用） |
| `7db4e0af` | feat(cache): C02-C05 同步协议收口 | `docs/.../protocol-budget-frozen.md`、`src/lib/imboy_cache_policy.erl`、`src/lib/imboy_cache_sync.erl`、`src/lib/imboy_cache_sync_proto.erl`（新）、`src/lib/imboy_cache_sync_snapshot.erl`（新）、`test/lib/imboy_cache_sync_tests.erl` | 否 | 否 |
| `6217ec8e` | docs(cache): C00/C01 基线与冻结契约 | 仅 `docs/architecture/cache-v2.2.1/closeout-v1/` 下 7 个 json/md | 否 | 否 |

补充事实（影响回滚设计）：

- 三个代码提交的文件面**零重叠**（revert 互不冲突，任意顺序均可干净执行）。
- 4 个提交未出现在任何远程分支（`git branch -r --contains` 对 `6217ec8e`/`675b0744` 均为空），可安全 revert。
- `6217ec8e` 与 `7db4e0af` 之间在 main 历史上还隔着 5 个无关 docs 提交
  （`736cc67f`…`a7783438`），全量回滚**不需要**碰它们。
- **工作树前置条件**：当前工作树有未提交改动，其中
  `src/lib/imboy_cache_sync.erl`、`src/lib/imboy_cache_sync_proto.erl`、
  `test/lib/imboy_cache_sync_tests.erl`、`docs/.../protocol-budget-frozen.md`
  与 `7db4e0af` 的文件面重叠。**任何 revert 7db4e0af 之前必须先把这些
  改动提交（协调者计划内）或 stash**，否则 git revert 会以"本地修改将被
  覆盖"拒绝执行。
- 迁移现状：`priv/migrations/` 当前最高版本即 00000170，且该迁移只由
  `675b0744` 引入（`git log --follow` 唯一记录）。up 迁移注释明确：
  仅在专属临时测试库 `imboy_cache_closeout_v1` 验证过 apply/rollback，
  **生产从未 apply**，生产启用属另一轮部署决策。

## 1. 路径 A：全量回滚（revert 三个代码提交，6217ec8e 纯文档保留）

按提交倒序逐个 revert，每步 revert 是一个独立原子提交（符合仓储整洁规范：
一次 revert = 一次回滚主题，禁止三个合成一个提交）。

```bash
# 前置：确认工作树干净（尤其 cache_sync 三文件 + frozen.md 的未提交改动已处置）
git status --porcelain

# 步骤 1：回滚 G-QR
git revert 675b0744
# 验证（见 1.1）

# 步骤 2：回滚 G-01
git revert e001fb8e
# 验证（见 1.2）

# 步骤 3：回滚 C02-C05 同步核心
git revert 7db4e0af
# 验证（见 1.3）
```

### 1.1 revert `675b0744` 之后验证什么

- 编译通过：`make compile`。随 revert 一并删除的模块
  `qr_login_session_repo`（以及测试 `test/repo/qr_login_session_repo_tests.erl`）
  只被 `qr_login_handler.erl` / `qr_login_sse_handler.erl` 引用，而这两个
  handler 同 revert 回到父版本（父版本已核实无 `authority`/`get_env` 引用），
  不存在悬空引用。
- 测试应保持绿：`test/api/qr_login_handler_tests.erl` 与
  `test/api/qr_login_sse_handler_tests.erl`（这两个文件不在提交文件面内，
  revert 不触碰它们；它们针对的是 cache 旧行为，父版本 handler 恢复后应绿）。
  运行方式：`make eunit`（或 eunit 过滤 qr 相关用例）。
- DB 侧：生产未 apply 00000170，**无需任何数据库动作**。若某环境（如
  `imboy_cache_closeout_v1` 测试库）已 apply，按第 2 节的顺序补跑 down。

### 1.2 revert `e001fb8e` 之后验证什么

- 编译通过：`make compile`。`imboy_account_allocator` 模块随 revert 删除；
  它在 HEAD 中仅被 `account_ds.erl` 与其测试引用（已核实无 supervision、
  无 app.src 引用），account_ds 同 revert 回到父版本后无悬空引用。
- 测试应保持绿：`test/ds/account_ds_tests.erl`（该文件随 revert 回到父
  版本，运行父版本套件应绿；G-01 新增的 6 个 allocator 并发用例随 revert
  消失，属预期）。
- 行为回退说明：回到父版本即回到"allocate/0 读-改-写"路径——G-01 修复的
  并发重复分配缺陷在该版本下**重新存在**（两并发注册可能领到同一账号
  编号）。这是已知历史缺陷，回滚即接受该风险，须在回滚决策中明示。

### 1.3 revert `7db4e0af` 之后验证什么

- 前置（再次强调）：revert 前工作树上 `imboy_cache_sync.erl` /
  `imboy_cache_sync_proto.erl` / `imboy_cache_sync_tests.erl` /
  `protocol-budget-frozen.md` 的未提交改动必须已提交或 stash。
- 编译通过：`make compile`。新模块 `imboy_cache_sync_proto` /
  `imboy_cache_sync_snapshot` 随 revert 删除；它们在父版本（a7783438）中
  不存在，故父版本代码不存在对它们的引用，回退自洽。
- 测试应保持绿：`test/lib/imboy_cache_sync_tests.erl` 随 revert 回到父
  版本（17 个升级前用例），运行应绿；C02-C05 新增 12 个用例随 revert
  消失，属预期。
- **集群一致性**：C02-C05 变更了 V2 同步信封语义（epoch/config_hash/
  registry_hash/incarnation 注册宽限窗等）。若在多节点部署中回滚，
  必须全集群同版本回退，禁止 7db4e0af 节点与父版本节点混跑（混跑期
  行为未定义，prometheus `ImBoyCacheSyncUnknownEvents` 告警正是混跑/
  协议漂移信号）。

### 1.4 deploy 联动：无需变更（取证理由）

- `7db4e0af` 文件面不含 `deploy/` 与 `config/`（`git diff --name-only
  7db4e0af^ 7db4e0af` 过滤 deploy/config 零命中）。
- prometheus 告警 `deploy/prometheus/rules/imboy-alerts.yml` 的
  `imboy.cache_sync` 组引用指标 `reject_total` / `unknown_total`（及面板用
  publish/receive/reconcile）。这些指标名在父提交 `a7783438` 的
  `imboy_cache_sync.erl` 中已存在（`git grep` 取证），`7db4e0af` 只是沿用
  并新增计数点——回滚后指标名仍存在，告警表达式继续有效。
- env 集合零变化：`7db4e0af` 前后 cache 模块读取的 application env 完全
  相同（`cache_fenced_namespaces` / `cache_registry_overrides` /
  `cache_replication` / `cache_sync_protocol`），无新增/删除 env，无需
  配置回滚。
- C03 预算（Nmax/Tmax/Bmax/Pmax/Rmax/Dmax/high_water）全部为
  `imboy_cache_policy.erl` 内的 `-define` 编译常量（DEFAULT_N_MAX 等），
  随代码 revert 自动回退，无运行时配置残留。

**结论：路径 A 全程不需要动 `deploy/` 下任何文件，无联动配置项。**

## 2. 路径 B：单特性回滚（仅回滚 G-QR，含 DB down 迁移）

适用：只撤销二维码登录 PG 权威特性，保留 G-01 修复与 C02-C05 同步核心。

### 2.1 执行顺序：先跑 DB down，后 revert 代码（顺序不可颠倒）

**正确顺序（按 erlang_migrate 严格模式语义推导）：**

```
前置：若目标环境 qr_login_authority 曾设为 pg，先切回 cache（见 2.3）
第 1 步：代码仍在位（含 priv/migrations/00000170_*.sql）时执行 DB down
第 2 步：git revert 675b0744（移除迁移文件与代码）
```

down 调用形态（imboy_migrate 只封装了 up，down 需直调 erlang_migrate，
Config 构造对齐 `src/lib/imboy_migrate.erl` 的 `#{conn => ..., dir => ...,
strict => true}`）：

```erlang
%% 生产/目标库连接（super_account 配置），dir 为 priv/migrations 绝对路径
erlang_migrate:down(Config, 1).   %% 只回退 1 步 = 00000170
%% 或精确跳转：erlang_migrate:goto(Config, 169).
```

**顺序不可颠倒的理由（erlang_migrate 源码取证，strict => true 为
imboy_migrate 固定配置）：**

1. down 的执行前提是**能读到 down SQL 文件**：`erlang_migrate:down/2` →
   `erlang_migrate_source:scan(Dir)` 扫描迁移目录构建文件清单 →
   `pending_down/2` 从**文件清单**（不是 DB history）中取
   `version =< Current` 的迁移 → `run_one_down` 调
   `erlang_migrate_source:read_sql(DownFile, down)` 读取 down SQL。
   若先 revert（删除 00000170 文件），down SQL 无从读取，00000170 永远
   无法通过工具回退。
2. 更严重的次生风险：文件删除后 `Current` 仍为 170，而 `pending_down`
   只看文件清单——此时执行 `down(Config, 1)` 会**跳过 170 直接回退
   00000169**（personal_message_acceptance_order），误伤无关迁移。
3. strict 模式的 out_of_order 检测（strict_prepare 的 enforce 模式）只在
   **up** 方向拒绝"文件存在但版本 =< Current 且不在 history"；down 方向
   用 init 模式（仅补 history）。因此"先删文件"不会报错拦截，只会留下
   current=170 但 170 无文件的隐性漂移状态，且补救只能走
   `erlang_migrate:force/2` 手工改版本号 + 手工 DROP TABLE，脱离标准
   工具路径。
4. 先 down 后 revert 的终态是干净的：down 成功后 current_version 回到
   169、`schema_migrations_history` 删除 170 行（strict_record_down），
   再删文件时 DB 记录与文件集重新一致，下一次 up 不会触发任何
   strict 告警。

### 2.2 down SQL 的安全语义（token 无复活路径——已核对）

- `00000170_qr_login_session.down.sql` 全文即一条
  `DROP TABLE IF EXISTS public.qr_login_session;`（文件自带注释：
  "回滚不得把 token 恢复成可再次领取，表删除后无任何残留路径"）。
- up 迁移只创建一张表 + 一个 `expires_at` 部分索引，**无触发器、无
  备份表、无事件表**保存 token 副本——DROP 后 login_token（含已确认
  未消费的）随表消失，不存在任何 SQL 层面的复活/残留路径。
- 代码层同样闭合：`qr_login_session_repo:claim_login_token/1` 的原子消费
  为单语句
  `UPDATE ... SET login_token = NULL, consumed_at = now() WHERE session_token = $1 AND status = 'confirmed' AND login_token IS NOT NULL AND consumed_at IS NULL AND expires_at > now() RETURNING OLD.login_token, scanned_by`
  ——PG 行级锁下恰好一个并发消费者拿到 `OLD.login_token`（取出即置
  NULL + consumed_at 打戳），0 行返回映射 `not_claimable`；WHERE 守卫
  （login_token IS NOT NULL AND consumed_at IS NULL）保证已消费行不可能
  被二次领取。confirm 侧 `transition_confirm/3` 亦为单语句 CAS 写入
  token（WHERE login_token IS NULL AND consumed_at IS NULL），不存在
  "confirmed 但 token 未写"的中间态。回滚删表后，PG 权威路径整体消失，
  与"token 不可复活"语义一致。

### 2.3 回滚后旧 QR 行为恢复（cache 模式 authority 开关——只读确认）

`675b0744` 版本 `src/api/qr_login_handler.erl` 的权威选择实现：

```erlang
-spec authority() -> cache | pg.
authority() ->
    case application:get_env(imboy, qr_login_authority, cache) of
        pg -> pg;
        _ -> cache
    end.
```

- env 开关用法：`application env {imboy, qr_login_authority}`，取值
  `pg | cache`，**默认 cache**。任何非 `pg` 值（含未设置）都落在 cache
  路径。运行时可经 release 的 sys.config 设置，或
  `application:set_env(imboy, qr_login_authority, pg)` 热切换；handler
  每个请求都调用 `authority/0`（create/status/scan/confirm/cancel 全部
  走该分发），无需重启。
- 已核实：`config/` 目录从未设置 `qr_login_authority`，全仓只有
  `qr_login_handler.erl` 一处出现该键——**生产/本地默认一直运行 cache
  路径，行为与收口前完全一致**。
- 回滚视角：PG 路径仅当显式设 `pg` 且迁移已 apply 才激活；单特性回滚
  若曾在某环境显式开过 `pg`，须在第 2.1 节第 1 步之前先把该 env 切回
  `cache`（或直接移除），再 down、再 revert——否则 revert 前的窗口内
  pg 路径会访问一张即将/已被 DROP 的表。cache 路径（depcache 读写 +
  confirm 事件带 token 的 SSE 推送）在父版本代码中保持原样，revert 后
  无任何开关残留问题（env 即使残留 `pg` 值，authority/0 已随 revert
  消失，无消费者）。
- 已知行为差异回退说明：G-QR 修复的"cancel-after-confirmed 覆盖
  confirmed 状态"地雷，在 cache 路径下重新存在（cache 路径
  handle_cancel 会覆盖 confirmed 状态）；单特性回滚即接受该旧语义。

### 2.4 单特性回滚后验证

- `make compile` + `make eunit`：重点 `test/api/qr_login_handler_tests.erl`
  / `test/api/qr_login_sse_handler_tests.erl` 保持绿（同 1.1）。
- DB 核对：`SELECT version FROM schema_migrations;`（应回到 169，无
  dirty 位）；目标库 `\d public.qr_login_session` 应报不存在。
- G-01 / C02-C05 不受影响：`test/ds/account_ds_tests.erl`（HEAD 版，
  含 allocator 用例）与 `test/lib/imboy_cache_sync_tests.erl`（HEAD 版，
  含 C02-C05 用例）保持绿，作为"未误伤"的回归哨兵。

## 3. 两路径速查

| | 路径 A：全量回滚 | 路径 B：仅回滚 G-QR |
|---|---|---|
| git 动作 | revert 675b0744 → e001fb8e → 7db4e0af（各成原子提交） | 仅 revert 675b0744 |
| DB 动作 | 生产无（未 apply）；已 apply 的环境按 2.1 先 down 后 revert | 同左（顺序强制：env 切 cache → down → revert） |
| 前置 | 工作树 cache_sync 三文件 + frozen.md 改动先提交/stash | 工作树不与 675b0744 文件面重叠，可直接执行 |
| 部署联动 | 无（deploy/config 零命中，指标名/env 集合/预算常量三重取证） | 无 |
| 绿名单 | 1.1/1.2/1.3 各节测试 | 2.4 节测试 |
| 保留物 | 6217ec8e 及 5 个中间 docs 提交留在历史 | G-01、C02-C05 全保留 |
| 接受的风险 | G-01 并发 ID 缺陷重现；C02-C05 协议加固消失 | cache 路径 cancel-after-confirmed 地雷重现 |
