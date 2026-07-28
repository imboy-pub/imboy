# E2EE-062：OTK claim 幂等租约（第一刀）

- **Slice**：`22-claude-code-execution-state.md` §1.1 自动推进队列第 3 项
- **会话**：`20260728-1930-claude-code`
- **仓库**：`imboy`（后端单仓）
- **状态**：`PARTIAL` —— 幂等租约闭合；四层限流与耗尽告警**未做**（见 §5）
- **上位验收**：`21-claude-code-execution-playbook.md` E2EE-025

---

## 1. 做了什么

X3DH 的 one-time prekey 是**一次性资源**：每次 claim 消费池里一条。
改动前 claim **没有任何幂等键**——客户端一次网络超时后的重试、或 app 重启后的
重发，都会再消费一条 OTK。正常使用即可把对端池打空；恶意方只要重放同一个
请求就能定向耗尽某个用户的池，把其所有新会话逼到复用同一条 fallback prekey
（前向保密显著下降）。

本轮实现 playbook 验收表第 2 条的前半句：**「同 request id 重放 100 次只消费一次」**。

| # | 接缝 | 改动 |
|---|---|---|
| 1 | schema | 迁移 `00000049`：`olm_one_time_key` 加 `claim_request_id varchar(64)` + 部分唯一索引 `uk_olm_otk_claim_request (claimed_by, user_id, device_id, claim_request_id) WHERE claim_request_id IS NOT NULL` |
| 2 | repo | `olm_identity_repo:claim_one_time_key/4`：先查租约 → 未命中则消费并登记 → 撞 23505 回查（TOCTOU 兜底） |
| 3 | ds | `olm_identity_ds:claim_one_time_key/4` |
| 4 | logic | `olm_identity_logic:claim_keys/4` + `claim_with_identity/5` |
| 5 | handler | `POST /api/v1/e2ee/olm/claim` 读可选 `request_id`，经 `normalize_request_id/1` 白名单归一化 |

### 1.1 四处设计取舍（按「选安全那个」裁决，未询问）

**取舍一：租约按「领取方」隔离，唯一索引含 `claimed_by`。**
若只按 `(user_id, device_id, request_id)` 建键，攻击者猜到别人的 request_id
即可读回**别人已领的 OTK 公钥**——`request_id` 就成了越权读取通道。
虽然读到的是公钥、危害有限，但这属于把幂等键当能力凭证，方向就错。
守护用例：`lease_is_scoped_to_claimant` / `lease_scoped_to_claimant`（单测 + 真 PG 各一条）。

**取舍二：部分唯一索引不可省，光靠「先查后写」是 TOCTOU。**
两个并发同 `request_id` 的请求可能同时查空租约。加 `WHERE claim_request_id
IS NOT NULL` 的部分唯一索引后，第二条 UPDATE 撞 23505，repo 捕获后回查返回
第一条的结果——**不重复消费**。真 PG 50 路并发用例即为此而设。

**取舍三：`request_id` 非法时降级为「无幂等」，而不是拒绝请求。**
`normalize_request_id/1` 只接受 `[A-Za-z0-9_.-]{1,64}`，不合规一律降为 `<<>>`
（= 逐次消费的旧语义）。幂等键是可选优化、**不是安全边界**；用它来拒绝请求
反而给攻击者开了一个新的拒绝服务面（构造非法 request_id 让对方 API 报错）。
长度上限 64 对齐迁移的 `varchar(64)`，防超长写入直接触发 DB 错误。

**取舍四：租约回查失败（DB 错误）时 fail-closed 地返回 `not_found`。**
`find_claim_by_request/4` 在 `{error, _}` 分支返回 `{error, not_found}`。
这看起来像 fail-open（会去消费一条新 key），但另一侧是「查询抖动即整条 claim
失败 → 无法建立会话」。取舍理由：**多消费一条 OTK 的代价（池水位下降）远小于
新会话建不起来**，且池耗尽还有 fallback 兜底与后续的限流层。此处已加注释。
**这是本轮唯一一处主动接受的降级，明确记录以便复审。**

### 1.2 保留原调用形状（A2-a 教训，本轮再次被实证）

新增 arity 时**没有**把旧 arity 改成「委托新 arity」：
- `olm_identity_logic:claim_with_identity/4` 仍直调 `olm_identity_ds:claim_one_time_key/3`；
- handler 在 `request_id` 为空时调 `claim_keys/3`，非空才调 `/4`。

第一版没这么做，结果 `olm_handler_claim_throttle_tests` **真实回归**：
它按 arity 给 `claim_keys/3` 挂 meck 期望，handler 改走 `/4` 后期望不命中，
静默穿透到真实实现 → `{noproc,{gen_server,call,[pgsql,...]}}`。
**这是本会话第二次踩同一个坑（A2-a 是第一次）**，已在 22 的队列备注里加粗记录。

---

## 2. RED 记录

### 2.1 第一次尝试：`meck` 拒绝挂不存在的 arity（不算 RED）

`meck:new(olm_identity_ds, [passthrough])` + `expect(..., claim_one_time_key, 4)`
直接报 `{undefined_function,{olm_identity_ds,claim_one_time_key,4}}`。
这是**结构缺失**不是行为失败。因此先只加**承载 arity**
（ds/repo/logic 的 `/4` 全部原样委托 `/3`，**不实现任何幂等语义**），
把 RED 降格成纯行为问题。

### 2.2 真正的 RED（行为失败）

```
=======================================================
  Failed: 1.  Skipped: 0.  Passed: 4.
```

唯一红的是 `replay_consumes_once`：

```
{expected,[<<"k1">>]},
{value,[<<"k1">>,<<"k2">>,<<"k3">>]}
```

100 次重放把 3 条 OTK **全部消费光** —— 正是要修的耗尽缺陷，逐字复现。

### 2.3 对照组（harness 有效性）

RED 阶段就绿、且修复后必须**仍然绿**的 4 例：

- **`distinct_requests_consume_each`（正向可用性）** —— 不同 request_id 各自消费一条。
  这一条专门否掉「永远返回同一条 key」的作弊实现：那种实现在幂等指标上恒得满分，
  却让所有会话共用一条 prekey。**没有这条，幂等验收就是无效验收。**
- `legacy_no_request_id` —— 旧客户端零破坏。
- `lease_is_scoped_to_claimant` —— 换领取方不得命中他人租约。
- `replay_after_exhaustion` —— 耗尽后走 fallback，重放仍幂等。

四条在改前改后都绿 → harness 本身没坏、断言不是恒成立。

### 2.4 状态化 mock，不是静态返回值

单测沿用 `olm_otk_lifecycle_tests` 的 ets 池写法：「消费了几条」是**可观测的真实
状态**，不是调用次数的间接推断。mock 的 `claim_one_time_key/4` 忠实复刻服务端
应有语义，并在注释里指明生产实现位置（`olm_identity_repo:claim_one_time_key/4`）。

---

## 3. 生产调用方核实

| 被测函数 | 生产调用链 |
|---|---|
| `olm_identity_repo:claim_one_time_key/4` | `olm_handler:do_claim_key1/2` → `olm_identity_logic:claim_keys/4` → `olm_identity_ds:claim_one_time_key/4` → 本函数 |
| `olm_identity_logic:claim_keys/4` | `olm_handler:do_claim_key1/2`（`POST /api/v1/e2ee/olm/claim`，带 `request_id` 时） |
| `olm_identity_logic:claim_keys/3` | 同上（不带 `request_id` 时）+ `batch_claim_keys/3` |

---

## 4. 验收命令与结果

### 4.1 门禁套件

```
$ make e2ee-verify
  All 309 tests passed.
=== E2EE verify ALL PASSED ===
```

基线 292 →（A2-a 后 304）→ **309**（本轮 +5）。
新模块 `e2ee_otk_claim_idempotency_tests` **已加进 Makefile Modules 清单**。

### 4.2 真 PostgreSQL 验收（**已实证**）

```
$ IMBOYENV=local make eunit t=e2ee_otk_claim_idempotency_integration_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  All 5 tests passed.
```

5 例全部打在 `olm_identity_repo:claim_one_time_key/4`（生产链末端）：

1. 同 request_id 重放 **100 次**：池只少 1 条，恒返回同一条 key；
2. **正向可用性**：不同 request_id 各自消费一条；
3. 旧客户端（`<<>>` 与 `/3`）连续 3 次消费 3 条，NULL 行不撞唯一索引；
4. 换领取方不得命中他人租约；
5. **50 路并发**同 request_id：50 个成功返回、`usort` 后只有 1 个 key_id、池只少 1 条。

> 该模块**未**加进 e2ee-verify Modules 清单，与既有
> `e2ee_message_pipeline_integration_tests` 的处置一致（无 DB 时会 skip，
> 放进硬门禁只会得到一个假绿）。运行方式如上，已写进本文件。

### 4.3 迁移落地实证（直连本地 PostgreSQL，非推断）

```
$ psql ... "select * from schema_migrations order by version desc limit 1;"
49|f|2026-07-28 18:06:24.79567+08

$ psql ... "select column_name,data_type,character_maximum_length from information_schema.columns
            where table_name='olm_one_time_key' and column_name='claim_request_id';"
claim_request_id|character varying|64

$ psql ... "select indexname from pg_indexes where tablename='olm_one_time_key'
            and indexname='uk_olm_otk_claim_request';"
uk_olm_otk_claim_request
```

### 4.4 受影响模块的回归

| 套件 | 结果 |
|---|---|
| `olm_handler_claim_throttle_tests` | 在 e2ee-verify 内全绿（**先红过 1 例**，见 §1.2） |
| `olm_identity_repo_tests` | 10/10 ✅ |
| `olm_identity_logic_tests` | 28/28 ✅ |
| `olm_handler_tests` | 5/5 ✅ |
| `olm_otk_lifecycle_tests` | 5/5 ✅ |
| `git diff --check` | 通过 |
| `erlfmt --check`（全部改动文件） | 通过 |

---

## 5. 残留风险（**本任务远未完成**）

playbook E2EE-025 共 5 条验收，本轮只闭合了第 1 条的一半 + 第 2 条的前半句。
**任务状态因此是 `PARTIAL`，不是 `PASS`。** 未做的：

1. **四层限流只有一层。** 现状只有 per-claimant（`throttle:check(olm_claim, CurrentUid)`,
   30/min）。playbook 要求**单目标 / 单请求者 / 单租户 / 全局**四层，且硬上限
   不可被关闭。**缺 per-target 层意味着：N 个账号协同仍可定向耗尽同一个目标的池**——
   幂等租约挡不住「每次换新 request_id」的攻击。这是本任务剩下最重要的一刀。
   实现位置：`olm_handler:do_claim_key1/2` 解析出 `TargetUid` 之后加一次
   `throttle:check(olm_claim_target, TargetUid)`，配 `config/sys.config` 新 scope。
   **认识论状态：设计已想清，未实现、未验证。**

2. **`batch_claim` 完全没接幂等租约。** `olm_identity_logic:batch_claim_keys/3`
   逐设备调 `claim_keys/3`（无 request_id），因此多设备 fan-out 路径的重试
   仍会逐次消费。需要 per-device 的 request_id 派生（如 `<<ReqId/binary, ":", DeviceId/binary>>`）。
   **认识论状态：文件级阅读结论，未实证。**

3. **租约无过期/清理。** `claim_request_id` 随审计行由
   `cleanup_consumed_one_time_keys/1` 一并清理（按 `consumed_at` 保留期）。
   保留期一过，同 request_id 重放会**重新消费一条**。playbook 说的是「有界租约」，
   当前边界是审计保留期，不是独立的租约 TTL。是否可接受**未经人工确认**。

4. **OTK 耗尽的 fallback 未验签。** playbook 要求「只使用协议允许且**身份验证通过**的
   signed fallback prekey，或拒发」。当前 `claim_fallback_key/2` 直接返回存储的
   fallback 公钥，未在服务端校验其签名。**认识论状态：文件级阅读结论，未实证。**

5. **「耗尽/限流绝不触发 RSA/Megolm/明文」未加守护用例。** 该不变量目前靠
   E2EE-HOTFIX-02/03 的 fail-closed 与客户端 Olm-only cutover 间接保证，
   本轮**没有**针对「OTK 耗尽」这一具体触发条件写反向守护。

6. **低水位补充（replenish）与耗尽告警缺失。** 池见底时没有主动通知设备补传，
   也没有运维可见的耗尽指标。属可用性缺口，不是安全缺口。

7. **客户端未接 `request_id`。** 后端已接受该字段，但 imboyapp 侧仍不发送，
   因此**当前生产流量一条也走不到幂等路径**。本轮改动是纯服务端能力铺设，
   端到端收益要等客户端接线。**这一条最重要——不要据本轮 evidence 认为
   「重试不再耗尽 OTK」已在生产成立。**

8. **真机双端未验证**（与既有真机腿同属停放区）。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 同 request_id 重放 100 次只消费一条、恒返回同一条 key | **已实证**（真 PostgreSQL） |
| 50 路并发同 request_id 只消费一条（TOCTOU 被唯一索引兜住） | **已实证**（真 PostgreSQL） |
| 不同 request_id 各自消费（未过度去重） | **已实证** |
| 租约按领取方隔离 | **已实证** |
| 旧客户端 / NULL 行零破坏 | **已实证** |
| 迁移 49 落地（列 + 部分唯一索引） | **已实证**（直连 PG 查 `information_schema` / `pg_indexes`） |
| 旧 arity 调用方零回归 | **已实证**（且是被 `olm_handler_claim_throttle_tests` 打脸后才修对的） |
| `batch_claim` 未接幂等 | **文件级阅读结论，未实证** |
| fallback 未验签 | **文件级阅读结论，未实证** |
| 「重试不再耗尽 OTK」在**生产**成立 | **不成立**——客户端未发送 `request_id`（残留风险 7） |
| 「OTK 抗耗尽」整体达成 | **不成立**——缺 per-target 限流（残留风险 1） |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改任何 ADR、未改协议规范、未动 E2EE-012/023/024/025/029 的状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略。
- §5 的 8 项残留全部原样留白，未以任何方式伪装成已完成。
