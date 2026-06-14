# imboy 后端 E2EE 端到端加密实现 —— 最终审计报告

> 审计日期：2026-06-13 ｜ 审计对象：`imboy/`（Erlang/OTP 4 层架构）
> 方法：健康会话内逐字核实（小 limit 读取规避超长会话污染），每个结论附 `文件:行号`。
> 置信度分层：**[高]** 已逐字读到证据 ｜ **[中]** 证据充分但依赖运行时行为推断 ｜ **[低]** 需运行环境验证。

---

## ① E2EE 如何工作（架构实证）

### 加密方案
- **信封加密**：RSA-OAEP-256（包装 AES 密钥）+ AES-256-GCM（加密消息体）。**[高]**
  - 套件标识 `<<"RSA-OAEP-256+AES-256-GCM">>`（见 `imboy/CLAUDE.md` E2EE 段 / `msg_c2c_ds` e2ee 字段）。
- 每条消息随机 AES 密钥，对每个**接收设备**用其 RSA 公钥单独包装。
- `e2ee` 字段结构（Map）：`{e2ee, e2ee_ver, e2ee_suite, nonce, keys:[{did, kid, wrap_alg, ek}]}`。
- **非 Signal 协议**：无 X3DH / prekey / double-ratchet，**无前向保密**。每次密钥轮换靠 `report_device_key` 全量替换公钥。

### 密钥存储与零知识
- `user_device` 表仅存 `public_key`、`key_id`，**无 `private_key` 列**（`priv/migrations/00000001_foundation.up.sql:2721-2736`）。**[高]**
- 唯一约束 `uk_userid_deviceid UNIQUE (user_id, device_id)`（迁移 `:2743-2744`），多设备模型正确。**[高]**
- 服务端无解密 `msg_*` payload 的逻辑，仅路由/存储密文。**零知识成立。** **[高]**

### 调用链
```
report_device_key  : e2ee_handler:report_device_key → e2ee_logic:report_device_key/6 → user_device_ds:update_public_key/save → user_device_repo
user_keys          : e2ee_handler → e2ee_logic:user_keys → user_keys_payload → user_device_ds:list_public_keys → user_device_repo:list_public_keys
group_member_keys  : e2ee_handler → e2ee_logic:group_member_keys → user_device_ds:list_public_keys_by_uids → user_device_repo
实时 E2EE 投递      : websocket_handler → msg_*_logic（原始 Map 直接转发，不经 DB 读取路径）
离线/历史 E2EE      : msg_c2c_ds:read_msg_filter → msg_c2c_repo:read_msg（经 DB 读取路径，见争议 D）
```

### 鉴权
- `/v1/e2ee/*` 全部走 JWT，`current_uid` 取自 token State，不信任客户端入参；SQL 全参数化。**[高]**

---

## ② A–G 争议逐项结论

> ⚠️ 总体修正：上一会话"倾向判断"中 **A、B、C 三项均与代码事实相反**。但 Agent 所报的"崩溃级"措辞同样不准确——这些 SQL 引用不存在的列时，`elib_pg:query/2` 把 epgsql 错误**收敛为 `{error, Reason}`**（不抛异常、不崩进程），因此实际表现为**功能性失效**而非崩溃。

### A. `get_private_key`/`update_private_key`/`get_public_by_uid` 是否废弃存根？
**结论：不是存根。倾向判断错误。是真实查询 `private_key` 列的活跃函数，但因列不存在而功能失效（不崩溃）。** **[高]**

- `user_device_repo:get_private_key/2`（`:182-196`）真实执行 `SELECT private_key FROM user_device ...`（`:185`）。
- `user_device_repo:update_private_key/3`（`:205-211`）真实执行 `UPDATE ... SET private_key = $1`（`:209`）。
- `user_device_repo:get_public_by_uid/1`（`:168-175`）SELECT 列表含 `private_key`（`:170`）。
- 表中**无 `private_key` 列** → epgsql 返回 `{error, #error{}}` → `elib_pg:query/3` 经 `Error -> Error`（`elib_pg.erl:229`）收敛为 `{error, Reason}`。**[中：依赖 epgsql 对未知列返回 error 而非抛异常的标准行为]**
- **活跃调用方（非死代码）**：
  - `get_private_key` ← `e2ee_social_logic:232`、`e2ee_social_handler:431`
  - `get_public_by_uid` ← `e2ee_transfer_logic:224`、`e2ee_social_logic:229`
  - `update_private_key`：仅 `user_device_ds` 薄封装，无上层调用方（实际死代码）。
- **Agent 报"private_key 列崩溃(C1)"：措辞不成立（不崩溃），但功能 bug 真实存在**——见 ③-真实 bug-5。

### B. 各 SELECT 是否都含 `key_id` 列？
**结论：都不含。倾向判断错误。Agent 报"漏 key_id (HIGH-1)"在事实层成立。** **[高]**

- `list_public_keys`（`repo:52`）：`device_id, device_type, public_key, last_active_at` —— 无 `key_id`。
- `list_public_keys_by_uids`（`repo:85`）：`user_id, device_id, device_type, public_key, last_active_at` —— 无 `key_id`。
- `get_public_by_uid`（`repo:170`）：含 `private_key` 但仍无 `key_id`。
- 表中**有 `key_id` 列**（迁移 `:2735`），SELECT 却不取。
- 影响：`/v1/e2ee/user_keys`、`/v1/e2ee/group_member_keys`（`e2ee_logic:130/28`）下发的设备列表**缺 `kid`**。客户端组装 `keys[].kid` 时拿不到密钥版本，密钥轮换/多 kid 场景会降级（无法精确匹配版本）。
- 严重度：HIGH→MEDIUM（取决于客户端是否强依赖 kid；若客户端按 `did` 兜底匹配仍可工作）。

### C. `elib_pg:query/2` 真实返回格式 & repo 匹配是否一致？
**结论：query 返回二元组 `{ok, [map()]}`，非三元组。倾向判断错误。多处 repo 用过时三元组匹配，导致静默失效（不崩溃）。** **[高]**

- `elib_pg:query/3`（`:222-231`）：把 epgsql 的 `{ok,Cols,Rows}`/`{ok,Count,Cols,Rows}` 统一转为 **`{ok, rows_to_maps(...)}` 二元组**（`:226/228`），spec 亦为 `{ok, [map()]}`（`:210-211`）。
- **过时匹配点（与新格式不符）**：
  - `count_other_device_keys`（`repo:73-78`）：`{ok, _, [#{<<"count">> := Count}]}` 三元组 → 不匹配二元组 → 落 `_ -> 0`（`:77`），**静默永远返回 0**。
  - `get_private_key`（`repo:188-193`）：`{ok, _, [{PrivateKey}]}` 三元组 + tuple 行，双重过时（叠加 A 的列缺失）。
- **正确范例对比**：`compliance_key_repo:find_active` 用 `{ok, [Row]}` 二元组匹配 query（正确）；`page`/`list_public_keys`/`list_public_keys_by_uids` 直接透传 query 结果给上层二元组匹配（正确）。
- 说明这是一次 `query/2` 返回格式重构后 **`user_device_repo` 漏适配**，系统性不一致。
- **Agent 报"格式崩溃(C2/H1)"：措辞不成立（不崩溃，因 query 收敛 + repo 有 catch-all/error 分支），但 `count_other_device_keys` 静默返回 0 是真 bug**——见 ③-真实 bug-3。

### D. msg ds 读库时 e2ee 字段是否做 JSON 反序列化？
**结论：未做。Agent 报告成立。这是影响最大的真实 bug。** **[高]**

- 写入：`msg_c2c_repo:write_msg`（`:96-105`）将 e2ee map `jsone:encode` 成 **JSON 字符串**存入 `e2ee` 列（`:101`）。
- 读取：
  - `msg_c2c_ds:read_msg_filter`（`:329-334`）SELECT 含 `e2ee` 列，但只 `json_decode_field(Row, <<"payload">>)`（`:334`），**未对 `e2ee` 反序列化**。
  - `msg_c2g_ds`（`:263`、`:287`）：同样只 decode `payload`。
  - `msg_s2c_ds`（`:255`）：只 decode `payload`。
- `json_decode_field/2`（`elib_response.erl:96-122`）只把**指定字段**的 binary JSON 解析为 map；e2ee 列未传入 → 原样下发。
- 影响：**离线消息 + 历史消息拉取**的 E2EE 元数据以转义 JSON 字符串下发，客户端（要求 e2ee 为 Map）解析失败 → 历史/离线 E2EE 密文无法解密。
- 范围界定：**实时在线投递走 WebSocket 原始 Map 转发，不经 DB 读取路径，不受影响**；仅离线/历史链路受损。与记忆 `project_e2ee_cross_device_recovery`"换机/离线不平滑"吻合。

### E. `compliance_key/2` 模式匹配与 logic 返回是否一致？
**结论：不一致，case_clause 崩溃。Agent 报"C2"成立。** **[高]**

- `compliance_key_repo:find_active`（`repo` 函数体）返回 **二元组 `{ok, Row}`**（Row 是 map）。
- `e2ee_logic:get_active_compliance_key/0`（`:173-175`）透传该二元组。
- `e2ee_handler:compliance_key/2`（`:377-388`）仅有 `{ok, KeyId, PublicKey}`（三元组，`:379`）、`{error, not_found}`、`{error, _}` 三分支。
- 二元组 `{ok, Map}` 不匹配任一分支 → **case_clause 异常 → cowboy 返回 500**。该端点一调用即崩。

### F. `create_shards` 是否校验 `proxy_uid` 归属？
**结论：未校验。Agent 报"H2"成立（事实层）。严重度 MEDIUM。** **[高]**

- `e2ee_social_handler:do_create_shards`（`:96-124`）：仅 `throttle` 限流（`:98`）+ 字段非空（`:110-112`），未校验 proxy_uid。
- `e2ee_social_logic:create_shards`（`:47-137`）：仅校验分片数 ≤ `MAX_SHARDS`（5，`:57`）+ 每片含 `proxy_uid`/`encrypted_shard` 字段（`:63-80`），**不校验 proxy_uid 是否为调用者好友/可信联系人**。
- 对比：`add_trusted_contact` 路径有 `{error, not_friend}` 校验（`handler:360`），说明系统具备好友校验能力却未用于 create_shards。
- 危害评估：可向任意 uid 注入分片记录（数据完整性/垃圾注入/轻度 DoS），受 `MAX_SHARDS=5` + throttle 限制；分片是用代理公钥**预加密**的密文，攻击者不掌握代理私钥，**不泄密**。故 MEDIUM 而非 CRITICAL。

### G. `compliance_key` 端点是否缺能力闸门？
**结论：缺 `ensure_e2ee_enabled`。Agent 报"M1"成立。严重度 MEDIUM。** **[高]**

- `e2ee_handler` 其余 8 个端点均在入口调用 `ensure_e2ee_enabled`（`:70,96,125,207,236,265,318,340`）。
- `compliance_key/2`（`:377`）**未调用**，直接进 logic。E2EE 全局关闭时该端点仍可访问（仅泄露合规公钥，信息量低）。
- 注：因争议 E 该端点本就一调用即 500，G 属叠加在已崩溃端点上的次要缺陷。

---

## ③ 真实 Bug 清单（按严重度）

| # | 严重度 | 位置 | 问题 | 影响 | 置信度 |
|---|--------|------|------|------|--------|
| 1 | **HIGH** | `msg_c2c_ds:334`、`msg_c2g_ds:263/287`、`msg_s2c_ds:255` | 读历史/离线消息只 decode `payload`，未 decode `e2ee` 列（写入 `msg_c2c_repo:101` 存的是 JSON 字符串） | 所有离线/历史 E2EE 消息元数据以字符串下发，客户端无法解密 | 高 |
| 2 | **HIGH** | `e2ee_handler:379` vs `compliance_key_repo:find_active` | handler 三元组匹配 vs logic 二元组返回 → case_clause | `GET /v1/e2ee/compliance_key` 一调用即 500，合规双密钥加密不可用 | 高 |
| 3 | **HIGH** | `user_device_repo:73-78` | `count_other_device_keys` 三元组匹配 query 二元组 → 静默返回 0 | 换设备/重装检测永久失效，E2EE 恢复横幅永不显示（`e2ee_logic:99-103`） | 高 |
| 4 | **HIGH** | `e2ee_logic:216-219` | `pull_key_changes_from_db` SELECT 查不存在的 `ud.updated_at` 列 | `pull_key_notifications` 端点永久返回 `{error}`，密钥变更轮询不可用 | 高 |
| 5 | **MEDIUM→HIGH** | `user_device_repo:168/182` + `e2ee_social_handler:252-258` | "服务端解密代理分片"端点取服务端私钥（`get_proxy_private_key`）解密，与零信任设计矛盾；因 `private_key` 列缺失而永久失败 | 社交恢复的服务端解密路径永久失败（客户端本地重组路径 `recover_key`/`validate_shards` 不受影响）；更是架构设计缺陷 | 高（行为）/ 中（设计意图） |
| 6 | **MEDIUM** | `repo:52/85/170` | E2EE 公钥下发 SELECT 缺 `key_id` 列 | `user_keys`/`group_member_keys` 缺 `kid`，密钥轮换/多版本场景降级 | 高 |
| 7 | **MEDIUM** | `e2ee_social_logic:47-137` + `handler:96-124` | `create_shards` 不校验 `proxy_uid` 归属 | 可向任意 uid 注入加密分片（数据完整性/垃圾注入，不泄密） | 高 |
| 8 | **MEDIUM** | `e2ee_handler:377` | `compliance_key` 缺 `ensure_e2ee_enabled` 闸门 | E2EE 关闭时仍可访问（叠加在 #2 已崩溃端点上） | 高 |
| 9 | **LOW** | `user_device_repo:205` | `update_private_key/3` 引用不存在列，但无上层调用方 | 死代码地雷，未触发 | 高 |

> 说明：上一会话 Agent 报的"崩溃级 C1/C2/H1"中，"崩溃"措辞普遍不准确（`elib_pg:query/2` 把列错误收敛为 `{error}`，且各 repo 多有 `{error}`/catch-all 兜底）。唯一真正 case_clause 崩溃的是 **#2 compliance_key（E）**。其余为功能性失效/静默错误。

---

## ④ 能不能正常使用？——明确判断

**可用，但仅限"同设备、在线、当前会话"的基础 E2EE；凡涉及"换设备 / 重装 / 离线 / 历史恢复 / 合规密钥"的链路均存在真实缺陷。**

| 能力 | 可用性 | 依据 |
|------|--------|------|
| 实时在线 C2C/C2G E2EE 投递 | ✅ 正常 | WS 转发原始 Map，不经 DB 读取路径；零知识成立 |
| 公钥上报 `report_device_key` | ✅ 正常 | `update_public_key`（`ds:152-159`）SET 列均存在，写路径正确 |
| `user_keys`/`group_member_keys` 公钥下发 | ⚠️ 降级 | 缺 `kid`（#6），多数客户端可按 `did` 兜底 |
| **离线/历史 E2EE 消息解密** | ❌ 不可用 | #1 e2ee 列未反序列化 |
| **换设备恢复横幅** | ❌ 失效 | #3 `count_other_device_keys` 永远 0 |
| 社交恢复（Shamir，客户端本地重组） | ✅ 可用 | `recover_key`/`validate_shards` 不依赖 private_key 列 |
| 社交恢复（服务端解密代理分片端点） | ❌ 永久失败 | #5 私钥列缺失 + 零信任矛盾设计 |
| `pull_key_notifications` 轮询 | ❌ 不可用 | #4 `updated_at` 列缺失 |
| `compliance_key` 合规密钥 | ❌ 崩溃 500 | #2 case_clause |

**总评**：核心实时加密链路健全、零知识架构成立、鉴权与参数化到位；但作为 IM 至关重要的"换机/重装/历史消息恢复"体验，受 #1/#3/#4/#5 拖累而不平滑——这与记忆 `project_e2ee_cross_device_recovery` 的结论在**后端技术根因层面**高度吻合（此前归因偏"缺自动备份机制"，本次定位到更具体的 4 个后端代码缺陷）。

---

## ⑤ 修复优先级

**P0（HIGH，影响核心恢复体验，改动小）**
1. **#1**：在 `msg_c2c_ds:334`、`msg_c2g_ds:263/287`、`msg_s2c_ds:255` 对 `e2ee` 列追加 `json_decode_field(Row, <<"e2ee">>)`（与 payload 同等处理）。
2. **#2**：`e2ee_handler:compliance_key/2` 改为二元组匹配：`{ok, #{<<"key_id">>:=KeyId, <<"public_key">>:=PublicKey}} -> ...`。
3. **#3**：`user_device_repo:count_other_device_keys` 改用二元组匹配 `{ok, [#{<<"count">> := Count}]}`；并全量排查 `user_device_repo` 中所有 `{ok, _, ...}` 三元组匹配，统一改为二元组。
4. **#4**：`e2ee_logic:pull_key_changes_from_db` 的 SQL 把 `ud.updated_at` 改为 `ud.last_active_at`（或迁移补加 `updated_at` 列并在 `update_public_key` 维护）。

**P1（架构决策）**
5. **#5**：决策"社交恢复是否坚持零信任"。建议**移除** `get_proxy_private_key` 及 `e2ee_social_handler:252-258` 的服务端解密分支、移除 `user_device_repo:get_private_key/get_public_by_uid` 对 `private_key` 列的引用（改 `get_public_by_uid` SELECT 去掉 `private_key`），明确"私钥永不落库、解密仅在客户端"。

**P2（MEDIUM，安全/契约完整性）**
6. **#6**：`list_public_keys`/`list_public_keys_by_uids`/`get_public_by_uid` 的 SELECT 补 `key_id` 列并在响应映射出 `kid`。
7. **#7**：`e2ee_social_logic:create_shards` 增加 `proxy_uid` 校验（须为调用者好友或可信联系人，复用 `not_friend` 校验链）。
8. **#8**：`e2ee_handler:compliance_key/2` 入口补 `ensure_e2ee_enabled`。

**P3（清理）**
9. **#9**：删除死代码 `user_device_repo:update_private_key/3` 及 `user_device_ds` 对应封装。

---

## 附：核实方法说明
- 全程小 limit 读取关键文件，凡遇 grep 输出含 `\d+:0:` 异常前缀（污染特征）即改用 `Read` 工具逐字复核（如表定义、find_active、write_msg）。
- `imboy_db.erl` 不存在；DB 入口为 `elib_pg.erl`，争议 C 据此核实。
- 未发现"社交恢复列名 nickname bug"（确认 `e2ee_social_repo` 用 `contact_nickname`，与既有结论一致，未重复展开）。

---

## ⑥ 修复实施记录（2026-06-13）

### 已实施并通过 `erlc +strong_validation` 校验

| 项 | 文件:行 | 改动 |
|----|---------|------|
| #1 | `msg_c2c_ds`/`msg_c2g_ds`/`msg_s2c_ds` | 读历史/离线消息时对 `e2ee` 列追加 `json_decode_field`（与 payload 同等反序列化） |
| #2 | `e2ee_handler` `do_compliance_key` | `compliance_key` 改二元组匹配 `{ok, #{<<"key_id">>:=.., <<"public_key">>:=..}}`，消除 case_clause 崩溃 |
| #3 | `user_device_repo:count_other_device_keys` | 三元组→二元组匹配，修复换设备检测静默返回 0 |
| #4 | `e2ee_logic:pull_key_changes_from_db` | `updated_at`列→`last_active_at`；`{ok,_,Rows}`三元组→二元组；取值键同步 |
| #5底层 | `user_device_repo:get_public_by_uid` | SELECT 去掉不存在的 `private_key` 列（私钥永不落库），同时补 `key_id` |
| #6 | `user_device_repo` 三处 SELECT | `list_public_keys`/`list_public_keys_by_uids`/`get_public_by_uid` 补 `key_id` 列，使 `user_keys`/`group_member_keys` 下发 kid |
| #7 | `e2ee_social_logic:create_shards` | 增加 `is_trusted_contact(Uid, ProxyUid)` 校验，拒绝向非可信联系人注入分片（`?ERR_FORBIDDEN`） |
| #8 | `e2ee_handler:compliance_key` | 入口补 `ensure_e2ee_enabled` 能力闸门 |

> 未做 git 提交（遵循规范，未获授权）。`make compile` 在本机异常（疑似 RTK 代理），改用 `erlc -I include -I deps +strong_validation` 单文件校验，全部无 error；上线前建议跑完整 `make compile` + `make eunit` + `make dialyze`。

### ⚠️ 重大补充发现：#5 的真实范围波及整个 e2ee_transfer 模块（待决策）

核实 #5 移除范围时发现，服务端取明文私钥并非孤例，而是**两个核心端点的共同设计前提**：

1. **social `decrypt_shard`**（`e2ee_social_handler:do_decrypt_shard` 232-284）：服务端取 `get_proxy_private_key`(252) 解密代理分片。
2. **transfer `create_transfer`**（`e2ee_transfer_handler:106-120`）：服务端取发送方明文私钥 `get_sender_private_key`(106)，传入 `e2ee_transfer_logic:create_transfer(..., PrivateKeyPem, ...)`(120) 做设备间私钥中转。

**即整个 e2ee_transfer 设备间传输模块的核心建立在"服务端能拿到用户明文私钥"这一违反零知识的前提上。** 目前两端点均因 `private_key` 列缺失而永久失败（`get_private_key`/`get_sender_private_key`/`get_proxy_private_key` 链路返回 `{error}`），"歪打正着"维持了零信任，但代码意图与零信任架构直接冲突。

本次仅完成无副作用的底层清理（`get_public_by_uid` 去 `private_key` 列），**未删除** `get_private_key`/`get_sender_private_key`/`get_proxy_private_key` 及两个端点——因为"彻底移除"等于废除 decrypt_shard 与 create_transfer 两个功能端点，并需将设备间传输/社交恢复的解密重新设计为客户端本地完成（服务端只中转密文）。这是功能级重构 + API 契约变更 + 客户端配套改造，需单独立项决策，不宜在本审计会话内盲目删除。

**建议后续工作**：
- 设计零信任版 transfer：发送方客户端用接收方公钥加密自身私钥后上传密文，服务端只存/转密文，`create_transfer` 不再接收明文 `PrivateKeyPem`。
- 设计零信任版 decrypt_shard：服务端只返回 `encrypted_shard`，由代理客户端用本地私钥解密。
- 完成后统一删除 `user_device_repo:get_private_key/update_private_key`、`user_device_ds` 对应封装、`get_sender_private_key`/`get_proxy_private_key`。
