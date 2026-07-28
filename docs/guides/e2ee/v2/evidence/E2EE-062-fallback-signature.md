# E2EE-062 残留 6：fallback prekey 服务端验签（第一阶段）

- **Slice**：E2EE-062 残留 6「fallback prekey 未在服务端验签」
- **会话**：`20260729-1200-claude-code`
- **仓库**：`imboy`
- **状态**：**第一阶段完成**（能验、缺口可见）；**本项未关闭**——签名仍非必填（§1.3）。
  E2EE-062 整体仍为 `PARTIAL`

---

## 1. 做了什么

playbook E2EE-025 验收标准：

> OTK 耗尽只使用协议允许且**身份验证通过的 signed fallback prekey**，或拒发。

现状 `report_fallback_key/4` 只校验 `key_id` / `key_base64` 非空，**没有任何签名**。

### 1.1 威胁：这不是理论问题

E2EE-013 用 token 绑定设备所有权——只有持设备 D 的 token 才能写 D 的密钥。
但 **token 在网络上传输，identity 私钥不会**：盗 token 远比盗设备 ed25519 私钥容易。

持有被盗 token 的攻击者**今天**可以给 D 上传**自己控制的** fallback prekey。
此后凡是 D 的 OTK 耗尽、对端回退 fallback 的会话——也就是第七/八刀一直在防的
那个耗尽场景——用的都是**攻击者的预密钥**。

要求 fallback key 由**设备已注册的 ed25519 身份键**签名，就把它绑到了一个
token 窃取者拿不到的秘密上。

**认识论状态：`report_fallback_key/4` 无签名校验为已实证（逐行）；
"token 比 identity 私钥更易被盗"是威胁模型推理，非本项目实测。**

| 接缝 | 改动 |
|---|---|
| `src/logic/olm_identity_logic.erl` | 新增 `report_fallback_key/5`：canonical 单射守卫 → 查已注册 ed25519 → 验签 → 通过才落库 |
| `src/api/olm_handler.erl` | `do_report_fallback1` 读可选 `signature`，统一走 `/5` |
| `Makefile` | e2ee-verify Modules 清单 +1 |

无迁移、无新依赖、无协议破坏（`signature` 是**可选**字段）。

### 1.2 canonical 载荷与单射守卫

签名对象是 `key=value\n`、ASCII 字典序、末字段无尾随换行：

```
device_id=<did>\n key_base64=<b64>\n key_id=<kid>\n user_id=<uid>
```

与 `e2ee_trust_logic:canonical_payload/1` 同一方案（项目既有、双语言对齐），
也与上一轮冻结的 KT profile §3 一致——**不发明第三套编码**。

值内含 `\n`/`\r` 会让编码**非单射**（同一串字节可对应多组字段拆分 = 签名伪造），
故 `no_ctrl_chars/1` fail-closed 拒收。

### 1.3 ⚠️ 取舍：签名为空时**仍然接受**（并计数）——本项因此未关闭

今天**没有任何客户端发送签名**。此刻要求必填 = 所有设备都发布不了 fallback key
→ 每次 OTK 耗尽都变成 `no_prekey_available` → **新会话直接建不起来**。

因此走两阶段，与第四刀（客户端发 `request_id`）**同一形状**：

| 阶段 | 内容 | 状态 |
|---|---|---|
| 一 | 服务端能验；带签名的一律严格校验；未签名的接受但**计数** | ✅ 本刀 |
| 二 | 客户端普遍带上签名后改为**必填** | ❌ 未做 |

**在第二阶段完成前，被盗 token 的攻击者仍可通过"干脆不带签名"绕过整道校验。
本项不算关闭。** 新指标 `olm_fallback_unsigned_total` 就是用来看第二阶段何时可以启动的。

### 1.4 取舍：复制 8 行验签原语，而非依赖既有模块

两个候选都不合适：

- `e2ee_trust_logic:verify_signature/3` —— **私有函数**，导出它会造成 logic→logic 耦合；
- `imboy_plugin_signature` —— 模块头标注 **`@status FROZEN`**（v2 动态加载子系统暂停），
  从活跃 E2EE 路径依赖一个冻结模块是更差的选择。

故在 `olm_identity_logic` 内复制 `crypto:verify(eddsa, none, ...)` 这 8 行，
并在注释里**互相注明位置**，日后要合并两处都可检索到。
安全原语重复有漂移风险，但耦合冻结模块 / 跨 logic 依赖的代价更大。

### 1.5 计数为什么必须打在 `/5` 而不是 `/4`

`/5` 验签成功后**内部复用 `/4`** 落库。若把 `olm_fallback_unsigned_total` 打在 `/4`，
**签名合法的上传会被误计成"未签名"**，指标随即失去意义。
故计数打在 `/5` 的 `<<>>` 子句，handler 统一走 `/5`。
守护用例：`signed_not_counted_as_unsigned`。

已 grep 核实**全仓无任何测试按 arity mock `report_fallback_key`**
（`olm_otk_lifecycle_tests` 是直接调用 `/4`，不是 mock），
故 handler 统一调 `/5` 不会造成静默穿透。`/4` 本体一字未改。

---

## 2. RED 记录

新增 `test/logic/e2ee_fallback_signature_tests.erl`（7 例）。
**用 `crypto:generate_key(eddsa, ed25519)` 生成真实密钥对并真实签名，
不 mock 任何密码学函数。**

### 2.1 第一次不是合格的 RED

首跑 `Failed: 5, Passed: 1`——但走 `/5` 的用例失败于 **`undef`**（函数不存在），
是**编译级失败**，不是行为失败。按铁律先落**载体**（`/5` 原样委托 `/4`、
忽略签名，无新语义），再取 RED：

```
=======================================================
  Failed: 4.  Skipped: 0.  Passed: 2.
```

**4 红均为行为失败**：无效签名被接受、换 key 复用签名被接受、
设备未注册仍被接受、未签名上传无计数。

### 2.2 对照组与正向可用性

- **对照组** `unsigned_still_accepted`：未带签名的旧客户端仍照常落库。
  改前改后**都绿**——它红就说明我把所有设备的 fallback 发布能力一起砍掉了。
- **正向可用性** `valid_signature_accepted`：带**有效**签名照常落库。
  一个「一律拒绝」的实现在"能拦下伪造"上恒得满分，被这条否掉。
  （该用例在载体阶段是空绿，实现后才真正有意义。）
- **反重放** `signature_binds_key`：拿到一次合法签名后换一把自己控制的 key
  复用它必须失效。
- **不得"验不了就放行"** `unregistered_device_rejected`：设备未注册 identity 时拒绝。
  若放行，攻击者只需先让 identity 查不到即可绕开整道验签。

---

## 3. 生产调用方核实

```
POST /api/v1/e2ee/olm/fallback_key
  → olm_handler:report_fallback → do_report_fallback → do_report_fallback1
  → olm_identity_logic:report_fallback_key/5     ← 本刀
  → （验签通过）report_fallback_key/4 → olm_identity_ds:upsert_fallback_key
```

`do_report_fallback` 前置已有 `ensure_device_owner`（E2EE-013），本刀在其之后加签名层。

---

## 4. 验收命令与结果

```
$ IMBOYENV=local make eunit t=e2ee_fallback_signature_tests
  All 7 tests passed.

$ make e2ee-verify
  All 350 tests passed.        # 上一轮基线 343，本刀 +7
=== E2EE verify ALL PASSED ===

$ erlfmt --check src/logic/olm_identity_logic.erl src/api/olm_handler.erl \
    test/logic/e2ee_fallback_signature_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

既有 `olm_otk_lifecycle_tests`（直接调 `/4` 三处）在门禁内**全绿**
→ `/4` 语义未被改变。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **⚠️ 本项未关闭：签名仍非必填**（§1.3）。被盗 token 的攻击者可以"干脆不带签名"
   绕过整道校验。第二阶段（客户端签名 + 服务端改必填）未做。
2. **客户端不发送签名** —— 与第四刀 `request_id` 同样的形状：服务端已就绪，
   生产流量走不到。需 imboyapp 侧一刀。
3. **未做 identity 上报本身的验签** —— `report_identity` 的 `signature` 字段只校验
   非空，未验证。**认识论状态：已实证（逐行读 `report_identity/6`）。**
   但注意它与本刀不同：identity 自签只能证明内部一致，无法证明所有权连续性，
   真正的防护在客户端 TOFU 与 KT（E2EE-065）。
4. 告警规则未做（上一刀残留）；`/metrics` 输出未实证；
5. 被拦下的重发行仍被扫描器每轮捡起（不写库、不出网）；滞留后 UX 无具体提示；
6. 幂等/补传链路端到端未实证；单租户/全局限流未做；租约无独立 TTL；
   60/min 未压测；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
7. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| `report_fallback_key/4` 原本无任何签名校验 | **已实证**（逐行） |
| 带有效签名 → 落库成功（正向可用性） | **已实证**（真实 Ed25519 签名） |
| 带无效签名 → 拒绝且不落库 | **已实证** |
| 换 key 复用同一签名 → 失效 | **已实证** |
| 设备未注册 identity → 拒绝 | **已实证** |
| 未签名上传被计数、且有效签名不被误计 | **已实证** |
| `/4` 语义未被改变 | **已实证**（`olm_otk_lifecycle_tests` 全绿） |
| 全仓无测试按 arity mock `report_fallback_key` | **已实证**（grep） |
| 「token 比 identity 私钥更易被盗」 | **威胁模型推理，非本项目实测** |
| 「fallback prekey 已受身份验证保护」 | **不成立** —— 签名非必填（§5.1） |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范（`signature` 为**可选**字段，旧客户端零破坏）。
- 未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未把签名改为必填**（§1.3）；未做客户端侧签名。
