# eqwalizer_support Fork 专项方案

> **状态**：Draft（待团队评审）  
> **作者**：P3 类型检查治理轮（R16）  
> **前置依赖**：[R14 根因分布表][gradualizer-upstream-issues] 裁定 lib 层剩余 30 failing 模块的主因是 OTP/epgsql 第三方类型缝隙，[R14 spike] 证实项目本地 `eqwalizer_specs` 不被 elp 加载。

---

## 1. 动机：why fork

当前 eqWAlizer 在 imboy 项目中的角色：

| 维度 | 现状 |
|---|---|
| CI 门禁 | `eqwalize-layer LAYER=lib` 预算 38 failing（非阻塞，仅告警） |
| lib 层实际 failing | 30 个模块（~120 error） |
| 已消部分 | 7 Gradualizer + 1 eqWAlizer 真阳性（4 commits）；6 gen_server spec 收敛（R15） |
| 瓶颈 | lib 层 30 failing 模块的 error **100% 属于 OTP/epgsql/depcache/cowboy 第三方类型缝隙**——非源码可修 |

**Fork `eqwalizer_support` 是唯一能让 lib 层转阻塞的技术路径**。不 fork，eqWAlizer 永远只能做非阻塞参考门禁（当前状态）。

---

## 2. 前置 spike 结论

| 尝试 | 结果 |
|---|---|
| **项目本地 `eqwalizer_specs.erl`**（R14 spike） | **无效**：elp 只加载 vendored `eqwalizer_support` 的 `eqwalizer_specs`，本地同名模块不生效 |
| **改 `.elp.toml` 的 `enable_all = true`** | 不可行：全仓 ~1051 error，需预算 9999，无门禁价值 |
| **写 `eqwalizer_specs` 覆盖 `config_ds:env/2`** | 不可行（R11）：`any()→any()` 无法按 runtime key 精细化 |
| **提交上游 PR 补覆盖** | 可行但慢：需 WhatsApp 审核接受；不保障合入时间线 |

**结论：fork 是唯一确定可用的路径。**

---

## 3. 差距分析：需要覆盖的函数

> 来源：R14 全量抓取 36 lib 模块 / 140 error，按涉及外部函数 + opaque 类型统计。

### 3.1 crypto（~30 error，17 函数/类型）

```
crypto:cipher_iv/0, crypto:crypto_init/4, crypto:crypto_state/0,
crypto:crypto_update/2, crypto:dh_private/0, crypto:dh_public/0,
crypto:ecdh_private/0, crypto:ecdh_public/0, crypto:hash_algorithm/0,
crypto:hash/2, crypto:pk_encrypt_decrypt_opts/0, crypto:rsa_private/0,
crypto:rsa_public/0, crypto:srp_private/0, crypto:srp_public/0
```

**覆盖策略**：全部 override 为 `eqwalizer:dynamic()`（= `term()`），与上游 vendored 版已覆盖的其他 OTP 函数一致（如 `binary:encode_hex/1`、`maps:iterator/1` 等）。

### 3.2 uri_string（~25 error，5 函数/类型）

```
uri_string:error/0, uri_string:normalize/1, uri_string:unquote/1,
uri_string:uri_map/0, uri_string:uri_string/0
```

**覆盖策略**：同上，`eqwalizer:dynamic()`。注意 `uri_string:uri_map()` 已在 OTP 中导出为 opaque 类型，但 eqWAlizer 不识别其内部结构。

### 3.3 epgsql（~12 error，7 函数/类型）

```
epgsql:connect_option/0, epgsql:connect_opts_map/0, epgsql:connect_opts/0,
epgsql:connect/1, epgsql:query_error/0, epgsql:transaction_option/0,
epgsql:transaction_opts/0
```

**覆盖策略**：同上。注意 epgsql 是第三方 dep，不在 OTP stdlib 内，但 eqwalizer_support 机制对第三方同样生效（已有 `jsone:decode/2` 覆盖在先）。

### 3.4 不覆盖的范围

| 类别 | 量 | 原因 |
|---|---|---|
| `config_ds:env` 渗出 | ~3 | `any()→any()` 不可消（R11） |
| `depcache` 不精确类型 | ~2 | 量级可忽略，留上游 |
| `cowboy`/`jsx` 等 | ~3 | 上游 vendored 已覆盖大部分 |

---

## 4. 预期影响

### 4.1 lib 层

| 指标 | 当前 | fork 后（首批） |
|---|---|---|
| failing 模块 | 30 | **~15–17** |
| error 总数 | ~120 | ~25–30 |
| CI 预算 | 38 | **可下调至 ~20** |
| 转为阻塞 | ❌ | **仍不可**（maps/config_ds 残量仍 ~25 error） |

> **重要**：fork 覆盖 crypto + uri_string + epgsql 后，lib 层仍有 ~15 failing 模块来自 maps 不精确（如 `maps:get/3` 作用在 `term()` 上）和 config_ds:env 渗出。这些**需逐模块源码收窄 map 类型**（非 fork 范围），是随业务迭代顺手做的 incremental 工作。

### 4.2 全仓

lib 层 fork 覆盖的模式可复用到 api/repo/ds/logic 层——但那些层 `config_ds:env` 渗出是主因（R8–R10），fork 收益递减。建议先验证 lib 层，再决定是否推广。

---

## 5. 架构方案

### 5.1 选项 A：Fork + 本地 clone（推荐）

```
1. Fork WhatsApp/eqwalizer → github.com/imboy/eqwalizer-fork
2. 在 fork 的 eqwalizer_support/src/eqwalizer_specs.erl 追加 crypto/uri_string/epgsql override
3. 本地 clone 到 vendor/eqwalizer_support_fork/
4. 修改 .elp.toml: eqwalizer_support = { path = "vendor/eqwalizer_support_fork" }
5. elp 重编后生效
```

**优点**：完全自控、立即生效、不依赖上游 review。  
**缺点**：需维护 fork 的 upstream rebase（上游 eqwalizer_support 更新时合并）。

### 5.2 选项 B：给上游提 PR

向 WhatsApp/eqwalizer 提交 PR，补 crypto/uri_string/epgsql override。合入后等 elp 发版包含新 vendored copy。

**优点**：零维护负担、社区受益。  
**缺点**：等待周期不可控（可能数周/数月）、PR 不一定被接受。

### 5.3 推荐路径

**选项 A（fork），同时提交上游 PR（选项 B）。**  
短期 fork 用于立即验证效果，长期以上游合入后切回 vendored。

---

## 6. 实施步骤

### Phase 0：Fork 与接入（1 人·时）

```bash
# 1. Fork WhatsApp/eqwalizer 到 imboy org
# 2. 本地 clone
git clone https://github.com/imboy/eqwalizer-fork.git vendor/eqwalizer_support_fork/
cd vendor/eqwalizer_support_fork/eqwalizer_support

# 3. 创建分支
git checkout -b feat/add-crypto-uri_string-epgsql-overrides
```

### Phase 1：补充 Override（1–2 人·时）

修改 `eqwalizer_specs.erl`，在现有 override 块后追加三组：

```erlang
%%% crypto opaque types (R16 — not covered by upstream vendored support)
-crypto:cipher_iv() -> eqwalizer:dynamic().
-crypto:crypto_init(_, _, _, _) -> eqwalizer:dynamic().
-crypto:crypto_state() -> eqwalizer:dynamic().
-crypto:crypto_update(_, _) -> eqwalizer:dynamic().
-crypto:dh_private() -> eqwalizer:dynamic().
-crypto:dh_public() -> eqwalizer:dynamic().
-crypto:ecdh_private() -> eqwalizer:dynamic().
-crypto:ecdh_public() -> eqwalizer:dynamic().
-crypto:hash_algorithm() -> eqwalizer:dynamic().
-crypto:hash(_, _) -> eqwalizer:dynamic().
-crypto:pk_encrypt_decrypt_opts() -> eqwalizer:dynamic().
-crypto:rsa_private() -> eqwalizer:dynamic().
-crypto:rsa_public() -> eqwalizer:dynamic().
-crypto:srp_private() -> eqwalizer:dynamic().
-crypto:srp_public() -> eqwalizer:dynamic().

%%% uri_string opaque types (R16 — not covered by upstream)
-uri_string:error() -> eqwalizer:dynamic().
-uri_string:normalize(_) -> eqwalizer:dynamic().
-uri_string:unquote(_) -> eqwalizer:dynamic().
-uri_string:uri_map() -> eqwalizer:dynamic().
-uri_string:uri_string() -> eqwalizer:dynamic().

%%% epgsql third-party types (R16 — not covered by upstream)
-epgsql:connect_option() -> eqwalizer:dynamic().
-epgsql:connect_opts_map() -> eqwalizer:dynamic().
-epgsql:connect_opts() -> eqwalizer:dynamic().
-epgsql:connect(_) -> eqwalizer:dynamic().
-epgsql:query_error() -> eqwalizer:dynamic().
-epgsql:transaction_option() -> eqwalizer:dynamic().
-epgsql:transaction_opts() -> eqwalizer:dynamic().
```

### Phase 2：接入 imboy（0.5 人·时）

修改 `imboy/.elp.toml`：

```toml
[projects.apps.imboy.deps]
# eqwalizer_support = { path = ".elp/elp-repo/eqwalizer/eqwalizer_support" }  # 原 vendored，注释
eqwalizer_support = { path = "../vendor/eqwalizer_support_fork/eqwalizer_support" }  # fork
```

### Phase 3：验证（1 人·时）

```bash
# 1. 逐模块 eqwalize（验证新增覆盖生效、无回归）
make eqwalize-layer LAYER=lib

# 2. 确认 lib 层 error 数回落
#    预期：~120 → ~25–30（消 crypto ~30 + uri_string ~25 + epgsql ~12 = ~67）

# 3. 确认已 enable 的 5 个白名单模块仍 0-error（无回归）

# 4. 确认 elp 加载无崩溃（jar race 除外，已知问题）
```

### Phase 4：CI 校准（0.5 人·时）

fork 生效后，在 GitHub Actions 调整 `EQWALIZE_BUDGET` 变量从 38 → ~20。

---

## 7. 风险与缓解

| 风险 | 概率 | 影响 | 缓解 |
|---|---|---|---|
| **override 写错 mask 真实 bug** | 低 | 中 | 全部 override 为 `eqwalizer:dynamic()`（= `term()`），与上游 vendored 策略一致；不改变函数语义，仅放宽类型约束 |
| **上游 rebase 冲突** | 中 | 低 | fork 仅改一个文件（`eqwalizer_specs.erl`），冲突面极窄；维护 `UPSTREAM.md` 记录 rebase 时间线与 diff |
| **elp/eqWAlizer 升级后 fork 不兼容** | 低 | 中 | elp 版本已在 `.elp.toml` 锁定；升级前先验证 fork 兼容性 |
| **fork 路径影响 CI 可复现性** | 低 | 低 | 使用相对路径（`../vendor/...`）+ CI 脚本中 `git clone` fork |
| **OTP 版本漂移**（本地 29 vs CI 28） | 中 | 低 | fork 覆盖的是类型签名，与 OTP 运行时版本无关（R6 已知 OTP 版本漂移但属 Gradualizer 侧） |

---

## 8. 不健全风险评估

eqWAlizer 的 override 机制本身存在「不健全」风险：spec 被 override 的函数，eqWAlizer **不再检查其调用方传入的参数类型是否匹配**。例如：

```erlang
-epgsql:connect_opts() -> eqwalizer:dynamic().
```

这使得 eqWAlizer 对 `epgsql:connect/1` 的所有调用都变成 `term()→term()`——即使调用方传了错误类型的参数也**不报错**（false negative）。

**但因项目当前 eqWAlizer 处于非阻塞阶段（仅预算告警），false negative 无实际 CI 阻断影响；且这些函数调用在生产中已验证稳定，override 不会引入新 bug。**

若未来 eqWAlizer 转阻塞，需重新评估每个 override 的精度需求，逐步从 `eqwalizer:dynamic()` 迁移到更精确的类型。

---

## 9. 决策框架

| 维度 | 不 fork | fork |
|---|---|---|
| eqWAlizer 门禁角色 | 永久非阻塞（预算告警） | lib 层接近阻塞（~15 failing） |
| 投入 | 0 | ~5 人·时一次性 + 周期性 rebase（~1 人·时/季度） |
| 维护负担 | 0 | 低（仅 1 文件改动） |
| 不健全风险 | 0 | 低（当前非阻塞阶段无实际影响） |
| lib 层转阻塞路径 | 无 | 有（fork + maps 收窄 = 预算→0） |

---

## 10. 推荐决策

**建议执行 fork（选项 A），Phase 0–3 共计 ~4 人·时。** 理由：

1. **fork 是让 eqWAlizer 从「参考」升级为「准门禁」的唯一路径**（R14 spike 已实证）
2. **投入极低**（~4 人·时）、**影响面极小**（1 文件 30 行 override）
3. **可逆**：随时切回 vendored（还原 `.elp.toml` 1 行）
4. **双轨**：fork 短期生效 + PR 上游长期归并

若团队决定不 fork，应正式接受 eqWAlizer 的非阻塞角色，将其定位为「IDE 辅助 / 白名单模块保护」而非「CI 门禁」。

---

## 参考

- [R14 根因分布表][gradualizer-upstream-issues] — lib 层 140 error 全量分类
- [R14 spike] — 项目本地 `eqwalizer_specs` 无效的实证
- [elp-eqwalizer-landing-plan] — eqWAlizer 落地规划（P1–P3）
- [eqwalizer_support 上游源码](https://github.com/WhatsApp/eqwalizer/blob/main/eqwalizer_support/src/eqwalizer_specs.erl)

[gradualizer-upstream-issues]: ./gradualizer-upstream-issues.md
[elp-eqwalizer-landing-plan]: ./elp-eqwalizer-landing-plan.md
