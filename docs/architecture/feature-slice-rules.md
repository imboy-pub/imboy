# Feature Slice 架构铁律（规范性）

> **状态**: 冻结 v1.5 — 2026-09-14
> **性质**: 规范性（normative）。与 ADR 0007 配套；ADR 记录"为什么"，本文记录"必须怎么做"。
> **适用**: `src/features/`（Feature）、`src/products/`（Product）、`src/plugins/`（Plugin）下所有代码
> **修订流程**: 铁律只增不改；确需变更时新开 ADR 引用本篇，并在附录登记。

---

## 0. 概念模型：三个业务分类 + 一条横切扩展机制

**一句话先记住：Core / Feature / Product 是三种业务归属（粒度不同，回答"这是谁的职责"）；Plugin 不是第四种业务分类，而是横跨 Core 与 Feature 的扩展机制（回答"实现怎么换"）。** 混用会直接产出"Feature 里套 Plugin、Plugin 又像 Feature"的大泥球。

两条**正交**的栈，不要画成一条：

```
业务分类栈                                扩展机制栈
（回答"这是谁的职责"）                    （回答"实现怎么换"）

        Core                              Extension Point / Port / behaviour
         ↓                                              ↑
      Feature                              ┌────────────┴────────────┐
         ↓                                 Built-in                 Plugin
      Product                                        ↓
                                              Registry / 装配
```

落盘位置：

```
src/
├── lib/          Core     —— 稳定基础能力（平台共享，所有单元都依赖它）
│                            Identity / Organization / Authorization / Message / Realtime / Storage
├── features/     Feature  —— 业务能力（可复用）；可声明「扩展点」
├── products/     Product  —— 产品业务组合（选 Core+Feature，承载产品专属业务）
└── plugins/      Plugin   —— 扩展点的可插拔「实现」（横切机制，不是业务分类）
```

依赖方向（**注意 Plugin 挂在扩展点上，不与 Feature/Product 平级竞争**）：

```
                 Core ──────→ 被 Feature / Product / Plugin 依赖（单向，永不反向）
                   ↑
                   │
                Feature ─────────→ 声明 Extension Point（behaviour）
                   ↑                        ↑
                   │                 ┌──────┴──────┐
                Product          Built-in        Plugin
                 Moya            （随核心发布）   （按产品/部署装配）
                                                        ↑
                                                  Registry / 装配
```

### 0.1 四者职责（一句话判据）

| 概念 | 回答的问题 | 判据 |
|---|---|---|
| **Core** | 我要提供稳定的平台能力？ | 由平台职责与稳定性决定，不按当前消费者数量决定 → `src/lib/` |
| **Feature** | 我要提供**一种业务能力**？ | 会被第二个产品复用 → `src/features/<bc>/` |
| **Product** | 我要做**一个具体业务产品**？ | 只服务当前产品 → `src/products/<product>/` |
| **Plugin** | 我要支持**一种可替换/可插拔的实现**？ | 是某个**扩展点**的实现、且不随核心默认发布 → `src/plugins/<域>/<impl>/` |

### 0.2 新增代码前的四问口诀（规范性）

```
我要提供一种业务能力？            → Feature
我要支持一种可替换/可插拔实现？    → Plugin
我要做一个具体业务产品？          → Product
我要提供稳定的平台能力？            → Core
都不是？                          → 它可能是「扩展点声明」（behaviour）或装配代码，归声明方
```

**不要靠文件名前缀组织架构**。`moya_*.erl` / `other_*.erl` / `plugin_*.erl` / `feature_*.erl` 这类前缀只是结果，不是分类依据；先答上面四问，再决定落哪。

### 0.3 统一词汇（消除 Port / Plugin / 扩展点 的重叠）

本篇与 PRP 里出现过的「端口（Port）」和本篇的「Plugin」，是**同一机制的两面**，不是两套东西：

| 概念 | 同义词 | 由谁声明 | 仓内实例 |
|---|---|---|---|
| **扩展点 / Extension Point** | Port（端口）、`behaviour` | Core 或 Feature | `payment_gateway`、`imboy_llm`、`cs_dispatch_strategy`、`cs_realtime_port` |
| **内置实现** | Built-in | 随声明方发布 | `payment_wechat_gateway`、`imboy_llm_openai`、`cs_dispatch_round_robin` |
| **Plugin** | 可插拔实现 | 仓内 `src/plugins/` 或外部提供 | 第三方 `stripe` / `openai` / `shopify` 适配器 |
| **Registry / 装配** | Provider Registry、注册表 | Platform 或声明方 | `imboy_llm_registry` + `llm_providers` 表 |

> **关键区分：内置实现与 Plugin 的差别不是机制差别，而是来源与装配差别**——两者实现同一个 behaviour，只是前者随核心发布、后者按产品/部署装配。**不要为了区分它们再造一套机制。**

> **这套机制 imboy 早已在用**（本次实证）：`-behaviour(payment_gateway)` 有 **5 个实现**（`payment_alipay_gateway` / `payment_stripe_gateway` / `payment_wechat_gateway` / `payment_wallet_gateway` / `payment_mock_gateway`），`-behaviour(imboy_llm)` 有 **2 个实现**（`imboy_llm_openai` / `imboy_llm_qianfan`）+ `imboy_llm_registry` + `llm_providers` 表。**本篇是把既有成熟模式正名为 Plugin，而不是引入新机制。**

### 0.4 与既有文档/代码的术语对照

| 本篇 | 既有词汇 | 说明 |
|---|---|---|
| **Core** | Kernel（内核/平台） | 同义 |
| **Feature** | 旧文档中的部分 Plugin/业务 Feature | 代码归属形态；表示可复用 BC，不等同于配置里的 Capability |
| **Product** | `product_id` 轴 | **与 `profile`（交付档位）是两个轴，勿混用**（见 §4.6） |
| **Plugin** | —— | 本篇新正名；**特指"扩展点的可插拔实现"**，不是业务分类 |
| **Capability** | 系统运行规则 | 保留既有含义，如 E2EE/search/audit；不因本 ADR 改名 |
| ~~Plugin（旧业务称呼）~~ | channel/moment/location 等可开关业务功能 | 在本 ADR 的代码归属上属于 Feature；旧称逐步停止使用 |
| `imboy_plugin_*` | Plugin Runtime | 动态装载运行时，ADR 0003 已冻结，本架构不启用 |

---

## 1. 目录形态（铁律 1、2、9 的载体）

```
src/
├── lib/                     # Core（禁止依赖 features/ products/ plugins/）
├── features/<bc>/           # Feature：可复用业务能力
│   ├── <bc>_facade.erl      #   按需：首次出现跨单元调用时创建
│   ├── <bc>_feature.erl     #   按需：参与开关/裁剪/license/扩展点登记时创建
│   ├── <bc>_sup.erl         #   按需：拥有长期运行进程时创建
│   └── domain/ application/ interfaces/ infrastructure/  # 有对应职责才创建
├── products/<product>/      # Product：产品业务组合 + 产品专属业务
│   ├── <product>_product.erl   # 按需：代码组装不能由 manifest 表达时创建
│   └── domain/ application/ interfaces/ infrastructure/  # 有对应职责才创建
└── plugins/<域>/<impl>/     # Plugin：可插拔实现（实现别处声明的扩展点）
    ├── payment/stripe/
    ├── llm/openai/
    └── storage/minio/
```

- 纵切单元根部**只允许**上述白名单文件，但白名单文件和四层目录均非强制全建；没有职责就不创建。
- 模块名前缀 = 命名空间（Erlang 无语言级命名空间）：`customer_service` → `cs_`，`moya` → `moya_`。
- **铁律 1-7 对 Feature 与 Product 同等适用**（都是纵切单元）；**铁律 8 规定二者区分；铁律 9 规定 Plugin 的定位**。
- **目录不产生编译期约束**（所有 beam 平铺进 `ebin/`）。目录是意图，**门禁才是约束**（见 §3）。

---

## 2. 九条铁律

### 铁律 1 — 纵切边界必须可识别

**MUST**：一个可复用 BC = `src/features/<bc>/` 一棵子树；一个产品的组装与产品专属规则 = `src/products/<product>/` 一棵子树。Product 是组合边界，不等同于单一 BC。
**MUST NOT**：把新业务的 handler/logic/ds/repo 平铺进既有 `src/api|logic|ds|repo/`。

*为什么*：`api/logic/ds/repo` 是**技术分层**，不表达业务边界。`moya` 的 30 个 `moya_*.erl` 平铺进这四个横向目录后，代码结构上无法回答"墨芽业务由哪些文件构成"，也就无法裁剪、无法单独解释、无法算清第二个业务的成本。
*强制*：`arch-check` 校验纵切目录存在且结构合法；新增业务模块落在 `src/<技术层>/` 且带新业务前缀 → 红。

### 铁律 2 — 层名与依赖方向统一，层按需存在

**MUST**：存在相应职责时使用 `domain / application / interfaces / infrastructure` 固定层名并遵守依赖方向。
**MUST NOT**：为满足目录模板创建空层、空模块或无行为转发器；不得自创层名（`service/`、`biz/`、`core/`）规避边界。

*为什么*：固定层名让迁移与评审可机械检查；按需创建则避免一个小用例被强制膨胀成 handler/logic/ds/repo 加三个根模块。
*强制*：`arch-check` 校验已出现目录的白名单和依赖方向，不校验空目录是否齐备。

### 铁律 3 — `<bc>_facade` = 公开 API，只能进 Application

**MUST**：首次出现跨单元同步调用时创建目标单元的 `<bc>_facade`（Product 同理），调用只能经过该 facade；facade 只做参数收敛与委派，只调 `application/`。
**MUST NOT**：facade 引用 `*_repo` / `*_ds`、写 SQL、直调 `infrastructure/`、绕过 application 改状态。

*为什么*：facade 若可绕层直达 repo，它只是"名字好看的转发器"，边界形同不存在。
*强制*：`arch-check` 专项规则——`*_facade.erl` 出现 `_repo`/`_ds`/`elib_pg`/`elib_pg_sql` 即红。

### 铁律 4 — Domain 必须纯净

**MUST**：`domain/` 内模块是**纯函数**——输入数据、输出数据（含领域事件），无副作用、无 I/O。
**MUST NOT**：domain 依赖 HTTP/Cowboy、DB/Repo/DS/`elib_pg`、事件总线、外部客户端（`imboy_syn`/`httpc`/LLM）、以及 `os:timestamp`/`rand` 等隐式环境依赖（时间/随机量必须**由调用方作为参数传入**）。只有纯业务策略 behaviour 可放 domain；realtime/storage/payment/LLM 等 I/O 端口契约放 application，不能仅因声明了 `-callback` 就进入 domain。

*为什么*：这是"DDD 是否真落地"的唯一可证伪判据——**domain 单测必须零 mock**。领域事件按既有设计以返回值表达（`{ok, NewState, [Event]}`），由 application 在事务提交后发布。
*强制*：`arch-check` 对 `domain/` 禁符号表；**测试契约**：`test/**/domain/*_tests.erl` 不得出现 `?WITH_MECKS`。

### 铁律 5 — 单元只依赖 Core；单元 ↔ 单元只经 Public API / Event

**MUST**：Feature → Core（单向）；单元 A → 单元 B **只能**经 `B_facade`，或经领域事件解耦。
**MUST NOT**：跨单元引用对方 domain/application/interfaces/infrastructure 任意非 facade 模块；Core 反向引用任何单元。

*为什么*：Core 反向依赖单元会让"平台"无法脱离具体业务发布；单元间直连会让 BC 退化成"共享内部实现的一个大模块"。
*强制*：`arch-check` 边检查矩阵——先建"模块 → (单元, 层)"索引，再判定每个引用的方向与可见性。

### 铁律 6 — 租户作用域必须显式贯穿，禁止跨 Org 数据访问

**MUST**：每个 tenant-scoped use case 入参携带 `org_id`；对应 repo 函数的第一个业务参数是 `org_id`；对应 SQL 含 org 约束（`org_id = $n`，或经 join 到 org 的等价约束）。登录前流程、应用注册表和其他全局资源必须显式标记为 global-scoped，并单独评审其信任边界。
**MUST NOT**：对 tenant-scoped 资源采用"先查资源、再比对归属"的方式做鉴权；不得以 global-scoped 标记绕过本应存在的租户隔离。

*为什么*：多租户下"越权读别家数据"是最高危缺陷。两步式校验依赖每个调用点都记得比对，漏一处即漏洞；把 org 作为强制参数 + SQL 强制条件，可让漏洞在编译/门禁/测试三层都难以藏身。
*强制*：①`arch-check` 断言 tenant-scoped repo 导出函数 spec/首参含 `org_id`；②其 SQL 文本缺 `org_id` 即红；③**测试契约**：每个 tenant-scoped 资源类型至少有一个跨 Org 负例，必须 403/404 且不泄漏资源存在性。

### 铁律 7 — 并发状态迁移必须具备跨进程/跨节点保护

**MUST**：存在并发写入可能的持久化状态机（会话 `queued → active → closed` 等）每次推进用 **DB 级 CAS**：
`UPDATE ... SET status=$new, ... WHERE id=$1 AND org_id=$2 AND status=$expected` → 仅当**影响行数 = 1** 才判定成功；否则返回 `{error, conflict}`。
**MUST NOT**：依赖 Erlang 单进程串行化保证一致性；**MUST NOT** 采用"先 SELECT 判合法、再无条件 UPDATE"的检查-使用序列。

*为什么*：坐席 `claim` 天然被多坐席并发触发；imboy 支持多节点（`imboy_syn` 集群），单进程串行化跨节点**必然失效**且最难复现。CAS 把并发裁决交给 PostgreSQL。
*强制*：①有并发状态迁移时使用下列模板；②该状态迁移必须有并发负例——两进程竞争时仅一个成功，另一个得 `{error, conflict}`；③不存在持久化状态迁移的单元不强制创建此类测试。
```erlang
%% infrastructure 层：CAS 是唯一合法推进方式
advance(OrgId, SessionId, Expected, Next) ->
    Sql = <<"UPDATE customer_service_session SET status=$1, updated_at=now() "
            "WHERE id=$2 AND org_id=$3 AND status=$4">>,
    case elib_pg:execute(Sql, [Next, SessionId, OrgId, Expected]) of
        {ok, 1}         -> {ok, advanced};
        {ok, 0}         -> {error, conflict};   %% 状态已被他处推进
        {error, Reason} -> {error, {db, Reason}}
    end.
```

### 铁律 8 — 产品/业务入口 ≠ Feature（Feature 与 Product 分离）

**规范条文**：

> 产品/业务入口 ≠ Feature。Feature 表达**可复用**业务能力；Product 表达**产品业务组合**。产品可以依赖 Core/Feature，Core/Feature 不得依赖具体 Product。只有真正产品专属的业务才进入 Product；不要为了复用而提前抽象。

**依赖方向**：

| 方向 | 判定 |
|---|---|
| Product → Core | ✅ 允许 |
| Product → Feature（经 `_facade`） | ✅ 允许 |
| Product A → Product B | ❌ 禁止 |
| Feature → Product | ❌ **禁止**（Feature 不得知道任何具体产品） |
| Core → Product / Core → Feature | ❌ **禁止** |
| Core/Feature 中出现**具体产品名**符号或分支 | ❌ 禁止 |

**归属判据**：会被第二个产品复用 → `src/features/<bc>/`；只服务当前产品 → `src/products/<product>/`；平台通用 → `src/lib/`。

**提升路径（"不要为了复用而提前抽象"的正式化）**：
1. 新业务规则先落 `src/products/<product>/`（**默认放 Product，不放 Feature**）；
2. **当且仅当**第二个产品也需要该能力时，才提升为 `src/features/<bc>/`，并在两个产品 manifest 同时 include；
3. **禁止**单一产品使用时为"将来可能复用"提前抽成 Feature。

*为什么*：提前抽象会产出**既非通用、又因抽象而失真的中间物**，污染 Feature 命名空间；反过来把产品专属业务硬留 Feature 层，会让 Feature 集合变成产品差异的垃圾场。两个方向都必须机器可拦。
*强制*：①`arch-check` 对 `src/lib/**`、`src/features/**` 做**产品名符号扫描**（集合取自 manifests 的 `product_id`），命中即红；②边检查增列 Feature→Product、Product A→Product B 禁止；③**软告警**：某 Feature 仅被单一 product 引用 → 提示"可能应下移 products/"。

### 铁律 9 — Plugin 是「可插拔实现机制」，不与 Feature/Product 同级

**规范条文**：

> Plugin = 能力的可插拔/可扩展**实现机制**，不是业务分类。**Feature/Core 定义扩展点（Extension Point），Plugin 实现扩展点**；Plugin 通常挂在扩展点上，而不是与 Feature 平级竞争。Plugin 不得反向污染 Core，Core/Feature 不得依赖具体 Plugin（只依赖扩展点）。

**展开为可执行规则**：

| 方向 | 判定 |
|---|---|
| Plugin → Core | ✅ 允许（只用基础能力） |
| Plugin → 扩展点契约模块（behaviour） | ✅ 允许（这是它的存在意义） |
| Plugin → Feature/Product 的 domain/application/infrastructure | ❌ **禁止**（只可依赖其**扩展点声明**） |
| Feature/Core → 具体 Plugin 名（`stripe`/`openai`/`minio`…） | ❌ **禁止**（只可依赖扩展点 + Registry 装配） |
| Plugin → Plugin | ❌ 禁止（跨 Plugin 交互经扩展点或 Core） |
| Plugin 内含业务规则（org/session/领域状态机） | ❌ **禁止**——那是 Feature/Product 的职责，说明分类错了 |

**「假 Plugin」边界（本条的静态判据，最关键）**：

Plugin 可以依赖**扩展点契约**，但**不得把 Feature 的普通公共函数误认为扩展点**。反例（表面守 Port、实际偷依赖内部）：

```
custom_dispatch_plugin
        ↓
cs_conversation_app            ← Feature 的普通 application 模块（非扩展点）
        ↓
   内部函数（偷偷调用）
```

正确关系只能是：

```
Plugin
   │
   └──→ Extension Point / Behaviour
                ↑
                │
       Feature / Core 定义契约
```

而**不是**：

```
Plugin ──→ Feature
```

**判据（可静态检查，减少人工判断）**：

> 扩展点模块必须声明 `-callback(...)`，并由所属单元在 `manifest.extension_points` 中显式登记。

Erlang 中 `-callback` 仅用于**定义** behaviour（用 `-behaviour(X)` 是**使用** behaviour），二者语法上不会混淆。但 `-callback` 只能证明模块的语法形态，不能单独证明它是有意开放、承诺稳定的 Plugin 契约；Feature 扩展点还要由 manifest 登记表达。

**Plugin 的 `-behaviour(X)` 目标必须含 `-callback(`；若 X 来自 Feature，还必须登记于该 Feature 的 `manifest.extension_points`。Plugin 不得调用 Feature 的其他模块。** 普通 Core 公共能力允许依赖，不要求都声明 callback；否则日志、配置、HTTP 等基础依赖也会被误杀。

- 这条把"behaviour vs 普通公共函数"变成**语法事实**；
- Feature 新扩展点还**必须**在该单元的 manifest 中登记（`extension_points` 字段，见 `<bc>_feature:manifest/0`），使其成为**有意声明**的契约而非偶然的 behaviour（登记先作软告警，收敛后转硬门）。

**扩展点的声明位置与形态**：
- **形态**：Erlang `behaviour`（纯契约，零依赖）。
- **位置**：Core 的扩展点放其现有归属层；Feature 的纯业务策略扩展点可放 `domain/`（如 `cs_dispatch_strategy.erl`），realtime/storage/payment/LLM 等 I/O 端口契约放 `application/`。目录由契约语义决定，不能仅凭 `-callback` 决定。
- **装配**：由 Registry/配置选择实现——Core 的走 `imboy_llm_registry` 同款（含 `llm_providers` 表）；Feature 的走 `cs_ports:realtime()` 同款（`application:get_env` 默认值 + 可覆盖）。**默认实现必须存在**，保证零配置可运行。

**内置实现 vs Plugin（务必分清，避免"什么都做成 Plugin"）**：

| | 内置实现 | Plugin |
|---|---|---|
| 机制 | 同一个 behaviour | 同一个 behaviour |
| 差别 | **来源与装配**：随核心/Feature 发布，默认启用 | 不随核心默认发布，按产品/部署装配 |
| 位置 | 跟随声明方（`src/lib/` 或 Feature 内） | `src/plugins/<域>/<impl>/` |
| 例子 | `payment_wechat_gateway`、`imboy_llm_openai`、`cs_dispatch_round_robin` | 第三方 `stripe` / `openai` / `shopify` 适配器 |

> **禁止**：为"可插拔"而把**业务能力**做成 Plugin。Customer Service **不是** Plugin，它是 Bounded Context（Feature）——即使它「可启用/禁用」，那也是 Feature Registry 的职责（`id/version/dependencies/enabled/routes/permissions/migrations`），**不代表它要叫 Plugin**。

*为什么*：三者混用的典型退化是 `feature_*` 里塞 `plugin_*`、`plugin_*` 里写业务规则，最终 `moya_*`、`plugin_*`、`cs_*` 三套代码互相调用回到大泥球。把 Plugin 限定为"扩展点的实现"，就能保证：业务边界永远在 Feature/Product，技术可替换性永远在 Plugin，二者不互相渗透。
*强制*：①`arch-check` 对 `src/plugins/**` 禁引用 Feature/Product 内部模块（只允许已登记扩展点与 Core）；②Core/Feature 禁静态引用具体 Plugin 实现模块，配置或注册表中的 provider id 不在此限；③`src/plugins/**` 内出现领域状态机/org 上下文等业务符号 → 红。

### 铁律 10+ — 预留

铁律只增不改。新增铁律需经 ADR 引用本篇并登记附录。

---

## 3. 强制矩阵：规则 → 检查项 → 工具

**未接线的规则视为不存在。** 全部落在 `make arch-check`（扩展 `scripts/check_module_boundaries.sh`，沿用其既有 perl+rg 抽取风格）：

| 铁律 | 检查项 | 实现 |
|---|---|---|
| 1 | 纵切目录存在；新增业务模块不落在既有技术层 | 路径 + 前缀扫描 |
| 2 | 已存在的纵切子目录/根文件属于白名单；不要求空层齐备 | 目录白名单 |
| 3 | `*_facade.erl` 无 `_repo`/`_ds`/`elib_pg*` | 符号禁用 |
| 4 | `domain/**` 无 cowboy/epgsql/elib_pg/imboy_syn/imboy_domain_event/httpc；domain 测试无 `?WITH_MECKS` | 符号禁用 ×2 |
| 5 | 模块→(单元,层) 索引 + 引用边判定；core→单元 反向即红 | 边检查 |
| 6 | tenant-scoped repo 首参含 `org_id`；对应 SQL 含 org 约束 | spec + SQL 断言 |
| 7 | 存在并发状态迁移时使用 CAS 并提供竞争负例 | 评审 + eunit 契约 |
| 8 | `src/lib/**`、`src/features/**` 无具体 `product_id` 符号/分支 | 产品名扫描（集合取自 manifests） |
| 8 | Feature → Product、Product A → Product B | 边检查增列 |
| 8 | Feature 仅被单一 product 引用 | 软告警（非阻断） |
| **9** | `src/plugins/**` 只引用扩展点与 Core，无 Feature/Product 内部模块 | 边检查（plugins 专属可见集） |
| **9** | Plugin 的 `-behaviour(X)` 目标必须含 `-callback(`；Feature 扩展点还须登记于其 manifest | behaviour 语法断言 + manifest 登记 |
| **9** | Core/Feature 不得静态引用具体 Plugin 实现模块 | 模块引用边检查 |
| **9** | `src/plugins/**` 无业务符号（org/session/领域状态机） | 符号禁用 |
| 反模式 | 纵切单元出现疑似通用能力命名（见 §4.5） | 软告警 + 人工归属审查 |

**接线位置（三处，缺一即视为未接线）**：
1. `Makefile` 新目标 `arch-check`，并入既有 `make security-gate`（`Makefile:249-255`）；
2. `lefthook.yml`（已有 `check_migrations.sh` 挂钩）同款追加；
3. `.github/workflows/backend-ci.yml`（已装 ripgrep）加同一步骤。

**验收金丝雀**：接线后故意注入四条违规 —— ①facade 调 repo；②`src/lib/` 出现 `moya_`；③Feature 静态引用具体 Plugin 实现模块；④Plugin 的 `-behaviour(X)` 指向不含 `-callback` 或未登记于 `manifest.extension_points` 的 Feature 模块 —— `make arch-check` **必须全部变红**；任一不变红即接线失败。

---

## 4. 新业务的成本模型（回应「是不是又要一堆 `other_*.erl`」）

### 4.1 诊断：`moya` 的 30 个模块分三类

以 `moya`（墨芽习字教培）为实测样本：

| 类 | 含义 | `moya` 实例（已逐个核实源码） | 应有归宿 |
|---|---|---|---|
| **A 通用能力被冠了业务前缀** | 能力本身通用（或访问通用表/通用路由），只是挂了业务名 | ① `moya_wechat_client`——**已是参数化通用模块**（`jscode2session(AppId, Secret, Code)`），唯一问题是名字带 moya；② `moya_auth_handler`/`moya_auth_logic`（微信小程序登录）——**路由本就是通用前缀** `/api/v1/auth/wechat-mini/login`，却住在业务模块里，且凭证读**全局单例** `wechat_mini_appid`；③ `moya_org_settings_repo`/`_logic`——读写的是**通用表** `organization.settings` | **提升为 Core 并参数化**（访问器通用化 + 键命名空间 `settings.<bc>.*`）；业务侧零重写 |
| **B 产品专属业务** | 只服务墨芽这一个产品的领域规则 | `moya_assignment_*`、`moya_task_*`、`moya_review_*`、`moya_roster_*`、`moya_submission_repo`、`moya_learner_bind_*`、`moya_context_*`、`moya_acl`、`moya_ai_draft_logic`、`moya_ai_worker`，以及**形态已正确**的 `moya_attach_logic`、`moya_error` | 按需落入 **`src/products/moya/`** 的对应层（依铁律 8）；**仅当**该能力被第二个产品复用时才提升为 Feature |
| **C 配置化缺口** | 代码已通用、但配置是单例 | `moya_auth_logic:83-84` 读**单一全局** `wechat_mini_appid`/`wechat_mini_secret` | 配置层升级为**应用注册表（数据行）** + 放开 `product_id` 硬校验 |

> **已有正确先例（勿误当作反模式）**：
> - `moya_attach_logic`：通用 `attach_logic` + `<<"teaching">>` **scope 子句**，经四处最小侵入钩子扩展。这是"**扩展通用能力，而不是 fork 它**"的标准做法。
> - `moya_error`：域内 `reason 原子 → error_code` 映射表，属域内词汇。
> 二者与 A 类的区别：A 类是"**通用能力的实现**被搬进业务模块"；B 类是"**业务规则**正确地挂在通用能力之上"。

### 4.2 结论

> **新业务代码量 = ① 该业务独有领域规则（不可省，且是价值） + ② 被误写成业务前缀的通用能力（必须归 Core，可归零）。**

**实测计数**（`moya` 共 30 个模块）：**A 类 5 个**可归 Core 归零；**B 类 25 个**是真实领域深度。所以**不是**"每个新业务都要 30 个模块"——但**当前确实会**，因为 A 类尚未提升、且凭证/应用注册仍是全局单例。目标态下，新增纵切的代码量应约等于其领域规则体量；凭证、配额、品牌、路由前缀等一律是**数据行**。

### 4.3 前置判据（**先决定"建不建单元、建哪一种"**）

**新前端 ≠ 新 Feature ≠ 新 Product。** 先做这个判断，否则会凭空造出 `other_*.erl`：

| 情形 | 判据 | 该做什么 | 后端代码量 |
|---|---|---|---|
| **A. 新前端 + 已有域** | 业务规则与既有单元相同，只是又一个入口/端 | **不建新单元**。注册应用行 + 复用既有 API；产品维度只在 manifest 表达 | **≈ 0 个模块** |
| **B. 新前端 + 新域（产品专属）** | 存在该产品**独有**的规则、状态机、名词 | 在 **`src/products/<product>/`** 的必要层中实现 | 等于真实领域规则体量，无模块配额 |
| **C. 该新域被第二个产品复用了** | 同一能力出现第二个消费方 | **此时才**提升为 `src/features/<bc>/`，两个产品 manifest 同时 include | 提升即重构，非净增 |
| **D. 似 A 实 B** | 起初像 A，逐渐长出独有规则 | 先按 A；出现独有规则后转 B | 转 B 时才建单元 |

> **`other_*.erl` 爆炸的成因，是把情形 A 当成情形 B 做；Feature 污染的成因，是把情形 B 当成情形 C 做（提前抽象）。** 二者分别由铁律 8 的两个方向拦下。

### 4.4 接入检查表（情形 B 的 6 步，含验收）

1. **注册应用行**（appid/secret/platform/branding/entitlements）→ 数据，不写模块；
2. **复用 Core 能力**：登录、附件、消息/WS、AI provider 接线、org settings → **零模块**；需替换实现时，加一个实现既有扩展点的 Plugin，不改调用方；
3. 在 `src/products/<product>/`（或情形 C 的 `src/features/<bc>/`）中**只创建实际需要的层与模块**：domain 放纯业务规则、application 放用例、interfaces 放协议适配、infrastructure 放 I/O。
   *自查*：若你在重复写微信客户端、通用登录或 organization settings 访问器，判据可能用错了，或该能力应先归 Core；
4. 有路由、权限、cron、license、裁剪或扩展点登记需求时才声明 manifest；纯数据配置不生成模块；
5. **跨单元只经 facade 或领域事件**（铁律 5/8/9）；
6. **过门禁**：`make arch-check`；涉及 tenant-scoped 资源时加跨 Org 负例，涉及并发状态迁移时加竞争负例。

### 4.5 反模式告警（机器筛选、人工定性）

纵切单元出现下列命名模式时产生软告警，提示审查它是否属于 Core；名称只能筛选嫌疑，不能证明归属，业务特有的认证流程允许保留：

```
{features,products}/*/**/{*_auth_handler,*_auth_logic,*_wechat_client,*_sms_client,*_org_settings_repo,*_org_settings_logic}.erl
```

判定口径（避免误伤）：禁的是"**通用能力的实现**被写进纵切单元"；**不**禁止"**业务规则挂在通用能力之上**"——故 `*_attach_logic`（scope 子句扩展）、`*_error`（域内错误码段）等**合法**。若某业务模块确需扩展语，应走"改 Core 加 scope 子句"，而不是新建 `<bc>_attach_*`。

### 4.6 两轴辨析（Product 轴与交付档位轴，勿混用）

| 轴 | 字段 | 含义 | 今天的状态 |
|---|---|---|---|
| **产品** | `product_id` | 产品业务组合（`imboy` / `moya` / …） | **生成器硬校验 == `imboy`**（`scripts/generate_product_features.py:108`）；放开该校验 + 新增 manifest 即可支持第二产品（生成宏不含产品名，机制天然产品无关） |
| **交付档位** | `profile` | 同一产品内的交付预设（`community`/`enterprise`/`overseas_baseline`/`agent_hub`/`full-selected`），决定 `selected_features` | 现存唯一变体轴，机制已就绪 |

---

## 5. 与其他文档的关系

| 文档 | 关系 |
|---|---|
| ADR 0007 | 本文的决策依据（"为什么"） |
| ADR 0005 / 0001 / 0006 | 上位决策；本文是其**落地形态**，不冲突 |
| ADR 0003 | `imboy_plugin_*` 动态插件平台（**Plugin Runtime**）已冻结；本文 §0.4 已辨析"Runtime vs Plugin" |
| `docs/architecture/module-layer-cheatsheet.md` | 旧术语来源；其中“Plugin=可开关业务功能”在代码归属上由本文的 Feature 取代，Capability 含义保留 |
| `docs/explanation/product-profile-and-plugin-registry-design.md` | `product_profile` / `profile` 定义来源；§4.6 两轴辨析以其为准 |
| `config/product-feature-manifest.json` + `scripts/generate_product_features.py` | Product 轴**现成机制**（manifest → 编译期宏 → 裁剪） |
| `docs/architecture/database-access.md` | 铁律 6/7 在 SQL 层的展开 |
| `docs/plans/2026-09-13-customer-service-prp-v2.md` | 首个实施样本（本地工作稿，gitignored） |
| `docs/standards/feature-slice-checklist.md` | **待产出**：首个样本收官时沉淀的"新增纵切单元检查表" |

---

## 附录：冻结登记

| 版本 | 日期 | 变更 |
|---|---|---|
| v1.0 | 2026-09-14 | 首次冻结，7 条铁律 + 强制矩阵 + 新业务成本模型 |
| v1.1 | 2026-09-14 | §4.3 补前置判据（新前端 ≠ 新 BC 的分流表） |
| v1.2 | 2026-09-14 | 新增铁律 8（Product ≠ Feature）；§0 增 product_id/profile 两轴辨析；§1 增 `src/products/` |
| v1.3 | 2026-09-14 | **§0 重构为四层概念模型**（Core/Feature/Product/Plugin + 四问口诀 + Port≡扩展点 统一词汇 + Runtime/Plugin 辨析）；**新增铁律 9（Plugin = 可插拔实现机制）**；§1 增 `src/plugins/`；§3 增铁律 9 三项检查与三金丝雀；§4.4 第 2 步补"需替换实现时加 Plugin" |
| v1.4 | 2026-09-14 | §0 改述为"**三个业务分类 + 一条横切扩展机制**"（Plugin 非第四种分类，两条栈正交）；铁律 9 补**「假 Plugin」边界**：Plugin 只可依赖**含 `-callback` 的模块**（扩展点），不得依赖 Feature 普通公共函数——判据由主观变**语法事实**；§3 增该检查行与第 4 条金丝雀 |
| v1.5 | 2026-09-14 | 四层与 facade/manifest/sup 改为按需创建；租户与 CAS 规则改为条件约束；`-callback` 明确为必要非充分条件并结合 manifest 登记；澄清既有 Capability/Plugin 旧称 |
