# ADR: Feature Slice 架构 —— 业务功能按限界上下文纵切，清晰区分 Plugin / Feature / Product

- Status: Accepted
- Date: 2026-09-14
- 关联：ADR 0001（四层单向依赖）、ADR 0005（模块化单体边界）、ADR 0006（DDD 迁移端点线）、ADR 0003（插件路由命名空间 / `imboy_plugin_*` 冻结）
- 规范文本：`docs/architecture/feature-slice-rules.md`（9 条铁律，规范性）
- 首个实施样本：客服系统（概念见 `docs/concepts/customer-service.md`）

## Context

`ADR 0005` 已决定"继续模块化单体、以仓内模块与公开入口组织领域边界"，但未规定**目录形态**与**强制手段**，导致三个具体问题：

1. **新业务只能继续横向铺文件**。`moya`（墨芽习字教培）落地时新增了 30 个 `moya_*.erl`，全部平铺进既有 `src/api/`、`src/logic/`、`src/ds/`、`src/repo/`。这使"一个业务"无法在代码结构上被识别，第二个业务（及之后的 `other_*`）只能重复同一模式，且其中混入了本属平台通用的能力（`moya_wechat_client` 已是参数化通用模块、`moya_org_settings_repo` 读写的是通用 `organization.settings`、微信小程序登录挂在通用路由 `/api/v1/auth/wechat-mini/login` 上却叫 `moya_auth_handler`）。
2. **DDD 只停留在名义**。仓库已有 `src/domain/`（聚合/值对象/策略），但 domain 与 `api/logic/ds/repo` 是**并列的横向目录**，而非"某业务内部的纵向切层"。若客服照此实现，得到的将是"加了几个 domain 模块的旧四层"。
3. **三个概念未被分开：Plugin / Feature / Product**。这会在多产品承载时直接退化为大泥球——典型症状是 `feature_*` 里塞 `plugin_*`、`plugin_*` 里写业务规则、`moya_*`/`plugin_*`/`cs_*` 三套代码互相调用。具体表现：
   - **产品与可复用能力未分层**："只有真正产品专属的业务才进入 Product"在代码上无处表达；产品维度今天只在 `config/product-feature-manifest.json` 的 `product_id` 上，且生成器**硬校验 `product_id == "imboy"`**（`scripts/generate_product_features.py:108`），尚无第二个产品的位置。
   - **Plugin 与业务能力未分层**：仓库里"扩展点 + 多实现"其实**早已成型且运行良好**，但没有正名。本次实证：`-behaviour(payment_gateway)` 有 **5 个实现**（`payment_alipay_gateway` / `payment_stripe_gateway` / `payment_wechat_gateway` / `payment_wallet_gateway` / `payment_mock_gateway`），`-behaviour(imboy_llm)` 有 **2 个实现**（`imboy_llm_openai` / `imboy_llm_qianfan`）+ `imboy_llm_registry` + `llm_providers` 表。**"可插拔实现"这套机制不需要发明，只需要正名为 Plugin 并限定其边界。**
   - 术语上还叠着第三个混淆：`imboy_plugin_*`（动态装载**运行时**，ADR 0003 已冻结）与"Plugin 实现"同名不同物。

同时，落地 Feature/Product/Plugin 目录需要先证明工程可行性，本次已实证：

- `erlang.mk:1438` `ALL_SRC_FILES := $(sort $(call core_find,src/,*))`，`core_find`（`erlang.mk:189`）对任意深度目录**完全递归** → `src/features/**`、`src/products/**`、`src/plugins/**` **零构建改动**可编译；测试同理（`erlang.mk:2001`）。
- 但源根**硬编码为 `src/`**，且 `erlang.mk` 为 vendored 冻结（`AGENTS.md` 禁止修改）→ 纵切目录必须置于 `src/` 之下。
- **所有 beam 平铺进 `ebin/`**：目录结构**不产生任何编译期约束**（Erlang 模块是全局原子）。因此目录若要成为架构而非装饰，**强制必须来自门禁脚本**。
- 生成物宏为 `IMBOY_FEATURE_<FEATURE>` 与 `IMBOY_PRODUCT_FEATURE_*`，**不含具体产品名** → 编译期裁剪机制天然产品无关，支持多产品只需放开上述校验与新增 manifest。
- 现存两处门禁盲区（须随本 ADR 修复）：`Makefile:98` 的 `ERLC_EXCLUDE_PATHS` 只 glob 到 3 层、gradualizer（`Makefile:394/399`）只扫 2 层——深层纵切代码会**逃过编译期物理裁剪与类型门**。

## Decision

采用 **Feature Slice**：业务功能按限界上下文纵切，并把 **Core / Feature / Product** 定位为三个业务归属层级，把 **Plugin** 定位为横切的装配/扩展机制。

1. 目录形态：
   - Core：`src/lib/`（既有，不动）；
   - Feature：`src/features/<bc>/`，代码按需落入 `domain/application/interfaces/infrastructure`；只有出现相应职责时才创建该层；
   - Product：`src/products/<product>/`，遵守同一分层方向，只承载产品组装和产品专属规则；
   - Plugin：`src/plugins/<域>/<impl>/`，实现 Core/Feature 明确声明的扩展点；
   - `_facade`、`_feature`、`_sup`、`_product` 均按触发条件创建，不生成空壳。
2. **9 条架构铁律**为规范文本，冻结于 `docs/architecture/feature-slice-rules.md`：①纵切边界清晰 ②层名与依赖方向统一 ③跨单元调用经 facade ④Domain 纯净 ⑤单元只依赖 Core、单元间只经 Public API/Event ⑥租户作用域显式贯穿 ⑦并发状态迁移须有跨节点保护 ⑧**Product ≠ Feature** ⑨**Plugin 只实现显式扩展点**。
3. **概念定位与判据**（四问口诀）：
   - 提供一种业务能力 → Feature（会被第二个产品复用）/ Product（只服务当前产品）；
   - 支持一种可替换实现 → Plugin（`src/plugins/`）；
   - 提供稳定的平台能力 → Core（`src/lib/`）；判据是职责，不是当前消费者数量。
4. 铁律 1-7 约束 Feature 与 Product 中实际存在的代码；铁律 8 规定二者的区分与**提升路径**（默认先落 Product，第二个真实消费方出现时才提升为 Feature，禁止提前抽象）；铁律 9 约束 Plugin 只能实现显式扩展点。
5. 扩展点沿用既有 `behaviour + registry/config`。Plugin 的 `-behaviour(X)` 目标必须声明 `-callback`；若 X 属于 Feature，还必须登记于该 Feature 的 `manifest.extension_points`。前者立即硬门，后者在存量收敛期先告警、完成登记后转硬门；普通 Core 公共能力不受 callback 要求限制。
6. 强制手段：`make arch-check`，实现为 `scripts/check_feature_architecture.sh`（模块→(单元,层) 索引 + 引用边矩阵 + 产品名扫描 + Plugin 边界扫描 + **假 Plugin 检测** + 单一消费方软告警）。它**独立于** `check_module_boundaries.sh`（后者管旧四层 handler→logic→ds→repo 边界），二者由 `make security-gate` 串联。接入三处：`make security-gate`、`lefthook` pre-commit、CI（经 `e2ee-verify: security-gate` 传递）；并配 `make arch-check-self-test` 金丝雀自检（10 条），另对 manifest 登记项**软告警**。**未接线的规则视为不存在**。
7. 术语对齐：Core = Kernel；Feature Slice 是代码边界形态；Product = `product_id` 轴（**与 `profile` 交付档位是两个轴，勿混用**）。既有速查表中把 channel/moment 等可开关业务功能称为 Plugin 的用法视为旧称，在本 ADR 中归入 Feature；`imboy_plugin_*` 仍专指已冻结的动态装载运行时。
8. 存量不迁移：`src/api|logic|ds|repo|domain/` 既有代码原地保留，**新增一律进 `src/features/`、`src/products/` 或 `src/plugins/`**；旧域（group/channel/project/moya）迁移须独立提案。
9. 修复两处构建盲区（`Makefile` glob 递归化），使纵切深层代码同样受裁剪与类型门约束。

## Consequences

- ✅ 新业务在代码结构上可被识别、可被裁剪、可被单独解释（产品组合/交付裁剪/边界三件事同时成立）。
- ✅ **多产品底座**：`moya` 等产品专属业务有明确归宿（`src/products/moya/`），无需伪装成"通用能力"；Feature 集合保持"真可复用"的语义纯度。
- ✅ **可替换性有处安放**：第三方支付/AI/存储/集成以 Plugin 形式接入，业务代码只依赖扩展点——`payment_gateway` / `imboy_llm` 已是该模式的成功先例，本 ADR 只是把它推广并加门禁。
- ✅ 三角色不再互渗：业务边界永远在 Feature/Product，技术可替换性永远在 Plugin，二者不互相污染，杜绝"Feature 里套 Plugin"。
- ✅ 首个样本（客服）跑完后沉淀 `docs/standards/feature-slice-checklist.md`，`group/channel` 等可按映射表机械迁移。
- ✅ Domain 纯净换来可测性：domain 单测**零 mock**（不需 meck 数据库/HTTP），这是分层是否真落地的判据。
- ✅ 修复 glob 盲区后，"关 Feature/Product"在**路由不注册 + 监督树不启动 + beam 被裁**三处一致。
- ⚠️ 新旧两套目录并存，需明文规则约束"该放哪"：存量不迁、新增进 features/products/plugins。
- ⚠️ 门禁若只写脚本不接线，目录将退化为装饰——故以"故意注入四类违规必须全部变红"为金丝雀验收。
- ⚠️ 支持第二个 `product_id` 需放开 `generate_product_features.py:108` 的硬校验并新增 manifest；在此之前产品维度只能靠 `profile` 近似表达。
- ⚠️ 一次性工作稿不入库；ADR 与规范文本必须落在被跟踪的 `docs/adr/`、`docs/architecture/`。

## Non-Goals

不拆微服务；不做全项目一次性大重构；不启用/扩展 `imboy_plugin_*` **动态装载运行时**（Plugin 在本 ADR 中仅指编译期/装配期的可插拔实现）；不把每个 Feature/Product 变成 OTP application（仅允许 `_sup` 受开关门控）；不修改既有对外路由协议；不在本 ADR 内迁移任何既有业务域；不引入多产品**运行时**隔离（多产品仍是同一 OTP 应用内的编译期组合）。
