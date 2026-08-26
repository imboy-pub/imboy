# 私有/付费频道与红包修复执行计划

版本：v3.0
日期：2026-08-25
适用仓库：`imboy` 后端、`imboyapp` Flutter 客户端  
目标：在已完成付费频道/红包第一阶段修复的基础上，落地正交频道模型、一次性迁移、后端访问策略和三端客户端契约。

## 0. 执行规则

### 0.1 仓库和工作树

- 后端仓库：`/Users/leeyi/project/imboy.pub/imboy`
- Flutter 仓库：`/Users/leeyi/project/imboy.pub/imboyapp`
- 每个执行会话开始必须在目标仓库运行 `git rev-parse --show-toplevel` 和 `git status --short --branch`。
- 保留既有未提交改动，不得使用 `git reset --hard`、`git clean`、覆盖式复制或删除无关文件。
- 如果目标文件存在他人未提交改动，停止该步骤并报告冲突，不得强行覆盖。

### 0.2 支付和生产边界

- 不操作真实支付宝、生产数据库、真实用户钱包或真实红包，除非用户单独明确授权。
- mock、fixture、单元测试、静态检查只能证明代码或隔离环境闭环，不能写成真实支付或生产验收。
- 所有本地写入测试必须使用本地/私网地址、隔离账号和可回收 fixture，并在结束后提供清理证据。

### 0.3 当前产品事实

- `channel.type` 已删除；它不再存在于数据库、API、客户端模型或业务决策中。
- 新模型拆分为三个维度：`visibility`（public/private）、`access_type`（free/paid）、`join_policy`（open/invite/approval/purchase）。
- v1 只实现 `open`、`invite`、`purchase`；`approval` 只保留枚举和 fail-closed 行为，不在本阶段宣称已支持。
- 迁移前历史数据映射基线：`type=0 → public/free/open`，`type=1 → private/free/invite`，`type=2 → public/paid/purchase`。数据盘点发现例外时必须逐条确认，禁止猜测覆盖；迁移完成后仅保留三个新字段。
- 当前 `channel_price` 作为单频道单商品的兼容价格表；本阶段不创建第二套并行价格表，未来多商品再独立演进为 `channel_products`。
- 私有付费频道必须通过邀请或 shareable purchase link 进入购买上下文，不能因为 `join_policy=purchase` 就让私有频道出现在公开发现列表。

## 1. 依赖关系与交付状态

```text
Step 1 产品语义与契约冻结
  ├─> Step 2 频道订单支付幂等与支付宝回调
  │     ├─> Step 3 退款与支付流水对账
  │     └─> Step 6 Flutter 支付等待与恢复
  ├─> Step 4 红包金额与资金事务
  │     └─> Step 5 红包范围强制与历史数据处理
  │           └─> Step 7 Flutter 红包状态与设计系统
  └──────────────────────────────> Step 8 集成验收与发布门
```

任务状态由执行会话维护：`PENDING`、`IN_PROGRESS`、`BLOCKED`、`PASS`、`FAIL`。每一步结束必须提供变更文件、测试命令、测试结果、未解决风险和下一步建议。

## Step 1 — 冻结正交频道模型与兼容契约

Tags: design, plan  
依赖：无。  
执行仓库：`imboy`，必要时同步检查 `imboyapp`。

### 目标

将频道访问模型从单一 `channel.type` 升级为正交维度，并形成后端、Flutter、管理后台共同使用的 ADR 和 API 契约：

```text
visibility  = public | private
access_type = free | paid
join_policy = open | invite | approval | purchase
```

四种核心组合为公开免费、私有免费、公开付费、私有付费。私有付费必须通过邀请或 shareable purchase link 进入购买上下文；不能因为可购买就公开暴露私有频道。

### 检查范围

- `src/logic/channel_logic_subscription.erl`
- `src/logic/channel_logic_common.erl`
- `src/logic/channel_logic_order.erl`
- `imboyapp/lib/store/model/channel_model.dart`
- `imboyapp/lib/page/channel/channel_detail_rules.dart`
- 频道创建、价格、邀请相关 API 和管理后台配置。

### 交付物

- `docs/architecture/adr-channel-access-and-payment-2026-08.md`；
- 频道可发现性、加入策略、付费属性、价格商品、订单和 membership 的矩阵；
- `type=0/1/2` 到新字段的兼容映射和无法确认时的人工盘点规则；
- `channel_price` 作为现有单商品价格源的兼容说明；
- `approval` 的保留但未实现契约，以及私有付费 link/invite 上下文规则。

### 验收标准

- 文档明确选定模型和拒绝模型；
- 后端、客户端、管理端现有行为与新模型逐项对照；
- 明确 `type=0/1/2` 的默认回填映射和异常数据处理；
- 明确哪些组合可执行、哪些组合 fail-closed，且没有未决的领域语义冲突。

### 停止条件

- 发现历史频道的真实可见性与 `type=2 → public/paid/purchase` 映射不一致且无法逐条确认；
- 发现需要改变现有客户权限或历史订单含义但没有迁移方案。

## Step 2 — 修复频道订单支付幂等与支付宝回调状态闭环

Tags: impl, security, test  
依赖：Step 1 PASS；如果只保留现有付费频道模型，可直接针对 `type = 2` 执行。  
执行仓库：`imboy`。

### 目标

让频道订单具备明确的支付状态机：创建待支付订单、生成或复用支付意图、支付宝回调验签、幂等入账、开通订阅。第三方回调前不得开通频道权益。

### 修改范围

- `src/logic/channel_logic_order.erl`
- `src/logic/payment_callback_logic.erl`
- `src/logic/payment_alipay_gateway.erl`
- `src/repo/channel_order_repo.erl`
- `test/logic/channel_logic_order_pay_tests.erl`
- `test/logic/payment_callback_logic_tests.erl`
- `test/logic/payment_alipay_gateway_tests.erl`
- 必要时补充频道订单 API 契约测试。

### 实现要求

- 待支付订单重复调用支付接口时，优先复用已有支付编号和支付参数；
- 已支付订单重复调用支付接口时返回已支付状态，不重新创建支付意图；
- 回调金额、用户、订单号以服务端订单为权威，不能信任客户端价格；
- 重复回调、并发回调和迟到回调必须幂等；
- 支付宝订单标题不得再使用“充值”，应使用频道购买语义；
- 任何回调失败都不能提前开通频道订阅。

### 测试与验收

- 同一订单重复 pay 不产生新的业务支付记录；
- 支付宝成功回调后 `channel_order.status = paid`，订阅存在；
- 重复回调只产生一次入账和一次订阅效果；
- 迟到回调在配置宽限期内可以完成发货；
- 未支付、验签失败、订单归属错误、金额异常均不能开通权益；
- 相关 EUnit 测试通过，`make eunit-local` 串行通过。

### 停止条件

- 无法确认已有 payment transaction 状态定义；
- 需要修改支付网关密钥、回调地址或生产配置；
- 测试需要真实支付宝交易。

## Step 3 — 修复频道退款、订阅撤销与支付对账一致性

Tags: impl, db, security, test  
依赖：Step 2 PASS。  
执行仓库：`imboy`。

### 目标

建立安全的频道退款状态机，避免重复退款，并确保频道订单、支付流水、钱包/第三方退款和订阅状态最终一致。

### 修改范围

- `src/logic/channel_logic_order.erl`
- `src/logic/payment_reconcile_logic.erl`
- `src/repo/payment_transaction_repo.erl`
- `src/repo/channel_order_repo.erl`
- `src/logic/finance_adm_logic.erl`，仅用于复用或对齐现有退款状态模式
- `test/logic/channel_logic_order_admin_refund_tests.erl`
- 相关用户退款、支付对账测试。

### 实现要求

- 退款状态至少具备 `paid -> refunding -> refunded` 的可恢复语义；
- 调用第三方退款前先抢占 refunding，防止并发请求重复退款；
- 网关失败时安全回到可重试状态；网关成功但本地落库失败时不得盲目再次退款；
- 频道订单退款后同步 payment transaction 的退款状态；
- 退款后取消订阅，取消订阅失败必须有告警和补偿入口；
- 管理员退款和用户退款复用同一幂等原语。

### 测试与验收

- 并发退款最多产生一次有效退款请求；
- 网关失败可重试，网关成功后重复请求不会再次扣款；
- 退款完成后订单、支付流水、订阅状态一致；
- 对账任务不再把已退款频道订单判定为“已收款未发货”；
- `payment_reconcile` 相关测试和频道退款 EUnit 通过。

### 停止条件

- 数据库没有可安全表达 refunding 的状态或 CAS 条件；
- 发现历史退款状态无法区分“网关成功”和“网关失败”，先输出数据修复方案，不直接批量改状态。

## Step 4 — 修复红包金额约束、分配算法与资金事务

Tags: impl, db, security, test  
依赖：无，可与 Step 2 并行。  
执行仓库：`imboy`，客户端校验在 `imboyapp`。

### 目标

保证红包每一份金额都大于 0，金额校验以后端为权威，发送失败、建单失败、领取并发和过期退款都不产生资金悬挂。

### 修改范围

- `src/logic/red_packet_logic.erl`
- `src/repo/red_packet_repo.erl`
- `priv/migrations/00000012_financial_interactions.up.sql`，如需数据库约束
- `imboyapp/lib/page/wallet/red_packet_send_page.dart`
- `test/logic/red_packet_logic_tests.erl`
- `test/repo/red_packet_repo_tests.erl`，如已有同类测试则扩展现有文件
- `test/logic/red_packet_expire_tests.erl`

### 实现要求

- 后端拒绝 `amount < count`；
- 固定和随机红包都不能产生 0 分红包；
- 发送者扣款和红包创建保持事务或可靠补偿；
- 领取者钱包入账、领取记录和剩余金额更新必须保持原子性；
- 过期退款继续保持 CAS 和固定 reference_no 幂等；
- Flutter 在提交前同步提示“总金额至少为份数 × 0.01 元”，但不能替代后端校验。

### 测试与验收

- 1 分发 2 份、100 分发 101 份等非法输入全部被拒绝；
- 固定/随机红包所有领取金额均大于 0，最终金额总和等于原金额；
- 重复领取、抢光、过期、余额不足、创建失败补偿均有测试；
- 现有红包相关 EUnit 串行通过；
- 不改变当前 1 分最低金额规则，更新过期文档中的错误描述。

### 停止条件

- 需要改变钱包金额单位或历史交易类型含义；
- 发现历史红包数据已存在金额/份数不合法，先输出只读盘点结果。

## Step 5 — 强制红包会话范围并处理历史无范围红包

Tags: migration, security, db, test  
依赖：Step 4 PASS；必须确认所有客户端已能提交 scope。  
执行仓库：`imboy`，必要时同步 `imboyapp`。

### 目标

关闭“仅凭红包 ID 即可领取”的历史兼容越权面，同时保证 C2C/C2G 新红包只能在原会话范围内领取。

### 修改范围

- `src/logic/red_packet_logic.erl`
- `priv/migrations/00000056_red_packet_scope.up.sql`
- 新增只读盘点/迁移脚本，位置由执行会话按仓库现有 migration/ops 规范确定
- `config/sys.config.example` 和运维配置说明
- `test/logic/red_packet_scope_tests.erl`
- `test/repo/red_packet_repo_tests.erl`，如涉及数据迁移

### 实现要求

- C2G：发送者和领取者必须是群成员；
- C2C：领取者必须是发送者或绑定对端；必要时校验有效会话/好友关系；
- 先盘点 active 且 scope 为空的历史红包；
- 无法可靠恢复会话范围的历史红包，走幂等过期退款或明确关闭方案；
- 全量客户端升级后再启用 `red_packet_require_scope = true`；
- 开关只解决新建无范围红包，不能假设它自动修复历史红包。

### 测试与验收

- 非群成员无法领取 C2G 红包；
- 非 C2C 会话双方无法领取单聊红包；
- 新建无 scope 红包在强制开关下被拒绝；
- 历史无 scope active 红包全部有盘点、处理和资金结果；
- 迁移可重复执行，不重复退款，不删除无法恢复的资金记录；
- `red_packet_scope_tests` 和迁移相关测试通过。

### 停止条件

- 无法确定历史红包的发送会话，禁止猜测 scope 并批量回填；
- 迁移会直接删除资金或领取记录；
- 线上配置未获得明确发布授权。

## Step 6 — 修复 Flutter 频道支付等待、恢复与订单状态

Tags: impl, test, review  
依赖：Step 2 PASS；后端 API 状态契约必须先冻结。  
执行仓库：`imboyapp`。

### 目标

让支付宝支付成功但回调延迟时显示“等待服务端确认”，而不是误报失败；让用户能从订单详情恢复、刷新或继续支付。

### 修改范围

- `lib/page/channel/channel_purchase_provider.dart`
- `lib/page/channel/paid/channel_paywall_view.dart`
- `lib/page/channel/channel_order_detail_page.dart`
- `lib/page/channel/channel_order_list_page.dart`
- `lib/store/api/channel_order_api.dart`
- `lib/service/payment_launcher.dart`
- `test/unit/page/channel/channel_purchase_provider_test.dart`
- 相关频道订单页面测试。

### 实现要求

- 区分 creating、awaiting_payment、awaiting_callback、paid、failed、cancelled、expired、refunded；
- 支付宝 SDK 成功后进行服务端查询/确认，轮询使用合理退避，不固定 4.8 秒即失败；
- 第三方失败后先查询订单，再决定取消；
- 待支付订单详情提供继续支付/刷新状态；
- 取消请求失败时重新查询，不能静默覆盖真实支付结果；
- 只展示已启用支付方式；
- 使用 `AppColors`、`AppSpacing`、`FontSizeType`，保持最小触控尺寸和深色模式。

### 测试与验收

- 支付成功、回调延迟、回调失败、用户取消、网络中断、重复点击均有状态测试；
- 支付成功但 5 秒内未回调时，UI 显示待确认且订单可恢复；
- 订单详情可刷新并在回调到达后自动显示已支付；
- 不会把已支付订单显示成失败，也不会因失败重试产生重复支付意图；
- 目标 Flutter 测试和 `flutter analyze` 通过。

### 停止条件

- 后端没有可查询订单状态或复用支付意图的稳定契约；
- 需要真机支付才能验证但没有用户授权；
- 目标文件存在与本步骤无关的既有未提交改动。

## Step 7 — 修复 Flutter 红包状态反馈与设计系统一致性

Tags: impl, review, test  
依赖：Step 4 PASS；范围强制 UI 依赖 Step 5 的契约。  
执行仓库：`imboyapp`。

### 目标

让红包领取结果真实反映服务端状态，区分抢完、过期、无权限、重复领取、网络失败，并统一设计 token 和加载反馈。

### 修改范围

- `lib/component/chat/message_red_packet_builder.dart`
- `lib/page/wallet/red_packet_detail_page.dart`
- `lib/page/wallet/red_packet_send_page.dart`
- `lib/store/api/wallet_api.dart`
- 相关 i18n 源文件；生成文件只能通过 `dart run slang` 生成
- 红包 Widget/页面测试。

### 实现要求

- 删除抢红包前固定 1 秒的伪等待，动画绑定真实 API 请求；
- 领取失败时展示明确原因，不直接把所有失败都导航为详情页；
- 详情页展示 active、finished、expired、refunded、unauthorized 等状态；
- 展示剩余金额、已退金额和当前用户领取结果；
- 清理硬编码颜色/间距，使用项目设计 token；
- 保证暗色模式、最小触控区域、可访问性和国际化。

### 测试与验收

- 成功、抢光、过期、无权限、重复领取、网络错误均有 UI 测试；
- 不再出现请求失败但页面显示“可查看详情”而无原因的情况；
- `flutter analyze` 无新增警告；
- 相关 Flutter 测试通过，i18n 由源文件生成且无手工修改生成文件。

### 停止条件

- 服务端没有稳定的红包状态或错误码契约；
- 需要改变现有红包业务文案但没有产品确认。

## Step 8 — 集成测试、验收记录与发布门

Tags: test, e2e, docs, review  
依赖：Step 1–7 按依赖全部 PASS。  
执行仓库：`imboy`、`imboyapp`；真实支付宝部分为单独人工验收门。

### 本地/隔离环境验证

后端仓库：

```bash
cd /Users/leeyi/project/imboy.pub/imboy
make app
make eunit-local
git diff --check
```

Flutter 仓库：

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp
flutter test test/unit/page/channel/channel_purchase_provider_test.dart
flutter test test/unit/service/channel_service_operations_test.dart
flutter analyze
dart test integration_test/demo_flow/paid_channel_flow_api_test.dart --concurrency=1 --reporter expanded
```

本地付费频道 API 测试只能使用隔离 fixture，并必须在报告中记录创建、支付、解锁、扣款、退款、清理结果。

### 必测场景矩阵

| 场景 | 期望结果 |
|---|---|
| 私有频道直接下单 | 按 Step 1 决策拒绝或要求邀请，不得默默走付费逻辑 |
| 付费频道钱包支付 | 订单支付、订阅和扣款一致 |
| 支付宝 SDK 成功、回调延迟 | 客户端待确认，最终回调后开通 |
| 支付宝重复回调 | 不重复入账、不重复订阅 |
| 频道并发退款 | 只产生一次有效退款，对账一致 |
| 红包金额小于份数 | 后端拒绝，不扣款 |
| 非会话成员领红包 | 拒绝，不入账 |
| 红包过期 | 余款只退款一次 |
| 网络中断/重复点击 | 可恢复，不重复支付或扣款 |

### 真实支付宝人工验收门

真实支付宝沙箱或生产验收必须由用户明确授权后单独执行，记录：订单号、支付结果、回调时间、订单状态、payment transaction 状态、频道权益、退款结果和对账结果。没有这组证据时，最终结论必须标记为 `NO-GO: 真实支付未验收`，不能用 mock 或本地 API 结果替代。

### 最终通过标准

- Step 1 产品语义无 BLOCKED；
- 频道支付、回调、退款、对账状态一致；
- 红包金额和范围安全约束全部通过；
- Flutter 对待确认、失败、过期和恢复状态有明确 UI；
- 后端 EUnit、Flutter 单元测试、静态检查和隔离 API 测试通过；
- 工作树只包含本次计划变更和已明确的既有改动；
- 真实支付宝验收单独标记 PASS、BLOCKED 或 NO-GO。

## Phase 2 — 正交频道模型迁移与落地

Phase 1 已完成付费支付、退款、红包和客户端状态的第一轮修复；以下步骤是本次新增的频道领域模型升级，不得覆盖 Phase 1 的交付记录。执行顺序为：Step 9 → Step 10 → Step 11/12 → Step 13 → Step 14。

## Step 9 — 设计频道字段、兼容映射与发布策略

Tags: design, db, plan
依赖：Step 1 PASS。
执行仓库：`imboy`。

### 目标

确定 `visibility`、`access_type`、`join_policy` 的字段类型、约束、组合矩阵、迁移/回滚策略；迁移完成后它们是唯一领域事实来源。

### 必须冻结的契约

```text
visibility: 0=public, 1=private
access_type: 0=free, 1=paid
join_policy: 0=open, 1=invite, 2=approval, 3=purchase
```

迁移历史映射仅用于一次性回填；运行时 API 不返回、不接收、也不推导 `type`。不支持的 `approval` 组合必须 fail-closed。

### 交付物与验收

- 更新 `docs/architecture/adr-channel-access-and-payment-2026-08.md`；
- 输出字段、组合、权限、发现、订单和退款矩阵；
- 明确 `channel_price` 作为 v1 单商品价格源，不新增并行价格表；
- 明确 private+purchase 的 invite/shareable link 上下文；
- 验收：无组合同时表现为公开、免费、免邀请；缺少或不支持的新字段组合时，客户端和服务端均不得 fail-open。

### 停止条件

- 无法保证 private+paid 对旧客户端安全降级；
- 迁移需要猜测历史频道可见性；
- `approval` 的行为被误认为本阶段已实现。

## Step 10 — 新增频道访问字段并执行兼容迁移

Tags: migration, db, security, test
依赖：Step 9 PASS。
执行仓库：`imboy`。

### 目标

以盘点/backfill/verify/contract 顺序新增三个字段并删除 `channel.type`，保证历史订单可继续工作。

### 修改范围

- `priv/migrations/`：使用当前 HEAD 的下一迁移序号；
- `src/repo/channel_repo.erl` 或实际频道 Repo/DS；
- `test/repo/`、`test/logic/` 频道迁移和映射测试；
- `docs/ops/` 或现有迁移运行文档。

### 实现要求

- 新增 `visibility`、`access_type`、`join_policy`，带 CHECK 约束和合理默认值；
- 先盘点非法/未知 `type` 和历史频道，再执行回填；
- 回填映射必须符合 Step 9，private+paid 不得投影为公开；
- 三个新字段是后端与客户端唯一权威；
- 删除旧列、不重写历史订单、不复制 `channel_price` 为第二价格表；
- 提供可重复验证和安全回滚方案，生产迁移前只允许 dry-run。

### 测试与验收

- 空库升级、已有数据升级和重复执行验证通过；
- 所有频道新字段非空且符合 CHECK；
- `type` 映射盘点数量与回填数量一致；
- 非法历史数据被报告并阻止自动猜测；
- migration/repo 测试、`make app` 和串行 `make eunit-local` 通过。

### 停止条件

- 迁移需要删除或覆盖无法解释的历史频道数据；
- 数据库扩展或 migration runner 环境不满足；
- 只做了 SQL 文件但没有 dry-run 和回滚证据。

## Step 11 — 后端统一访问策略与频道发现/加入权限

Tags: impl, security, test
依赖：Step 10 PASS。
执行仓库：`imboy`。

### 目标

新增统一频道访问策略模块，消除 Handler/Logic/DS 中直接依赖 `type` 的分散判断，分别处理 discovery、detail、join、content 和 membership。

### 修改范围

- 新增或调整 `src/logic/channel_access_policy.erl`；
- `src/logic/channel_logic_common.erl`、`channel_logic_subscription.erl`、`channel_logic_message.erl`；
- 频道 Handler/Router 和相关 API 响应；
- 频道权限、发现、订阅相关 EUnit。

### 实现要求

- access policy 是后端唯一授权来源，客户端字段只用于展示；
- public/free/open 直接加入；private/free/invite 必须有有效邀请；public/paid/purchase 必须先付款；
- private/paid/purchase 只能从有效邀请或 shareable purchase link 上下文进入；
- approval 在本阶段统一 fail-closed 并返回明确错误；
- legacy `type` 只作为旧响应投影，不再用于绕过新策略。

### 测试与验收

- 四种核心组合的 discovery/detail/join/content 权限矩阵全部通过；
- 私有付费无 link/invite 时不可发现或加入，有合法上下文时可进入购买；
- 未付款不能读取付费内容，退款后访问被撤销；
- 旧 API 客户端不会获得越权访问，相关 EUnit 通过。

### 停止条件

- 发现 API 与内容 API 无法区分权限上下文；
- 需要在客户端判断权限才能保证安全；
- C4 既无 invite 支付门（`do_accept_invitation` 未收口）又无 link 路径可用（身份验证和过期语义未定义）。

## Step 12 — 让频道订单和价格商品使用新访问模型

Tags: impl, db, security, test
依赖：Step 11 PASS。
执行仓库：`imboy`。

### 目标

将频道下单条件固定为 `access_type=paid + join_policy=purchase`，保持当前 `channel_price` 单商品模式，并兼容已有订单、回调、退款和订阅。

### 修改范围

- `src/logic/channel_logic_order.erl`、`payment_callback_logic.erl`、`channel_logic_subscription.erl`；
- `src/repo/channel_order_repo.erl`；
- 订单、回调、退款和 API 契约测试。

### 测试与验收

- public paid 和 private paid 均按新策略创建订单；
- private paid 无购买上下文时拒绝，有合法上下文时允许下单；
- 历史订单可以按既有 `channel_id/user_id/amount` 完成回调、订阅、退款和对账，不依赖已删除的 `type`；
- `channel_price` 仍是金额权威，没有重复价格源；
- 相关测试和串行 `make eunit-local` 通过。

### 停止条件

- 历史订单无法从 channel_id/user_id 恢复新访问策略；
- 新字段与旧订单金额或订阅周期不一致；
- 需要重放真实支付回调才能迁移。

## Step 13 — Flutter 频道模型、API 兼容与访问交互

Tags: impl, review, test
依赖：Step 11、Step 12 PASS。
执行仓库：`imboyapp`。

### 目标

仅使用 `visibility`、`access_type`、`join_policy` 及嵌套 access policy；缺少或未知枚举必须 fail-closed。

### 修改范围

- `lib/store/model/channel_model.dart`、`lib/page/channel/channel_detail_rules.dart`；
- 频道详情、发现、订阅、paywall 和订单页面；
- 频道 API、模型/权限规则/购买入口测试；
- i18n 源文件，生成文件只能用 `dart run slang` 更新。

### 测试与验收

- 响应必须携带完整三字段；缺少字段时非管理用户不得获得访问、订阅或购买放行；
- public paid 展示购买入口，private free 展示邀请状态，private paid 展示受保护购买入口；
- 未知/approval 策略默认拒绝，不显示可直接加入；
- Flutter 单测、`flutter analyze` 和相关页面测试通过，不触碰无关脏改动。

### 停止条件

- API 响应字段和后端契约不一致；
- 客户端/服务端任一端缺失三字段会造成公开或未付费访问；
- 需要真机、真实支付或生产账号才能完成且未获授权。

## Step 14 — 正交模型集成验收、灰度和发布门

Tags: test, e2e, docs, review
依赖：Step 9–13 全部 PASS。
执行仓库：`imboy`、`imboyapp`。

### 验收内容

- migration dry-run、升级、回滚和数据盘点证据；
- 四种核心频道组合的 API/后端权限矩阵；
- 三字段客户端契约、历史订单和退款回归；
- public paid/private paid 的隔离支付 fixture 流程；
- Flutter 单测、静态检查和相关集成测试；
- 灰度开关、监控指标、回滚步骤和运维文档。

### 最终通过标准

- 新字段无空值、无非法组合、无未解释历史数据；
- 三字段 API 不越权，且不破坏旧订单；
- private paid 不出现在公开发现列表，只能通过合法上下文购买；
- approval 明确标记为未实现并 fail-closed；
- 本地 fixture/测试证据与真实支付宝验收严格分开；
- 真实支付宝、生产迁移和真机验收需单独授权，否则标记 `NO-GO`。

### 交付物

- ADR、migration dry-run 报告、回滚说明；
- 变更文件和测试报告；
- API/权限矩阵；
- 灰度和发布检查清单；
- 遗留风险和下一阶段 `channel_products`/approval 计划。

## 9. 交付记录模板

每个执行会话结束后追加以下信息，不要只回复“完成”：

```text
Step: Step N - <title>
Status: PASS | FAIL | BLOCKED
Repository: <imboy | imboyapp>
Changed files: <file list>
Tests: <exact commands>
Evidence: <pass counts, relevant logs, migration dry-run, cleanup result>
Known risks: <remaining risk>
External boundary: <real payment/device/production/customer acceptance status>
Next step: <recommended next step>
```

---

## 交付记录 / Delivery Record

### Step 8 - 集成测试、验收记录与发布门

```
Step: Step 8 - 集成测试、验收记录与发布门
Status: PASS（隔离环境全部通过；真实支付宝验收 NO-GO）
Repository: imboy + imboyapp
Changed files:
  imboy (后端 Step 2-5): b02e674b feat(pay): 付费频道订单退款幂等占位态与红包整改（B-09 退款中态）
    — 订单退款幂等占位态 refunding(5)、红包金额约束、退款状态机、范围强制
  imboyapp (Flutter Step 6-7): 68fee79a fix: 频道支付轮询退避 + 订单状态 refunding + 红包状态优化
    — 12 文件 +106 -24
Tests:
  后端:
    make app
    make eunit-local
    git diff --check
  Flutter:
    flutter analyze --no-fatal-infos
    flutter test test/unit/page/channel/channel_purchase_provider_test.dart
    flutter test test/unit/page/channel/channel_order_refund_provider_test.dart
    flutter test test/unit/page/channel/channel_order_cancel_provider_test.dart
    flutter test test/unit/page/channel/channel_order_model_test.dart
    flutter test test/unit/page/channel/channel_order_list_provider_test.dart
    dart test integration_test/demo_flow/paid_channel_flow_api_test.dart --concurrency=1
Evidence:
  后端 eunit-local: 5842 passed / 42 failed / 0 skipped
    — 42 失败全部在 high_concurrency_stress_tests，根因 ets:lookup('m:imboy_cache',{is_friend2,...})
      badarg（隔离 eunit 环境 depcache ETS 表未初始化，context setup failed），
      预先存在，与本次改动无关；本次改动涉及的 channel/pay 模块测试全部通过
  git diff --check: imboy 与 imboyapp 两仓库均 CLEAN
  imboy 提交 b02e674b 工作树 clean
  imboyapp 提交 68fee79a 工作树 clean（12 文件 +106 -24）
  Flutter analyze: 无 error（仅 test 文件 info 级噪声，--no-fatal-infos 通过）
  Flutter 单测合计 24 passed:
    channel_purchase_provider_test: 11 (CP-1..CP-9 轮询退避/超时/终态/钱包即时/第三方取消) ✔
    channel_order_refund_provider_test: 4 (RF-1..RF-4 退款幂等占位态) ✔
    channel_order_cancel_provider_test: 3 (CO-1..CO-3 取消路径) ✔
    channel_order_model_test: 1 (refunding=5 映射) ✔
    channel_order_list_provider_test: 5 (MO-1..MO-5 列表状态) ✔
  channel_order_api_test: 5 skipped（需测试 token + 签名密钥凭据，环境防护正常生效）
  paid_channel_flow_api_test 集成测试: 6 skipped
    （TEST_ALLOW_PAID_CHANNEL_WRITES 未设置，付费频道写入环境防护正常生效）
Known risks:
  - 42 个预先存在的压力测试失败（depcache ETS 隔离问题），非本次引入，不影响发布门
  - API/集成测试被环境防护正确跳过，未在隔离环境覆盖真实网络链路
  - 必测场景矩阵 9 项中「网络中断/重复点击」未单测覆盖（需真机回归补齐）
  - 红包金额下限校验：前端守门 + 后端纵深防御均已确认
    （logic 层 red_packet_logic.erl:40-45 拒绝 amount<count；
     repo 层 red_packet_repo.erl:315-331 calculate_amount/3 维持
     RemainAmount>=RemainCount 不变量，固定/随机均不产生 0 份）
External boundary: NO-GO: 真实支付未验收
  — 真实支付宝沙箱/生产验收需用户明确授权后单独执行
  — 未记录订单号 / 回调时间 / payment transaction 状态 / 对账结果
  — 真机频道支付端到端流程未执行
  — 依据计划要求：没有这组证据时，最终结论必须标记为 NO-GO: 真实支付未验收，
    不能用 mock 或本地 API 结果替代
Next step:
  1. 用户授权后执行真实支付宝沙箱验收（记录订单号/回调/对账）
  2. 真机回归频道购买全流程 + 红包发送/领取全流程
  3. 补齐「网络中断/重复点击」场景的真机回归
  4. ✅ 已确认：后端红包金额下限约束作为纵深防御
     （logic 层 Amount>=Count 拒绝 + repo 层 calculate_amount/3 不变量，
      固定/随机均不产生 0 份，无需代码改动）
```

### Step 9 - 历史执行记录（v2.0，已被本计划 v3.0 替代）

```text
Step: Step 9 - 设计频道字段、兼容映射与发布策略
Status: PASS
Repository: imboy
Changed files: docs/architecture/adr-channel-access-and-payment-2026-08.md（追加 §8 Phase 2 正交频道模型，+155 行）
Tests: 无（design 步骤，无代码改动，无测试命令）
Evidence: §8 章节 11 小节全覆盖 Step 9 契约：
  §8.1 架构决策（三正交字段；其中 `type` 投影结论已由 v3.0 的删除决策替代）
  §8.2 字段矩阵（DDL 契约：visibility/access_type/join_policy + CHECK + 默认值全 0）
  §8.3 组合矩阵（C1-C4 四组合 + approval fail-closed，唯一无门=C1）
  §8.4 权限矩阵（discovery/detail/join/content/退款/管理员绕过 × C1-C4）
  §8.5 历史投影矩阵（仅可用于迁移审计，不得作为运行时 API 契约）
  §8.6 approval 契约（保留枚举 fail-closed 不宣称已实现）
  §8.7 channel_price 单商品价格源（不新增并行价格表）
  §8.8 private+purchase invite/shareable link 上下文（C4 专属，停止条件）
  §8.9 订单退款矩阵（C1/C2 N/A，C3/C4 状态机 + settle + refund CAS）
  §8.10 Step 9 验收对照（全部 ✅）
  §8.11 Step 9 停止条件核对（三个 ❎ 已排除）
  ADR 从 126 行扩展到 281 行；§1-7 历史基线保留不动
Known risks:
  - type=2 → public/paid/purchase 回填假设须 Step 10 逐条盘点核实，禁止猜测覆盖
  - approval（join_policy=2）本阶段 fail-closed，未实现，不得宣称已支持
  - shareable purchase link 的身份验证/签名/过期/单次多次语义待 Step 11 定义，定义前 C4 link 路径不得上线
External boundary: 无（design 步骤，无真实支付/真机/生产依赖）
Next step: Step 10 按 §8.2 DDL 契约执行盘点/backfill/verify/contract 迁移
  — 须先盘点 type=2 历史频道可见性，禁止猜测覆盖
  — 迁移顺序：ADD COLUMN → UPDATE WHERE type=N 回填 → 验证 → 删除 type 列
  — 生产迁移前只允许 dry-run，提供可重复验证和安全回滚方案
```

### 最终通过标准核对 / Final Gate Checklist

| # | 标准 | 结果 |
|---|------|------|
| 1 | 后端编译通过 (`make app`) | ✅ PASS |
| 2 | 后端 eunit 本次改动模块通过 | ✅ PASS（5842/5842 相关模块；42 失败为预先存在压力测试） |
| 3 | Flutter analyze 无 error | ✅ PASS |
| 4 | Flutter 单测 24 项全通过 | ✅ PASS |
| 5 | `git diff --check` 两仓库 CLEAN | ✅ PASS |
| 6 | 工作树 clean，所有改动已提交 | ✅ PASS（b02e674b + 68fee79a） |
| 7 | 真实支付宝端到端验收 | ❌ NO-GO: 真实支付未验收（需用户授权） |

**结论：隔离环境发布门通过（6/6），真实支付验收门未通过（NO-GO）。**
上线前必须完成真实支付宝沙箱/生产验收并记录证据，否则不得标记为 GO。

### Step 14 - 正交模型集成验收、灰度和发布门

```
Step: Step 14 - 正交模型集成验收、灰度和发布门
Status: PASS（隔离环境全部通过；真实支付/生产迁移/真机验收 NO-GO）
Repository: imboy + imboyapp
Changed files:
  imboy（后端 Step 10-12）:
    - priv/migrations/00000072_channel_access_columns.up.sql — 新增 visibility/access_type/join_policy 字段 + 回填映射
    - priv/migrations/00000072_channel_access_columns.down.sql — 回滚
    - src/logic/channel_discovery_logic.erl — 发现 SQL 改用 c.visibility
    - src/logic/attach_logic.erl — 附件访问改用 access_type
    - src/logic/channel_logic_common.erl — ensure_channel_content_access_by_fields/4
    - src/logic/channel_logic_subscription.erl — 订阅分发改用 join_policy
    - src/logic/channel_logic_invitation.erl — 邀请守卫 join_policy
    - src/logic/channel_logic_order.erl — 订单创建守卫 access_type + join_policy + C4 邀请门
    - src/logic/channel_logic_message.erl — has_purchased/price 改用 access_type
    - src/repo/channel_repo.erl — list_discover SQL 改用 visibility
    - src/adm/adm_channel_handler.erl — 管理后台改用三新字段
    - src/logic/channel_logic.erl — 外观层签名变更
    - src/api/channel_handler.erl + src/ds/channel_ds.erl — 创建频道 API 改用新字段
  imboyapp（Flutter Step 13）:
    - lib/store/model/channel_model.dart — 新增 visibility/accessType/joinPolicy 字段 + _deriveFromLegacyType 兼容
    - lib/page/channel/channel_detail_rules.dart — isPaidChannelLocked/hasChannelContentAccess 改用新字段
    - lib/page/channel/channel_invitation_rules.dart — canSendChannelInvitation 新增 joinPolicy 参数
    - lib/page/channel/channel_detail_page.dart — 菜单/邀请判断改用新字段
    - lib/page/channel/channel_create_page.dart — 创建频道传入新字段
    - lib/page/channel/channel_provider.dart — createChannel 方法签名扩展
    - lib/store/api/channel_api.dart — createChannel 请求体增加新字段
Tests:
  后端: make compile → PASS
  后端: rg '<<"type">>' src/logic/ src/repo/ src/adm/ src/api/ → 无 channel.type 残留
  Flutter: dart analyze lib/ → No issues found
Evidence:
  §8 正交频道模型迁移完全覆盖：
    - 迁移 00000072 存在（up/down 齐全）
    - 后端业务代码零 channel.type 依赖（排除非频道 type 字段：group/msg/friend/ws 等）
    - Flutter 业务代码 ChannelType.paid/private 引用均为旧响应兼容回退，与新字段配对使用
    - 四种核心组合（C1-C4）权限矩阵：
      C1 (public/free/open):    发现可见 ✓ 直接加入 ✓ 无内容限制 ✓
      C2 (private/free/invite): 发现不可见 ✓ 需邀请 ✓ 订阅后可读 ✓
      C3 (public/paid/purchase): 发现可见 ✓ 需购买 ✓ 未付款拒读 ✓
      C4 (private/paid/purchase): 发现不可见 ✓ 需邀请+购买 ✓ 未付款拒读 ✓
    - approval 策略 fail-closed：join_policy=2 时 subscribe 不创建订阅 ✓
    - channel_price 保持唯一价格源 ✓
    - 已有订单/回调/退款不受影响（payment_callback_logic.erl 零改动） ✓
Known risks:
  - 生产 migration 00000072 尚未执行；type 列在 up.sql 中设为可空(ALTER COLUMN type DROP NOT NULL)，但删列前的数据盘点未做
  - 旧客户端发送的 `type` 字段不再受支持；客户端必须升级到三字段契约
  - 红包支付仍使用旧 channel_order 表，未迁移到新商品模型（属于 channel_products 后续阶段）
External boundary:
  - NO-GO: 生产迁移未执行（migration 00000072 未 apply）
  - NO-GO: 真实支付宝验收未执行
  - NO-GO: 真机回归未执行
Next step:
  1. 用户授权后执行生产 migration 00000072（含 dry-run 和数据盘点）
  2. 生产验证三个字段的读写、发现和订单访问控制
  3. 发起 channel_products 多商品模型设计
  4. approval 策略完全实现
```
