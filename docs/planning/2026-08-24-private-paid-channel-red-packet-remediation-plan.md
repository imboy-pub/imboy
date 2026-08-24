# 私有/付费频道与红包修复执行计划

版本：v1.0  
日期：2026-08-24  
适用仓库：`imboy` 后端、`imboyapp` Flutter 客户端  
目标：把付费频道支付宝下单链路、退款对账链路、红包资金与会话范围链路修复到可测试、可验收状态。

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

- 当前 `channel.type = 1` 是私有频道：邀请校验后订阅，不创建订单。
- 当前 `channel.type = 2` 是付费频道：创建订单，付款成功后订阅。
- 是否新增“私有且付费”频道是前置产品决策。未确认前，不得自行把 `type = 1` 改成收费或改变现有权限语义。

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

## Step 1 — 冻结私有频道与付费频道产品语义

Tags: design, plan  
依赖：无。  
执行仓库：`imboy`，必要时同步检查 `imboyapp`。

### 目标

确认以下三种模型中的一种，并形成可供后端、Flutter、管理后台共同使用的契约：

1. 私有频道只允许邀请，不收费；
2. 付费频道只按订单购买；
3. 新增“私有付费频道”，同时要求邀请和支付。

### 检查范围

- `src/logic/channel_logic_subscription.erl`
- `src/logic/channel_logic_common.erl`
- `src/logic/channel_logic_order.erl`
- `imboyapp/lib/store/model/channel_model.dart`
- `imboyapp/lib/page/channel/channel_detail_rules.dart`
- 频道创建、价格、邀请相关 API 和管理后台配置。

### 交付物

- `docs/architecture/adr-channel-access-and-payment-2026-08.md`；
- 频道类型、是否需要邀请、是否需要价格、订单状态、退款后权益的矩阵；
- API 兼容和迁移策略；
- 明确哪些后续步骤可以执行，哪些必须等待产品确认。

### 验收标准

- 文档明确选定模型和拒绝模型；
- 后端、客户端、管理端现有行为与选定模型逐项对照；
- 如果选择模型 3，列出新增字段/类型和旧数据迁移方案；
- 未得到产品确认时，执行状态必须为 `BLOCKED`，不得修改权限或订单逻辑。

### 停止条件

- 频道类型含义无法由现有代码和产品负责人确认；
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

