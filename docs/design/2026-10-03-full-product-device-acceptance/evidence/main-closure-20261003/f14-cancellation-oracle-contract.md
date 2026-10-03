# F14 取消支付的后端零写入取证合同

状态：PREPARATION_ONLY；未采集真实设备、请求或数据库证据；不关闭 F14-A01/A02/A03。

## 当前源码约束

- `billing_subscription` 包含 `id, tenant_id, plan_id, owner_uid, status, current_period_start, current_period_end, auto_renew, created_at, updated_at`。
- `billing_invoice.subscription_id` 指向订阅；账单没有独立 owner_uid，归属必须由订阅反查。
- tenant_id 是逻辑分组而非授权依据；同 tenant 的其他账号记录不能归到当前合成身份。
- 客户端取消支付方式面板只 pop；未选择方式时不应调用订阅、生成账单或支付接口。

## 执行前置条件

独立执行者先验证用户资源授权、专属设备租约、预登录合成身份及 expected_uid、隔离后端/数据库指纹、冻结候选和 fixture generation。描述符哈希形状及 runner boolean 不替代这些核验。

后台计费任务及其他写入者须在本次专属 fixture 的已批准运行配置中受控；不得改变共享数据库、生产服务或默认配置。App 启动的推送注册、同步和外部访问必须另有已核验边界。设备入口仍保持禁用，本文不授权运行。

## 采集顺序

1. 先启用已批准的独立请求及数据库审计采集器，验证采集器运行、事件序号连续、无丢弃；先完成 App 启动及套餐加载。
2. 给本次动作分配唯一 run_id、attempt_id、case_id、platform、fixture_generation 和 correlation_id，固定设备、候选、配置与构建摘要。记录同一采集器的起始游标。
3. 用独立只读数据库连接采集 before 快照。按绑定参数 expected_uid 和已核验 tenant_id 选择订阅全部状态，不能只选 active；用 subscription_id 关联全部账单，不能只看第一页或最新一张。
4. 设备打开指定套餐支付方式面板，记录可见面板；点击取消，记录面板消失及 paying=false。不能点击任何支付方式，也不能主动请求 `/billing/cancel`：该端点取消订阅，与关闭 UI 面板不是同一个动作。
5. 等动作完成、该动作未决请求耗尽，再采集 after 快照和结束游标。固定、明确的观察期限结束仍有未决请求时记 BLOCKED，不以超时证明零写入。
6. 保存原始请求事件、数据库写入审计、快照及设备事件，逐文件计算摘要，独立执行者核验时间窗、游标完整性和实例身份。

## 快照必须核对的内容

订阅集按 id 稳定排序，逐字段比较上述所有订阅字段；账单集按 id 稳定排序，逐字段比较 `billing_invoice_repo` 当前 COLUMNS 全集。金额保留整数分，64-bit ID 使用精确十进制表示，不经过浮点数转换。记录 SQL 模板摘要和绑定参数摘要；不保存连接密码、token 或真实个人数据。

before/after 的完整内容应相同。记录数、金额总和或哈希单项相同都不能单独判定零写入。只读 API 返回空对象也不能替代底层完整集：归属拒绝或分页可能掩盖实际变更。

## 零写入必须同时满足的 oracle

- 动作窗口内没有该 fixture/correlation 对应的计费变更请求：subscribe、renew、cancel、usage、invoice/generate、invoice/pay。HTTP 200 或空响应不表示无写入。
- 已核验完整性及覆盖范围的数据库审计中，没有 billing_subscription 或 billing_invoice 的 INSERT/UPDATE/DELETE/TRUNCATE 或相关写入语句/触发器副作用。租约范围内的写入者、后台任务和连接均须纳入覆盖。
- 已存在订阅的 owner_uid/tenant_id 变更、账单重新关联，以及短暂插入后删除，不能通过当前账号过滤或前后快照相同而漏掉。
- before/after 完整快照相同，设备身份及原始动作记录与同一冻结候选、fixture、审计窗口关联。

快照相同但没有完整请求/数据库审计，只能记 `STATE_UNCHANGED_NOT_ZERO_WRITE_PROVEN`。审计缺口、未知写入来源、未决请求或身份不一致记 BLOCKED/FAIL，不手写 PASS。不得修改实例审计配置来补证而绕过资源审批。

## 负例校准与剩余工作

在获授权的独立校准 fixture 中，至少验证：订阅新增；已有订阅更新；账单新增；新增后删除；owner_uid 改变；无事件但采集器断流；别的平台/fixture/candidate 混入。每个坏样本必须被拒绝，原始失败不得覆盖。

剩余：实现并审查独立采集器及其完整性判定；经授权在 Android/macOS 分别执行；填入原计划逐 case/platform 的命令、退出码、业务 oracle、证据路径/摘要与独立审查。当前设备准备入口不自动签署任何原验收 ID。
