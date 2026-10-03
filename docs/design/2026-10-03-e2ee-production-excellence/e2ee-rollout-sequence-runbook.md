# E2EE rollout 顺序合同：客户端先发布 → 后端强制（C12 / AC-25）

> 状态：run-20261003-094804 C12 产出 | 顺序是**合同**（不可交换步骤），不是建议。
> 依据：cards/C12/version-gate-investigation.md（版本门调查与最小实现）、
> cards/C12/offline-gates.md（蓝绿部署与迁移门控制流证据）。

## 0. 为什么顺序不可交换

- 服务端先强制（先配 min_vsn 或先开 required E2EE）而客户端未发布新版本：
  存量用户被硬门挡在 E2EE 端点之外（5070）且无可用新版本可升级 → **加密通信整体中断**；
- 版本门实现（`app_version_logic:e2ee_gate/2` + `auth_middleware_api_v1:e2ee_version_gate/2`）
  在**未配置门时零行为变化**，因此「客户端先发布、后端再收紧」期间现网无感。

## 1. 五步 rollout（每步含检查点与中止条件）

### 步骤 1：客户端先发布（T0）

- 发布携带新版 E2EE 协议栈（PFv3 信封、anchor、证明验证）的 imboyapp 到应用市场，
  `app_version` 表登记该版本（vsn / min_supported_vsn **暂不收紧**，保持 0.0.0 或现值）。
- 后端此时：**门全开**（policy.min_vsn 与版本级均未配置或为 0.0.0）。

**检查点（通过才进步骤 2）**
- [ ] `GET /api/v1/app_version/check?vsn=<新版>&cos=android` 返回 `updatable=false`（登记成功）；
- [ ] 新版客户端全量走通：上报身份键 / claim OTK / 群发 Megolm / backup put-get / 恢复演练；
- [ ] 市场分发的最低系统要求与热修能力确认（出问题能停发+撤档，不能只靠服务端）。

**中止条件**：新版任一 E2EE 主流程故障 → 停在这里修复；不进入步骤 2。

### 步骤 2：观测旧端占比（T0 + 自然渗透期，≥7 天或达留存阈值）

- 观测源：`/api/v1/app_upgrade/report`（app_upgrade_log）与 WS `vsn` 头分布
  （websocket_handler 连接期已采集，user_log_ds 落库）。
- 计算旧端（低于计划门）的**日活占比**。

**检查点**
- [ ] 低于计划门的旧端 DAU 占比 ≤ 运营阈值（建议 ≤5%，或业务方书面接受的比例）；
- [ ] `olm_otk_exhausted_total` / `olm_prekey_unavailable_total` 无异常抬升（新版协议未引发 OTK 消耗异常）。

### 步骤 3：配置硬版本门（T1，收紧但不强制 required）

- 后台设置 `app_version_policy.min_vsn`（按平台）= 计划门版本（即步骤 1 发布的版本）。
  也可同时在该版本行设置 `min_supported_vsn`——生效门取两者较高（e2ee_gate 语义）。
- 生效即時：低于门的旧端调用 21 条 `/api/v1/e2ee/*` 与 `/api/v1/group/set_e2ee_mode`
  收到 `code=5070`「客户端版本过低，E2EE 功能要求版本 >= X」；旧端同时继续收到
  WS `app_upgrade`（force）提醒引导升级；**非 E2EE 端点不受影响**（明文消息、好友、群管理照常）。

**检查点（观察 48h）**
- [ ] 5070 拒绝速率 = 预估旧端调用量（无超预期放大）；
- [ ] 升级转化：app_upgrade_log 中旧端占比持续下降；
- [ ] 无 5070 误伤新版本的工单（负例合同：`scripts/test/e2ee_version_gate_test.sh` P1/P2 用例保证等于/高于门放行，边界已锁）；
- [ ] E2EE 核心指标（发送成功率、olm claim 成功率）无回归。

**回滚（降门）**：后台把 min_vsn 调回 0.0.0 即恢复全开——**这只放开「版本门」，不触碰任何
required E2EE 状态**（见 §3 不变量证明）。

### 步骤 4：开启 required E2EE / anchor / 证明验证强制（T2）

- 在版本门已把旧端挡在外面之后，才开启会话/群级 required E2EE 强制与证明验证
  （对应 C06/C07/C01 的强制面；群级 fail-closed 门 msg_c2g_logic:group_e2ee_gate/5）。
- 此时可逐步把存量群切换 e2ee_mode=1（`/api/v1/group/set_e2ee_mode`）。

**检查点**
- [ ] 群加密门拒收（`encrypted_message_required`）数量与剩余旧端调用量吻合（可观测缺口：
  该路径暂无计数器，见 cards/C12/observability-survey.md §3 补丁 1——**建议先落计数器再进步骤 4**）；
- [ ] C2C/C2G 消息成功率、群发 P95/P99 满足 D-07 冻结 SLO（见 cards/C12/slo-measurement-method.md）。

### 步骤 5：收尾固化（T2 + 30 天）

- [ ] 旧端占比趋零，5070 速率归零；
- [ ] 版本门配置（min_vsn）纳入变更管理（任何调整需走评审，防误配置误伤）；
- [ ] 归档本 runbook 的执行记录到 run 证据目录。

## 2. 回滚矩阵（每步的回滚动作）

| 步骤 | 回滚动作 | 恢复时间 | 是否降级安全不变量 |
|---|---|---|---|
| 1 客户端发布 | 市场停发+撤档；服务端无任何状态需回滚 | 小时级 | 不涉及 |
| 2 观测 | 无状态 | — | 不涉及 |
| 3 配置版本门 | min_vsn 调回 0.0.0（单条 UPDATE / 后台表单） | 即时 | **不触碰** required E2EE 状态、密钥、anchor——门只是请求层闸 |
| 4 required E2EE | 按对应面（C06/C07）的回滚程序执行；**不得**通过降低版本门来「回滚」required E2EE（降门只会放进旧端撞墙） | 按对应 runbook | 见 §3 |
| 5 固化 | 无 | — | — |

## 3. 「回滚不降级」安全不变量及证明方式

**不变量 I-1：版本门回滚不放松加密强度。**
证明：门（e2ee_gate）只读 app_version/app_version_policy 两表并返回 allow/block，
不写任何密钥/群/会话状态（源码级：函数无副作用，测试 P3/P4 锁定「门全开」语义=未配置，
而非「关闭 required」语义）。把 min_vsn 调回 0.0.0 后，已开启 e2ee_mode=1 的群仍拒收明文
（group_e2ee_gate 与版本门是两个独立门，各自独立测试覆盖）。

**不变量 I-2：required E2EE 一旦开启，回滚不降级为「允许明文」。**
群级 fail-closed 门（msg_c2g_logic）的回滚路径只有「显式把群 e2ee_mode 改回 0」的
受控操作（有审计），不存在「整体回滚部署就放明文」的路径——部署回滚走蓝绿
（deploy_sequence_test：回滚不重复迁移、不谎报成功），代码版本回退不会绕过群标志。
证明方式：`bash scripts/test/migrate_gate_test.sh` + `bash scripts/test/deploy_sequence_test.sh`
（71/72，唯一 FAIL 为文案断言漂移，安全断言全过）+ group_e2ee_gate 的既有单测。

**不变量 I-3：旧端在任何时刻不能绕过 required E2EE/anchor/证明验证。**
- REST 面：步骤 3 后旧端被 5070 硬拒（负例测试 N1-N3）；
- WS 消息面：群 e2ee_mode=1 拒收明文内容消息（fail-closed，配置查询失败也拒收）；
- anchor/证明验证：C01/C07 域的强制校验不受版本门状态影响（两套独立机制）。
证明方式：`bash scripts/test/e2ee_version_gate_test.sh`（45/45）+ 既有 group 门测试 +
C01/C07 各自合同测试（见对应 cards）。

## 4. 与蓝绿部署/迁移门的衔接（AC-24 联动）

- 步骤 3/4 涉及的后端变更上线走蓝绿：先迁移后切流、迁移失败 fail-closed
  （migrate_gate_test 15/15 证据）；本轮 C01 无 schema 变更、无新迁移。
- 版本门与 required E2EE 的配置变更（数据行变更）不依赖部署重启，即时生效；
  出问题按 §2 回滚矩阵处理，无需回滚部署。

## 5. 未尽事项（BLOCKED 移交）

- 步骤 4 前应先落「群加密门拒收计数器」（observability-survey §3 补丁 1，非 C12 owned）。
- 真实负载下的 SLO 验证需预生产环境（BLOCKED，见 cards/C12/blocked-resources.md）。
