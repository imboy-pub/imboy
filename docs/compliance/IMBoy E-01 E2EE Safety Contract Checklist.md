# IMBoy E-01 E2EE Safety Contract Checklist

> 任务：E-01 — E2EE Safety Contract（海外合规实施计划）
> Goal: Document and test what server, admin, push, logs and report flows can see.
> Files: existing E2EE policy docs/tests, message/report contracts. No E2EE rewrite.
> 状态：**DONE（工程可执行部分）** | 执行日期：2026-09-07
> E-02（真机两账号多设备验收）需 Android+iOS 真机，留待用户执行。

---

## Implementation（计划要求 → 实际交付）

### 1. Visibility matrix（C2C/C2G/attachments/multi-device/offline/backup/push）

- [x] 新建 `docs/compliance/e2ee-visibility-matrix.md` v1.0：
  - §1.1 流程 × 部署模式矩阵：C2C / C2G / 消息编辑 / 附件 / 多设备 / 离线 / 备份 / 全文检索，逐项区分 required（服务端只见密文）与 optional/disabled（明文=设计内），每项带代码锚点。
  - §1.2 消费方矩阵：server / admin / logs / push / report / AI-agent / moderation / 导出，逐项写"能看到什么 + 门槛/约束 + 代码锚点 + 测试名"。
- [x] 矩阵全部条目基于源码勘察（非转述旧文档），关键锚点：`imboy_policy:validate_message_write/5`、`msg_c2g_logic:group_e2ee_gate/5`、`adm_message_handler:sanitize_row_by_audit_mode/2`、`push_notification_logic:get_push_body/1`、`report_logic:create_message_visible/7`、`moderation_policy` surface 白名单、`log.hrl` 的 `debug` 宏条件编译。

### 2. Report evidence is explicit user disclosure

- [x] 勘察确认 R-01 已实现显式披露模型：`e2ee_consent=true` 门 + ≤500 字符最小摘录 + 哈希/上下文元数据；无同意带摘录 fail-closed 拒绝；服务端不解密/不索取密钥/不读无关消息。
- [x] 矩阵 §2 把举报链画成时序（客户端本地解密 → 用户同意 → 最小证据 → 服务端核验落库），定性"用户显式披露，不属于服务端破解 E2EE"。
- [x] 测试证据引用：`report_logic_message_tests`。

### 3. AI/moderation cannot receive E2EE plaintext by default

- [x] AI 面：agent 读到的群消息 payload 在 required 下是密文（无有效回复）；agent 主动明文回复经 C2G 加密门被拒发（fail-closed 拒绝非降级）。证据：`ai_agent_reply_tests`（required 拒发用例）、`ai_agent_proactive_tests`。
- [x] moderation 面：`moderation_policy` 只接 `channel_message`/`moment_post` 公开 surface，模块头契约注释明文"C2C/C2G 私信路径绝不接入"；决定性关键词、无 AI provider。证据：R-03 审核队列测试。
- [x] 两条契约均写入矩阵 §1.2"AI / 平台 agent"与"自动审核"行。

### 4. Acceptance: tests fail if plaintext reaches server logs/admin/push in required mode

三面验收缺口补齐 + 汇聚契约套件（**不 mock imboy_policy，判定链全程真实**）：

- [x] 新建 `test/e2ee_safety_contract_tests.erl`（6/6 通过）：
  - `c2c_required_plaintext_never_reaches_logs_test_` — **日志面（此前零覆盖）**：明文哨兵消息被真实 policy 拒收后，meck lager 全量捕获无哨兵；staging/入队/投递零调用；S2C 回执不回显内容。
  - `c2g_required_plaintext_never_reaches_logs_test_` — 日志面群聊路径同构。
  - `c2c_required_olm_envelope_accepted_and_log_clean_test_` — **PFv3 信封形态此前无真实判定覆盖**：payload 空串 + `e2ee.devices` 信封必须放行（防误伤全站 Olm 消息）且日志无哨兵。
  - `admin_audit_mode_contract_test_` — **admin full 方向此前无断言**：metadata/none 置空 payload（已有测试覆盖），full=落库 payload 原样——required 下即密文，证明服务端不存在"解密后转发 admin"通道。
- [x] push 面不重复造轮：`push_notification_logic_tests` 已有三例零知识不变量（legacy e2ee / v2.0 text / C2G），矩阵与本文档引用。
- [x] 勘察确认日志面生产风险基线：release 与 eunit 构建均未定义 `debug` 宏，`?DEBUG_LOG` 展开恒 true（零输出）；ERROR/WARN/INFO 级打点经 `elib_log → log_redact` 双层脱敏（V-02）。

## Verification

| 项 | 结果 |
|---|---|
| `make eunit-local t=e2ee_safety_contract_tests ERLC_EXCLUDE=agent_task_repo` | **6/6 通过** |
| 回归 `t=msg_c2c_logic_tests` | 34/34 通过 |
| 回归 `t=msg_c2g_logic_tests` | 22/22 通过 |
| erlfmt | 已格式化 |
| E2EE rewrite | 零（未改任何 `src/` 生产代码，仅新增 1 测试文件 + 2 文档） |

## Owner 待办（不阻塞本任务）

1. **e2ee_mode 生产取值**：海外部署须以 `required`（或 `storage_mode=secure_e2ee`）上线；`compliance` 模式须先完成合规私钥托管法务审阅（e2ee-policy.md §3）。
2. **E-02 真机验收**：两账号、多设备、Android+iOS 真机行为验证——需设备，留用户。
3. 矩阵 §4 边界项（TOFU 首钥局限 / optional 部署明文设计内）如需对外披露，对外措辞交法务/产品审定。

## 提交

- imboy：`feat(E-01): E2EE 可见性矩阵与安全契约测试`（本仓 1 测试文件 + 2 文档；只 commit 不 push）
