# imboy E2EE 可见性矩阵（E2EE Visibility Matrix）

> 版本：v1.0 | 最后更新：2026-09-07
> 任务：海外合规计划 E-01 — E2EE Safety Contract（"Document and test what server, admin, push, logs and report flows can see"）
> 关联文档：[e2ee-policy.md](./e2ee-policy.md)（e2ee_mode 四态与 compliance 托管披露）、[IMBoy A-01 Privileged Message Access Hardening Checklist.md](./IMBoy%20A-01%20Privileged%20Message%20Access%20Hardening%20Checklist.md)（管理端内容访问工单制）、`docs/security/audits/e2ee-2026-09-07/`（红队审计 v2.0）
> 适用：imboy 后端 + imboyadmin + imboyapp

---

## 0. 结论（TL;DR）

- **`e2ee_mode=required`/`compliance`（或 `storage_mode=*_e2ee`）部署下，服务器任何组件（持久化、管理端、日志、推送、AI、审核、全文检索）能接触到的消息内容只有密文。** 服务端没有用户私钥、没有 compliance 私钥（compliance 私钥只在审计方本地，见 e2ee-policy.md §3.3），因此**不存在**任何"服务端解密后转发"的通道。
- 明文进入服务端唯一合法路径是**举报面**（R-01）：举报人**显式勾选同意**（`e2ee_consent=true`）后提交的**最小明文摘录**（≤500 字符）+ 哈希/上下文元数据。无同意时服务端拒绝摘录（fail-closed）。
- AI（平台 agent）与自动审核（moderation）**默认不接收 E2EE 明文**：agent 读到的群消息 payload 是密文（无法理解，不产生有效回复）；审核面只接入公开内容（频道帖子/动态），C2C/C2G 绝不进入审核管道。

---

## 1. 可见性矩阵

图例：**明文** = 可读原文；密文 = E2EE 密文（服务端不可读）；元数据 = ID/时间/收发双方/类型等；— = 不适用。

### 1.1 按流程 × 部署模式

| 流程 | required / compliance 部署：服务端可见 | optional / disabled 部署：服务端可见 | 代码锚点 |
|---|---|---|---|
| C2C 消息 | 顶层 `e2ee` 信封 + 密文 payload（v2.0：`payload="base64(nonce).base64(ct)"`；PFv3：`payload=""` + `e2ee.devices` 逐设备信封） | 消息明文（设计内，部署方自选） | `imboy_policy:encrypted_message_body/3`、`msg_c2c_logic.erl` |
| C2G 消息 | 同上，另有**群级 fail-closed 门**（群 `e2ee_mode=1` 拒明文，配置查询失败同样拒发） | 同上 | `msg_c2g_logic:group_e2ee_gate/5` |
| 消息编辑 | 编辑入站经 `message_edit` 门（密文才放行）；E2EE 编辑只读 `e2ee.edit_of` 做权限/时间窗校验，正文密文原样转发 | 明文可读 | `msg_c2g_logic:encrypted_edit_policy/4`、`handle_encrypted_group_edit/4` |
| 附件 | 客户端加密后上传，服务端/对象存储（Garage S3）只见密文对象；预签名 URL 绑定 MIME | 同左（附件加密与消息模式独立） | `test/attachment_cipher_tests`、`e2ee_presign_mime_binding_tests` |
| 多设备 | 服务端只中转逐设备公钥 wrap 的信封（每设备一个 `ek`），无私钥 | 同左 | `e2ee_sender_device_envelope_tests`、`olm_identity_*` |
| 离线消息 | 密文落库、密文投递（拉取同步原样转发） | 明文落库 | `msg_store_ds`、`e2ee_offline_sender_did_tests` |
| 备份 | 用户自有密钥加密的备份对象，服务端零知识；恢复需用户口令/密钥 | 同左 | `e2ee_backup_logic_tests`、`e2ee_recovery_logic_tests` |
| 全文检索（fts） | 服务端 pg_jieba 全文检索对密文**无效**（搜不到词，也不泄漏）→ E2EE 会话内搜索由客户端在本地 SQLite 做 | 服务端可检索明文（设计内） | `fts_user_repo:search_c2c_msg/4` |

### 1.2 按消费方（required 部署下）

| 消费方 | 能看到什么 | 门槛/约束 | 代码锚点 | 测试 |
|---|---|---|---|---|
| **服务器（持久化/转发）** | 密文 + 元数据；明文在入口即被拒收（`encrypted_message_required`），拒收零日志 | 部署级门 + 群级门双层 | `imboy_policy:validate_message_write/5` | `e2ee_safety_contract_tests`、`msg_c2c_logic_tests`（real-policy 组）、`msg_c2g_logic_tests` |
| **管理端（imboyadmin）** | 默认仅元数据（payload 置空）。内容读需同时满足：policy `audit_mode=full` + `messages:content:read` 权限 + 有效举报工单 + 处理原因 + 逐次审计（fail-closed）；且 full 所见=落库 payload 原样=**密文** | A-01 工单制 | `adm_message_handler:sanitize_row_by_audit_mode/2`、`authorize_message_access/2`、`resolve_content_access/3` | `adm_message_handler_tests`、`e2ee_safety_contract_tests:admin_audit_mode_contract_test_` |
| **日志（lager）** | 消息链路日志只有 ID/状态元数据；release 与 eunit 构建均未定义 `debug` 宏，`?DEBUG_LOG` 打点恒零输出；任何经 `elib_log` 的内容先过 `log_redact`（键+值双层脱敏，fail-closed 落 `[REDACT_ERROR]`） | V-02 sink 收敛 | `include/log.hrl`、`elib_log:safe_log/*`、`log_redact` | `e2ee_safety_contract_tests`（日志哨兵两例）、`log_redact_tests` |
| **推送** | Title=发送者昵称（元数据）；Body=**静态类型占位**（text→"发来一条消息"、e2ee→"发来一条加密消息"、image→"[图片]" 等）。`maybe_push_for_c2c/4` 的 payload 参数显式忽略（`_Payload`），**永不携带消息内容或密文** | 推送零知识不变量 | `push_notification_logic:get_push_body/1`、`maybe_push_for_c2c/4` | `push_notification_logic_tests:e2ee_push_body_never_leaks_ciphertext` / `e2ee_v2_push_body_generic` / `e2ee_c2g_push_body_never_leaks` |
| **举报（report）** | 仅举报人**显式同意**（`e2ee_consent=true`）时接受 `content_excerpt`（≤500 字符最小摘录）+ 哈希/上下文元数据；服务端不解密、不索取会话密钥、不读无关消息；无同意带摘录 → 拒绝 | R-01 显式披露 | `report_logic:create_message_visible/7`（`e2ee_consent` 门）、`finalize_evidence/4` | `report_logic_message_tests` |
| **AI / 平台 agent** | agent 作为群成员读到的 payload 与普通成员客户端相同——required 下是**密文**，无法理解，不产生有效回复；agent 主动回复是明文 text，但**同样要过 C2G 加密门**——required 下被拒发（fail-closed，拒绝而非降级） | 计划红线"AI/moderation cannot receive E2EE plaintext by default" | `ai_agent_group_reply.erl`、`ai_agent_proactive:send_text` 自带同款门 | `ai_agent_reply_tests`（required 拒发而非降级）、`ai_agent_proactive_tests` |
| **自动审核（moderation）** | 只接入**公开面**：`channel_message` / `moment_post` 两个 surface；决定性关键词规则、无 AI provider；**C2C/C2G 私信路径绝不接入本模块** | surface 白名单 | `moderation_policy`（模块头契约注释） | R-03 审核 queue 测试 |
| **导出（P-01）** | 用户导出 categories 明确**排除** messages/attachments——E2EE 消息不进入服务端导出载荷 | scope 声明 | `user_export_logic:scope/0` | `user_export_logic_tests` |

---

## 2. 明文进入服务端的唯一合法路径（举报面）

```
客户端（举报人设备，持有会话私钥，本地解密）
  │  用户点举报 → 客户端展示同意弹窗
  │  用户显式同意 → 客户端构造最小证据
  ▼
POST /api/v1/report/message
  { chat_type, target_id, scope_id, reason,
    evidence: { content_excerpt: "≤500字符最小摘录",
                e2ee_consent: true,
                msg_id, hash, ... } }
  ▼
服务端 report_logic：
  · IsE2EE ∧ Excerpt≠"" ∧ e2ee_consent≠true → 拒绝（"提交加密消息内容需要您的明确同意"）
  · 核验举报对象存在、举报人可见权（收发双方/群成员/频道订阅者）
  · 落库 evidence jsonb（含摘录+哈希+内容态）
```

- 服务端**不参与解密**：摘录由举报人在自己设备上解密后自愿提交。
- 管理端查看该工单 evidence 仍受 A-01 审计模式约束（metadata 模式 payload/evidence 脱敏，full 需工单+原因+审计）。
- 该设计的合规定性：**用户显式披露（explicit user disclosure）**，不属于服务端破解 E2EE。

## 3. 验收条款与测试映射

计划验收："tests fail if plaintext reaches server logs/admin/push in required mode."

| 面 | 契约 | 测试（全部现存可跑） |
|---|---|---|
| logs | 明文哨兵消息被真实 policy 门拒收后，lager 全量捕获无哨兵；零落库/投递；S2C 回执不回显内容 | `test/e2ee_safety_contract_tests.erl` → `c2c_required_plaintext_never_reaches_logs_test_`、`c2g_required_plaintext_never_reaches_logs_test_` |
| admin | metadata/none 置空 payload；full=落库原样（密文） | `e2ee_safety_contract_tests:admin_audit_mode_contract_test_`、`adm_message_handler_tests`（含 `sanitize_row_by_audit_mode_metadata` 与 init 路径断言） |
| push | Body=静态占位，永不携带 payload/密文 | `push_notification_logic_tests` 三例零知识不变量 |
| （补充）持久化 | 明文 C2C/C2G/编辑在 required 下拒收、零字节落库 | `msg_c2c_logic_tests:real-policy 组`、`msg_c2g_logic_tests` |
| （补充）PFv3 信封 | payload 空串+devices 信封必须放行（防误伤）且日志干净 | `e2ee_safety_contract_tests:c2c_required_olm_envelope_accepted_and_log_clean_test_` |
| （补充）AI | required 下 agent 明文回复拒发而非降级 | `ai_agent_reply_tests` |

运行：`make eunit-local t=e2ee_safety_contract_tests`（6/6 通过，2026-09-07）。

## 4. 边界与已知局限（如实声明）

1. **optional/disabled 部署**：消息明文过服务端是**设计内行为**（部署方选择），本矩阵只对 required/compliance/`storage_mode=*_e2ee` 声明零明文。
2. **TOFU 首钥局限**：compliance 公钥 TOFU 锚定只防"已固定后的服务端偷换"，首次接触的恶意服务端仍可注入首钥（e2ee-policy.md §3.3，根治依赖 Key Transparency，台账 IMB-2026-007）。
3. **客户端侧行为**不在服务端契约内：客户端本地日志/iPrint 已由 V-02 口径约束（`kDebugMode` only），Sentry 事件经 LogRedactor 清洗、面包屑整体禁用。
4. **debug 构建日志**：定义 `debug` 宏的开发构建中 `?DEBUG_LOG` 会输出消息 Data（含当时 payload——required 下即密文）；release/eunit 构建零输出。开发机构建不面向生产数据。
5. **e2ee_mode 四态语义**、**compliance 托管披露**（依法留存通道破坏纯端到端语义）见 [e2ee-policy.md](./e2ee-policy.md)，本矩阵不重复。

## 5. 参考

- 计划：`IMBoy Overseas Compliance Implementation Plan.md` Task E-01
- 契约测试：`imboy/test/e2ee_safety_contract_tests.erl`（E-01 新增汇聚套件）
- 后端策略：`imboy/src/lib/imboy_policy.erl`（`validate_message_write/5`、`encrypted_message_body/3`、`message_encryption_required/0`、`content_bearing_action/1`）
- 管理端访问控制：`imboy/src/adm/adm_message_handler.erl`（A-01）
- 推送零知识：`imboy/src/logic/push_notification_logic.erl`
- 举报显式披露：`imboy/src/logic/report_logic.erl`（R-01）
- 审核面白名单：`imboy/src/logic/moderation_policy.erl`（R-03）
- 日志脱敏：`imboy/src/lib/log_redact.erl`（V-02）
