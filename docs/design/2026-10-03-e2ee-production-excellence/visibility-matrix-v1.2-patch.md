# e2ee-visibility-matrix v1.2 补丁（C17/C17b 对账修正）

> 日期：2026-10-03 | run：e2ee-excellence / run-20261003-094804
> 作者：C17（对账发现）+ C17b（本文档成文）
> 目标文件：`docs/compliance/e2ee-visibility-matrix.md`（v1.1，2026-09-09）
> 形态：**补丁文档（不入主矩阵文件）**——imboy 主仓为共享工作区，
> 合入时机与措辞由 owner 决定；本文按节给出 v1.1 → v1.2 的逐节 diff，
> owner 审核后可直接套用。
> 依据：
> - `docs/design/2026-10-03-e2ee-production-excellence/privacy-profile.md` §2（D1-D7 对账）
> - imboyapp `test/unit_test/service/e2ee/metadata_visibility_inventory_test.dart`（14 例，dfc5acdb）
> - `docs/security/audits/e2ee-2026-09-07/E2EE_AUDIT_REPORT.md` §0（F2/R2/D3/M1 决策记录）
> - imboyapp commit `94cb822b`（2026-09-08，Push 常量化）

---

## 修订总览

| # | 节 | 类型 | 摘要 |
|---|---|---|---|
| D1 | §1.2 推送行 | 更正（文档落后于代码） | Push Title 已常量化（"新消息"），非"发送者昵称" |
| D2 | §1.2 新增行 | 补充声明 | typing 明文控制帧（未声明的元数据出口） |
| D3 | §1.2 新增行 | 补充声明 | 已读回执明文（msg_ids + read_at） |
| D4 | §1.1 C2C 行 | 补充定性 | fan-out `devices` 键（含 `skipped_devices`）= 服务器可见设备清单 |
| D5 | §1.1 C2C/C2G 行 | 补充声明 | 顶层 `msg_type` 明文（内容形状泄漏） |
| GL-23 | §4.6 | 状态更新 | "待 F/R/D/M 产品决策" → 已决策 `F2/R2/D3/M1/AI-ID=B`（引用审计 §0） |

---

## D1 — §1.2 推送行：Title 声明更正

**v1.1 现文（§1.2 推送行，"能看到什么"列开头）：**

> Title=发送者昵称（元数据）；Body=**静态类型占位**（text→"发来一条消息"、e2ee→"发来一条加密消息"、image→"[图片]" 等）。……

**v1.2 建议文：**

> Title=**静态常量「新消息」**（`94cb822b`，2026-09-08，"离线推送恒用固定 title/body 封锁元数据泄露"；FCM/APNs/JPush 三 provider 同口径，锚点 `push_notification_logic.erl` `?PUSH_TITLE`）；Body=**静态类型占位**（text→"发来一条消息"、e2ee→"发来一条加密消息"、image→"[图片]" 等）。`maybe_push_for_c2c/4` 的 payload 参数显式忽略（`_Payload`），**永不携带消息内容或密文**

**理由**：v1.1（09-09 修订）未同步 09-08 的常量化提交；代码比文档声明**更最小化**（连昵称也不给 Push provider）。守护测试：`push_notification_logic_tests` 三例 + imboyapp `metadata_visibility_inventory_test.dart`。

---

## D2/D3 — §1.2 新增行：typing 与已读回执明文控制帧

**v1.1 现文**：§1.2 表无此两行（矩阵未声明的元数据出口）。

**v1.2 建议文**（插入 §1.2 表，位于"日志"行之后）：

> | **控制帧：typing** | `action=message_input` **明文**帧（from/to/status），3s 节流；服务器与群内全员可见输入节奏（行为画像侧信道） | 明文控制帧（设计内、未最小化） | imboyapp `typing_indicator_rules.dart`、`chat_network_service.dart` sendInputStatus | imboyapp `metadata_visibility_inventory_test.dart`（typing 明文锚点例） |
> | **控制帧：已读回执** | `action=messageRead`，payload=`{msg_ids, read_at}` **明文**；服务器持久化并供 read_stats 聚合，读行为时序可见 | 明文控制帧（设计内、未最小化；批量化/延迟上报属最小化候选） | imboyapp `buildReadReceiptItem` | 同上（已读回执明文锚点例） |

**定性说明**（可并入 §4 边界）：E2EE 契约只覆盖**内容帧**；typing/已读控制帧是明文控制面，泄漏的是行为元数据（何时在输入、何时已读），非消息内容。最小化路径见 privacy-profile.md §3（typing 定间隔化 / 已读批量+延迟），属后续实施项，不改变本矩阵"零明文内容"结论。

---

## D4 — §1.1 C2C 行：fan-out 设备清单定性

**v1.1 现文（§1.1 C2C 行，required 列）：**

> 顶层 `e2ee` 信封 + 密文 payload（v2.0：…；PFv3：`payload=""` + `e2ee.devices` 逐设备信封）

**v1.2 建议文（句尾追加）：**

> ……逐设备信封。注意 `devices` 的**键 = 对端设备 ID 清单**，另有 `skipped_devices`（套件门卫跳过项）同层明文：服务器可直接读出对端**设备数与设备 ID**（设备关系图谱元数据；密文逐设备 wrap，但"给谁 wrap"不保密）

**理由**：v1.1 只把 devices 当加密结构描述，未定性为元数据出口。最小化候选（密文共享+仅 wrap key / 键哈希化）见 privacy-profile.md §3 P2。

---

## D5 — §1.1 C2C/C2G 行：顶层 msg_type 明文

**v1.1 现文**：§1.1 元数据口径笼统（"元数据 = ID/时间/收发双方/类型等"图例已含类型），但矩阵行未列 `msg_type`。

**v1.2 建议文（§1.1 图例句扩充）：**

> 图例：……元数据 = ID/时间/收发双方/**消息类型（顶层 `msg_type` 与 PFv3 `protected_header.message_type`，明文——内容形状可推断：语音 vs 文本 vs 图片）**/会话引用等

**理由**：msg_type 是 v2.0 API 契约（服务端路由用），required 模式下仍明文；并入加密语义类型需后端 v2.1 API 版本协商（privacy-profile.md §3 P2，跨仓建议）。

---

## GL-23 — §4.6：C2G 群历史边界决策状态更新

**v1.1 现文（§4.6）：**

> 6. **C2G 群历史边界（E2EE-2026-012）**：`/msg/history` 与批量 sync 目前只按当前 active membership 开放整个群归档，缺不可变 join boundary；新成员/重入/新设备历史策略待 F/R/D/M 产品决策（决策未落定，决策包为历史计划文档已移除），决策与实现完成前该面为已知安全设计缺口。

**v1.2 建议文：**

> 6. **C2G 群历史边界（E2EE-2026-012）**：历史策略的 F/R/D/M 产品决策**已落定**——用户于 2026-09-11 书面确认 `F=F2, R=R2, D=D3, M=M1, AI-ID=B` 并在风险说明后回复"确认接受"；2026-09-12 进一步确认 D3 细节：archive ciphertext 与 historical room-key grant **分开授权**（账号按获权 generation 下载归档密文；historical room key 仅经显式、限范围、可审计的恢复授权）。决策记录与本地实现候选（migration 112 server-authoritative session attestation、D3 grant 生命周期，scratch PostgreSQL 真库通过）见审计报告 §0——**决策已记录 ≠ 实现完成**：生产规模 DDL/cutover、旧客户端 rollout、真实设备与 A 级攻击复测（`A_LEVEL_ATTACK_RETEST=BLOCKED`）未闭环，发布结论维持 `E2EE_RELEASE=NO-GO`；该面在 A 级复测完成前仍按已知缺口对待。

**理由**：v1.1 写作时（09-09）决策确实未落定；09-11/09-12 决策与 D3 细节已固化于审计报告 §0（唯一事实源），矩阵 §4.6 应同步，避免读者以为决策仍悬置。

---

## 套用说明（owner 操作指引）

1. 按上文逐节替换/插入 `docs/compliance/e2ee-visibility-matrix.md`，版本行改为
   `v1.2（2026-10-03 C17 对账修正：D1 Push 常量化更正 + D2-D5 元数据出口补声明 + §4.6 决策状态更新）| v1.1 / 2026-09-09 | v1.0 / 2026-09-07`。
2. §5 参考可补两行：
   - `docs/design/2026-10-03-e2ee-production-excellence/privacy-profile.md`（C17 元数据 inventory，D1-D7 对账与最小化建议）
   - imboyapp `test/unit_test/service/e2ee/metadata_visibility_inventory_test.dart`（客户端元数据出口锚点测试）
3. 本文（patch 文档）随计划目录管理，不入主仓 git。
