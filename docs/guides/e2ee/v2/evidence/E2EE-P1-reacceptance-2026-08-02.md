# E2EE-P1 重验证据（2026-08-02）：E2EE-012/024/025 重新签发为 PASS

> **性质**：验收重签（Re-acceptance after ruling）
> **前置裁定**：`E2EE-P0-ruling-2026-08-02.md` R1——三任务由 PASS 回退 PARTIAL，
> 在生产路径重新验收；22 号状态机已补 `PASS -> PARTIAL` 人工裁定通道。
> **验收边界**：`E2EE-012-024-025-029-reacceptance.md`（旁路验收不采信，
> 必须以生产入口 `decryptInboundV3` 与真实投递帧为准）。
> **执行**：Claude Code，2026-08-02，imboy HEAD `fa07b133` / imboyapp HEAD `54d0d17f`

---

## 1. 重验电池（全部实跑，非静态推断）

### 1.1 生产路径关键文件（7 个，40/40 绿）

| 文件 | 结果 | 覆盖任务 |
|---|---|---|
| `production_inbound_frame_gate_test.dart` | **11/11 绿** | 012 重签门（真实投递帧正向可用性+接线守护+fail-closed 负向） |
| `production_session_ref_wiring_test.dart` | **9/9 绿** | 025（方案 A session_ref=真实 Olm session id，RC-01/02 转绿） |
| `mutation_matrix_test.dart` | **1/1 绿** | 024 负向（mutation 全拒） |
| `replay_counter_epoch_test.dart` | **4/4 绿** | 025（ADR 26 选项 C counter/epoch 语义） |
| `fan_out_per_device_test.dart` | **6/6 绿** | 029（多设备 fan-out 只取本机信封） |
| `v3_receive_path_e2e_test.dart` | **4/4 绿** | 接收路径端到端 |
| `decrypt_on_read_v3_gap_test.dart` | **5/5 绿** | A2-b 离线 decrypt-on-read 接线 |

### 1.2 完整套件

| 命令 | 结果 |
|---|---|
| `flutter test test/service/e2ee/` | **588/588 绿** |
| `dart analyze lib` | 1 issue = 既有 info（`component/ui/ios_settings_ui.dart`，与 E2EE 无关，重验文档 §4 已登记） |
| `make e2ee-verify`（imboy） | **387/387 绿**（含 `e2ee_sender_device_envelope_tests` 7 项后端守护） |

## 2. 逐项重签结论

| 任务 | 回退原因（原判定缺陷） | 本次重签依据 | 结论 |
|---|---|---|---|
| **E2EE-012** | 原验收对象在生产 WS 路径未接线；evidence 自记「改测试对齐 sessionRef」，判定过程不可采信 | 以 `production_inbound_frame_gate_test`（生产真实投递帧门）重新签发：正向可读+接线守护+伪造 sender_did 拒收 | **PASS** |
| **E2EE-024** | 「100% Mutation Rejection Rate」在拒绝所有消息的实现上恒成立，缺正向半 | 负向=mutation_matrix（真实现全拒）；正向半=production_inbound_frame_gate 正向可用性门（2026-07-28 补建） | **PASS** |
| **E2EE-025** | 原 PASS 建立在旁路；生产曾 `context_mismatch_session_id` 整条拒收 | 方案 A（session_ref=真实 Olm session id，2026-07-28 人工签字）已实施，RC-01/02 转绿；counter 语义 ADR 26 选项 C 由 replay_counter_epoch 4 例锁定 | **PASS** |
| E2EE-029 | （未回退，登记残余） | fan_out_per_device 6 例生产姿势绿；**残余**=2用户×3设备真机矩阵（P2-4 腿） | 维持 PASS |

## 3. 未闭合残留（登记，不阻塞本次重签）

1. **真机双端未验证**——本次全部结论在单测/真 PG 层成立；真机腿在 P2（人工门）。
2. **行为级接线守护未建**——`_receiveMessage` 副作用链未解耦（候选任务 B），当前为结构级守护。
3. **C2G PFv3 未覆盖**——C2G 走 Megolm v2 不受影响；若 C2G 上 PFv3，`msg_c2g_logic` 需同步接 `with_sender_device`。
4. **迁移 48/v25 之前的旧离线行永久不可读**——fail-closed 设计选择，无回填路径（已在 22 号文件登记）。

## 4. 状态回写

22 号文件 §5.2：E2EE-012/024/025 由 `PARTIAL` 回签 `PASS`（本文件为重签依据）。
Gap Matrix X1 关闭。
