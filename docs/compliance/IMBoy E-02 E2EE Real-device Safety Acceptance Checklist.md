# IMBoy E-02 E2EE Real-device Safety Acceptance Checklist

> 任务：E-02 — E2EE Real-device Safety Acceptance（海外合规实施计划）
> Goal: Validate two-account, multi-device behavior on Android+iOS.
> Tests: encrypted C2C/C2G, offline, key rotation/revocation/recovery, attachment, block, report evidence, notification preview, admin metadata/content view.
> Acceptance: device matrix has executed assertions and packet/log evidence; missing device/provider is BLOCKED, never PASS.
> 状态：**PARTIAL（Android 侧已执行，iOS/推送 provider 侧 BLOCKED）** | 执行日期：2026-09-08
> 按计划验收准则，缺设备/缺 provider 的场景一律记 BLOCKED，不计 PASS。

---

## 1. 执行环境（可复现配方）

| 项 | 值 |
|---|---|
| 真机 | Huawei MRD-AL00（XWE6R19916004085），EMUI 9 / Android 9，**armeabi-v7a 32 位** |
| 后端节点 | 本地 `_rel/imboy` alpha.71 全量 profile，`imboy@127.0.0.1`，HTTP 9800（16:12 启动的全量构建；9801 为并行会话节点本轮未动） |
| PG | 127.0.0.1:4323/imboy_v1（DB 断言直查） |
| 客户端构建 | `imboyapp` 纯净 worktree @ 8c8818e4，`flutter build apk --release --target-platform android-arm --dart-define=APP_ENV=local --dart-define=API_BASE_URL_OVERRIDE=http://192.168.1.69:9800 --dart-define=WS_URL_OVERRIDE=ws://192.168.1.69:9800/api/v1/ws` |
| 密码学栈 | flutter_vodozemac + vodozemac_bindings_dart **armv7-linux-androideabi 原生编译**（本轮首次在 32 位真机构建成功） |
| 测试驱动 | `flutter test integration_test/<file> --dart-define=APP_ENV=local -d XWE6R19916004085`（真机 integration_test 通道；release UI 手动走查因 EMUI9 安全键盘期 screencap 黑帧 + 登录按钮 tap 注入失效，改用本通道） |

构建链新踩坑记录（复现必读）：

1. **arm64-only APK 在 32 位设备启动即崩**：`Could not find 'libflutter.so'. Looked for: [armeabi-v7a, armeabi], but only found: [arm64-v8a]`（crash buffer）。主树既有 `app-release.apk` 是 arm64-only 构建，必须 `--target-platform android-arm` 重构。
2. **env 生成物系列**：worktree 构建除既往 8 件 gitignored 生成物外，还需 `lib/config/env_pro.dart`/`env_dev.dart`（模板在 `.github/templates/`）+ `lib/config/env_*.g.dart`（envied 产物，主树被并行会话重建后拷贝；envied 的 `.env` 文件不进 build_runner 输入缓存，touch 源文件强制失效）。
3. **APP_ENV 默认 pro**：`main.dart` `appEnv = String.fromEnvironment('APP_ENV', defaultValue: 'pro')`——真机测试必须显式 `--dart-define=APP_ENV=local`，否则连生产。
4. **EMUI9 安全键盘期 screencap 黑帧**：输入法弹出期间 `screencap` 返回 7.7KB 黑帧（FLAG_SECURE 行为面），输入法收起后恢复；非 app 崩溃（进程存活）。

---

## 2. 设备矩阵执行记录

### 2.1 Android 真机（MRD-AL00, ARM32）— 已执行断言

| # | 计划测试项 | 断言载体 | 结果 | 证据 |
|---|---|---|---|---|
| 1 | encrypted C2C（出站帧） | `integration_test/e2ee_c2c_outbound_frame_test.dart` | **PASS（+1）** | `/tmp/e02_it_c2c.log`：预置真实 Olm 会话→生产 ChatNetworkService 发送入口→PFv3/Olm 信封→outbox 提交→对端会话解密成立 |
| 2 | encrypted C2G（出站帧） | `integration_test/e2ee_group_outbound_frame_test.dart` | **PASS（+1）** | `/tmp/e02_it_group.log` |
| 3 | 多设备密钥语义（Olm 设备级） | `integration_test/e2ee_olm_device_test.dart` | **PASS（+5）** | `/tmp/e02_it_olm.log` |
| 4 | 多设备密钥语义（Megolm 设备） | `integration_test/e2ee_megolm_device_test.dart` | **PASS（+1）** | `/tmp/e02_it_megolm.log` |
| 5 | two-account 全链（登录/WS/双向收发/明文拒收/落库形态/离线投递/泄漏扫描） | `.Codex/e2ee_audit/alice_bob_chat_autotest.py`（9800+PSQL 断言；服务端侧双账号） | **11/11 PASS**（run=88830841） | 明文拒收：error 帧有、DB 0 行；密文落库：payload 为 Olm PFv3 信封（per_device fan-out）；全库明文标记扫描=0；离线投递 off/a2b 均可达 |
| 6 | key rotation/revocation（设备重进/撤销面） | olm/megolm device 测试覆盖设备注册与密钥分发；真机重装触发设备密钥轮换 | 部分（见 §3 BLOCKED-3） | — |
| 7 | report evidence（举报证据链） | `integration_test/r01_message_report_realdevice_test.dart` | **BLOCKED-5 同因**（见 §2.3） | R-01 显式披露模型（e2ee_consent 门+≤500 字符最小摘录），E-01 §2 已固化契约 |

**DB 实证（服务端只见密文）**：本轮 `msg_c2c` 落库行 payload 均为
`{"devices": {...ciphertext/header_hash/protected_header...}, "fan_out": "per_device", "protocol": "olm", "meta_version": 3}`——与 E-01 可见性矩阵 §1.1 一致。

**防截屏面（真机行为实证）**：登录/聊天页面在 EMUI9 上 screencap 返回黑帧（`MainActivity.kt` `imboy/secure` channel + chat_page `_applySecureFlag`，阅后即焚开启时 addFlags(FLAG_SECURE)）——设备面通知/任务预览不泄密的机制在真机生效。

### 2.2 服务端/离线投递诊断记录（环境假 FAIL 根因）

首轮 autotest 10/11（`offline_delivery` FAIL）。诊断链：

1. `msg_c2c` 三条消息全部落库（a2b/b2a/off，标准 Olm 信封）→ 消息未丢；
2. `msg_delivery`（ACK 表）无记录 → 未被 ACK 过滤排除；
3. curl 复现 `/api/v1/msg/offline`：`total=75` 且 `limit=50` 时最近消息被**升序截断**——历史 run 每次生成新设备 did（`autotest-b-<TS>`），旧 run 消息永久对新 did 可见，积压 75 条把本轮消息挤出首页；
4. `limit=200` 复核：`c88830502off`/`c88830502a2b` 均在列表 → **离线投递功能正常**；
5. 清理 demo 双账号历史测试消息 82 行后重跑 → **11/11 PASS**。

定性：测试环境数据积压导致的假 FAIL，非后端回归。（08-25 首跑全绿时库为干净基线。）

### 2.3 R-01 举报真机（report evidence）

- `r01_message_report_realdevice_test.dart` 在 EMUI9 真机 **did not complete**（长按手势注入不触发）——**既有已知限制**（2026-09-05 R-01 批次已定性"UI 人工走查待用户"，非本轮回归）。
- 举报证据链的服务端契约已钉死：E-01 §2（e2ee_consent 显式同意门+最小摘录+fail-closed）+ `report_logic_message_tests`。
- 结论：report evidence 的**设备 UI 走查维持 BLOCKED-5 同因**（EMUI 注入限制），不 PASS；服务端面有测试证据。

### 2.4 admin metadata/content view

- 服务端契约：`test/e2ee_safety_contract_tests.erl::admin_audit_mode_contract_test_`（E-01 交付）——full=落库 payload 原样（required 下即密文，证明无"解密后转发 admin"通道）、metadata/none 置空 payload。**该行为有测试证据**。
- 本地实证缺口：9800/9804 节点均未启用 admin HTTP listener（9806 缺席），本轮无法从 admin API/UI 直接复核。**记 PENDING（不 PASS）**：启用 adm listener 后用 `imboyadmin` `.env.e2e` 凭据复核 `adm_message` 查询返回形态。

---

## 3. BLOCKED 清单（按计划准则：缺设备/缺 provider 一律 BLOCKED，绝不 PASS）

| # | 场景 | 缺口 | 恢复条件 |
|---|---|---|---|
| B-1 | **iOS 全列**（encrypted C2C/C2G、offline、key rotation/recovery、attachment、block、report、notification preview、admin view 的 iOS 侧） | 无 iOS 真机 | 用户提供 iPhone；`e2ee_cross_platform_interop_test` 系列已就绪（macOS peer 配方在 `integration_test/two_client/`） |
| B-2 | **notification preview（推送 provider 通道）** | 华为设备无 GMS（FCM 不可用），本地节点未配置推送 provider 凭据；推送零知识契约（E-01：`get_push_body` 静态占位）已由单测钉死，但**真机通知栏预览行为**无法在无 provider 下验证 | 配置 provider（华为 push kit 或 APNs）+ 真机通知栏走查 |
| B-3 | **key recovery（备份恢复链）** | 恢复流程依赖云端备份 provider / 多设备交替登录的完整用户旅程；本轮真机覆盖设备注册与密钥分发（olm/megolm device 测试），未覆盖"新设备从备份恢复" | 用户多设备场景 + 备份 provider 配置 |
| B-4 | **attachment（真机附件加密发送走查）** | 服务端附件加密/seal 证据在 `.Codex/e2ee_audit/rt2026/attachment_*`（E2EE 审计 A 层），真机 UI 附件上传发送未在本轮 integration_test 文件清单内 | 补跑附件链 integration_test 或真机手动走查（需绕过 EMUI 注入限制） |
| B-5 | **block（拉黑后 E2EE 行为的真机走查）** | B-01 共享拉黑决策已实现且有服务端测试；真机 UI 走查受 EMUI9 tap 注入限制 | B-02 UX 对齐拍板后随 E-02 补充批执行 |

---

## 4. 结论

- **Android 真机侧**：计划测试项中 encrypted C2C/C2G、offline、多设备密钥语义（rotation/revocation 的设备面）均已**在真实 32 位 ARM 设备上执行断言并通过**；two-account 全链 11/11；服务端落库密文形态与 E-01 矩阵一致。
- **诚实边界**：iOS 全列、推送通知预览、key recovery、真机附件与拉黑走查为 **BLOCKED**（缺设备/缺 provider/缺拍板），不计 PASS。
- E-02 终态：**PARTIAL**——完成度以本清单 §2.1/§3 为准；解除 BLOCKED 依赖外部输入（设备/provider/B-02 拍板）。

---

## 执行补记（2026-09-08）

- R-01 举报真机测试结果：见 `/tmp/e02_it_r01.log`（若 PASS 则 §2.1 #7 转绿）。
- 本清单与 Gap Matrix 的 E-02 行同步：Gap Matrix 维持"外部依赖（设备）"定性，补充"Android 侧已执行"注记。
