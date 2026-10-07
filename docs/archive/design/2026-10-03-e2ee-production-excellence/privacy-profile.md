# C17 — 元数据保护隐私画像（Privacy Profile / Metadata Visibility Inventory）

> 计划：run-20261003-094804（E2EE Production Excellence）
> Worker：C17（元数据保护与持续安全维护——inventory 阶段，不实施流量改动）
> 日期：2026-10-03 | 状态：**现状快照（inventory）**，最小化实施属 C17 后半
> 关联：[AC-34 元数据最小化（before/after oracle + 开销预算）]、[AC-35 fuzz/SBOM 前置]
> 对账基线：`docs/compliance/e2ee-visibility-matrix.md` v1.1（2026-09-09）
> 配套测试：imboyapp `test/unit_test/service/e2ee/metadata_visibility_inventory_test.dart`（14 例全绿）

---

## 0. 结论（TL;DR）

1. E2EE 只保护**内容**。真实消息流中，服务器与被动观察者可见的元数据面
   **大而完整**：收发双方 uid、群 gid、设备图谱（fan-out 的 devices 键 +
   服务端设备密钥端点）、消息类型、毫秒级时间戳、密文长度（≈明文长度，无
   padding）、IP、在线状态、typing/已读控制帧全文。这是**主流 IM（含
   Signal/WhatsApp 公开承认）的同类水平**，但 imboy 目前**未做任何一项**
   元数据最小化（无 padding、无批量化、无 typing 抑制开关）。
2. 与 visibility-matrix v1.1 对账发现 **1 处声明落后于代码**（Push Title 已
   于 94cb822b 常量化，文档仍声明"发送者昵称"）与 **4 处矩阵未声明的元数据
   出口**（typing 帧、已读回执明文、fan-out 设备列表、msg_type 顶层明文）。
3. 威胁排序（P0→P2）：**服务端/被动观察者：密文长度相关性**（内容长度
   泄漏）与**时序/在线时段**（typing + push 判定 + heartbeat）最高价值；
   **恶意群成员**：群成员图谱只读；**Push provider**：已最小化（常量文案）。
4. AC-34 测量方法（本轮定义、不实施）：长度 oracle 用"明文字节数 →
   密文字节数"线性回归 R² 与桶化分布；时序 oracle 用 inter-arrival
   直方图 KL 散度。开销预算以"每消息额外字节数"与"每会话额外消息数"
   两个标量给出。
5. fuzz 缺口：所有 parser（PFv3 CBOR / descriptor / backup / proof /
   handshake）均只有**手工固定恶意用例**，无持续随机化 fuzz；Dart 生态无
   AFL/libFuzzer 等价物，建议手写 seeded 随机腐蚀 harness（成本最低）。
6. SBOM 缺口：imboyapp 372 个锁定依赖包，**无 SBOM 工具接线**（无
   syft/cyclonedx/osv-scanner）；imboyapp 仓**无 SECURITY.md**（后端有）。

---

## 1. 元数据可见性现状矩阵

图例：可见者分三档——**S**=服务器/恶意服务端（含 DB/日志/Push 判定路径）；
**O**=被动观察者（网络链路窃听，TLS 前提下仅见流量特征；TLS 终止点=S）；
**M**=恶意群成员（对 C2G 面）；**P**=Push provider（FCM/APNs/JPush）。
"可最小化"列给方向，代价列为粗粒度评估（实施属 C17 后半）。

### 1.1 身份与关系

| # | 元数据项 | 现状（代码锚点） | S | O | M | 可最小化 | 代价 |
|---|---|---|---|---|---|---|---|
| 1 | 发送者/接收者 uid | WS 帧顶层 `from`/`to`（`chat_network_service.dart` sendWsMsg msg map） | ✔ | (TLS) | — | 极难（路由必需） | 高：需 SEA/PQ 混合网或盲化中继 |
| 2 | 群 gid | 帧顶层 `to` + `conversation_id`（PFv3 header） | ✔ | (TLS) | ✔ | 难 | 高：同上 |
| 3 | 设备关系图谱 | ① C2C fan-out `e2ee.devices` 的键=对端每台设备 ID（`_encryptC2COlmFanOut`）；② 服务端设备密钥端点（claim/OTK）本就登记 uid↔did | ✔ | (TLS) | — | 中 | 中：可改 fan_out 密文批处理（所有设备同密文+逐设备 wrap），隐藏"几台设备" |
| 4 | 群成员图谱 | `group/detail` 返回 member_list（`group_handler.erl`）；群发时 MemberUids 服务端展开 | ✔ | ✘ | ✔(成员可见) | 难（成员可见性是产品语义） | 高：需成员列表加密/分片 |
| 5 | conversation_id | PFv3 `protected_header.conversation_id`（C2C=排序 uid 对，`_c2cConversationId`） | ✔ | ✘(在密文内? 否—header 是 base64 明文) | — | 低价值 | 低：本就派生自 1/2 |

注：PFv3 的 `protected_header` 是 **base64url(canonical CBOR) 明文**——
它被认证（header_hash 进加密域）但**不保密**。header 内 uid/gid/
message_id/session_ref/created_at_ms 全部对服务器直接可读。

### 1.2 时序与行为

| # | 元数据项 | 现状 | S | O | M | 可最小化 | 代价 |
|---|---|---|---|---|---|---|---|
| 6 | 消息发送时刻（毫秒） | 帧顶层 `created_at` + PFv3 `created_at_ms` + 服务端落库时间 | ✔ | (TLS) | ✔(群) | 抖动/量化 | 低：量化到分钟可先做（UX 影响小） |
| 7 | 消息间隔（输入节奏侧信道） | typing 帧 `action=message_input`（**明文**，payload={status}）每 3s 节流发送（`typing_indicator_rules.dart`） | ✔ | (TLS) | ✔ | 抑制/固定化 | 中：关 typing 损 UX；改为定间隔心跳式可保 UX 半损 |
| 8 | 在线时段 | `imboy_syn:count_user`（push 判定用）+ WS 连接/心跳（FrameType 0x01/0x02） | ✔ | (TLS) | — | 难（在线判定是 push 前提） | 高：需恒定心跳+统一 push |
| 9 | 已读时序 | 已读回执 `action=messageRead`，payload=`{msg_ids, read_at}` **明文**（`buildReadReceiptItem`） | ✔ | (TLS) | ✔ | 批量化/延迟 | 中：批量+延迟上报（如每 5min 或退出会话时） |
| 10 | 阅读-到达差 | read_stats API 返回已读数/总数（聚合，不含个体时序）；但回执消息本身见 #9 | ✔ | ✘ | ✔ | 同 #9 | 同 #9 |
| 11 | client_send_ts | 仅**明文分支**进 payload（E2EE 分支 `removeKeys:['client_send_ts']` 移除后加密） | ✔ | (TLS) | — | 已部分处理 | —（required 模式下不外露） |

### 1.3 长度与内容形状

| # | 元数据项 | 现状 | S | O | M | 可最小化 | 代价 |
|---|---|---|---|---|---|---|---|
| 12 | 密文长度≈明文长度 | vodozemac Olm/Megolm 无 padding；PFv3 inner CBOR(base64) 加密，长度线性传导；`payload` 落库长度=密文长度 | ✔ | ✔(流量长度) | ✔ | **padding 分桶** | 中：+0~N 字节/条。建议 2 的幂桶（64B 步进）或 Matrix 风格固定档 |
| 13 | 消息类型 | 帧顶层 `msg_type`（text/image/...）+ PFv3 header `message_type` **明文** | ✔ | (TLS) | ✔ | 并入加密语义类型 | 低-中：`msg_type` 顶层是 v2.0 API 契约（服务端路由用），需评估路由器改造 |
| 14 | 附件大小/尺寸 | 明文分支顶层 payload 含 `size/width/height/duration_ms`；E2EE 分支这些字段在密文内（descriptor） | 明文分支✔ | (TLS) | — | required 模式下已覆盖 | — |

（inventory 测试已断言：发送帧顶层**不出现** text/uri/size/name/width/
height/duration_ms/waveform/thumbhash/file_hash256/mime_type——防将来
把内容字段提升到明文顶层。）

### 1.4 网络与推送

| # | 元数据项 | 现状 | S | O | M | P | 可最小化 | 代价 |
|---|---|---|---|---|---|---|---|---|
| 15 | 源 IP / 连接五元组 | WS/TLS 终止点必然可见；反代/CDN 日志留存 | ✔ | ✔ | — | ✘ | 难 | 高：需中继网络 |
| 16 | Push 触发映射 | 离线判定（#8）→ push；**provider 侧可见 uid↔device token 映射与推送时刻**（=在线时段反演） | ✘ | ✘ | — | ✔ | 难（provider 信任是外部前提） | 高：自建 push / 统一推送 |
| 17 | Push 内容 | Title=`"新消息"`、Body=`"发来一条消息"`（**常量**，`push_notification_logic.erl` ?PUSH_TITLE/?PUSH_BODY；FCM/APNs/JPush 三 provider 同口径） | ✘ | ✘ | — | ✔(仅常量) | **已完成** | — |
| 18 | 离线拉取行为 | `/api/v1/msg/offline` 带 `did` 查询参数（按设备过滤）+ c2c/c2g/s2c last_msg_at 游标 | ✔ | ✘ | — | ✘ | 低价值 | 低（did 本就服务端登记） |

### 1.5 会话与密钥管理元数据

| # | 元数据项 | 现状 | S | 可最小化 | 代价 |
|---|---|---|---|---|---|
| 19 | session_id / session_ref | Olm: `protocol_metadata.session_id`；Megolm: `session_id`+`gid`（均明文随帧） | ✔ | 低价值（会话连续性对服务器本可从流量关联） | — |
| 20 | 密钥轮换时刻 | Megolm room key 分发（to_device）+ OTK refill 请求 | ✔ | 难 | 中 |
| 21 | 设备增删 | 设备密钥注册/吊销端点 | ✔ | 难 | — |
| 22 | epoch_or_counter | PFv3 header（Megolm 消息序号，明文） | ✔ | 低价值 | — |

---

## 补记（集成阶段）

- `e2ee.skipped_devices`（C07 逐设备套件门卫跳过项）随 fan-out 信封顶层明文：与 `devices` 键同为服务器可见设备 ID 集合，计入设备关系图谱面（P2 同级）。

## 2. 与 e2ee-visibility-matrix.md v1.1 对账差异清单

| # | 文档声明 | 代码现状 | 判定 |
|---|---|---|---|
| D1 | §1.2 推送行："Title=**发送者昵称**（元数据）" | `94cb822b`（2026-09-08）"离线推送恒用固定 title/body 封锁元数据泄露"：Title=`"新消息"`、Body=`"发来一条消息"`，FCM/APNs/JPush 三 provider 一致，有 `push_notification_logic_tests` 等契约测试 | **文档落后于代码**（代码比声明更最小化）。文档 v1.1（09-09 修订）未同步 09-08 的常量化提交 |
| D2 | 矩阵无 typing 行 | `sendInputStatus`：明文控制帧（from/to/status），3s 节流，群内全员可见 | **未声明的元数据出口**（建议矩阵补行） |
| D3 | 矩阵无已读回执行 | `buildReadReceiptItem`：msg_ids+read_at 明文，服务器持久化并供 read_stats 聚合 | **未声明的元数据出口** |
| D4 | §1.1 C2C 行提到 "e2ee.devices 逐设备信封"，但未把 devices 键=设备清单本身定性为元数据 | fan-out 信封 `devices:{peerDid: envelope}`：服务器直接读出对端设备数与 ID | **定性缺失**（矩阵只把它当加密结构描述） |
| D5 | §1.1 称元数据可见，但未列 msg_type | 帧顶层 `msg_type` 明文（text/image/voice/...），可推断内容形状 | **未声明项** |
| D6 | §1.2 日志行/举报行/AI 行/管理端行 | 抽查一致（`log_redact`、`report_logic` e2ee_consent 门、moderation surface 白名单均如声明） | **对账通过** |
| D7 | §4.4 客户端 Sentry 声明 | LogRedactor 接线 + 面包屑禁用（`log_redactor_e2ee_test.dart` 等存在） | **对账通过** |

---

## 3. 威胁优先级排序与最小化建议

威胁模型三角色：**被动观察者 O**（链路窃听/流量分析）、**恶意服务端 S**
（可读全部落库元数据+主动注入）、**恶意群成员 M**（读群内元数据+服务端
API 返回的成员图谱）。评分维度=泄漏价值×可实现性×最小化 ROI。

| 优先级 | 项 | 威胁方 | 理由 | 最小化建议（C17 后半候选） | 开销预算框架 |
|---|---|---|---|---|---|
| **P0** | #12 密文长度 | S/O/M | 唯一直接泄漏**内容属性**（长度→文本长度/图片尺寸量级）的通道；实现便宜 | padding 分桶：inner_frame 加密后 pad 到 64B 步进桶（或 Matrix 风格 1KiB/4KiB 固定档）；附件密文已按 chunk 分块（descriptor 有 chunk_count 一致性校验，改桶需同步） | 每消息额外 0~63B（均值 32B，+5%~15% 体积）；oracle：桶化后长度→明文长度互信息应为 0 |
| **P1** | #7+#8+#16 时序/在线 | S/O/M | 在线时段+输入节奏=行为画像；push 判定依赖在线状态使"永远在线"不可行，但可抬高成本 | ①typing 定间隔化（3s→固定 5s 心跳式，牺牲微小 UX）或会话级开关；②已读回执批量+延迟（#9 一并处理）；③群消息服务端合批投递（抖动 ±2s） | 每会话 typing 帧数 -40%；已读帧合并为 1 条/批；oracle：inter-arrival 直方图 KL(before,after) 显著下降 |
| **P1** | #9 已读时序 | S/M | 读行为=敏感行为信号（何时看私聊）；批量上报实现简单 | 见上 | 同上 |
| **P2** | #3 fan-out 设备数 | S | uid→活跃设备数与设备 ID 明文；单设备多密文也放大流量指纹 | fan_out 密文共享+逐设备仅 wrap key（Megolm 化 C2C 或 Olm 信封外再套一层）；或 devices 键哈希化（服务端仍可枚举，仅挡被动观察者） | 改造中：C2C 双套件并存期回归面大；先做 devices 键不透明化（低成本挡 O） |
| **P2** | #13 msg_type 明文 | S/M | 类型泄漏内容形状（语音 vs 文本） | 语义类型并密文（路由改用 action 之外的哑路由位）；需后端 v2.1 API 演进 | API 破坏性变更，需版本协商窗口 |
| **P3** | #1/#2/#15/#16 身份/IP/push 映射 | S/O/P | 根本性泄漏，业界普遍未解 | 超出本轮：PQ 混合网/中继（见 pq-profile.md）/自建 push | 战略级，单独立项 |
| **P0(文档)** | D1 文档漂移 | — | visibility-matrix 推送行与代码不符 | 更新矩阵 v1.2（一行改动，本轮建议但未实施——文档属 compliance 面，由 owner 决定合入时机） | 零 |

### AC-34 测量方法定义（本轮交付，不实施）

- **长度 oracle（before/after）**：
  1. 固定语料：中/英文文本各 200 条（长度 1B~4KB 对数分布）+ 图片 20 张
     （EXIF 剥离、分辨率已知）。
  2. before：现网加密，记录 `(plaintext_len, wire_len)` 对，做线性回归
     （预期 R²>0.99、斜率≈1）。
  3. after：padding 启用后同语料，断言 `wire_len` 落桶集合有限（≤8 桶/
     语料段）且与 `plaintext_len` 的互信息≈0（用桶-长度列联表卡方检验
     p>0.05 作通过线）。
  4. 开销预算：报告 P50/P95 额外字节数；预算线=均值 ≤48B/条（对 1KB
     消息 ≤+20%）。
- **时序 oracle**：
  1. 录制脚本化会话（固定打字节奏回放），收集发送侧 inter-arrival
     （消息+typing+已读三类帧，服务端视角=到达时间）。
  2. before/after 各跑 50 轮，用 KL 散度比较 typing 帧间隔分布与"真实
     打字节奏"的相关性（before 应显著相关，after 目标=不相关）。
  3. 开销预算：每分钟会话额外帧数 ≤+2（合批抖动引入的空帧）。
- 落点：两 oracle 实现为 imboyapp e2e 测试（`test/` 下，真机/模拟回放），
  C17 后半立项时一并交付。

---

## 4. fuzz/parser 现状（AC-35 前置）

### 4.1 现有覆盖（手工固定用例，非 fuzz）

| parser | 文件 | 恶意输入用例现状 |
|---|---|---|
| PFv3 canonical CBOR | `protected_frame_v3.dart` | 较全：indefinite-length 拒收、non-shortest int、duplicate key、trailing bytes、非 canonical 排序、嵌套/条目上限、oversized header/envelope 密码学前拒绝、header_hash 篡改（`protected_frame_v3_test.dart` PF3-01..08） |
| PFv3 envelope | 同上 | tampering×2、bounds×2 |
| attachment descriptor | `attachment_descriptor.dart` | 很全：缺字段/未知字段/类型混淆/base64 非法/长度不符（content_key≠32B、nonce≠12B）/块数谎报（截断/多块）/三者自洽性 |
| megolm backup section | `megolm_backup_section.dart` | 部分：invalid grant 结构拒收（少量） |
| Olm/Megolm 帧解析 | vodozemac native | 库自身 hardened；Dart 侧 metadata 解析（peer_uid/message_type int.tryParse 等）散布在 decrypt 路径，无系统恶意用例 |
| handshake（X3DH claim/OTK） | `olm_session_service.dart` | 无恶意输入测试 |

### 4.2 缺口清单

1. **无持续随机化 fuzz**：全部为固定向量。一旦 parser 改动引入新分支，
   固定用例不覆盖。
2. **CBOR 深层嵌套/超长参数组合**：maxDepth=16/maxMapEntries=128 的
   **组合**边界（如 depth 16 × 每层 map 128 entries 的合法但极端输入）
   无用例。
3. **vodozemac 密文 metadata 解析**（Dart 侧 `int.tryParse` 默认值回退：
   `messageType ?? 1`——恶意 metadata 可把 message_type 强制改 0/2 触发
   Olm prekey 分支）无攻击用例。
4. **backup URL/JSON 下载体**（`e2ee_backup_url_download_service.dart`）
   面向服务端可控内容，无恶意响应测试。
5. handshake/claim 响应完全未测恶意形态。

### 4.3 工具评估与 corpus 方案

- AFL/libFuzzer：**不适用**（需要进程级 coverage-instrumented entry，
  Dart VM/AOT 无官方支持）。
- dart-fuzz / Dart Fuzzing API（dart:developer 侧实验性）：仅覆盖
  core-lib 语义 fuzz，不可挂自定义 parser target，**不适用**。
- **推荐：手写 seeded 随机腐蚀 harness**（成本低、可进 CI）：
  1. corpus 来源：现有测试的真实构造帧（roundtrip 测试已产出合法
     envelope/header/descriptor 字节）→ 作为 seed corpus 存
     `test/fixtures/fuzz_seeds/`。
  2. 变换算子：单字节翻转 / 边界截断 / 长度字段 ±1~±2^16 / 深层嵌套
     注入 / 编码变体（non-shortest、indefinite）。
  3. harness 形态：`flutter test --plain-name "fuzz"` 内跑 N=2000 次/
     seed（固定 seed，CI 确定性复现），不变量=**不抛非
     CborParseException/FrameBoundsException 的异常**（即 fail-closed
     只允许白名单异常类，内存/格式错误=失败）。
  4. 挂钩现有 `TEST_COVERAGE_MATRIX.md` 登记。
- Rust 侧（vodozemac 绑定）可另行用 cargo-fuzz（超出 imboyapp 范围，
  记为跨仓建议）。

## 5. SBOM / 供应链现状

| 项 | 现状 | 缺口 |
|---|---|---|
| 依赖规模 | pubspec.lock **372 个包**（direct main 125 + dev 19 + 传递）；git/路径依赖：plugin/flutter_chat_ui（submodule v2.11.0+9）、flutter_vodozemac ^0.8.1（pub） | 无任何 SBOM 产出 |
| SBOM 工具 | 无 syft/cyclonedx/spdx 接线（scripts/ 无相关脚本；CI 无生成步骤） | **缺口 G1**：`dart pub deps --json` + `pubspec.lock` → CycloneDX 转换脚本（社区有 `cyclonedx-dart` 但维护弱；建议直接 lockfile→SPDX Lite 自写转换，~100 行） |
| CVE 扫描 | `flutter pub outdated` 手动可跑（当前 67 包有新版本）；无 osv-scanner/audit CI | **缺口 G2**：osv-scanner 支持 pubspec.lock（`osv-scanner --lockfile=pubspec.lock`），建议进 pre_push_gate.sh |
| 版本钉扎 | lockfile 存在但 vendor 子模块可漂移；flutter_vodozemac 是唯一密码学关键依赖 | **缺口 G3**：关键依赖（flutter_vodozemac/crypto/pointycastle）版本变更需进 CHANGELOG 安全段（无此流程） |
| 漏洞响应文档 | 后端 imboy 仓有 `SECURITY.md`（上报渠道/PGP/版本支持表）；**imboyapp 仓无** | **缺口 G4**：imboyapp 需 SECURITY.md（可指回后端渠道，但需声明移动端披露面） |
| 依赖准入 | 无第三方依赖评审流程文档 | **缺口 G5**：新依赖（尤其涉及加密/网络/存储）无准入检查清单 |

---

## 6. C17 后半（实施阶段）依赖项

1. **后端配合项**：msg_type 并密文需 v2.1 API 版本协商（P2）；已读回执
   批量化需服务端接受聚合 action（现有 messageRead 单条语义）。
2. **产品决策**：typing 抑制档位（完全关/定间隔/维持现状）与已读回执
   延迟参数（建议 5min/退出会话两档）——影响 UX，需 owner 拍板。
3. **前置实施**：AC-34 oracle 语料库（本文件 §3 已定义方法，语料文件
   待建）。
4. **文档合入**：visibility-matrix v1.2（D1~D5 修正）与 imboyapp
   SECURITY.md（G4）——建议随 C17 后半一并提 PR。
5. **fuzz harness**（§4.3 方案）与 SBOM 转换脚本（G1）可先行独立合入，
   不依赖上述决策。

## 7. 证据

- inventory 测试：imboyapp 分支 `e2ee/C17-run-20261003-094804`，
  `test/unit_test/service/e2ee/metadata_visibility_inventory_test.dart`
  （14 例，`flutter test` All tests passed，FRB_DART_LOAD_EXTERNAL_
  LIBRARY_NATIVE_LIB_DIR 指向主仓 release dylib）。
- 代码锚点（本仓 integration worktree `e2ee/integration-run-20261003-094804`）：
  - `lib/service/e2ee/protected_frame_v3.dart`（信封/header 字段）
  - `lib/page/chat/chat/services/chat_network_service.dart`（sendWsMsg/
    fan-out/已读回执/client_send_ts 移除）
  - `lib/service/message_actions.dart`（typing 帧）
  - `lib/modules/messaging/domain/policy/typing_indicator_rules.dart`（3s 节流）
  - `lib/service/e2ee/olm_protocol.dart` / `megolm_protocol.dart`
    （protocol_metadata 外露键）
  - 后端 `src/logic/push_notification_logic.erl`（常量 Push，94cb822b）
  - 后端 `src/api/msg_handler.erl`（/msg/offline did 参数）
  - 后端 `src/api/group_handler.erl`（detail 返回 member_list）
