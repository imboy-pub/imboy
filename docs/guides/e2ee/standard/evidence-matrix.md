# Evidence Matrix：对外声明 → 验证工件 → 复现方法 → 状态

> **层**：证据层 ｜ **建立**：2026-08-01 ｜ **维护**：每验收动作完成后更新；每次发布前全量复核
> **铁律**：① 对外只许说"允许口径"列的话；② 状态=红的行，对应能力对外按不存在处理；③ 真机结果与自动化结果严格分列，禁止拿模拟器/旁路结果充真机。
> **状态**：🟢绿（最近验证通过+锚）/🔴红（未闭环）/🟡部分（注明哪半）

## 1. 核心安全声明

| # | 允许对外口径 | 验证工件 | 复现方法 | 状态（日期/锚） |
|---|---|---|---|---|
| EV-1 | "服务端不存储、不接触消息明文与私钥" | `scripts/check_server_zero_crypto.sh`；PEM 拒收测试；密文保真集成测试 | `cd imboy && make e2ee-verify`（含守护段） | 🟡（守护在 CI 未运行；P4-6 扩展后升绿） |
| EV-2 | "单聊消息逐设备 Olm 双棘轮加密" | `chat_network_service.dart:595-699`；`olm_session_service.dart`；e2ee 套件 | `cd imboyapp && flutter test test/service/e2ee/` | 🟡（单测绿；**真机未验=P2**） |
| EV-3 | "群聊 Megolm 加密，room key 经 Olm 逐设备包裹" | `group_session_service.dart`；ADR 13；round-trip 测试 | `flutter test test/integration/room_key_olm_roundtrip_test.dart` | 🟡（单进程实证；真机=P2-3） |
| EV-4 | "加密不可降级：strict 模式拒绝明文/RSA 回退" | `policy_gate.dart`；`rsa_legacy_protocol.dart:40-47`（encrypt 抛错）；retry_plaintext_guard 三测试 | `flutter test test/service/e2ee/` | 🟢单测层（2026-07-29 基线）；真机=P2 |
| EV-5 | "PFv3 信封路由字段全认证，篡改即拒收" | `protected_frame_v3.dart`；mutation_matrix_test | 同上 | 🔴（PASS 被判失效，P1-2 重验中） |
| EV-6 | "重放/乱序消息被拒绝" | replay_counter_epoch_test；ADR 26 | 同上 | 🔴（生产链路拒收一切，P1-1 修复中） |
| EV-7 | "OTK 耗尽攻击被限流+幂等租约缓解，且不触发明文降级" | 迁移 49；`olm_handler_claim_throttle_tests`；retry guard 实证 | `make e2ee-verify`（e2ee_otk_* 模块） | 🟢后端；🟡端到端拼接未实证（P3-7） |
| EV-8 | "登出/换号本地秘密全清除" | `e2ee_secret_inventory.dart`；sqlite_uid_isolation_test | `flutter test test/service/e2ee/e2ee_secret_inventory_test.dart` | 🟡（单测绿；真机旅程=P2-5） |
| EV-9 | "密钥备份服务端只见密文（4S 模型）" | 迁移 36；`e2ee_backup_logic`（版本单调/PEM 拒收）；`e2ee_crypto_service.dart` | `make e2ee-verify`（e2ee_backup_*）+ `flutter test test/api/e2ee_backup_api_test.dart` | 🟢（2026-07-12 基线，真 PG 往返实证） |
| EV-10 | "附件端到端加密（内容+缩略图）" | E2EE-061 Slice 1-8；attachment_* 12 测试文件 | `flutter test test/service/e2ee/`（attachment_*） | 🔴（**开关未翻开，生产明文直传**；P2-2 前禁止任何附件加密口径） |

## 2. 可验证性声明（当前大部分不可宣称）

| # | 允许对外口径 | 验证工件 | 复现方法 | 状态 |
|---|---|---|---|---|
| EV-11 | "支持安全码（SAS）设备互验" | `safety_number.dart`（算法） | — | 🔴（零 UI，**不可宣称**；P3-5） |
| EV-12 | "交叉签名跨设备信任传递" | `device_manifest.dart`/`identity_verifier.dart`（未接线） | — | 🔴（**不可宣称**；P3-4） |
| EV-13 | "身份密钥变更对会话方告警" | TOFU pin+`e2ee_peer_key_warning_rule.dart` | `flutter test test/page/chat/e2ee_peer_key_warning_rule_test.dart` | 🟡（横幅级；KT 自动检测=P3-8） |
| EV-14 | "Key Transparency：身份密钥目录可第三方复算" | `e2ee_kt_merkle.erl`（库完成） | `make e2ee-verify`（e2ee_kt_merkle_tests） | 🔴（未接线+profile 未签字，**不可宣称**——07-31 审计红线项） |
| EV-15 | "换设备群聊历史可恢复" | P3-1 交付物 | 换设备恢复测试 | 🔴（当前备份仅 RSA；P3-1 后可宣称"群可恢复+1:1 不可恢复=设计"） |

## 3. 运维与工程声明

| # | 允许对外口径 | 验证工件 | 复现方法 | 状态 |
|---|---|---|---|---|
| EV-16 | "E2EE 测试套件为 CI 合并硬门" | workflow 配置 | GitHub Actions 页面 | 🔴（**CI 从未运行**；P0-4/P4-1） |
| EV-17 | "设备吊销即时生效" | P3-3 交付物 | 吊销后 claim/解密对抗测试 | 🔴（当前吊销不清键；P3-3） |
| EV-18 | "信任事件审计日志不可篡改" | `trust_audit_repo.erl`（append-only，无 update/delete） | `make e2ee-verify`（trust_audit/e2ee_trust_*） | 🟢（append-only 成立；树头签名=P3-8 后升级） |
| EV-19 | "token 绑定设备，跨设备写密钥被拒" | E2EE-013；`olm_handler_tests`（DT-01/02） | `make e2ee-verify` | 🟢（2026-07-21 基线） |
| EV-20 | "审计就绪包可交付第三方" | audit-ready-package.md 六件套 | 按 SOW 模板走查 | 🔴（P5 交付） |

## 4. 不可宣称清单（对外材料红线，与 2026-07-31 审计"七项口径"联动）

1. **附件已加密** —— 开关未翻开，生产明文直传（EV-10）。
2. **Key Transparency** —— 库未接线（EV-14）。
3. **MLS / 后量子（PQ）** —— 路线图项，只许用"路线图"措辞（TT-R1/R2）。
4. **"零知识证明"** —— 可说"服务端不解密消息内容"（EV-1），"可执行验证的零知识架构"须待 P4-6。
5. **1:1 历史换设备可恢复** —— 设计选择=不可恢复（对齐 Signal），只允许说"群聊历史可恢复（P3-1 后）+1:1 换设备不可恢复属行业通行设计"。
6. **附件元数据完全不可见** —— MIME 不隐藏为已拍板项（E2EE-061 §6），只说"内容加密"。
7. **形式化验证 / 第三方审计已完成** —— 就绪包≠已审计（TT-D5 降级形态）。
8. **真机/生产验证** —— 一切"已验证"措辞须区分单测/真 PG/真机三档，真机档在 P2 完成前为空白。

## 5. 复核记录

| 日期 | 复核人 | 范围 | 结果 |
|---|---|---|---|
| 2026-08-01 | （体系建立） | 全量初值 | 本表 |
