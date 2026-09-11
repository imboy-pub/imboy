# 「[加密消息]」占位发生率压降方案 / Placeholder Occurrence Reduction

> 最后更新 / Last updated: 2026-09-11
> 关联 / Related: [history-recoverability.md](./history-recoverability.md)、[key-lifecycle.md](./key-lifecycle.md)
> 状态：**四路径 + 度量全部实施完毕**（2026-09-10/11），改动未提交

---

## 目标 / Goal

`[加密消息]` 占位气泡（decrypt-on-read 失败的**设计内**产物，ADR 15 §5）对用户
不可接受的不是"存在"，而是"遇到"——尤其换设备后文案承诺「恢复后即可查看」，
而单聊（Olm）session 有意不备份（key reuse / ratchet fork，Signal/Matrix 同），
恢复密钥也永远解不开。

**目标不是消灭占位（换机且无备份时它在数学上就无法消灭），而是把发生率压到
用户一辈子遇不到。**

真根因：**承诺与密码学事实不一致**。修复分四条路径 + 一套度量仪表。

---

## 四路径 / Four Paths（全部已实施）

| 路径 | 内容 | 关键文件 |
|------|------|---------|
| 1. 原因三分（客户端） | `no_device_envelope` / `fan_out_missing_devices` =「注定解不开」→ 边界说明文案 `chat.e2eeMsgBeforeDevice`，点击**不**弹恢复引导；`crypto_store_unavailable` → 重试文案；其余 → 通用占位 + 引导 | `imboyapp/lib/service/e2ee_service.dart`（`isUnrecoverableDecryptFailure` / `e2eeFailedPlaceholderText`）、`chat_page.dart` tap 分流 |
| 4. 恢复后自动重试 | 密钥恢复成功（文件/云端/URL 三入口共用）→ 自动 `retryFailedMessages` 全量自愈 + 弹窗报恢复条数；失败不阻断成功态 | `imboyapp/lib/page/settings/e2ee_backup_import_page.dart`（`_applyRestoredKeys`） |
| 2. 服务端信封过滤（治本） | 下发前按 `e2ee.fan_out.devices` 过滤不含请求设备 DID 的 C2C 收件消息；offline 滤行同时按 (uid,did) 标记 acked；history 纯过滤、`next_seq` 按全量行推进；did 缺省 fail-open（旧客户端零破坏） | `imboy/src/logic/messaging_logic.erl`（`c2c_deliverable_to_device/3` 等）、`src/api/msg_handler.erl`；客户端 `msg_api.dart` / `chat_archive_service.dart` 带 `did` |
| 3. 备份默认化 | 首次启用强制备份向导（不可跳过）+ 密钥变化自动重传 | `imboyapp/lib/page/settings/e2ee_backup_setup_page.dart`、`lib/service/e2ee_backup_setup_service.dart`、`conversation_page.dart` 触发链、`e2ee_key_service.dart` 钩子 |

注意：**路径2 后端过滤需重启后端节点后生效**。

### 路径3 口令方案决策记录（2026-09-11 拍板）

**方案A：独立恢复口令 / 随机恢复密钥。** 口令只在本地参与 PBKDF2，服务器永远
见不到；向导内提供一键生成 160-bit 随机恢复密钥（8 组 5 位大写十六进制）。

- **明确不做「登录密码派生」**：登录密码在登录时发给服务端验证，服务端（或拖库
  者）见过密码即可解开备份——零信任卖点倒退。要把"全自动恢复"做对，正路是
  零知识登录协议（SRP/OPAQUE），属认证体系重构，不在备份功能内。
- **禁止数字 PIN**：百万级组合对 PBKDF2-310k 仍可 GPU 穷举，独立方案必须配
  口令短语或随机恢复密钥。
- **A+E 双钥匙槽**（口令 + 随机恢复密钥各 wrap 一份，age/Matrix 同款）为后续
  增强：需备份二进制格式升版兼容，单独一轮做。
- **B 端灵活性**：私有化客户若要求"员工忘口令可恢复"，走部署级 PolicyGate
  开关放开弱模型，默认产品行为永远是方案A。

实现要点：

- 向导**不可跳过**：无返回键 + PopScope 拦系统返回；仅上传失败一次后开放
  「稍后再说」（完成标记不落，下次启动重推）。触发改推 **rootNavigator**，
  避免宿主壳 tab 切换绕过。
- 口令缓存进安全存储，值 = JSON `{uid, passphrase}` **uid 绑定**：换号后读不到
  并清除，防止新账号的备份被旧账号口令加密成废包；currentUid 为空的瞬态只拒绝
  不清除（防误删唯一凭据副本）。
- 服务端已有备份的老用户（导出页手动传过）→ 触发判定静默补完成标记，不被打扰。
- 自动重传 fail-open：无缓存口令 / 密钥缺失 / 上传失败一律静默跳过，绝不阻塞
  密钥生成主链路。

---

## 度量 / Measurement（双口径，已实施）

「压到一辈子遇不到」必须可验证：

| 口径 | 实现 | 语义 |
|------|------|------|
| **累计发生**（只增不减） | `E2EEDecryptFailureMetrics`（`e2ee_service.dart`），按 reason 分桶持久化于 `e2ee_decrypt_fail_metrics` | 用户到底遇到过几次。计数点仅两处：落库漏斗（`message.dart`，幂等重投不计数）+ mapper 读时解密失败（每次启动按 行×原因 去重，防逐帧膨胀） |
| **存量盘点**（会下降） | `E2EEHealthCheckService.failureInventory()`：扫 `payload LIKE '%_e2ee_failed%'` 按 reason 分桶，拆 `unrecoverable` / `recoverable`；`getHealthStatus()` 暴露 `decrypt_failures` | 此刻设备上还挂着几条；被路径4自愈消化后应下降，`recoverable` 应趋零 |

判定阈值：`unrecoverable` 是设计内历史边界（换机前消息），不追逐归零；
**`recoverable` 与累计增量趋零即达成目标**。

---

## 验证 / Verification

```bash
# 客户端（imboyapp/）
flutter analyze
flutter test --timeout 180s test/unit_test/service/e2ee_failed_placeholder_text_test.dart \
  test/unit_test/service/e2ee_failure_metrics_test.dart \
  test/unit_test/service/e2ee_health_check_service_test.dart \
  test/unit_test/service/e2ee_backup_setup_service_test.dart \
  test/unit_test/page/settings/e2ee_backup_setup_page_widget_test.dart \
  test/unit_test/page/settings/e2ee_backup_import_page_widget_test.dart \
  test/unit_test/page/chat/chat_archive_service_fail_visibility_test.dart

# 路径3 真壳集成验收（9801 local_office；输出禁 tail 管道）
flutter test integration_test/settings/e2ee_backup_setup_wizard_test.dart -d macos \
  --dart-define=APP_ENV=local_office \
  --dart-define=API_BASE_URL=http://127.0.0.1:9801 \
  --dart-define=API_BASE_URL_OVERRIDE=http://127.0.0.1:9801 \
  --dart-define=WS_URL_OVERRIDE=ws://127.0.0.1:9801/api/v1/ws \
  --dart-define=TEST_PHONE=smoke_bob --dart-define=TEST_PASSWORD=admin888 \
  --dart-define=TEST_EXPECTED_UID=1000000056 \
  --dart-define=TEST_ALLOW_WORKSPACE_ACCEPTANCE=true

# 路径1 集成验收（同 define 配方）
flutter test integration_test/settings/e2ee_boundary_no_device_envelope_test.dart -d macos …

# 后端（imboy/，裸 erl 直跑勿用 make eunit-local 免扰运行节点）
erl -pa ebin -pa test -eval 'eunit:test(messaging_envelope_filter_tests, [verbose]), halt(0).'
```

---

## 残留与后续 / Residual

- **olm_decrypt_error 残留口径**：单聊 v3 信封失败的主导 reason 已是
  `no_device_envelope`（不再误导恢复），但 legacy v1/v2 路径的
  `olm_decrypt_error`/`decrypt_error` 仍属"引导恢复"类——其中 session 丢失型
  实际也解不回，属极少数遗留形状，待路径1 分类粒度细化。
- A+E 双钥匙槽（见上）。
- 部署级 PolicyGate 派生开关（见上）。
