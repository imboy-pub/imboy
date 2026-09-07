# IMBoy V-02 Logging and Crash Redaction Checklist

> 任务：日志与崩溃脱敏（Overseas Compliance Implementation Plan Task V-02）
> 日期：2026-09-08 | 状态：**DONE（双端 sink 层脱敏 + 可运行回归语料）**
> 提交：imboy + imboyapp（本轮，均未 push）

## Goal 对照

阻止 token、凭据、联系方式、明文消息与举报证据进入日志/Sentry。
在 sink 层实现共享脱敏，不改写任何调用点（计划红线 "Do not rewrite every call site"）。

## 勘察结论（治理面收敛的依据）

| 端 | 第三方 sink | 结论 |
|---|---|---|
| imboy 后端 | lager（经 elib_log 单一入口，?INFO_LOG 等全部宏收口于此） | ✅ 单点接入脱敏 |
| imboyapp | Sentry（sentry_flutter 9.29.0；DSN 编译期注入，空则整体禁用） | ✅ beforeSend/beforeBreadcrumb/sendDefaultPii 三项配置 |
| imboyapp 控制台 | iPrint（kDebugMode 才输出）/AppLogger（生产最小化） | ✅ 不出第三方，无泄漏面 |
| erlang_pay | 无任何日志输出（grep lager/logger/io:format 零命中） | ✅ 无 sink 需治理 |

## 后端（imboy）

- **新增 src/lib/log_redact.erl**，两层语义：
  - `term/1` 键脱敏：map/proplist 递归，**精确键名**匹配（与 P-01 sanitize 的子串口径不同——
    日志键来自代码与协议字段，精确集避免 design/assignment 误判 sign）；禁止集含
    password/token/secret/authorization/cookie/sign/signature/openid/verify_code 系列/
    plaintext 系列（E2EE 明文禁入日志）等 33 个键。
  - `text/1` 值模式兜底：JWT 三段式 / Bearer 头 / 手机号（捕获组保分隔符）/ 邮箱 /
    URL 敏感参数（保留参数名只清值）；对格式化后的最终文本再跑一轮。
  - fail-closed：脱敏自身出错返回 [REDACT_ERROR]，绝不回退原文（日志丢失可排查，
    秘密泄漏不可撤回）。正则经 persistent_term 预编译缓存。
- **elib_log 接入**：safe_log/4 与 safe_log/5 两路径在交 lager 前过 term→字符串化→text；
  顺带修一个泄漏面——格式化失败分支旧实现把未脱敏 Fmt/Args 全量回显进日志，改为
  [INVALID_FORMAT_REDACTED]。
- 测试 test/lib/log_redact_tests.erl **13/13**：嵌套 map、headers、URL、异常文本 JWT、
  E2EE 明文键剥离/密文保留、支付回调 proplist（sign/openid）、精确键误伤对照、
  sink 级断言（meck lager，原始秘密在 lager 入口缺席）。

## Flutter（imboyapp）

- **新增 lib/service/log_redactor.dart**：与后端同一判定口径（同一禁止键集、同五类值模式）；
  scrubText（值模式）/scrubDynamic（键+值递归）/scrubMaybeJson（内嵌 JSON 展开清洗）。
- **main.dart SentryFlutter.init 强化**：
  - `sendDefaultPii = false`
  - `beforeBreadcrumb` 恒返回 null（默认面包屑会自动记录路由与请求，无法逐一审计，整体禁用）
  - `beforeSend` 清洗事件：exception.value 文本、extra map（键+值）、tags（逐值文本）。
- 测试 test/service/log_redactor_test.dart **13/13**（回归语料与后端场景一一对应）；
  `flutter analyze` 0 error / 0 warning（main.dart 余 2 条 extra deprecated info，系 SDK 9
  弃用提示，清洗面保留，迁移 Contexts 列 owner 待办）。

## 计划测试条款对照

| 条款 | 后端 | Flutter |
|---|---|---|
| nested maps | ✅ nested_map_redacted_test | ✅ 嵌套 map 用例 |
| headers | ✅ authorization/cookie/content-type | ✅ 同 |
| URLs | ✅ URL query token/sign | ✅ 同 |
| exceptions | ✅ 异常文本 JWT 剥离 | ✅ 同 + beforeSend 覆盖 exception.value |
| E2EE plaintext/ciphertext | ✅ plaintext 键剥/密文保留 | ✅ 同 |
| payment callbacks | ✅ sign/openid proplist | ✅ sign/openid map |
| assert raw secrets absent at sink | ✅ meck lager 捕获最终文本断言 | ✅ 清洗产物断言 |

## 诚实记录（未覆盖/取舍）

1. **SentryStackFrame.vars 未清洗**：SDK 9.29 的 frame.vars 无 setter（只读），且 Flutter
   AOT release 不产生局部变量快照，风险低；如需覆盖需重建 StackTrace 对象。
2. **裸 code 键不脱敏**：error_code/http status code 误伤面太广；验证码由
   verify_code/sms_code/verification_code/email_code 精确键覆盖，调用点纪律
   （验证码不进日志）仍是最后防线。
3. **值模式抓不住无标识明文**：消息正文若无 plaintext 类键名包装，sink 层无从识别；
   本仓纪律是日志只记元数据（ID/行数/状态），消息内容永不入日志——V-02 提供的是
   键级+已知模式兜底，不是内容级审查。
4. **provider 侧设置（外部证据）**：Sentry 项目侧的 Data Scrubbing/Rate Limiting 等
   需登录 Sentry 后台确认截图归档，属账号操作，留 owner（配合 V-01 的 DPA 签署一并做）。
5. mobile 正则针对中国大陆号段（1[3-9] 开头 11 位）；海外号码不覆盖，overseas 发布前
   需按运营区域扩展。

## 验证记录

- 后端：`make eunit-local t=log_redact_tests` → **All 13 tests passed**（erlfmt 前后各一轮）
- 回归：`t=user_export_logic_tests`（elib_log 消费方）→ **All 18 tests passed**
- Flutter：`flutter test test/service/log_redactor_test.dart` → **All 13 tests passed**
- `flutter analyze` → 0 error / 0 warning（main.dart 2 条 extra deprecated info 为 SDK 弃用提示）
- 双端改动均经 erlfmt / dart format
