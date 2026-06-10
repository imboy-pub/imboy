# Sentry DSN 生产接入指南 / Sentry DSN Production Integration Guide

> 版本 / Version：1.0 | 日期 / Date：2026-05-27
> 适用范围 / Scope：Flutter 客户端（imboyapp）+ Erlang/OTP 后端（imboy）

---

## 目录 / Table of Contents

1. [Flutter 端配置](#1-flutter-端配置--flutter-configuration)
2. [Erlang 后端配置](#2-erlang-后端配置--erlang-backend-configuration)
3. [Source Map 符号文件上传](#3-source-map-符号文件上传--source-map-symbol-upload)
4. [CI/CD 集成](#4-cicd-集成--cicd-integration)
5. [验证步骤](#5-验证步骤--verification-steps)
6. [隐私注意事项](#6-隐私注意事项--privacy-considerations)

---

## 1. Flutter 端配置 / Flutter Configuration

### 1.1 已有集成现状 / Current Integration State

项目已通过 `sentry_flutter: ^9.16.0` 接入 Sentry SDK，DSN 通过编译期环境变量注入（`String.fromEnvironment`），不硬编码在源码中。

The project already integrates `sentry_flutter: ^9.16.0`. The DSN is injected at compile time via `String.fromEnvironment` and is never hardcoded in source.

```dart
// imboyapp/lib/service/sentry_service.dart
static const String _dsn = String.fromEnvironment(
  'SENTRY_DSN',
  defaultValue: '',
);
```

当 `SENTRY_DSN` 为空时，`SentryService.isEnabled` 返回 `false`，所有 Sentry 调用静默跳过——**不会影响未配置环境的运行**。

When `SENTRY_DSN` is empty, `SentryService.isEnabled` returns `false` and all Sentry calls are silently skipped—**no impact on unconfigured environments**.

### 1.2 获取 DSN / Obtaining the DSN

1. 登录 [https://sentry.io](https://sentry.io) → 选择或新建项目（Platform: Flutter）
2. **Settings → Projects → `<your-project>` → Client Keys (DSN)**
3. 复制完整 DSN，格式为：`https://<public-key>@<host>.ingest.sentry.io/<project-id>`

### 1.3 本地开发注入 / Local Development Injection

**方式 A：`.env.pro` 文件（推荐）**

在 `imboyapp/.env.pro` 末尾添加：

```dotenv
SENTRY_DSN=https://<public-key>@<host>.ingest.sentry.io/<project-id>
```

然后通过 `--dart-define-from-file` 读取：

```bash
flutter run --dart-define-from-file=.env.pro
flutter build apk --release --dart-define-from-file=.env.pro
flutter build ios --release --dart-define-from-file=.env.pro
```

> `.env.pro` 已在 `.gitignore` 中，**不会提交到版本库**。
> `.env.pro` is already listed in `.gitignore`—**it will not be committed**.

**方式 B：直接 `--dart-define`（快速测试）**

```bash
flutter run --dart-define=SENTRY_DSN=https://<public-key>@<host>.ingest.sentry.io/<project-id>
```

### 1.4 `SentryFlutter.init` 初始化配置 / Initialization Configuration

在 `main.dart` 中的 `SentryFlutter.init` 建议加入以下配置：

In `main.dart`, configure `SentryFlutter.init` as follows:

```dart
await SentryFlutter.init(
  (options) {
    options.dsn = SentryService.dsn;

    // 环境标识 / Environment tag
    options.environment = const String.fromEnvironment('ENV', defaultValue: 'dev');

    // 采样率：生产建议 0.1~0.2（10%–20% 性能追踪）
    // Sampling: 0.1–0.2 recommended for production performance tracing
    options.tracesSampleRate = 0.1;

    // Release 版本自动关联符号文件
    // Automatically links uploaded debug symbols
    options.release = 'imboy@${const String.fromEnvironment('APP_VERSION', defaultValue: '0.0.0')}';

    // 隐私过滤（见第 6 节）/ PII filter (see section 6)
    options.beforeSend = _beforeSendFilter;
  },
  appRunner: () => runApp(const MyApp()),
);
```

---

## 2. Erlang 后端配置 / Erlang Backend Configuration

### 2.1 当前集成状态 / Current Integration State

**Erlang 后端（`imboy/`）目前尚未集成 Sentry SDK。**

`docker-compose.prod.yml` 中已预留 `SENTRY_DSN` 环境变量占位符：

```yaml
# deploy/docker-compose.prod.yml
environment:
  SENTRY_DSN: ${SENTRY_DSN:-}
```

**The Erlang backend (`imboy/`) does not currently integrate a Sentry SDK.**
The `SENTRY_DSN` env var placeholder exists in `docker-compose.prod.yml` for future use.

### 2.2 推荐后端错误监控方案 / Recommended Backend Error Monitoring

Erlang/OTP 生态目前没有官方 Sentry SDK，推荐以下两种方案：

There is no official Sentry SDK for Erlang/OTP. Two approaches are recommended:

**方案 A：`sentry_sdk`（第三方 Hex 包，适合 Elixir 混合项目）**

若项目未来引入 Elixir，可使用 `sentry ~> 10.x`（Hex.pm）。

**方案 B：通过 HTTP API 上报（当前可行方案）**

Sentry 提供标准 HTTP Envelope API，可在 Erlang 中用 `httpc` 或已有的 HTTP 客户端直接调用：

```erlang
%% 伪代码示例 / Pseudo-code example
report_to_sentry(Exception, Stacktrace) ->
    Dsn = os:getenv("SENTRY_DSN", ""),
    case Dsn of
        "" -> ok;
        _ ->
            Payload = build_sentry_envelope(Exception, Stacktrace),
            {Host, ProjectId, PublicKey} = parse_dsn(Dsn),
            Url = lists:concat(["https://", Host, "/api/", ProjectId, "/envelope/"]),
            Headers = [{"X-Sentry-Auth",
                        lists:concat(["Sentry sentry_version=7,",
                                      "sentry_key=", PublicKey])}],
            httpc:request(post, {Url, Headers, "application/x-sentry-envelope", Payload},
                          [{timeout, 5000}], [])
    end.
```

**方案 C：暂不集成，使用 Grafana/Loki 替代**

项目已部署 Loki + Grafana 日志聚合（`docker-compose.prod.yml`），可在 Grafana 中设置 ERROR 级别日志告警，覆盖后端错误监控需求，**无需强依赖 Sentry**。

The project already deploys Loki + Grafana log aggregation. ERROR-level log alerts in Grafana can cover backend error monitoring **without requiring Sentry**.

### 2.3 生产环境变量配置 / Production Env Var Configuration

在 `deploy/.env`（对应 `deploy/docker-compose.prod.yml`）中设置：

```dotenv
# deploy/.env
SENTRY_DSN=https://<public-key>@<host>.ingest.sentry.io/<project-id>
```

该值会自动注入 `imboy_backend` 容器的 `SENTRY_DSN` 环境变量。

This value is automatically injected into the `imboy_backend` container's `SENTRY_DSN` env var.

---

## 3. Source Map 符号文件上传 / Source Map & Symbol Upload

### 3.1 安装 sentry-dart-plugin / Install sentry-dart-plugin

在 `imboyapp/pubspec.yaml` 的 `dev_dependencies` 中添加：

```yaml
dev_dependencies:
  sentry_dart_plugin: ^2.0.0
```

在 `imboyapp/` 根目录新建 `sentry.properties`（**加入 `.gitignore`**）：

```properties
# imboyapp/sentry.properties
defaults.url=https://sentry.io/
defaults.org=<your-org-slug>
defaults.project=<your-project-slug>
auth.token=<your-sentry-auth-token>
```

> `auth.token` 在 Sentry → Settings → Auth Tokens → Create New Token 生成，
> 需要 `project:releases` + `org:read` + `project:write` 权限。

### 3.2 Android 符号上传命令 / Android Symbol Upload

```bash
cd imboyapp

# 构建并上传 Android 符号文件
flutter build apk --release \
  --obfuscate \
  --split-debug-info=build/debug-info \
  --dart-define-from-file=.env.pro

# 上传符号文件到 Sentry
dart run sentry_dart_plugin \
  --upload-debug-symbols \
  --debug-symbols-path=build/debug-info
```

### 3.3 iOS 符号上传命令 / iOS Symbol Upload

```bash
cd imboyapp

# 构建并上传 iOS 符号文件
flutter build ios --release \
  --obfuscate \
  --split-debug-info=build/debug-info-ios \
  --dart-define-from-file=.env.pro

# 上传符号文件到 Sentry
dart run sentry_dart_plugin \
  --upload-debug-symbols \
  --debug-symbols-path=build/debug-info-ios
```

> `--split-debug-info` 输出目录包含私钥映射，**不得提交到版本库**，已在 `.gitignore` 中添加 `build/` 目录。

---

## 4. CI/CD 集成 / CI/CD Integration

### 4.1 GitHub Secrets 配置 / GitHub Secrets Setup

在 GitHub 仓库 → **Settings → Secrets and variables → Actions** 中添加：

| Secret 名称 | 说明 / Description |
|------------|-------------------|
| `SENTRY_DSN` | 完整 Sentry DSN URL |
| `SENTRY_AUTH_TOKEN` | Sentry API Token（用于符号上传）|
| `SENTRY_ORG` | Sentry 组织 slug |
| `SENTRY_PROJECT` | Sentry 项目 slug |

### 4.2 在 ci.yml 中添加符号上传步骤 / Add Symbol Upload Steps to ci.yml

在 `imboyapp/.github/workflows/ci.yml` 的 `build-android` 任务中，**在 `Build APK` 步骤之后**添加：

In `ci.yml`, **after the `Build APK` step** in the `build-android` job, add:

```yaml
      - name: Upload Android Debug Symbols to Sentry
        if: ${{ secrets.SENTRY_AUTH_TOKEN != '' }}
        env:
          SENTRY_AUTH_TOKEN: ${{ secrets.SENTRY_AUTH_TOKEN }}
          SENTRY_ORG: ${{ secrets.SENTRY_ORG }}
          SENTRY_PROJECT: ${{ secrets.SENTRY_PROJECT }}
        working-directory: imboyapp
        run: |
          dart run sentry_dart_plugin \
            --upload-debug-symbols \
            --debug-symbols-path=build/debug-info \
            --auth-token="$SENTRY_AUTH_TOKEN" \
            --org="$SENTRY_ORG" \
            --project="$SENTRY_PROJECT"
```

在 `build-ios` 任务中类似添加：

Similarly for the `build-ios` job:

```yaml
      - name: Upload iOS Debug Symbols to Sentry
        if: ${{ secrets.SENTRY_AUTH_TOKEN != '' }}
        env:
          SENTRY_AUTH_TOKEN: ${{ secrets.SENTRY_AUTH_TOKEN }}
          SENTRY_ORG: ${{ secrets.SENTRY_ORG }}
          SENTRY_PROJECT: ${{ secrets.SENTRY_PROJECT }}
        working-directory: imboyapp
        run: |
          dart run sentry_dart_plugin \
            --upload-debug-symbols \
            --debug-symbols-path=build/debug-info-ios \
            --auth-token="$SENTRY_AUTH_TOKEN" \
            --org="$SENTRY_ORG" \
            --project="$SENTRY_PROJECT"
```

### 4.3 现有 CI 状态说明 / Current CI State

`ci.yml` 已正确通过 `--dart-define=SENTRY_DSN="${SENTRY_DSN:-}"` 注入 DSN，
**符号上传步骤（`sentry_dart_plugin`）尚未添加**，需按 4.2 节补充。

`ci.yml` already injects the DSN correctly via `--dart-define`. **The symbol upload step is not yet added**—follow section 4.2.

---

## 5. 验证步骤 / Verification Steps

### 5.1 触发测试异常 / Trigger a Test Exception

在 `SentryService.isEnabled` 为 `true` 的构建中，临时调用：

```dart
// 仅测试用，验证后删除 / For testing only, remove after verification
await Sentry.captureException(
  Exception('Sentry integration test - please ignore'),
  stackTrace: StackTrace.current,
);
```

或更安全的方式——通过 Sentry 的 `SentryWidget` 触发：

```dart
// 在调试菜单中添加
SentryFlutter.reportFullyDisplayed();
```

### 5.2 检查 Sentry 控制台 / Check Sentry Dashboard

1. 打开 Sentry 项目 → **Issues** 页面
2. 应在 30 秒内看到新 Issue，标题为 `Exception: Sentry integration test`
3. 确认 **Environment** 字段正确显示 `pro`（或对应环境）
4. 确认 **Release** 字段包含版本号

### 5.3 验证符号文件是否有效 / Verify Debug Symbols

上传后，打开一条真实崩溃 Issue：
- Stack trace 应显示 **Dart 类名和行号**，而非混淆后的符号
- 如显示 `<unknown>` 则说明符号文件未上传或版本不匹配

### 5.4 后端连通性测试 / Backend Connectivity Test

若后端已集成 Sentry，在容器内执行：

```bash
docker exec imboy_backend printenv SENTRY_DSN
```

确认 DSN 已正确注入容器环境。

---

## 6. 隐私注意事项 / Privacy Considerations

### 6.1 `beforeSend` 过滤器 / PII Filter via beforeSend

在 `main.dart` 中的 `SentryFlutter.init` 配置 `beforeSend`，过滤敏感字段：

```dart
import 'package:sentry_flutter/sentry_flutter.dart';

/// 在上报前过滤 PII / Filter PII before sending to Sentry
SentryEvent? _beforeSendFilter(SentryEvent event, Hint hint) {
  // 1. 移除用户标识信息（除非明确需要关联用户）
  //    Remove user identity (unless user association is explicitly needed)
  final sanitizedUser = event.user?.copyWith(
    email: null,          // 不上报邮箱 / no email
    username: null,       // 不上报用户名 / no username
    ipAddress: null,      // 不上报 IP / no IP
    // 可保留匿名 ID 用于跨 session 关联
    // Keep anonymous ID for cross-session correlation
    id: event.user?.id,
  );

  // 2. 清理请求头中的 Authorization / Strip Authorization from request headers
  final sanitizedRequest = event.request?.copyWith(
    headers: _stripSensitiveHeaders(event.request?.headers),
    cookies: null,         // 不上报 cookies / no cookies
    data: null,            // 不上报请求体 / no request body
  );

  // 3. 清理 extra 中的手机号、token 等字段
  //    Strip phone numbers, tokens from extra context
  final sanitizedExtra = _stripSensitiveExtra(event.extra);

  return event.copyWith(
    user: sanitizedUser,
    request: sanitizedRequest,
    extra: sanitizedExtra,
  );
}

Map<String, String>? _stripSensitiveHeaders(Map<String, String>? headers) {
  if (headers == null) return null;
  const sensitiveKeys = {'authorization', 'cookie', 'x-auth-token', 'x-api-key'};
  return {
    for (final entry in headers.entries)
      if (!sensitiveKeys.contains(entry.key.toLowerCase()))
        entry.key: entry.value,
  };
}

Map<String, dynamic>? _stripSensitiveExtra(Map<String, dynamic>? extra) {
  if (extra == null) return null;
  const sensitiveKeys = {'phone', 'mobile', 'token', 'password', 'access_token',
                          'refresh_token', 'id_card', 'bank_card'};
  return {
    for (final entry in extra.entries)
      if (!sensitiveKeys.contains(entry.key.toLowerCase()))
        entry.key: entry.value,
  };
}
```

### 6.2 关闭 IP 自动收集 / Disable Automatic IP Collection

在 Sentry 项目 → **Settings → Security & Privacy** 中：

- 勾选 **Prevent Storing of IP Addresses**
- 勾选 **Enable Data Scrubbing**
- 在 **Additional Sensitive Fields** 中添加：`phone`, `mobile`, `id_card`

### 6.3 数据留存策略 / Data Retention Policy

根据中国《个人信息保护法》（PIPL）要求：

- Sentry 的数据存储区域应选择**中国大陆以外区域**（Sentry.io 默认在美国），或部署自托管 Sentry
- 错误数据保留时间建议 **≤ 90 天**（在 Sentry 项目 Settings → Data Management 中配置）
- 如上报中包含 IM 消息内容，**必须**在 `beforeSend` 中过滤 `payload` 字段

Per China PIPL requirements: consider data residency, set retention to ≤ 90 days, and always filter IM message payload content in `beforeSend`.

### 6.4 DSN 安全性说明 / DSN Security Note

Flutter 的 `--dart-define` 值会编译进二进制包，**可通过反编译提取**。

- DSN 泄露的最坏后果是攻击者可向你的 Sentry 项目发送垃圾数据（不影响用户数据安全）
- 建议在 Sentry → **Settings → Security Headers** 中配置 **Rate Limiting** 限制滥用
- 不要将后端数据库密码等真正的 secret 通过 `--dart-define` 传入

Flutter `--dart-define` values are embedded in the binary and can be extracted via reverse engineering. DSN exposure only allows spam submission—it does not expose user data. Enable rate limiting in Sentry settings to mitigate abuse.

---

## 快速参考 / Quick Reference

```bash
# 本地生产构建（含 Sentry DSN）
cd imboyapp
flutter build apk --release \
  --obfuscate \
  --split-debug-info=build/debug-info \
  --dart-define-from-file=.env.pro

# 上传符号文件
dart run sentry_dart_plugin \
  --upload-debug-symbols \
  --debug-symbols-path=build/debug-info

# 验证 Docker 容器中 DSN 注入
docker exec imboy_backend printenv SENTRY_DSN
```
