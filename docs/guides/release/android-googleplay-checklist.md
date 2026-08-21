# Google Play 内测轨配置清单
# Google Play Internal Testing Track Checklist

> 基于文件扫描日期 / Based on file scan date：2026-05-27
> 项目版本 / Project version：`1.0.0-rc.2+3`（pubspec.yaml）

---

## 核查结果汇总 / Summary

| # | 核查项 / Check Item | 状态 / Status |
|---|---|---|
| 1 | applicationId 规范性 | ⚠️ 需确认 |
| 2 | 签名配置安全性 | ✅ 已就绪 |
| 3 | minSdkVersion / targetSdkVersion | ⚠️ 需确认 target API |
| 4 | 权限声明完整性 | ✅ 已就绪 |
| 5 | 版本名和版本码 | ⚠️ 需同步确认 |
| 6 | ProGuard/R8 混淆规则 | ⚠️ 需补充 |
| 7 | 64 位支持（abiFilters）| ⚠️ 未显式配置 |
| 8 | Play Store 内测轨发布步骤 | 📋 见下方说明 |

---

## 详细核查 / Detailed Checks

### 1. applicationId 规范性 ⚠️ 需确认

**当前值 / Current value：** `imboy.chat`（`android/app/build.gradle.kts` → `namespace` 和 `applicationId`）

**问题 / Issue：**
- `imboy.chat` 不是标准反域名格式（应为 `pub.imboy.chat` 或 `pub.imboy.app`）。
- Play Store 要求 applicationId 至少包含两段（含一个点），`imboy.chat` 技术上合法，但与 iOS Bundle ID（`pub.imboy.2`）命名体系不一致。
- 一旦应用发布到 Play Store，applicationId **不可修改**，否则视为新应用（历史评价/下载量清零）。

**操作建议 / Action：**
在首次发布前确认 applicationId：
```kotlin
// 推荐：与域名一致
applicationId = "pub.imboy.app"
```
同步修改 `JPUSH_PKGNAME` manifest placeholder 及极光推送后台注册包名。

---

### 2. 签名配置安全性 ✅ 已就绪

**当前配置 / Current config（`build.gradle.kts`）：**
```kotlin
signingConfigs {
    create("release") {
        enableV1Signing = true
        enableV2Signing = true
        localProperties.getProperty("storeFile")?.let { storeFile = file(it) }
        keyAlias = localProperties.getProperty("keyAlias")
        keyPassword = localProperties.getProperty("keyPassword")
        storePassword = localProperties.getProperty("storePassword")
    }
}
```

**状态 / Status：** ✅ 签名凭证通过 `local.properties` 读取，**未硬编码**在源码中。

**注意事项 / Notes：**
- 确认 `local.properties` 已加入 `.gitignore`（Flutter 默认已忽略）。
- CI/CD 流水线中通过 Secret 变量注入 `local.properties` 或使用 Keystore 文件 + 环境变量。
- V1（JAR signing）+ V2（APK Signature Scheme v2）同时启用，兼容 Android 6.0 以下设备；建议同时启用 V3（Android 9+）：

```kotlin
enableV3Signing = true
enableV4Signing = true  // 增量更新支持
```

**keystore 备份提醒 / Keystore Backup：**
- Keystore 文件是唯一凭证，丢失后**无法更新已上架应用**，必须异地多份加密备份。

---

### 3. minSdkVersion / targetSdkVersion ⚠️ 需确认 target API

**当前配置 / Current config：**
```kotlin
minSdk = flutter.minSdkVersion   // Flutter 默认值通常为 21
targetSdk = flutter.targetSdkVersion  // Flutter 默认通常为 34
minSdkVersion(24)                 // 覆盖为 24（存在两处设置，以 minSdkVersion(24) 生效）
```

**问题 / Issue：**
- `defaultConfig` 中同时存在 `minSdk = flutter.minSdkVersion` 和 `minSdkVersion(24)`，存在重复设置。建议统一为：
  ```kotlin
  minSdk = 24
  ```
- `targetSdk = flutter.targetSdkVersion`：Flutter 3.x 工具链通常注入 API 34，需确认实际值。
- **Play Store 要求**：2024 年 8 月起，新应用 targetSdkVersion 必须 ≥ API 34（Android 14）；2025 年 11 月起，更新的应用也必须 ≥ API 35（Android 15）。

**操作建议 / Action：**
在 `build.gradle.kts` 中显式声明，不依赖 Flutter 工具链默认值：
```kotlin
defaultConfig {
    minSdk = 24
    targetSdk = 35  // Android 15，满足 2025 年 Play 政策
    compileSdk = 35
}
```

**minSdk = 24 覆盖范围 / Coverage：** Android 7.0+，覆盖约 95%+ 活跃 Android 设备。

---

### 4. 权限声明完整性 ✅ 已就绪

**已声明权限 / Declared permissions（`AndroidManifest.xml`）：**

| 权限 | 状态 | 说明 |
|---|---|---|
| `INTERNET` | ✅ | 网络访问 |
| `CAMERA` | ✅ | 相机 |
| `RECORD_AUDIO` | ✅ | 录音（语音消息/通话）|
| `POST_NOTIFICATIONS` | ✅ | Android 13+ 推送通知 |
| `ACCESS_FINE_LOCATION` | ✅ | 精确位置 |
| `ACCESS_COARSE_LOCATION` | ✅ | 粗略位置 |
| `READ_MEDIA_IMAGES` | ✅ | Android 13+ 图片访问 |
| `READ_MEDIA_VIDEO` | ✅ | Android 13+ 视频访问 |
| `READ_EXTERNAL_STORAGE` | ✅ | maxSdkVersion=32，兼容旧版 |
| `WRITE_EXTERNAL_STORAGE` | ✅ | maxSdkVersion=32，兼容旧版 |
| `FOREGROUND_SERVICE` | ✅ | 前台服务 |
| `FOREGROUND_SERVICE_DATA_SYNC` | ✅ | Android 14+ 前台服务类型 |
| `FOREGROUND_SERVICE_REMOTE_MESSAGING` | ✅ | Android 14+ 推送前台服务 |

**已正确移除的敏感权限 / Correctly Removed：**
- `READ_PHONE_STATE`（`tools:node="remove"`）✅
- `ACCESS_BACKGROUND_LOCATION`（`tools:node="remove"`）✅
- `QUERY_ALL_PACKAGES`（`tools:node="remove"`）✅
- 蓝牙相关权限（`tools:node="remove"`）✅

**缺失 / Missing：**
- `READ_CONTACTS`：应用有联系人功能（pubspec 有 `permission_handler`），若实际访问通讯录需补充：
  ```xml
  <uses-permission android:name="android.permission.READ_CONTACTS"/>
  ```

**Play Store 政策注意 / Policy Notes：**
- `ACCESS_FINE_LOCATION` 属于危险权限，需在 Play Console 的数据安全表单中申报。
- `RECORD_AUDIO` 需在隐私政策中说明用途。

---

### 5. 版本名和版本码 ⚠️ 需同步确认

**当前配置 / Current config：**
```kotlin
versionCode = flutter.versionCode    // 来自 local.properties → pubspec.yaml：3
versionName = flutter.versionName    // 来自 local.properties → pubspec.yaml：1.0.0-rc.2
```

**问题 / Issue：**
- `versionName = "1.0.0-rc.2"` 包含预发布标签；Google Play 接受任意字符串作为版本名（不影响上架），但建议使用 `1.0.0` 作为正式发布版本名。
- `versionCode = 3` 是当前值；Play Store 要求每次上传的 versionCode 必须递增（包括内测轨更新）。

**操作建议 / Action：**
```
flutter build appbundle \
  --release \
  --build-name=1.0.0 \
  --build-number=1
```
后续每次构建递增 `--build-number`（如 `2`, `3`...）。

---

### 6. ProGuard/R8 混淆规则 ⚠️ 需补充

**当前规则覆盖 / Current coverage：**
- ✅ Flutter 核心类（`io.flutter.**`）
- ✅ 高德地图 SDK
- ✅ WebView（`@JavascriptInterface`）
- ✅ JVerify（极光认证）

**缺失的关键规则 / Missing Rules：**

```pro
# ==========================
# Firebase / FCM
# ==========================
-keep class com.google.firebase.** { *; }
-keep class com.google.android.gms.** { *; }
-dontwarn com.google.firebase.**
-dontwarn com.google.android.gms.**

# ==========================
# Sentry 错误追踪
# ==========================
-keep class io.sentry.** { *; }
-dontwarn io.sentry.**

# ==========================
# BouncyCastle 加密（E2EE 核心）
# ==========================
-keep class org.bouncycastle.** { *; }
-dontwarn org.bouncycastle.**

# ==========================
# Protobuf（WebSocket 二进制帧）
# ==========================
-keep class com.google.protobuf.** { *; }
-dontwarn com.google.protobuf.**

# ==========================
# JVerify 极光认证
# ==========================
-keep class cn.jiguang.** { *; }
-keep class cn.jpush.** { *; }
-dontwarn cn.jiguang.**
-dontwarn cn.jpush.**

# ==========================
# Flutter Secure Storage
# ==========================
-keep class com.it_nomads.fluttersecurestorage.** { *; }

# ==========================
# Kotlin 序列化
# ==========================
-keepattributes *Annotation*, InnerClasses
-dontnote kotlinx.serialization.AnnotationsKt
-keep class kotlinx.serialization.** { *; }

# ==========================
# R8 全模式兼容（Flutter 推荐）
# ==========================
-keep class androidx.lifecycle.** { *; }
-keep class androidx.arch.core.** { *; }
```

**操作建议 / Action：** 将上述规则追加到 `android/app/proguard-rules.pro`，并在 release 构建后运行 `./gradlew bundleRelease` 验证无崩溃。

---

### 7. 64 位支持（abiFilters）⚠️ 未显式配置

**当前状态 / Current state：** `build.gradle.kts` 中未配置 `abiFilters`，依赖 Flutter 默认行为。

**Flutter 默认行为 / Flutter Default：**
- `flutter build apk`：生成 fat APK（包含 arm64-v8a + armeabi-v7a + x86_64）
- `flutter build appbundle`：AAB 格式，Google Play 按设备分发对应 ABI，自动满足 64 位要求

**Play Store 要求 / Play Store Requirement：**
- 2019 年起强制要求 64 位支持（arm64-v8a）
- **推荐使用 AAB（Android App Bundle）**，而非 APK：

```bash
flutter build appbundle --release
# 输出：build/app/outputs/bundle/release/app-release.aab
```

**可选显式配置 / Optional explicit config（减小下载包体）：**
```kotlin
defaultConfig {
    ndk {
        abiFilters += listOf("arm64-v8a", "armeabi-v7a")
    }
}
```

---

### 8. Play Store 内测轨发布步骤 / Internal Testing Track Submission Steps

#### 前置准备 / Prerequisites

- [ ] Google Play Console 账号已注册（$25 一次性注册费）
- [ ] 开发者账号身份验证完成（手机号验证 + D-U-N-S 或政府 ID）
- [ ] `google-services.json` 已放置在 `android/app/`（不应提交到公开仓库）
- [ ] Release Keystore 已生成并安全备份

#### 第一步：创建应用 / Create App

1. 登录 [Google Play Console](https://play.google.com/console)
2. 点击"创建应用" → 填写应用名称（IMBoy）、默认语言（中文简体 / 英文）、类型（应用）
3. 选择应用类别：**通讯**（Communication）
4. 填写声明（免费应用、针对的受众群体）

#### 第二步：构建 AAB / Build AAB

```bash
# 清理构建缓存
flutter clean && flutter pub get

# 生成 Release AAB
flutter build appbundle \
  --release \
  --build-name=1.0.0 \
  --build-number=1 \
  --obfuscate \
  --split-debug-info=./debug-info/android/

# 输出文件
# build/app/outputs/bundle/release/app-release.aab
```

**`--split-debug-info` 说明：** 将调试符号分离输出，用于 Sentry 崩溃堆栈还原；`debug-info/` 目录不提交到 git。

#### 第三步：配置内测轨 / Configure Internal Testing Track

1. Play Console → 侧边栏"测试" → "内部测试"
2. 点击"创建新版本" → 上传 `app-release.aab`
3. 填写版本说明（What's new），例如：
   ```
   v1.0.0 内测版 / Internal Test Build
   - 核心聊天功能（文字/图片/语音/视频）
   - 端到端加密 E2EE
   - 群聊与频道
   - WebSocket 实时通讯
   ```
4. 点击"保存" → "审核版本"

#### 第四步：添加测试员 / Add Testers

1. "内部测试" → "测试人员" 标签页
2. 创建测试员列表（邮件地址，最多 100 人）
3. 或使用"测试员选择加入网址"分发链接

#### 第五步：填写商品详情 / Store Listing（内测可最小化填写）

必填项：
- [ ] 应用名称：IMBoy
- [ ] 简短说明（80 字以内）：安全、私密的即时通讯应用
- [ ] 完整说明（4000 字以内）
- [ ] 图标：512×512px（PNG，32 位）
- [ ] 特色图片：1024×500px
- [ ] 手机截图：至少 2 张，16:9 或 9:16

#### 第六步：数据安全表单 / Data Safety Form（必填）

Google Play 要求申报数据收集情况：

| 数据类型 | 是否收集 | 说明 |
|---|---|---|
| 位置（精确）| 是 | 分享位置功能 |
| 联系人 | 是 | 好友发现功能 |
| 麦克风 | 是 | 语音消息/通话 |
| 摄像头 | 是 | 视频通话/拍照 |
| 消息内容 | 是 | 端到端加密传输 |
| 设备 ID | 是 | FCM 推送令牌 |

#### 第七步：发布到内测轨 / Publish to Internal Track

1. 确认所有必填项已完成
2. 点击"推出版本" → 选择"内部测试"轨道
3. 测试员收到邮件后可通过 Play 商店安装

#### 进阶：升级到开放测试 / Upgrade to Open Testing

内测完成后：
1. Play Console → "测试" → "开放式测试"（最多 2000 名测试员）
2. 或直接 → "正式版" 提交全面审核

---

## CI/CD 自动化构建建议 / CI/CD Build Automation

```yaml
# .github/workflows/android-release.yml 示例
- name: Build Release AAB
  run: |
    echo "$KEYSTORE_BASE64" | base64 -d > android/app/keystore.jks
    cat > android/local.properties << EOF
    storeFile=keystore.jks
    keyAlias=$KEY_ALIAS
    keyPassword=$KEY_PASSWORD
    storePassword=$STORE_PASSWORD
    EOF
    flutter build appbundle \
      --release \
      --build-number=$GITHUB_RUN_NUMBER \
      --obfuscate \
      --split-debug-info=./debug-info/android/
```

---

## 合规检查清单 / Compliance Checklist

- [ ] 隐私政策 URL 已准备（HTTPS，中英双语）
- [ ] 应用不包含第三方广告 SDK（若有需额外申报）
- [ ] 出口合规：应用使用 AES-256-GCM 加密，需在 Play Console 申报加密用途
- [ ] 目标受众：非儿童应用（无需 COPPA 合规）
- [ ] 极光 JVerify（手机号认证）符合中国区运营商要求

---

*生成时间 / Generated：2026-05-27 | IMBoy Flutter 1.0.0-rc.2*
