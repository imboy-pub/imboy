# iOS App Store 上架准备清单
# iOS App Store Submission Checklist

> 基于文件扫描日期 / Based on file scan date：2026-05-27
> 项目版本 / Project version：`1.0.0-rc.2+3`（pubspec.yaml）

---

## 核查结果汇总 / Summary

| # | 核查项 / Check Item | 状态 / Status |
|---|---|---|
| 1 | Bundle ID 格式 | ⚠️ 需确认 |
| 2 | Privacy Usage Descriptions | ⚠️ 部分缺失 |
| 3 | 版本号 / Build 号 | ⚠️ 需同步 |
| 4 | 最低 iOS 版本 | ✅ 已就绪 |
| 5 | App 图标完整性 | ✅ 已就绪 |
| 6 | LaunchScreen.storyboard | ✅ 已就绪 |
| 7 | 网络权限（ATS） | ⚠️ 需补全 |
| 8 | 推送通知 Entitlement | ❌ 缺失 |
| 9 | UIBackgroundModes | ⚠️ 不完整 |
| 10 | TestFlight 材料清单 | ⚠️ 待准备 |

---

## 详细核查 / Detailed Checks

### 1. Bundle ID 格式 ⚠️ 需确认

**当前值 / Current value：** `pub.imboy.2`（来自 `project.pbxproj`）

**问题 / Issue：**
- 标准反域名格式应为 `pub.imboy.app` 或 `pub.imboy.imboy`，末尾数字 `.2` 不符合常规命名规范。
- 一旦提交 App Store 后 Bundle ID **不可修改**，需在首次提交前确认。
- `Info.plist` 中使用 `$(PRODUCT_BUNDLE_IDENTIFIER)` 变量引用，实际值由 Xcode build settings 决定。

**操作建议 / Action：**
```
在 App Store Connect 中注册 Bundle ID 时，确认使用 pub.imboy.2 还是
更改为 pub.imboy.app（推荐）。更改后同步更新 project.pbxproj 所有
配置节（Debug / Release / Profile）。
```

---

### 2. Privacy Usage Descriptions ⚠️ 部分缺失

**已配置 / Configured：**

| 键 / Key | 描述语言 | 状态 |
|---|---|---|
| `NSCameraUsageDescription` | 英文 | ✅ |
| `NSMicrophoneUsageDescription` | 英文 | ✅ |
| `NSLocationWhenInUseUsageDescription` | 英文 | ✅ |
| `NSPhotoLibraryUsageDescription` | 中文 | ✅ |
| `NSPhotoLibraryAddUsageDescription` | 中文 | ✅ |

**缺失 / Missing：**

| 键 / Key | 用途 | 状态 |
|---|---|---|
| `NSContactsUsageDescription` | 通讯录（联系人功能，pubspec 依赖 `permission_handler`）| ❌ 缺失 |
| `NSUserNotificationsUsageDescription` | 推送通知（`firebase_messaging` / `flutter_local_notifications`）| ❌ 缺失 |
| `NSLocationAlwaysUsageDescription` | 后台定位（若需要）| ❌ 缺失（按需添加）|
| `NSSpeechRecognitionUsageDescription` | 语音转文字（`flutter_sound` 录音功能）| ❌ 缺失 |
| `NSBluetoothAlwaysUsageDescription` | 蓝牙（Entitlements 已配 `device.bluetooth`）| ⚠️ 按需确认 |

**操作建议 / Action：**
在 `imboyapp/ios/Runner/Info.plist` 中补全以下键值（描述语言建议中英双语）：
```xml
<key>NSContactsUsageDescription</key>
<string>IMBoy needs access to your contacts to help you find and invite friends.</string>

<key>NSUserNotificationsUsageDescription</key>
<string>IMBoy needs notification permission to alert you of new messages and calls.</string>

<key>NSSpeechRecognitionUsageDescription</key>
<string>IMBoy uses speech recognition to convert your voice messages to text.</string>
```

**注意 / Note：** 描述字符串必须明确说明用途；审核人员会拒绝泛泛的描述（如"需要访问相机"）。

---

### 3. 版本号（CFBundleShortVersionString）和 Build 号（CFBundleVersion）⚠️ 需同步

**当前状态 / Current state：**

| 来源 / Source | 版本 / Version | Build / Build |
|---|---|---|
| `pubspec.yaml` | `1.0.0-rc.2` | `3` |
| `project.pbxproj` MARKETING_VERSION | `1.0.0` | — |
| `project.pbxproj` CURRENT_PROJECT_VERSION | — | `1` |

**问题 / Issue：**
- `Info.plist` 使用 `$(FLUTTER_BUILD_NAME)` 和 `$(FLUTTER_BUILD_NUMBER)` 变量，Flutter 构建时会从 `pubspec.yaml` 注入。
- `project.pbxproj` 中的硬编码 `MARKETING_VERSION = 1.0.0` 与 pubspec 的 `1.0.0-rc.2` 不一致（rc 预发布标签在 App Store 会被截断）。
- App Store 要求版本号为三段数字格式（`x.y.z`），**不接受预发布标签**（`-rc.2`）。

**操作建议 / Action：**
1. 正式提交前将 `pubspec.yaml` 的版本改为 `1.0.0+1`（去除 `-rc.2` 后缀）。
2. Build 号（`+N`）每次提交必须递增，App Store Connect 拒绝重复 Build 号。
3. 使用 `flutter build ipa --build-name=1.0.0 --build-number=1` 覆盖构建参数。

---

### 4. 最低 iOS 版本 ✅ 已就绪

**当前值 / Current value：** `IPHONEOS_DEPLOYMENT_TARGET = 13.0`（所有配置节一致）

**状态 / Status：** iOS 13.0 满足 App Store 最低要求（Apple 目前要求 iOS 16+，但 13 仍可提交），覆盖主流设备。

**建议 / Recommendation：** 考虑在 1.0 GA 后将目标升至 iOS 16，以使用更多系统 API 并符合 Apple 2025 年的最新指引。

---

### 5. App 图标完整性 ✅ 已就绪

**已找到的图标文件 / Found icons（`Assets.xcassets/AppIcon.appiconset/`）：**

| 文件 | 用途 |
|---|---|
| `ios-marketing-1024x1024.png`（1.5MB）| App Store 展示图标 ✅ |
| `iphone-20x20@2x/3x` | 通知图标 ✅ |
| `iphone-29x29@2x/3x` | Settings 图标 ✅ |
| `iphone-40x40@2x/3x` | Spotlight 图标 ✅ |
| `iphone-60x60@2x/3x` | 主屏图标 ✅ |
| `ipad-*` 系列 | iPad 图标 ✅ |

**注意 / Note：** 1024×1024 App Store 图标文件大小为 1.5MB，需确认图像质量（无 alpha 通道、无圆角遮罩，Apple 自动添加圆角）。

---

### 6. LaunchScreen.storyboard ✅ 已就绪

**证据 / Evidence：**
- `Info.plist` 中 `UILaunchStoryboardName = LaunchScreen`
- `ios/Runner/Base.lproj/` 目录存在（包含 storyboard）

**建议 / Recommendation：** 确认 LaunchScreen 无硬编码文字（避免 i18n 问题），使用 App 图标 + 纯色背景为最佳实践。

---

### 7. 网络权限（NSAppTransportSecurity / ATS）⚠️ 需补全

**当前配置 / Current config：**
```xml
<key>NSAppTransportSecurity</key>
<dict>
    <key>NSAllowsLocalNetworking</key>
    <true/>
</dict>
```

**问题 / Issue：**
- 仅允许了本地网络（`NSAllowsLocalNetworking`），对公网域名未配置例外。
- 应用使用 `dio` + WebSocket 连接生产服务器，若服务器支持 HTTPS/TLS，则默认 ATS 策略已覆盖，无需额外配置。
- 但若后端在测试/生产中任何场景使用 HTTP 明文，审核会被拒绝。

**操作建议 / Action：**
1. 确认生产服务器全部为 HTTPS（项目使用 nginx + certbot 自动 TLS，应已满足）。
2. 移除 `NSAllowsLocalNetworking`（生产包中无意义），或保留仅用于开发。
3. **绝对不要**添加 `NSAllowsArbitraryLoads = true`（App Store 会要求说明理由）。

---

### 8. 推送通知 Entitlement ❌ 缺失

**当前状态 / Current state：**
- `Runner.entitlements`（Debug）：无 `aps-environment`
- `RunnerRelease.entitlements`：无 `aps-environment`
- `RunnerProfile.entitlements`：无 `aps-environment`

**问题 / Issue：**
- 项目依赖 `firebase_messaging`（FCM），必须配置 APNs 推送通知 Entitlement。
- 没有此 Entitlement，App 无法在真机上注册推送，TestFlight 测试时推送完全失效。

**操作建议 / Action：**

1. 在 Xcode → Signing & Capabilities → 点击"+"→ 添加 **Push Notifications**
2. Xcode 会自动在 Entitlements 文件中添加：
```xml
<key>aps-environment</key>
<string>development</string>   <!-- Debug 配置 -->
```
```xml
<key>aps-environment</key>
<string>production</string>    <!-- Release / Profile 配置 -->
```
3. 在 Apple Developer Portal 中启用 App ID 的 Push Notifications 能力。
4. 上传 APNs 证书或 APNs Auth Key（.p8）到 Firebase Console。

---

### 9. UIBackgroundModes ⚠️ 不完整

**当前配置 / Current config：**
```xml
<key>UIBackgroundModes</key>
<array>
    <string>audio</string>
</array>
```

**已有 / Present：** `audio`（支持后台音频录制/播放）

**缺失 / Missing：**

| Background Mode | 用途 | 建议 |
|---|---|---|
| `remote-notification` | 推送唤醒（FCM 静默推送）| ❌ 必须添加 |
| `fetch` | 后台数据刷新（可选）| ⚠️ 按需 |
| `voip` | VoIP 通话（WebRTC，`flutter_webrtc` 依赖）| ⚠️ 若支持来电需要 |

**操作建议 / Action：**
在 `Info.plist` 的 `UIBackgroundModes` 数组中添加：
```xml
<string>remote-notification</string>
```

若 WebRTC 视频/语音通话需要后台接听（CallKit 集成），还需添加：
```xml
<string>voip</string>
```

---

### 10. TestFlight 内测材料清单 ⚠️ 待准备

以下材料需在 App Store Connect 填写后才能分发 TestFlight：

#### 必填 / Required

- [ ] **App 名称**：IMBoy（已确认，`CFBundleDisplayName = IMBoy`）
- [ ] **主要语言**：中文（简体）或英文
- [ ] **Bundle ID**：在 App Store Connect 中注册（见核查项 1）
- [ ] **SKU**：唯一产品标识符（如 `imboy-pub-2026`）
- [ ] **测试说明（What to Test）**：告知测试人员重点功能和测试路径，英文，最多 4000 字符
  - 建议内容：注册/登录流程、发送消息（文字/图片/语音）、群聊、WebSocket 断线重连、E2E 加密验证
- [ ] **Beta App 描述**：面向测试员的应用简介，英文
- [ ] **反馈邮箱**：接收测试反馈
- [ ] **隐私政策 URL**：App Store 审核必需，需有效的 HTTPS 链接

#### 截图 / Screenshots（TestFlight 可跳过，正式上架必需）

| 设备 | 尺寸 |
|---|---|
| iPhone 6.9 英寸（iPhone 16 Pro Max）| 1320×2868px |
| iPhone 6.7 英寸（iPhone 15 Plus）| 1290×2796px |
| iPad Pro 12.9 英寸（第 6 代）| 2048×2732px |

每个尺寸至少 1 张，最多 10 张。建议展示：主界面、聊天页、群聊、个人资料页。

#### 出口合规 / Export Compliance

- 当前 `Info.plist` 已设置 `ITSAppUsesNonExemptEncryption = false`（✅ 已配置）
- 但应用使用 RSA-OAEP + AES-256-GCM 端到端加密，需在 App Store Connect 的出口合规问卷中**据实填写**。
- 建议咨询法务确认是否需要 ERN（Encryption Registration Number）。

#### Firebase / Google Service ⚠️

- 确认 `ios/` 目录下存在 `GoogleService-Info.plist`（不应提交到公开仓库）
- FCM APNs Key 已上传到 Firebase Console

---

## 上架流程参考 / Submission Flow

```
1. flutter build ipa --release --build-name=1.0.0 --build-number=1
2. 使用 Xcode Organizer 或 Transporter 上传 .ipa
3. App Store Connect → TestFlight → 填写测试说明 → 邀请内测员
4. 内测完成后 → App Review → 提交正式审核
5. 审核通过 → 手动/自动发布
```

---

*生成时间 / Generated：2026-05-27 | IMBoy Flutter 1.0.0-rc.2*
