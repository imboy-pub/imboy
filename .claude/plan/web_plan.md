# ImBoy Web 改造执行计划

> 使用 `sqflite_common_ffi` 实现 Web 平台支持
> 可在任何电脑上执行的标准流程

---

## 📋 改造计划概览

| 阶段 | 任务 | 预计时间 | 风险等级 |
|------|------|----------|----------|
| 1️⃣ 依赖配置 | 添加 Web 支持依赖 | 5 分钟 | 🟢 低 |
| 2️⃣ 数据库适配 | 改造 sqlite.dart | 15 分钟 | 🟢 低 |
| 3️⃣ WebSocket 适配 | 修改 websocket.dart | 10 分钟 | 🟢 低 |
| 4️⃣ 平台判断封装 | 创建平台适配器 | 10 分钟 | 🟢 低 |
| 5️⃣ 存储服务适配 | 修改 storage*.dart | 15 分钟 | 🟡 中 |
| 6️⃣ 条件导入优化 | 修改 dart:io 导入 | 20 分钟 | 🟡 中 |
| 7️⃣ 测试验证 | Web 构建和测试 | 15 分钟 | 🟢 低 |

**总预计时间：约 90 分钟**

---

## 📁 项目路径

```
项目根目录: /Users/leeyi/project/imboy.pub/imboyapp
改造计划文件: /Users/leeyi/project/imboy.pub/imboy/.claude/plan/web_plan.md
```

---

## 1️⃣ 依赖配置

### 1.1 修改 `pubspec.yaml`

**文件位置**: `pubspec.yaml`

**修改内容**: 在 `dependencies` 部分添加

```yaml
dependencies:
  # 现有依赖保持不变...

  # ┌─ Web 平台支持 ─────────────────────────────────────────┐
  sqflite_common: ^2.5.0              # 新增：sqflite 公共接口
  sqflite_common_ffi: ^2.4.0+2        # 新增：Web FFI 实现
  # 注意：sqflite: ^2.4.2 保持不变，移动端继续使用

  # Web 文件存储（替代部分 path_provider 功能）
  web: ^0.5.0                         # 新增：浏览器 API 封装
```

### 1.2 执行依赖安装

```bash
# 在项目根目录执行
cd /Users/leeyi/project/imboy.pub/imboyapp

# 清理并重新获取依赖
flutter clean
flutter pub get
```

---

## 2️⃣ 数据库适配

### 2.1 修改 `lib/service/sqlite.dart`

**文件位置**: `lib/service/sqlite.dart`

**步骤 1**: 在文件头部添加导入

```dart
// 在现有 import 后添加
import 'dart:io';
import 'package:sqflite/sqflite.dart';
import 'package:flutter/foundation.dart' show kIsWeb;

// 👇 新增：Web 平台导入
import 'package:sqflite_common_ffi/sqflite_ffi.dart';
```

**步骤 2**: 修改 `_initDatabase` 方法

找到 `SqliteService` 类中的数据库初始化方法，添加 Web 平台支持：

```dart
/// 初始化数据库
Future<Database> _initDatabase(String userId) async {
  // 👇 新增：Web 平台 FFI 初始化
  if (kIsWeb) {
    sqfliteFfiInit();
    databaseFactory = databaseFactoryFfi;
  }

  // 修改数据库路径以支持 Web
  final String path = kIsWeb
      ? 'imboy_$userId.db'  // Web 平台使用相对路径
      : await join(await getDatabasesPath(), 'imboy_$userId.db');

  final Database database = await openDatabase(
    path,
    version: version,
    onCreate: _onCreate,
    onUpgrade: _onUpgrade,
    onDowngrade: _onDowngrade,
    singleInstance: true,
    onConfigure: _onConfigure,
  );

  return database;
}
```

**步骤 3**: 修改 `close` 方法（如果存在）

```dart
/// 关闭数据库
Future<void> close() async {
  await _db?.close();
  _db = null;
}
```

---

## 3️⃣ WebSocket 适配

### 3.1 修改 `lib/service/websocket.dart`

**文件位置**: `lib/service/websocket.dart`

**步骤 1**: 修改导入部分（第 9-10 行）

```dart
// 原代码：
// import 'package:web_socket_channel/io.dart';
// import 'package:web_socket_channel/web_socket_channel.dart';

// 👇 修改为：
import 'package:flutter/foundation.dart' show kIsWeb;
import 'package:web_socket_channel/web_socket_channel.dart';

// 👇 条件导入：仅在非 Web 平台导入 IOWebSocketChannel
import 'package:web_socket_channel/io.dart';
```

**步骤 2**: 修改连接方法

找到创建 WebSocket 连接的代码（可能在 `openSocket` 或 `_connectWebSocket` 方法中）：

```dart
// 查找类似这样的代码：
// _channel = IOWebSocketChannel.connect(Uri.parse(wsUrl));

// 👇 修改为条件创建：
if (kIsWeb) {
  _channel = WebSocketChannel.connect(Uri.parse(wsUrl));
} else {
  _channel = IOWebSocketChannel.connect(Uri.parse(wsUrl));
}
```

---

## 4️⃣ 平台判断封装

### 4.1 创建 `lib/config/platform_adapter.dart`

**文件位置**: `lib/config/platform_adapter.dart` (新文件)

```dart
/// 平台适配器
///
/// 提供统一的平台判断接口，屏蔽各平台差异
library;

import 'package:flutter/foundation.dart' show kIsWeb;
import 'dart:io';

/// 平台适配器
///
/// 提供统一的平台判断接口，屏蔽 Web、移动端、桌面端差异
class PlatformAdapter {
  /// 当前是否为 Web 平台
  static bool get isWeb => kIsWeb;

  /// 当前是否为移动平台（iOS/Android）
  static bool get isMobile => !kIsWeb && (Platform.isIOS || Platform.isAndroid);

  /// 当前是否为桌面平台（macOS/Windows/Linux）
  static bool get isDesktop =>
      !kIsWeb && (Platform.isMacOS || Platform.isWindows || Platform.isLinux);

  /// 当前是否为 iOS
  static bool get isIOS => !kIsWeb && Platform.isIOS;

  /// 当前是否为 Android
  static bool get isAndroid => !kIsWeb && Platform.isAndroid;

  /// 当前是否为 macOS
  static bool get isMacOS => !kIsWeb && Platform.isMacOS;

  /// 当前是否为 Windows
  static bool get isWindows => !kIsWeb && Platform.isWindows;

  /// 当前是否为 Linux
  static bool get isLinux => !kIsWeb && Platform.isLinux;

  /// 根据平台选择值
  ///
  /// 示例：
  /// ```dart
  /// final path = PlatformAdapter.choose(
  ///   web: '/web/path',
  ///   mobile: '/mobile/path',
  ///   desktop: '/desktop/path',
  /// );
  /// ```
  static T choose<T>({
    required T web,
    required T mobile,
    T? desktop,
  }) {
    if (isWeb) return web;
    if (isDesktop && desktop != null) return desktop;
    return mobile;
  }

  /// 获取平台名称（用于日志和调试）
  static String get platformName {
    if (isWeb) return 'Web';
    if (isIOS) return 'iOS';
    if (isAndroid) return 'Android';
    if (isMacOS) return 'macOS';
    if (isWindows) return 'Windows';
    if (isLinux) return 'Linux';
    return 'Unknown';
  }

  /// 调试输出当前平台信息
  static void debugPrintInfo() {
    debugPrint('📱 Platform: ${PlatformAdapter.platformName}');
    debugPrint('   - isWeb: $isWeb');
    debugPrint('   - isMobile: $isMobile');
    debugPrint('   - isDesktop: $isDesktop');
  }
}

void debugPrint(String message) {
  // 简单的调试输出
  print(message);
}
```

---

## 5️⃣ 存储服务适配

### 5.1 修改 `lib/service/storage_secure.dart`

**文件位置**: `lib/service/storage_secure.dart`

**步骤 1**: 添加 Web 平台导入

```dart
import 'package:flutter/foundation.dart' show kIsWeb;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:shared_preferences/shared_preferences.dart';
```

**步骤 2**: 修改类方法以支持 Web

找到 `SecureStorageService` 类，修改其方法：

```dart
class SecureStorageService {
  static const FlutterSecureStorage _secureStorage = FlutterSecureStorage(
    aOptions: AndroidOptions(
      encryptedSharedPreferences: true,
    ),
  );

  /// 读取字符串（Web 兼容）
  static Future<String?> getString(String key) async {
    if (kIsWeb) {
      // Web 平台使用 SharedPreferences（浏览器环境无加密存储）
      final prefs = await SharedPreferences.getInstance();
      return prefs.getString('secure_$key');
    }
    return await _secureStorage.read(key: key);
  }

  /// 写入字符串（Web 兼容）
  static Future<void> setString(String key, String value) async {
    if (kIsWeb) {
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString('secure_$key', value);
      return;
    }
    await _secureStorage.write(key: key, value: value);
  }

  /// 删除键（Web 兼容）
  static Future<void> remove(String key) async {
    if (kIsWeb) {
      final prefs = await SharedPreferences.getInstance();
      await prefs.remove('secure_$key');
      return;
    }
    await _secureStorage.delete(key: key);
  }

  /// 清空所有数据（Web 兼容）
  static Future<void> clear() async {
    if (kIsWeb) {
      final prefs = await SharedPreferences.getInstance();
      final keys = prefs.getKeys().where((k) => k.startsWith('secure_'));
      for (final key in keys) {
        await prefs.remove(key);
      }
      return;
    }
    await _secureStorage.deleteAll();
  }
}
```

### 5.2 创建 `lib/service/web_storage.dart`

**文件位置**: `lib/service/web_storage.dart` (新文件)

```dart
/// Web 平台存储工具
///
/// 提供浏览器特定的存储功能（IndexedDB、文件下载等）
library;

import 'package:web/web.dart' as web;

/// Web 平台存储工具
///
/// 提供浏览器特定的存储功能
class WebStorage {
  /// IndexedDB 数据库名称
  static const String _dbName = 'imboy_db';
  static const int _dbVersion = 1;

  /// 下载文件到本地
  ///
  /// [fileName] 下载的文件名
  /// [bytes] 文件字节数据
  static void downloadFile(String fileName, List<int> bytes) {
    final blob = web.Blob([bytes]);
    final url = web.URL.createObjectURL(blob);
    final anchor = web.HTMLAnchorElement()
      ..href = url
      ..download = fileName
      ..click();
    web.URL.revokeObjectURL(url);
  }

  /// 保存数据到 IndexedDB（简化版）
  ///
  /// 实际项目中可以使用 `package:indexed_db` 或 `package:sembast_web`
  static Future<void> saveToIndexedDB(String key, dynamic value) async {
    // TODO: 实现 IndexedDB 存储
    // 可以使用第三方包：
    // - indexed_db: ^2.0.0
    // - sembast_web: ^2.0.0
  }

  /// 从 IndexedDB 读取数据
  static Future<dynamic?> getFromIndexedDB(String key) async {
    // TODO: 实现 IndexedDB 读取
    return null;
  }
}
```

---

## 6️⃣ 条件导入优化

### 6.1 需要修改的文件清单

以下文件使用了 `dart:io`，需要进行适配：

| 文件路径 | 修改方式 | 优先级 |
|---------|----------|--------|
| `lib/config/init.dart` | 添加 `kIsWeb` 判断 | P0 高 |
| `lib/page/passport/passport_notifier.dart` | 使用 `PlatformAdapter` | P0 高 |
| `lib/component/extension/device_ext.dart` | 添加 Web 设备信息 | P1 中 |
| `lib/component/voice_record/voice_widget.dart` | 禁用 Web 语音录制 | P2 低 |
| `lib/page/chat/widget/chat_input.dart` | 条件导入图片选择 | P2 低 |
| `lib/page/mine/user_device/user_device_provider.dart` | 添加 Web 分支 | P2 低 |

### 6.2 修改 `lib/config/init.dart`

**文件位置**: `lib/config/init.dart`

**步骤**: 添加 Web 平台的数据库初始化

在 `AppInitializer.initialize()` 方法中添加：

```dart
class AppInitializer {
  static Future<void> initialize() async {
    // 👇 新增：Web 平台 FFI 初始化（必须在数据库操作前）
    if (kIsWeb) {
      // 仅在导入时初始化一次
      // sqfliteFfiInit(); // 这个可以放在 sqlite.dart 中
    }

    // 现有初始化代码...
    await StorageService.init();
    await UserRepoLocal.onInit();
    // ...
  }
}
```

### 6.3 修改 `lib/component/extension/device_ext.dart`

**文件位置**: `lib/component/extension/device_ext.dart`

**步骤**: 添加 Web 平台的设备信息获取

```dart
import 'package:imboy/config/platform_adapter.dart';
import 'package:flutter/foundation.dart' show kIsWeb, debugPrint;

class DeviceExt {
  static DeviceExt? _instance;
  static DeviceExt get did => _instance ??= DeviceExt._internal();
  DeviceExt._internal();

  Map<String, dynamic> _info = {};

  /// 获取设备信息（Web 兼容）
  Future<Map<String, dynamic>> get info async {
    if (_info.isNotEmpty) return _info;

    if (kIsWeb) {
      _info = await _getWebDeviceInfo();
    } else {
      _info = await _getMobileDeviceInfo();
    }

    return _info;
  }

  /// Web 平台设备信息
  Future<Map<String, dynamic>> _getWebDeviceInfo() async {
    return {
      'platform': 'web',
      'userAgent': web.window.navigator.userAgent,
      'language': web.window.navigator.language,
      'screenWidth': web.window.screen.width,
      'screenHeight': web.window.screen.height,
      'vendor': web.window.navigator.vendor,
    };
  }

  /// 移动端设备信息
  Future<Map<String, dynamic>> _getMobileDeviceInfo() async {
    // 现有实现保持不变...
    final deviceInfo = DeviceInfoPlugin();
    // ...
  }
}
```

---

## 7️⃣ 测试验证

### 7.1 本地开发测试

```bash
# 在项目根目录执行
cd /Users/leeyi/project/imboy.pub/imboyapp

# Chrome 测试（推荐）
flutter run -d chrome

# 或使用 Edge
flutter run -d edge

# 预期结果：
# ✅ 应用在浏览器中启动
# ✅ 登录页面正常显示
# ✅ 可以输入账号密码
# ✅ WebSocket 连接成功
# ✅ 登录后进入会话列表
```

### 7.2 构建测试

```bash
# Web 构建
flutter build web --release

# 预期结果：
# ✅ build/web 目录生成
# ✅ 无编译错误
# ✅ main.dart.js 文件生成
# ✅ assets 文件正确复制
```

### 7.3 本地服务器测试

```bash
# 进入构建目录
cd build/web

# 方式 1: 使用 Python
python3 -m http.server 8080

# 方式 2: 使用 Node.js
npx http-server -p 8080

# 方式 3: 使用 PHP
php -S localhost:8080

# 访问 http://localhost:8080
```

---

## 📝 改造检查清单

执行此清单，逐项检查：

```
✅ 1. pubspec.yaml 已添加 sqflite_common 和 sqflite_common_ffi
✅ 2. flutter clean && flutter pub get 执行成功
✅ 3. lib/config/platform_adapter.dart 文件已创建
✅ 4. lib/service/sqlite.dart 已添加 Web FFI 初始化
✅ 5. lib/service/websocket.dart 已修改连接创建代码
✅ 6. lib/service/storage_secure.dart 已添加 Web 分支
✅ 7. lib/service/web_storage.dart 文件已创建
✅ 8. lib/config/init.dart 已添加 Web 初始化
✅ 9. lib/component/extension/device_ext.dart 已添加 Web 分支
✅ 10. dart format . 执行成功
✅ 11. flutter analyze 无错误
✅ 12. flutter build web --release 成功
✅ 13. 本地服务器正在运行 (http://localhost:8080)
✅ 14. device_ext_web.dart 已修复并优化
✅ 15. passport_notifier.dart 已添加 JVerify Web 兼容
✅ 16. chat_input.dart 已添加字体大小 Web 兼容
```

---

## 🚀 WhatsApp Web 风格增强功能

### Phase 1 - 已完成 ✅

| 功能 | 前端 | 后端 | 状态 |
|------|------|------|------|
| QR 码扫码登录 | `lib/page/passport/web_login_page.dart` | `src/api/qr_login_handler.erl` | ✅ |
| Web 登录页面 | WhatsApp Web 风格双栏布局 | - | ✅ |
| 设备 ID 管理 | `lib/service/web_storage.dart` | - | ✅ |
| 桌面通知服务 | `lib/service/web_notification_service.dart` | - | ✅ |
| 文件拖拽上传 | `lib/component/web/drop_zone.dart` | - | ✅ |
| 键盘快捷键 | `lib/component/web/keyboard_shortcuts.dart` | - | ✅ |

### Phase 2 - 已完成 ✅

| 功能 | 前端 | 后端 | 状态 |
|------|------|------|------|
| 暗色模式切换 | 已有 Riverpod 主题系统 | - | ✅ 使用现有实现 |
| 消息搜索 | `lib/page/search/web_search_page.dart` | - | ✅ |
| 响应式布局 | `lib/component/web/responsive_layout.dart` | - | ✅ |
| Web 会话列表 | `lib/page/conversation/web_conversation_page.dart` | - | ✅ |
| 多标签页同步 | `lib/service/web_tab_sync_service.dart` | - | ✅ |

### Phase 3 - 已完成 ✅

| 功能 | 前端 | 后端 | 状态 |
|------|------|------|------|
| PWA 离线支持 | `lib/service/pwa_service.dart` + `web/manifest.json` | - | ✅ |
| 多标签页同步 | `lib/service/web_tab_sync_service.dart` | - | ✅ |
| 视频通话 | `lib/service/webrtc_service.dart` | - | ✅ |
| 屏幕共享 | `lib/service/webrtc_service.dart` | - | ✅ |

---

## 📁 新增文件清单

### 前端文件

```
lib/
├── page/passport/
│   └── web_login_page.dart          # Web 专用登录页面（QR码登录）
├── page/conversation/
│   └── web_conversation_page.dart   # Web 会话列表页面
├── page/search/
│   └── web_search_page.dart         # Web 全局搜索页面
├── service/
│   ├── web_storage.dart             # Web 存储服务（更新）
│   ├── web_notification_service.dart # 桌面通知服务
│   ├── web_tab_sync_service.dart    # 多标签页同步服务
│   └── pwa_service.dart             # PWA 离线支持服务
└── component/web/
    ├── drop_zone.dart               # 文件拖拽组件
    ├── keyboard_shortcuts.dart      # 键盘快捷键
    └── responsive_layout.dart       # 响应式布局组件

web/
└── manifest.json                    # PWA 清单（更新）
```

### 后端文件

```
src/
├── api/
│   └── qr_login_handler.erl         # QR 码登录 API
└── include/
    └── error_code.hrl               # 新增 QR 登录错误码（5200-5205）
```

---

## 🚨 常见问题处理

### 问题 1：编译错误 "Target of URI doesn't exist"

**原因**: 依赖未正确安装或导入路径错误

**解决方案**:
```bash
flutter clean
flutter pub get
flutter pub upgrade
```

### 问题 2：WebSocket 连接失败

**原因**: Web 平台对 WebSocket 有特殊要求

**解决方案**:
```dart
// 确保使用正确的协议
final wsUrl = kIsWeb
    ? 'wss://your-domain.com/ws'  // Web 必须使用 wss://
    : 'ws://your-domain.com/ws';  // 移动端可以使用 ws://
```

### 问题 3：数据库初始化失败

**原因**: FFI 未在正确的时机初始化

**解决方案**:
```dart
// 在 main.dart 中确保正确初始化
void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  // Web 平台 FFI 初始化
  if (kIsWeb) {
    sqfliteFfiInit();
    databaseFactory = databaseFactoryFfi;
  }

  runApp(const MyApp());
}
```

### 问题 4：浏览器控制台报错 "sqflite is not defined"

**原因**: Web 构建缺少必要的 JavaScript 文件

**解决方案**:
```bash
# 确保 flutter build web 成功
# 检查 build/web 目录中是否包含：
# - main.dart.js
# - flutter.js
# - assets/
# - canvaskit/
```

---

## 📦 完整文件清单

### 新增文件（3 个）
```
lib/config/platform_adapter.dart          # 平台适配器
lib/service/web_storage.dart              # Web 存储工具
```

### 修改文件（6 个核心）
```
pubspec.yaml                              # 添加依赖
lib/service/sqlite.dart                   # Web 数据库支持
lib/service/websocket.dart                # WebSocket 连接适配
lib/service/storage_secure.dart           # 安全存储 Web 兼容
lib/config/init.dart                      # 初始化流程
lib/component/extension/device_ext.dart   # 设备信息获取
```

### 可选修改文件（按需）
```
lib/page/passport/passport_notifier.dart    # 登录逻辑
lib/component/voice_record/voice_widget.dart # 语音录制
lib/page/chat/widget/chat_input.dart        # 聊天输入
```

---

## 🚀 下一步计划

完成基础 Web 支持后，可以考虑以下增强功能：

### Phase 2 - 功能增强
- [ ] 图片选择器 Web 实现（使用 `file_picker` 或 HTML API）
- [ ] 文件上传/下载功能
- [ ] 消息历史加载优化
- [ ] 响应式 UI 优化

### Phase 3 - 体验优化
- [ ] PWA 支持（Service Worker、Manifest）
- [ ] 深链接支持
- [ ] 浏览器通知
- [ ] 离线缓存

---

## 📞 技术支持

如遇到问题，请检查：

1. **Flutter 版本**: 确保 Flutter SDK >= 3.8.0
2. **依赖版本**: 确保所有依赖版本兼容
3. **浏览器版本**: 推荐使用 Chrome/Edge 最新版本
4. **日志输出**: 查看 `flutter run -d chrome` 的控制台输出

---

**文档版本**: v1.1
**最后更新**: 2026-02-14
**适用项目**: ImBoy App
**目标平台**: Web 浏览器（Chrome、Edge、Firefox、Safari）
