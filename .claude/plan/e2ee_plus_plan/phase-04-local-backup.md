# 阶段 4: 本地备份

> **预计工期**: 3-5 天
> **依赖**: 阶段 1（准备工作）
> **安全等级**: ⭐⭐⭐⭐⭐（最安全，但需要用户负责）

---

## 目标

实现本地加密备份文件功能：
1. 前端：备份导出/导入、密码管理、文件验证
2. 后端：备份元数据记录（可选）
3. 完整的备份/恢复流程测试

---

## 安全原则

```
┌────────────────────────────────────────────────────────────┐
│                      安全设计原则                           │
├────────────────────────────────────────────────────────────┤
│ 1. 备份文件完全由用户控制 - 服务器不存储                   │
│ 2. 使用 PBKDF2 派生密钥 - 310,000 次迭代                   │
│ 3. 使用 AES-256-GCM 加密 - 提供认证加密                    │
│ 4. 密码不由服务器存储 - 遗忘无法恢复                       │
│ 5. 包含完整性校验 - SHA-256                                │
│ 6. 备份文件可存储到任何地方 - 邮件、云盘、U盘等            │
└────────────────────────────────────────────────────────────┘
```

---

## 备份文件格式

### 文件结构

```
imboy_e2ee_backup_v1.enc
├── [Header - 32 bytes]
│   ├── Magic Number (8 bytes): "IMBOYBKP"
│   ├── Version (2 bytes): 0x0001
│   ├── Algorithm ID (2 bytes): 0x0001 (PBKDF2+AES256GCM)
│   ├── PBKDF2 Iterations (4 bytes): 310000
│   ├── Salt Length (2 bytes): 16
│   ├── IV Length (2 bytes): 12
│   ├── Auth Tag Length (2 bytes): 16
│   └── Reserved (6 bytes)
├── [Salt - 16 bytes]
├── [IV - 12 bytes]
├── [Auth Tag - 16 bytes]
├── [Encrypted Data - variable]
│   ├── [Private Key - variable]
│   ├── [Device ID - variable]
│   ├── [Timestamp - 8 bytes]
│   └── [SHA-256 Checksum - 32 bytes]
└── [Footer - optional]
    └── [User Notes - variable]
```

### 备份数据格式（加密前）

```json
{
  "version": 1,
  "device_id": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  "private_key": "-----BEGIN RSA PRIVATE KEY-----\n...\n-----END RSA PRIVATE KEY-----",
  "public_key": "-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----",
  "key_id": "key_xxxxx",
  "created_at": "2026-01-30T10:00:00Z",
  "checksum": "sha256:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
}
```

---

## 步骤 1: 前端 - 备份服务实现

### 1.1 创建备份服务

```dart
// lib/service/e2ee_local_backup_service.dart

import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';
import 'package:crypto/crypto.dart';
import 'package:imboy/service/storage_service.dart';
import 'package:imboy/service/cipher_service.dart';
import 'package:path_provider/path_provider.dart';
import 'package:share_plus/share_plus.dart';

/// E2EE 本地备份服务
class E2EELocalBackupService {
  // 备份文件版本
  static const int _backupVersion = 1;

  // 加密算法标识
  static const int _algorithmPbkdf2Aes256Gcm = 0x0001;

  // PBKDF2 迭代次数（符合 OWASP 2021 建议）
  static const int _pbkdf2Iterations = 310000;

  // Salt 长度
  static const int _saltLength = 16;

  // IV 长度（GCM 推荐）
  static const int _ivLength = 12;

  // Auth Tag 长度（GCM 推荐）
  static const int _authTagLength = 16;

  /// 导出备份
  ///
  /// [password] 备份密码（必须由用户记住）
  /// [userNotes] 用户备注（可选）
  ///
  /// 返回备份文件路径
  static Future<String> exportBackup({
    required String password,
    String? userNotes,
  }) async {
    // 1. 验证密码强度
    _validatePassword(password);

    // 2. 获取当前设备的密钥对
    final privateKey = await StorageService.getE2EEPrivateKey();
    final publicKey = await StorageService.getE2EEPublicKey();
    final deviceId = await StorageService.getDeviceId();
    final keyId = await StorageService.getE2EEKeyId();

    if (privateKey == null || publicKey == null) {
      throw Exception('密钥不存在');
    }

    // 3. 构建备份数据
    final backupData = {
      'version': _backupVersion,
      'device_id': deviceId,
      'private_key': privateKey,
      'public_key': publicKey,
      'key_id': keyId,
      'created_at': DateTime.now().toUtc().toIso8601String(),
    };

    // 4. 计算 SHA-256 校验和
    final checksum = sha256.convert(utf8.encode(json.encode(backupData)));
    backupData['checksum'] = 'sha256:$checksum';

    // 5. 序列化备份数据
    final plaintext = utf8.encode(json.encode(backupData));

    // 6. 生成随机 Salt 和 IV
    final salt = _generateRandomBytes(_saltLength);
    final iv = _generateRandomBytes(_ivLength);

    // 7. 使用 PBKDF2 派生密钥
    final derivedKey = await _deriveKey(password, salt);

    // 8. 使用 AES-256-GCM 加密
    final encryptedDataWithTag = await _encryptAesGcm(
      plaintext,
      derivedKey,
      iv,
    );

    // 9. 分离密文和认证标签
    final ciphertext = encryptedDataWithTag.sublist(
      0,
      encryptedDataWithTag.length - _authTagLength,
    );
    final authTag = encryptedDataWithTag.sublist(
      encryptedDataWithTag.length - _authTagLength,
    );

    // 10. 构建备份文件
    final backupFile = await _buildBackupFile(
      salt: salt,
      iv: iv,
      authTag: authTag,
      ciphertext: ciphertext,
      userNotes: userNotes,
    );

    // 11. 保存到临时目录
    final tempDir = await getTemporaryDirectory();
    final timestamp = DateTime.now().toIso8601String().replaceAll(':', '-');
    final filePath = '${tempDir.path}/imboy_e2ee_backup_$timestamp.enc';
    final file = File(filePath);
    await file.writeAsBytes(backupFile);

    return filePath;
  }

  /// 导入备份
  ///
  /// [filePath] 备份文件路径
  /// [password] 备份密码
  ///
  /// 返回导入的密钥信息
  static Future<Map<String, dynamic>> importBackup({
    required String filePath,
    required String password,
  }) async {
    // 1. 读取备份文件
    final file = File(filePath);
    if (!await file.exists()) {
      throw Exception('备份文件不存在');
    }

    final fileBytes = await file.readAsBytes();

    // 2. 解析文件头
    final header = _parseHeader(fileBytes);

    // 3. 提取加密组件
    final salt = fileBytes.sublist(32, 32 + _saltLength);
    final iv = fileBytes.sublist(
      32 + _saltLength,
      32 + _saltLength + _ivLength,
    );
    final authTag = fileBytes.sublist(
      32 + _saltLength + _ivLength,
      32 + _saltLength + _ivLength + _authTagLength,
    );
    final ciphertext = fileBytes.sublist(
      32 + _saltLength + _ivLength + _authTagLength,
      fileBytes.length - (header['footer_length'] ?? 0),
    );

    // 4. 验证算法
    if (header['algorithm'] != _algorithmPbkdf2Aes256Gcm) {
      throw Exception('不支持的加密算法');
    }

    // 5. 派生密钥
    final derivedKey = await _deriveKey(password, salt);

    // 6. 解密数据
    final decryptedData = await _decryptAesGcm(
      ciphertext,
      derivedKey,
      iv,
      authTag,
    );

    // 7. 解析备份数据
    final backupData = json.decode(utf8.decode(decryptedData));

    // 8. 验证校验和
    final storedChecksum = backupData['checksum'];
    final dataWithoutChecksum = Map<String, dynamic>.from(backupData);
    dataWithoutChecksum.remove('checksum');
    final calculatedChecksum = sha256.convert(
      utf8.encode(json.encode(dataWithoutChecksum)),
    );

    if ('sha256:$calculatedChecksum' != storedChecksum) {
      throw Exception('备份文件已损坏（校验和不匹配）');
    }

    // 9. 验证版本
    if (backupData['version'] != _backupVersion) {
      throw Exception('不支持的备份版本');
    }

    // 10. 存储密钥
    await StorageService.saveE2EEPrivateKey(backupData['private_key']);
    await StorageService.saveE2EEPublicKey(backupData['public_key']);
    await StorageService.saveE2EEKeyId(backupData['key_id']);

    return {
      'device_id': backupData['device_id'],
      'key_id': backupData['key_id'],
      'created_at': backupData['created_at'],
    };
  }

  /// 验证备份文件（不解密）
  static Future<Map<String, dynamic>> verifyBackup(String filePath) async {
    final file = File(filePath);
    if (!await file.exists()) {
      throw Exception('备份文件不存在');
    }

    final fileBytes = await file.readAsBytes();
    final header = _parseHeader(fileBytes);

    return {
      'version': header['version'],
      'algorithm': header['algorithm'],
      'iterations': header['iterations'],
      'file_size': fileBytes.length,
      'is_valid': true,
    };
  }

  /// 分享备份文件（邮件、云盘等）
  static Future<void> shareBackup(String filePath) async {
    await Share.shareXFiles([XFile(filePath)]);
  }

  /// 验证密码强度
  static void _validatePassword(String password) {
    if (password.length < 12) {
      throw Exception('密码长度至少 12 位');
    }

    // 检查密码复杂度
    final hasLower = password.contains(RegExp(r'[a-z]'));
    final hasUpper = password.contains(RegExp(r'[A-Z]'));
    final hasDigit = password.contains(RegExp(r'[0-9]'));
    final hasSpecial = password.contains(RegExp(r'[!@#$%^&*(),.?":{}|<>]'));

    final complexityScore = [hasLower, hasUpper, hasDigit, hasSpecial]
        .where((e) => e)
        .length;

    if (complexityScore < 3) {
      throw Exception('密码必须包含大写字母、小写字母、数字和特殊符号中的至少 3 种');
    }
  }

  /// 使用 PBKDF2 派生密钥
  static Future<Uint8List> _deriveKey(
    String password,
    Uint8List salt,
  ) async {
    // 使用 Flutter 的 crypto 库实现 PBKDF2
    // 这里简化为伪代码，实际需要使用 pointycastle 或类似库
    final passwordBytes = utf8.encode(password);

    // 伪代码：实际需要调用真正的 PBKDF2 实现
    // final derivedKey = pbkdf2(
    //   password: passwordBytes,
    //   salt: salt,
    //   iterations: _pbkdf2Iterations,
    //   keyLength: 32, // 256 bits
    //   hash: sha256,
    // );

    // TODO: 实现真正的 PBKDF2-HMAC-SHA256
    throw UnimplementedError('PBKDF2 implementation required');
  }

  /// 使用 AES-256-GCM 加密
  static Future<Uint8List> _encryptAesGcm(
    Uint8List plaintext,
    Uint8List key,
    Uint8List iv,
  ) async {
    // 使用 Flutter 的 cipher 库实现 AES-GCM
    // 这里简化为伪代码
    // TODO: 实现真正的 AES-256-GCM
    throw UnimplementedError('AES-256-GCM implementation required');
  }

  /// 使用 AES-256-GCM 解密
  static Future<Uint8List> _decryptAesGcm(
    Uint8List ciphertext,
    Uint8List key,
    Uint8List iv,
    Uint8List authTag,
  ) async {
    // TODO: 实现真正的 AES-256-GCM 解密
    throw UnimplementedError('AES-256-GCM decryption implementation required');
  }

  /// 生成随机字节
  static Uint8List _generateRandomBytes(int length) {
    final random = SecureRandom();
    return Uint8List.fromList(
      List.generate(length, (_) => random.nextInt(256)),
    );
  }

  /// 构建备份文件
  static Future<Uint8List> _buildBackupFile({
    required Uint8List salt,
    required Uint8List iv,
    required Uint8List authTag,
    required Uint8List ciphertext,
    String? userNotes,
  }) async {
    final output = BytesBuilder();

    // 1. 写入文件头（32 bytes）
    output.add(_buildHeader());

    // 2. 写入 Salt
    output.add(salt);

    // 3. 写入 IV
    output.add(iv);

    // 4. 写入 Auth Tag
    output.add(authTag);

    // 5. 写入密文
    output.add(ciphertext);

    // 6. 写入用户备注（可选）
    if (userNotes != null && userNotes.isNotEmpty) {
      final notesBytes = utf8.encode(userNotes);
      final notesLength = ByteData(4)..setUint32(0, notesBytes.length);
      output.add(notesLength.buffer.asUint8List());
      output.add(notesBytes);
    }

    return output.toBytes();
  }

  /// 构建文件头
  static Uint8List _buildHeader() {
    final header = BytesBuilder();

    // Magic Number (8 bytes): "IMBOYBKP"
    header.add(utf8.encode('IMBOYBKP'));

    // Version (2 bytes)
    final versionData = ByteData(2)..setUint16(0, _backupVersion);
    header.add(versionData.buffer.asUint8List());

    // Algorithm ID (2 bytes)
    final algoData = ByteData(2)..setUint16(0, _algorithmPbkdf2Aes256Gcm);
    header.add(algoData.buffer.asUint8List());

    // PBKDF2 Iterations (4 bytes)
    final iterData = ByteData(4)..setUint32(0, _pbkdf2Iterations);
    header.add(iterData.buffer.asUint8List());

    // Salt Length (2 bytes)
    final saltLenData = ByteData(2)..setUint16(0, _saltLength);
    header.add(saltLenData.buffer.asUint8List());

    // IV Length (2 bytes)
    final ivLenData = ByteData(2)..setUint16(0, _ivLength);
    header.add(ivLenData.buffer.asUint8List());

    // Auth Tag Length (2 bytes)
    final tagLenData = ByteData(2)..setUint16(0, _authTagLength);
    header.add(tagLenData.buffer.asUint8List());

    // Reserved (6 bytes)
    final reserved = Uint8List(6);
    header.add(reserved);

    return header.toBytes();
  }

  /// 解析文件头
  static Map<String, dynamic> _parseHeader(Uint8List fileBytes) {
    if (fileBytes.length < 32) {
      throw Exception('文件格式无效');
    }

    final header = fileBytes.sublist(0, 32);
    final byteData = ByteData.sublistView(ByteData(4), header.buffer);

    // 验证 Magic Number
    final magic = utf8.decode(header.sublist(0, 8));
    if (magic != 'IMBOYBKP') {
      throw Exception('不是有效的 Imboy 备份文件');
    }

    return {
      'magic': magic,
      'version': byteData.getUint16(8),
      'algorithm': byteData.getUint16(10),
      'iterations': byteData.getUint32(12),
      'salt_length': byteData.getUint16(16),
      'iv_length': byteData.getUint16(18),
      'auth_tag_length': byteData.getUint16(20),
    };
  }
}
```

---

## 步骤 2: 后端支持（可选）

### 2.1 创建备份元数据记录

```erlang
%% src/repo/e2ee_local_backup_repo.erl

-module(e2ee_local_backup_repo).

%% API 函数
-export([create/1]).
-export([find_latest/1]).
-export([list_by_uid/1]).

%% @doc 创建备份元数据记录
-spec create(map()) -> {ok, map()} | {error, term()}.
create(BackupMap) ->
    Uid = maps:get(<<"uid">>, BackupMap),
    DeviceId = maps:get(<<"device_id">>, BackupMap),
    Checksum = maps:get(<<"checksum">>, BackupMap),

    Sql = <<"INSERT INTO e2ee_local_backups (uid, device_id, key_checksum)
             VALUES ($1, $2, $3)
             RETURNING *">>,

    case elib_pg:query(Sql, [Uid, DeviceId, Checksum]) of
        {ok, _, [{Result}]} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查找最新的备份记录
-spec find_latest(integer()) -> {ok, map()} | {error, not_found}.
find_latest(Uid) ->
    Sql = <<"SELECT * FROM e2ee_local_backups
             WHERE uid = $1
             ORDER BY created_at DESC
             LIMIT 1">>,

    case elib_pg:query(Sql, [Uid]) of
        {ok, _, [Result]} -> {ok, Result};
        {ok, _, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.
```

### 2.2 添加 Handler 端点（可选）

```erlang
%% src/api/e2ee_handler.erl

%% @doc 记录备份元数据（可选）
-spec record_backup_metadata(cowboy_req:req(), map()) -> cowboy_req:req().
record_backup_metadata(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    CurrentDeviceId = auth_ds:current_device_id(State),

    {ok, Body} = elib_req:body(Req0, []),
    Checksum = maps:get(<<"checksum">>, Body, <<>>),

    case byte_size(Checksum) > 0 of
        true ->
            BackupMap = #{
                <<"uid">> => CurrentUid,
                <<"device_id">> => CurrentDeviceId,
                <<"checksum">> => Checksum
            },
            case e2ee_local_backup_repo:create(BackupMap) of
                {ok, _} ->
                    elib_response:success(Req0, #{<<"status">> => <<"recorded">>});
                {error, Reason} ->
                    elib_response:error(Req0, error_msg(?ERR_INTERNAL_ERROR), ?ERR_INTERNAL_ERROR)
            end;
        false ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
    end.
```

---

## 步骤 3: UI 实现

### 3.1 备份导出页面

```dart
// lib/pages/settings/e2ee_backup_export_page.dart

class E2EEBackupExportPage extends StatefulWidget {
  @override
  _E2EEBackupExportPageState createState() => _E2EEBackupExportPageState();
}

class _E2EEBackupExportPageState extends State<E2EEBackupExportPage> {
  final _passwordController = TextEditingController();
  final _confirmPasswordController = TextEditingController();
  final _notesController = TextEditingController();
  bool _isExporting = false;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('导出 E2EE 备份'),
      ),
      body: ListView(
        padding: EdgeInsets.all(16),
        children: [
          // 警告提示
          Card(
            color: Colors.orange.shade50,
            child: Padding(
              padding: EdgeInsets.all(16),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Row(
                    children: [
                      Icon(Icons.warning, color: Colors.orange),
                      SizedBox(width: 8),
                      Text('重要提示', style: TextStyle(fontWeight: FontWeight.bold)),
                    ],
                  ),
                  SizedBox(height: 8),
                  Text('备份密码无法找回，请务必牢记！'),
                  Text('建议将备份文件存储到多个安全位置。'),
                ],
              ),
            ),
          ),

          SizedBox(height: 24),

          // 密码输入
          TextField(
            controller: _passwordController,
            decoration: InputDecoration(
              labelText: '备份密码',
              hintText: '至少 12 位，包含大小写字母、数字和特殊符号',
              prefixIcon: Icon(Icons.lock),
            ),
            obscureText: true,
          ),

          SizedBox(height: 16),

          // 确认密码
          TextField(
            controller: _confirmPasswordController,
            decoration: InputDecoration(
              labelText: '确认密码',
              prefixIcon: Icon(Icons.lock_outline),
            ),
            obscureText: true,
          ),

          SizedBox(height: 16),

          // 用户备注
          TextField(
            controller: _notesController,
            decoration: InputDecoration(
              labelText: '备注（可选）',
              hintText: '例如：主手机备份 - 2026年1月',
              prefixIcon: Icon(Icons.note),
            ),
            maxLines: 2,
          ),

          SizedBox(height: 24),

          // 密码强度指示器
          _buildPasswordStrengthIndicator(),

          SizedBox(height: 24),

          // 导出按钮
          ElevatedButton(
            onPressed: _isExporting ? null : _handleExport,
            child: _isExporting
                ? CircularProgressIndicator(color: Colors.white)
                : Text('导出备份'),
            style: ElevatedButton.styleFrom(
              minimumSize: Size(double.infinity, 48),
            ),
          ),

          SizedBox(height: 16),

          // 分享按钮
          OutlinedButton.icon(
            onPressed: () => _handleShare(context),
            icon: Icon(Icons.share),
            label: Text('通过邮件/云盘分享'),
            style: OutlinedButton.styleFrom(
              minimumSize: Size(double.infinity, 48),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildPasswordStrengthIndicator() {
    final password = _passwordController.text;
    final strength = _calculatePasswordStrength(password);

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text('密码强度', style: TextStyle(fontSize: 12)),
        SizedBox(height: 4),
        LinearProgressIndicator(
          value: strength,
          backgroundColor: Colors.grey.shade200,
          color: _getStrengthColor(strength),
        ),
        SizedBox(height: 4),
        Text(_getStrengthLabel(strength), style: TextStyle(fontSize: 12)),
      ],
    );
  }

  double _calculatePasswordStrength(String password) {
    if (password.isEmpty) return 0.0;

    double strength = 0.0;

    // 长度
    if (password.length >= 12) strength += 0.25;
    if (password.length >= 16) strength += 0.15;

    // 复杂度
    if (password.contains(RegExp(r'[a-z]'))) strength += 0.15;
    if (password.contains(RegExp(r'[A-Z]'))) strength += 0.15;
    if (password.contains(RegExp(r'[0-9]'))) strength += 0.15;
    if (password.contains(RegExp(r'[!@#$%^&*(),.?":{}|<>]'))) strength += 0.15;

    return strength.clamp(0.0, 1.0);
  }

  Color _getStrengthColor(double strength) {
    if (strength < 0.3) return Colors.red;
    if (strength < 0.6) return Colors.orange;
    if (strength < 0.8) return Colors.yellow;
    return Colors.green;
  }

  String _getStrengthLabel(double strength) {
    if (strength < 0.3) return '弱';
    if (strength < 0.6) return '中等';
    if (strength < 0.8) return '强';
    return '非常强';
  }

  Future<void> _handleExport() async {
    final password = _passwordController.text;
    final confirmPassword = _confirmPasswordController.text;

    // 验证密码
    if (password != confirmPassword) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('两次输入的密码不一致')),
      );
      return;
    }

    try {
      setState(() => _isExporting = true);

      // 导出备份
      final filePath = await E2EELocalBackupService.exportBackup(
        password: password,
        userNotes: _notesController.text,
      );

      setState(() => _isExporting = false);

      // 显示成功对话框
      showDialog(
        context: context,
        builder: (context) => AlertDialog(
          title: Text('备份导出成功'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text('备份文件已保存到：'),
              SizedBox(height: 8),
              Text(filePath, style: TextStyle(fontSize: 12)),
              SizedBox(height: 16),
              Text('请妥善保管此文件和密码。'),
            ],
          ),
          actions: [
            TextButton(
              onPressed: () => Navigator.pop(context),
              child: Text('知道了'),
            ),
          ],
        ),
      );
    } catch (e) {
      setState(() => _isExporting = false);
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('导出失败: $e')),
      );
    }
  }

  Future<void> _handleShare(BuildContext context) async {
    // 类似实现，先导出再分享
  }
}
```

### 3.2 备份导入页面

```dart
// lib/pages/settings/e2ee_backup_import_page.dart

class E2EEBackupImportPage extends StatefulWidget {
  final String? filePath;

  E2EEBackupImportPage({this.filePath});

  @override
  _E2EEBackupImportPageState createState() => _E2EEBackupImportPageState();
}

class _E2EEBackupImportPageState extends State<E2EEBackupImportPage> {
  final _passwordController = TextEditingController();
  bool _isImporting = false;
  Map<String, dynamic>? _backupInfo;

  @override
  void initState() {
    super.initState();
    if (widget.filePath != null) {
      _verifyBackup(widget.filePath!);
    }
  }

  Future<void> _verifyBackup(String filePath) async {
    try {
      final info = await E2EELocalBackupService.verifyBackup(filePath);
      setState(() => _backupInfo = info);
    } catch (e) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('备份文件验证失败: $e')),
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('导入 E2EE 备份'),
      ),
      body: ListView(
        padding: EdgeInsets.all(16),
        children: [
          if (_backupInfo != null) ...[
            Card(
              child: Padding(
                padding: EdgeInsets.all(16),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text('备份信息', style: TextStyle(fontWeight: FontWeight.bold)),
                    SizedBox(height: 8),
                    Text('版本: ${_backupInfo!['version']}'),
                    Text('文件大小: ${_backupInfo!['file_size']} bytes'),
                  ],
                ),
              ),
            ),
            SizedBox(height: 24),
          ],

          TextField(
            controller: _passwordController,
            decoration: InputDecoration(
              labelText: '备份密码',
              prefixIcon: Icon(Icons.lock),
            ),
            obscureText: true,
          ),

          SizedBox(height: 24),

          ElevatedButton(
            onPressed: _isImporting ? null : _handleImport,
            child: _isImporting
                ? CircularProgressIndicator(color: Colors.white)
                : Text('导入备份'),
            style: ElevatedButton.styleFrom(
              minimumSize: Size(double.infinity, 48),
            ),
          ),
        ],
      ),
    );
  }

  Future<void> _handleImport() async {
    // 类似导出，实现导入逻辑
  }
}
```

---

## 完成检查清单

- [ ] 前端备份服务完成
- [ ] PBKDF2 实现（310,000 次迭代）
- [ ] AES-256-GCM 加密/解密实现
- [ ] 备份文件格式实现
- [ ] 密码强度验证实现
- [ ] SHA-256 校验和实现
- [ ] 导出 UI 完成
- [ ] 导入 UI 完成
- [ ] 分享功能完成
- [ ] 单元测试通过
- [ ] 集成测试通过
- [ ] 文档更新完成

---

## 下一阶段

完成本阶段后，请继续执行：
- [阶段 5: 前端 UI](./phase-05-frontend-ui.md)

---

**最后更新**: 2026-01-30
**作者**: Claude AI Planning Agent
