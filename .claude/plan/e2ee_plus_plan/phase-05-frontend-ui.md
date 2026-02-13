# 阶段 5: 前端 UI 集成

> **预计工期**: 5-7 天
> **依赖**: 阶段 2, 3, 4
> **交付物**: 完整的用户界面和用户体验

---

## 目标

实现统一的 E2EE 密钥恢复入口界面：
1. 密钥恢复主页（三种方法入口）
2. 设置菜单集成
3. 引导流程和帮助文档
4. 完整的用户体验优化

---

## UI 结构

```
设置页面
    │
    └── [账号与安全]
           │
           └── [E2EE 密钥管理] ← 新入口
                  │
                  ├── E2EE 密钥恢复主页
                  │     │
                  │     ├── ┌─────────────┐
                  │     │   │ 设备间传输  │ ← (推荐)
                  │     │   │ ⭐⭐⭐⭐⭐   │
                  │     │   │ 最安全      │
                  │     │   └─────────────┘
                  │     │
                  │     ├── ┌─────────────┐
                  │     │   │ 社交恢复    │ ← (需提前准备)
                  │     │   │ ⭐⭐⭐⭐     │
                  │     │   │ 依赖好友    │
                  │     │   └─────────────┘
                  │     │
                  │     └── ┌─────────────┐
                  │         │ 本地备份    │ ← (最灵活)
                  │         │ ⭐⭐⭐⭐⭐   │
                  │         │ 用户负责    │
                  │         └─────────────┘
                  │
                  └── [当前密钥信息]
                        ├── 设备 ID
                        ├── 密钥 ID
                        ├── 创建时间
                        └── [导出公钥]
```

---

## 步骤 1: 主恢复页面

### 1.1 创建主页

```dart
// lib/pages/settings/e2ee_key_recovery_page.dart

import 'package:flutter/material.dart';
import 'package:imboy/pages/settings/e2ee_device_transfer_page.dart';
import 'package:imboy/pages/settings/e2ee_social_recovery_page.dart';
import 'package:imboy/pages/settings/e2ee_local_backup_page.dart';
import 'package:imboy/service/storage_service.dart';

class E2EEKeyRecoveryPage extends StatelessWidget {
  const E2EEKeyRecoveryPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('E2EE 密钥恢复'),
        actions: [
          IconButton(
            icon: Icon(Icons.help_outline),
            onPressed: () => _showHelp(context),
          ),
        ],
      ),
      body: ListView(
        padding: EdgeInsets.all(16),
        children: [
          // 警告提示
          _buildWarningCard(context),

          SizedBox(height: 24),

          // 当前密钥信息
          _buildKeyInfoCard(context),

          SizedBox(height: 24),

          // 恢复方法标题
          Text(
            '选择恢复方法',
            style: TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
            ),
          ),

          SizedBox(height: 16),

          // 设备间传输（推荐）
          _buildMethodCard(
            context,
            title: '设备间传输',
            description: '从旧设备直接传输密钥到新设备',
            securityLevel: 5,
            icon: Icons.devices,
            color: Colors.green,
            isRecommended: true,
            onTap: () => _navigateToDeviceTransfer(context),
          ),

          SizedBox(height: 16),

          // 社交恢复
          _buildMethodCard(
            context,
            title: '社交恢复',
            description: '通过可信好友的帮助恢复密钥',
            securityLevel: 4,
            icon: Icons.people,
            color: Colors.blue,
            requirements: ['需要至少 3 个可信好友', '需要提前设置'],
            onTap: () => _navigateToSocialRecovery(context),
          ),

          SizedBox(height: 16),

          // 本地备份
          _buildMethodCard(
            context,
            title: '本地备份',
            description: '从加密备份文件恢复密钥',
            securityLevel: 5,
            icon: Icons.backup,
            color: Colors.purple,
            requirements: ['需要提前创建备份', '需要记住备份密码'],
            onTap: () => _navigateToLocalBackup(context),
          ),

          SizedBox(height: 24),

          // 重要提示
          _buildImportantNotice(context),
        ],
      ),
    );
  }

  Widget _buildWarningCard(BuildContext context) {
    return Card(
      color: Colors.orange.shade50,
      child: Padding(
        padding: EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                Icon(Icons.warning_amber_rounded, color: Colors.orange),
                SizedBox(width: 8),
                Text(
                  '重要说明',
                  style: TextStyle(
                    fontWeight: FontWeight.bold,
                    color: Colors.orange.shade900,
                  ),
                ),
              ],
            ),
            SizedBox(height: 8),
            Text(
              '真正的端到端加密意味着：换设备后，旧消息无法访问是预期行为。'
              '如果您需要访问旧消息，必须恢复旧设备的私钥。',
              style: TextStyle(fontSize: 13, color: Colors.orange.shade900),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildKeyInfoCard(BuildContext context) {
    return FutureBuilder<Map<String, dynamic>>(
      future: _getKeyInfo(),
      builder: (context, snapshot) {
        if (!snapshot.hasData) {
          return Center(child: CircularProgressIndicator());
        }

        final info = snapshot.data!;
        return Card(
          child: Padding(
            padding: EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Text(
                      '当前密钥',
                      style: TextStyle(
                        fontWeight: FontWeight.bold,
                        fontSize: 16,
                      ),
                    ),
                    Icon(Icons.check_circle, color: Colors.green),
                  ],
                ),
                SizedBox(height: 12),
                _buildInfoRow('设备 ID', info['device_id']),
                _buildInfoRow('密钥 ID', info['key_id']),
                _buildInfoRow('创建时间', info['created_at']),
                SizedBox(height: 12),
                OutlinedButton.icon(
                  onPressed: () => _exportPublicKey(context),
                  icon: Icon(Icons.share),
                  label: Text('导出公钥'),
                ),
              ],
            ),
          ),
        );
      },
    );
  }

  Widget _buildInfoRow(String label, String? value) {
    return Padding(
      padding: EdgeInsets.only(bottom: 8),
      child: Row(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          SizedBox(
            width: 80,
            child: Text(
              label,
              style: TextStyle(color: Colors.grey.shade600),
            ),
          ),
          Expanded(
            child: Text(
              value ?? '未知',
              style: TextStyle(fontFamily: 'monospace'),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildMethodCard(
    BuildContext context, {
    required String title,
    required String description,
    required int securityLevel,
    required IconData icon,
    required Color color,
    bool isRecommended = false,
    List<String>? requirements,
    required VoidCallback onTap,
  }) {
    return Card(
      elevation: isRecommended ? 4 : 1,
      child: InkWell(
        onTap: onTap,
        borderRadius: BorderRadius.circular(8),
        child: Padding(
          padding: EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Row(
                children: [
                  Container(
                    padding: EdgeInsets.all(8),
                    decoration: BoxDecoration(
                      color: color.withOpacity(0.1),
                      borderRadius: BorderRadius.circular(8),
                    ),
                    child: Icon(icon, color: color, size: 24),
                  ),
                  SizedBox(width: 12),
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Row(
                          children: [
                            Text(
                              title,
                              style: TextStyle(
                                fontWeight: FontWeight.bold,
                                fontSize: 16,
                              ),
                            ),
                            if (isRecommended) ...[
                              SizedBox(width: 8),
                              Container(
                                padding: EdgeInsets.symmetric(
                                  horizontal: 8,
                                  vertical: 2,
                                ),
                                decoration: BoxDecoration(
                                  color: Colors.green,
                                  borderRadius: BorderRadius.circular(12),
                                ),
                                child: Text(
                                  '推荐',
                                  style: TextStyle(
                                    color: Colors.white,
                                    fontSize: 10,
                                    fontWeight: FontWeight.bold,
                                  ),
                                ),
                              ),
                            ],
                          ],
                        ),
                        SizedBox(height: 4),
                        Text(
                          description,
                          style: TextStyle(
                            fontSize: 13,
                            color: Colors.grey.shade600,
                          ),
                        ),
                      ],
                    ),
                  ),
                ],
              ),
              SizedBox(height: 12),
              Row(
                children: [
                  Text('安全等级：', style: TextStyle(fontSize: 12)),
                  ...List.generate(5, (index) {
                    return Icon(
                      index < securityLevel ? Icons.star : Icons.star_border,
                      size: 16,
                      color: Colors.amber,
                    );
                  }),
                ],
              ),
              if (requirements != null && requirements.isNotEmpty) ...[
                SizedBox(height: 8),
                ...requirements.map((req) => Padding(
                      padding: EdgeInsets.only(top: 4),
                      child: Row(
                        children: [
                          Icon(
                            Icons.info_outline,
                            size: 14,
                            color: Colors.blue,
                          ),
                          SizedBox(width: 4),
                          Expanded(
                            child: Text(
                              req,
                              style: TextStyle(fontSize: 12),
                            ),
                          ),
                        ],
                      ),
                    )),
              ],
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildImportantNotice(BuildContext context) {
    return Card(
      color: Colors.blue.shade50,
      child: Padding(
        padding: EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                Icon(Icons.info_outline, color: Colors.blue, size: 20),
                SizedBox(width: 8),
                Text(
                  '安全建议',
                  style: TextStyle(
                    fontWeight: FontWeight.bold,
                    color: Colors.blue.shade900,
                  ),
                ),
              ],
            ),
            SizedBox(height: 8),
            Text(
              '• 建议定期创建本地备份（每月一次）\n'
              '• 换设备前先使用设备间传输\n'
              '• 社交恢复需要提前设置可信好友\n'
              '• 所有恢复方法都保证服务器无法访问私钥',
              style: TextStyle(
                fontSize: 13,
                color: Colors.blue.shade900,
                height: 1.5,
              ),
            ),
          ],
        ),
      ),
    );
  }

  Future<Map<String, dynamic>> _getKeyInfo() async {
    final deviceId = await StorageService.getDeviceId();
    final keyId = await StorageService.getE2EEKeyId();
    final createdAt = await StorageService.getE2EEKeyCreatedAt();

    return {
      'device_id': deviceId ?? '未知',
      'key_id': keyId ?? '未知',
      'created_at': createdAt ?? '未知',
    };
  }

  void _showHelp(BuildContext context) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('关于 E2EE 密钥恢复'),
        content: SingleChildScrollView(
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            mainAxisSize: MainAxisSize.min,
            children: [
              _buildHelpSection(
                '为什么需要密钥恢复？',
                '当您更换设备时，新设备会生成新的密钥对。'
                '如果您想查看旧消息，需要恢复旧设备的私钥。',
              ),
              SizedBox(height: 16),
              _buildHelpSection(
                '哪种方法最安全？',
                '• 设备间传输：最安全，直接传输，服务器不接触私钥\n'
                '• 本地备份：最灵活，但需要您负责保管备份文件\n'
                '• 社交恢复：相对安全，需要提前设置可信好友',
              ),
              SizedBox(height: 16),
              _buildHelpSection(
                '服务器能看到我的私钥吗？',
                '绝对不能！所有恢复方法都确保服务器永远无法访问您的私钥。'
                '服务器只负责转发加密数据。',
              ),
            ],
          ),
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('知道了'),
          ),
        ],
      ),
    );
  }

  Widget _buildHelpSection(String title, String content) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          title,
          style: TextStyle(fontWeight: FontWeight.bold),
        ),
        SizedBox(height: 4),
        Text(
          content,
          style: TextStyle(fontSize: 13, color: Colors.grey.shade700),
        ),
      ],
    );
  }

  void _exportPublicKey(BuildContext context) {
    // 导出公钥功能
  }

  void _navigateToDeviceTransfer(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(builder: (context) => E2EEDeviceTransferPage()),
    );
  }

  void _navigateToSocialRecovery(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(builder: (context) => E2EESocialRecoveryPage()),
    );
  }

  void _navigateToLocalBackup(BuildContext context) {
    Navigator.push(
      context,
      MaterialPageRoute(builder: (context) => E2EELocalBackupPage()),
    );
  }
}
```

---

## 步骤 2: 设置菜单集成

### 2.1 修改设置页面

```dart
// lib/pages/settings/settings_page.dart

class SettingsPage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('设置')),
      body: ListView(
        children: [
          // ... 现有设置项

          // 新增：E2EE 密钥管理入口
          _buildSecuritySection(context),

          // ... 其他设置项
        ],
      ),
    );
  }

  Widget _buildSecuritySection(BuildContext context) {
    return ListTileGroup(
      title: '账号与安全',
      children: [
        ListTile(
          leading: Icon(Icons.lock, color: Colors.green),
          title: Text('E2EE 密钥管理'),
          subtitle: Text('管理端到端加密密钥'),
          trailing: Icon(Icons.chevron_right),
          onTap: () {
            Navigator.push(
              context,
              MaterialPageRoute(
                builder: (context) => E2EEKeyRecoveryPage(),
              ),
            );
          },
        ),
        // ... 其他安全相关设置
      ],
    );
  }
}
```

---

## 步骤 3: 首次引导流程

### 3.1 创建引导页面

```dart
// lib/pages/onboarding/e2ee_onboarding_page.dart

class E2EEOnboardingPage extends StatefulWidget {
  const E2EEOnboardingPage({Key? key}) : super(key: key);

  @override
  _E2EEOnboardingPageState createState() => _E2EEOnboardingPageState();
}

class _E2EEOnboardingPageState extends State<E2EEOnboardingPage> {
  final PageController _pageController = PageController();
  int _currentPage = 0;

  final List<OnboardingStep> _steps = [
    OnboardingStep(
      title: '欢迎使用端到端加密',
      description: '您的消息受到端到端加密保护，只有您和您的聊天对象能查看消息内容。',
      icon: Icons.security,
      color: Colors.green,
    ),
    OnboardingStep(
      title: '密钥安全很重要',
      description: '您的加密密钥存储在当前设备。如果您更换设备，旧消息将无法访问。',
      icon: Icons.vpn_key,
      color: Colors.orange,
    ),
    OnboardingStep(
      title: '设置密钥恢复',
      description: '为了避免丢失旧消息，建议您现在设置密钥恢复方式。',
      icon: Icons.backup,
      color: Colors.blue,
    ),
    OnboardingStep(
      title: '选择恢复方式',
      description: '• 设备间传输（推荐）\n• 社交恢复\n• 本地备份\n\n您也可以稍后在设置中配置。',
      icon: Icons.devices,
      color: Colors.purple,
    ),
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        child: Column(
          children: [
            Expanded(
              child: PageView.builder(
                controller: _pageController,
                onPageChanged: (index) {
                  setState(() => _currentPage = index);
                },
                itemCount: _steps.length,
                itemBuilder: (context, index) {
                  return _buildStep(_steps[index]);
                },
              ),
            ),
            _buildBottomBar(context),
          ],
        ),
      ),
    );
  }

  Widget _buildStep(OnboardingStep step) {
    return Padding(
      padding: EdgeInsets.all(32),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Container(
            padding: EdgeInsets.all(24),
            decoration: BoxDecoration(
              color: step.color.withOpacity(0.1),
              shape: BoxShape.circle,
            ),
            child: Icon(step.icon, size: 80, color: step.color),
          ),
          SizedBox(height: 32),
          Text(
            step.title,
            style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
            textAlign: TextAlign.center,
          ),
          SizedBox(height: 16),
          Text(
            step.description,
            style: TextStyle(fontSize: 16, color: Colors.grey.shade700),
            textAlign: TextAlign.center,
          ),
        ],
      ),
    );
  }

  Widget _buildBottomBar(BuildContext context) {
    return Padding(
      padding: EdgeInsets.all(24),
      child: Column(
        children: [
          // 页面指示器
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: List.generate(_steps.length, (index) {
              return AnimatedContainer(
                duration: Duration(milliseconds: 300),
                margin: EdgeInsets.symmetric(horizontal: 4),
                height: 8,
                width: _currentPage == index ? 24 : 8,
                decoration: BoxDecoration(
                  color: _currentPage == index
                      ? Colors.green
                      : Colors.grey.shade300,
                  borderRadius: BorderRadius.circular(4),
                ),
              );
            }),
          ),
          SizedBox(height: 24),
          // 操作按钮
          Row(
            children: [
              if (_currentPage > 0)
                Expanded(
                  child: OutlinedButton(
                    onPressed: _previousPage,
                    child: Text('上一步'),
                  ),
                ),
              if (_currentPage > 0) SizedBox(width: 16),
              Expanded(
                child: ElevatedButton(
                  onPressed: _currentPage < _steps.length - 1
                      ? _nextPage
                      : _finishOnboarding,
                  child: Text(_currentPage < _steps.length - 1
                      ? '下一步'
                      : '完成'),
                ),
              ),
            ],
          ),
        ],
      ),
    );
  }

  void _nextPage() {
    _pageController.nextPage(
      duration: Duration(milliseconds: 300),
      curve: Curves.easeInOut,
    );
  }

  void _previousPage() {
    _pageController.previousPage(
      duration: Duration(milliseconds: 300),
      curve: Curves.easeInOut,
    );
  }

  void _finishOnboarding() {
    // 标记引导已完成
    StorageService.setE2EEOnboardingCompleted(true);

    // 跳转到密钥恢复页面
    Navigator.of(context).pushAndRemoveUntil(
      MaterialPageRoute(builder: (context) => E2EEKeyRecoveryPage()),
      (route) => false,
    );
  }
}

class OnboardingStep {
  final String title;
  final String description;
  final IconData icon;
  final Color color;

  OnboardingStep({
    required this.title,
    required this.description,
    required this.icon,
    required this.color,
  });
}
```

---

## 步骤 4: 用户体验优化

### 4.1 添加加载状态

```dart
// lib/widgets/e2ee_loading_widget.dart

class E2EELoadingWidget extends StatelessWidget {
  final String message;
  final double progress;

  const E2EELoadingWidget({
    required this.message,
    this.progress = 0,
  });

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Card(
        child: Padding(
          padding: EdgeInsets.all(24),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              CircularProgressIndicator(value: progress > 0 ? progress : null),
              SizedBox(height: 16),
              Text(message),
              if (progress > 0) ...[
                SizedBox(height: 8),
                Text('${(progress * 100).toInt()}%', style: TextStyle(fontSize: 12)),
              ],
            ],
          ),
        ),
      ),
    );
  }
}
```

### 4.2 添加成功/失败反馈

```dart
// lib/widgets/e2ee_result_widget.dart

class E2EEResultWidget extends StatelessWidget {
  final bool success;
  final String title;
  final String message;
  final VoidCallback? onAction;
  final String? actionLabel;

  const E2EEResultWidget({
    required this.success,
    required this.title,
    required this.message,
    this.onAction,
    this.actionLabel,
  });

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Card(
        color: success ? Colors.green.shade50 : Colors.red.shade50,
        child: Padding(
          padding: EdgeInsets.all(24),
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              Icon(
                success ? Icons.check_circle : Icons.error,
                size: 64,
                color: success ? Colors.green : Colors.red,
              ),
              SizedBox(height: 16),
              Text(
                title,
                style: TextStyle(
                  fontSize: 18,
                  fontWeight: FontWeight.bold,
                ),
              ),
              SizedBox(height: 8),
              Text(
                message,
                style: TextStyle(fontSize: 14),
                textAlign: TextAlign.center,
              ),
              if (onAction != null) ...[
                SizedBox(height: 16),
                ElevatedButton(
                  onPressed: onAction,
                  child: Text(actionLabel ?? '确定'),
                ),
              ],
            ],
          ),
        ),
      ),
    );
  }
}
```

---

## 步骤 5: 测试清单

### UI 测试

- [ ] 主恢复页面布局正确
- [ ] 所有卡片可点击
- [ ] 导航流程正确
- [ ] 设置菜单入口显示
- [ ] 引导流程完整
- [ ] 加载状态正确显示
- [ ] 成功/失败反馈正确

### 用户体验测试

- [ ] 首次用户引导清晰
- [ ] 错误提示友好
- [ ] 操作有确认步骤
- [ ] 敏感操作有二次确认
- [ ] 所有按钮有反馈
- [ ] 长时间操作有进度提示

### 兼容性测试

- [ ] Android 测试通过
- [ ] iOS 测试通过
- [ ] 不同屏幕尺寸适配
- [ ] 深色模式适配
- [ ] 横屏模式适配

---

## 完成检查清单

- [ ] 主恢复页面完成
- [ ] 设置菜单集成完成
- [ ] 首次引导流程完成
- [ ] 设备传输页面集成
- [ ] 社交恢复页面集成
- [ ] 本地备份页面集成
- [ ] 用户体验优化完成
- [ ] 加载状态组件完成
- [ ] 结果反馈组件完成
- [ ] UI 测试通过
- [ ] 用户体验测试通过
- [ ] 兼容性测试通过
- [ ] 文档更新完成
- [ ] 截图和录屏准备

---

## 项目完成检查清单

所有阶段完成后：

- [ ] 所有代码已提交
- [ ] 所有测试通过
- [ ] 代码审查完成
- [ ] 安全审查完成
- [ ] 文档更新完成
- [ ] 用户手册完成
- [ ] 发布说明准备
- [ ] 回滚方案准备

---

**恭喜！** 🎉

您已经完成了 E2EE 密钥恢复方案 D 的全部实施！

---

**最后更新**: 2026-01-30
**作者**: Claude AI Planning Agent
