# Imboy AI 驱动测试框架

> **版本**: v1.0
> **状态**: 设计阶段
> **最后更新**: 2026-02-04

---

  /orchestrate feature "添加用户认证功能"
  /orchestrate bugfix "修复聊天消息发送失败问题"
  /orchestrate refactor "重构消息存储层"
  /orchestrate security "审查E2EE加密实现"
  /orchestrate custom "planner,tdd-guide,code-reviewer" "优化WebSocket连接管理"

/everything-claude-code:orchestrate imboy 的前后端项目有没有比现在方案更好的能够“自动化的AI驱动的AI 分析并修复代码，意图解析器, 自愈合引擎, 知识库系统, 路径探索器, 模拟真人测试”测试框架

/everything-claude-code:orchestrate imboy 的前后端项目有没有比现在方案（/Users/leeyi/project/imboy.pub/imboyapp/integration_test/TESTING_GUIDE.md）更好的能够“自动化的AI驱动的AI 分析并修复代码，意图解析器, 自愈合引擎, 知识库系统, 路径探索器, 模拟真人测试”测试框架，期望全自动化和智能化


## 📚 文档导航

### 📖 核心文档

| 文档 | 说明 | 适合人群 |
|------|------|----------|
| **[整体方案设计](./ai_test_framework_design.md)** | 完整的技术架构和模块设计 | 技术人员、架构师 |
| **[落地执行计划](./ai_test_implementation_plan.md)** | 详细的实施步骤和时间计划 | 项目经理、开发团队 |

---

## 🎯 项目简介

Imboy AI 驱动测试框架是一个**智能化、自适应、可学习**的测试框架，通过 AI 技术实现：

- 🤖 **AI 自动生成测试** - 从用户故事自动生成测试用例
- 🔧 **自动愈合** - 测试失败后自动分析和修复
- 🧠 **知识库系统** - 从历史测试中学习和积累
- 🔍 **路径探索** - 自动发现边缘场景和隐藏缺陷
- 👤 **真人模拟** - 模拟真实用户行为，发现用户体验问题

---

## 📊 项目指标

| 指标 | 现状 | 目标 | 提升 |
|------|------|------|------|
| 测试编写效率 | 手动编写 | AI 自动生成 | **+300%** |
| 测试维护成本 | 人工修复 | 自动愈合 | **-70%** |
| 测试覆盖率 | 65% | 85%+ | **+20%** |
| Bug 发现率 | 基准 | AI 增强 | **+50%** |

---

## 🚀 快速开始

### 环境准备

```bash
# 1. 添加依赖
flutter pub add openai anthropic pinecone

# 2. 配置环境变量
# 编辑 .env.local_office 添加：
# OPENAI_API_KEY=sk-your-key
# PINECONE_API_KEY=your-key

# 3. 运行示例测试
flutter test integration_test/ai_generated_test.dart \
  --dart-define=APP_ENV=local_office \
  -d macos
```

### 快速验证

```dart
// integration_test/ai_quick_test.dart
import 'package:imboy/ai_test/intent/intent_parser.dart';

void main() {
  final parser = IntentParser();

  // 从用户故事生成测试
  final tests = await parser.parseFromUserStory('''
    作为用户，我想要发送消息给好友
  ''');

  print('生成了 ${tests.length} 个测试用例');
}
```

---

## 📅 实施进度

```
2026 年 2 月 - 6 月 (20 周)

阶段 0: 环境准备    ████████                        [第 1 周]
阶段 1: 意图解析    ████████████████                 [第 2-3 周]
阶段 2: 自愈合引擎  ██████████████████████           [第 4-5 周]
阶段 3: 知识库      ████████████████████████████     [第 6-8 周]
阶段 4: 路径探索    ██████████████████████████████   [第 9-11 周]
阶段 5: 真人模拟    ████████████████████████████████ [第 12-14 周]
阶段 6: 集成优化    ████████████████████████████████████████ [第 15-18 周]
阶段 7: 验收交付    ████████████████████             [第 19-20 周]
```

---

## 🏗️ 架构概览

```
┌─────────────────────────────────────────────────────────────────┐
│                    测试编排层                                   │
│  测试计划生成 | 智能调度 | 资源管理 | 报告生成                   │
└─────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────┐
│                    AI 智能引擎层                                 │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐           │
│  │意图解析  │ │自愈合引擎│ │知识库系统│ │路径探索器│           │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘           │
│  ┌────────────────────────────────────────────┐                 │
│  │           真人模拟引擎                      │                 │
│  └────────────────────────────────────────────┘                 │
└─────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────┐
│                    执行层                                       │
│  Flutter 测试引擎 | Erlang 后端测试 | 设备农场                   │
└─────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────┐
│                    数据层                                       │
│  向量数据库 | PostgreSQL | 知识图谱                             │
└─────────────────────────────────────────────────────────────────┘
```

---

## 📦 模块说明

### 1. AI 意图解析器
将用户需求转换为测试用例

```dart
final parser = IntentParser();
final tests = await parser.parseFromUserStory(userStory);
```

### 2. 自愈合引擎
自动修复失败的测试

```dart
final engine = SelfHealingEngine();
final result = await engine.heal(failure);
```

### 3. 知识库系统
存储和检索测试经验

```dart
final kb = KnowledgeBase();
await kb.storeExperience(experience);
final results = await kb.retrieveExperiences(query);
```

### 4. 路径探索器
自动发现测试场景

```dart
final explorer = PathExplorer();
final graph = await explorer.buildUIGraph();
final edgeCases = await explorer.discoverEdgeCases(graph);
```

### 5. 真人模拟引擎
模拟真实用户行为

```dart
final simulator = HumanSimulator();
final persona = await simulator.generatePersona('teenager');
await simulator.simulateUserSession(persona);
```

---

## 💰 成本估算

| 阶段 | 开发成本 | API 成本/月 | 基础设施 | 总投入 |
|------|---------|------------|---------|--------|
| 环境准备 | - | $0 | $0 | $0 |
| 意图解析 | ¥75,000 | $10-50 | $0 | ¥75-110k |
| 自愈合 | ¥100,000 | $20-100 | $0 | ¥100-170k |
| 知识库 | ¥150,000 | $20-100 | $0-70 | ¥150-270k |
| 完整系统 | ¥575,000 | $50-200 | $70 | ¥582-850k |

**省钱技巧：**
- 使用 GPT-4o-mini（便宜 10 倍）
- 使用本地 Ollama（免费）
- 缓存常见请求

---

## 🎯 成功标准

| 指标 | 基准值 | 目标值 | 验收标准 |
|------|--------|--------|----------|
| 测试生成效率 | 手动编写 | AI 自动生成 | 80% 用例可自动生成 |
| 自愈合成功率 | N/A | > 70% | 70% 失败可自动修复 |
| 测试覆盖率 | 65% | > 85% | 覆盖率提升 20% |
| Bug 发现率 | 基准 | +50% | 发现更多缺陷 |
| 测试维护成本 | 基准 | -50% | 维护时间减半 |

---

## 📞 联系方式

| 角色 | 邮箱 | 职责 |
|------|------|------|
| 项目负责人 | - | 整体协调 |
| 前端负责人 | - | Flutter 测试框架 |
| 后端负责人 | - | Erlang 测试框架 |
| AI 工程师 | - | 模型和提示词 |

---

## 📝 相关资源

### 技术文档
- [OpenAI API 文档](https://platform.openai.com/docs)
- [Claude API 文档](https://docs.anthropic.com)
- [Pinecone 文档](https://docs.pinecone.io)
- [Flutter 测试文档](https://docs.flutter.dev/testing)

### 项目文档
- [Imboy 架构文档](../architecture/overview.md)
- [Imboy 编码规范](../standards/)
- [Imboy API 文档](../api/rest-api.md)

---

## 🔄 更新日志

### v1.0 (2026-02-04)
- ✅ 初始版本
- ✅ 完成整体方案设计文档
- ✅ 完成落地执行计划文档

---

**最后更新**: 2026-02-04



prompt
 体验IM聊天APP
 代码解读
复制代码
作为一名顶级的移动产品战略分析师，请基于我提供的[市场数据文件]，完成以下任务：1.  **总结市场现状:** 目前市场的主要玩家、产品特点和用户群体分别是什么？2.  **挖掘用户痛点:** 分析所有用户评论，尤其是1-3星的差评，归纳出用户尚未被满足的核心需求，至少列出5个。3.  **寻找创新机会点:** 结合现有产品的空白和用户痛点，提出3个全新的、有差异化的App创意概念。4.  **功能与技术可行性评估:** 对每个创意，给出核心功能列表（MVP），并初步评估其技术实现难度。你的分析需要贯穿所有提供的资料，并引用关键信息来源。


我计划做一个IM APP （单聊+群聊+频道）功能，主要面向用户，中小企业，个体企业，小餐馆、小商贩，个人的频道展示，附近的服务，预约下单（信用预约），下单成绩（频道内容人工认证+AI认证自动失败内容健康），需要再深圳运营，需要符合中国大陆安全政策监管

/everything-claude-code:orchestrate 验证(自动开启APP验证)更新秘钥收费会通知好友重新获取秘钥（这通知获取的有效性，如果通讯消息丢失，还有无备选方案）

新增三种密钥恢复方法：
- 设备间传输 (e2ee_transfer): 安全地将私钥传输到新设备
- 社交恢复 (e2ee_social): 使用 Shamir Secret Sharing 分片恢复
- 本地备份 (e2ee_local_backup): 加密备份到本地存储

新增模块：
- Handler: e2ee_transfer_handler, e2ee_social_handler, cors_middleware
- Logic: e2ee_transfer_logic, e2ee_social_logic, login_security_logic
- DS: e2ee_transfer_ds, e2ee_social_ds, login_attempt_ds
- Repo: e2ee_transfer_repo, e2ee_social_repo, e2ee_local_backup_repo, msg_read_repo
- Lib: shamir_secret_sharing, elib_metric
