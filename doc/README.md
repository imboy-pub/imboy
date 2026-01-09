# Imboy 文档中心

> **版本**: 0.7.3
> **更新时间**: 2026-01-06

---

## 快速导航

### 📚 核心文档
- **[架构设计](architecture/)** - 系统架构与设计思想
- **[API 文档](api/)** - REST API 和 WebSocket API
- **[编码规范](standards/)** - 代码风格和规范约定

### 🚀 开发指南
- **[安装部署](guides/)** - 环境搭建与部署流程
- **[核心库](libraries/)** - 异步、缓存、重试等核心组件
- **[数据库](database/)** - PostgreSQL 和 TimescaleDB 使用

### 🔧 运维相关
- **[系统优化](operations/optimization.md)** - CentOS 和 Erlang 优化
- **[安全文档](operations/security.md)** - 安全策略和最佳实践
- **[依赖服务](operations/dependencies.md)** - 外部依赖说明

### 📋 参考资料
- **[常见问题](references/faq.md)** - FAQ
- **[变更日志](references/changelog.md)** - 版本更新记录
- **[应用升级](references/appup.md)** - Appup 升级指南

---

## 目录结构

```
doc/
├── README.md              # 本文件
├── architecture/          # 架构设计
│   ├── overview.md        # 系统概览 (DDD)
│   ├── design-thinking.md # 设计思考
│   ├── nomenclature.md    # 术语约定
│   └── database-access.md # 数据库访问层
│
├── api/                   # API 文档
│   ├── rest-api.md        # REST API
│   └── websocket-api.md   # WebSocket API
│
├── standards/             # 编码规范
│   ├── api-format.md      # API 格式规范
│   ├── error-codes.md     # 错误码规范
│   ├── hashid-encoding.md # HashID 编码
│   ├── utf8-encoding.md   # UTF-8 编码
│   └── type-specification.md # 类型规范
│
├── guides/                # 开发指南
│   ├── installation.md    # 安装指南
│   ├── deployment.md      # 部署流程
│   ├── kerl.md            # Erlang 版本管理
│   ├── debug-tools.md     # 调试工具
│   └── redbug.md          # Redbug 使用
│
├── libraries/             # 核心库文档
│   ├── async.md           # 异步执行
│   ├── retry.md           # 重试机制
│   ├── cache.md           # 缓存系统
│   ├── sync.md            # 进程同步
│   ├── message-ack.md     # 消息确认
│   └── pgsync.md          # PostgreSQL 同步
│
├── operations/            # 运维相关
│   ├── optimization.md    # 系统优化
│   ├── security.md        # 安全文档
│   └── dependencies.md    # 依赖服务
│
├── database/              # 数据库
│   ├── postgresql.md      # PostgreSQL 使用
│   ├── timescaledb.md     # TimescaleDB 使用
│   └── migrations/        # 数据库迁移
│
├── references/            # 参考资料
│   ├── appup.md           # 应用升级
│   ├── faq.md             # 常见问题
│   ├── changelog.md       # 变更日志
│   └── art.md             # 艺术相关
│
├── planning/              # 计划文档
│   ├── features.md        # 功能计划
│   └── testing.md         # 测试相关
│
├── ai/                    # AI 辅助
│   ├── README.md          # AI 辅助说明
│   └── prompts/           # AI 提示词库
│
└── modules/               # 模块索引
    └── README.md          # 源码模块说明
```

---

## 按角色查看

### 👨‍💻 后端开发
1. 阅读 [架构设计](architecture/)
2. 了解 [编码规范](standards/)
3. 学习 [核心库](libraries/) 使用
4. 查阅 [API 文档](api/)

### 🚀 运维人员
1. 参考 [安装部署](guides/)
2. 配置 [系统优化](operations/optimization.md)
3. 查看 [安全文档](operations/security.md)
4. 了解 [依赖服务](operations/dependencies.md)

### 🤖 AI 辅助开发
- [AI 提示词库](ai/prompts/) - 辅助 AI 理解代码的提示词

---

## 贡献指南

### 新增文档

1. 确定文档所属分类
2. 在对应目录创建文件
3. 使用连字符 `-` 命名（小写）
4. 更新本索引

### 文档规范

- **命名**: 全部小写，使用连字符分隔
- **格式**: Markdown
- **标题**: 使用 `#` 表示文档标题
- **代码块**: 指定语言（如 ```erlang）

---

## 相关链接

- **源码**: [src/](../src/)
- **测试**: [test/](../test/)
- **配置**: [config/](../config/)
- **主文档**: [CLAUDE.md](../CLAUDE.md)

---

**最后更新**: 2026-01-06
