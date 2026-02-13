# E2EE 密钥恢复方案 D 实施计划

> **方案类型**: 混合方案（无服务器存储私钥）
> **创建时间**: 2026-01-30
> **预计工期**: 4 周
> **安全等级**: ⭐⭐⭐⭐⭐

---

## 方案概述

本方案解决用户换设备后 E2EE 消息无法访问的问题，同时**不破坏端到端加密的安全原则**：

### 核心原则

1. **服务器永不存储或解密私钥** - 保持真正的 E2EE
2. **用户完全控制自己的密钥** - 不依赖服务器信任
3. **提供多种恢复方式** - 适应不同场景
4. **旧消息不可访问是预期行为** - 这是 E2EE 的特性，不是 bug

### 三种恢复方法

| 方法 | 安全等级 | 前置条件 | 适用场景 |
|------|---------|---------|---------|
| **A. 设备间传输** | ⭐⭐⭐⭐⭐ | 需要旧设备 | 换手机前准备，最安全 |
| **B. 社交恢复** | ⭐⭐⭐⭐ | 3 个可信好友 | 旧设备不可用时 |
| **C. 本地备份** | ⭐⭐⭐⭐⭐ | 提前备份 | 定期备份，用户负责 |

---

## 目录结构

```
.claude/plan/e2ee_plus_plan/
├── README.md                           # 本文件 - 项目概述
├── phase-01-preparation.md             # 阶段1: 准备工作（数据库迁移）
├── phase-02-device-transfer.md         # 阶段2: 设备间传输
├── phase-03-social-recovery.md         # 阶段3: 社交恢复
├── phase-04-local-backup.md            # 阶段4: 本地备份
├── phase-05-frontend-ui.md             # 阶段5: 前端 UI
├── checklist.md                        # 执行检查清单
└── architecture-diagram.md             # 架构图（可选）
```

---

## 快速开始

### 前置要求

#### 后端环境
- Erlang/OTP 28+
- PostgreSQL 18+
- 已有 Imboy 项目运行环境

#### 前端环境
- Flutter 3.x
- Dart 3.x
- Flutter 项目位于 `/Users/leeyi/project/imboy.pub/imboyapp`

### 执行顺序

**严格按顺序执行各阶段**：

```bash
# 1. 阅读本文件
cat .claude/e2ee_plus_plan/README.md

# 2. 执行阶段 1: 准备工作
# 查看 phase-01-preparation.md

# 3. 执行阶段 2: 设备间传输
# 查看 phase-02-device-transfer.md

# 4. 执行阶段 3: 社交恢复
# 查看 phase-03-social-recovery.md

# 5. 执行阶段 4: 本地备份
# 查看 phase-04-local-backup.md

# 6. 执行阶段 5: 前端 UI
# 查看 phase-05-frontend-ui.md

# 7. 检查完成情况
# 查看 checklist.md
```

---

## 各阶段概览

### 阶段 1: 准备工作 (1-2 天)

**目标**: 完成数据库迁移和错误码定义

**交付物**:
- [ ] 4 个新数据表
- [ ] 10 个新错误码
- [ ] 测试环境验证

**详细内容**: 查看 [phase-01-preparation.md](./phase-01-preparation.md)

---

### 阶段 2: 设备间传输 (5-7 天)

**目标**: 实现设备间直接传输私钥（最安全的方式）

**交付物**:
- [ ] 后端: 4 个新模块（logic, repo, ds, handler）
- [ ] 前端: 2 个新服务（service, api）
- [ ] 二维码生成和扫描
- [ ] 传输会话管理

**详细内容**: 查看 [phase-02-device-transfer.md](./phase-02-device-transfer.md)

**核心流程**:
```
旧设备 → 生成传输会话 → 显示二维码
新设备 → 扫描二维码 → 验证 → 接收私钥
服务器 → 仅转发加密数据 → 不解密
```

---

### 阶段 3: 社交恢复 (7-10 天)

**目标**: 实现通过可信好友恢复密钥

**交付物**:
- [ ] 后端: 3 个新模块（logic, repo, ds, handler）
- [ ] 前端: 2 个新服务
- [ ] Shamir 秘密共享实现
- [ ] 好友管理 UI

**详细内容**: 查看 [phase-03-social-recovery.md](./phase-03-social-recovery.md)

**核心流程**:
```
用户 → 选择 3 个好友 → 使用 Shamir 分割私钥
好友 → 各保存 1/3 密钥分片 → 服务器仅转发
恢复 → 联系 3 个好友 → 重组密钥分片
```

---

### 阶段 4: 本地备份 (3-5 天)

**目标**: 实现本地备份文件导出/导入

**交付物**:
- [ ] 前端: 1 个备份服务
- [ ] 加密备份文件格式
- [ ] 文件管理 UI

**详细内容**: 查看 [phase-04-local-backup.md](./phase-04-local-backup.md)

**核心流程**:
```
用户 → 设置备份密码 → 导出加密备份文件
恢复 → 导入备份文件 → 输入密码 → 解密私钥
注意: 备份文件完全本地管理，服务器不存储
```

---

### 阶段 5: 前端 UI (5-7 天)

**目标**: 实现统一的密钥恢复入口界面

**交付物**:
- [ ] 密钥恢复主页
- [ ] 三种方法的入口卡片
- [ ] 设置菜单入口
- [ ] 引导流程

**详细内容**: 查看 [phase-05-frontend-ui.md](./phase-05-frontend-ui.md)

---

## 安全设计

### 零信任架构

```
┌─────────────────────────────────────────────────────────┐
│                     用户设备                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │   私钥存储   │  │   加密解密   │  │   密钥恢复   │     │
│  │  (安全存储)  │  │  (本地执行)  │  │  (用户控制)  │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
└─────────────────────────────────────────────────────────┘
                    ↓ 仅转发加密数据
┌─────────────────────────────────────────────────────────┐
│                   Imboy 服务器                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │   路由转发   │  │   会话管理   │  │   好友关系   │     │
│  │ (不解密数据) │  │ (仅元数据)  │  │  (仅关系)   │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
│                                                         │
│  ⚠️ 永不存储私钥 | 永不解密数据 | 用户完全控制          │
└─────────────────────────────────────────────────────────┘
```

### 加密规范

| 组件 | 算法 | 密钥长度 | 用途 |
|------|------|---------|------|
| **消息加密** | AES-256-GCM | 256 位 | 消息内容 |
| **密钥交换** | RSA-2048-PKCS8 | 2048 位 | 传输加密 |
| **密钥派生** | PBKDF2-HMAC-SHA256 | 310,000 次迭代 | 备份密码 |
| **秘密共享** | Shamir's Secret Sharing | (t, n) = (2, 3) | 社交恢复 |

---

## 数据库变更

### 新增表结构

```sql
-- 1. 设备传输会话表
CREATE TABLE e2ee_transfer_sessions (
    id BIGSERIAL PRIMARY KEY,
    session_id VARCHAR(48) NOT NULL UNIQUE,
    from_uid INTEGER NOT NULL,
    from_device_id VARCHAR(64) NOT NULL,
    to_uid INTEGER NOT NULL,
    to_device_id VARCHAR(64),
    status VARCHAR(20) NOT NULL DEFAULT 'pending',
    encrypted_key_bundle TEXT NOT NULL,
    expires_at TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 2. 可信联系人表
CREATE TABLE e2ee_trusted_contacts (
    id BIGSERIAL PRIMARY KEY,
    uid INTEGER NOT NULL,
    contact_uid INTEGER NOT NULL,
    contact_nickname VARCHAR(100),
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(uid, contact_uid)
);

-- 3. 密钥分片表
CREATE TABLE e2ee_key_shares (
    id BIGSERIAL PRIMARY KEY,
    owner_uid INTEGER NOT NULL,
    trustee_uid INTEGER NOT NULL,
    encrypted_share TEXT NOT NULL,
    share_index INTEGER NOT NULL,
    threshold INTEGER NOT NULL DEFAULT 2,
    total_shares INTEGER NOT NULL DEFAULT 3,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(owner_uid, trustee_uid)
);

-- 4. 本地备份元数据表
CREATE TABLE e2ee_local_backups (
    id BIGSERIAL PRIMARY KEY,
    uid INTEGER NOT NULL,
    device_id VARCHAR(64) NOT NULL,
    backup_version INTEGER NOT NULL,
    key_checksum VARCHAR(64) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

---

## API 端点

### 设备传输相关

| 端点 | 方法 | 说明 |
|------|------|------|
| `/v1/e2ee/transfer/create` | POST | 创建传输会话 |
| `/v1/e2ee/transfer/accept` | POST | 接受传输 |
| `/v1/e2ee/transfer/confirm` | POST | 确认完成 |
| `/v1/e2ee/transfer/status` | GET | 查询状态 |

### 社交恢复相关

| 端点 | 方法 | 说明 |
|------|------|------|
| `/v1/e2ee/social/contacts` | GET | 列出可信联系人 |
| `/v1/e2ee/social/contacts/add` | POST | 添加联系人 |
| `/v1/e2ee/social/contacts/remove` | POST | 移除联系人 |
| `/v1/e2ee/social/shares/create` | POST | 创建密钥分片 |
| `/v1/e2ee/social/shares/retrieve` | GET | 获取分片 |
| `/v1/e2ee/social/recover` | POST | 重组密钥 |

---

## 测试策略

### 单元测试

- [ ] 后端: 每个新模块 ≥80% 覆盖率
- [ ] 前端: 每个新服务 ≥70% 覆盖率

### 集成测试

- [ ] 设备传输完整流程
- [ ] 社交恢复完整流程
- [ ] 本地备份完整流程

### 安全测试

- [ ] 渗透测试（确保服务器无法解密）
- [ ] 中间人攻击测试
- [ ] 重放攻击防护

---

## 风险评估

| 风险 | 影响 | 缓解措施 |
|------|------|---------|
| 用户丢失所有恢复方式 | 高 | 提供多种方法，强烈建议用户提前准备 |
| 社交恢复好友串通 | 中 | 需 3 个好友，至少 2 个串通才能恢复 |
| 备份文件密码遗忘 | 高 | 明确提示用户，密码无法找回 |
| 二维码被截获 | 低 | 二维码短期有效（5 分钟），加密传输 |

---

## 时间估算

| 阶段 | 工作量 | 依赖 |
|------|--------|------|
| 阶段 1: 准备 | 1-2 天 | 无 |
| 阶段 2: 设备传输 | 5-7 天 | 阶段 1 |
| 阶段 3: 社交恢复 | 7-10 天 | 阶段 1 |
| 阶段 4: 本地备份 | 3-5 天 | 阶段 1 |
| 阶段 5: 前端 UI | 5-7 天 | 阶段 2,3,4 |
| **总计** | **21-31 天** | - |

---

## 执行检查清单

完整的执行检查清单请查看: [checklist.md](./checklist.md)

---

## 常见问题

### Q: 为什么不让服务器存储加密的私钥？

A: 因为用户明确表示"对用户来说不可信，不够绝对安全"。真正的 E2EE 要求用户完全控制密钥，服务器只做数据转发。

### Q: 旧消息无法访问怎么办？

A: 这是 E2EE 的**预期行为**，不是 bug。如果需要访问旧消息，必须恢复旧设备的私钥。本方案提供了三种恢复方式。

### Q: 社交恢复安全吗？

A: 相对安全。使用 Shamir 秘密共享，需要 3 个好友中的至少 2 个才能恢复。建议选择真正可信的好友。

### Q: 本地备份文件安全吗？

A: 最安全，但需要用户负责。备份文件使用 PBKDF2 + AES-256-GCM 加密，密码必须由用户记住。如果密码丢失，备份无法恢复。

---

## 后续优化

1. **硬件密钥支持**: 支持 YubiKey 等硬件安全模块
2. **生物识别**: 结合指纹/Face ID 加密备份
3. **多签钱包**: 借鉴区块链多签技术
4. **零知识证明**: 增强隐私保护

---

**文档维护**: 在执行各阶段时，请及时更新对应文档和检查清单。

**项目路径**:
- 后端: `/Users/leeyi/project/imboy.pub/imboy`
- 前端: `/Users/leeyi/project/imboy.pub/imboyapp`

**开始执行**: 请查看 [phase-01-preparation.md](./phase-01-preparation.md)

ba
