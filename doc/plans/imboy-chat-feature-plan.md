# Imboy 单聊和群聊功能完整实施计划

## 项目概述

本文档详细分析了 Imboy 项目的单聊（C2C）和群聊（C2G）功能现状，并提供完整的 100% 功能实施路径。

**项目技术栈**:
- 后端: Erlang/OTP 28+, Cowboy 2.10, PostgreSQL 18+
- 前端: Flutter (跨平台)
- 架构: DDD 4层架构 (Handler → Logic → DS → Repo)

**当前版本**: v0.7.3
**分析日期**: 2026-02-16
**分析范围**: 单聊、群聊核心功能及扩展功能

---

## 一、功能现状分析

### 1.1 单聊（C2C）功能矩阵

| 功能类别 | 功能项 | 状态 | 说明 | 文件位置 |
|---------|--------|------|------|---------|
| **基础功能** | 发送文本消息 | ✅ 已实现 | WebSocket API v2.0 | src/logic/msg_c2c_logic.erl:c2c/3 |
| | 发送图片消息 | ✅ 已实现 | 支持 base64/URL | - |
| | 发送语音消息 | ✅ 已实现 | - | - |
| | 发送视频消息 | ✅ 已实现 | - | - |
| | 发送文件消息 | ✅ 已实现 | - | - |
| | 发送位置消息 | ✅ 已实现 | - | - |
| | 消息重试机制 | ✅ 已实现 | 2s→5s→7s→11s | src/logic/msg_c2c_logic.erl |
| **消息状态** | 发送中状态 | ✅ 已实现 | 客户端本地 | - |
| | 已发送状态 | ✅ 已实现 | SERVER_ACK | - |
| | 已送达状态 | ✅ 已实现 | CLIENT_ACK | msg_c2c_client_ack/3 |
| | 已读状态 | ✅ 已实现 | 消息已读回执 | msg_c2c_read/3 |
| **消息操作** | 撤回消息 | ✅ 已实现 | 2分钟内可撤回 | msg_c2c_revoke/3 |
| | 编辑消息 | ✅ 已实现 | 修改消息内容 | msg_c2c_edit/3 |
| | 转发消息 | ❌ 待实现 | 需新增 | - |
| | 删除消息 | ⚠️ 部分实现 | 仅本地删除 | - |
| | 引用回复 | ❌ 待实现 | 需新增 | - |
| | 消息置顶 | ✅ 已实现 | 单条消息置顶 | msg_pinned_logic.erl |
| **富媒体** | @提及 | ❌ 待实现 | 单聊不需要 | - |
| | 表情回应 | ❌ 待实现 | emoji反应 | - |
| | 多媒体消息 | ✅ 已实现 | 图片/视频/文件 | - |
| **离线消息** | 离线消息存储 | ✅ 已实现 | PostgreSQL | msg_c2c_ds.erl |
| | 离线消息拉取 | ✅ 已实现 | HTTP API | msg_handler.erl:offline/2 |
| | 离线消息确认 | ✅ 已实现 | 清理机制 | offline_ack/2 |
| **搜索功能** | 全文搜索 | ✅ 已实现 | pg_jieba | fts_logic.erl |
| | 按日期搜索 | ⚠️ 部分实现 | 需完善 | - |
| | 按类型搜索 | ⚠️ 部分实现 | 需完善 | - |
| **E2EE** | 端到端加密 | ✅ 已实现 | RSA+AES | e2ee_logic.erl |
| | 设备间传输 | ✅ 已实现 | 密钥恢复 | e2ee_transfer_logic.erl |
| | 社交恢复 | ✅ 已实现 | 好友设备 | e2ee_social_logic.erl |
| **会话管理** | 会话列表 | ✅ 已实现 | - | conversation_handler.erl |
| | 会话置顶 | ❌ 待实现 | 会话级置顶 | - |
| | 会话免打扰 | ❌ 待实现 | 消息免打扰 | - |
| | 会话删除 | ❌ 待实现 | 删除会话 | - |


### 1.2 群聊（C2G）功能矩阵

| 功能类别 | 功能项 | 状态 | 说明 | 文件位置 |
|---------|--------|------|------|---------|
| **群组管理** | 创建群组 | ✅ 已实现 | 邀请成员 | group_logic:add/4 |
| | 解散群组 | ✅ 已实现 | 仅群主 | group_logic:dissolve/4 |
| | 退出群组 | ✅ 已实现 | 成员退出 | group_member_logic:leave/3 |
| | 群转让 | ✅ 已实现 | 转让群主 | group_logic:transfer/3 |
| | 群信息修改 | ✅ 已实现 | 名称/头像/简介 | group_handler.erl:edit/2 |
| | 群二维码 | ✅ 已实现 | 扫码加入 | group_handler.erl:qrcode/2 |
| | 面对面建群 | ✅ 已实现 | 随机码 | group_logic:face2face/4 |
| **成员管理** | 邀请成员 | ✅ 已实现 | 批量邀请 | group_member_handler.erl:join/2 |
| | 移除成员 | ✅ 已实现 | 踢出群组 | - |
| | 成员列表 | ✅ 已实现 | 分页查询 | page/2 |
| | 成员角色 | ✅ 已实现 | 设置管理员 | role/2 |
| | 成员禁言 | ✅ 已实现 | 禁言功能 | mute/2 |
| | 群内昵称 | ✅ 已实现 | 别名设置 | alias/2 |
| | 共同群组 | ✅ 已实现 | 查询共同群 | same_group/2 |
| **群消息** | 发送群消息 | ✅ 已实现 | 广播机制 | msg_c2g_logic.erl:c2g/3 |
| | @所有人 | ✅ 已实现 | 管理员权限 | - |
| | @特定成员 | ✅ 已实现 | mentions字段 | - |
| | 消息撤回 | ✅ 已实现 | 2分钟限制 | msg_c2g_revoke/3 |
| | 消息编辑 | ✅ 已实现 | 修改内容 | msg_c2g_edit/3 |
| | 群已读统计 | ✅ 已实现 | 已读/总数 | read_stats/2 |
| **群公告** | 发布公告 | ✅ 已实现 | - | group_notice_logic.erl |
| | 公告列表 | ⚠️ 部分实现 | 需完善 | - |
| **群设置** | 群名称 | ✅ 已实现 | - | - |
| | 群头像 | ✅ 已实现 | - | - |
| | 群简介 | ✅ 已实现 | - | - |
| | 加群方式 | ✅ 已实现 | 1不需/2需审核/3仅邀请 | - |
| | 成员上限 | ✅ 已实现 | member_max | - |
| | 群类型 | ✅ 已实现 | 公开/私有 | - |
| **扩展功能** | 群文件 | ❌ 待实现 | 文件共享 | - |
| | 群相册 | ❌ 待实现 | 图片共享 | - |
| | 群投票 | ❌ 待实现 | 投票功能 | - |
| | 群作业 | ❌ 待实现 | 任务分配 | - |
| | 群日程 | ❌ 待实现 | 日程安排 | - |
| | 群直播 | ❌ 待实现 | 直播功能 | - |


---

## 二、待实现功能详细规划

### 2.1 单聊缺失功能

#### P0 - 核心缺失功能

| 功能 | 描述 | 优先级 | 工作量 |
|------|------|--------|--------|
| **消息转发** | 将单条或多条消息转发给其他好友 | P0 | 4人日 |
| **引用回复** | 回复消息时引用原消息内容 | P0 | 3人日 |
| **会话置顶** | 会话列表中置顶重要对话 | P0 | 2人日 |
| **会话删除** | 删除会话及其消息历史 | P0 | 2人日 |

#### P1 - 重要增强功能

| 功能 | 描述 | 优先级 | 工作量 |
|------|------|--------|--------|
| **消息搜索增强** | 按类型/日期/发送者筛选 | P1 | 3人日 |
| **表情回应** | 对消息添加emoji反应 | P1 | 3人日 |
| **消息多选** | 批量选择消息进行操作 | P1 | 2人日 |
| **消息导出** | 导出聊天记录为文件 | P1 | 3人日 |

#### P2 - 体验优化功能

| 功能 | 描述 | 优先级 | 工作量 |
|------|------|--------|--------|
| **消息提醒** | 特殊消息的强提醒 | P2 | 2人日 |
| **输入状态** | 显示"对方正在输入" | P2 | 2人日 |
| **消息已显示** | 消息在屏幕上显示的状态 | P2 | 1人日 |
| **消息撤回增强** | 撤回后编辑重新发送 | P2 | 2人日 |

### 2.2 群聊缺失功能

#### P0 - 核心缺失功能

| 功能 | 描述 | 优先级 | 工作量 |
|------|------|--------|--------|
| **群公告完善** | 公告列表/历史记录/置顶 | P0 | 3人日 |
| **群文件共享** | 文件上传/下载/管理 | P0 | 5人日 |
| **群相册** | 图片共享/管理 | P0 | 4人日 |
| **会话置顶** | 会话列表中置顶群聊 | P0 | 2人日 |

#### P1 - 重要增强功能

| 功能 | 描述 | 优先级 | 工作量 |
|------|------|--------|--------|
| **@提及增强** | @历史/建议列表 | P1 | 2人日 |
| **群管理增强** | 副群主/权限细分 | P1 | 4人日 |
| **群分组** | 群组分类管理 | P1 | 3人日 |
| **群标签** | 群组标签/标记 | P1 | 2人日 |

#### P2 - 体验优化功能

| 功能 | 描述 | 优先级 | 工作量 |
|------|------|--------|--------|
| **群投票** | 发起投票/统计结果 | P2 | 4人日 |
| **群日程** | 日程安排/提醒 | P2 | 5人日 |
| **群作业** | 任务分配/跟踪 | P2 | 4人日 |
| **群直播** | 群内直播功能 | P2 | 8人日 |


---

## 三、技术实现方案

### 3.1 消息转发功能

#### 数据库设计

```sql
-- 新增消息转发记录表
CREATE TABLE msg_forward (
    id BIGSERIAL PRIMARY KEY,
    original_msg_id varchar(40) NOT NULL,
    original_from_id bigint NOT NULL,
    original_to_id bigint NOT NULL,
    original_type varchar(10) NOT NULL,  -- c2c/c2g
    forward_msg_id varchar(40) NOT NULL,
    forward_from_id bigint NOT NULL,
    forward_to_id bigint NOT NULL,
    forward_type varchar(10) NOT NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX idx_msg_forward_original ON msg_forward(original_msg_id);
CREATE INDEX idx_msg_forward_from ON msg_forward(forward_from_id);
```

#### 后端实现

**文件**: src/logic/msg_forward_logic.erl (新建)

```erlang
-module(msg_forward_logic).
-export([forward/4]).

%% @doc 转发消息
%% @param MsgIds 要转发的消息ID列表
%% @param CurrentUid 当前用户ID
%% @param ToId 目标会话ID（单聊用户ID或群聊群ID）
%% @param ToType 目标类型（c2c/c2g）
forward(MsgIds, CurrentUid, ToId, ToType) ->
    % 1. 验证权限（是否是消息发送者或接收者）
    % 2. 获取原始消息内容
    % 3. 创建转发消息
    % 4. 发送到目标会话
    % 5. 记录转发关系
    ok.
```

### 3.2 引用回复功能

#### 数据库设计

```sql
-- 为 msg_c2c 和 msg_c2g 表添加引用回复字段
ALTER TABLE msg_c2c ADD COLUMN IF NOT EXISTS reply_to_msg_id varchar(40);
ALTER TABLE msg_c2c ADD COLUMN IF NOT EXISTS reply_to_from_id bigint;
ALTER TABLE msg_c2c ADD COLUMN IF NOT EXISTS reply_snippet text;

ALTER TABLE msg_c2g ADD COLUMN IF NOT EXISTS reply_to_msg_id varchar(40);
ALTER TABLE msg_c2g ADD COLUMN IF NOT EXISTS reply_to_from_id bigint;
ALTER TABLE msg_c2g ADD COLUMN IF NOT EXISTS reply_snippet text;

CREATE INDEX idx_msg_c2c_reply ON msg_c2c(reply_to_msg_id) WHERE reply_to_msg_id IS NOT NULL;
CREATE INDEX idx_msg_c2g_reply ON msg_c2g(reply_to_msg_id) WHERE reply_to_msg_id IS NOT NULL;
```

### 3.3 会话置顶功能

#### 数据库设计

```sql
-- 会话置顶表
CREATE TABLE conversation_pin (
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    conversation_id varchar(40) NOT NULL,
    conversation_type varchar(10) NOT NULL,  -- c2c/c2g
    pinned_at timestamptz DEFAULT CURRENT_TIMESTAMP,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(user_id, conversation_id, conversation_type)
);

CREATE INDEX idx_conversation_pin_user ON conversation_pin(user_id, pinned_at DESC);
```

### 3.4 表情回应功能

#### 数据库设计

```sql
-- 消息表情回应表
CREATE TABLE msg_reaction (
    id BIGSERIAL PRIMARY KEY,
    msg_id varchar(40) NOT NULL,
    user_id bigint NOT NULL,
    emoji varchar(100) NOT NULL,  -- emoji字符或emoji代码
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(msg_id, user_id, emoji)
);

CREATE INDEX idx_msg_reaction_msg ON msg_reaction(msg_id);
CREATE INDEX idx_msg_reaction_user ON msg_reaction(user_id);
```

### 3.5 群文件共享功能

#### 数据库设计

```sql
-- 群文件表
CREATE TABLE group_file (
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    file_id varchar(40) NOT NULL,
    file_name varchar(255) NOT NULL,
    file_size bigint NOT NULL,
    file_type varchar(100) NOT NULL,
    file_url text NOT NULL,
    uploader_id bigint NOT NULL,
    upload_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    status smallint DEFAULT 1  -- 1正常 0删除
);

CREATE INDEX idx_group_file_group ON group_file(group_id, upload_at DESC);
CREATE INDEX idx_group_file_uploader ON group_file(uploader_id);
```


---

## 四、实施计划

### 阶段1: 核心功能完善 (预计 4 周)

#### Week 1-2: 单聊核心功能

| 任务 | 负责模块 | 预计工时 | 依赖 |
|------|---------|---------|------|
| 消息转发功能 | msg_forward_logic | 4人日 | - |
| 引用回复功能 | msg_c2c_logic (修改) | 3人日 | - |
| 会话置顶 | conversation_pin_logic | 2人日 | - |
| 会话删除 | conversation_logic (修改) | 2人日 | - |

#### Week 3-4: 群聊核心功能

| 任务 | 负责模块 | 预计工时 | 依赖 |
|------|---------|---------|------|
| 群公告完善 | group_notice_logic (修改) | 3人日 | - |
| 群文件共享 | group_file_logic (新建) | 5人日 | OSS配置 |
| 群相册 | group_album_logic (新建) | 4人日 | OSS配置 |
| 会话置顶 | conversation_pin_logic (共用) | 1人日 | Week1-2成果 |

### 阶段2: 增强功能 (预计 3 周)

#### Week 5-6: 富媒体功能

| 任务 | 负责模块 | 预计工时 | 依赖 |
|------|---------|---------|------|
| 表情回应 | msg_reaction_logic | 3人日 | - |
| 消息搜索增强 | fts_logic (修改) | 3人日 | - |
| 消息多选 | 前端为主 | 2人日 | - |
| @提及增强 | msg_c2g_logic (修改) | 2人日 | - |

#### Week 7: 管理功能

| 任务 | 负责模块 | 预计工时 | 依赖 |
|------|---------|---------|------|
| 群管理增强 | group_member_logic (修改) | 4人日 | - |
| 群分组 | group_category_logic (新建) | 3人日 | - |
| 群标签 | group_tag_logic (新建) | 2人日 | - |

### 阶段3: 扩展功能 (预计 4 周)

#### Week 8-10: 协作功能

| 任务 | 负责模块 | 预计工时 | 依赖 |
|------|---------|---------|------|
| 群投票 | group_vote_logic (新建) | 4人日 | - |
| 群日程 | group_schedule_logic (新建) | 5人日 | - |
| 群作业 | group_task_logic (新建) | 4人日 | - |

#### Week 11: 体验优化

| 任务 | 负责模块 | 预计工时 | 依赖 |
|------|---------|---------|------|
| 输入状态 | user_status_logic (新建) | 2人日 | - |
| 消息已显示 | msg_display_logic (新建) | 1人日 | - |
| 消息提醒 | msg_notify_logic (新建) | 2人日 | - |
| 群直播 | webrtc_logic (扩展) | 8人日 | WebRTC基础设施 |


---

## 五、技术架构调整

### 5.1 新增模块清单

#### Logic 层 (10个新增)

- msg_forward_logic.erl           # 消息转发
- msg_reaction_logic.erl          # 表情回应
- conversation_pin_logic.erl      # 会话置顶
- group_file_logic.erl            # 群文件
- group_album_logic.erl           # 群相册
- group_vote_logic.erl            # 群投票
- group_schedule_logic.erl        # 群日程
- group_task_logic.erl            # 群作业
- user_status_logic.erl           # 用户状态
- msg_notify_logic.erl            # 消息提醒

#### Handler 层 (6个新增)

- msg_forward_handler.erl         # 消息转发API
- msg_reaction_handler.erl        # 表情回应API
- conversation_pin_handler.erl    # 会话置顶API
- group_file_handler.erl          # 群文件API
- group_vote_handler.erl          # 群投票API
- user_status_handler.erl         # 用户状态API

#### Repo 层 (10个新增)

- msg_forward_repo.erl            # 消息转发仓库
- msg_reaction_repo.erl           # 表情回应仓库
- conversation_pin_repo.erl       # 会话置顶仓库
- group_file_repo.erl             # 群文件仓库
- group_album_repo.erl            # 群相册仓库
- group_vote_repo.erl             # 群投票仓库
- group_schedule_repo.erl         # 群日程仓库
- group_task_repo.erl             # 群作业仓库
- user_status_repo.erl            # 用户状态仓库
- msg_notify_repo.erl             # 消息提醒仓库

#### DS 层 (10个新增)

- msg_forward_ds.erl              # 消息转发数据服务
- msg_reaction_ds.erl             # 表情回应数据服务
- conversation_pin_ds.erl         # 会话置顶数据服务
- group_file_ds.erl               # 群文件数据服务
- group_album_ds.erl              # 群相册数据服务
- group_vote_ds.erl               # 群投票数据服务
- group_schedule_ds.erl           # 群日程数据服务
- group_task_ds.erl               # 群作业数据服务
- user_status_ds.erl              # 用户状态数据服务
- msg_notify_ds.erl               # 消息提醒数据服务

### 5.2 数据库迁移脚本

#### 迁移文件清单

- 00000053_msg_forward.sql         # 消息转发表
- 00000054_msg_reply.sql           # 引用回复字段
- 00000055_msg_reaction.sql        # 表情回应表
- 00000056_conversation_pin.sql    # 会话置顶表
- 00000057_group_file.sql          # 群文件表
- 00000058_group_album.sql         # 群相册表
- 00000059_group_vote.sql          # 群投票表
- 00000060_group_schedule.sql      # 群日程表
- 00000061_group_task.sql          # 群作业表
- 00000062_user_status.sql         # 用户状态表
- 00000063_msg_notify.sql          # 消息提醒表

---

## 六、测试策略

### 6.1 单元测试

#### 测试覆盖目标

| 模块类型 | 覆盖率目标 | 重点 |
|---------|-----------|------|
| Logic 层 | 80%+ | 业务逻辑 |
| Repo 层 | 90%+ | 数据操作 |
| DS 层 | 70%+ | 数据服务 |
| Handler 层 | 60%+ | API 接口 |

### 6.2 集成测试

#### 测试场景

1. **消息转发流程**
   - 单聊 → 单聊转发
   - 单聊 → 群聊转发
   - 群聊 → 单聊转发
   - 群聊 → 群聊转发
   - 批量转发

2. **引用回复流程**
   - 创建引用回复
   - 查看引用消息
   - 嵌套引用（回复的回复）

3. **会话置顶流程**
   - 置顶会话
   - 取消置顶
   - 置顶排序
   - 跨设备同步


---

## 七、风险评估与缓解

### 7.1 技术风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| 数据库性能下降 | 高 | 中 | 添加索引、分表、缓存 |
| 消息延迟增加 | 高 | 中 | 优化消息队列、异步处理 |
| 存储成本增加 | 中 | 高 | OSS生命周期管理、定期清理 |
| 并发冲突 | 中 | 中 | 乐观锁、分布式锁 |

### 7.2 业务风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| 用户滥用功能 | 中 | 高 | 限流、权限控制 |
| 内容安全问题 | 高 | 中 | 敏感词过滤、人工审核 |
| 数据泄露 | 高 | 低 | 加密存储、访问控制 |

### 7.3 进度风险

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| 需求变更 | 中 | 高 | 迭代开发、MVP优先 |
| 人力不足 | 高 | 中 | 外包协作、优先级调整 |
| 技术难点 | 中 | 低 | 技术预研、专家咨询 |

---

## 八、成功标准

### 8.1 功能完整性

- [ ] 100% 单聊核心功能（P0）
- [ ] 100% 群聊核心功能（P0）
- [ ] 80% 增强功能（P1）
- [ ] 50% 扩展功能（P2）

### 8.2 性能指标

- 消息发送延迟 < 200ms (P99)
- 消息接收延迟 < 500ms (P99)
- 并发在线用户 > 100,000
- 数据库查询 < 100ms (P95)

### 8.3 质量指标

- 单元测试覆盖率 > 70%
- 集成测试通过率 100%
- 线上故障率 < 0.1%
- 用户满意度 > 90%

---

## 九、总结

本实施计划详细分析了 Imboy 项目的单聊和群聊功能现状，识别了已实现和待实现的功能，并提供了完整的技术方案和实施路径。

### 关键发现

1. **基础功能完善**: 核心的单聊和群聊功能已基本实现，包括消息发送、接收、撤回、编辑、已读回执等
2. **扩展功能缺失**: 高级功能如消息转发、引用回复、表情回应、群文件共享等需要新增
3. **架构优势**: DDD 4层架构使得新功能添加清晰、模块化
4. **技术栈成熟**: Erlang/OTP + PostgreSQL + TimescaleDB 提供了高性能和可扩展性

### 实施建议

1. **优先级排序**: 先完成 P0 核心功能，再考虑 P1/P2 扩展功能
2. **迭代开发**: 采用敏捷开发，每2周一个迭代
3. **持续测试**: 每个功能完成后立即编写测试
4. **文档同步**: 及时更新 API 文档和用户手册

### 预期成果

完成本计划后，Imboy 将具备与微信、Telegram 等主流 IM 应用相当的单聊和群聊功能，为用户提供完整的即时通讯体验。

---

**文档版本**: v1.0
**最后更新**: 2026-02-16
**维护者**: Imboy 开发团队
**审核者**: 技术负责人

