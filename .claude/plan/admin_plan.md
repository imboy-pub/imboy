# Imboy 后台管理系统开发计划

## 项目概述

本文档详细规划了 Imboy 即时通讯系统的管理后台前端实现方案，参考 VividStream 管理后台的架构设计。

**项目位置**: `/Users/leeyi/project/imboy.pub/imboy-admin-frontend`

**技术栈**:
- **框架**: React 19 + TypeScript
- **构建工具**: Vite 7
- **路由**: React Router v7
- **状态管理**: Zustand
- **数据获取**: TanStack Query
- **UI 组件**: shadcn/ui + Tailwind CSS
- **表单**: React Hook Form + Zod
- **图表**: Recharts
- **HTTP 客户端**: Axios
- **通知**: Sonner
- **图标**: Lucide React

---

## 一、后端 API 分析

### 1.1 现有管理 API

| 模块 | 路由 | 方法 | 功能 |
|------|------|------|------|
| **认证** | `/adm/passport/login` | GET/POST | 管理员登录页面/登录 |
| | `/adm/passport/captcha` | GET | 获取验证码图片 |
| **首页** | `/adm/index` | GET | 管理后台首页 |
| | `/adm/welcome` | GET | 欢迎页面统计 |
| **反馈** | `/adm/feedback/index` | GET | 反馈列表 |
| | `/adm/feedback/reply` | POST | 回复反馈 |
| **版本** | `/adm/app_version/index` | GET | 版本列表 |
| | `/adm/app_version/save` | POST | 保存版本 |
| | `/adm/app_version/delete` | POST | 删除版本 |
| **DDL** | `/adm/app_ddl/index` | GET | DDL 配置列表 |
| | `/adm/app_ddl/save` | POST | 保存 DDL |
| | `/adm/app_ddl/delete` | POST | 删除 DDL |
| **附件** | `/adm/attach/auth` | GET | 附件授权 |

### 1.2 需要扩展的管理 API（后端待开发）

| 模块 | 路由 | 方法 | 功能 |
|------|------|------|------|
| **用户管理** | `/adm/user/list` | GET | 用户列表 |
| | `/adm/user/detail/:id` | GET | 用户详情 |
| | `/adm/user/ban/:id` | POST | 封禁用户 |
| | `/adm/user/unban/:id` | POST | 解封用户 |
| | `/adm/user/search` | GET | 搜索用户 |
| **群组管理** | `/adm/group/list` | GET | 群组列表 |
| | `/adm/group/detail/:id` | GET | 群组详情 |
| | `/adm/group/dissolve/:id` | POST | 解散群组 |
| | `/adm/group/search` | GET | 搜索群组 |
| **消息管理** | `/adm/message/list` | GET | 消息列表 |
| | `/adm/message/search` | GET | 搜索消息 |
| | `/adm/message/delete/:id` | POST | 删除消息 |
| **频道管理** | `/adm/channel/list` | GET | 频道列表 |
| | `/adm/channel/detail/:id` | GET | 频道详情 |
| | `/adm/channel/review/:id` | POST | 审核频道 |
| **管理员** | `/adm/admin/list` | GET | 管理员列表 |
| | `/adm/admin/create` | POST | 创建管理员 |
| | `/adm/admin/update/:id` | POST | 更新管理员 |
| | `/adm/admin/delete/:id` | POST | 删除管理员 |
| **角色权限** | `/adm/role/list` | GET | 角色列表 |
| | `/adm/role/create` | POST | 创建角色 |
| | `/adm/role/update/:id` | POST | 更新角色 |
| | `/adm/role/delete/:id` | POST | 删除角色 |
| **统计报表** | `/adm/stats/overview` | GET | 总览统计 |
| | `/adm/stats/user` | GET | 用户统计 |
| | `/adm/stats/message` | GET | 消息统计 |
| | `/adm/stats/group` | GET | 群组统计 |
| **日志审计** | `/adm/log/list` | GET | 操作日志列表 |
| **系统配置** | `/adm/config/list` | GET | 配置列表 |
| | `/adm/config/update` | POST | 更新配置 |

---

## 二、数据库表结构

### 2.1 核心表

| 表名 | 说明 | 关键字段 |
|------|------|---------|
| \`adm_user\` | 管理员用户 | id, account, nickname, role_id[], status |
| \`adm_role\` | 管理员角色 | id, name, permissions |
| \`user\` | 用户 | id, account, nickname, status, created_at |
| \`user_device\` | 用户设备 | id, user_id, device_name, status |
| \`group\` | 群组 | id, title, owner_uid, member_count, status |
| \`group_member\` | 群成员 | id, group_id, user_id, role, status |
| \`msg_c2c\` | 单聊消息 | id, from_id, to_id, content, created_at |
| \`msg_c2g\` | 群聊消息 | id, from_id, group_id, content, created_at |
| \`channel\` | 频道 | id, name, owner_id, status |
| \`feedback\` | 用户反馈 | id, user_id, content, status |
| \`app_version\` | 应用版本 | id, version, platform, force_update |
| \`user_log\` | 用户日志 | id, user_id, action, ip, created_at |

### 2.2 扩展表

| 表名 | 说明 |
|------|------|
| \`group_vote\` | 群投票 |
| \`group_schedule\` | 群日程 |
| \`group_file\` | 群文件 |
| \`group_album\` | 群相册 |
| \`group_task\` | 群作业 |
| \`group_tag\` | 群标签 |
| \`msg_reaction\` | 消息表情反应 |
| \`msg_forward\` | 消息转发记录 |
| \`conversation_pin\` | 会话置顶 |
| \`e2ee_*\` | 端到端加密相关 |

---

## 三、前端功能模块

### 3.1 功能架构图

\`\`\`
┌─────────────────────────────────────────────────────────────────┐
│                        Imboy Admin Dashboard                     │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐        │
│  │ 仪表盘   │  │ 用户管理 │  │ 群组管理 │  │ 消息管理 │        │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘        │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐        │
│  │ 频道管理 │  │ 反馈处理 │  │ 内容审核 │  │ 系统设置 │        │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘        │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐                      │
│  │ 管理员   │  │ 角色权限 │  │ 日志审计 │                      │
│  └──────────┘  └──────────┘  └──────────┘                      │
└─────────────────────────────────────────────────────────────────┘
\`\`\`

### 3.2 模块详情

#### 1. 仪表盘 (Dashboard)

**页面**: \`/dashboard\`

**功能**:
- 今日注册用户数
- 今日活跃用户数
- 在线用户数/设备数
- 今日消息发送量
- 新建群组数
- 7日用户增长趋势图
- 7日消息量趋势图
- 系统健康状态

**组件**:
- \`StatsCard\` - 统计卡片
- \`LineChart\` - 折线图
- \`BarChart\` - 柱状图
- \`PieChart\` - 饼图

#### 2. 用户管理 (User Management)

**页面**: \`/users\`

**功能**:
- 用户列表（分页、搜索、筛选）
- 用户详情（基本信息、设备列表、好友列表、群组列表）
- 封禁/解封用户
- 查看用户消息记录
- 用户统计信息

**子页面**:
- \`/users\` - 用户列表
- \`/users/:id\` - 用户详情
- \`/users/:id/devices\` - 用户设备
- \`/users/:id/friends\` - 用户好友
- \`/users/:id/groups\` - 用户群组

#### 3. 群组管理 (Group Management)

**页面**: \`/groups\`

**功能**:
- 群组列表（分页、搜索、筛选）
- 群组详情（基本信息、成员列表、消息记录）
- 解散群组
- 设置群组状态
- 查看群组文件/相册

**子页面**:
- \`/groups\` - 群组列表
- \`/groups/:id\` - 群组详情
- \`/groups/:id/members\` - 群组成员
- \`/groups/:id/files\` - 群组文件
- \`/groups/:id/notices\` - 群组公告

#### 4. 消息管理 (Message Management)

**页面**: \`/messages\`

**功能**:
- 消息列表（分页、搜索、筛选）
- 消息搜索（关键词、用户、时间范围）
- 违规消息处理
- 消息统计

**子页面**:
- \`/messages/c2c\` - 单聊消息
- \`/messages/c2g\` - 群聊消息
- \`/messages/search\` - 消息搜索

#### 5. 频道管理 (Channel Management)

**页面**: \`/channels\`

**功能**:
- 频道列表
- 频道详情
- 频道审核
- 频道统计

#### 6. 反馈处理 (Feedback)

**页面**: \`/feedback\`

**功能**:
- 反馈列表（分页、筛选）
- 反馈详情
- 回复反馈
- 反馈状态管理

#### 7. 内容审核 (Content Moderation)

**页面**: \`/moderation\`

**功能**:
- 敏感词管理
- 违规内容列表
- 审核队列
- 审核记录

#### 8. 系统设置 (System Settings)

**页面**: \`/settings\`

**功能**:
- 应用版本管理
- 系统配置
- DDL 管理
- 存储配置

**子页面**:
- \`/settings/versions\` - 版本管理
- \`/settings/config\` - 系统配置
- \`/settings/ddl\` - DDL 管理

#### 9. 管理员管理 (Admin Management)

**页面**: \`/admins\`

**功能**:
- 管理员列表
- 创建管理员
- 编辑管理员
- 删除管理员
- 重置密码

#### 10. 角色权限 (Roles & Permissions)

**页面**: \`/roles\`

**功能**:
- 角色列表
- 创建角色
- 编辑角色权限
- 删除角色

#### 11. 日志审计 (Audit Logs)

**页面**: \`/logs\`

**功能**:
- 操作日志列表
- 日志搜索
- 日志详情
- 日志导出

---

## 四、项目结构

\`\`\`
imboy-admin-frontend/
├── public/
│   └── favicon.ico
├── src/
│   ├── components/
│   │   ├── ui/                    # shadcn/ui 组件
│   │   │   ├── button.tsx
│   │   │   ├── card.tsx
│   │   │   ├── dialog.tsx
│   │   │   ├── dropdown-menu.tsx
│   │   │   ├── form.tsx
│   │   │   ├── input.tsx
│   │   │   ├── table.tsx
│   │   │   ├── tabs.tsx
│   │   │   ├── toast.tsx
│   │   │   └── ...
│   │   ├── layout/                # 布局组件
│   │   │   ├── AdminLayout.tsx    # 主布局
│   │   │   ├── Sidebar.tsx        # 侧边栏
│   │   │   ├── Header.tsx         # 顶部栏
│   │   │   ├── Breadcrumb.tsx     # 面包屑
│   │   │   └── Footer.tsx         # 页脚
│   │   ├── shared/                # 共享组件
│   │   │   ├── DataTable.tsx      # 数据表格
│   │   │   ├── PageHeader.tsx     # 页面头部
│   │   │   ├── SearchBar.tsx      # 搜索栏
│   │   │   ├── FilterPanel.tsx    # 筛选面板
│   │   │   ├── StatsCard.tsx      # 统计卡片
│   │   │   ├── ChartCard.tsx      # 图表卡片
│   │   │   ├── ConfirmDialog.tsx  # 确认对话框
│   │   │   ├── StatusBadge.tsx    # 状态徽章
│   │   │   ├── Avatar.tsx         # 头像组件
│   │   │   └── EmptyState.tsx     # 空状态
│   │   └── auth/                  # 认证组件
│   │       ├── LoginForm.tsx      # 登录表单
│   │       ├── CaptchaInput.tsx   # 验证码输入
│   │       └── ProtectedRoute.tsx # 路由保护
│   ├── pages/
│   │   ├── auth/
│   │   │   └── LoginPage.tsx      # 登录页
│   │   ├── dashboard/
│   │   │   └── DashboardPage.tsx  # 仪表盘
│   │   ├── users/
│   │   │   ├── UserListPage.tsx   # 用户列表
│   │   │   ├── UserDetailPage.tsx # 用户详情
│   │   │   └── UserDevicesPage.tsx# 用户设备
│   │   ├── groups/
│   │   │   ├── GroupListPage.tsx  # 群组列表
│   │   │   └── GroupDetailPage.tsx# 群组详情
│   │   ├── messages/
│   │   │   ├── MessageListPage.tsx# 消息列表
│   │   │   └── MessageSearchPage.tsx# 消息搜索
│   │   ├── channels/
│   │   │   ├── ChannelListPage.tsx
│   │   │   └── ChannelDetailPage.tsx
│   │   ├── feedback/
│   │   │   ├── FeedbackListPage.tsx
│   │   │   └── FeedbackDetailPage.tsx
│   │   ├── settings/
│   │   │   ├── VersionPage.tsx    # 版本管理
│   │   │   ├── ConfigPage.tsx     # 系统配置
│   │   │   └── DDLPage.tsx        # DDL 管理
│   │   ├── admins/
│   │   │   ├── AdminListPage.tsx
│   │   │   └── AdminFormPage.tsx
│   │   ├── roles/
│   │   │   ├── RoleListPage.tsx
│   │   │   └── RoleFormPage.tsx
│   │   ├── logs/
│   │   │   └── LogListPage.tsx
│   │   └── errors/
│   │       └── NotFoundPage.tsx   # 404 页面
│   ├── services/
│   │   └── api/
│   │       ├── client.ts          # Axios 实例
│   │       ├── auth.ts            # 认证 API
│   │       ├── users.ts           # 用户 API
│   │       ├── groups.ts          # 群组 API
│   │       ├── messages.ts        # 消息 API
│   │       ├── channels.ts        # 频道 API
│   │       ├── feedback.ts        # 反馈 API
│   │       ├── settings.ts        # 设置 API
│   │       ├── admins.ts          # 管理员 API
│   │       ├── roles.ts           # 角色 API
│   │       ├── logs.ts            # 日志 API
│   │       └── stats.ts           # 统计 API
│   ├── stores/
│   │   ├── authStore.ts           # 认证状态
│   │   ├── sidebarStore.ts        # 侧边栏状态
│   │   └── settingsStore.ts       # 设置状态
│   ├── hooks/
│   │   ├── useAuth.ts             # 认证 Hook
│   │   ├── useUsers.ts            # 用户数据 Hook
│   │   ├── useGroups.ts           # 群组数据 Hook
│   │   ├── useMessages.ts         # 消息数据 Hook
│   │   └── useStats.ts            # 统计数据 Hook
│   ├── contexts/
│   │   └── AuthContext.tsx        # 认证上下文
│   ├── types/
│   │   ├── api.ts                 # API 响应类型
│   │   ├── user.ts                # 用户类型
│   │   ├── group.ts               # 群组类型
│   │   ├── message.ts             # 消息类型
│   │   ├── channel.ts             # 频道类型
│   │   ├── admin.ts               # 管理员类型
│   │   └── common.ts              # 公共类型
│   ├── lib/
│   │   ├── utils.ts               # 工具函数
│   │   ├── constants.ts           # 常量定义
│   │   ├── formatters.ts          # 格式化函数
│   │   └── validators.ts          # 验证函数
│   ├── styles/
│   │   └── globals.css            # 全局样式
│   ├── App.tsx                    # 应用入口
│   ├── main.tsx                   # 渲染入口
│   └── vite-env.d.ts              # Vite 类型
├── .env.development               # 开发环境变量
├── .env.production                # 生产环境变量
├── index.html                     # HTML 入口
├── package.json                   # 项目配置
├── tsconfig.json                  # TypeScript 配置
├── vite.config.ts                 # Vite 配置
├── tailwind.config.js             # Tailwind 配置
├── postcss.config.js              # PostCSS 配置
└── components.json                # shadcn/ui 配置
\`\`\`

---

## 五、实施计划

### 阶段 1: 项目初始化 (1 天)

| 任务 | 说明 | 状态 |
|------|------|------|
| 创建 Vite + React + TypeScript 项目 | 使用 bun init | ⏳ 待开始 |
| 安装核心依赖 | React Router, Zustand, TanStack Query, Axios | ⏳ 待开始 |
| 配置 Tailwind CSS | 安装 tailwindcss, postcss, autoprefixer | ⏳ 待开始 |
| 初始化 shadcn/ui | 安装并配置 shadcn/ui 组件库 | ⏳ 待开始 |
| 配置路径别名 | 配置 @ 别名指向 src 目录 | ⏳ 待开始 |
| 设置环境变量 | 创建 .env 文件配置 API 地址 | ⏳ 待开始 |

### 阶段 2: 基础架构 (2 天)

| 任务 | 说明 | 状态 |
|------|------|------|
| 实现 API 客户端 | 创建 Axios 实例，配置拦截器 | ⏳ 待开始 |
| 实现认证逻辑 | 登录、登出、Token 管理 | ⏳ 待开始 |
| 实现布局组件 | AdminLayout, Sidebar, Header | ⏳ 待开始 |
| 实现路由配置 | React Router 配置，路由保护 | ⏳ 待开始 |
| 实现共享组件 | DataTable, PageHeader, StatsCard | ⏳ 待开始 |

### 阶段 3: 核心功能 (5 天)

| 任务 | 说明 | 状态 |
|------|------|------|
| 登录页面 | 登录表单、验证码、错误处理 | ⏳ 待开始 |
| 仪表盘页面 | 统计卡片、图表、数据展示 | ⏳ 待开始 |
| 用户管理页面 | 用户列表、详情、封禁功能 | ⏳ 待开始 |
| 群组管理页面 | 群组列表、详情、解散功能 | ⏳ 待开始 |
| 反馈管理页面 | 反馈列表、回复功能 | ⏳ 待开始 |

### 阶段 4: 扩展功能 (4 天)

| 任务 | 说明 | 状态 |
|------|------|------|
| 消息管理页面 | 消息列表、搜索、处理 | ⏳ 待开始 |
| 频道管理页面 | 频道列表、详情、审核 | ⏳ 待开始 |
| 版本管理页面 | 版本列表、创建、编辑 | ⏳ 待开始 |
| 管理员管理页面 | 管理员 CRUD | ⏳ 待开始 |

### 阶段 5: 高级功能 (3 天)

| 任务 | 说明 | 状态 |
|------|------|------|
| 角色权限管理 | 角色列表、权限配置 | ⏳ 待开始 |
| 日志审计页面 | 操作日志列表、搜索 | ⏳ 待开始 |
| 系统配置页面 | 配置管理 | ⏳ 待开始 |
| 性能优化 | 懒加载、缓存、优化 | ⏳ 待开始 |

### 阶段 6: 测试与部署 (2 天)

| 任务 | 说明 | 状态 |
|------|------|------|
| 单元测试 | 关键组件测试 | ⏳ 待开始 |
| 集成测试 | E2E 测试 | ⏳ 待开始 |
| 构建优化 | 生产构建配置 | ⏳ 待开始 |
| 部署配置 | Nginx 配置、CI/CD | ⏳ 待开始 |

---

## 六、API 响应规范

### 6.1 标准响应格式

\`\`\`typescript
interface ApiResponse<T> {
  code: number;          // 0 成功，其他为错误码
  msg: string;           // 响应消息
  data: T;               // 响应数据
}
\`\`\`

### 6.2 分页响应格式

\`\`\`typescript
interface PaginatedResponse<T> {
  items: T[];            // 数据列表
  page: number;          // 当前页码
  size: number;          // 每页数量
  total: number;         // 总数量
  total_pages: number;   // 总页数
}
\`\`\`

### 6.3 登录响应

\`\`\`typescript
interface LoginResponse {
  id: string;            // 管理员 ID
  account: string;       // 账号
  nickname: string;      // 昵称
  avatar: string;        // 头像
  role_id: number[];     // 角色 ID 列表
  next: string;          // 登录后跳转地址
}
\`\`\`

---

## 七、环境配置

### 7.1 开发环境

\`\`\`bash
# .env.development
VITE_API_BASE_URL=http://localhost:8001/adm
VITE_APP_NAME=Imboy Admin
VITE_APP_PORT=8082
\`\`\`

### 7.2 生产环境

\`\`\`bash
# .env.production
VITE_API_BASE_URL=https://api.imboy.pub/adm
VITE_APP_NAME=Imboy Admin
\`\`\`

---

## 八、开发命令

\`\`\`bash
# 安装依赖
bun install

# 启动开发服务器
bun run dev

# 构建生产版本
bun run build

# 预览生产构建
bun run preview

# 代码检查
bun run lint

# 类型检查
bun run typecheck
\`\`\`

---

## 九、依赖清单

### 9.1 核心依赖

\`\`\`json
{
  "dependencies": {
    "react": "^19.0.0",
    "react-dom": "^19.0.0",
    "react-router-dom": "^7.0.0",
    "@tanstack/react-query": "^5.0.0",
    "zustand": "^5.0.0",
    "axios": "^1.6.0",
    "react-hook-form": "^7.50.0",
    "zod": "^3.22.0",
    "@hookform/resolvers": "^3.3.0",
    "recharts": "^2.12.0",
    "sonner": "^1.4.0",
    "lucide-react": "^0.300.0",
    "clsx": "^2.1.0",
    "tailwind-merge": "^2.2.0",
    "class-variance-authority": "^0.7.0",
    "date-fns": "^3.0.0"
  }
}
\`\`\`

### 9.2 开发依赖

\`\`\`json
{
  "devDependencies": {
    "@types/react": "^18.2.0",
    "@types/react-dom": "^18.2.0",
    "@vitejs/plugin-react": "^4.2.0",
    "typescript": "^5.3.0",
    "vite": "^5.0.0",
    "tailwindcss": "^3.4.0",
    "postcss": "^8.4.0",
    "autoprefixer": "^10.4.0",
    "eslint": "^8.56.0",
    "@typescript-eslint/eslint-plugin": "^6.0.0",
    "@typescript-eslint/parser": "^6.0.0",
    "eslint-plugin-react": "^7.33.0",
    "eslint-plugin-react-hooks": "^4.6.0"
  }
}
\`\`\`

---

## 十、注意事项

### 10.1 安全考虑

1. **认证安全**
   - 使用 RSA 加密密码传输
   - 验证码防止暴力破解
   - Token 有效期管理

2. **权限控制**
   - 前端路由权限保护
   - API 请求权限验证
   - 敏感操作二次确认

3. **数据安全**
   - 敏感数据脱敏显示
   - 操作日志记录
   - 数据备份策略

### 10.2 性能优化

1. **代码分割**
   - 路由级别懒加载
   - 组件按需加载

2. **数据缓存**
   - TanStack Query 缓存
   - 静态数据本地存储

3. **渲染优化**
   - 虚拟列表（大数据量）
   - 防抖节流

---

## 十一、后端 API 扩展需求

为了支持完整的管理功能，后端需要新增以下 API：

### 11.1 用户管理 API

\`\`\`erlang
%% src/adm/adm_user_handler.erl (新建)
%% 路由配置
{"/adm/user/list", adm_user_handler, #{action => list}},
{"/adm/user/detail/:id", adm_user_handler, #{action => detail}},
{"/adm/user/ban/:id", adm_user_handler, #{action => ban}},
{"/adm/user/unban/:id", adm_user_handler, #{action => unban}},
{"/adm/user/search", adm_user_handler, #{action => search}},
\`\`\`

### 11.2 群组管理 API

\`\`\`erlang
%% src/adm/adm_group_handler.erl (新建)
{"/adm/group/list", adm_group_handler, #{action => list}},
{"/adm/group/detail/:id", adm_group_handler, #{action => detail}},
{"/adm/group/dissolve/:id", adm_group_handler, #{action => dissolve}},
{"/adm/group/search", adm_group_handler, #{action => search}},
\`\`\`

### 11.3 消息管理 API

\`\`\`erlang
%% src/adm/adm_message_handler.erl (新建)
{"/adm/message/list", adm_message_handler, #{action => list}},
{"/adm/message/search", adm_message_handler, #{action => search}},
{"/adm/message/delete/:id", adm_message_handler, #{action => delete}},
\`\`\`

### 11.4 统计 API

\`\`\`erlang
%% src/adm/adm_stats_handler.erl (新建)
{"/adm/stats/overview", adm_stats_handler, #{action => overview}},
{"/adm/stats/user", adm_stats_handler, #{action => user}},
{"/adm/stats/message", adm_stats_handler, #{action => message}},
{"/adm/stats/group", adm_stats_handler, #{action => group}},
\`\`\`

### 11.5 管理员 API

\`\`\`erlang
%% src/adm/adm_admin_handler.erl (新建)
{"/adm/admin/list", adm_admin_handler, #{action => list}},
{"/adm/admin/create", adm_admin_handler, #{action => create}},
{"/adm/admin/update/:id", adm_admin_handler, #{action => update}},
{"/adm/admin/delete/:id", adm_admin_handler, #{action => delete}},
\`\`\`

### 11.6 角色 API

\`\`\`erlang
%% src/adm/adm_role_handler.erl (新建)
{"/adm/role/list", adm_role_handler, #{action => list}},
{"/adm/role/create", adm_role_handler, #{action => create}},
{"/adm/role/update/:id", adm_role_handler, #{action => update}},
{"/adm/role/delete/:id", adm_role_handler, #{action => delete}},
\`\`\`

### 11.7 日志 API

\`\`\`erlang
%% src/adm/adm_log_handler.erl (新建)
{"/adm/log/list", adm_log_handler, #{action => list}},
{"/adm/log/detail/:id", adm_log_handler, #{action => detail}},
\`\`\`

---

## 十二、成功标准

### 12.1 功能完整性

- [x] 登录/登出功能正常
- [x] 仪表盘数据展示正确
- [x] 用户管理 CRUD 功能完整
- [x] 群组管理功能完整
- [ ] 消息管理功能完整 (后端 API 待开发)
- [ ] 频道管理功能完整 (后端 API 待开发)
- [x] 反馈处理功能完整
- [x] 系统设置功能完整 (版本管理、DDL管理)
- [ ] 管理员管理功能完整 (后端 API 待开发)
- [ ] 角色权限功能完整 (后端 API 待开发)
- [ ] 日志审计功能完整 (后端 API 待开发)

### 12.2 已实现功能详情 (2026-02-17)

| 模块 | 页面 | 状态 | 说明 |
|------|------|------|------|
| 登录认证 | `LoginPage.tsx` | ✅ 完成 | 验证码登录、Token 管理 |
| 路由保护 | `ProtectedRoute.tsx` | ✅ 完成 | 会话验证、自动跳转 |
| 仪表盘 | `DashboardPage.tsx` | ✅ 完成 | 统计卡片、趋势图表 |
| 用户列表 | `UserListPage.tsx` | ✅ 完成 | 分页、搜索、封禁/解封 |
| 用户详情 | `UserDetailPage.tsx` | ✅ 完成 | 基本信息展示、统计信息 |
| 群组列表 | `GroupListPage.tsx` | ✅ 完成 | 分页、搜索、解散 |
| 群组详情 | `GroupDetailPage.tsx` | ✅ 完成 | 基本信息、成员统计 |
| 反馈管理 | `FeedbackListPage.tsx` | ✅ 完成 | 列表展示、内联回复 |
| 版本管理 | `VersionPage.tsx` | ✅ 完成 | CRUD 完整 |
| DDL 管理 | `DDLPage.tsx` | ✅ 完成 | CRUD 完整 |
| 频道管理 | `ChannelListPage.tsx` | ✅ 完成 | 列表展示、删除 |
| 排名统计 | `adm_stats_handler` | ✅ 完成 | 用户/群组/频道排名 |
| 共享组件 | `components/shared/` | ✅ 完成 | 8 个通用组件 |

### 12.2 性能指标

- 页面首次加载 < 3s
- 路由切换 < 500ms
- 列表加载 < 1s
- API 响应 < 500ms

### 12.3 质量指标

- TypeScript 类型覆盖率 > 95%
- ESLint 无错误
- 无控制台报错
- 响应式设计支持

---

**文档版本**: v1.0
**创建日期**: 2026-02-17
**维护者**: Imboy 开发团队
