# ADM 层文档 - 管理后台 API

[根目录](../CLAUDE.md) > **src/adm**

> **最后更新**: 2026-06-10 | **模块数量**: 27 个
> **职责**: 处理管理后台 HTTP API 请求，权限控制，系统配置与监控
> **以 `find src/adm -maxdepth 1 -name '*.erl' | wc -l` 为准（截至 2026-06）**

---

## 模块职责

ADM 层负责：管理员认证与权限控制、系统配置管理、用户反馈处理、版本发布管理、数据库 DDL 管理、群组资源管理（群公告、日程、任务、投票）、附件授权、消息管理、统计数据、插件管理、首启初始化、用户账户管理。

---

## 路由定义（src/imboy_router.erl）

```erlang
AdmRoutes = [
    {"/adm", adm_index_handler, #{action => index}},
    {"/api/adm/passport/login", adm_passport_handler, #{action => login}},
    {"/api/adm/feedback/index", adm_feedback_handler, #{action => index}},
    {"/api/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
    {"/api/adm/app_version/index", adm_app_version_handler, #{action => index}},
    {"/static/admin/[...]", cowboy_static, {priv_dir, imboy, "static/admin", [...]}}
].
```

认证中间件：`src/adm/adm_auth_middleware.erl`（验证码 + 密码认证）

---

## API 接口清单

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_passport_handler` | `/api/adm/passport/login` | 管理员登录 |
| `adm_passport_handler` | `/api/adm/passport/captcha` | 获取验证码 |
| `adm_passport_handler` | `/api/adm/passport/do_login` | 执行登录 |
| `adm_feedback_handler` | `/api/adm/feedback/index` | 反馈列表 |
| `adm_feedback_handler` | `/api/adm/feedback/reply` | 回复反馈 |
| `adm_app_ddl_handler` | `/api/adm/app_ddl/index` | DDL 配置列表 |
| `adm_app_ddl_handler` | `/api/adm/app_ddl/save` | 保存 DDL 配置 |
| `adm_app_ddl_handler` | `/api/adm/app_ddl/delete` | 删除 DDL 配置 |
| `adm_app_version_handler` | `/api/adm/app_version/index` | 版本列表 |
| `adm_app_version_handler` | `/api/adm/app_version/save` | 保存版本 |
| `adm_app_version_handler` | `/api/adm/app_version/delete` | 删除版本 |
| `adm_attach_handler` | `/api/adm/storage/stats` | 存储统计（总数/各类/今日） |
| `adm_attach_handler` | `/api/adm/storage/index` | 附件分页列表（mime_type/keyword 筛选） |
| `adm_attach_handler` | `/api/adm/storage/download` | 签发下载 presigned GET URL |
| `adm_attach_handler` | `/api/adm/storage/disable` | 禁用附件（status→0） |
| `adm_attach_handler` | `/api/adm/storage/enable` | 启用附件（status→1） |
| `adm_attach_handler` | `/api/adm/storage/delete` | 软删除附件（status→-1） |
| `adm_attach_handler` | `/api/adm/storage/orphan` | 孤儿附件统计 |
| `adm_attach_handler` | `/api/adm/storage/orphan/cleanup` | 物理清理孤儿（先删 S3 再删 DB） |

---

## 模块清单

### 核心管理

| 模块 | 说明 |
|------|------|
| `adm_acl` | RBAC 权限校验共享模块 |
| `adm_auth_middleware` | 认证中间件（验证码+密码） |
| `adm_index_handler` | 管理后台首页（角色、权限列表） |
| `adm_role_handler` | 角色管理接口 |

### 用户与账户

| 模块 | 说明 |
|------|------|
| `adm_admin_handler` | 管理员账户管理 |
| `adm_user_handler` | 用户管理接口 |
| `adm_logout_apply_handler` | 用户注销申请审计接口 |
| `adm_passport_handler` | 管理员认证与登录 |

### 系统与应用

| 模块 | 说明 |
|------|------|
| `adm_app_ddl_handler` | 应用 DDL 配置管理 |
| `adm_app_version_handler` | 应用版本发布管理 |
| `adm_setup_handler` | 首启初始化向导 |
| `adm_stats_handler` | 统计与数据分析接口 |

### 内容与资源

| 模块 | 说明 |
|------|------|
| `adm_announcement_handler` | 全局公告管理 |
| `adm_attach_handler` | 存储/附件管理（统计/列表/下载/禁用/启用/软删/孤儿清理） |
| `adm_channel_handler` | 频道管理接口 |
| `adm_feedback_handler` | 用户反馈处理与回复 |
| `adm_message_handler` | 消息管理接口 |
| `adm_moment_handler` | 动态/瞬间内容管理 |
| `adm_plugin_handler` | 插件市场管理接口 |
| `adm_report_handler` | 举报与违规管理 |

### 群组管理

| 模块 | 说明 |
|------|------|
| `adm_group_handler` | 群组基础管理（CRUD） |
| `adm_group_helper` | 群组管理共用工具函数 |
| `adm_group_content_handler` | 群组文件、相册等子资源管理 |
| `adm_group_notice_handler` | 群组公告管理 |
| `adm_group_schedule_handler` | 群组日程管理 |
| `adm_group_task_handler` | 群组任务管理 |
| `adm_group_vote_handler` | 群组投票管理 |

---

## 依赖关系

| ADM Handler | 依赖 Logic | 依赖 Repo |
|-------------|-----------|----------|
| `adm_passport_handler` | `adm_passport_logic` | `adm_user_repo` |
| `adm_feedback_handler` | - | `feedback_repo` |
| `adm_app_ddl_handler` | - | `app_ddl_repo` |
| `adm_app_version_handler` | `adm_app_version_logic` | `app_version_repo` |
| `adm_user_handler` | `user_logic` | `user_repo` |
| `adm_group_handler` | `group_logic` | `group_repo` |

---

## 关键数据模型

登录请求：`{"username": "admin", "password": "...", "captcha": "1234"}`
反馈回复：`{"feedback_id": "...", "reply": "回复内容"}`
权限缓存键：`{adm_user_permission, AdmUserId}`

---

## 架构特点

- **权限集中**：RBAC 权限校验统一在 `adm_acl` 模块，避免重复代码
- **中间件验证**：请求必须经过 `adm_auth_middleware` 验证（免鉴权路由需在 `imboy_router:open/0` 白名单）
- **群组子资源**：群组相关接口拆分为多个 handler（notice、schedule、task、vote、content），共用 `adm_group_helper` 工具函数
- **初始化保护**：`adm_setup_handler` 首启仅执行一次（配置 flag + 表存在性双重防线）

---

## 操作指南

- **添加新管理 API**：`src/adm/` 建 handler → `imboy_router.erl` 的 `AdmRoutes` 加路由 → `src/adm/adm_auth_middleware` 配置鉴权规则 → 写测试
- **权限管理**：通过 `adm_acl:ensure_permission/3` 检查单个权限，或 `ensure_any_permission/3` 检查多个权限
- **修改验证码配置**：`config/sys.config` 中配置 `simple_captcha` 参数
- **清除权限缓存**：`adm_acl:flush_cache(AdmUserId)` 或后台主动更新角色后自动清除

---

## 测试配置

- 框架：EUnit；超时：30s；环境：`application:set_env(imboy, env, test)`
- 测试目录：`test/adm/` → `adm_passport_logic_tests.erl` 等

