# ADM 层文档 - 管理后台 API

[根目录](../CLAUDE.md) > **src/adm**

> **最后更新**: 2026-01-20 08:48:18 CST
> **模块数量**: 7 个
> **职责**: 处理管理后台 HTTP API 请求，系统配置与监控

---

## 模块职责

ADM 层是 Imboy 管理后台的入口层，负责：
- 处理管理后台 API 请求
- 管理员认证与权限控制
- 系统配置管理
- 用户反馈处理
- 版本发布管理
- 数据库 DDL 管理

---

## 入口与启动

### 路由定义

管理后台路由在 `src/imboy_router.erl` 中定义：

```erlang
% Admin routes
AdmRoutes = [
    {"/adm", adm_index_handler, #{action => index}},
    {"/adm/welcome", adm_index_handler, #{action => welcome}},
    {"/adm/passport/login", adm_passport_handler, #{action => login}},
    {"/adm/feedback/index", adm_feedback_handler, #{action => index}},
    {"/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
    {"/adm/app_version/index", adm_app_version_handler, #{action => index}},
    {"/static/admin/[...]", cowboy_static, {priv_dir, imboy, "static/admin", [{mimetypes, cow_mimetypes, all}]}}
].
```

### 认证中间件

- **主中间件**: `src/adm/adm_auth_middleware.erl`

管理员登录使用验证码 + 密码认证。

---

## 对外接口

### 管理员认证 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_passport_handler.erl` | `/adm/passport/login` | 管理员登录 |
| `adm_passport_handler.erl` | `/adm/passport/captcha` | 获取验证码 |
| `adm_passport_handler.erl` | `/adm/passport/do_login` | 执行登录 |

### 用户反馈管理 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_feedback_handler.erl` | `/adm/feedback/index` | 反馈列表 |
| `adm_feedback_handler.erl` | `/adm/feedback/reply` | 回复反馈 |

### 应用配置 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_app_ddl_handler.erl` | `/adm/app_ddl/index` | DDL 配置列表 |
| `adm_app_ddl_handler.erl` | `/adm/app_ddl/save` | 保存 DDL 配置 |
| `adm_app_ddl_handler.erl` | `/adm/app_ddl/delete` | 删除 DDL 配置 |

### 版本管理 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_app_version_handler.erl` | `/adm/app_version/index` | 版本列表 |
| `adm_app_version_handler.erl` | `/adm/app_version/save` | 保存版本 |
| `adm_app_version_handler.erl` | `/adm/app_version/delete` | 删除版本 |

### 附件管理 API

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_attach_handler.erl` | `/adm/attach/auth` | 附件授权 |

---

## 关键依赖与配置

### 依赖的 Logic 模块

| ADM Handler | 依赖的 Logic |
|-------------|-------------|
| `adm_passport_handler` | `adm_passport_logic` |
| `adm_feedback_handler` | - |
| `adm_app_ddl_handler` | - |
| `adm_app_version_handler` | `adm_app_version_logic` |

### 依赖的 Repo 模块

- `adm_user_repo.erl`: 管理员用户
- `feedback_repo.erl`: 用户反馈
- `app_ddl_repo.erl`: DDL 配置
- `app_version_repo.erl`: 版本管理

---

## 数据模型

### 管理员登录请求

```json
{
  "username": "admin",
  "password": "password",
  "captcha": "1234"
}
```

### 反馈回复请求

```json
{
  "feedback_id": "feedback_id",
  "reply": "回复内容"
}
```

---

## 测试与质量

### 测试文件位置

```
test/adm/
└── adm_passport_logic_tests.erl
```

### 测试配置

- **超时**: 30 秒
- **环境标记**: `application:set_env(imboy, env, test)`
- **测试框架**: EUnit

---

## 常见问题 (FAQ)

### Q: 如何添加新的管理 API?

1. 在 `src/adm/` 创建新的 handler 文件
2. 在 `src/imboy_router.erl` 添加路由到 `AdmRoutes`
3. 编写测试

### Q: 如何修改验证码配置?

在 `config/sys.config` 中配置 `simple_captcha` 相关参数。

---

## 相关文件清单

### Handler 文件 (7 个)

```
src/adm/
├── adm_app_ddl_handler.erl
├── adm_app_version_handler.erl
├── adm_attach_handler.erl
├── adm_auth_middleware.erl
├── adm_feedback_handler.erl
├── adm_index_handler.erl
└── adm_passport_handler.erl
```

### Logic 文件

```
src/logic/
├── adm_app_version_logic.erl
├── adm_passport_logic.erl
└── adm_user_logic.erl
```

### 测试文件

```
test/adm/
└── adm_passport_logic_tests.erl
```

---

## 变更记录 (Changelog)

### 2026-01-20
- 完善 ADM 层文档
- 新增管理后台 API 文档

---

**文档维护**: 请在添加新的管理 API 时同步更新此文档。
