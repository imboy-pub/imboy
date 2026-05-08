# ADM 层文档 - 管理后台 API

[根目录](../CLAUDE.md) > **src/adm**

> **最后更新**: 2026-01-20 | **模块数量**: 7 个
> **职责**: 处理管理后台 HTTP API 请求，系统配置与监控

---

## 模块职责

ADM 层负责：管理员认证与权限控制、系统配置管理、用户反馈处理、版本发布管理、数据库 DDL 管理。

---

## 路由定义（src/imboy_router.erl）

```erlang
AdmRoutes = [
    {"/adm", adm_index_handler, #{action => index}},
    {"/adm/passport/login", adm_passport_handler, #{action => login}},
    {"/adm/feedback/index", adm_feedback_handler, #{action => index}},
    {"/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
    {"/adm/app_version/index", adm_app_version_handler, #{action => index}},
    {"/static/admin/[...]", cowboy_static, {priv_dir, imboy, "static/admin", [...]}}
].
```

认证中间件：`src/adm/adm_auth_middleware.erl`（验证码 + 密码认证）

---

## API 接口清单

| Handler | 路由 | 说明 |
|---------|------|------|
| `adm_passport_handler` | `/adm/passport/login` | 管理员登录 |
| `adm_passport_handler` | `/adm/passport/captcha` | 获取验证码 |
| `adm_passport_handler` | `/adm/passport/do_login` | 执行登录 |
| `adm_feedback_handler` | `/adm/feedback/index` | 反馈列表 |
| `adm_feedback_handler` | `/adm/feedback/reply` | 回复反馈 |
| `adm_app_ddl_handler` | `/adm/app_ddl/index` | DDL 配置列表 |
| `adm_app_ddl_handler` | `/adm/app_ddl/save` | 保存 DDL 配置 |
| `adm_app_ddl_handler` | `/adm/app_ddl/delete` | 删除 DDL 配置 |
| `adm_app_version_handler` | `/adm/app_version/index` | 版本列表 |
| `adm_app_version_handler` | `/adm/app_version/save` | 保存版本 |
| `adm_app_version_handler` | `/adm/app_version/delete` | 删除版本 |
| `adm_attach_handler` | `/adm/attach/auth` | 附件授权 |

---

## 依赖关系

| ADM Handler | 依赖 Logic | 依赖 Repo |
|-------------|-----------|----------|
| `adm_passport_handler` | `adm_passport_logic` | `adm_user_repo` |
| `adm_feedback_handler` | - | `feedback_repo` |
| `adm_app_ddl_handler` | - | `app_ddl_repo` |
| `adm_app_version_handler` | `adm_app_version_logic` | `app_version_repo` |

---

## 关键数据模型

登录请求：`{"username": "admin", "password": "...", "captcha": "1234"}`
反馈回复：`{"feedback_id": "...", "reply": "回复内容"}`

---

## 文件清单

```
src/adm/
├── adm_app_ddl_handler.erl
├── adm_app_version_handler.erl
├── adm_attach_handler.erl
├── adm_auth_middleware.erl
├── adm_feedback_handler.erl
├── adm_index_handler.erl
└── adm_passport_handler.erl

src/logic/
├── adm_app_version_logic.erl
├── adm_passport_logic.erl
└── adm_user_logic.erl

test/adm/
└── adm_passport_logic_tests.erl
```

---

## 测试配置

- 框架：EUnit；超时：30s；环境：`application:set_env(imboy, env, test)`

## 操作指南

- **添加新管理 API**：`src/adm/` 建 handler → `imboy_router.erl` 的 `AdmRoutes` 加路由 → 写测试
- **修改验证码配置**：`config/sys.config` 中配置 `simple_captcha` 参数
