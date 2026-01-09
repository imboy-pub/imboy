[根目录](../CLAUDE.md) > **src/adm**

---

# ADM 层 (src/adm/)

> **最后更新**: 2026-01-07 10:05:54 CST
> **模块数量**: 7 个 | **覆盖率**: 50%

## 模块职责

ADM 层是 Imboy 系统的 **管理后台接口层**，负责：

1. **后台认证**: 管理员登录、权限验证
2. **反馈管理**: 用户反馈的查看和回复
3. **版本管理**: APP 版本发布和更新
4. **DDL 管理**: 数据库结构管理
5. **附件鉴权**: 文件上传权限控制

## 模块列表

| 模块 | 说明 | 路由 |
|------|------|------|
| `adm_index_handler.erl` | 后台首页、欢迎页 | `GET /adm` |
| `adm_passport_handler.erl` | 后台登录、验证码 | `GET/POST /adm/passport/*` |
| `adm_feedback_handler.erl` | 反馈列表、回复 | `GET/POST /adm/feedback/*` |
| `adm_app_version_handler.erl` | 版本管理 | `GET/POST /adm/app_version/*` |
| `adm_app_ddl_handler.erl` | DDL 管理 | `GET/POST /adm/app_ddl/*` |
| `adm_attach_handler.erl` | 附件上传鉴权 | `POST /adm/attach/auth` |
| `adm_auth_middleware.erl` | 认证中间件 | - |

## 对外接口

### 后台首页 (`adm_index_handler.erl`)

```
GET /adm            # 后台首页
GET /adm/index      # 首页
GET /adm/welcome    # 欢迎页
```

### 后台登录 (`adm_passport_handler.erl`)

```
GET  /adm/passport/login     # 登录页面
POST /adm/passport/login     # 提交登录
GET  /adm/passport/captcha   # 验证码
```

### 反馈管理 (`adm_feedback_handler.erl`)

```
GET  /adm/feedback/index     # 反馈列表
POST /adm/feedback/reply     # 回复反馈
```

### 版本管理 (`adm_app_version_handler.erl`)

```
GET  /adm/app_version/index  # 版本列表
POST /adm/app_version/save   # 保存版本
POST /adm/app_version/delete # 删除版本
```

### DDL 管理 (`adm_app_ddl_handler.erl`)

```
GET  /adm/app_ddl/index  # DDL 列表
POST /adm/app_ddl/save   # 保存 DDL
POST /adm/app_ddl/delete # 删除 DDL
```

### 附件鉴权 (`adm_attach_handler.erl`)

```
POST /adm/attach/auth  # 附件上传鉴权
```

## 认证机制

### Cookie-based 认证

```erlang
% 认证中间件检查 adm_user_id Cookie
adm_auth_middleware:is_logged_in(Req) -> {true, Req} | {false, Req}

% 获取当前管理员
adm_auth_middleware:current_user(Req) -> Uid | undefined
```

### 路由保护

```erlang
% 未登录用户重定向到登录页
adm_auth_middleware:ensure_logged_in(Req) ->
    case is_logged_in(Req) of
        {true, _} -> next(Req);
        {false, _} -> redirect_to_login(Req)
    end.
```

## 关键依赖

### 上游依赖
- Cowboy 2.10: HTTP 服务器
- ErlyDTL: 模板引擎

### 下游调用
- `src/logic/adm_*_logic.erl`: 后台业务逻辑
- `src/repo/adm_user_repo.erl`: 管理员数据

## 数据模型

### 管理员数据结构

```erlang
#{id => Uid,
  username => Username,
  role => Role,
  created_at => Timestamp}
```

### 反馈数据结构

```erlang
#{id => Id,
  user_id => Uid,
  content => Content,
  status => Status,
  reply => Reply,
  created_at => Timestamp}
```

## 静态资源

### 目录结构

```
priv/static/admin/
├── index.html
├── css/
├── js/
└── images/
```

### 路由配置

```
GET /static/admin/[...]  # 管理后台静态资源
```

## 测试覆盖

### 测试文件

```
test/adm/
├── adm_index_handler_tests.erl
├── adm_passport_logic_tests.erl
├── adm_user_logic_tests.erl
└── ...
```

### 覆盖情况

- **覆盖率**: 约 50%
- **已测试**: 基本功能
- **待补充**: 完整功能测试

## 常见问题

### Q: 如何添加新的后台功能?

A:
1. 在 `src/adm/` 创建 `adm_{模块}_handler.erl`
2. 在 `src/imboy_router.erl` 添加路由
3. 在 `src/logic/` 创建对应的 logic
4. 编写测试文件

### Q: 如何使用模板引擎?

A:
```erlang
% 渲染模板
{ok, Content} = imboy_dtl:render(template_file, Data),
{ok, Req} = cowboy_req:reply(200, #{}, Content, Req).
```

### Q: 如何设置权限?

A:
```erlang
% 检查权限
case check_permission(Uid, Resource) of
    ok -> next(Req);
    {error, forbidden} -> render_error(Req, 403)
end.
```

## 相关文件

- `src/imboy_router.erl`: 路由定义
- `src/adm/adm_auth_middleware.erl`: 认证中间件
- `priv/static/admin/`: 静态资源
- `test/adm/`: 测试文件

## 变更记录

### 2026-01-07
- 更新模块列表
- 更新覆盖率统计

### 2026-01-03
- 初始化 ADM 层文档
- 整理后台接口和认证机制
