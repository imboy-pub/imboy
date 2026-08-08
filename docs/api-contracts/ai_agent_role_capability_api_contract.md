# AI Agent 角色与能力管理 API 契约

> 状态：实施契约。路径前缀为 `/api/adm`，请求需要管理员登录态；读权限使用 `users:read`，写权限使用 `users:update`。

## 1. 术语

- `AI 助手 / Agent`：现有 `ai_agent` 账号实例。
- `role_code`：角色模板的稳定业务编码，对应现有 `ai_agent.role_id` varchar 字段；第一阶段不是 TSID。
- `published_version`：当前运行时使用的角色版本。
- `draft_version`：尚未影响运行时的编辑版本。

## 2. 能力策略

```json
{
  "knowledge": {
    "mode": "off|on_demand|required",
    "source": "faq|group_rule|all",
    "max_context_bytes": 2400
  },
  "group_reply": {
    "mode": "off|mention_only"
  },
  "proactive": {
    "mode": "off|welcome_only",
    "daily_limit": 1
  }
}
```

默认安全策略：knowledge=`on_demand`、group_reply=`off`、proactive=`off`。未知 key、未知 mode、负数额度和超过最大上下文限制均返回参数错误。

## 3. 角色分页

### `GET /ai_agent/role/list`

请求参数：

```text
page=1&size=10&keyword=welcome&status=1
```

响应：

```json
{
  "code": 0,
  "msg": "ok",
  "payload": {
    "items": [
      {
        "code": "official_welcome",
        "name": "官方新手助手",
        "description": "负责新用户欢迎",
        "status": 1,
        "active_version": 2,
        "bound_agent_count": 3,
        "updated_at": "2026-08-08T10:00:00Z"
      }
    ],
    "page": 1,
    "size": 10,
    "total": 1
  }
}
```

## 4. 角色详情

### `GET /ai_agent/role/detail?role_code=official_welcome`

响应 payload：

```json
{
  "code": "official_welcome",
  "name": "官方新手助手",
  "description": "负责新用户欢迎",
  "status": 1,
  "active_version": 2,
  "bound_agent_count": 3,
  "version": 2,
  "state": "published",
  "system_prompt": "你是 imboy 官方 AI 新手助手……",
  "capabilities": { "knowledge": true },
  "knowledge_policy": {
    "knowledge": { "mode": "on_demand", "source": "all", "max_context_bytes": 2400 },
    "group_reply": { "mode": "off" },
    "proactive": { "mode": "welcome_only", "daily_limit": 0 }
  }
}
```

## 5. 创建和保存草稿

### `POST /ai_agent/role/create`

请求 body：

```json
{
  "code": "official_welcome",
  "name": "官方新手助手",
  "description": "负责新用户欢迎"
}
```

### `POST /ai_agent/role/draft`

请求 body：

```json
{
  "role_code": "official_welcome",
  "system_prompt": "你是 imboy 官方 AI 新手助手……",
  "capabilities": { "knowledge": true },
  "knowledge_policy": {
    "knowledge": { "mode": "on_demand", "source": "all", "max_context_bytes": 2400 },
    "group_reply": { "mode": "off" },
    "proactive": { "mode": "welcome_only", "daily_limit": 0 }
  }
}
```

保存成功只更新 draft，不更新 active version。

## 6. 发布

### `POST /ai_agent/role/publish`

请求 body：

```json
{
  "role_code": "official_welcome",
  "version": 3
}
```

发布操作本身是显式影响确认；服务端从已认证管理态记录 published_by，不接受请求体伪造发布人。发布成功后所有绑定 Agent 使用新版本。

## 7. Agent 列表和详情补充

Agent 管理列表行增加：

```json
{
  "role_code": "official_welcome",
  "role_name": "官方新手助手",
  "role_version": 2
}
```

Agent 运行时只接受已发布角色作为行为来源；旧接口保留 system_prompt、capabilities 字段用于未绑定角色的兼容回退。绑定角色的后台编辑不应依赖这些字段覆盖角色。

## 8. 统一错误

至少覆盖：

- `role_not_found`
- `role_inactive`
- `role_invalid_policy`
- `role_version_not_found`
- `role_publish_conflict`
- `role_bound_agents_changed`
- `role_legacy_fallback`
- `capability_denied`

错误响应继续使用项目统一 `elib_response:error/3` 信封。
