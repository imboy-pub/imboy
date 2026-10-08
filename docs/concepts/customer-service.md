# 客服域（Customer Service）

> Purpose：定义 IMBoy 内置客服系统的坐席、会话、访客接入与挂件模型。
> 本文描述领域模型与接口规则；实现存在不代表当前交付候选已完成验收。

## Concept：三层接入结构

```
外部访客（Visitor，非 user 账号）
   │  门店密钥（Shop Key）签发访问令牌 / 或嵌入挂件（Widget）
   ▼
客服会话（CS Session：queued → active → closed）
   │  消息不落本域 —— 经企业会话真源（enterprise_message）
   ▼
坐席（Seat = 业务身份上的运营属性：并发上限/启停）
```

核心设计：**坐席挂靠业务身份（Business Identity）而非具体用户**——换人（identity rebind）后坐席原地可用；主键即 `business_identity_id`。

## Current

**已实现：**

- **坐席管理**：`customer_service_seat`（`function_key='customer_service'` 的业务身份才可成为坐席，FK 层拒绝 sales 身份）；租户面 `/cs/organizations/:org_id/seats*`；平台面 `/api/adm/customer-service/organizations/:org_id/seats`（单企业，workspace_id 必填，suspend/resume 同面）与 `/api/adm/customer-service/seats`（跨企业分页：`organization_id` 可选过滤、含已停用坐席、`after_id/limit` 键集分页）。
- **会话状态机**：`customer_service_session`——claim/transfer/close/rating 全部 CAS（`expected_version`）；「同一企业会话至多一个非 closed 客服会话」（部分唯一索引）；rating 仅 closed 可评（1-5）。
- **排队与认领**：键集分页队列（after_id）；claim 在 seat 行锁内做 (status,version) CAS。
- **访客接入**：`customer_service_visit_token`（digest + 过期 + 吊销三条件校验才可发消息）；`customer_service_shop_key`（明文只返回一次）。
- **挂件**：`customer_service_widget_installation(_identity_key/_nonce)`（迁移 132）——公开 widget_id、来源白名单、签名钥摘要、jti 防重放；路由 `/api/v1/cs/widget/*`；imboyadmin 独立构建 `dist-widget/`（iframe + loader）。
- **事件**：`customer_service_event` append-only（actor_kind: seat/visitor/tenant_admin/platform_admin/system）。
- **三端分布**：App 端 `lib/modules/customer_service/`（坐席工作台：入口闸 `cs_seat_gate`、会话状态机、队列）；Admin 端 `/customer-service` 运营面 + 会话列表 + Widget 接入；坐席读写消息复用企业业务端点（会话/消息/ack/资产三步上传）。

验收报告、运行记录和历史计划保存在仓外；交付结论应绑定实际候选、平台与测试结果，不沿用历史状态快照。

## Contract

1. **访客不进 user 表**（不占 License 配额）——客服系统三裁决之一。
2. 客服会话**不存消息副本**：消息唯一真源是企业会话（`enterprise_message`），客服域只管状态与路由。
3. 平台面（admin）客服路径**每条必带 `workspace_id`**（缺失 422）。
4. 挂件凭证只走 `x-cs-visit-token` 头（URL 出现 token 即客户端先拒）。
5. 当前坐席嵌入页面 `/seat/:public_id`：有效可用的 ID 返回 200；缺失、形状非法、不存在或停用的 ID 统一返回 404 `seat_console_unavailable`；非 GET 返回 405；查询参数出现凭证键返回 400。公开 ID 不是登录凭证，页面访问仍受后续票据和来源校验约束。

旧状态码修订曾要求形状非法的 ID 返回 400；当前处理器尚未采用该规则。此合同冲突应在交付验收中明确裁决，不能引用旧执行记录宣称已经实现。

## Constraints

- 坐席身份绑定 `function_key='customer_service'`；`sales` 身份被数据库拒绝。
- active 会话必有坐席、queued 必无坐席（CHECK 组合）。

## References

- 迁移：114（业务身份）、**125**（客服基础）、**132**（挂件）
- 契约：`docs/architecture/2026-09-16-enterprise-organization-cs-compatibility.md`
- 客户端：imboyapp `lib/modules/customer_service/`；imboyadmin `src/modules/customer_service/` + `src/widget/customer_service/`
