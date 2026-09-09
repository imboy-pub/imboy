# STEP-08 备注 — 实现说明与设计决策

## 交付文件（imboy@5b7e2055 基线上新增/修改）

| 文件 | 层 | 说明 |
|---|---|---|
| `include/error_code.hrl` | M | 教学域 5400-5485 错误码段 + ERROR_MSG_MAP 中文条目（契约 STEP-04/error-codes.md 冻结值的落地） |
| `src/repo/teaching_context_repo.erl` | 新 Repo | 教学 SQL 唯一入口：三类上下文解析、五种 ACL 关系查询、submission/assignment 资源链五跳 JOIN |
| `src/logic/teaching_acl.erl` | 新 Logic | 集中式 deny-by-default 守卫：resolve_guardian/2,3、resolve_staff/2,3、resolve_org_owner/2、assert_same_org/2、submission_access/2 |
| `src/logic/teaching_auth_logic.erl` | 新 Logic | 微信登录：provider 配置 fail-closed → jscode2session → sso_identity_ds:find_uid → token 签发 |
| `src/logic/teaching_wechat_client.erl` | 新 Logic | jscode2session HTTP 薄封装（独立模块仅为可 meck；errcode 全部折叠 invalid_code，T11） |
| `src/logic/teaching_context_logic.erl` | 新 Logic | contexts 组装 + switch 归属校验（无凭证语义）；TSID 全部 integer_to_binary 输出 |
| `src/api/teaching_auth_handler.erl` | 新 Handler | POST /api/v1/auth/wechat-mini/login（错误码映射 5401-5404/422） |
| `src/api/teaching_context_handler.erl` | 新 Handler | contexts / switch（错误码映射 5420/5421/422） |
| `src/imboy_router.erl` | M | +3 路由；login 加入 open/0 白名单（wx.login 握手前无 sign/did 头，code 即凭证） |
| `test/logic/teaching_auth_logic_tests.erl` | 新 | AUTH-01（8 用例） |
| `test/logic/teaching_acl_tests.erl` | 新 | ACL-01/ACL-02（13 用例） |

## 关键设计决策

1. **登录即"绑定校验"而非注册**：openid 无 sso_identity 映射 → 5404（identity_none）。
   首版家长账号由机构侧建立绑定（试点流程），符合 AUTH-01"未绑定用户按契约失败"。
   sso_identity 绑定入口沿用 sso_identity_ds:bind/4（本波未开放新端点，属机构管理域）。
2. **teacher_review 权限细分前置**：resolve_staff(_, _, write) 白名单 [manager, teacher]，
   assistant 只读（TEACHER-02 在 Step 9 的 publish/review-draft 上复用该守卫）。
3. **switch 无凭证**：只校验归属 + 回显快照，服务端不存"当前上下文"（无表、无 token、
   无 session 态）；客户端选择由 moya 本地保存。ACL-02 语义（每次请求独立鉴权）由
   submission_access 等守卫保证，与 switch 完全解耦。
4. **guardian 上下文 LEFT JOIN enrollment**：未入班学员也是合法上下文（家长先看到孩子），
   group/workspace 字段为空串；claims_mismatch 对空值实际拒绝（deny-by-default）。
5. **submission_access 判定顺序 staff→guardian→owner-deny→forbidden**：
   同一人兼具两身份返回 staff 超集视角；owner_not_granted 与 forbidden 在 handler 层
   同映射 403，原子不同仅供测试区分（T5）。
6. **token 不绑设备**（legacy did=<<>>）：设备绑定随 Step 13 客户端会话设计再启用，
   不影响本波 AUTH-01。
7. **wechat 配置项**：`wechat_mini_appid` / `wechat_mini_secret`（config_ds env，未配置
   即 5403）、`wechat_mini_jscode_url`（本地 mock 端点覆盖用）。本波未配置任何真实凭据。

## 给 Step 9 的接口约定

- `teaching_acl:submission_access/2` 已返回完整资源链（assignment_id/task_id/group_id/
  org_id/staff|guardian 及关系行），Step 9 的队列/工作台/发布直接复用，不再重复解析。
- `teaching_acl:resolve_guardian(Uid, LearnerId, submit)` = 提交守卫（can_submit=true）。
- `teaching_context_repo:assignment_scope/1` 已就绪（assignment→task→group→workspace→org
  + task_status），供家长作业列表与提交校验。
- 错误码 atom→宏映射在 handler 层；新端点沿用同一模式。

## 已知限制 / 风险

- Handler 层无独立 eunit（薄适配）；HTTP 层契约验证在 Step 17。
- contexts 每次实时三查（无缓存）；班级规模 <100 人无压力（与 STEP-06 风险评估一致）。
- `resolve_org_owner` 未细分 archived 机构（首版机构管理域未开放，见 teaching_acl 注释）。
- git index 出现与本任务无关的污染（error_code.hrl 显示 staged M），按 Coordinator
  指令未执行任何 git 操作；文件内容以工作区为准。
