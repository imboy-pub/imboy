# 墨芽教学域错误码表 — Step 4 冻结

> 前置事实（读代码确认）：imboy 错误响应统一 envelope 为
> `{code: integer, msg: string, sv_ts: int64_ms, payload: object}`，
> 由 `src/lib/elib_response.erl` 的 `reply_json/5` 产出；业务错误 HTTP 200 +
> envelope code != 0，认证边界可用 `error_with_status/4` 发真实 HTTP 状态码。
> 错误码宏定义在 `imboy/include/error_code.hrl`，现有号段：0 成功 / 1 通用 /
> 4xx 参考 HTTP / 9xx IM 业务 / 5200 QR 登录 / 5300-5399 群作业 / 5000-5063 E2EE /
> 5100-5104 设备会话 / 5190-5191 功能开关。
>
> **教学域分配新号段 5400-5519**（紧邻 5300 群作业段之后，无冲突；本表为
> Step 9 实现时写入 error_code.hrl 的冻结依据）。所有 msg 中文、不泄漏内部细节。

## 复用的既有通用码（不重定义）

| code | 宏（现有） | 教学域用法 |
|---|---|---|
| 400 | ERR_BAD_REQUEST | 参数畸形（非数字 ID、JSON 结构错误） |
| 401 | ERR_UNAUTHORIZED | 认证边界；真实 HTTP 401 + envelope |
| 403 | ERR_FORBIDDEN | 资源级 ACL 拒绝（跨 Org/Workspace、非 staff、Owner 越权），msg 统一"无权访问该资源"，不区分不存在/无权（T14） |
| 404 | ERR_NOT_FOUND | 仅用于对请求者确属"自身资源不存在"的场景（如自己的 assignment 列表里查无此项）；跨权资源一律 403 |
| 409 | ERR_CONFLICT | 非法状态跳转（withdrawn→submitted 等，状态机文档 §5） |
| 410 | ERR_GONE | 资源已删除/关闭且请求者有权知道 |
| 422 | ERR_MISSING_PARAM / ERR_PARAM_INVALID | 缺参/参数值非法（校验类） |
| 429 | ERR_TOO_MANY_REQUESTS | 限流（沿用现有机制） |

## 教学域新码（5400-5519）

### 5400-5419 登录与身份

| code | 宏名（建议） | msg | 语义 |
|---|---|---|---|
| 5401 | ERR_WECHAT_LOGIN_FAILED | 微信登录失败 | jscode2session 网络失败/配置缺失之外的登录失败（AUTH-01） |
| 5402 | ERR_WECHAT_CODE_INVALID | 微信登录凭证无效或已使用 | code 重放/无效（T11）；不区分细节 |
| 5403 | ERR_TEACHING_PROVIDER_UNCONFIGURED | 登录服务未配置 | wechat_mini provider 未配置/appsecret 缺失；响应不泄漏内部组件名（AUTH-01） |
| 5404 | ERR_TEACHING_IDENTITY_NONE | 暂无教学身份 | 登录成功但无任何 guardian/staff/owner 关系（引导绑定，非错误场景可改为 code=0 空列表，由 Step 9 定；登记以防需要） |

### 5420-5439 上下文与 ACL

| code | 宏名（建议） | msg | 语义 |
|---|---|---|---|
| 5420 | ERR_TEACHING_CONTEXT_INVALID | 上下文不属于当前用户 | switch 到非本人身份（T12） |
| 5421 | ERR_TEACHING_CONTEXT_INACTIVE | 上下文已失效 | 关系行 removed/archived 后切换 |
| 5422 | ERR_TEACHING_LEARNER_NOT_GUARDED | 未监护该学员 | 列表请求 learner_id 非本人监护（显式拒绝，非空列表，T3） |
| 5423 | ERR_TEACHING_NOT_GUARDIAN | 无监护提交权限 | 提交时非 can_submit 监护人 / body.learner_id 与资源不符 |
| 5424 | ERR_TEACHING_NOT_STAFF | 非本班任课老师 | 队列/工作台访问者非该班 class_staff（T4/T5/T6） |
| 5425 | ERR_TEACHING_STAFF_WRITE_DENIED | 当前教学角色无写权限 | assistant 调 review-draft/publish（T13） |
| 5426 | ERR_TEACHING_CROSS_ORG | 跨机构访问被拒绝 | 资源链 Org 不匹配（T1）；msg 与 403 策略一致时可并入 403，独立码便于审计分类 |

### 5440-5459 作业与提交

| code | 宏名（建议） | msg | 语义 |
|---|---|---|---|
| 5440 | ERR_ASSIGNMENT_NOT_FOUND | 作业不存在 | 对请求者不可见的 assignment（含跨权，T14 一致化） |
| 5441 | ERR_SUBMISSION_ASSETS_INVALID | 提交附件不合规 | 缺视频/照片超量/附件非本人 confirm/MIME 不符 |
| 5442 | ERR_ASSIGNMENT_CLOSED | 作业已截止或关闭 | task 关闭后提交（状态机 §4） |
| 5443 | ERR_SUBMISSION_NOT_FOUND | 提交不存在或不可见 | 同 T14 一致化 |
| 5444 | ERR_SUBMISSION_WITHDRAWN | 该提交已撤回 | 对 withdrawn submission 的写操作 |

### 5460-5479 幂等与并发

| code | 宏名（建议） | msg | 语义 |
|---|---|---|---|
| 5460 | ERR_IDEMPOTENCY_CONFLICT | 请求与幂等键已绑定内容冲突 | 同 key 不同 body（T8b）；不覆盖原结果 |
| 5461 | ERR_IDEMPOTENCY_KEY_REQUIRED | 缺少幂等键 | createSubmission 未带 Idempotency-Key |

### 5480-5499 回评

| code | 宏名（建议） | msg | 语义 |
|---|---|---|---|
| 5480 | ERR_REVIEW_DRAFT_NOT_FOUND | 无可发布的回评草稿 | publish 时无 draft 行且无已发布结果（状态机 §2） |
| 5481 | ERR_SUBMISSION_REVIEWED | 该提交已有发布回评，不可撤回 | withdrawn 守卫（状态机 §1） |
| 5482 | ERR_REVIEW_SUBMISSION_WITHDRAWN | 提交已撤回，无法发布回评 | publish 时 submission=withdrawn |
| 5483 | ERR_REVIEW_CONFIRM_MISMATCH | 发布确认学员不一致 | confirm_learner_id ≠ submission.learner_id（二次确认防线） |
| 5484 | ERR_REVIEW_FIELD_NOT_ACCEPTED | 请求包含服务端保留字段 | 提交 reviewer_uid/status/published_at 伪造（T7；忽略模式则不触发） |
| 5485 | ERR_REVIEW_EMPTY_CONTENT | 回评内容为空 | 发布时无任何有效反馈内容（四要素+视频全空） |

### 5500-5519 AI 草稿（预留，老师侧）

| code | 宏名（建议） | msg | 语义 |
|---|---|---|---|
| 5501 | ERR_AI_DRAFT_UNAVAILABLE | AI 草稿不可用 | 老师侧读取异常（非 failed —— failed 是正常态，见 ai-review-draft schema） |

> 家长侧永远不出现 5501：家长 payload 无 AI 草稿字段（D-10），只有三态 ai_status_hint。

## 使用规则（冻结）

1. 新码必须先写入 `include/error_code.hrl`（含 ERROR_MSG_MAP 中文条目）再在 Logic 层使用；本表宏名为 Step 9 的实现建议名。
2. 任何新增错误码不得复用 5300-5399（群作业既有段）。
3. 错误响应 payload 最多携带字段级提示（如 `{"field":"learner_id"}`），不携带跨租户资源信息、内部 SQL、openid、对象键。
4. 403 与 5422/5423/5424 的分工：542x 是"身份关系明确不匹配"的教学域语义码（便于小程序端引导绑定/提示）；通用 403 用于其余资源级拒绝。两者响应体同为 ErrorEnvelope。
