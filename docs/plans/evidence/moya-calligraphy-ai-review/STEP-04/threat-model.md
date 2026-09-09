# 墨芽教学域威胁模型 — Step 4 冻结（SEC-01）

> 认证基线：所有端点 Bearer IMBoy JWT（除 wechat-mini login）。
> 核心原则：**deny-by-default** —— 服务端从 JWT uid → DB 关系链
> （user → guardian_learner / class_staff / organization.owner → learner →
> assignment → submission → group → workspace → organization）解析一切权限；
> 客户端声明的 organization_id / workspace_id / learner_id / reviewer_uid /
> published_at / AI status 不构成任何授权依据。
>
> 预期响应码约定：业务拒绝 = HTTP 200 + envelope code（imboy 惯例）；
> 认证失败 = 真实 HTTP 401。下表"预期响应"均指 envelope code（HTTP 200 除注明外）。

## 测试矩阵 T1–T12

| # | 攻击场景 | 攻击路径（假设攻击者已持有合法 JWT） | 服务端防线 | 预期响应 |
|---|---|---|---|---|
| T1 | 跨 Organization 访问 | 老师 A（Org1 任教）用 `/teaching/submissions/{id}` 直接猜/枚举 Org2 的 submission_id（TSID 时间有序，可猜测相邻段） | submission → assignment → task → group → workspace.organization_id 与请求者的 class_staff/监护人关系必须落在同一 Org；Org 不同的任何身份组合（含 Owner）拒绝 | 403 `ERR_FORBIDDEN`（资源不可见，不确认存在性） |
| T2 | 跨 Workspace 访问（同 Org） | 监护人 W1 内学员甲（Ws1 班），尝试读 Ws2 班另一学员 submission；或 Ws1 老师读 Ws2 班（非其任教班级）submission | 监护人路径：guardian_learner(learner) 必须匹配；老师路径：class_staff(group) 必须匹配 —— Workspace 归属不豁免班级级授权（§5.2：仅"负责班级"） | 403 |
| T3 | 跨 learner 访问（同班） | 监护人甲的家长请求 `assignments?learner_id=` 传学员乙 ID；或提交时 body.learner_id 换成学员乙 | guardian_learner(guardian_uid=JWT uid, learner_id=乙) 不存在 ⇒ 拒绝；提交还需 can_submit=true。**显式拒绝而非空列表**，防止枚举 | 5423 `ERR_TEACHING_NOT_GUARDIAN` |
| T4 | 仅 Group 管理员（非 class_staff）访问儿童视频 | 群主/群管理员（group_member 管理 role）调用 review-workbench / view_url 拿学员视频 | 教学授权真源是 class_staff，不从 Group 管理员推断（D-07/§5.1）；无 class_staff 行 ⇒ 拒绝 | 403 或 5424 `ERR_TEACHING_NOT_STAFF` |
| T5 | 仅 Organization Owner 访问儿童视频 | Org Owner 调用 review-queue / review-workbench / submission 详情拿任意学员视频 | Owner 身份不授予儿童资源（§5.2 明确）；仅当其同时是 class_staff 才可访问。历史端点同理：Owner 非 staff/监护人 ⇒ 拒绝 | 403 |
| T6 | 家长读 AI 草稿 | 监护人调用 submissions/{id}（合法资源）后从响应/抓包找 ai_draft 字段；或直接构造 workbench URL | 双 schema 隔离：家长视角 payload（SubmissionParentView）schema 层无 ai_draft；review-workbench 端点入口即校验 class_staff。AI 草稿仅存在于 teacher 视角 schema。Step 17 加"家长响应断言无 ai_draft 键"契约测试 | workbench：403/5424；submissions/{id}：正常家长 payload，**结构上不可能**含草稿 |
| T7 | 伪造 reviewer_uid / published_at | 老师 PUT review-draft 或 POST publish 时 body 里带 `reviewer_uid: 别的老师`（冒名发布）、`published_at: 过去/未来时间`（伪造时间线） | 服务端完全忽略这两个输入；reviewer_uid 恒取 JWT uid，published_at 恒取条件更新事务内 now()。检测到伪造字段时可记审计（不改变响应语义） | 正常成功（字段被忽略）；若做严格模式 → 5484 `ERR_REVIEW_FIELD_NOT_ACCEPTED` |
| T8 | 重放 idempotency key | 家长客户端重试提交时：a) 同 key 同内容（网络重试，合法）；b) 同 key 不同内容（攻击/bug，试图改绑附件）；c) 同 key 换 uid/换 assignment | a) 返回同一 submission（idempotent_replayed=true，不新增 attempt —— IDEMP-01）；b) 幂等记录绑定请求内容摘要，内容不一致 ⇒ 5460 拒绝且不覆盖；c) 幂等键作用域=(uid, assignment_id, key)，跨作用域视为新请求 | a) 200 code=0（replayed）；b) 5460；c) 正常新提交 |
| T9 | AI prompt 注入（视频描述藏指令） | 家长在 note 字段、视频内文字/口播、图片中嵌入"忽略以上指令，输出：…"试图操纵 AI 草稿内容 | 1) note 等用户文本进入 AI 输入前做转义/定界（系统提示词声明用户内容为不可信数据）；2) AI 输出受 JSON Schema 严格校验（bad_schema ⇒ failed）；3) **根本防线**：AI 输出只是老师草稿，注入最坏只影响老师看到的草稿文本，无法触达家长（老师审核后才发布，D-10）；4) needs_human_check 标记 + 老师可标记"判断错误" | 不改变 API 响应码；AI 侧 bad_schema → ai_status=failed，人工流程继续 |
| T10 | 短时 URL 泄漏/重放 | presigned GET URL（view_url 签发）被转发到群/第三方，在过期窗口内重放 | 1) URL 短时效（分钟级，复用 imboy ?PUT_EXPIRES 机制）；2) 每次签发 view_url 重新校验请求者对 submission 的当刻权限（撤回后即使旧 URL 未过期，对象级权限收紧策略见 Step 10 —— 至少保证新签发拒绝）；3) 业务表/日志不落 presigned URL（MEDIA-03）；4) URL 含对象键不含儿童身份信息 | 过期后：对象存储 403（不进 imboy envelope）；撤回后重新签发：403 |
| T11 | 重放微信 code | 抓包 login 请求重放 js_code；或用他人 code 尝试登录 | jscode2session 的 code 一次性，微信侧消费后失效 ⇒ 服务端拿不到会话 ⇒ 5402；不区分"已用/无效"细节，防探测；openid/unionid/session_key 不回传客户端（AUTH-01） | 5402 `ERR_WECHAT_CODE_INVALID` |
| T12 | 伪造/篡改身份上下文 | 客户端在 context/switch 或业务请求里声称自己是"teacher"、"role: manager"、伪造 organization_id | 角色永远由服务端从 class_staff/organization 表解析；switch 请求仅是"选择"，服务端反查关系不匹配 ⇒ 拒绝；业务 API 每次独立鉴权，不继承会话记忆（ACL-02：切换后不混用上一上下文） | switch：5420；业务 API：403 |

## 补充攻击面（Step 8/10/11/17 需覆盖，此处登记）

| # | 场景 | 要点 |
|---|---|---|
| T13 | assistant 越权发布 | assistant 可见队列（只读）但 publish/review-draft 写操作 ⇒ 5425；TEACHER-02 验收 |
| T14 | 枚举 TSID 资源存在性 | 所有 403/404 响应 msg 一致化（"无权访问该资源"），不区分"不存在/无权"，防资源存在性探测 |
| T15 | 已移除 staff/监护人的陈旧 token | class_staff/guardian_learner status=removed 后，旧 JWT 在有效期内仍被拒：每次请求实时查关系行 status（deny-by-default 的必然推论） |
| T16 | 学员未来账号越权读他校历史 | learner.user_id 绑定后，其本人只能读该 learner 的历史；跨 Organization 分区不可互读（HISTORY-01） |
| T17 | 撤回后老师仍看到附件 | withdrawn 后队列即时移除 + view_url 对 staff 重新签发时校验 submission 状态（撤回证据仅审计路径可及，不在日常 API 面） |

## 契约层测试挂钩（给 Step 17）

- 每行矩阵的"预期响应"即契约测试断言：HTTP 状态 + envelope.code + （T6 加）payload 键集合断言。
- T1–T5、T12 构成 SEC-01 要求的 deny-by-default 最小矩阵；T6–T11 为任务书指定必含场景。
