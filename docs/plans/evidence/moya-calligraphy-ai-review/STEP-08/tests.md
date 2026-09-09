# STEP-08 证据 — 测试结果明细

## EUnit（meck，无真实网络/密钥）

### teaching_auth_logic_tests — AUTH-01（8/8 PASS）

| 用例 | 断言 | 结果 |
|---|---|---|
| provider_unconfigured | appid/secret 为空 → `{error, provider_unconfigured}`（5403 路径真实可测） | PASS |
| code_invalid | 微信侧 errcode → `{error, code_invalid}`（5402） | PASS |
| code_replay | 同 code 二次调用（mock 微信侧已消费）→ `{error, code_invalid}`（T11 折叠语义） | PASS |
| network_error | httpc 失败 → `{error, login_failed}`（5401） | PASS |
| identity_none | sso_identity 无映射 → `{error, identity_none}`（5404） | PASS |
| login_success_no_openid_leak | 成功 payload = {token, expires_in, refresh_token, has_teaching_identity}，键集合断言**无 openid/session_key/unionid** | PASS |
| missing_code / short_code | 缺 code / 长度<5 → missing_code / invalid_code | PASS |

### teaching_acl_tests — ACL-01 + ACL-02（13/13 PASS）

ACL-01 矩阵（submission_access/2 = 儿童提交视频访问守卫）：

| 攻击 | 主体→资源 | 预期 | 结果 |
|---|---|---|---|
| T1 跨 Organization | A 机构老师 → B 机构提交 | `{error, forbidden}` | PASS |
| T3 跨 learner | L2 监护人 → L1 提交 | `{error, forbidden}` | PASS |
| T3 变体 can_view_review=false | L2 监护人 → L2 自己提交 | `{error, forbidden}` | PASS |
| T4 仅 Group 管理员 | 群管理员（无 class_staff）→ 提交 | `{error, forbidden}` | PASS |
| T5 仅 Org Owner | 机构 Owner（非 staff/guardian）→ 提交 | `{error, owner_not_granted}` | PASS |
| 合法 staff | 本班老师 → 提交 | `{ok, staff, _}` | PASS |
| 合法 guardian | can_view_review 监护人 → 提交 | `{ok, guardian, _}` | PASS |
| T14 不可见资源 | 老师 → 不存在 submission | `{error, not_found}` | PASS |

ACL-02（多身份不串上下文）：

| 用例 | 断言 | 结果 |
|---|---|---|
| contexts 双身份 | guardian+teacher 并存；**全部 TSID 字段为 binary 字符串**（learner_id=<<"3001">>） | PASS |
| switch 归属 | 本人 G2 teacher→ok；非本人 G1 teacher→`context_mismatch`；本人 guardian→ok | PASS |
| T12 自报机构 | switch 带伪造 organization_id=200（实际 100）→ `context_mismatch` | PASS |
| 不串资源 | G2 老师（MULTI_U）访问 G1 提交走 guardian 路径，不获 staff 视角 | PASS |
| assistant 只读 | resolve_staff 基础 ok；`write` / roles 白名单 → `role_denied`（TEACHER-02 前置） | PASS |

### 波及既有套件（路由/认证改动回归）

auth_middleware_tests、billing_route_tests、channel_webhook_handler_tests、
feature_route_http_tests、auth_ds_tests → **全部 PASS**（RESULT: ok）。

## 真库 SQL 验证（moya_mig_test@4323，BEGIN...ROLLBACK）

behavior-acl.sql 种子：2 机构 / 2 workspace / 3 班 / 3 学员 / 2 作业 / 2 提交 /
2 任教关系 / 2 监护关系 / 1 群管理员（含 workspace_member 子集约束前置）。
`SET CONSTRAINTS ALL IMMEDIATE` 三处触发器检查点全部通过。

关键输出（PSQL_EXIT=0）：

- Q1 submission_scope(987001) → `(987001, 986001, 984001, submitted, 1, task98_hash_001, 984001, 983001, 981000)` —— 五跳 JOIN 链与 deferred 触发器下数据一致
- Q2 guardian_contexts(980006) → 2 行（L1 同时入 A1/A2 两班，跨班连续）
- Q3 staff_contexts(980006) → manager @ A2（org 981000）
- Q4 owner_contexts(980004) → 机构A
- Q5 群管理员 class_staff 行数 = 0（T4 的 DB 层事实）
- Q9 群管理员 guardian_learner 行数 = 0（T3/T4 的 DB 层事实）

## 覆盖说明

- Handler 为薄适配层（参数解析+错误码映射），未单独 eunit；契约级 HTTP 行为留给
  Step 17 E2E。
- teaching_context_repo 的 SQL 经真库验证；Logic 经 meck 单测；两层叠加等价于
  ACL-01/ACL-02 的本地集成验证（真 Erlang+真 PG 的完整 HTTP 链路需起 Cowboy+JWT，
  属 Step 17 范围）。
