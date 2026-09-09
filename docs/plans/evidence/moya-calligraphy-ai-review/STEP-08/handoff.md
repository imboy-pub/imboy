# STEP-08 交接 — Agent B（API-ACL）

```
AGENT: B
STEP: 8
STATUS: PASS
BASE_SHA: imboy=5b7e2055 moya=9644a2e（全程未执行 git 命令；error_code.hrl 的 staged M 为已登记的外部污染，内容以工作区为准）
OWNED_PATHS: STEP-08/**；src/{api,logic,repo}/*teaching*（新文件）；src/imboy_router.erl 教学路由段；include/error_code.hrl 教学错误码段；test/logic/teaching_*_tests.erl
FILES_CHANGED:
  include/error_code.hrl（M：教学段 5401-5485 + ERROR_MSG_MAP）
  src/repo/teaching_context_repo.erl（新）
  src/logic/teaching_acl.erl（新：resolve_guardian/resolve_staff/resolve_org_owner/submission_access）
  src/logic/teaching_auth_logic.erl（新：微信登录）
  src/logic/teaching_wechat_client.erl（新：jscode2session 可 meck 薄封装）
  src/logic/teaching_context_logic.erl（新：contexts/switch，TSID 字符串输出）
  src/api/teaching_auth_handler.erl（新）
  src/api/teaching_context_handler.erl（新）
  src/imboy_router.erl（M：+3 路由，login 进 open/0）
  test/logic/teaching_auth_logic_tests.erl（新，8 用例）
  test/logic/teaching_acl_tests.erl（新，13 用例）
  STEP-08/{commands,tests,notes,schema-gap}.md、behavior-acl.sql
COMMANDS_RUN: make compile（EXIT=0）；erlc 两测试文件（EXIT=0）；
  eunit [teaching_acl_tests] verbose=13/13 ok；eunit [teaching_auth_logic_tests,teaching_acl_tests]=ok；
  eunit [auth_middleware_tests,billing_route_tests,channel_webhook_handler_tests,feature_route_http_tests,auth_ds_tests]=ok；
  psql moya_mig_test@4323 -f behavior-acl.sql（PSQL_EXIT=0，BEGIN...ROLLBACK）
TEST_RESULTS: AUTH-01 8/8（重放/无效/未配置/未绑定/网络/成功无 openid 泄漏/参数边界）；
  ACL-01 8/8（跨Org/跨learner/can_view=false/仅群管理员/仅Owner 全拒 + 合法 staff/guardian 放行 + not_found）；
  ACL-02 5/5（双身份 contexts、switch 归属、自报机构拒绝、不串资源、assistant 只读）；
  真库：repo 全部 SQL 在 00000001→98 schema 上 9 组查询通过（误连 5432 曾致误判，已纠正）
ACCEPTANCE_RESULTS: AUTH-01=PASS ACL-01=PASS ACL-02=PASS
EVIDENCE_PATH: /Users/leeyi/project/imboy.pub/imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-08/
KNOWN_RISKS: Handler 层无独立 eunit（薄适配，HTTP 契约验证留 Step 17）；token 未绑设备（legacy did，Step 13 启用）；微信绑定入口（sso_identity_ds:bind）未开新端点（机构管理域）
DEPENDENCIES: 给 Step 9：teaching_acl:submission_access/2 返回资源链+staff/guardian 视角；resolve_guardian(_,_,submit)=提交守卫；assignment_scope/1 就绪；错误码 atom→宏映射模式；task_status 语义按既有 group_task（1=进行中）
CONFLICTS: 无（STEP-08-DB/ 为 Agent C 独立目录，未触碰）
NEXT_ACTION: R3 继续 Step 9（本波同一指令内连续执行）
EXTERNAL_GATES: 无（未 git 写、未改迁移、未碰 9800/9801、未读 .env、未配真实凭据；曾误建 moya_s8_test 库已 DROP）
```
