# STEP-09 交接 — Agent B（API-ACL）

```
AGENT: B
STEP: 9
STATUS: PASS（主链全绿；4 项低风险 PARTIAL 见 notes.md「PARTIAL 项」）
BASE_SHA: imboy=5b7e2055 moya=9644a2e（未 git 操作）
OWNED_PATHS: STEP-09/**；src/{api,logic,repo}/*teaching*（新文件）；imboy_router.erl 教学路由段；test/repo/teaching_flow_integration_tests.erl
FILES_CHANGED:
  src/repo/teaching_submission_repo.erl（新）
  src/repo/teaching_review_repo.erl（新）
  src/logic/teaching_assignment_logic.erl（新）
  src/logic/teaching_review_logic.erl（新）
  src/api/teaching_error.erl（新）
  src/api/teaching_assignment_handler.erl（新）
  src/api/teaching_review_handler.erl（新）
  src/imboy_router.erl（M：+10 教学路由）
  test/repo/teaching_flow_integration_tests.erl（新）
  STEP-09/{commands,tests,notes}.md
COMMANDS_RUN: make compile（EXIT=0）；erlc 集成测试（EXIT=0）；
  eunit [teaching_flow_integration_tests] verbose = All 6 tests passed；
  eunit [全部教学 + 波及既有 6 套件] no_tty = ALL_RESULT: ok；
  scratch 残留检查 2×count=0
TEST_RESULTS: FLOW-01=PASS（提交→AI queued 占位→草稿→lock-first 发布→家长读唯一
  published→发布后撤回互斥拒绝）；IDEMP-01=PASS（同 key 同 digest 同 submission；
  异 digest=5460；异 key=attempt2；attachment 零重复；attempt 序列 [1,2]）；
  STATE-01=PASS×4（无草稿 5480 / 重复发布幂等 already_published / 撤回后发布 5482+
  重复撤回 5444+withdrawn_by 审计 / AI failed 不阻断人工发布 D-10）
ACCEPTANCE_RESULTS: FLOW-01=PASS IDEMP-01=PASS STATE-01=PASS
EVIDENCE_PATH: /Users/leeyi/project/imboy.pub/imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-09/
KNOWN_RISKS: Handler 层无 Cowboy 级单测（Step 17）；assignments/:id 详情为概要版；
  SubmissionCreated 未回填 submitted_at/learner_id；queue ai_status=failed 筛选未单测；
  TSID 用默认生成器（多节点需确认 default 注册于 imboy_app init——已确认 names 列表含
  default 路径，agent_payment_mandate_repo 同款先例）
DEPENDENCIES: Step 10（view_url 挂 submission_access + assets_tx.path）；Step 11
  （enqueue_ai_draft_tx 入队点 + uk_crd 防重）；Step 13（双通道错误判定/TSID string/
  Idempotency-Key UUID）；Step 16（history 已 org 内跨 workspace）
CONFLICTS: 无
NEXT_ACTION: 建议 Coordinator 放行 Step 10（附件授权）与 Step 11（AI Worker）并行
EXTERNAL_GATES: 无（未 git 写、未改迁移、未碰业务库/9800/9801/.env/真实凭据）
```
