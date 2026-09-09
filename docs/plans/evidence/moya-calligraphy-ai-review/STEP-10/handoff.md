# STEP-10 交接 — Agent B（API-ACL）

```
AGENT: B
STEP: 10
STATUS: PASS（MEDIA-01/02/03 全过；2 项标注遗留：ecron 接线一行、POST 直传端点待真机决策）
BASE_SHA: imboy=5b7e2055（未 git 操作）
OWNED_PATHS: STEP-10/**；src/logic/teaching_attach_logic.erl（新）；src/logic/attach_logic.erl
  （教学钩子 4 处 + confirm payload，最小侵入）；src/repo/teaching_submission_repo.erl（教学函数）；
  test/logic/teaching_attach_logic_tests.erl、test/repo/teaching_attach_integration_tests.erl
FILES_CHANGED:
  src/logic/teaching_attach_logic.erl（新：can_upload/check_mime/verify_upload/authorize/
    list_unbound/cleanup_unbound，上限三配置项）
  src/logic/attach_logic.erl（M：presign 教学预检拆分、verify_and_save 教学复核钩子、
    can_upload+authorize teaching 子句、confirm_payload 补 attachment_id）
  src/repo/teaching_submission_repo.erl（M：submission_for_asset_path(/_tx)、
    unbound_teaching_attachments(/_tx)）
  test ×2（新）、STEP-10 证据 ×4
COMMANDS_RUN: make compile（COMPILE_EXIT=0 零告警）；erlc 两测试（EXIT=0）；
  eunit [teaching_attach_logic_tests] no_tty = UNIT: ok（19 用例）；
  eunit [teaching_attach_integration_tests] verbose = All 3 tests passed（真库 BEGIN/ROLLBACK）；
  回归 eunit [attach_logic_tests, attach_pending_cleanup_tests, teaching 全部 5 套件,
  auth_middleware, billing_route, auth_ds]：除 attach_logic_tests 36/37（唯一失败=
  moment_ds 编译期裁剪的既有环境失败 {undefined_module,moment_ds}，非本 Step 引入）
  其余全部 ok
TEST_RESULTS: MEDIA-01=PASS（meck 矩阵 9 用例：合法 guardian/staff 放行；未授权家长/
  同班成员/非任课老师/仅 Owner/未绑定/已撤回 全拒 + 真库路径解析 4 断言）；
  MEDIA-02=PASS（meck 3 用例：孤儿删、对象删失败保行、空；真库 5 附件夹具恰好只列
  超龄未绑定 teaching——绑定/撤回证据/新近/他 scope 不误删）；
  MEDIA-03=PASS（源码扫描：教学 repo 零 presign 调用、attach 落库 url==path==ObjectKey；
  真库行级：5 行无 X-Amz/Expires/Signature、submission_asset 无 URL 列）
ACCEPTANCE_RESULTS: MEDIA-01=PASS MEDIA-02=PASS MEDIA-03=PASS
EVIDENCE_PATH: /Users/leeyi/project/imboy.pub/imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-10/
KNOWN_RISKS: cleanup_unbound 未挂 ecron（接线=attach_cleanup_logic 同款条目一行，
  建议 config teaching_unbound_cleanup_age_hours）；duration 为客户端上报（未上报放行，
  服务端抽帧复核 Step 11）；POST 直传端点首版不实现（待 moya 真机 ArrayBuffer PUT 结果，
  已按任务书在 notes.md 标注）；attach_logic_tests moment 用例既有环境失败（非本 Step）
DEPENDENCIES: moya 联调五 gap 全收口（notes.md 有对照表）：①confirm 已回 attachment_id
  （字符串，object_key 保留）②scope 定名 teaching ③POST 直传待真机决策 ④hash 空串接受
  ⑤view_url TTL=600s（moya 4 分钟缓存安全）；Step 11 复用 enqueue_ai_draft_tx 与
  verify_upload 的时长配置项（抽帧后服务端强校验）
CONFLICTS: 无（attach_logic.erl 为本 Step 任务书明确指派的修改点，4 钩子最小侵入，
  既有 scope 行为零变化——36/37 既有用例回归佐证）
NEXT_ACTION: 建议 Coordinator 放行 Step 11（AI Worker）；ecron 接线可随 Step 11 一并处理
EXTERNAL_GATES: 无（未 git 写、未改迁移、未碰业务库/9800/9801/.env/真实凭据；
  未配置真实对象存储凭据——OSS 操作仅经既有 elib_oss 抽象，测试全 meck）
```
