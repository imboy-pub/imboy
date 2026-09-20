%% moya_teaching_migration_tests
%% 墨芽习字 Step 5-7 迁移（00000095/96/97）SQL 结构断言测试。
%% 模式与 test/repo/message_dedup_migration_tests.erl 一致：纯文本断言，不连数据库。

-module(moya_teaching_migration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MIG_DIR, "priv/migrations/").
-define(STEP5_UP, ?MIG_DIR "00000095_organization_foundation.up.sql").
-define(STEP5_DOWN, ?MIG_DIR "00000095_organization_foundation.down.sql").
-define(STEP6_UP, ?MIG_DIR "00000096_teaching_identity.up.sql").
-define(STEP6_DOWN, ?MIG_DIR "00000096_teaching_identity.down.sql").
-define(STEP7_UP, ?MIG_DIR "00000097_homework_review_loop.up.sql").
-define(STEP7_DOWN, ?MIG_DIR "00000097_homework_review_loop.down.sql").
-define(STEP98_UP, ?MIG_DIR "00000098_submission_idempotency_withdraw.up.sql").
-define(STEP98_DOWN, ?MIG_DIR "00000098_submission_idempotency_withdraw.down.sql").
-define(STEP99_UP, ?MIG_DIR "00000099_teaching_admin_audit.up.sql").
-define(STEP99_DOWN, ?MIG_DIR "00000099_teaching_admin_audit.down.sql").
-define(STEP100_UP, ?MIG_DIR "00000100_teaching_sentinel_unify.up.sql").
-define(STEP100_DOWN, ?MIG_DIR "00000100_teaching_sentinel_unify.down.sql").
-define(STEP105_UP, ?MIG_DIR "00000105_teaching_review_asset.up.sql").
-define(STEP105_DOWN, ?MIG_DIR "00000105_teaching_review_asset.down.sql").

read(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

%% ===================================================================
%% Step 5: organization + workspace.organization_id
%% ===================================================================

step5_organization_table_test() ->
    Up = read(?STEP5_UP),
    % 表与核心列（计划 §6.1）
    ?assertNotEqual(nomatch, binary:match(Up, <<"CREATE TABLE IF NOT EXISTS organization">>)),
    lists:foreach(
        fun(Col) -> ?assertNotEqual(nomatch, binary:match(Up, Col)) end,
        [
            <<"id          bigint">>,
            <<"name        character varying(200)">>,
            <<"owner_id    bigint">>,
            <<"status      text                         DEFAULT 'active'">>,
            <<"branding    jsonb">>,
            <<"settings    jsonb">>,
            <<"created_at  timestamp with time zone">>,
            <<"updated_at  timestamp with time zone">>
        ]
    ),
    % status CHECK + FK
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"ck_organization_status CHECK (status = ANY (ARRAY['active'::text, 'archived'::text]))">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"fk_organization_owner">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"REFERENCES \"user\"(id) ON DELETE CASCADE">>)).

step5_workspace_nullable_org_test() ->
    Up = read(?STEP5_UP),
    % expand-first：可空列（无 NOT NULL）、不回填
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"ADD COLUMN IF NOT EXISTS organization_id bigint">>)
    ),
    ?assertEqual(nomatch, binary:match(Up, <<"organization_id bigint NOT NULL">>)),
    ?assertEqual(nomatch, binary:match(Up, <<"UPDATE workspace">>)),
    % fail-closed：RESTRICT（非 CASCADE）
    ?assertNotEqual(nomatch, binary:match(Up, <<"fk_workspace_organization">>)),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"REFERENCES organization(id) ON DELETE RESTRICT">>)
    ),
    ?assertEqual(nomatch, binary:match(Up, <<"REFERENCES organization(id) ON DELETE CASCADE">>)).

step5_down_completeness_test() ->
    Down = read(?STEP5_DOWN),
    lists:foreach(
        fun(Frag) -> ?assertNotEqual(nomatch, binary:match(Down, Frag)) end,
        [
            <<"DROP CONSTRAINT IF EXISTS fk_workspace_organization">>,
            <<"DROP COLUMN IF EXISTS organization_id">>,
            <<"DROP TABLE IF EXISTS organization">>
        ]
    ).

%% ===================================================================
%% Step 6: 教学身份五表 + 机构一致性
%% ===================================================================

step6_five_tables_test() ->
    Up = read(?STEP6_UP),
    lists:foreach(
        fun(Table) ->
            ?assertNotEqual(
                nomatch, binary:match(Up, <<"CREATE TABLE IF NOT EXISTS ", Table/binary>>)
            )
        end,
        [
            <<"class_profile">>,
            <<"class_staff">>,
            <<"learner">>,
            <<"class_enrollment">>,
            <<"guardian_learner">>
        ]
    ).

step6_learner_partial_unique_test() ->
    Up = read(?STEP6_UP),
    % 同机构 user 唯一仅对非空 user_id 生效（DB-BIND-01：跨机构可重复绑定）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"uk_learner_org_user">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"WHERE user_id IS NOT NULL">>
        )
    ).

step6_org_consistency_trigger_test() ->
    Up = read(?STEP6_UP),
    % 可延迟约束触发器（DB-LEARNER-01）+ learner 机构防篡改
    ?assertNotEqual(nomatch, binary:match(Up, <<"fn_class_enrollment_org_check">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"DEFERRABLE INITIALLY DEFERRED">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"JOIN workspace w ON w.id = g.workspace_id">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"trg_learner_org_change_guard">>)),
    % down 完整清理触发器与函数
    Down = read(?STEP6_DOWN),
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"DROP FUNCTION IF EXISTS fn_class_enrollment_org_check()">>)
    ),
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"DROP FUNCTION IF EXISTS fn_learner_org_change_guard()">>)
    ).

step6_check_constraints_test() ->
    Up = read(?STEP6_UP),
    lists:foreach(
        fun(Frag) -> ?assertNotEqual(nomatch, binary:match(Up, Frag)) end,
        [
            <<"ck_class_profile_course_type">>,
            <<"ck_class_staff_role">>,
            <<"ck_guardian_learner_relation">>,
            <<"can_submit">>,
            <<"can_view_review">>
        ]
    ).

%% ===================================================================
%% Step 7: assignment 扩展 + 回课四表
%% ===================================================================

step7_partial_unique_contract_test() ->
    Up = read(?STEP7_UP),
    % 全量唯一约束 -> 同名部分唯一索引（错误契约保留，DB-COMPAT-01/DB-ASSIGN-01）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"DROP CONSTRAINT IF EXISTS group_task_assignment_task_id_user_id_key">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"CREATE UNIQUE INDEX IF NOT EXISTS group_task_assignment_task_id_user_id_key">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"WHERE learner_id IS NULL">>)),
    % 教学作业分支
    ?assertNotEqual(nomatch, binary:match(Up, <<"uk_group_task_assignment_task_learner">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"WHERE learner_id IS NOT NULL">>)).

step7_assignment_columns_test() ->
    Up = read(?STEP7_UP),
    lists:foreach(
        fun(Frag) -> ?assertNotEqual(nomatch, binary:match(Up, Frag)) end,
        [
            <<"ADD COLUMN IF NOT EXISTS learner_id bigint">>,
            <<"ADD COLUMN IF NOT EXISTS submitted_by bigint">>,
            <<"fn_gta_learner_org_check">>
        ]
    ).

step7_submission_tables_test() ->
    Up = read(?STEP7_UP),
    lists:foreach(
        fun(Table) ->
            ?assertNotEqual(
                nomatch, binary:match(Up, <<"CREATE TABLE IF NOT EXISTS ", Table/binary>>)
            )
        end,
        [
            <<"homework_submission">>,
            <<"submission_asset">>,
            <<"calligraphy_review_draft">>,
            <<"teacher_review">>
        ]
    ),
    % attempt 唯一 + 正数（DB-SUBMIT-01）
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"uk_homework_submission_attempt UNIQUE (assignment_id, attempt_no)">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"CHECK (attempt_no > 0)">>)),
    % 附件唯一 + kind CHECK
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"uk_submission_asset_attachment UNIQUE (submission_id, attachment_id)">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"'practice_video'::text, 'final_photo'::text">>)).

step7_review_publish_constraints_test() ->
    Up = read(?STEP7_UP),
    % 发布完整性 CHECK（DB-REVIEW-01）：reviewer/published_at/至少一种内容
    ?assertNotEqual(nomatch, binary:match(Up, <<"ck_teacher_review_published">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"reviewer_uid IS NOT NULL">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"published_at IS NOT NULL">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"video_attachment_id IS NOT NULL">>)),
    % 单已发布回评 + 单有效 AI 草稿
    ?assertNotEqual(nomatch, binary:match(Up, <<"uk_tr_published_per_submission">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"WHERE status = 'published'">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"uk_crd_active_per_submission">>)),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"WHERE status IN ('queued', 'running', 'succeeded')">>)
    ),
    % AI 完成时间 CHECK
    ?assertNotEqual(nomatch, binary:match(Up, <<"ck_crd_completed">>)).

step7_down_restores_original_constraint_test() ->
    Down = read(?STEP7_DOWN),
    % down 恢复 00000001 原全量唯一约束，且有数据保护预检（不静默删数据）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"ADD CONSTRAINT group_task_assignment_task_id_user_id_key UNIQUE (task_id, user_id)">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Down, <<"GROUP BY task_id, user_id">>)),
    ?assertNotEqual(nomatch, binary:match(Down, <<"HAVING count(*) > 1">>)),
    ?assertEqual(nomatch, binary:match(Down, <<"DELETE FROM group_task_assignment">>)).

%% ===================================================================
%% R2 修复迁移 00000098: 幂等键 + 撤回审计 + 互斥 backstop
%% ===================================================================

step98_idempotency_columns_test() ->
    Up = read(?STEP98_UP),
    lists:foreach(
        fun(Frag) -> ?assertNotEqual(nomatch, binary:match(Up, Frag)) end,
        [
            <<"ADD COLUMN IF NOT EXISTS idempotency_key">>,
            <<"ADD COLUMN IF NOT EXISTS request_digest">>,
            <<"ADD COLUMN IF NOT EXISTS withdrawn_at">>,
            <<"ADD COLUMN IF NOT EXISTS withdrawn_by">>
        ]
    ).

step98_idempotency_partial_unique_test() ->
    Up = read(?STEP98_UP),
    % 有效唯一约束：同 submitted_by+assignment+idempotency_key 仅键非空时生效
    ?assertNotEqual(nomatch, binary:match(Up, <<"uk_homework_submission_idempotency">>)),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"ON homework_submission USING btree (submitted_by, assignment_id, idempotency_key)">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"WHERE idempotency_key IS NOT NULL">>)).

step98_withdraw_audit_check_test() ->
    Up = read(?STEP98_UP),
    % 撤回审计 CHECK：submitted 态两列空 / withdrawn 态全非空含 submitted_at
    ?assertNotEqual(nomatch, binary:match(Up, <<"ck_homework_submission_withdraw_audit">>)),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"(status = 'submitted' AND withdrawn_at IS NULL AND withdrawn_by IS NULL)">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"(status = 'withdrawn' AND withdrawn_at IS NOT NULL AND withdrawn_by IS NOT NULL">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"AND submitted_at IS NOT NULL)">>)),
    % withdrawn_by 注销语义与 learner.user_id 一致（SET NULL）
    ?assertNotEqual(nomatch, binary:match(Up, <<"fk_hs_withdrawn_by">>)),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"FOREIGN KEY (withdrawn_by) REFERENCES \"user\"(id) ON DELETE SET NULL">>
        )
    ).

step98_mutex_backstop_triggers_test() ->
    Up = read(?STEP98_UP),
    % 撤回/发布双向互斥 backstop（READ COMMITTED 竞态实测修复）
    ?assertNotEqual(nomatch, binary:match(Up, <<"fn_homework_submission_withdraw_guard">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"trg_homework_submission_withdraw_guard">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"fn_teacher_review_publish_guard">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"trg_teacher_review_publish_guard">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"DEFERRABLE INITIALLY IMMEDIATE">>)),
    % down 完整清理
    Down = read(?STEP98_DOWN),
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"DROP FUNCTION IF EXISTS fn_teacher_review_publish_guard()">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Down, <<"DROP FUNCTION IF EXISTS fn_homework_submission_withdraw_guard()">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Down, <<"DROP COLUMN IF EXISTS idempotency_key">>)),
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"DROP INDEX IF EXISTS uk_homework_submission_idempotency">>)
    ).

%% ===================================================================
%% 收官 00000099: teaching_admin_audit 审计表（Step 16 handoff #2）
%% ===================================================================

step99_audit_table_test() ->
    Up = read(?STEP99_UP),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"CREATE TABLE IF NOT EXISTS teaching_admin_audit">>)
    ),
    % action 枚举 CHECK（bind/unbind + §9.3 治理动作）
    ?assertNotEqual(nomatch, binary:match(Up, <<"ck_teaching_admin_audit_action">>)),
    lists:foreach(
        fun(A) -> ?assertNotEqual(nomatch, binary:match(Up, A)) end,
        [
            <<"'bind_learner'::text">>,
            <<"'unbind_learner'::text">>,
            <<"'learner_archive'::text">>,
            <<"'data_export'::text">>,
            <<"'data_delete'::text">>
        ]
    ),
    % 索引：learner 维度 + operator 维度
    ?assertNotEqual(nomatch, binary:match(Up, <<"i_teaching_admin_audit_learner">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"i_teaching_admin_audit_operator">>)).

step99_sentinel_no_fk_test() ->
    Up = read(?STEP99_UP),
    % sentinel uid 0 匿名化语义写入注释（禁 NULL 抹操作人）。
    % 注意：binary 字面量中文字符按 latin1 截断（码点 band 255），
    % 中文断言必须带 /utf8 修饰符（R6 修复：截断字节匹配不到 SQL 文件真实 UTF-8 序列）。
    ?assertNotEqual(nomatch, binary:match(Up, <<"0=sentinel">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"禁置NULL"/utf8>>)),
    % C 决策：裸列无 FK（审计先于实体存活；预插 uid=0 会触发账号触发器）
    ?assertEqual(nomatch, binary:match(Up, <<"FOREIGN KEY">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"sync_fts_user()">>)),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"ck_teaching_admin_audit_uids CHECK (operator_uid >= 0)">>)
    ),
    % down 整表清理（含审计删除告警注释）
    Down = read(?STEP99_DOWN),
    ?assertNotEqual(nomatch, binary:match(Down, <<"DROP TABLE IF EXISTS teaching_admin_audit">>)),
    ?assertNotEqual(nomatch, binary:match(Down, <<"物理删除审计历史"/utf8>>)).

%% ===================================================================
%% R8 00000100: sentinel 统一（withdrawn_by / reviewer_uid 由 FK SET NULL 改裸列+CHECK）
%% ===================================================================

step100_up_drops_fk_adds_sentinel_check_test() ->
    Up = read(?STEP100_UP),
    % 摘除 97/98 两个 SET NULL FK（幂等 IF EXISTS）
    ?assertNotEqual(nomatch, binary:match(Up, <<"DROP CONSTRAINT IF EXISTS fk_hs_withdrawn_by">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"DROP CONSTRAINT IF EXISTS fk_tr_reviewer">>)),
    % up 不新增任何 FK（裸列语义）
    ?assertEqual(nomatch, binary:match(Up, <<"FOREIGN KEY">>)),
    % sentinel CHECK（99 同款：NULL 合法 / >=0 / 0=sentinel）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"ck_homework_submission_withdrawn_by_sentinel">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"CHECK (withdrawn_by IS NULL OR withdrawn_by >= 0)">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"ck_teacher_review_reviewer_uid_sentinel">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"CHECK (reviewer_uid IS NULL OR reviewer_uid >= 0)">>
        )
    ),
    % 注释对齐 sentinel 语义（中文断言 /utf8 修饰——R6 教训）
    ?assertNotEqual(nomatch, binary:match(Up, <<"禁置NULL"/utf8>>)).

step100_down_failfast_guard_test() ->
    Down = read(?STEP100_DOWN),
    % 预检 fail-fast：存在 sentinel 0 行拒绝回滚（审计痕迹不可逆抹除；
    % 候选 b UPDATE 0→NULL 会撞 98/97 的 withdrawn_audit/published CHECK，链走不通）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"homework_submission WHERE withdrawn_by = 0">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"teacher_review WHERE reviewer_uid = 0">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Down, <<"RAISE EXCEPTION">>)),
    ?assertNotEqual(nomatch, binary:match(Down, <<"审计痕迹不可逆抹除"/utf8>>)),
    % 恢复 97/98 原语义（同名 FK 重建）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"ADD CONSTRAINT fk_hs_withdrawn_by">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"FOREIGN KEY (withdrawn_by) REFERENCES \"user\"(id) ON DELETE SET NULL">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"ADD CONSTRAINT fk_tr_reviewer">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"FOREIGN KEY (reviewer_uid) REFERENCES \"user\"(id) ON DELETE SET NULL">>
        )
    ),
    % down 摘除 sentinel CHECK
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"DROP CONSTRAINT IF EXISTS ck_homework_submission_withdrawn_by_sentinel">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Down,
            <<"DROP CONSTRAINT IF EXISTS ck_teacher_review_reviewer_uid_sentinel">>
        )
    ).

step100_no_touch_history_test() ->
    % 历史迁移铁律：100 只换约束，不改写历史列（无 DROP COLUMN / ALTER COLUMN / 数据 UPDATE）
    Up = read(?STEP100_UP),
    ?assertEqual(nomatch, binary:match(Up, <<"DROP COLUMN">>)),
    ?assertEqual(nomatch, binary:match(Up, <<"ALTER COLUMN">>)),
    ?assertEqual(nomatch, binary:match(Up, <<"UPDATE ">>)).

%% ===================================================================
%% P0-4 回评媒体 00000105: review_asset（MN-MEDIA-01）
%% ===================================================================

step105_review_asset_table_test() ->
    Up = read(?STEP105_UP),
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"CREATE TABLE IF NOT EXISTS review_asset">>)
    ),
    % 最小字段集（计划 §P0-4：id/review_id/attachment_id/kind/sort_order/created_by/created_at）
    lists:foreach(
        fun(Frag) -> ?assertNotEqual(nomatch, binary:match(Up, Frag)) end,
        [
            <<"review_id     bigint                       NOT NULL">>,
            <<"attachment_id bigint                       NOT NULL">>,
            <<"sort_order    integer                      DEFAULT 0 NOT NULL">>,
            <<"created_by    bigint                       NOT NULL">>,
            <<"created_at    timestamp with time zone     DEFAULT CURRENT_TIMESTAMP NOT NULL">>
        ]
    ),
    % kind 枚举 CHECK（仅 feedback_image|feedback_video）
    ?assertNotEqual(nomatch, binary:match(Up, <<"ck_review_asset_kind">>)),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up,
            <<"CHECK (kind = ANY (ARRAY['feedback_image'::text, 'feedback_video'::text]))">>
        )
    ),
    % FK：review CASCADE（连带解除）；attachment RESTRICT（fail-closed 不静默删附件）
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up, <<"FOREIGN KEY (review_id) REFERENCES teacher_review(id) ON DELETE CASCADE">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Up, <<"FOREIGN KEY (attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT">>
        )
    ),
    % created_by sentinel 语义（100 同款：裸列 + CHECK >= 0，禁 FK 抹审计）
    ?assertNotEqual(nomatch, binary:match(Up, <<"ck_review_asset_created_by_sentinel">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"CHECK (created_by >= 0)">>)).

step105_attachment_uniqueness_test() ->
    Up = read(?STEP105_UP),
    % attachment_id 全表 UNIQUE（一对一：防同一附件绑多个 review）
    ?assertNotEqual(
        nomatch, binary:match(Up, <<"uk_review_asset_attachment UNIQUE (attachment_id)">>)
    ),
    % 单 review 至多 1 视频：部分唯一索引
    ?assertNotEqual(nomatch, binary:match(Up, <<"uk_review_asset_one_video_per_review">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"WHERE kind = 'feedback_video'">>)).

step105_image_cap_trigger_test() ->
    Up = read(?STEP105_UP),
    % 3 图上限：CONSTRAINT TRIGGER 语句末新鲜快照计数（98 withdraw_guard 风格）
    ?assertNotEqual(nomatch, binary:match(Up, <<"fn_review_asset_image_cap">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"trg_review_asset_image_cap">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"IF v_count > 3 THEN">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"DEFERRABLE INITIALLY IMMEDIATE">>)).

step105_backfill_idempotent_test() ->
    Up = read(?STEP105_UP),
    % 回填：video_attachment_id → feedback_video（sort_order=0，created_by=COALESCE(reviewer_uid,0)）
    ?assertNotEqual(nomatch, binary:match(Up, <<"'feedback_video'">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"COALESCE(tr.reviewer_uid, 0)">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"ON CONFLICT (attachment_id) DO NOTHING">>)),
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"att.id = tr.video_attachment_id AND att.status >= 0">>)
    ),
    % up 幂等：全 IF NOT EXISTS / IF EXISTS；无 BEGIN;/COMMIT; 事务语句
    % （头注释含"禁止 BEGIN/COMMIT"说明文字，故断言带分号的语句形态）
    ?assertEqual(nomatch, binary:match(Up, <<"BEGIN;">>)),
    ?assertEqual(nomatch, binary:match(Up, <<"COMMIT;">>)).

step105_down_fail_closed_test() ->
    Down = read(?STEP105_DOWN),
    % fail-closed 预检：feedback_image 或单 review 多视频 → RAISE 拒绝（不可静默丢数据）
    ?assertNotEqual(nomatch, binary:match(Down, <<"RAISE EXCEPTION">>)),
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"WHERE kind = 'feedback_image'">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Down, <<"HAVING count(*) > 1">>)),
    ?assertNotEqual(nomatch, binary:match(Down, <<"旧单视频列无法表示"/utf8>>)),
    % 清理顺序：触发器 → 函数 → 表；旧列 video_attachment_id 不动（兼容读窗口保留）
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"DROP TRIGGER IF EXISTS trg_review_asset_image_cap">>)
    ),
    ?assertNotEqual(
        nomatch, binary:match(Down, <<"DROP FUNCTION IF EXISTS fn_review_asset_image_cap()">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Down, <<"DROP TABLE IF EXISTS review_asset">>)),
    ?assertEqual(nomatch, binary:match(Down, <<"DROP COLUMN">>)),
    % 旧列 video_attachment_id（00000097 资产）保留：down 不删该列
    ?assertEqual(
        nomatch, binary:match(Down, <<"DROP COLUMN IF EXISTS video_attachment_id">>)
    ).
