-- 00000224_retire_moya.up.sql
-- 墨芽已迁出独立产品；本迁移仅收缩空结构，不迁移或删除业务数据。
-- 由 erlang_migrate 在单个事务中执行。非空、未知依赖或锁超时均整笔失败。
-- 应先部署不再提供旧教学接口/任务的代码，停用旧节点与旧配置后再由管理员升级。
SET LOCAL lock_timeout = '5s';
SET LOCAL row_security = off;

LOCK TABLE public.class_profile, public.class_staff, public.learner,
    public.class_enrollment, public.guardian_learner, public.homework_submission,
    public.submission_asset, public.calligraphy_review_draft, public.teacher_review,
    public.review_asset, public.teaching_admin_audit, public.moya_subscribe_grant,
    public.moya_invite_code, public.group_task_assignment, public.group_task,
    public.attachment, public.attach_pending IN ACCESS EXCLUSIVE MODE;

DO $$
DECLARE
    retired_table text;
    occupied boolean;
    dependency text;
BEGIN
    FOREACH retired_table IN ARRAY ARRAY[
        'class_profile', 'class_staff', 'learner', 'class_enrollment', 'guardian_learner',
        'homework_submission', 'submission_asset', 'calligraphy_review_draft',
        'teacher_review', 'review_asset', 'teaching_admin_audit',
        'moya_subscribe_grant', 'moya_invite_code'
    ] LOOP
        EXECUTE format('SELECT EXISTS (SELECT 1 FROM public.%I)', retired_table) INTO occupied;
        IF occupied THEN
            RAISE EXCEPTION 'retire_moya: table public.% is not empty; migration refused', retired_table
                USING HINT = '先迁出业务数据与审计记录；删除数据须另行取得专门授权，本迁移不会自动删除。';
        END IF;
    END LOOP;

    IF EXISTS (SELECT 1 FROM public.group_task_assignment
               WHERE learner_id IS NOT NULL OR submitted_by IS NOT NULL)
       OR EXISTS (SELECT 1 FROM public.group_task
                  WHERE idempotency_key IS NOT NULL OR request_digest IS NOT NULL) THEN
        RAISE EXCEPTION 'retire_moya: shared group task tables contain teaching metadata; migration refused'
            USING HINT = '先迁出教学 assignment/task；不得清空字段、合并行或删除共享群任务来绕过本门。';
    END IF;
    IF EXISTS (SELECT 1 FROM public.attachment WHERE scope IN ('teaching', 'moya'))
       OR EXISTS (SELECT 1 FROM public.attach_pending WHERE scope IN ('teaching', 'moya')) THEN
        RAISE EXCEPTION 'retire_moya: legacy teaching attachments or upload sessions remain; migration refused'
            USING HINT = '先迁出媒体、上传会话及其引用；本迁移不删除附件行、对象存储内容或上传会话。';
    END IF;

    SELECT pg_describe_object(dep.classid, dep.objid, dep.objsubid) INTO dependency
      FROM pg_depend dep
      JOIN pg_attribute col ON col.attrelid = dep.refobjid AND col.attnum = dep.refobjsubid
     WHERE dep.refclassid = 'pg_class'::regclass
       AND ((col.attrelid = 'public.group_task_assignment'::regclass
             AND col.attname IN ('learner_id', 'submitted_by'))
         OR (col.attrelid = 'public.group_task'::regclass
             AND col.attname IN ('idempotency_key', 'request_digest')))
       AND NOT (dep.classid = 'pg_constraint'::regclass AND dep.objid IN (
           SELECT oid FROM pg_constraint
            WHERE conrelid = 'public.group_task_assignment'::regclass
              AND conname IN ('fk_gta_learner', 'fk_gta_submitted_by')))
       AND NOT (dep.classid = 'pg_trigger'::regclass AND dep.objid IN (
           SELECT oid FROM pg_trigger
            WHERE tgrelid = 'public.group_task_assignment'::regclass
              AND tgname = 'trg_gta_learner_org_consistency'
              AND tgfoid = 'public.fn_gta_learner_org_check()'::regprocedure))
       AND NOT (dep.classid = 'pg_class'::regclass AND dep.objid IN (
           SELECT indexrelid FROM pg_index
            WHERE indrelid IN ('public.group_task_assignment'::regclass, 'public.group_task'::regclass)
              AND indexrelid IN (
                  to_regclass('public.group_task_assignment_task_id_user_id_key'),
                  to_regclass('public.uk_group_task_assignment_task_learner'),
                  to_regclass('public.i_group_task_assignment_learner'),
                  to_regclass('public.uk_group_task_idempotency'))))
     LIMIT 1;
    IF dependency IS NOT NULL THEN
        RAISE EXCEPTION 'retire_moya: unknown shared-column dependency: %; migration refused', dependency
            USING HINT = '先审查共享依赖并制定单独迁移；禁止使用宽泛 CASCADE。';
    END IF;

    SELECT proc.oid::regprocedure::text INTO dependency
      FROM pg_proc proc JOIN pg_namespace ns ON ns.oid = proc.pronamespace
     WHERE ns.nspname = 'public'
       AND proc.proname NOT IN (
           'fn_class_enrollment_org_check', 'fn_learner_org_change_guard',
           'fn_gta_learner_org_check', 'fn_homework_submission_learner_check',
           'fn_homework_submission_withdraw_guard', 'fn_teacher_review_publish_guard',
           'fn_review_asset_image_cap')
       AND proc.prosrc ~* '\m(class_profile|class_staff|learner|class_enrollment|guardian_learner|homework_submission|submission_asset|calligraphy_review_draft|teacher_review|review_asset|teaching_admin_audit|moya_subscribe_grant|moya_invite_code)\M'
     LIMIT 1;
    IF dependency IS NOT NULL THEN
        RAISE EXCEPTION 'retire_moya: function may reference retired tables: %; migration refused', dependency
            USING HINT = '函数体的动态 SQL 依赖无法由 PostgreSQL RESTRICT 完整推断，须单独审查。';
    END IF;
END;
$$;

DROP TRIGGER trg_gta_learner_org_consistency ON public.group_task_assignment;
DROP INDEX public.group_task_assignment_task_id_user_id_key;
ALTER TABLE public.group_task_assignment
    DROP COLUMN learner_id RESTRICT,
    DROP COLUMN submitted_by RESTRICT;
ALTER TABLE public.group_task_assignment
    ADD CONSTRAINT group_task_assignment_task_id_user_id_key UNIQUE (task_id, user_id);
ALTER TABLE public.group_task
    DROP COLUMN idempotency_key RESTRICT,
    DROP COLUMN request_digest RESTRICT;

DROP TABLE public.review_asset RESTRICT;
DROP TABLE public.teacher_review RESTRICT;
DROP TABLE public.calligraphy_review_draft RESTRICT;
DROP TABLE public.submission_asset RESTRICT;
DROP TABLE public.homework_submission RESTRICT;
DROP TABLE public.guardian_learner RESTRICT;
DROP TABLE public.class_enrollment RESTRICT;
DROP TABLE public.class_staff RESTRICT;
DROP TABLE public.class_profile RESTRICT;
DROP TABLE public.learner RESTRICT;
DROP TABLE public.teaching_admin_audit RESTRICT;
DROP TABLE public.moya_subscribe_grant RESTRICT;
DROP TABLE public.moya_invite_code RESTRICT;
DROP FUNCTION public.fn_gta_learner_org_check() RESTRICT;
DROP FUNCTION public.fn_class_enrollment_org_check() RESTRICT;
DROP FUNCTION public.fn_learner_org_change_guard() RESTRICT;
DROP FUNCTION public.fn_homework_submission_learner_check() RESTRICT;
DROP FUNCTION public.fn_homework_submission_withdraw_guard() RESTRICT;
DROP FUNCTION public.fn_teacher_review_publish_guard() RESTRICT;
DROP FUNCTION public.fn_review_asset_image_cap() RESTRICT;

COMMENT ON TABLE public.organization IS '机构业务对象；授权与隔离边界为部署实例，非租户';
