-- 00000096_teaching_identity.down.sql
-- 安全回滚：先删触发器/函数，再按依赖逆序删表（guardian_learner / class_enrollment / learner / class_staff / class_profile）。

DROP TRIGGER IF EXISTS trg_learner_org_change_guard ON learner;
DROP FUNCTION IF EXISTS fn_learner_org_change_guard();

DROP TRIGGER IF EXISTS trg_class_enrollment_org_consistency ON class_enrollment;
DROP FUNCTION IF EXISTS fn_class_enrollment_org_check();

DROP TABLE IF EXISTS guardian_learner;
DROP TABLE IF EXISTS class_enrollment;
DROP TABLE IF EXISTS learner;
DROP TABLE IF EXISTS class_staff;
DROP TABLE IF EXISTS class_profile;
