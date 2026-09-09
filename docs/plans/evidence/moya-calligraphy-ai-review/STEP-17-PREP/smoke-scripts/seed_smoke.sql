-- 冒烟种子（7xxxx 段）：教学 API 契约打点最小链。
-- 库：moya_boot_smoke@127.0.0.1:4323（99+ 全链迁移态）。每段可重复执行前先手工回滚/清理。
-- 幂等注意：含固定 ID INSERT，重复执行会 23505——仅首次或清理后执行。
BEGIN;
INSERT INTO "user" (id, password, account, reg_ip, reg_cosv) VALUES
 (770001,'x','t77_teacher','127.0.0.1','x'),
 (770002,'x','t77_parent','127.0.0.1','x'),
 (770003,'x','t77_assist','127.0.0.1','x'),
 (770004,'x','t77_mgr','127.0.0.1','x'),
 (770005,'x','t77_orgb','127.0.0.1','x'),
 (770006,'x','t77_target','127.0.0.1','x');
INSERT INTO organization (id, name, owner_id) VALUES
 (771000,'orgA77',770001),(771100,'orgB77',770005);
INSERT INTO workspace (id, name, owner_id, organization_id) VALUES
 (772001,'A-hq77',770001,771000),(772100,'B-hq77',770005,771100);
INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, status) VALUES
 (772001,770001,'owner',770001,'active'),(772001,770002,'member',770001,'active');
INSERT INTO "group" (id, owner_uid, creator_uid, scope, workspace_id, title) VALUES
 (773001,770001,770001,'workspace',772001,'A1班77');
INSERT INTO learner (id, organization_id, display_name) VALUES (774001,771000,'L77');
INSERT INTO class_enrollment (group_id, learner_id) VALUES (773001,774001);
INSERT INTO class_staff (group_id, user_id, role) VALUES
 (773001,770001,'teacher'),(773001,770004,'manager'),(773001,770003,'assistant');
INSERT INTO guardian_learner (guardian_uid, learner_id, can_submit, can_view_review)
 VALUES (770002,774001,true,true);
INSERT INTO group_task (id, group_id, task_id, title, creator_id, status) VALUES
 (775001,773001,'task77_hash_1','任务77',770001,1);
INSERT INTO group_task_assignment (id, task_id, user_id, learner_id) VALUES
 (776001,'task77_hash_1',770002,774001);
INSERT INTO attachment (id, file_hash256, path, mime_type, creator_user_id) VALUES
 (778001,'h77video','p/778001','video/mp4',770002),
 (778002,'h77photo','p/778002','image/jpeg',770002);
COMMIT;
