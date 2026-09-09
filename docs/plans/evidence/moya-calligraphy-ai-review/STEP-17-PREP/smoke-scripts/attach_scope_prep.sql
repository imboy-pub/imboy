-- 附件三门打点前置：教学 scope + 撤回态专用附件（R9）。
-- 依赖 seed_smoke.sql + matrix_teaching.sh 已产生 sid2（withdrawn 提交，见下注）。
-- 778001/778002 提升为 teaching scope；778003 仅绑定撤回提交（T17 撤回证据不可达场景）。
-- 注意 111731315222775808 是 R8 实测产生的 submission id——本地复现时替换为你的 sid2。
BEGIN;
UPDATE attachment SET scope = 'teaching' WHERE id IN (778001, 778002);
INSERT INTO attachment (id, file_hash256, path, mime_type, creator_user_id, scope)
 VALUES (778003,'h77v3','p/778003','video/mp4',770002,'teaching');
INSERT INTO submission_asset (id, submission_id, attachment_id, kind)
 VALUES (778003, 111731315222775808, 778003, 'practice_video');
COMMIT;
