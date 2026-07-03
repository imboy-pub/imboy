-- 【T10/D4】删除 E2EE 本地备份孤岛：create 端点从未实现过（e2ee_local_backup_ds
-- 按 YAGNI 从未封装 create/1），该表理论上一直为空；同账号换机 transfer（T09）
-- 已是主恢复路径。部署前已用 SELECT count(*) 核实生产表为空再执行。
DROP TABLE IF EXISTS public.e2ee_local_backups CASCADE;
