-- user_log.type 允许值增加 130（个人数据导出审计）
--
-- 背景（真 bug）：C0-GOV-01 让 user_export_logic:audit/2 以 type=130 写
-- user_log，但 chk_user_log_type 仍是建库时的 ARRAY[100,102,110,901,902,903]，
-- 插入被 CHECK 拦下（23514 check_violation）。审计写入包在 try...catch 里、
-- 失败只记 ERROR 不阻断导出 —— 于是 GDPR 导出一直**没有任何审计留痕**，
-- 而合规声明说它有。单测把 user_log_ds mock 掉，看不见这条约束。
--
-- 幂等：先删旧约束再按新值建，重复执行安全。

ALTER TABLE public.user_log
    DROP CONSTRAINT IF EXISTS chk_user_log_type;
--;

ALTER TABLE public.user_log
    ADD CONSTRAINT chk_user_log_type
    CHECK (type = ANY (ARRAY[100, 102, 110, 130, 901, 902, 903]));
--;

COMMENT ON COLUMN public.user_log.type IS
    '日志类型: 100 用户注销备份  102 用户注销申请记录  110 修改密码  130 个人数据导出';
--;
