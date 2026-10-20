-- 00000207 down: 恢复 00000001 建表时的两条 CHECK 白名单。
-- 注意: 若 up 之后已写入 verify_exception / downgrade 事件或 upgrade_type='' 的行，
--       加回旧约束会失败——需先清理这些行。
ALTER TABLE public.app_upgrade_log
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_event;
ALTER TABLE public.app_upgrade_log
    ADD CONSTRAINT chk_app_upgrade_log_event CHECK (
        event IN (
            'check', 'prompted', 'download_start', 'download_done',
            'verify_ok', 'verify_fail', 'install', 'cancel', 'error'
        )
    );

ALTER TABLE public.app_upgrade_log
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_upgrade_type;
ALTER TABLE public.app_upgrade_log
    ADD CONSTRAINT chk_app_upgrade_log_upgrade_type CHECK (
        upgrade_type IN ('force', 'recommend', 'silent', 'none')
    );
