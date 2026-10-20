-- 00000207: app_upgrade_log 的 CHECK 白名单与客户端升级漏斗词表对齐（F16）
-- 背景: 客户端（imboyapp upgrade_page / app_upgrade_service / app_upgrade_orchestrator）
--       上报的 event 含 verify_exception 与 downgrade，不在 chk_app_upgrade_log_event 内；
--       且除 check/prompted 外的事件不携带 upgrade_type，服务端按列 DEFAULT '' 落空串，
--       而 chk_app_upgrade_log_upgrade_type 不允许 ''。违约插入在
--       app_upgrade_log_handler 的异步 spawn 中静默失败——漏斗自 prompted 之后全部丢失
--       （2026-10-20 真机升级联调 INSERT 探针复现）。
-- 决策: 事件白名单补入客户端实际使用的 verify_exception / downgrade；upgrade_type 允许
--       空串（语义「未指定」，与列 DEFAULT '' 自洽）。不改列类型、不动既有数据。
--       服务端 app_upgrade_log_logic:validate/1 持有同一份词表并在入库前校验——
--       两处必须同步修改。
ALTER TABLE public.app_upgrade_log
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_event;
ALTER TABLE public.app_upgrade_log
    ADD CONSTRAINT chk_app_upgrade_log_event CHECK (
        event IN (
            'check', 'prompted', 'download_start', 'download_done',
            'verify_ok', 'verify_fail', 'verify_exception', 'install',
            'cancel', 'error', 'downgrade'
        )
    );

ALTER TABLE public.app_upgrade_log
    DROP CONSTRAINT IF EXISTS chk_app_upgrade_log_upgrade_type;
ALTER TABLE public.app_upgrade_log
    ADD CONSTRAINT chk_app_upgrade_log_upgrade_type CHECK (
        upgrade_type IN ('force', 'recommend', 'silent', 'none', '')
    );
