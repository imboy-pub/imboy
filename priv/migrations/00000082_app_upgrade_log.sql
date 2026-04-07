-- 00000082_app_upgrade_log.sql
-- APP 升级事件日志表
-- 记录客户端升级过程中的关键事件，用于版本分布统计和问题排查
--
-- Upgrade:
CREATE TABLE IF NOT EXISTS app_upgrade_log (
    id             bigint PRIMARY KEY,
    -- 设备ID（用于关联用户设备，匿名上报也有 did）
    did            varchar(128) NOT NULL DEFAULT '',
    -- 用户ID（可为0表示未登录用户）
    uid            bigint NOT NULL DEFAULT 0,
    -- 平台：android / ios / web / macos
    cos            varchar(20) NOT NULL DEFAULT '',
    -- 客户端当前版本
    client_vsn     varchar(40) NOT NULL DEFAULT '',
    -- 目标版本（升级到哪个版本）
    target_vsn     varchar(40) NOT NULL DEFAULT '',
    -- 事件类型：check / prompted / download_start / download_done
    --           verify_ok / verify_fail / install / cancel / error
    event          varchar(40) NOT NULL DEFAULT '',
    -- 升级类型：force / recommend / silent / none
    upgrade_type   varchar(20) NOT NULL DEFAULT '',
    -- 额外信息（JSON），如错误详情、耗时等
    extra          jsonb DEFAULT '{}',
    -- 创建时间
    created_at     timestamptz DEFAULT now()
);

-- 按时间范围查询 + 按版本统计
CREATE INDEX IF NOT EXISTS idx_app_upgrade_log_created_at
    ON app_upgrade_log (created_at);
CREATE INDEX IF NOT EXISTS idx_app_upgrade_log_event
    ON app_upgrade_log (event, created_at);
CREATE INDEX IF NOT EXISTS idx_app_upgrade_log_client_vsn
    ON app_upgrade_log (client_vsn);

-- Downgrade:
-- DROP TABLE IF EXISTS app_upgrade_log;
