-- 管理员操作日志表（合规审计）
CREATE TABLE IF NOT EXISTS admin_operation_logs (
    id          bigint      NOT NULL,
    adm_user_id bigint      NOT NULL,
    action      varchar(64) NOT NULL,   -- 操作类型：force_logout/ban_user/unban_user/delete_msg/update_config/...
    target_id   bigint,                  -- 操作对象 ID（用户ID/消息ID等）
    target_type varchar(32),             -- 对象类型：user/message/group/channel
    detail      jsonb,                   -- 操作详情（变更前后值、原因等）
    ip          varchar(64),             -- 操作者 IP
    created_at  bigint      NOT NULL    -- 毫秒时间戳
);
ALTER TABLE admin_operation_logs ADD PRIMARY KEY (id);
CREATE INDEX idx_admin_op_logs_adm_user ON admin_operation_logs(adm_user_id, created_at DESC);
CREATE INDEX idx_admin_op_logs_action ON admin_operation_logs(action, created_at DESC);
COMMENT ON TABLE admin_operation_logs IS '管理员操作审计日志（等保2.0合规）';
