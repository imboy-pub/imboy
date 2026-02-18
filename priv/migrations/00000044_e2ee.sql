-- ================================================================
-- E2EE+ 端到端加密增强功能 - 数据库迁移
-- 版本: 00000044
-- 日期: 2026-02-01
-- 说明: 统一的 E2EE+ 功能迁移文件
-- ================================================================
-- 功能模块:
--   1. 设备间传输 (Device Transfer)
--   2. 社交恢复 (Social Recovery)
--   3. 本地备份 (Local Backup)
-- ================================================================

-- ================================================================
-- 1. 设备间传输会话表
-- 用途: 管理设备间私钥传输会话
-- ================================================================

CREATE TABLE IF NOT EXISTS e2ee_transfer_sessions (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 会话标识（UUID v4）
    session_id VARCHAR(48) UNIQUE NOT NULL,

    -- 发送方信息（旧设备）
    from_uid BIGINT NOT NULL,
    from_device_id VARCHAR(64) NOT NULL,

    -- 接收方信息（新设备）
    to_uid BIGINT NOT NULL,
    to_device_id VARCHAR(64),

    -- 会话状态
    -- pending: 等待接受
    -- accepted: 已接受
    -- confirmed: 已确认
    -- expired: 已过期
    -- cancelled: 已取消
    status VARCHAR(20) NOT NULL DEFAULT 'pending',

    -- 加密的密钥包（使用接收方公钥 RSA-OAEP-256 加密的发送方私钥）
    encrypted_key_bundle TEXT NOT NULL,

    -- 过期时间（会话创建后 5 分钟过期）
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,

    -- 创建时间
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 确认时间
    confirmed_at TIMESTAMP WITH TIME ZONE,

    -- 更新时间
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 约束检查
    CONSTRAINT e2ee_transfer_sessions_from_uid_check CHECK (from_uid > 0),
    CONSTRAINT e2ee_transfer_sessions_to_uid_check CHECK (to_uid > 0)
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_session_id
    ON e2ee_transfer_sessions(session_id);

CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_from_uid
    ON e2ee_transfer_sessions(from_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_to_uid
    ON e2ee_transfer_sessions(to_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_status
    ON e2ee_transfer_sessions(status);

CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_expires_at
    ON e2ee_transfer_sessions(expires_at);

-- 复合索引：查询用户的待处理会话
CREATE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_to_uid_status_expires
    ON e2ee_transfer_sessions(to_uid, status, expires_at);

-- 并发保护唯一索引：防止同一对用户之间同时存在多个活跃传输会话
-- 部分唯一索引只在 status 为 'pending' 或 'accepted' 时生效
-- 注意：不包含 expires_at 条件，因为 NOW() 不是 IMMUTABLE 函数
CREATE UNIQUE INDEX IF NOT EXISTS idx_e2ee_transfer_sessions_unique_active
    ON e2ee_transfer_sessions(from_uid, to_uid)
    WHERE status IN ('pending', 'accepted');

-- 注释
COMMENT ON TABLE e2ee_transfer_sessions IS 'E2EE 设备间传输会话表 - 管理设备间私钥传输会话';
COMMENT ON COLUMN e2ee_transfer_sessions.id IS '主键';
COMMENT ON COLUMN e2ee_transfer_sessions.session_id IS '会话标识（UUID v4）';
COMMENT ON COLUMN e2ee_transfer_sessions.from_uid IS '发送方用户 ID';
COMMENT ON COLUMN e2ee_transfer_sessions.from_device_id IS '发送方设备 ID';
COMMENT ON COLUMN e2ee_transfer_sessions.to_uid IS '接收方用户 ID';
COMMENT ON COLUMN e2ee_transfer_sessions.to_device_id IS '接收方设备 ID';
COMMENT ON COLUMN e2ee_transfer_sessions.status IS '会话状态：pending/accepted/confirmed/expired/cancelled';
COMMENT ON COLUMN e2ee_transfer_sessions.encrypted_key_bundle IS '加密的密钥包（RSA-OAEP-256 加密）';
COMMENT ON COLUMN e2ee_transfer_sessions.expires_at IS '过期时间（5 分钟）';
COMMENT ON COLUMN e2ee_transfer_sessions.created_at IS '创建时间';
COMMENT ON COLUMN e2ee_transfer_sessions.confirmed_at IS '确认时间';
COMMENT ON COLUMN e2ee_transfer_sessions.updated_at IS '更新时间';

-- ================================================================
-- 2. 可信联系人表
-- 用途: 管理社交恢复功能中的可信好友
-- ================================================================

CREATE TABLE IF NOT EXISTS e2ee_trusted_contacts (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 用户 ID
    uid BIGINT NOT NULL,

    -- 可信联系人 ID
    contact_uid BIGINT NOT NULL,

    -- 联系人昵称（可选，方便识别）
    contact_nickname VARCHAR(100),

    -- 状态: active（活跃）, removed（已移除）
    status VARCHAR(20) NOT NULL DEFAULT 'active',

    -- 创建时间
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 更新时间
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 唯一约束：一个用户不能重复添加同一联系人
    UNIQUE(uid, contact_uid),

    -- 约束检查
    CONSTRAINT e2ee_trusted_contacts_uid_check CHECK (uid > 0),
    CONSTRAINT e2ee_trusted_contacts_contact_uid_check CHECK (contact_uid > 0),
    CONSTRAINT e2ee_trusted_contacts_status_check CHECK (status IN ('active', 'removed'))
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_e2ee_trusted_contacts_uid
    ON e2ee_trusted_contacts(uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_trusted_contacts_contact_uid
    ON e2ee_trusted_contacts(contact_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_trusted_contacts_status
    ON e2ee_trusted_contacts(status);

-- 注释
COMMENT ON TABLE e2ee_trusted_contacts IS 'E2EE 可信联系人表（社交恢复）';
COMMENT ON COLUMN e2ee_trusted_contacts.uid IS '用户 ID';
COMMENT ON COLUMN e2ee_trusted_contacts.contact_uid IS '可信联系人 ID';
COMMENT ON COLUMN e2ee_trusted_contacts.contact_nickname IS '联系人昵称，方便用户识别';
COMMENT ON COLUMN e2ee_trusted_contacts.status IS '状态：active（活跃）或 removed（已移除）';
COMMENT ON COLUMN e2ee_trusted_contacts.created_at IS '创建时间';
COMMENT ON COLUMN e2ee_trusted_contacts.updated_at IS '更新时间';

-- ================================================================
-- 3. 密钥分片表
-- 用途: 存储社交恢复中的密钥分片（加密存储）
-- ================================================================

CREATE TABLE IF NOT EXISTS e2ee_key_shares (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 密钥所有者 ID
    owner_uid BIGINT NOT NULL,

    -- 受托人 ID（存储密钥分片的好友）
    trustee_uid BIGINT NOT NULL,

    -- 加密的密钥分片（使用受托人的公钥加密）
    encrypted_share TEXT NOT NULL,

    -- 分片索引（1-5，表示第几个分片）
    share_index INTEGER NOT NULL,

    -- 恢复阈值（需要多少个分片才能恢复，默认 2）
    threshold INTEGER NOT NULL DEFAULT 2,

    -- 总分片数（默认 3）
    total_shares INTEGER NOT NULL DEFAULT 3,

    -- 创建时间
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 更新时间
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 唯一约束：一个受托人只能存储所有者的一个分片
    UNIQUE(owner_uid, trustee_uid),

    -- 约束检查
    CONSTRAINT e2ee_key_shares_owner_uid_check CHECK (owner_uid > 0),
    CONSTRAINT e2ee_key_shares_trustee_uid_check CHECK (trustee_uid > 0),
    CONSTRAINT e2ee_key_shares_share_index_check CHECK (share_index >= 1 AND share_index <= 5),
    CONSTRAINT e2ee_key_shares_threshold_check CHECK (threshold >= 1 AND threshold <= total_shares),
    CONSTRAINT e2ee_key_shares_total_shares_check CHECK (total_shares >= 2 AND total_shares <= 5)
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_e2ee_key_shares_owner_uid
    ON e2ee_key_shares(owner_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_key_shares_trustee_uid
    ON e2ee_key_shares(trustee_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_key_shares_share_index
    ON e2ee_key_shares(share_index);

-- 注释
COMMENT ON TABLE e2ee_key_shares IS 'E2EE 密钥分片表（社交恢复）';
COMMENT ON COLUMN e2ee_key_shares.owner_uid IS '密钥所有者 ID';
COMMENT ON COLUMN e2ee_key_shares.trustee_uid IS '受托人 ID（存储密钥分片的好友）';
COMMENT ON COLUMN e2ee_key_shares.encrypted_share IS '加密的密钥分片，使用受托人公钥加密';
COMMENT ON COLUMN e2ee_key_shares.share_index IS '分片索引（1-5）';
COMMENT ON COLUMN e2ee_key_shares.threshold IS '恢复阈值（需要多少分片才能恢复）';
COMMENT ON COLUMN e2ee_key_shares.total_shares IS '总分片数（默认 3）';
COMMENT ON COLUMN e2ee_key_shares.created_at IS '创建时间';
COMMENT ON COLUMN e2ee_key_shares.updated_at IS '更新时间';

-- ================================================================
-- 4. 社交恢复分片表
-- 用途: 使用 Shamir Secret Sharing 将密钥分割存储在信任的联系人处
-- ================================================================

CREATE TABLE IF NOT EXISTS e2ee_social_shards (
    id BIGSERIAL PRIMARY KEY,
    uid BIGINT NOT NULL,
    key_version VARCHAR(32) NOT NULL DEFAULT 'latest',
    shard_index INTEGER NOT NULL,
    total_shards INTEGER NOT NULL,
    threshold INTEGER NOT NULL,
    encrypted_shard TEXT NOT NULL,
    proxy_uid BIGINT NOT NULL,
    shard_id VARCHAR(64) UNIQUE NOT NULL,
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    used_at TIMESTAMP WITH TIME ZONE
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_uid
    ON e2ee_social_shards(uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_proxy_uid
    ON e2ee_social_shards(proxy_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_key_version
    ON e2ee_social_shards(key_version);

CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_shard_id
    ON e2ee_social_shards(shard_id);

CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_status
    ON e2ee_social_shards(status);

-- 复合索引：用户的活跃分片
CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_user_active
    ON e2ee_social_shards(uid, key_version, status)
    WHERE status = 'active';

-- 复合索引：代理的活跃分片
CREATE INDEX IF NOT EXISTS idx_e2ee_social_shards_proxy_active
    ON e2ee_social_shards(proxy_uid, status)
    WHERE status = 'active';

-- 并发保护唯一索引：防止同一用户同时创建多个活跃的分片集
CREATE UNIQUE INDEX IF NOT EXISTS idx_e2ee_social_shards_unique_active
    ON e2ee_social_shards(uid, key_version)
    WHERE status = 'active';

-- 注释
COMMENT ON TABLE e2ee_social_shards IS 'E2EE 社交恢复分片表 - 使用 Shamir Secret Sharing 将密钥分割存储在信任的联系人处';
COMMENT ON COLUMN e2ee_social_shards.uid IS '用户 ID - 密钥所有者';
COMMENT ON COLUMN e2ee_social_shards.key_version IS '密钥版本号 - 用于标识不同版本的密钥';
COMMENT ON COLUMN e2ee_social_shards.shard_index IS '分片索引 - 从 0 开始的编号';
COMMENT ON COLUMN e2ee_social_shards.total_shards IS '总分片数 - 通常为 3 或 5';
COMMENT ON COLUMN e2ee_social_shards.threshold IS '恢复阈值 - 需要多少分片才能恢复（通常为 2）';
COMMENT ON COLUMN e2ee_social_shards.encrypted_shard IS '加密的分片数据 - 使用代理的公钥加密';
COMMENT ON COLUMN e2ee_social_shards.proxy_uid IS '代理用户 ID - 存储分片的信任联系人';
COMMENT ON COLUMN e2ee_social_shards.shard_id IS '分片唯一标识符';
COMMENT ON COLUMN e2ee_social_shards.status IS '状态 - active（活跃）或 used（已使用）';
COMMENT ON COLUMN e2ee_social_shards.used_at IS '使用时间 - 分片被用于恢复密钥的时间';

-- ================================================================
-- 5. 本地备份元数据表
-- 用途: 记录用户的本地备份历史（仅元数据，不包含实际私钥）
-- ================================================================

CREATE TABLE IF NOT EXISTS e2ee_local_backups (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 用户 ID
    uid BIGINT NOT NULL,

    -- 设备 ID
    device_id VARCHAR(64) NOT NULL,

    -- 备份版本号（递增）
    backup_version INTEGER NOT NULL,

    -- 密钥校验和（SHA-256，用于验证备份完整性）
    key_checksum VARCHAR(64) NOT NULL,

    -- 备份文件大小（bytes）
    file_size BIGINT,

    -- 备份备注（用户可选）
    user_notes TEXT,

    -- 创建时间
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 约束检查
    CONSTRAINT e2ee_local_backups_uid_check CHECK (uid > 0),
    CONSTRAINT e2ee_local_backups_backup_version_check CHECK (backup_version > 0),
    CONSTRAINT e2ee_local_backups_file_size_check CHECK (file_size >= 0)
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_e2ee_local_backups_uid
    ON e2ee_local_backups(uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_local_backups_device_id
    ON e2ee_local_backups(device_id);

CREATE INDEX IF NOT EXISTS idx_e2ee_local_backups_created_at
    ON e2ee_local_backups(created_at);

-- 注释
COMMENT ON TABLE e2ee_local_backups IS 'E2EE 本地备份元数据表（仅记录备份历史）';
COMMENT ON COLUMN e2ee_local_backups.uid IS '用户 ID';
COMMENT ON COLUMN e2ee_local_backups.device_id IS '设备 ID';
COMMENT ON COLUMN e2ee_local_backups.backup_version IS '备份版本号（递增）';
COMMENT ON COLUMN e2ee_local_backups.key_checksum IS '密钥校验和（SHA-256），用于验证备份完整性';
COMMENT ON COLUMN e2ee_local_backups.file_size IS '备份文件大小（bytes）';
COMMENT ON COLUMN e2ee_local_backups.user_notes IS '备份备注（用户可选）';
COMMENT ON COLUMN e2ee_local_backups.created_at IS '创建时间';

-- ================================================================
-- 触发器函数
-- ================================================================

-- 自动更新 updated_at 触发器
CREATE OR REPLACE FUNCTION update_e2ee_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- 为 e2ee_transfer_sessions 创建触发器
DROP TRIGGER IF EXISTS trg_e2ee_transfer_sessions_updated_at ON e2ee_transfer_sessions;
CREATE TRIGGER trg_e2ee_transfer_sessions_updated_at
    BEFORE UPDATE ON e2ee_transfer_sessions
    FOR EACH ROW
    EXECUTE FUNCTION update_e2ee_updated_at();

-- 为 e2ee_trusted_contacts 创建触发器
DROP TRIGGER IF EXISTS trg_e2ee_trusted_contacts_updated_at ON e2ee_trusted_contacts;
CREATE TRIGGER trg_e2ee_trusted_contacts_updated_at
    BEFORE UPDATE ON e2ee_trusted_contacts
    FOR EACH ROW
    EXECUTE FUNCTION update_e2ee_updated_at();

-- 为 e2ee_key_shares 创建触发器
DROP TRIGGER IF EXISTS trg_e2ee_key_shares_updated_at ON e2ee_key_shares;
CREATE TRIGGER trg_e2ee_key_shares_updated_at
    BEFORE UPDATE ON e2ee_key_shares
    FOR EACH ROW
    EXECUTE FUNCTION update_e2ee_updated_at();

-- ================================================================
-- 清理函数（可选，用于定时任务）
-- ================================================================

-- 清理过期的传输会话
CREATE OR REPLACE FUNCTION cleanup_expired_transfer_sessions()
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    -- 删除超过 1 小时的过期会话
    DELETE FROM e2ee_transfer_sessions
    WHERE expires_at < NOW()
      AND created_at < NOW() - INTERVAL '1 hour';

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION cleanup_expired_transfer_sessions() IS '清理过期的传输会话';

-- 清理已使用的旧分片
CREATE OR REPLACE FUNCTION cleanup_used_social_shards()
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    -- 删除已使用超过 90 天的分片
    DELETE FROM e2ee_social_shards
    WHERE status = 'used'
      AND used_at < NOW() - INTERVAL '90 days';

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION cleanup_used_social_shards() IS '清理已使用超过 90 天的旧分片';

-- ================================================================
-- 验证查询
-- ================================================================

-- 迁移完成后，运行以下验证查询：

-- 1. 检查表是否创建成功
SELECT tablename FROM pg_tables
WHERE schemaname = 'public'
  AND tablename LIKE 'e2ee_%'
ORDER BY tablename;

-- 预期结果:
-- tablename
-- -----------------------
-- e2ee_key_shares
-- e2ee_local_backups
-- e2ee_shard_transmission_log
-- e2ee_social_shards
-- e2ee_transfer_sessions
-- e2ee_trusted_contacts

-- 2. 检查索引是否创建成功
SELECT indexname, tablename FROM pg_indexes
WHERE schemaname = 'public'
  AND tablename LIKE 'e2ee_%'
ORDER BY tablename, indexname;

-- 3. 检查触发器
SELECT trigger_name, event_object_table
FROM information_schema.triggers
WHERE event_object_table LIKE 'e2ee_%'
ORDER BY event_object_table, trigger_name;

-- 4. 检查函数
SELECT routine_name, routine_type
FROM information_schema.routines
WHERE routine_name LIKE 'e2ee_%' OR routine_name LIKE 'cleanup_%'
ORDER BY routine_name;

-- ================================================================
-- 6. 分片传输日志表（审计日志）
-- 用途: 零信任架构下的分片传输审计，记录所有分片操作
-- ================================================================

CREATE TABLE IF NOT EXISTS e2ee_shard_transmission_log (
    -- 主键
    id BIGSERIAL PRIMARY KEY,

    -- 分片标识
    shard_id VARCHAR(64) NOT NULL,

    -- 密钥版本号
    key_version VARCHAR(32) NOT NULL,

    -- 密钥所有者 UID（原始 ID，未 HashID 编码）
    uid BIGINT NOT NULL,

    -- 代理用户 UID（原始 ID，未 HashID 编码）
    proxy_uid BIGINT NOT NULL,

    -- 操作类型
    -- shard_created: 分片已创建
    -- shard_sent: 分片已发送给代理
    -- shard_stored: 代理已确认存储
    -- shard_decrypted: 分片已解密（恢复过程）
    -- shard_recovered: 分片已用于密钥恢复
    action VARCHAR(32) NOT NULL,

    -- 传输方向
    -- server_to_proxy: 服务端到代理（S2C）
    -- proxy_to_user: 代理到用户（C2C）
    direction VARCHAR(16) NOT NULL,

    -- 额外元数据（JSONB 格式）
    -- 包含加密状态、时间戳、设备信息等
    metadata JSONB,

    -- 客户端 IP 地址
    ip_address INET,

    -- 客户端 User-Agent
    user_agent TEXT,

    -- 创建时间
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),

    -- 约束检查
    CONSTRAINT e2ee_shard_transmission_log_uid_check CHECK (uid > 0),
    CONSTRAINT e2ee_shard_transmission_log_proxy_uid_check CHECK (proxy_uid > 0),
    CONSTRAINT e2ee_shard_transmission_log_action_check CHECK (
        action IN ('shard_created', 'shard_sent', 'shard_stored', 'shard_decrypted', 'shard_recovered')
    ),
    CONSTRAINT e2ee_shard_transmission_log_direction_check CHECK (
        direction IN ('server_to_proxy', 'proxy_to_user')
    )
);

-- 创建索引
CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_shard_id
    ON e2ee_shard_transmission_log(shard_id);

CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_key_version
    ON e2ee_shard_transmission_log(key_version);

CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_uid
    ON e2ee_shard_transmission_log(uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_proxy_uid
    ON e2ee_shard_transmission_log(proxy_uid);

CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_action
    ON e2ee_shard_transmission_log(action);

CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_created_at
    ON e2ee_shard_transmission_log(created_at);

-- 复合索引：查询分片的完整传输链路
CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_shard_action_time
    ON e2ee_shard_transmission_log(shard_id, action, created_at);

-- 复合索引：查询用户的所有传输记录
CREATE INDEX IF NOT EXISTS idx_e2ee_shard_transmission_log_uid_time
    ON e2ee_shard_transmission_log(uid, created_at DESC);

-- 注释
COMMENT ON TABLE e2ee_shard_transmission_log IS 'E2EE 分片传输日志表 - 零信任架构下的分片传输审计';
COMMENT ON COLUMN e2ee_shard_transmission_log.id IS '主键';
COMMENT ON COLUMN e2ee_shard_transmission_log.shard_id IS '分片唯一标识符';
COMMENT ON COLUMN e2ee_shard_transmission_log.key_version IS '密钥版本号';
COMMENT ON COLUMN e2ee_shard_transmission_log.uid IS '密钥所有者用户 ID（原始 ID，未 HashID 编码）';
COMMENT ON COLUMN e2ee_shard_transmission_log.proxy_uid IS '代理用户 ID（原始 ID，未 HashID 编码）';
COMMENT ON COLUMN e2ee_shard_transmission_log.action IS '操作类型：shard_created/shard_sent/shard_stored/shard_decrypted/shard_recovered';
COMMENT ON COLUMN e2ee_shard_transmission_log.direction IS '传输方向：server_to_proxy(S2C) 或 proxy_to_user(C2C)';
COMMENT ON COLUMN e2ee_shard_transmission_log.metadata IS '额外元数据（JSONB 格式，包含加密状态、时间戳等）';
COMMENT ON COLUMN e2ee_shard_transmission_log.ip_address IS '客户端 IP 地址';
COMMENT ON COLUMN e2ee_shard_transmission_log.user_agent IS '客户端 User-Agent';
COMMENT ON COLUMN e2ee_shard_transmission_log.created_at IS '创建时间';

-- ================================================================
-- 7. 审计视图：分片传输统计
-- ================================================================

CREATE OR REPLACE VIEW v_e2ee_shard_transmission_stats AS
SELECT
    key_version,
    shard_id,
    uid,
    COUNT(*) FILTER (WHERE action = 'shard_created') AS created_count,
    COUNT(*) FILTER (WHERE action = 'shard_sent') AS sent_count,
    COUNT(*) FILTER (WHERE action = 'shard_stored') AS stored_count,
    COUNT(*) FILTER (WHERE action = 'shard_decrypted') AS decrypted_count,
    COUNT(*) FILTER (WHERE action = 'shard_recovered') AS recovered_count,
    MIN(created_at) AS first_transmission_at,
    MAX(created_at) AS last_transmission_at
FROM e2ee_shard_transmission_log
GROUP BY key_version, shard_id, uid;

COMMENT ON VIEW v_e2ee_shard_transmission_stats IS 'E2EE 分片传输统计视图';

-- ================================================================
-- 8. 审计函数：检查分片传输异常
-- ================================================================

CREATE OR REPLACE FUNCTION check_e2ee_shard_transmission_anomaly(
    p_key_version VARCHAR,
    p_shard_id VARCHAR
)
RETURNS TABLE(
    anomaly_type TEXT,
    description TEXT,
    details JSONB
) AS $$
BEGIN
    RETURN QUERY
    WITH stats AS (
        SELECT
            COUNT(*) FILTER (WHERE action = 'shard_sent') AS sent_count,
            COUNT(*) FILTER (WHERE action = 'shard_stored') AS stored_count,
            COUNT(*) FILTER (WHERE action = 'shard_decrypted') AS decrypted_count
        FROM e2ee_shard_transmission_log
        WHERE key_version = p_key_version AND shard_id = p_shard_id
    )
    SELECT
        'missing_storage'::TEXT,
        '分片已发送但未收到存储确认'::TEXT,
        jsonb_build_object('sent', sent_count, 'stored', stored_count)
    FROM stats WHERE sent_count > 0 AND stored_count = 0

    UNION ALL

    SELECT
        'excessive_decryption'::TEXT,
        '分片解密次数异常'::TEXT,
        jsonb_build_object('decrypted', decrypted_count)
    FROM stats WHERE decrypted_count > 3

    UNION ALL

    SELECT
        'no_transmission'::TEXT,
        '分片无传输记录'::TEXT,
        jsonb_build_object('sent', sent_count, 'stored', stored_count)
    FROM stats WHERE sent_count = 0 AND stored_count = 0 AND decrypted_count = 0;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION check_e2ee_shard_transmission_anomaly IS '检查分片传输异常';

-- ================================================================
-- 清理函数（可选，用于定时任务）
-- ================================================================

-- 清理过期的传输日志（保留 90 天）
CREATE OR REPLACE FUNCTION cleanup_old_shard_transmission_logs()
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    -- 删除 90 天前的传输日志
    DELETE FROM e2ee_shard_transmission_log
    WHERE created_at < NOW() - INTERVAL '90 days';

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION cleanup_old_shard_transmission_logs() IS '清理 90 天前的分片传输日志';

-- ================================================================
-- 完成标记
-- ================================================================
-- 迁移版本: 00000044
-- 表数量: 6
-- 索引数量: 30+
-- 触发器数量: 3
-- 视图数量: 1
-- 函数数量: 4
-- ================================================================


-- ================================================================
-- 用户设备表添加 key_id 字段 - 数据库迁移
-- 版本: 00000046
-- 日期: 2026-02-09
-- 说明: 为 user_device 表添加 key_id 字段，用于标识 E2EE 密钥版本
-- ================================================================

-- 添加 key_id 字段
ALTER TABLE public.user_device ADD COLUMN IF NOT EXISTS key_id VARCHAR(255) DEFAULT ''::character varying;

-- 添加注释
COMMENT ON COLUMN public.user_device.key_id IS 'E2EE 密钥版本标识符，用于标识密钥变更';

-- 创建索引（可选，如果需要按 key_id 查询）
CREATE INDEX IF NOT EXISTS idx_user_device_key_id ON public.user_device(key_id) WHERE key_id != '';

-- ================================================================
-- 验证查询
-- ================================================================

-- 1. 检查字段是否添加成功
SELECT column_name, data_type, column_default
FROM information_schema.columns
WHERE table_schema = 'public'
  AND table_name = 'user_device'
  AND column_name = 'key_id';

-- 预期结果:
-- column_name | data_type | column_default
-- key_id      | character varying | ''::character varying
