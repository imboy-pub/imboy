-- ============================================================================
-- Imboy 多机房部署 - ID段动态分配服务 (完整版)
-- ============================================================================
-- 功能说明：
--   1. 支持机房动态注册/注销
--   2. 每个机房每张表维护独立的ID段
--   3. 自动续期机制
--   4. 完整的审计日志
--   5. 监控和统计功能
--
-- 版本: 1.0.0
-- 创建日期: 2026-01-28
-- ============================================================================

-- ============================================================================
-- 第一部分: 数据表创建
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 表 1: 机房注册表
-- ----------------------------------------------------------------------------
-- 用途: 管理所有机房的基本信息
-- 特点:
--   - 支持动态新增机房
--   - 软删除机制（保留历史数据）
--   - 审计日志

CREATE TABLE IF NOT EXISTS system_datacenter (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    region VARCHAR(50) NOT NULL,
    api_endpoint VARCHAR(255),
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE system_datacenter IS '机房注册表 - 支持动态增删';
COMMENT ON COLUMN system_datacenter.id IS '机房ID (0-32767), 由数据库自动分配';
COMMENT ON COLUMN system_datacenter.name IS '机房名称，业务标识，如: beijing, shanghai, shenzhen';
COMMENT ON COLUMN system_datacenter.region IS '机房所在区域，如: cn-north, cn-east, cn-south';
COMMENT ON COLUMN system_datacenter.api_endpoint IS '机房API地址，用于健康检查和数据同步';
COMMENT ON COLUMN system_datacenter.is_active IS '是否激活，注销时设置为FALSE（软删除）';
COMMENT ON COLUMN system_datacenter.created_at IS '机房注册时间';
COMMENT ON COLUMN system_datacenter.updated_at IS '机房信息最后更新时间';

CREATE INDEX IF NOT EXISTS i_datacenter_active ON system_datacenter(is_active);
CREATE INDEX IF NOT EXISTS i_datacenter_region ON system_datacenter(region);

-- ----------------------------------------------------------------------------
-- 表 2: ID段分配记录表
-- ----------------------------------------------------------------------------
-- 用途: 记录每个机房每张表的ID段分配历史
-- 特点:
--   - 支持查询历史分配记录
--   - 自动过期机制
--   - 使用率统计

CREATE TABLE IF NOT EXISTS system_id_segment (
    id BIGSERIAL PRIMARY KEY,
    datacenter_id SMALLINT NOT NULL REFERENCES system_datacenter(id),
    table_name VARCHAR(50) NOT NULL,
    segment_start BIGINT NOT NULL,
    segment_end BIGINT NOT NULL,
    allocated_size INT NOT NULL,
    used_count INT DEFAULT 0,
    is_active BOOLEAN DEFAULT TRUE,
    allocated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    expired_at TIMESTAMPTZ,
    CONSTRAINT uk_datacenter_table_active UNIQUE (datacenter_id, table_name)
);

COMMENT ON TABLE system_id_segment IS 'ID段分配记录 - 每个机房每张表维护独立的ID段';
COMMENT ON COLUMN system_id_segment.id IS '分配记录ID';
COMMENT ON COLUMN system_id_segment.datacenter_id IS '所属机房ID';
COMMENT ON COLUMN system_id_segment.table_name IS '表名，如: user, group, msg_c2c';
COMMENT ON COLUMN system_id_segment.segment_start IS 'ID段起始值 (包含)';
COMMENT ON COLUMN system_id_segment.segment_end IS 'ID段结束值 (包含)';
COMMENT ON COLUMN system_id_segment.allocated_size IS '分配的ID总数';
COMMENT ON COLUMN system_id_segment.used_count IS '已使用的ID数量 (估算值，用于监控)';
COMMENT ON COLUMN system_id_segment.is_active IS '是否为当前活跃段';
COMMENT ON COLUMN system_id_segment.allocated_at IS 'ID段分配时间';
COMMENT ON COLUMN system_id_segment.expired_at IS '过期时间，NULL 表示永不过期';

CREATE INDEX IF NOT EXISTS i_id_segment_dc_table ON system_id_segment(datacenter_id, table_name);
CREATE UNIQUE INDEX IF NOT EXISTS uk_datacenter_table_active ON system_id_segment(datacenter_id, table_name)
    WHERE is_active = TRUE;
CREATE INDEX IF NOT EXISTS i_id_segment_active ON system_id_segment(is_active, expired_at)
    WHERE is_active = TRUE;
CREATE INDEX IF NOT EXISTS i_id_segment_table ON system_id_segment(table_name);
CREATE INDEX IF NOT EXISTS i_id_segment_allocated_at ON system_id_segment(allocated_at DESC);

-- ----------------------------------------------------------------------------
-- 表 3: 机房变更审计日志
-- ----------------------------------------------------------------------------
-- 用途: 记录所有机房注册、注销、修改操作
-- 特点:
--   - 不可变记录
--   - JSONB 格式存储详细信息

CREATE TABLE IF NOT EXISTS system_datacenter_log (
    id BIGSERIAL PRIMARY KEY,
    datacenter_id SMALLINT,
    action VARCHAR(20) NOT NULL,
    details JSONB,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE system_datacenter_log IS '机房变更审计日志 - 记录所有注册/注销/修改操作';
COMMENT ON COLUMN system_datacenter_log.id IS '日志记录ID';
COMMENT ON COLUMN system_datacenter_log.datacenter_id IS '机房ID';
COMMENT ON COLUMN system_datacenter_log.action IS '操作类型: register(注册), unregister(注销), modify(修改)';
COMMENT ON COLUMN system_datacenter_log.details IS '详细信息，JSONB格式';
COMMENT ON COLUMN system_datacenter_log.created_at IS '操作时间';

CREATE INDEX IF NOT EXISTS i_dclog_dc_id ON system_datacenter_log(datacenter_id);
CREATE INDEX IF NOT EXISTS i_dclog_action ON system_datacenter_log(action);
CREATE INDEX IF NOT EXISTS i_dclog_created_at ON system_datacenter_log(created_at DESC);

-- ----------------------------------------------------------------------------
-- 表 4: ID段使用统计表
-- ----------------------------------------------------------------------------
-- 用途: 定期统计ID段使用情况
-- 特点:
--   - 时间序列数据
--   - 用于趋势分析

CREATE TABLE IF NOT EXISTS system_id_segment_stats (
    id BIGSERIAL PRIMARY KEY,
    datacenter_id SMALLINT NOT NULL,
    table_name VARCHAR(50) NOT NULL,
    stats_time TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    segment_start BIGINT,
    segment_end BIGINT,
    used_count INT,
    usage_percent NUMERIC(5,2),
    allocate_rate_per_sec NUMERIC(10,2)
);

COMMENT ON TABLE system_id_segment_stats IS 'ID段使用统计表 - 用于监控和趋势分析';
COMMENT ON COLUMN system_id_segment_stats.datacenter_id IS '机房ID';
COMMENT ON COLUMN system_id_segment_stats.table_name IS '表名';
COMMENT ON COLUMN system_id_segment_stats.stats_time IS '统计时间';
COMMENT ON COLUMN system_id_segment_stats.segment_start IS '当前使用的ID段起始值';
COMMENT ON COLUMN system_id_segment_stats.segment_end IS '当前使用的ID段结束值';
COMMENT ON COLUMN system_id_segment_stats.used_count IS '使用量估算';
COMMENT ON COLUMN system_id_segment_stats.usage_percent IS '使用率百分比';
COMMENT ON COLUMN system_id_segment_stats.allocate_rate_per_sec IS '每秒分配速率';

CREATE INDEX IF NOT EXISTS i_idstats_dc_table_time ON system_id_segment_stats(datacenter_id, table_name, stats_time DESC);

-- ============================================================================
-- 第二部分: 管理函数创建
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 函数 1: 注册新机房
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION register_datacenter(
    p_name VARCHAR(50),
    p_region VARCHAR(50),
    p_api_endpoint VARCHAR(255) DEFAULT NULL
) RETURNS SMALLINT AS $$
DECLARE
    v_dc_id SMALLINT;
BEGIN
    IF p_name IS NULL OR trim(p_name) = '' THEN
        RAISE EXCEPTION 'Datacenter name cannot be empty';
    END IF;

    IF p_region IS NULL OR trim(p_region) = '' THEN
        RAISE EXCEPTION 'Datacenter region cannot be empty';
    END IF;

    INSERT INTO system_datacenter (name, region, api_endpoint)
    VALUES (trim(p_name), trim(p_region), p_api_endpoint)
    ON CONFLICT (name) DO NOTHING
    RETURNING id INTO v_dc_id;

    IF v_dc_id IS NULL THEN
        SELECT id INTO v_dc_id
        FROM system_datacenter
        WHERE name = trim(p_name);
        RAISE NOTICE 'Datacenter "%" already exists with ID %', p_name, v_dc_id;
    ELSE
        INSERT INTO system_datacenter_log (datacenter_id, action, details)
        VALUES (v_dc_id, 'register', jsonb_build_object(
            'name', p_name,
            'region', p_region,
            'endpoint', p_api_endpoint
        ));

        RAISE NOTICE 'Successfully registered datacenter "%" with ID %', p_name, v_dc_id;
    END IF;

    RETURN v_dc_id;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION register_datacenter IS '注册新机房 - 返回机房ID';

-- ----------------------------------------------------------------------------
-- 函数 2: 注销机房 (软删除)
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION unregister_datacenter(
    p_dc_id SMALLINT
) RETURNS BOOLEAN AS $$
DECLARE
    v_name VARCHAR(50);
BEGIN
    SELECT name INTO v_name
    FROM system_datacenter
    WHERE id = p_dc_id AND is_active = TRUE;

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Datacenter % not found or already inactive', p_dc_id;
    END IF;

    UPDATE system_datacenter
    SET is_active = FALSE,
        updated_at = CURRENT_TIMESTAMP
    WHERE id = p_dc_id AND is_active = TRUE;

    INSERT INTO system_datacenter_log (datacenter_id, action, details)
    VALUES (p_dc_id, 'unregister', jsonb_build_object('name', v_name));

    RAISE NOTICE 'Successfully unregistered datacenter "%" (ID %)', v_name, p_dc_id;
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION unregister_datacenter IS '注销机房 - 软删除，保留数据';

-- ----------------------------------------------------------------------------
-- 函数 3: 重新激活机房
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION reactivate_datacenter(
    p_dc_id SMALLINT
) RETURNS BOOLEAN AS $$
DECLARE
    v_name VARCHAR(50);
BEGIN
    SELECT name INTO v_name
    FROM system_datacenter
    WHERE id = p_dc_id AND is_active = FALSE;

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Datacenter % not found or already active', p_dc_id;
    END IF;

    UPDATE system_datacenter
    SET is_active = TRUE,
        updated_at = CURRENT_TIMESTAMP
    WHERE id = p_dc_id AND is_active = FALSE;

    INSERT INTO system_datacenter_log (datacenter_id, action, details)
    VALUES (p_dc_id, 'reactivate', jsonb_build_object('name', v_name));

    RAISE NOTICE 'Successfully reactivated datacenter "%" (ID %)', v_name, p_dc_id;
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION reactivate_datacenter IS '重新激活已注销的机房';

-- ----------------------------------------------------------------------------
-- 函数 4: 获取或分配ID段 (核心函数)
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION get_or_allocate_id_segment(
    p_dc_id SMALLINT,
    p_table_name VARCHAR(50),
    p_segment_size INT DEFAULT 100000,
    p_expire_days INT DEFAULT 30
) RETURNS TABLE(segment_start BIGINT, segment_end BIGINT) AS $$
DECLARE
    v_segment_start BIGINT;
    v_segment_end BIGINT;
    v_max_allocated BIGINT;
    v_current_segment_id INT;
BEGIN
    IF p_dc_id IS NULL THEN
        RAISE EXCEPTION 'Datacenter ID cannot be NULL';
    END IF;

    IF p_table_name IS NULL OR trim(p_table_name) = '' THEN
        RAISE EXCEPTION 'Table name cannot be empty';
    END IF;

    IF p_segment_size <= 0 OR p_segment_size > 10000000 THEN
        RAISE EXCEPTION 'Segment size must be between 1 and 10,000,000';
    END IF;

    SELECT id, segment_start, segment_end
    INTO v_current_segment_id, v_segment_start, v_segment_end
    FROM system_id_segment
    WHERE datacenter_id = p_dc_id
        AND table_name = p_table_name
        AND is_active = TRUE
        AND (expired_at IS NULL OR expired_at > CURRENT_TIMESTAMP)
    FOR UPDATE SKIP LOCKED
    LIMIT 1;

    IF v_current_segment_id IS NOT NULL THEN
        RAISE NOTICE 'Using existing segment for datacenter %, table %: % to %',
            p_dc_id, p_table_name, v_segment_start, v_segment_end;
        RETURN QUERY SELECT v_segment_start, v_segment_end;
        RETURN;
    END IF;

    SELECT COALESCE(MAX(segment_end), 0)
    INTO v_max_allocated
    FROM system_id_segment
    WHERE table_name = p_table_name
    FOR UPDATE;

    v_segment_start := v_max_allocated + 1;
    v_segment_end := v_segment_start + p_segment_size - 1;

    IF v_segment_end > 9223372036854775807 THEN
        RAISE EXCEPTION 'ID segment would overflow BIGINT limit for table %', p_table_name;
    END IF;

    INSERT INTO system_id_segment (
        datacenter_id, table_name,
        segment_start, segment_end, allocated_size,
        expired_at
    ) VALUES (
        p_dc_id, p_table_name,
        v_segment_start, v_segment_end, p_segment_size,
        CASE WHEN p_expire_days > 0
             THEN CURRENT_TIMESTAMP + (p_expire_days || ' days')::INTERVAL
             ELSE NULL
        END
    );

    UPDATE system_id_segment
    SET is_active = FALSE
    WHERE datacenter_id = p_dc_id
        AND table_name = p_table_name
        AND is_active = TRUE
        AND id != (
            SELECT id
            FROM system_id_segment
            WHERE datacenter_id = p_dc_id AND table_name = p_table_name
            ORDER BY id DESC
            LIMIT 1
        );

    RAISE NOTICE 'Allocated new segment for datacenter %, table %: % to %',
        p_dc_id, p_table_name, v_segment_start, v_segment_end;

    RETURN QUERY SELECT v_segment_start, v_segment_end;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION get_or_allocate_id_segment IS '获取或分配ID段 - 自动续期 (核心函数)';

-- ----------------------------------------------------------------------------
-- 函数 5: 从ID段初始化序列
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION init_sequence_from_segment(
    p_table_name VARCHAR(50),
    p_dc_id SMALLINT
) RETURNS BOOLEAN AS $$
DECLARE
    v_segment_start BIGINT;
    v_segment_end BIGINT;
    v_sequence_name VARCHAR(100);
    v_sequence_exists BOOLEAN;
BEGIN
    SELECT segment_start, segment_end
    INTO v_segment_start, v_segment_end
    FROM get_or_allocate_id_segment(p_dc_id, p_table_name, 100000, 30);

    IF v_segment_start IS NULL THEN
        RAISE EXCEPTION 'Failed to allocate ID segment for table %', p_table_name;
    END IF;

    v_sequence_name := p_table_name || '_id_seq';

    SELECT EXISTS(
        SELECT 1 FROM pg_sequences
        WHERE schemaname = 'public' AND sequencename = v_sequence_name
    ) INTO v_sequence_exists;

    IF v_sequence_exists THEN
        EXECUTE format('ALTER SEQUENCE %I RESTART WITH %s', v_sequence_name, v_segment_start);
        RAISE NOTICE 'Reset existing sequence %I to start at %', v_sequence_name, v_segment_start;
    ELSE
        EXECUTE format('CREATE SEQUENCE %I START WITH %s', v_sequence_name, v_segment_start);
        RAISE NOTICE 'Created new sequence %I starting at %', v_sequence_name, v_segment_start;
    END IF;

    EXECUTE format('ALTER SEQUENCE %I MAXVALUE %s', v_sequence_name, v_segment_end);
    EXECUTE format('ALTER SEQUENCE %I CACHE 100', v_sequence_name);

    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION init_sequence_from_segment IS '从ID段初始化序列 - 应用启动时调用';

-- ----------------------------------------------------------------------------
-- 函数 6: 批量初始化所有表的序列
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION init_all_sequences(
    p_dc_id SMALLINT
) RETURNS TABLE(
    table_name VARCHAR(50),
    sequence_name VARCHAR(100),
    segment_start BIGINT,
    segment_end BIGINT,
    status BOOLEAN,
    message VARCHAR(255)
) AS $$
DECLARE
    v_table_name VARCHAR(50);
    v_seq_name VARCHAR(100);
    v_segment_start BIGINT;
    v_segment_end BIGINT;
    v_status BOOLEAN;
BEGIN
    FOR v_table_name IN SELECT unnest(ARRAY[
        'user', 'user_device', 'user_friend',
        'group', 'group_member',
        'msg_c2c', 'msg_c2g', 'msg_c2s', 'msg_s2c',
        'attachment', 'conversation'
    ])
    LOOP
        SELECT segment_start, segment_end
        INTO v_segment_start, v_segment_end
        FROM get_or_allocate_id_segment(p_dc_id, v_table_name, 100000, 30);

        v_seq_name := v_table_name || '_id_seq';

        v_status := init_sequence_from_segment(v_table_name, p_dc_id);

        RETURN QUERY SELECT
            v_table_name::VARCHAR(50) AS table_name,
            v_seq_name::VARCHAR(100) AS sequence_name,
            v_segment_start AS segment_start,
            v_segment_end AS segment_end,
            v_status AS status,
            'OK'::VARCHAR(255) AS message;
    END LOOP;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION init_all_sequences IS '批量初始化所有表的序列 - 部署时一键执行';

-- ----------------------------------------------------------------------------
-- 函数 7: 查看机房ID段使用情况
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION get_datacenter_segment_status(
    p_dc_id SMALLINT DEFAULT NULL,
    p_table_name VARCHAR(50) DEFAULT NULL
) RETURNS TABLE(
    datacenter_id SMALLINT,
    datacenter_name VARCHAR(50),
    datacenter_region VARCHAR(50),
    table_name VARCHAR(50),
    segment_start BIGINT,
    segment_end BIGINT,
    total_size BIGINT,
    used_count INT,
    usage_percent NUMERIC(5,2),
    is_active BOOLEAN,
    allocated_at TIMESTAMPTZ,
    expired_at TIMESTAMPTZ,
    remaining_days INT,
    status VARCHAR(20)
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        s.datacenter_id,
        dc.name AS datacenter_name,
        dc.region AS datacenter_region,
        s.table_name,
        s.segment_start,
        s.segment_end,
        (s.segment_end - s.segment_start + 1) AS total_size,
        s.used_count,
        CASE
            WHEN (s.segment_end - s.segment_start + 1) > 0 THEN
                ROUND((s.used_count::NUMERIC / (s.segment_end - s.segment_start + 1)) * 100, 2)
            ELSE 0
        END AS usage_percent,
        s.is_active,
        s.allocated_at,
        s.expired_at,
        CASE
            WHEN s.expired_at IS NOT NULL THEN
                EXTRACT(DAY FROM (s.expired_at - CURRENT_TIMESTAMP))::INT
            ELSE NULL
        END AS remaining_days,
        CASE
            WHEN NOT dc.is_active THEN 'INACTIVE'
            WHEN s.expired_at IS NOT NULL AND s.expired_at < CURRENT_TIMESTAMP THEN 'EXPIRED'
            WHEN s.used_count::NUMERIC / (s.segment_end - s.segment_start + 1) > 0.9 THEN 'CRITICAL'
            WHEN s.used_count::NUMERIC / (s.segment_end - s.segment_start + 1) > 0.8 THEN 'WARNING'
            ELSE 'OK'
        END AS status
    FROM system_id_segment s
    JOIN system_datacenter dc ON dc.id = s.datacenter_id
    WHERE (p_dc_id IS NULL OR s.datacenter_id = p_dc_id)
        AND (p_table_name IS NULL OR s.table_name = p_table_name)
    ORDER BY s.datacenter_id, s.table_name, s.allocated_at DESC;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION get_datacenter_segment_status IS '查看ID段使用状态 - 监控面板使用';

-- ----------------------------------------------------------------------------
-- 函数 8: 手动续期ID段
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION renew_id_segment(
    p_dc_id SMALLINT,
    p_table_name VARCHAR(50),
    p_segment_size INT DEFAULT 100000
) RETURNS TABLE(segment_start BIGINT, segment_end BIGINT) AS $$
DECLARE
    v_old_segment_id INT;
BEGIN
    UPDATE system_id_segment
    SET is_active = FALSE
    WHERE datacenter_id = p_dc_id
        AND table_name = p_table_name
        AND is_active = TRUE
    RETURNING id INTO v_old_segment_id;

    IF v_old_segment_id IS NOT NULL THEN
        RAISE NOTICE 'Deactivated old segment % for datacenter %, table %',
            v_old_segment_id, p_dc_id, p_table_name;
    END IF;

    RETURN QUERY
    SELECT * FROM get_or_allocate_id_segment(p_dc_id, p_table_name, p_segment_size, 30);
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION renew_id_segment IS '手动续期ID段 - 紧急情况使用';

-- ----------------------------------------------------------------------------
-- 函数 9: 获取机房列表
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION get_datacenters(
    p_active_only BOOLEAN DEFAULT TRUE
) RETURNS TABLE(
    id SMALLINT,
    name VARCHAR(50),
    region VARCHAR(50),
    api_endpoint VARCHAR(255),
    is_active BOOLEAN,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ,
    active_tables_count INT
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        dc.id,
        dc.name,
        dc.region,
        dc.api_endpoint,
        dc.is_active,
        dc.created_at,
        dc.updated_at,
        COUNT(DISTINCT seg.table_name)::INT AS active_tables_count
    FROM system_datacenter dc
    LEFT JOIN system_id_segment seg ON seg.datacenter_id = dc.id AND seg.is_active = TRUE
    WHERE (NOT p_active_only OR dc.is_active = TRUE)
    GROUP BY dc.id, dc.name, dc.region, dc.api_endpoint, dc.is_active, dc.created_at, dc.updated_at
    ORDER BY dc.id;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION get_datacenters IS '获取机房列表';

-- ----------------------------------------------------------------------------
-- 函数 10: 记录ID段使用统计 (定时任务调用)
-- ----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION collect_id_segment_stats(
    p_dc_id SMALLINT DEFAULT NULL
) RETURNS INT AS $$
DECLARE
    v_record_count INT;
BEGIN
    INSERT INTO system_id_segment_stats (
        datacenter_id,
        table_name,
        segment_start,
        segment_end,
        used_count,
        usage_percent
    )
    SELECT
        s.datacenter_id,
        s.table_name,
        s.segment_start,
        s.segment_end,
        s.used_count,
        CASE
            WHEN (s.segment_end - s.segment_start + 1) > 0 THEN
                ROUND((s.used_count::NUMERIC / (s.segment_end - s.segment_start + 1)) * 100, 2)
            ELSE 0
        END
    FROM system_id_segment s
    JOIN system_datacenter dc ON dc.id = s.datacenter_id
    WHERE s.is_active = TRUE
        AND dc.is_active = TRUE
        AND (p_dc_id IS NULL OR s.datacenter_id = p_dc_id);

    GET DIAGNOSTICS v_record_count = ROW_COUNT;

    DELETE FROM system_id_segment_stats
    WHERE stats_time < CURRENT_TIMESTAMP - INTERVAL '30 days';

    RETURN v_record_count;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION collect_id_segment_stats IS '记录ID段使用统计 - 定时任务调用';

-- ============================================================================
-- 第三部分: 视图创建
-- ============================================================================

CREATE OR REPLACE VIEW v_id_segment_monitor AS
SELECT * FROM get_datacenter_segment_status();

COMMENT ON VIEW v_id_segment_monitor IS 'ID段监控视图';

CREATE OR REPLACE VIEW v_datacenters AS
SELECT * FROM get_datacenters(TRUE);

COMMENT ON VIEW v_datacenters IS '活跃机房列表视图';

-- ============================================================================
-- 第四部分: 初始化默认机房
-- ============================================================================

INSERT INTO system_datacenter (name, region, api_endpoint)
VALUES ('beijing', 'cn-north', 'https://imboy-beijing.internal')
ON CONFLICT (name) DO NOTHING;

INSERT INTO system_datacenter (name, region, api_endpoint)
VALUES
    ('shanghai', 'cn-east', 'https://imboy-shanghai.internal'),
    ('shenzhen', 'cn-south', 'https://imboy-shenzhen.internal')
ON CONFLICT (name) DO NOTHING;

-- ============================================================================
-- 完成标记
-- ============================================================================
DO $$
BEGIN
    RAISE NOTICE '===========================================';
    RAISE NOTICE 'ID段服务创建完成';
    RAISE NOTICE '版本: 1.0.0';
    RAISE NOTICE '创建时间: %', NOW();
    RAISE NOTICE '===========================================';
    RAISE NOTICE '创建内容:';
    RAISE NOTICE '  - 4张数据表';
    RAISE NOTICE '  - 10个管理函数';
    RAISE NOTICE '  - 2个监控视图';
    RAISE NOTICE '  - 3个默认机房';
    RAISE NOTICE '===========================================';
    RAISE NOTICE '使用示例:';
    RAISE NOTICE '  -- 注册新机房';
    RAISE NOTICE '  SELECT register_datacenter(''guangzhou'', ''cn-south'');';
    RAISE NOTICE '  ';
    RAISE NOTICE '  -- 初始化序列 (datacenter_id = 1)';
    RAISE NOTICE '  SELECT * FROM init_all_sequences(1);';
    RAISE NOTICE '  ';
    RAISE NOTICE '  -- 查看ID段状态';
    RAISE NOTICE '  SELECT * FROM v_id_segment_monitor;';
    RAISE NOTICE '  ';
    RAISE NOTICE '  -- 查看机房列表';
    RAISE NOTICE '  SELECT * FROM v_datacenters;';
    RAISE NOTICE '===========================================';
END $$;
