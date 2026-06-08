-- ============================================================
-- 合并迁移 000001: foundation
-- 由 70 个历史迁移基线压缩而成 (fresh-install 等价)。
-- 本文件由 erlang_migrate 整体包裹在单事务中执行。
-- ============================================================


--

CREATE FUNCTION public.check_e2ee_shard_transmission_anomaly(p_key_version character varying, p_shard_id character varying) RETURNS TABLE(anomaly_type text, description text, details jsonb)
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.check_e2ee_shard_transmission_anomaly(p_key_version character varying, p_shard_id character varying) IS '检查分片传输异常';


--;

--

CREATE FUNCTION public.cleanup_expired_channel_invitations() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    updated_count INTEGER;
BEGIN
    -- 将过期的待处理邀请标记为已过期
    UPDATE public.channel_invitation
    SET status = 3, updated_at = NOW()
    WHERE status = 0 AND expires_at < NOW();

    GET DIAGNOSTICS updated_count = ROW_COUNT;
    RETURN updated_count;
END;
$$;


--;

--

COMMENT ON FUNCTION public.cleanup_expired_channel_invitations() IS '清理过期的频道邀请';


--;

--

CREATE FUNCTION public.cleanup_expired_channel_orders() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    updated_count INTEGER;
BEGIN
    -- 将过期的待支付订单标记为已过期
    UPDATE public.channel_order
    SET status = 4, updated_at = NOW()
    WHERE status = 0 AND expires_at < NOW();

    GET DIAGNOSTICS updated_count = ROW_COUNT;
    RETURN updated_count;
END;
$$;


--;

--

COMMENT ON FUNCTION public.cleanup_expired_channel_orders() IS '清理过期的频道订单';


--;

--

CREATE FUNCTION public.cleanup_expired_transfer_sessions() RETURNS integer
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.cleanup_expired_transfer_sessions() IS '清理过期的传输会话';


--;

--

CREATE FUNCTION public.cleanup_old_shard_transmission_logs() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    -- 删除 90 天前的传输日志
    DELETE FROM e2ee_shard_transmission_log
    WHERE created_at < NOW() - INTERVAL '90 days';

    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$;


--;

--

COMMENT ON FUNCTION public.cleanup_old_shard_transmission_logs() IS '清理 90 天前的分片传输日志';


--;

--

CREATE FUNCTION public.cleanup_used_social_shards() RETURNS integer
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.cleanup_used_social_shards() IS '清理已使用超过 90 天的旧分片';


--;

--

CREATE FUNCTION public.collect_id_segment_stats(p_dc_id smallint DEFAULT NULL::smallint) RETURNS integer
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.collect_id_segment_stats(p_dc_id smallint) IS '记录ID段使用统计 - 定时任务调用';


--;

--

CREATE FUNCTION public.fn_channel_invitation_accept() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- 当邀请被接受时，自动创建订阅关系
    IF NEW.status = 1 AND OLD.status = 0 THEN
        INSERT INTO public.channel_subscription (channel_id, user_id, subscribed_at, status)
        VALUES (NEW.channel_id, NEW.invitee_uid, NOW(), 1)
        ON CONFLICT (channel_id, user_id)
        DO UPDATE SET status = 1, subscribed_at = NOW();

        -- 更新邀请人的 accepted_at
        NEW.accepted_at := NOW();
    END IF;
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.fn_channel_subscribe_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.fn_moment_touch_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.fn_report_ticket_touch_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.fn_update_channel_message_reaction_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE public.channel_message cm
    SET reaction_summary = (
        SELECT json_object_agg(reaction_type, cnt)
        FROM (
            SELECT reaction_type, COUNT(*) as cnt
            FROM public.channel_reaction
            WHERE message_id = COALESCE(NEW.message_id, OLD.message_id)
            GROUP BY reaction_type
        ) sub
    )
    WHERE id = COALESCE(NEW.message_id, OLD.message_id);
    RETURN COALESCE(NEW, OLD);
END;
$$;


--;

--

CREATE FUNCTION public.fn_update_channel_message_view_count() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE public.channel_message
    SET view_count = view_count + 1
    WHERE id = NEW.message_id;
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.generate_channel_invitation_code() RETURNS character varying
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_code VARCHAR(64);
    v_chars VARCHAR(36) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
BEGIN
    -- 生成8位随机码
    v_code := '';
    FOR i IN 1..8 LOOP
        v_code := v_code || SUBSTRING(v_chars FROM FLOOR(RANDOM() * 36 + 1)::INT FOR 1);
    END LOOP;
    RETURN v_code;
END;
$$;


--;

--

COMMENT ON FUNCTION public.generate_channel_invitation_code() IS '生成频道邀请码';


--;

--

CREATE FUNCTION public.generate_channel_order_no() RETURNS character varying
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_order_no VARCHAR(64);
    v_timestamp BIGINT;
    v_random VARCHAR(8);
BEGIN
    -- 格式: CH + 时间戳(13位) + 随机数(6位)
    v_timestamp := EXTRACT(EPOCH FROM NOW()) * 1000;
    v_random := LPAD(FLOOR(RANDOM() * 1000000)::TEXT, 6, '0');
    v_order_no := 'CH' || v_timestamp::BIGINT::TEXT || v_random;
    RETURN v_order_no;
END;
$$;


--;

--

COMMENT ON FUNCTION public.generate_channel_order_no() IS '生成频道订单号';


--;

--

CREATE FUNCTION public.get_datacenter_segment_status(p_dc_id smallint DEFAULT NULL::smallint, p_table_name character varying DEFAULT NULL::character varying) RETURNS TABLE(datacenter_id smallint, datacenter_name character varying, datacenter_region character varying, table_name character varying, segment_start bigint, segment_end bigint, total_size bigint, used_count integer, usage_percent numeric, is_active boolean, allocated_at timestamp with time zone, expired_at timestamp with time zone, remaining_days integer, status character varying)
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.get_datacenter_segment_status(p_dc_id smallint, p_table_name character varying) IS '查看ID段使用状态 - 监控面板使用';


--;

--

CREATE FUNCTION public.get_datacenters(p_active_only boolean DEFAULT true) RETURNS TABLE(id smallint, name character varying, region character varying, api_endpoint character varying, is_active boolean, created_at timestamp with time zone, updated_at timestamp with time zone, active_tables_count integer)
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.get_datacenters(p_active_only boolean) IS '获取机房列表';


--;

--

CREATE FUNCTION public.get_or_allocate_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer DEFAULT 100000, p_expire_days integer DEFAULT 30) RETURNS TABLE(segment_start bigint, segment_end bigint)
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.get_or_allocate_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer, p_expire_days integer) IS '获取或分配ID段 - 自动续期 (核心函数)';


--;

--

CREATE FUNCTION public.imboy_msg_c2g_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    DELETE FROM public.msg_c2g_timeline WHERE msg_id = OLD.msg_id;
    return OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    return NEW;
  ELSIF (TG_OP = 'INSERT') THEN
  return NEW;
  END IF;
end;
$$;


--;

--

CREATE FUNCTION public.imboy_user_collect_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.attachment SET referer_time = referer_time - 1 WHERE md5 = any(string_to_array(OLD.attach_md5, ','));
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_collect 业务上不会有单独修改md5的可能性
    -- 所以不要考虑md5字段修改的情况
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
  return NEW;
  END IF;
end;
$$;


--;

--

CREATE FUNCTION public.imboy_user_for_fts_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE') THEN
    DELETE FROM public.fts_user WHERE user_id = OLD.id;
    DELETE FROM public.user_setting WHERE user_id = OLD.id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    INSERT INTO public.fts_user (user_id, allow_search, token) VALUES (new.id, 2, setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C'))
    ON CONFLICT (user_id) DO UPDATE SET token = setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C');
    -- UPDATE public.fts_user SET token = setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
    --       setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
    --       setweight(to_tsvector('jiebacfg', new.region), 'B') WHERE user_id=NEW.id;
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    INSERT INTO public.fts_user (user_id, allow_search, token) VALUES (new.id, 1, setweight(to_tsvector('jiebacfg', new.nickname), 'A') ||
          setweight(to_tsvector('jiebacfg', new.sign), 'B') ||
          setweight(to_tsvector('jiebacfg', new.region), 'C'));
  return new;
  END IF;
end;
$$;


--;

--

CREATE FUNCTION public.imboy_user_tag_relation_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.user_tag SET referer_time = referer_time - 1 WHERE creator_user_id = OLD.user_id AND scene = OLD.scene and id = OLD.tag_id;
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_tag 业务上不会有单独修改name的可能性
    -- 所以不要考虑name字段修改的情况
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    UPDATE public.user_tag SET referer_time = referer_time + 1 WHERE creator_user_id = NEW.user_id AND scene = NEW.scene and id = NEW.tag_id;
  return NEW;
  END IF;
end;
$$;


--;

--

CREATE FUNCTION public.init_all_sequences(p_dc_id smallint) RETURNS TABLE(table_name character varying, sequence_name character varying, segment_start bigint, segment_end bigint, status boolean, message character varying)
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.init_all_sequences(p_dc_id smallint) IS '批量初始化所有表的序列 - 部署时一键执行';


--;

--

CREATE FUNCTION public.init_sequence_from_segment(p_table_name character varying, p_dc_id smallint) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.init_sequence_from_segment(p_table_name character varying, p_dc_id smallint) IS '从ID段初始化序列 - 应用启动时调用';


--;

--

CREATE FUNCTION public.reactivate_datacenter(p_dc_id smallint) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.reactivate_datacenter(p_dc_id smallint) IS '重新激活已注销的机房';


--;

--

CREATE FUNCTION public.register_datacenter(p_name character varying, p_region character varying, p_api_endpoint character varying DEFAULT NULL::character varying) RETURNS smallint
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.register_datacenter(p_name character varying, p_region character varying, p_api_endpoint character varying) IS '注册新机房 - 返回机房ID';


--;

--

CREATE FUNCTION public.renew_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer DEFAULT 100000) RETURNS TABLE(segment_start bigint, segment_end bigint)
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.renew_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer) IS '手动续期ID段 - 紧急情况使用';


--;

--

CREATE FUNCTION public.set_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.sync_fts_user() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF (TG_OP = 'DELETE') THEN
        DELETE FROM public.fts_user WHERE user_id = OLD.id;
        RETURN OLD;
    END IF;
    INSERT INTO public.fts_user (user_id, token)
    VALUES (
        NEW.id,
        to_tsvector('jiebacfg',
            COALESCE(NEW.nickname, '') || ' ' ||
            COALESCE(NEW.account,  '') || ' ' ||
            COALESCE(NEW.mobile,   '') || ' ' ||
            COALESCE(NEW.sign,     '') || ' ' ||
            COALESCE(NEW.region,   '')
        )
    )
    ON CONFLICT (user_id) DO UPDATE
        SET token = to_tsvector('jiebacfg',
            COALESCE(NEW.nickname, '') || ' ' ||
            COALESCE(NEW.account,  '') || ' ' ||
            COALESCE(NEW.mobile,   '') || ' ' ||
            COALESCE(NEW.sign,     '') || ' ' ||
            COALESCE(NEW.region,   '')
        );
    RETURN NEW;
END;
$$;


--;

--

CREATE FUNCTION public.unregister_datacenter(p_dc_id smallint) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
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
$$;


--;

--

COMMENT ON FUNCTION public.unregister_datacenter(p_dc_id smallint) IS '注销机房 - 软删除，保留数据';


--;

--

CREATE FUNCTION public.update_e2ee_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--;

--

CREATE SEQUENCE public.system_datacenter_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--;

--

CREATE SEQUENCE public.user_dnd_rule_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--;

--

CREATE TABLE public.config (
    tab character varying(20) NOT NULL,
    key character varying(40) NOT NULL,
    value text NOT NULL,
    title character varying(40) NOT NULL,
    sort integer DEFAULT 20 NOT NULL,
    remark character varying(200) DEFAULT ''::character varying NOT NULL,
    system boolean DEFAULT false NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_config_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.config
    ADD CONSTRAINT config_pkey PRIMARY KEY (key);


--;

--

COMMENT ON TABLE public.config IS '系统配置';


--;

--

COMMENT ON COLUMN public.config.tab IS '配置选项，便于后台分类浏览';


--;

--

COMMENT ON COLUMN public.config.key IS '主键';


--;

--

COMMENT ON COLUMN public.config.title IS '标题';


--;

--

COMMENT ON COLUMN public.config.sort IS '排序 降序排序，大的值在前面';


--;

--

COMMENT ON COLUMN public.config.system IS '是否为系统配置，系统配置不可删除';


--;

--

COMMENT ON COLUMN public.config.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.config.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.config.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.system_datacenter (
    id integer NOT NULL,
    name character varying(50) NOT NULL,
    region character varying(50) NOT NULL,
    api_endpoint character varying(255),
    is_active boolean DEFAULT true,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--;

--

ALTER SEQUENCE public.system_datacenter_id_seq OWNED BY public.system_datacenter.id;


--;

--

ALTER TABLE ONLY public.system_datacenter ALTER COLUMN id SET DEFAULT nextval('public.system_datacenter_id_seq'::regclass);


--;

--

ALTER TABLE ONLY public.system_datacenter
    ADD CONSTRAINT system_datacenter_name_key UNIQUE (name);


--;

--

ALTER TABLE ONLY public.system_datacenter
    ADD CONSTRAINT system_datacenter_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_datacenter_active ON public.system_datacenter USING btree (is_active);


--;

--

CREATE INDEX i_datacenter_region ON public.system_datacenter USING btree (region);


--;

--

COMMENT ON TABLE public.system_datacenter IS '机房注册表 - 支持动态增删';


--;

--

COMMENT ON COLUMN public.system_datacenter.id IS '机房ID (0-32767), 由数据库自动分配';


--;

--

COMMENT ON COLUMN public.system_datacenter.name IS '机房名称，业务标识，如: beijing, shanghai, shenzhen';


--;

--

COMMENT ON COLUMN public.system_datacenter.region IS '机房所在区域，如: cn-north, cn-east, cn-south';


--;

--

COMMENT ON COLUMN public.system_datacenter.api_endpoint IS '机房API地址，用于健康检查和数据同步';


--;

--

COMMENT ON COLUMN public.system_datacenter.is_active IS '是否激活，注销时设置为FALSE（软删除）';


--;

--

COMMENT ON COLUMN public.system_datacenter.created_at IS '机房注册时间';


--;

--

COMMENT ON COLUMN public.system_datacenter.updated_at IS '机房信息最后更新时间';


--;

INSERT INTO public.system_datacenter (name, region, api_endpoint)
VALUES ('beijing', 'cn-north', 'https://imboy-beijing.internal')
ON CONFLICT (name) DO NOTHING;
INSERT INTO public.system_datacenter (name, region, api_endpoint)
VALUES
    ('shanghai', 'cn-east', 'https://imboy-shanghai.internal'),
    ('shenzhen', 'cn-south', 'https://imboy-shenzhen.internal')
ON CONFLICT (name) DO NOTHING;


--

CREATE TABLE public.system_datacenter_log (
    id bigint NOT NULL,
    datacenter_id smallint,
    action character varying(20) NOT NULL,
    details jsonb,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--;

--

ALTER TABLE ONLY public.system_datacenter_log
    ADD CONSTRAINT system_datacenter_log_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_dclog_action ON public.system_datacenter_log USING btree (action);


--;

--

CREATE INDEX i_dclog_created_at ON public.system_datacenter_log USING btree (created_at DESC);


--;

--

CREATE INDEX i_dclog_dc_id ON public.system_datacenter_log USING btree (datacenter_id);


--;

--

COMMENT ON TABLE public.system_datacenter_log IS '机房变更审计日志 - 记录所有注册/注销/修改操作';


--;

--

COMMENT ON COLUMN public.system_datacenter_log.id IS '日志记录ID';


--;

--

COMMENT ON COLUMN public.system_datacenter_log.datacenter_id IS '机房ID';


--;

--

COMMENT ON COLUMN public.system_datacenter_log.action IS '操作类型: register(注册), unregister(注销), modify(修改)';


--;

--

COMMENT ON COLUMN public.system_datacenter_log.details IS '详细信息，JSONB格式';


--;

--

COMMENT ON COLUMN public.system_datacenter_log.created_at IS '操作时间';


--;

--

CREATE TABLE public.system_id_segment (
    id bigint NOT NULL,
    datacenter_id smallint NOT NULL,
    table_name character varying(50) NOT NULL,
    segment_start bigint NOT NULL,
    segment_end bigint NOT NULL,
    allocated_size integer NOT NULL,
    used_count integer DEFAULT 0,
    is_active boolean DEFAULT true,
    allocated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    expired_at timestamp with time zone
);


--;

--

ALTER TABLE ONLY public.system_id_segment
    ADD CONSTRAINT system_id_segment_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.system_id_segment
    ADD CONSTRAINT uk_datacenter_table_active UNIQUE (datacenter_id, table_name);


--;

--

CREATE INDEX i_id_segment_active ON public.system_id_segment USING btree (is_active, expired_at) WHERE (is_active = true);


--;

--

CREATE INDEX i_id_segment_allocated_at ON public.system_id_segment USING btree (allocated_at DESC);


--;

--

CREATE INDEX i_id_segment_dc_table ON public.system_id_segment USING btree (datacenter_id, table_name);


--;

--

CREATE INDEX i_id_segment_table ON public.system_id_segment USING btree (table_name);


--;

--

COMMENT ON TABLE public.system_id_segment IS 'ID段分配记录 - 每个机房每张表维护独立的ID段';


--;

--

COMMENT ON COLUMN public.system_id_segment.id IS '分配记录ID';


--;

--

COMMENT ON COLUMN public.system_id_segment.datacenter_id IS '所属机房ID';


--;

--

COMMENT ON COLUMN public.system_id_segment.table_name IS '表名，如: user, group, msg_c2c';


--;

--

COMMENT ON COLUMN public.system_id_segment.segment_start IS 'ID段起始值 (包含)';


--;

--

COMMENT ON COLUMN public.system_id_segment.segment_end IS 'ID段结束值 (包含)';


--;

--

COMMENT ON COLUMN public.system_id_segment.allocated_size IS '分配的ID总数';


--;

--

COMMENT ON COLUMN public.system_id_segment.used_count IS '已使用的ID数量 (估算值，用于监控)';


--;

--

COMMENT ON COLUMN public.system_id_segment.is_active IS '是否为当前活跃段';


--;

--

COMMENT ON COLUMN public.system_id_segment.allocated_at IS 'ID段分配时间';


--;

--

COMMENT ON COLUMN public.system_id_segment.expired_at IS '过期时间，NULL 表示永不过期';


--;

--

CREATE TABLE public.system_id_segment_stats (
    id bigint NOT NULL,
    datacenter_id smallint NOT NULL,
    table_name character varying(50) NOT NULL,
    stats_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    segment_start bigint,
    segment_end bigint,
    used_count integer,
    usage_percent numeric(5,2),
    allocate_rate_per_sec numeric(10,2)
);


--;

--

ALTER TABLE ONLY public.system_id_segment_stats
    ADD CONSTRAINT system_id_segment_stats_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_idstats_dc_table_time ON public.system_id_segment_stats USING btree (datacenter_id, table_name, stats_time DESC);


--;

--

COMMENT ON TABLE public.system_id_segment_stats IS 'ID段使用统计表 - 用于监控和趋势分析';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.datacenter_id IS '机房ID';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.table_name IS '表名';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.stats_time IS '统计时间';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.segment_start IS '当前使用的ID段起始值';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.segment_end IS '当前使用的ID段结束值';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.used_count IS '使用量估算';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.usage_percent IS '使用率百分比';


--;

--

COMMENT ON COLUMN public.system_id_segment_stats.allocate_rate_per_sec IS '每秒分配速率';


--;

--

CREATE TABLE public.app_ddl (
    id bigint NOT NULL,
    ddl text NOT NULL,
    down_ddl text NOT NULL,
    admin_user_id bigint DEFAULT 0 NOT NULL,
    old_vsn integer DEFAULT 0 NOT NULL,
    new_vsn integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_app_ddl_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.app_ddl
    ADD CONSTRAINT app_ddl_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.app_ddl
    ADD CONSTRAINT uk_oldvsn_newvsn UNIQUE (old_vsn, new_vsn);


--;

--

CREATE INDEX i_ddl_status_newvsn ON public.app_ddl USING btree (status, new_vsn);


--;

--

CREATE INDEX idx_app_ddl_admin_user_id ON public.app_ddl USING btree (admin_user_id);


--;

--

COMMENT ON TABLE public.app_ddl IS 'APP sqlite3 数据库 DDL 语句版本管理表';


--;

--

COMMENT ON COLUMN public.app_ddl.id IS '主键 自增长ID 反馈ID';


--;

--

COMMENT ON COLUMN public.app_ddl.ddl IS '需要更新的DDL语句文本，每个SQL半角逗号 ; 分割，因为DDL语句可能有顺序要求，所以用 text类型，而不用 json 数据类型';


--;

--

COMMENT ON COLUMN public.app_ddl.old_vsn IS '版本号： 整形数字';


--;

--

COMMENT ON COLUMN public.app_ddl.new_vsn IS '版本号： 整形数字';


--;

--

COMMENT ON COLUMN public.app_ddl.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.app_ddl.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.app_ddl.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.app_version (
    id bigint NOT NULL,
    region_code character varying(40) DEFAULT 'cn'::character varying NOT NULL,
    type character varying(40) NOT NULL,
    package_name character varying(80),
    app_name character varying(80),
    vsn character varying(40),
    sign_key character varying(80),
    download_url text,
    description text NOT NULL,
    force_update boolean DEFAULT false NOT NULL,
    sort integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    min_supported_vsn character varying(40) DEFAULT '0.0.0'::character varying NOT NULL,
    grayscale_percent smallint DEFAULT 100 NOT NULL,
    upgrade_type character varying(20) DEFAULT 'recommend'::character varying NOT NULL,
    changelog jsonb DEFAULT '[]'::jsonb NOT NULL,
    file_size bigint DEFAULT 0 NOT NULL,
    file_hash character varying(128) DEFAULT ''::character varying NOT NULL,
    CONSTRAINT chk_app_version_grayscale CHECK (((grayscale_percent >= 0) AND (grayscale_percent <= 100))),
    CONSTRAINT chk_app_version_upgrade_type CHECK (((upgrade_type)::text = ANY ((ARRAY['force'::character varying, 'recommend'::character varying, 'silent'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.app_version
    ADD CONSTRAINT app_version_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.app_version
    ADD CONSTRAINT uk_vsn_pkgname_type UNIQUE (vsn, package_name, type);


--;

--

CREATE INDEX i_vsn_sort_updatedat ON public.app_version USING btree (sort, updated_at);


--;

--

CREATE INDEX i_vsn_status_type_regioncode ON public.app_version USING btree (status, type, region_code);


--;

--

COMMENT ON TABLE public.app_version IS 'APP版本管理表';


--;

--

COMMENT ON COLUMN public.app_version.id IS '主键 自增长ID 反馈ID';


--;

--

COMMENT ON COLUMN public.app_version.region_code IS 'The two-letter country code cn en  参考 https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2';


--;

--

COMMENT ON COLUMN public.app_version.type IS 'APP类型 web ios android macos windows';


--;

--

COMMENT ON COLUMN public.app_version.package_name IS '包名 | Bundle ID';


--;

--

COMMENT ON COLUMN public.app_version.app_name IS 'APP名称';


--;

--

COMMENT ON COLUMN public.app_version.vsn IS '版本号： x.y.z格式';


--;

--

COMMENT ON COLUMN public.app_version.sign_key IS 'APP签名密码';


--;

--

COMMENT ON COLUMN public.app_version.download_url IS '下载地址';


--;

--

COMMENT ON COLUMN public.app_version.description IS '描述';


--;

--

COMMENT ON COLUMN public.app_version.force_update IS '是否强制升级 1 是  2 否';


--;

--

COMMENT ON COLUMN public.app_version.sort IS '排序，值越大越靠前： major * 1_000_000 + minor * 1_000 + patch';


--;

--

COMMENT ON COLUMN public.app_version.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.app_version.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.app_version.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.app_version.min_supported_vsn IS '最低支持版本：低于此版本的客户端必须升级';


--;

--

COMMENT ON COLUMN public.app_version.grayscale_percent IS '灰度发布比例：0-100，100 表示全量发布';


--;

--

COMMENT ON COLUMN public.app_version.upgrade_type IS '升级类型：force=强制 / recommend=推荐 / silent=静默';


--;

--

COMMENT ON COLUMN public.app_version.changelog IS '结构化更新日志 JSON 数组';


--;

--

COMMENT ON COLUMN public.app_version.file_size IS '安装包文件大小（字节）';


--;

--

COMMENT ON COLUMN public.app_version.file_hash IS '安装包 SHA256 校验值';


--;

--

CREATE TABLE public.app_version_policy (
    id bigint NOT NULL,
    type character varying(20) NOT NULL,
    min_vsn character varying(40) DEFAULT '0.0.0'::character varying NOT NULL,
    grayscale_enabled boolean DEFAULT false NOT NULL,
    check_interval_hours integer DEFAULT 24 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.app_version_policy
    ADD CONSTRAINT app_version_policy_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.app_version_policy
    ADD CONSTRAINT uk_app_version_policy_type UNIQUE (type);


--;

--

COMMENT ON TABLE public.app_version_policy IS 'APP版本策略配置表（每个平台一条记录）';


--;

--

COMMENT ON COLUMN public.app_version_policy.id IS '主键 TSID';


--;

--

COMMENT ON COLUMN public.app_version_policy.type IS '平台类型：android / ios / web';


--;

--

COMMENT ON COLUMN public.app_version_policy.min_vsn IS '全局最低支持版本（兜底安全网）';


--;

--

COMMENT ON COLUMN public.app_version_policy.grayscale_enabled IS '是否启用灰度：0=关闭直接全量 1=启用';


--;

--

COMMENT ON COLUMN public.app_version_policy.check_interval_hours IS '客户端版本检查间隔（小时）';


--;

--

COMMENT ON COLUMN public.app_version_policy.status IS '状态：0=禁用 1=启用';


--;

--

COMMENT ON COLUMN public.app_version_policy.updated_at IS '最后更新时间';


--;

--

COMMENT ON COLUMN public.app_version_policy.created_at IS '创建时间';


--;

INSERT INTO public.app_version_policy (id, type, min_vsn, grayscale_enabled, check_interval_hours, status)
VALUES
    (1, 'android', '0.0.0', false, 24, 1),
    (2, 'ios',     '0.0.0', false, 24, 1),
    (3, 'web',     '0.0.0', false, 24, 1)
ON CONFLICT (type) DO NOTHING;


--

CREATE TABLE public.app_upgrade_log (
    id bigint NOT NULL,
    did character varying(128) DEFAULT ''::character varying NOT NULL,
    uid bigint DEFAULT 0 NOT NULL,
    cos character varying(20) DEFAULT ''::character varying NOT NULL,
    client_vsn character varying(40) DEFAULT ''::character varying NOT NULL,
    target_vsn character varying(40) DEFAULT ''::character varying NOT NULL,
    event character varying(40) DEFAULT ''::character varying NOT NULL,
    upgrade_type character varying(20) DEFAULT ''::character varying NOT NULL,
    extra jsonb DEFAULT '{}'::jsonb,
    created_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_app_upgrade_log_cos CHECK (((cos)::text = ANY ((ARRAY['android'::character varying, 'ios'::character varying, 'web'::character varying, 'macos'::character varying])::text[]))),
    CONSTRAINT chk_app_upgrade_log_event CHECK (((event)::text = ANY ((ARRAY['check'::character varying, 'prompted'::character varying, 'download_start'::character varying, 'download_done'::character varying, 'verify_ok'::character varying, 'verify_fail'::character varying, 'install'::character varying, 'cancel'::character varying, 'error'::character varying])::text[]))),
    CONSTRAINT chk_app_upgrade_log_upgrade_type CHECK (((upgrade_type)::text = ANY ((ARRAY['force'::character varying, 'recommend'::character varying, 'silent'::character varying, 'none'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.app_upgrade_log
    ADD CONSTRAINT app_upgrade_log_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_app_upgrade_log_client_vsn ON public.app_upgrade_log USING btree (client_vsn);


--;

--

CREATE INDEX idx_app_upgrade_log_created_at ON public.app_upgrade_log USING btree (created_at);


--;

--

CREATE INDEX idx_app_upgrade_log_event ON public.app_upgrade_log USING btree (event, created_at);


--;

--

CREATE TABLE public.announcement (
    id bigint NOT NULL,
    adm_user_id bigint NOT NULL,
    title character varying(200) DEFAULT ''::character varying NOT NULL,
    body text DEFAULT ''::text NOT NULL,
    type character varying(20) DEFAULT 'info'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    pinned boolean DEFAULT false NOT NULL,
    published_at timestamp with time zone,
    expired_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_announcement_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1, 2]))),
    CONSTRAINT chk_announcement_type CHECK (((type)::text = ANY ((ARRAY['info'::character varying, 'warning'::character varying, 'important'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_announcement_adm_user_id ON public.announcement USING btree (adm_user_id);


--;

--

CREATE INDEX idx_announcement_created_at ON public.announcement USING btree (created_at DESC);


--;

--

CREATE INDEX idx_announcement_pinned_true ON public.announcement USING btree (created_at DESC) WHERE (pinned = true);


--;

--

CREATE INDEX idx_announcement_status_time ON public.announcement USING btree (status, created_at DESC);


--;

--

COMMENT ON TABLE public.announcement IS '全局公告表';


--;

--

COMMENT ON COLUMN public.announcement.id IS '主键ID';


--;

--

COMMENT ON COLUMN public.announcement.adm_user_id IS '创建者管理员ID';


--;

--

COMMENT ON COLUMN public.announcement.title IS '公告标题';


--;

--

COMMENT ON COLUMN public.announcement.body IS '公告内容';


--;

--

COMMENT ON COLUMN public.announcement.type IS '公告类型: info/warning/important';


--;

--

COMMENT ON COLUMN public.announcement.status IS '状态: -1 已删除, 0 草稿, 1 已发布, 2 已撤回';


--;

--

COMMENT ON COLUMN public.announcement.pinned IS '是否置顶: 0 否, 1 是';


--;

--

COMMENT ON COLUMN public.announcement.published_at IS '发布时间';


--;

--

COMMENT ON COLUMN public.announcement.expired_at IS '过期时间';


--;

--

COMMENT ON COLUMN public.announcement.updated_at IS '更新时间';


--;

--

COMMENT ON COLUMN public.announcement.created_at IS '创建时间';


--;

--

CREATE TABLE public.attachment (
    id bigint NOT NULL,
    md5 character varying(40) DEFAULT ''::character varying NOT NULL,
    mime_type character varying(40) DEFAULT ''::character varying NOT NULL,
    ext character varying(20) DEFAULT ''::character varying NOT NULL,
    name text DEFAULT ''::character varying NOT NULL,
    path text DEFAULT ''::character varying NOT NULL,
    url text DEFAULT ''::character varying NOT NULL,
    size bigint DEFAULT 0 NOT NULL,
    info jsonb DEFAULT '{}'::jsonb,
    referer_time bigint DEFAULT 0 NOT NULL,
    last_referer_user_id bigint DEFAULT 0 NOT NULL,
    last_referer_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    creator_user_id bigint DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    CONSTRAINT chk_attachment_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT uk_attachment_md5 UNIQUE (md5);


--;

--

CREATE INDEX i_attachment_creatoruserid_status ON public.attachment USING btree (creator_user_id, status);


--;

--

CREATE INDEX i_attachment_referer_created ON public.attachment USING btree (referer_time, created_at) WHERE (status = 1);


--;

--

COMMENT ON TABLE public.attachment IS '文件附件表';


--;

--

COMMENT ON COLUMN public.attachment.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.attachment.md5 IS '附件MD5';


--;

--

COMMENT ON COLUMN public.attachment.mime_type IS '附件mime_type';


--;

--

COMMENT ON COLUMN public.attachment.ext IS '附件 扩展名';


--;

--

COMMENT ON COLUMN public.attachment.name IS '附件 名称';


--;

--

COMMENT ON COLUMN public.attachment.path IS '附件 path';


--;

--

COMMENT ON COLUMN public.attachment.url IS '附件 访问地址';


--;

--

COMMENT ON COLUMN public.attachment.size IS '附件 大小，单位 ';


--;

--

COMMENT ON COLUMN public.attachment.info IS '附件信息json';


--;

--

COMMENT ON COLUMN public.attachment.referer_time IS '被引用次数';


--;

--

COMMENT ON COLUMN public.attachment.last_referer_user_id IS '最后引用用户';


--;

--

COMMENT ON COLUMN public.attachment.last_referer_at IS '最后引用时间';


--;

--

COMMENT ON COLUMN public.attachment.creator_user_id IS '创建人用户ID';


--;

--

COMMENT ON COLUMN public.attachment.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.attachment.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.attachment.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

CREATE TABLE public.push_token (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id character varying(128) NOT NULL,
    device_type character varying(16) NOT NULL,
    platform character varying(16) NOT NULL,
    token text NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_push_token_device_type CHECK (((device_type)::text = ANY ((ARRAY['android'::character varying, 'ios'::character varying, 'web'::character varying])::text[]))),
    CONSTRAINT chk_push_token_platform CHECK (((platform)::text = ANY ((ARRAY['fcm'::character varying, 'apns'::character varying, 'web_push'::character varying])::text[]))),
    CONSTRAINT chk_push_token_status CHECK ((status = ANY (ARRAY[0, 1])))
);


--;

--

ALTER TABLE ONLY public.push_token
    ADD CONSTRAINT push_token_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_push_token_user_id ON public.push_token USING btree (user_id) WHERE (status = 1);


--;

--

CREATE INDEX idx_push_token_uid ON public.push_token USING btree (user_id);


--;

--

CREATE UNIQUE INDEX uk_push_token_user_device ON public.push_token USING btree (user_id, device_id) WHERE (status = 1);


--;

--

COMMENT ON TABLE public.push_token IS '推送 Token 管理表';


--;

--

COMMENT ON COLUMN public.push_token.user_id IS '用户 ID';


--;

--

COMMENT ON COLUMN public.push_token.device_id IS '设备唯一标识';


--;

--

COMMENT ON COLUMN public.push_token.device_type IS '设备类型: android | ios | web';


--;

--

COMMENT ON COLUMN public.push_token.platform IS '推送平台: fcm | apns | web_push';


--;

--

COMMENT ON COLUMN public.push_token.token IS '推送 token';


--;

--

COMMENT ON COLUMN public.push_token.status IS '状态: 1=活跃 0=无效';


--;

--

CREATE TABLE public."user" (
    id bigint NOT NULL,
    level_id bigint DEFAULT 1 NOT NULL,
    nickname character varying(80) DEFAULT ''::character varying NOT NULL,
    password text NOT NULL,
    account character varying(80) NOT NULL,
    mobile character varying(40),
    email character varying(80),
    region character varying(80) DEFAULT ''::character varying NOT NULL,
    gender smallint DEFAULT 0 NOT NULL,
    experience bigint DEFAULT 0 NOT NULL,
    avatar text DEFAULT ''::character varying NOT NULL,
    sign text DEFAULT ''::character varying NOT NULL,
    ref_user_id bigint DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    reg_ip character varying(40) NOT NULL,
    reg_cosv text NOT NULL,
    ref_parent_user_id bigint DEFAULT 0 NOT NULL,
    source character varying(80) DEFAULT ''::character varying NOT NULL,
    birthday character varying(20) DEFAULT ''::character varying NOT NULL,
    dnd_enabled boolean DEFAULT false NOT NULL,
    CONSTRAINT chk_user_gender CHECK ((gender = ANY (ARRAY[0, 1, 2, 3]))),
    CONSTRAINT chk_user_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1, 2])))
);


--;

--

ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--;

--

CREATE UNIQUE INDEX uk_account ON public."user" USING btree (account);


--;

--

CREATE UNIQUE INDEX uk_email ON public."user" USING btree (email) WHERE ((email IS NOT NULL) AND ((email)::text <> ''::text));


--;

--

CREATE UNIQUE INDEX uk_mobile ON public."user" USING btree (mobile) WHERE ((mobile IS NOT NULL) AND ((mobile)::text <> ''::text));


--;

--

COMMENT ON TABLE public."user" IS '用户表';


--;

--

COMMENT ON COLUMN public."user".id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public."user".level_id IS '会员等级ID';


--;

--

COMMENT ON COLUMN public."user".nickname IS '用户昵称';


--;

--

COMMENT ON COLUMN public."user".password IS '经过加盐的密码，由 elib_password:generate/1 生成（HMAC-SHA512）';


--;

--

COMMENT ON COLUMN public."user".account IS '会员账号';


--;

--

COMMENT ON COLUMN public."user".mobile IS '手机号码';


--;

--

COMMENT ON COLUMN public."user".email IS '会员注册Email';


--;

--

COMMENT ON COLUMN public."user".region IS '地区：广东 深圳';


--;

--

COMMENT ON COLUMN public."user".gender IS '性别 1 男  2 女  3 保密';


--;

--

COMMENT ON COLUMN public."user".experience IS '经验值';


--;

--

COMMENT ON COLUMN public."user".avatar IS '头像';


--;

--

COMMENT ON COLUMN public."user".sign IS '用户签名';


--;

--

COMMENT ON COLUMN public."user".ref_user_id IS '推荐人ID，0表示无推荐人';


--;

--

COMMENT ON COLUMN public."user".status IS '状态: -1 删除  0 禁用  1 启用  2 申请注销中';


--;

--

COMMENT ON COLUMN public."user".created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public."user".reg_ip IS '注册IP';


--;

--

COMMENT ON COLUMN public."user".reg_cosv IS '客户端操作系统版本，例如： Linux 5.11.0-1018-gcp #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021 | "Windows 10 Pro" 10.0 (Build 19043)';


--;

--

COMMENT ON COLUMN public."user".ref_parent_user_id IS '推荐人的推荐人user id';


--;

--

COMMENT ON COLUMN public."user".source IS '注册来源标记';


--;

--

COMMENT ON COLUMN public."user".birthday IS '生日，格式 YYYY-MM-DD';


--;

--

COMMENT ON COLUMN public."user".dnd_enabled IS '全局免打扰开关，默认关闭';


--;

--

CREATE TABLE public.user_setting (
    user_id bigint NOT NULL,
    setting jsonb,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_setting
    ADD CONSTRAINT pk_user_setting_uid PRIMARY KEY (user_id);


--;

--

COMMENT ON TABLE public.user_setting IS '用户设置表';


--;

--

COMMENT ON COLUMN public.user_setting.user_id IS '主键 用户表自增长ID';


--;

--

COMMENT ON COLUMN public.user_setting.setting IS '更多设置：json 数据，不同的业务不用的key( add_friend_type 加我方式： mobile 手机号; account 账号; qrcode 二维码; group 群聊; visit_card 名片)';


--;

--

COMMENT ON COLUMN public.user_setting.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.user_device (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_type character varying(40) DEFAULT ''::character varying,
    device_id character varying(40) NOT NULL,
    device_vsn text,
    device_name character varying(80),
    login_count integer DEFAULT 0 NOT NULL,
    last_login_ip character varying(40) DEFAULT ''::character varying NOT NULL,
    last_login_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    last_active_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    public_key character varying(2048) DEFAULT ''::character varying,
    key_id character varying(255) DEFAULT ''::character varying
);


--;

--

ALTER TABLE ONLY public.user_device
    ADD CONSTRAINT uk_userid_deviceid UNIQUE (user_id, device_id);


--;

--

ALTER TABLE ONLY public.user_device
    ADD CONSTRAINT user_device_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_user_device_key_id ON public.user_device USING btree (key_id) WHERE ((key_id)::text <> ''::text);


--;

--

CREATE INDEX idx_user_device_uid ON public.user_device USING btree (user_id);


--;

--

COMMENT ON TABLE public.user_device IS '用户登录设备表';


--;

--

COMMENT ON COLUMN public.user_device.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_device.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.user_device.device_type IS '设备类型 web ios android macos windows';


--;

--

COMMENT ON COLUMN public.user_device.device_id IS '设备ID web设备留空';


--;

--

COMMENT ON COLUMN public.user_device.device_vsn IS '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}';


--;

--

COMMENT ON COLUMN public.user_device.device_name IS '设备名称（用户可修改的）';


--;

--

COMMENT ON COLUMN public.user_device.login_count IS '登陆次数';


--;

--

COMMENT ON COLUMN public.user_device.last_login_ip IS '最后登陆IP';


--;

--

COMMENT ON COLUMN public.user_device.last_login_at IS '最后登录UTC时间';


--;

--

COMMENT ON COLUMN public.user_device.last_active_at IS '最近活跃时间';


--;

--

COMMENT ON COLUMN public.user_device.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.user_device.created_at IS '创建记录UTC时间';


--;

--

COMMENT ON COLUMN public.user_device.public_key IS '用户设备公钥';


--;

--

COMMENT ON COLUMN public.user_device.key_id IS 'E2EE 密钥版本标识符，用于标识密钥变更';


--;

--

CREATE TABLE public.user_collect (
    id bigint NOT NULL,
    user_id bigint DEFAULT 0 NOT NULL,
    kind integer DEFAULT 0 NOT NULL,
    kind_id character varying(40) DEFAULT ''::character varying NOT NULL,
    source text DEFAULT ''::character varying NOT NULL,
    remark text DEFAULT ''::character varying NOT NULL,
    tag text DEFAULT ''::character varying,
    info text,
    attach_md5 text DEFAULT ''::character varying NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_user_collect_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.user_collect
    ADD CONSTRAINT uk_user_collect_uid_kind UNIQUE (user_id, kind_id);


--;

--

ALTER TABLE ONLY public.user_collect
    ADD CONSTRAINT uk_user_collect_userid_status_kindid UNIQUE (user_id, status, kind_id);


--;

--

ALTER TABLE ONLY public.user_collect
    ADD CONSTRAINT user_collect_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_user_collect_userid_status_kind ON public.user_collect USING btree (user_id, status, kind);


--;

--

COMMENT ON TABLE public.user_collect IS '用户收藏记录表';


--;

--

COMMENT ON COLUMN public.user_collect.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_collect.user_id IS '资源的收藏者';


--;

--

COMMENT ON COLUMN public.user_collect.kind IS 'Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片';


--;

--

COMMENT ON COLUMN public.user_collect.kind_id IS '资源唯一标识';


--;

--

COMMENT ON COLUMN public.user_collect.source IS '收藏来源';


--;

--

COMMENT ON COLUMN public.user_collect.remark IS '收藏者备注';


--;

--

COMMENT ON COLUMN public.user_collect.tag IS '多个tag 用半角逗号分隔，单个tag不超过14字符';


--;

--

COMMENT ON COLUMN public.user_collect.info IS '被收藏的kind的json信息';


--;

--

COMMENT ON COLUMN public.user_collect.attach_md5 IS '收藏记录Md5,多个用逗号分割';


--;

--

COMMENT ON COLUMN public.user_collect.status IS '状态: 0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.user_collect.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.user_collect.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.user_tag (
    id bigint NOT NULL,
    creator_user_id bigint NOT NULL,
    scene integer DEFAULT 0,
    name character varying(80) DEFAULT ''::character varying,
    referer_time integer DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_tag
    ADD CONSTRAINT uk_scene_creatorid_name UNIQUE (scene, creator_user_id, name);


--;

--

ALTER TABLE ONLY public.user_tag
    ADD CONSTRAINT user_tag_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_user_tag_creator_uid ON public.user_tag USING btree (creator_user_id);


--;

--

COMMENT ON TABLE public.user_tag IS 'tag记录表';


--;

--

COMMENT ON COLUMN public.user_tag.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_tag.creator_user_id IS '创建人用户ID';


--;

--

COMMENT ON COLUMN public.user_tag.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';


--;

--

COMMENT ON COLUMN public.user_tag.name IS '标签名称';


--;

--

COMMENT ON COLUMN public.user_tag.referer_time IS '被引用次数 关联object_id 数量';


--;

--

COMMENT ON COLUMN public.user_tag.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.user_tag.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.user_tag_relation (
    id bigint NOT NULL,
    scene integer DEFAULT 0 NOT NULL,
    user_id bigint DEFAULT 0 NOT NULL,
    tag_id bigint DEFAULT 0 NOT NULL,
    object_id character varying(40) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_tag_relation
    ADD CONSTRAINT user_tag_relation_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_user_tag_relation_scene_tagid ON public.user_tag_relation USING btree (scene, tag_id);


--;

--

CREATE UNIQUE INDEX uk_user_tag_relation_scene_userid_objectid_tagid ON public.user_tag_relation USING btree (scene, user_id, object_id, tag_id);


--;

--

COMMENT ON TABLE public.user_tag_relation IS '用户标签记录表 scene 场景下，user_id 给 object_id 打了标签 tag_id';


--;

--

COMMENT ON COLUMN public.user_tag_relation.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_tag_relation.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';


--;

--

COMMENT ON COLUMN public.user_tag_relation.user_id IS '记录所属用户ID';


--;

--

COMMENT ON COLUMN public.user_tag_relation.tag_id IS '标签ID public.tag 表的自增长ID';


--;

--

COMMENT ON COLUMN public.user_tag_relation.object_id IS '被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)';


--;

--

COMMENT ON COLUMN public.user_tag_relation.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.user_log (
    ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP(6) NOT NULL,
    type smallint NOT NULL,
    uid bigint DEFAULT 0 NOT NULL,
    body jsonb NOT NULL,
    remark character varying(200) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_user_log_type CHECK ((type = ANY (ARRAY[100, 102, 110, 901, 902, 903])))
);


--;

-- TimescaleDB: 将 user_log 转为 hypertable 并配置压缩/保留策略
SELECT public.create_hypertable('public.user_log', 'created_at', chunk_time_interval => INTERVAL '30 days', if_not_exists => TRUE, migrate_data => TRUE, create_default_indexes => FALSE);
ALTER TABLE public.user_log SET (timescaledb.compress, timescaledb.compress_segmentby = 'uid', timescaledb.compress_orderby = 'created_at DESC');
SELECT public.add_compression_policy('public.user_log', INTERVAL '3 days', if_not_exists => TRUE);
SELECT public.add_retention_policy('public.user_log', INTERVAL '90 days', if_not_exists => TRUE);


--

CREATE INDEX i_user_log_type_uid_createdat ON public.user_log USING btree (type, uid, created_at);


--;

--

CREATE INDEX user_log_created_at_idx ON public.user_log USING btree (created_at DESC);


--;

--

COMMENT ON TABLE public.user_log IS '用户日志表';


--;

--

COMMENT ON COLUMN public.user_log.type IS '日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码';


--;

--

COMMENT ON COLUMN public.user_log.uid IS '用户ID';


--;

--

COMMENT ON COLUMN public.user_log.body IS '相关操作类型的json字符串数据';


--;

--

COMMENT ON COLUMN public.user_log.remark IS '备注';


--;

--

COMMENT ON COLUMN public.user_log.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.fts_user (
    user_id bigint NOT NULL,
    allow_search smallint DEFAULT 2 NOT NULL,
    token tsvector,
    CONSTRAINT chk_fts_user_allow_search CHECK ((allow_search = ANY (ARRAY[1, 2])))
);


--;

--

ALTER TABLE ONLY public.fts_user
    ADD CONSTRAINT pk_fts_user_uid PRIMARY KEY (user_id);


--;

--

CREATE INDEX user_fts_gin_idex ON public.fts_user USING gin (token);


--;

--

COMMENT ON TABLE public.fts_user IS '用户全文索引矢量信息表';


--;

--

COMMENT ON COLUMN public.fts_user.user_id IS '用户唯一ID';


--;

--

COMMENT ON COLUMN public.fts_user.allow_search IS '用户允许被搜索 1 是  2 否';


--;

--

COMMENT ON COLUMN public.fts_user.token IS '搜索矢量信息';


--;

--

CREATE TABLE public.geo_people_nearby (
    user_id bigint NOT NULL,
    location public.geometry,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.geo_people_nearby
    ADD CONSTRAINT pk_people_nearby_uid PRIMARY KEY (user_id);


--;

--

CREATE INDEX i_people_nearby_location ON public.geo_people_nearby USING gist (location);


--;

--

CREATE INDEX idx_geo_updated_at ON public.geo_people_nearby USING btree (updated_at);


--;

--

COMMENT ON TABLE public.geo_people_nearby IS '附近的人';


--;

--

CREATE TABLE public.user_dnd_rule (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    start_min smallint DEFAULT 0 NOT NULL,
    end_min smallint DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER SEQUENCE public.user_dnd_rule_id_seq OWNED BY public.user_dnd_rule.id;


--;

--

ALTER TABLE ONLY public.user_dnd_rule ALTER COLUMN id SET DEFAULT nextval('public.user_dnd_rule_id_seq'::regclass);


--;

--

ALTER TABLE ONLY public.user_dnd_rule
    ADD CONSTRAINT user_dnd_rule_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_dnd_rule_status ON public.user_dnd_rule USING btree (user_id, status) WHERE (status = 1);


--;

--

CREATE UNIQUE INDEX uk_dnd_rule_userid ON public.user_dnd_rule USING btree (user_id);


--;

--

COMMENT ON COLUMN public.user_dnd_rule.user_id IS '用户ID (TSID bigint)';


--;

--

CREATE TABLE public.verification_code (
    id character varying(80) NOT NULL,
    code character varying(40) DEFAULT ''::character varying NOT NULL,
    validity_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.verification_code
    ADD CONSTRAINT verification_code_pkey PRIMARY KEY (id);


--;

--

COMMENT ON TABLE public.verification_code IS '验证码记录表';


--;

--

COMMENT ON COLUMN public.verification_code.id IS '主键 唯一标示';


--;

--

COMMENT ON COLUMN public.verification_code.code IS '随机验证码';


--;

--

COMMENT ON COLUMN public.verification_code.validity_at IS '有效期截止时间';


--;

--

COMMENT ON COLUMN public.verification_code.created_at IS '创建记录UTC时间';


--;

--

CREATE TABLE public.adm_user (
    id bigint NOT NULL,
    account character varying(80) NOT NULL,
    mobile character varying(40),
    email character varying(80),
    nickname character varying(80) DEFAULT ''::character varying NOT NULL,
    password text NOT NULL,
    avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    role_id bigint[],
    login_count bigint DEFAULT 0 NOT NULL,
    last_login_ip character varying(40) DEFAULT ''::character varying NOT NULL,
    last_login_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_adm_user_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.adm_user
    ADD CONSTRAINT adm_user_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_adm_user_role_id ON public.adm_user USING gin (role_id);


--;

--

CREATE UNIQUE INDEX uk_adm_account ON public.adm_user USING btree (account);


--;

--

CREATE UNIQUE INDEX uk_adm_email ON public.adm_user USING btree (email);


--;

--

CREATE UNIQUE INDEX uk_adm_mobile ON public.adm_user USING btree (mobile);


--;

--

COMMENT ON TABLE public.adm_user IS '运营用户表';


--;

--

COMMENT ON COLUMN public.adm_user.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.adm_user.account IS '会员账号';


--;

--

COMMENT ON COLUMN public.adm_user.mobile IS '手机号码';


--;

--

COMMENT ON COLUMN public.adm_user.email IS '会员注册Email';


--;

--

COMMENT ON COLUMN public.adm_user.nickname IS '用户昵称';


--;

--

COMMENT ON COLUMN public.adm_user.password IS '经过加盐的密码，由 elib_password:generate/1 生成（HMAC-SHA512）';


--;

--

COMMENT ON COLUMN public.adm_user.avatar IS '头像';


--;

--

COMMENT ON COLUMN public.adm_user.role_id IS '角色ID';


--;

--

COMMENT ON COLUMN public.adm_user.login_count IS '登陆次数';


--;

--

COMMENT ON COLUMN public.adm_user.last_login_ip IS '最后登陆IP';


--;

--

COMMENT ON COLUMN public.adm_user.last_login_at IS '最后登录时间';


--;

--

COMMENT ON COLUMN public.adm_user.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.adm_user.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.adm_role (
    id bigint NOT NULL,
    parent_id bigint DEFAULT 0 NOT NULL,
    sort integer DEFAULT 100 NOT NULL,
    role_name character varying(80) NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_adm_role_sort CHECK ((sort >= 0)),
    CONSTRAINT chk_adm_role_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.adm_role
    ADD CONSTRAINT adm_role_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_adm_role_parent_id ON public.adm_role USING btree (parent_id);


--;

--

CREATE UNIQUE INDEX uk_adm_role_name_active ON public.adm_role USING btree (role_name) WHERE (status <> '-1'::integer);


--;

--

COMMENT ON TABLE public.adm_role IS '运营用户角色表';


--;

--

COMMENT ON COLUMN public.adm_role.id IS '主键 自增长ID 角色ID';


--;

--

COMMENT ON COLUMN public.adm_role.parent_id IS '父级角色ID 0 未顶级角色';


--;

--

COMMENT ON COLUMN public.adm_role.sort IS '排序(数字越小越靠前)';


--;

--

COMMENT ON COLUMN public.adm_role.role_name IS '角色名称';


--;

--

COMMENT ON COLUMN public.adm_role.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.adm_role.updated_at IS '修改记录Unix时间戳毫秒单位';


--;

--

COMMENT ON COLUMN public.adm_role.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.user_friend (
    id bigint NOT NULL,
    from_user_id bigint NOT NULL,
    to_user_id bigint NOT NULL,
    category_id bigint DEFAULT 0,
    remark character varying(80) DEFAULT ''::character varying,
    tag text DEFAULT ''::character varying,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    setting jsonb,
    last_seen_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_user_friend_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public.user_friend
    ADD CONSTRAINT user_friend_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_status_fromuid_cid ON public.user_friend USING btree (status, from_user_id, category_id);


--;

--

CREATE INDEX i_user_friend_category_id ON public.user_friend USING btree (category_id) WHERE (category_id > 0);


--;

--

CREATE INDEX i_user_friend_from_user_id ON public.user_friend USING btree (from_user_id);


--;

--

CREATE UNIQUE INDEX uk_fromuid_touid ON public.user_friend USING btree (from_user_id, to_user_id);


--;

--

COMMENT ON TABLE public.user_friend IS '聊天好友关系记录表（A请求B为好友，B接受之后，系统要自动加入一条B请求A的记录并且A自动确认 user_id 是 user表的主键）';


--;

--

COMMENT ON COLUMN public.user_friend.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_friend.from_user_id IS '发起人 记录归属人ID';


--;

--

COMMENT ON COLUMN public.user_friend.to_user_id IS '接受人朋友用户ID';


--;

--

COMMENT ON COLUMN public.user_friend.category_id IS '用户分组ID friend_category主键';


--;

--

COMMENT ON COLUMN public.user_friend.remark IS '朋友备注名';


--;

--

COMMENT ON COLUMN public.user_friend.tag IS '给朋友的标签，多个tag 用半角逗号分隔，单个tag不超过14字符';


--;

--

COMMENT ON COLUMN public.user_friend.status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public.user_friend.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.user_friend.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.user_friend.setting IS '好友权限设置等信息';


--;

--

COMMENT ON COLUMN public.user_friend.last_seen_at IS '用户最后在线时间';


--;

--

CREATE TABLE public.user_friend_category (
    id bigint NOT NULL,
    name character varying(80) DEFAULT ''::character varying,
    owner_user_id bigint DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_friend_category
    ADD CONSTRAINT user_friend_category_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_user_friend_category_owner_uid ON public.user_friend_category USING btree (owner_user_id);


--;

--

COMMENT ON TABLE public.user_friend_category IS '朋友分组表';


--;

--

COMMENT ON COLUMN public.user_friend_category.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_friend_category.name IS '分组名称';


--;

--

COMMENT ON COLUMN public.user_friend_category.owner_user_id IS '分组所属用户ID';


--;

--

CREATE TABLE public.user_denylist (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    denied_user_id bigint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_denylist
    ADD CONSTRAINT uk_userid_denieduserid UNIQUE (user_id, denied_user_id);


--;

--

ALTER TABLE ONLY public.user_denylist
    ADD CONSTRAINT user_denylist_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_user_denylist_uid ON public.user_denylist USING btree (user_id);


--;

--

COMMENT ON TABLE public.user_denylist IS '用户的拒绝聊天名单';


--;

--

COMMENT ON COLUMN public.user_denylist.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_denylist.user_id IS '归属用户ID';


--;

--

COMMENT ON COLUMN public.user_denylist.denied_user_id IS '被列入名单的用户ID';


--;

--

COMMENT ON COLUMN public.user_denylist.created_at IS '创建记录UTC时间';


--;

--

CREATE TABLE public.conversation (
    id bigint NOT NULL,
    client_id bigint DEFAULT 0 NOT NULL,
    user_id bigint DEFAULT 0 NOT NULL,
    peer_id bigint DEFAULT 0 NOT NULL,
    avatar character varying(320) DEFAULT ''::character varying,
    title character varying(400) DEFAULT ''::character varying,
    subtitle character varying(400) DEFAULT ''::character varying,
    region character varying(80) DEFAULT ''::character varying,
    sign character varying(320) DEFAULT ''::character varying,
    unread_num integer DEFAULT 0,
    type character varying(40) DEFAULT ''::character varying,
    msg_type character varying(40) DEFAULT ''::character varying,
    is_show integer DEFAULT 0,
    last_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    last_msg_id bigint DEFAULT 0,
    last_msg_status integer DEFAULT 0,
    payload text DEFAULT ''::text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_conversation_type CHECK (((type)::text = ANY ((ARRAY[''::character varying, 'C2C'::character varying, 'C2G'::character varying, 'C2S'::character varying, 'S2C'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.conversation
    ADD CONSTRAINT conversation_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_conversation_client_id ON public.conversation USING btree (client_id);


--;

--

CREATE UNIQUE INDEX uk_cvt_userid_type_peerid ON public.conversation USING btree (user_id, type, peer_id);


--;

--

COMMENT ON TABLE public.conversation IS '客户端会话记录表';


--;

--

COMMENT ON COLUMN public.conversation.user_id IS '发起会话用户ID (TSID bigint)';


--;

--

COMMENT ON COLUMN public.conversation.peer_id IS '对端ID (TSID bigint)';


--;

--

COMMENT ON COLUMN public.conversation.type IS '会话类型 C2C C2G C2S S2C';


--;

--

COMMENT ON COLUMN public.conversation.last_msg_id IS '最后消息ID (TSID bigint)';


--;

--

CREATE TABLE public.conversation_pin (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    conversation_type character varying(10) NOT NULL,
    pinned_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_conversation_pin_type CHECK (((conversation_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.conversation_pin
    ADD CONSTRAINT conversation_pin_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.conversation_pin
    ADD CONSTRAINT conversation_pin_user_conversation_uk UNIQUE (user_id, conversation_id, conversation_type);


--;

--

CREATE INDEX idx_conversation_pin_conversation ON public.conversation_pin USING btree (conversation_id, conversation_type);


--;

--

CREATE INDEX idx_conversation_pin_user ON public.conversation_pin USING btree (user_id, pinned_at DESC);


--;

--

COMMENT ON TABLE public.conversation_pin IS '会话置顶表';


--;

--

COMMENT ON COLUMN public.conversation_pin.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.conversation_pin.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';


--;

--

COMMENT ON COLUMN public.conversation_pin.conversation_type IS '会话类型：c2c-单聊，c2g-群聊';


--;

--

COMMENT ON COLUMN public.conversation_pin.pinned_at IS '置顶时间';


--;

--

COMMENT ON COLUMN public.conversation_pin.created_at IS '创建时间';


--;

--

CREATE TABLE public.conversation_delete (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    conversation_type character varying(10) NOT NULL,
    deleted_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_conversation_delete_type CHECK (((conversation_type)::text = ANY ((ARRAY['c2c'::character varying, 'c2g'::character varying])::text[])))
);


--;

--

ALTER TABLE ONLY public.conversation_delete
    ADD CONSTRAINT conversation_delete_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.conversation_delete
    ADD CONSTRAINT conversation_delete_user_conversation_uk UNIQUE (user_id, conversation_id, conversation_type);


--;

--

CREATE INDEX idx_conversation_delete_conversation ON public.conversation_delete USING btree (conversation_id, conversation_type);


--;

--

CREATE INDEX idx_conversation_delete_user ON public.conversation_delete USING btree (user_id, deleted_at DESC);


--;

--

COMMENT ON TABLE public.conversation_delete IS '会话软删除记录表';


--;

--

COMMENT ON COLUMN public.conversation_delete.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.conversation_delete.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';


--;

--

COMMENT ON COLUMN public.conversation_delete.conversation_type IS '会话类型：c2c-单聊，c2g-群聊';


--;

--

COMMENT ON COLUMN public.conversation_delete.deleted_at IS '删除时间，可用于恢复功能';


--;

--

COMMENT ON COLUMN public.conversation_delete.created_at IS '创建时间';


--;

--

CREATE TABLE public."group" (
    id bigint NOT NULL,
    type smallint DEFAULT 1,
    join_limit smallint DEFAULT 2,
    content_limit smallint DEFAULT 2,
    user_id_sum bigint DEFAULT 0 NOT NULL,
    owner_uid bigint NOT NULL,
    creator_uid bigint NOT NULL,
    member_max integer DEFAULT 1000 NOT NULL,
    member_count integer DEFAULT 1 NOT NULL,
    introduction character varying(2000) DEFAULT ''::character varying NOT NULL,
    avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    title character varying(200) DEFAULT ''::character varying NOT NULL,
    chat_aes_key character varying(2048) DEFAULT ''::character varying NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_group_creator_uid CHECK ((creator_uid > 0)),
    CONSTRAINT chk_group_member_count CHECK ((member_count >= 0)),
    CONSTRAINT chk_group_member_max CHECK ((member_max > 0)),
    CONSTRAINT chk_group_owner_uid CHECK ((owner_uid > 0)),
    CONSTRAINT chk_group_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--;

--

ALTER TABLE ONLY public."group"
    ADD CONSTRAINT group_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_creatorid_memberidsum ON public."group" USING btree (creator_uid, user_id_sum);


--;

--

CREATE INDEX i_status_owneruid_type ON public."group" USING btree (status, owner_uid, type);


--;

--

COMMENT ON TABLE public."group" IS '群组表';


--;

--

COMMENT ON COLUMN public."group".id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public."group".type IS '类型: 1 公开群组  2 私有群组';


--;

--

COMMENT ON COLUMN public."group".join_limit IS '加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入';


--;

--

COMMENT ON COLUMN public."group".content_limit IS '内部发布限制: 1 圈内不需审核  2 圈内需要审核  3 圈外需要审核';


--;

--

COMMENT ON COLUMN public."group".owner_uid IS '群组拥有者ID';


--;

--

COMMENT ON COLUMN public."group".creator_uid IS '群组创建者ID';


--;

--

COMMENT ON COLUMN public."group".member_max IS '允许最大成员数量';


--;

--

COMMENT ON COLUMN public."group".member_count IS '成员数量';


--;

--

COMMENT ON COLUMN public."group".introduction IS '简介';


--;

--

COMMENT ON COLUMN public."group".avatar IS '群组头像';


--;

--

COMMENT ON COLUMN public."group".title IS '群组名称';


--;

--

COMMENT ON COLUMN public."group".chat_aes_key IS '群聊消息秘钥';


--;

--

COMMENT ON COLUMN public."group".status IS '状态: -1 删除  0 禁用  1 启用';


--;

--

COMMENT ON COLUMN public."group".updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public."group".created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.group_member (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    invite_code character varying(40) DEFAULT ''::character varying,
    alias character varying(120) DEFAULT ''::character varying,
    description text DEFAULT ''::character varying,
    role smallint DEFAULT 0,
    is_join boolean DEFAULT false,
    join_mode character varying(120) DEFAULT ''::character varying,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    remark character varying(200) DEFAULT ''::character varying,
    mute_until timestamp with time zone,
    category_id bigint DEFAULT 0,
    CONSTRAINT chk_group_member_role CHECK (((role >= 0) AND (role <= 5))),
    CONSTRAINT chk_group_member_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1, 2])))
);


--;

--

ALTER TABLE ONLY public.group_member
    ADD CONSTRAINT group_member_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_uid_gid_isjoin ON public.group_member USING btree (user_id, group_id, is_join);


--;

--

CREATE INDEX i_uid_status ON public.group_member USING btree (user_id, status);


--;

--

CREATE INDEX idx_group_member_category ON public.group_member USING btree (user_id, category_id) WHERE (category_id > 0);


--;

--

CREATE INDEX idx_group_member_mute ON public.group_member USING btree (group_id, user_id) WHERE (mute_until IS NOT NULL);


--;

--

CREATE INDEX idx_group_member_role ON public.group_member USING btree (group_id, role);


--;

--

CREATE UNIQUE INDEX uk_gid_uid ON public.group_member USING btree (group_id, user_id);


--;

--

COMMENT ON TABLE public.group_member IS '群组成员表';


--;

--

COMMENT ON COLUMN public.group_member.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.group_member.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.group_member.user_id IS '群组成员用户ID';


--;

--

COMMENT ON COLUMN public.group_member.invite_code IS '入群邀请码';


--;

--

COMMENT ON COLUMN public.group_member.alias IS '群内别名';


--;

--

COMMENT ON COLUMN public.group_member.description IS '群内描述';


--;

--

COMMENT ON COLUMN public.group_member.role IS '角色: 0 未定义 1 普通成员 2 嘉宾 3 管理员 4 群主 5 副群主';


--;

--

COMMENT ON COLUMN public.group_member.is_join IS '是否加入的群： 1 是 0 否 （0 是群创建者或者拥有者 1 是 成员 嘉宾 管理员等）';


--;

--

COMMENT ON COLUMN public.group_member.join_mode IS '进群方式 :  invite_[uid]_[nickname] <a>leeyi</a>邀请进群  scan_qr_code 扫描二维码加入 face2face_join 面对面建群';


--;

--

COMMENT ON COLUMN public.group_member.status IS '状态: -1 删除  0 禁用  1 启用 ';


--;

--

COMMENT ON COLUMN public.group_member.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.group_member.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.group_member.remark IS '群备注，仅该成员自己可见';


--;

--

COMMENT ON COLUMN public.group_member.category_id IS '群组分类ID，0表示未分类';


--;

--

CREATE TABLE public.group_log (
    id bigint NOT NULL,
    type integer NOT NULL,
    option_uid bigint DEFAULT 0 NOT NULL,
    group_id bigint NOT NULL,
    body text NOT NULL,
    remark text DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_log
    ADD CONSTRAINT group_log_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_group_log_type_optionuid_createdat ON public.group_log USING btree (type, option_uid, created_at);


--;

--

CREATE INDEX idx_group_log_group_id ON public.group_log USING btree (group_id);


--;

--

COMMENT ON TABLE public.group_log IS '群组日志表';


--;

--

COMMENT ON COLUMN public.group_log.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.group_log.type IS '日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群';


--;

--

COMMENT ON COLUMN public.group_log.option_uid IS '操作者用户ID（0 表示主动退出）';


--;

--

COMMENT ON COLUMN public.group_log.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.group_log.body IS '相关操作类型的json字符串数据';


--;

--

COMMENT ON COLUMN public.group_log.remark IS '备注';


--;

--

COMMENT ON COLUMN public.group_log.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.group_random_code (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    code character varying(20) DEFAULT ''::character varying,
    location public.geometry,
    validity_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_random_code
    ADD CONSTRAINT group_random_code_pkey PRIMARY KEY (id);


--;

--

CREATE UNIQUE INDEX i_group_random_code_groupid ON public.group_random_code USING btree (group_id);


--;

--

CREATE INDEX i_group_random_code_location ON public.group_random_code USING gist (location);


--;

--

CREATE INDEX idx_group_random_code_uid ON public.group_random_code USING btree (user_id);


--;

--

COMMENT ON TABLE public.group_random_code IS '存储面对面建群的随机码和关联的群 ID';


--;

--

COMMENT ON COLUMN public.group_random_code.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.group_random_code.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.group_random_code.user_id IS '创建用户ID';


--;

--

COMMENT ON COLUMN public.group_random_code.code IS '随机码';


--;

--

COMMENT ON COLUMN public.group_random_code.validity_at IS '有效期截止时间，NULL 表示永久有效';


--;

--

COMMENT ON COLUMN public.group_random_code.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.group_notice (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    edit_user_id bigint,
    body text DEFAULT ''::character varying,
    status smallint DEFAULT 0 NOT NULL,
    expired_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    pinned boolean DEFAULT false,
    deleted_at timestamp with time zone,
    read_count integer DEFAULT 0,
    title character varying(200) DEFAULT ''::character varying
);


--;

--

ALTER TABLE ONLY public.group_notice
    ADD CONSTRAINT group_notice_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX i_gid_status_expiredat ON public.group_notice USING btree (group_id, status, expired_at);


--;

--

CREATE INDEX i_group_notice_deleted_at ON public.group_notice USING btree (deleted_at) WHERE (deleted_at IS NULL);


--;

--

CREATE INDEX i_group_notice_pinned ON public.group_notice USING btree (group_id, pinned, deleted_at) WHERE (deleted_at IS NULL);


--;

--

CREATE INDEX idx_group_notice_group_id ON public.group_notice USING btree (group_id);


--;

--

COMMENT ON TABLE public.group_notice IS '群组公告记录表';


--;

--

COMMENT ON COLUMN public.group_notice.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.group_notice.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.group_notice.user_id IS '创建用户ID';


--;

--

COMMENT ON COLUMN public.group_notice.body IS '公告类容';


--;

--

COMMENT ON COLUMN public.group_notice.status IS '状态 0 待发布  1 已发布 2 取消发布';


--;

--

COMMENT ON COLUMN public.group_notice.expired_at IS '公告有效期截止时间';


--;

--

COMMENT ON COLUMN public.group_notice.updated_at IS '最后更新时间';


--;

--

COMMENT ON COLUMN public.group_notice.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.group_notice.pinned IS '是否置顶';


--;

--

COMMENT ON COLUMN public.group_notice.deleted_at IS '软删除时间戳';


--;

--

COMMENT ON COLUMN public.group_notice.read_count IS '已读用户数量';


--;

--

COMMENT ON COLUMN public.group_notice.title IS '公告标题';


--;

--

CREATE TABLE public.user_group (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    group_id bigint NOT NULL,
    remark character varying(400) DEFAULT ''::character varying,
    setting jsonb DEFAULT '{}'::jsonb NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_group
    ADD CONSTRAINT user_group_pkey PRIMARY KEY (id);


--;

--

CREATE UNIQUE INDEX uk_ug_uid_gid ON public.user_group USING btree (user_id, group_id);


--;

--

COMMENT ON TABLE public.user_group IS '用户保存到通讯录的群';


--;

--

COMMENT ON COLUMN public.user_group.id IS '主键 自增长ID';


--;

--

COMMENT ON COLUMN public.user_group.user_id IS '用户ID';


--;

--

COMMENT ON COLUMN public.user_group.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.user_group.remark IS '群聊的备注仅自己可见';


--;

--

COMMENT ON COLUMN public.user_group.setting IS '用户对群的一些配置';


--;

--

COMMENT ON COLUMN public.user_group.status IS '状态: -1 删除  0 禁用  1 启用 ';


--;

--

COMMENT ON COLUMN public.user_group.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

COMMENT ON COLUMN public.user_group.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--;

--

CREATE TABLE public.user_group_category (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    category_name character varying(50) NOT NULL,
    sort_order integer DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.user_group_category
    ADD CONSTRAINT user_group_category_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_user_group_category_user ON public.user_group_category USING btree (user_id, sort_order);


--;

--

CREATE UNIQUE INDEX idx_user_group_category_user_name ON public.user_group_category USING btree (user_id, category_name);


--;

--

COMMENT ON TABLE public.user_group_category IS '群组分类表：用户自定义群组分组';


--;

--

COMMENT ON COLUMN public.user_group_category.id IS '主键，自增长ID';


--;

--

COMMENT ON COLUMN public.user_group_category.user_id IS '分类所属用户ID';


--;

--

COMMENT ON COLUMN public.user_group_category.category_name IS '分类名称';


--;

--

COMMENT ON COLUMN public.user_group_category.sort_order IS '排序顺序，数字越小越靠前';


--;

--

COMMENT ON COLUMN public.user_group_category.created_at IS '创建时间';


--;

--

CREATE TABLE public.group_tag (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    tag_name character varying(50) NOT NULL,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_tag
    ADD CONSTRAINT group_tag_group_id_tag_name_key UNIQUE (group_id, tag_name);


--;

--

ALTER TABLE ONLY public.group_tag
    ADD CONSTRAINT group_tag_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_tag_created_by ON public.group_tag USING btree (created_by);


--;

--

CREATE INDEX idx_group_tag_group ON public.group_tag USING btree (group_id);


--;

--

CREATE INDEX idx_group_tag_name ON public.group_tag USING btree (tag_name);


--;

--

COMMENT ON TABLE public.group_tag IS '群组标签表';


--;

--

COMMENT ON COLUMN public.group_tag.id IS '主键ID';


--;

--

COMMENT ON COLUMN public.group_tag.group_id IS '群组ID';


--;

--

COMMENT ON COLUMN public.group_tag.tag_name IS '标签名称';


--;

--

COMMENT ON COLUMN public.group_tag.created_by IS '创建者用户ID';


--;

--

COMMENT ON COLUMN public.group_tag.created_at IS '创建时间';


--;

--

CREATE TABLE public.group_schedule (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    schedule_id character varying(40) NOT NULL,
    title character varying(200) NOT NULL,
    description text,
    location character varying(200),
    creator_id bigint NOT NULL,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone NOT NULL,
    remind_before integer,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_schedule_remind_before CHECK (((remind_before IS NULL) OR (remind_before > 0))),
    CONSTRAINT chk_schedule_time CHECK ((end_at > start_at))
);


--;

--

ALTER TABLE ONLY public.group_schedule
    ADD CONSTRAINT group_schedule_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_schedule
    ADD CONSTRAINT group_schedule_schedule_id_key UNIQUE (schedule_id);


--;

--

CREATE INDEX idx_group_schedule_creator_id ON public.group_schedule USING btree (creator_id);


--;

--

CREATE INDEX idx_group_schedule_group_id ON public.group_schedule USING btree (group_id);


--;

--

CREATE INDEX idx_group_schedule_group_status ON public.group_schedule USING btree (group_id, status);


--;

--

CREATE INDEX idx_group_schedule_start_at ON public.group_schedule USING btree (start_at);


--;

--

COMMENT ON TABLE public.group_schedule IS '群组日程表';


--;

--

COMMENT ON COLUMN public.group_schedule.remind_before IS '提前多少分钟提醒';


--;

--

COMMENT ON COLUMN public.group_schedule.status IS '1待开始 2进行中 3已结束 4已取消';


--;

--

CREATE TABLE public.group_schedule_participant (
    id bigint NOT NULL,
    schedule_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    status smallint DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_schedule_participant
    ADD CONSTRAINT group_schedule_participant_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_schedule_participant
    ADD CONSTRAINT group_schedule_participant_schedule_id_user_id_key UNIQUE (schedule_id, user_id);


--;

--

CREATE INDEX idx_group_schedule_participant_schedule_id ON public.group_schedule_participant USING btree (schedule_id);


--;

--

CREATE INDEX idx_group_schedule_participant_schedule_status ON public.group_schedule_participant USING btree (schedule_id, status);


--;

--

CREATE INDEX idx_group_schedule_participant_user_id ON public.group_schedule_participant USING btree (user_id);


--;

--

COMMENT ON TABLE public.group_schedule_participant IS '群组日程参与人表';


--;

--

COMMENT ON COLUMN public.group_schedule_participant.status IS '0待确认 1参加 2不参加';


--;

--

CREATE TABLE public.group_schedule_remind (
    id bigint NOT NULL,
    schedule_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    remind_at timestamp with time zone NOT NULL,
    is_sent boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_schedule_remind
    ADD CONSTRAINT group_schedule_remind_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_schedule_remind_pending ON public.group_schedule_remind USING btree (remind_at) WHERE (is_sent = false);


--;

--

CREATE INDEX idx_group_schedule_remind_remind_at ON public.group_schedule_remind USING btree (remind_at);


--;

--

CREATE INDEX idx_group_schedule_remind_schedule_id ON public.group_schedule_remind USING btree (schedule_id);


--;

--

CREATE INDEX idx_group_schedule_remind_user_id ON public.group_schedule_remind USING btree (user_id);


--;

--

COMMENT ON TABLE public.group_schedule_remind IS '群组日程提醒表';


--;

--

CREATE TABLE public.group_file (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    file_id character varying(40) NOT NULL,
    file_name text NOT NULL,
    file_size bigint NOT NULL,
    file_type character varying(100) NOT NULL,
    file_category character varying(20) NOT NULL,
    file_url text NOT NULL,
    file_hash character varying(64),
    uploader_id bigint NOT NULL,
    download_count integer DEFAULT 0,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_group_file_download_count CHECK ((download_count >= 0)),
    CONSTRAINT chk_group_file_status CHECK ((status = ANY (ARRAY[0, 1])))
);


--;

--

ALTER TABLE ONLY public.group_file
    ADD CONSTRAINT group_file_file_id_key UNIQUE (file_id);


--;

--

ALTER TABLE ONLY public.group_file
    ADD CONSTRAINT group_file_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_file_category ON public.group_file USING btree (group_id, file_category);


--;

--

CREATE INDEX idx_group_file_file_id ON public.group_file USING btree (file_id);


--;

--

CREATE INDEX idx_group_file_group ON public.group_file USING btree (group_id, created_at DESC);


--;

--

CREATE INDEX idx_group_file_name ON public.group_file USING btree (group_id, file_name);


--;

--

CREATE INDEX idx_group_file_uploader ON public.group_file USING btree (uploader_id);


--;

--

COMMENT ON TABLE public.group_file IS '群文件共享表';


--;

--

COMMENT ON COLUMN public.group_file.file_category IS '文件分类: document/image/video/audio/other';


--;

--

COMMENT ON COLUMN public.group_file.file_hash IS '文件MD5哈希，用于去重';


--;

--

COMMENT ON COLUMN public.group_file.download_count IS '文件下载次数统计';


--;

--

COMMENT ON COLUMN public.group_file.status IS '状态: 1正常 0软删除';


--;

--

CREATE TABLE public.group_album (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    album_id character varying(40) NOT NULL,
    album_name character varying(100) NOT NULL,
    album_cover character varying(40),
    creator_id bigint NOT NULL,
    photo_count integer DEFAULT 0,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_group_album_photo_count CHECK ((photo_count >= 0))
);


--;

--

ALTER TABLE ONLY public.group_album
    ADD CONSTRAINT group_album_album_id_key UNIQUE (album_id);


--;

--

ALTER TABLE ONLY public.group_album
    ADD CONSTRAINT group_album_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_album_group ON public.group_album USING btree (group_id, created_at DESC);


--;

--

CREATE INDEX idx_group_album_uid ON public.group_album USING btree (creator_id);


--;

--

COMMENT ON TABLE public.group_album IS '群相册表';


--;

--

COMMENT ON COLUMN public.group_album.album_id IS '相册唯一ID';


--;

--

COMMENT ON COLUMN public.group_album.album_cover IS '封面图片ID (关联 group_album_photo.photo_id)';


--;

--

COMMENT ON COLUMN public.group_album.photo_count IS '照片数量';


--;

--

CREATE TABLE public.group_album_photo (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    album_id character varying(40) NOT NULL,
    photo_id character varying(40) NOT NULL,
    photo_name character varying(255) NOT NULL,
    photo_url text NOT NULL,
    thumbnail_url text,
    photo_size bigint NOT NULL,
    width integer,
    height integer,
    uploader_id bigint NOT NULL,
    like_count integer DEFAULT 0,
    comment_count integer DEFAULT 0,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_photo_comment_count CHECK ((comment_count >= 0)),
    CONSTRAINT chk_photo_like_count CHECK ((like_count >= 0))
);


--;

--

ALTER TABLE ONLY public.group_album_photo
    ADD CONSTRAINT group_album_photo_photo_id_key UNIQUE (photo_id);


--;

--

ALTER TABLE ONLY public.group_album_photo
    ADD CONSTRAINT group_album_photo_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_album_photo_album ON public.group_album_photo USING btree (album_id, created_at DESC);


--;

--

CREATE INDEX idx_group_album_photo_group ON public.group_album_photo USING btree (group_id, created_at DESC);


--;

--

CREATE INDEX idx_group_album_photo_uploader ON public.group_album_photo USING btree (uploader_id);


--;

--

COMMENT ON TABLE public.group_album_photo IS '群相册图片表';


--;

--

COMMENT ON COLUMN public.group_album_photo.photo_id IS '图片唯一ID';


--;

--

COMMENT ON COLUMN public.group_album_photo.photo_url IS '原图URL';


--;

--

COMMENT ON COLUMN public.group_album_photo.thumbnail_url IS '缩略图URL';


--;

--

COMMENT ON COLUMN public.group_album_photo.width IS '图片宽度 (像素)';


--;

--

COMMENT ON COLUMN public.group_album_photo.height IS '图片高度 (像素)';


--;

--

CREATE TABLE public.group_album_photo_comment (
    id bigint NOT NULL,
    photo_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    content text NOT NULL,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_album_photo_comment
    ADD CONSTRAINT group_album_photo_comment_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_album_photo_comment_photo ON public.group_album_photo_comment USING btree (photo_id, created_at DESC);


--;

--

CREATE INDEX idx_group_album_photo_comment_user ON public.group_album_photo_comment USING btree (user_id);


--;

--

COMMENT ON TABLE public.group_album_photo_comment IS '群相册图片评论表';


--;

--

CREATE TABLE public.group_album_photo_like (
    id bigint NOT NULL,
    photo_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_album_photo_like
    ADD CONSTRAINT group_album_photo_like_photo_id_user_id_key UNIQUE (photo_id, user_id);


--;

--

ALTER TABLE ONLY public.group_album_photo_like
    ADD CONSTRAINT group_album_photo_like_pkey PRIMARY KEY (id);


--;

--

CREATE INDEX idx_group_album_photo_like_photo ON public.group_album_photo_like USING btree (photo_id);


--;

--

COMMENT ON TABLE public.group_album_photo_like IS '群相册图片点赞表';


--;

--

CREATE TABLE public.group_task (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    task_id character varying(40) NOT NULL,
    title character varying(200) NOT NULL,
    description text,
    creator_id bigint NOT NULL,
    deadline timestamp with time zone,
    status smallint DEFAULT 1,
    attachment text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    deleted_at timestamp with time zone
);


--;

--

ALTER TABLE ONLY public.group_task
    ADD CONSTRAINT group_task_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_task
    ADD CONSTRAINT group_task_task_id_key UNIQUE (task_id);


--;

--

CREATE INDEX idx_group_task_deleted_at ON public.group_task USING btree (deleted_at) WHERE (deleted_at IS NULL);


--;

--

CREATE INDEX idx_group_task_group_id ON public.group_task USING btree (group_id);


--;

--

CREATE INDEX idx_group_task_group_status_alive ON public.group_task USING btree (group_id, status, id DESC) WHERE (deleted_at IS NULL);


--;

--

CREATE INDEX idx_group_task_task_id ON public.group_task USING btree (task_id);


--;

--

COMMENT ON TABLE public.group_task IS '群作业表';


--;

--

COMMENT ON COLUMN public.group_task.task_id IS '作业唯一标识（HashID编码）';


--;

--

COMMENT ON COLUMN public.group_task.status IS '状态: 1待完成 2进行中 3已截止';


--;

--

COMMENT ON COLUMN public.group_task.deleted_at IS '软删除时间戳，NULL 表示未删除';


--;

--

CREATE TABLE public.group_task_assignment (
    id bigint NOT NULL,
    task_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    status smallint DEFAULT 0,
    submitted_at timestamp with time zone,
    content text,
    attachment text,
    score integer,
    comment text,
    reviewed_by bigint,
    reviewed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_task_assignment_reviewed CHECK (((status <> 3) OR (reviewed_by IS NOT NULL)))
);


--;

--

ALTER TABLE ONLY public.group_task_assignment
    ADD CONSTRAINT group_task_assignment_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_task_assignment
    ADD CONSTRAINT group_task_assignment_task_id_user_id_key UNIQUE (task_id, user_id);


--;

--

CREATE INDEX idx_group_task_assignment_task_id ON public.group_task_assignment USING btree (task_id);


--;

--

CREATE INDEX idx_group_task_assignment_task_status ON public.group_task_assignment USING btree (task_id, status);


--;

--

CREATE INDEX idx_group_task_assignment_user_id ON public.group_task_assignment USING btree (user_id);


--;

--

COMMENT ON TABLE public.group_task_assignment IS '群作业分配表';


--;

--

COMMENT ON COLUMN public.group_task_assignment.status IS '状态: 0待完成 1进行中 2已提交 3已批改';


--;

--

CREATE TABLE public.group_vote (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    vote_id character varying(40) NOT NULL,
    title character varying(200) NOT NULL,
    description text,
    creator_id bigint NOT NULL,
    vote_type smallint DEFAULT 1,
    is_anonymous boolean DEFAULT false,
    status smallint DEFAULT 1,
    end_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_vote
    ADD CONSTRAINT group_vote_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_vote
    ADD CONSTRAINT group_vote_vote_id_key UNIQUE (vote_id);


--;

--

CREATE INDEX idx_group_vote_creator_id ON public.group_vote USING btree (creator_id);


--;

--

CREATE INDEX idx_group_vote_group_id ON public.group_vote USING btree (group_id);


--;

--

CREATE INDEX idx_group_vote_group_status ON public.group_vote USING btree (group_id, status);


--;

--

CREATE INDEX idx_group_vote_vote_id ON public.group_vote USING btree (vote_id);


--;

--

COMMENT ON TABLE public.group_vote IS '群投票主表';


--;

--

COMMENT ON COLUMN public.group_vote.vote_id IS '投票唯一标识 (UUID)';


--;

--

COMMENT ON COLUMN public.group_vote.vote_type IS '投票类型: 1=单选, 2=多选';


--;

--

COMMENT ON COLUMN public.group_vote.is_anonymous IS '是否匿名投票';


--;

--

COMMENT ON COLUMN public.group_vote.status IS '投票状态: 1=进行中, 2=已结束, 3=已取消';


--;

--

COMMENT ON COLUMN public.group_vote.end_at IS '投票截止时间';


--;

--

CREATE TABLE public.group_vote_option (
    id bigint NOT NULL,
    vote_id character varying(40) NOT NULL,
    option_id character varying(40) NOT NULL,
    option_text character varying(200) NOT NULL,
    sort_order integer DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_vote_option
    ADD CONSTRAINT group_vote_option_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_vote_option
    ADD CONSTRAINT group_vote_option_vote_id_option_id_key UNIQUE (vote_id, option_id);


--;

--

CREATE INDEX idx_group_vote_option_option_id ON public.group_vote_option USING btree (option_id);


--;

--

CREATE INDEX idx_group_vote_option_vote_id ON public.group_vote_option USING btree (vote_id);


--;

--

COMMENT ON TABLE public.group_vote_option IS '群投票选项表';


--;

--

COMMENT ON COLUMN public.group_vote_option.option_id IS '选项唯一标识';


--;

--

COMMENT ON COLUMN public.group_vote_option.sort_order IS '选项排序顺序';


--;

--

CREATE TABLE public.group_vote_record (
    id bigint NOT NULL,
    vote_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    option_ids jsonb DEFAULT '[]'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--;

--

ALTER TABLE ONLY public.group_vote_record
    ADD CONSTRAINT group_vote_record_pkey PRIMARY KEY (id);


--;

--

ALTER TABLE ONLY public.group_vote_record
    ADD CONSTRAINT group_vote_record_vote_id_user_id_key UNIQUE (vote_id, user_id);


--;

--

CREATE INDEX idx_group_vote_record_options ON public.group_vote_record USING gin (option_ids);


--;

--

CREATE INDEX idx_group_vote_record_user_id ON public.group_vote_record USING btree (user_id);


--;

--

CREATE INDEX idx_group_vote_record_vote_id ON public.group_vote_record USING btree (vote_id);


--;

--

COMMENT ON TABLE public.group_vote_record IS '群投票记录表';


--;

--

COMMENT ON COLUMN public.group_vote_record.option_ids IS '用户选择的选项ID列表(JSON数组格式)';


--;

