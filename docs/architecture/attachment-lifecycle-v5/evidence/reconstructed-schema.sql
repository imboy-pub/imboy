--
-- PostgreSQL database dump
--

\restrict JwWtw0Bzy5pPapekDOnuf7mg2yrvHesDliMn0SddDQqRYZRvj7ZhI8Rbma1ztOm

-- Dumped from database version 18.1 (Debian 18.1-1.pgdg13+2)
-- Dumped by pg_dump version 18.1 (Debian 18.1-1.pgdg13+2)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: timescaledb; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS timescaledb WITH SCHEMA public;


--
-- Name: EXTENSION timescaledb; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION timescaledb IS 'Enables scalable inserts and complex queries for time-series data (Community Edition)';


--
-- Name: btree_gin; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS btree_gin WITH SCHEMA public;


--
-- Name: EXTENSION btree_gin; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION btree_gin IS 'support for indexing common datatypes in GIN';


--
-- Name: btree_gist; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS btree_gist WITH SCHEMA public;


--
-- Name: EXTENSION btree_gist; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION btree_gist IS 'support for indexing common datatypes in GiST';


--
-- Name: pg_jieba; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_jieba WITH SCHEMA public;


--
-- Name: EXTENSION pg_jieba; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pg_jieba IS 'a parser for full-text search of Chinese';


--
-- Name: pg_stat_statements; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_stat_statements WITH SCHEMA public;


--
-- Name: EXTENSION pg_stat_statements; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pg_stat_statements IS 'track planning and execution statistics of all SQL statements executed';


--
-- Name: pg_trgm; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA public;


--
-- Name: EXTENSION pg_trgm; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pg_trgm IS 'text similarity measurement and index searching based on trigrams';


--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public;


--
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry and geography spatial types and functions';


--
-- Name: postgis_raster; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis_raster WITH SCHEMA public;


--
-- Name: EXTENSION postgis_raster; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION postgis_raster IS 'PostGIS raster types and functions';


--
-- Name: unaccent; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS unaccent WITH SCHEMA public;


--
-- Name: EXTENSION unaccent; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION unaccent IS 'text search dictionary that removes accents';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: channel_bump_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.channel_bump_version() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
 IF ROW(NEW.name,NEW.description,NEW.avatar,NEW.custom_id,NEW.tags,NEW.visibility,NEW.access_type,NEW.join_policy,NEW.status,NEW.scope,NEW.workspace_id,NEW.creator_uid,NEW.is_verified) IS DISTINCT FROM ROW(OLD.name,OLD.description,OLD.avatar,OLD.custom_id,OLD.tags,OLD.visibility,OLD.access_type,OLD.join_policy,OLD.status,OLD.scope,OLD.workspace_id,OLD.creator_uid,OLD.is_verified) THEN
  NEW.version := OLD.version + 1;
 ELSE
  NEW.version := OLD.version;
 END IF;
 RETURN NEW;
END $$;


--
-- Name: check_e2ee_shard_transmission_anomaly(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION check_e2ee_shard_transmission_anomaly(p_key_version character varying, p_shard_id character varying); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.check_e2ee_shard_transmission_anomaly(p_key_version character varying, p_shard_id character varying) IS '检查分片传输异常';


--
-- Name: cleanup_expired_channel_invitations(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION cleanup_expired_channel_invitations(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cleanup_expired_channel_invitations() IS '清理过期的频道邀请';


--
-- Name: cleanup_expired_channel_orders(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION cleanup_expired_channel_orders(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cleanup_expired_channel_orders() IS '清理过期的频道订单';


--
-- Name: cleanup_expired_transfer_sessions(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION cleanup_expired_transfer_sessions(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cleanup_expired_transfer_sessions() IS '清理过期的传输会话';


--
-- Name: cleanup_old_shard_transmission_logs(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION cleanup_old_shard_transmission_logs(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cleanup_old_shard_transmission_logs() IS '清理 90 天前的分片传输日志';


--
-- Name: cleanup_used_social_shards(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION cleanup_used_social_shards(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cleanup_used_social_shards() IS '清理已使用超过 90 天的旧分片';


--
-- Name: collect_id_segment_stats(smallint); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION collect_id_segment_stats(p_dc_id smallint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.collect_id_segment_stats(p_dc_id smallint) IS '记录ID段使用统计 - 定时任务调用';


--
-- Name: fn_agent_grant_event_append_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_agent_grant_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'agent_grant_event 是 append-only Grant 血统账本，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_agent_grant_event_append_only';
    END IF;

    -- 唯一允许的变更：user 行被删除时，actor_user_id 由外键 ON DELETE SET NULL 置 NULL。
    -- 事件内容（grant/event_type/actor_kind/version/detail/idempotency/时间）与既有 actor 一律不可改写。
    IF (NEW.id, NEW.grant_id, NEW.event_type, NEW.actor_kind, NEW.from_version,
        NEW.to_version, NEW.detail_json, NEW.idempotency_key, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.grant_id, OLD.event_type, OLD.actor_kind, OLD.from_version,
        OLD.to_version, OLD.detail_json, OLD.idempotency_key, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'agent_grant_event 是 append-only Grant 血统账本，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_agent_grant_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_agent_grant_event_append_only(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_agent_grant_event_append_only() IS 'Agent Grant event append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514，唯一例外是 fk_age_actor 的 ON DELETE SET NULL 把 actor_user_id 置 NULL（事件内容与既有 actor 不可改写，只允许 INSERT 追加）';


--
-- Name: fn_agent_run_event_append_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_agent_run_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'agent_run_event 是 append-only Run 血统账本，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_agent_run_event_append_only';
    END IF;

    -- 本表 actor_id 为 text 且无 user FK（不存在 ON DELETE SET NULL 例外），UPDATE 一律拒绝。
    RAISE EXCEPTION
        'agent_run_event 是 append-only Run 血统账本，禁止 UPDATE（只允许 INSERT 追加）'
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_agent_run_event_append_only';
END;
$$;


--
-- Name: FUNCTION fn_agent_run_event_append_only(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_agent_run_event_append_only() IS 'AgentRun event append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514（无例外），只允许 INSERT 追加';


--
-- Name: fn_channel_subscribe_updated_at(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_channel_subscribe_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--
-- Name: fn_class_enrollment_org_check(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_class_enrollment_org_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_group_org bigint;
    v_learner_org bigint;
BEGIN
    SELECT w.organization_id INTO v_group_org
      FROM "group" g
      JOIN workspace w ON w.id = g.workspace_id
     WHERE g.id = NEW.group_id;

    SELECT organization_id INTO v_learner_org
      FROM learner
     WHERE id = NEW.learner_id;

    IF v_group_org IS NULL THEN
        RAISE EXCEPTION
            '班级机构一致性：群 % 不是已归属机构的工作区群（无 workspace 或 workspace 无 organization），'
            '不能作为教学班级接收学员入班',
            NEW.group_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_class_enrollment_org_consistency',
                  HINT = '教学班级必须位于 organization_id 非空的 workspace 下';
    END IF;

    IF v_learner_org IS NULL OR v_group_org <> v_learner_org THEN
        RAISE EXCEPTION
            '班级机构一致性：学员 % 属于机构 %，但班级群 % 属于机构 %，跨机构入班被拒绝',
            NEW.learner_id, v_learner_org, NEW.group_id, v_group_org
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_class_enrollment_org_consistency',
                  HINT = 'learner.organization_id 必须等于 group->workspace->organization_id';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_class_enrollment_org_check(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_class_enrollment_org_check() IS '入班机构一致性：learner.organization_id == group→workspace→organization_id（提交时校验，可延迟；fail-closed：解析不出机构即拒绝）';


--
-- Name: fn_customer_service_event_append_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_customer_service_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'customer_service_event 是 append-only 审计真源，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_customer_service_event_append_only';
    END IF;

    IF (NEW.id, NEW.organization_id, NEW.workspace_id, NEW.session_id, NEW.business_identity_id,
        NEW.actor_kind, NEW.action, NEW.detail, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.workspace_id, OLD.session_id, OLD.business_identity_id,
        OLD.actor_kind, OLD.action, OLD.detail, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'customer_service_event 是 append-only 审计真源，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_customer_service_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_customer_service_event_append_only(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_customer_service_event_append_only() IS '客服审计 append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514（仅 FK 置 NULL actor_user_id 例外）';


--
-- Name: fn_efapi_application_mime_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_efapi_application_mime_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
DECLARE
    v_mime text;
    v_seen text[] := ARRAY[]::text[];
BEGIN
    IF NEW.allowed_mime_types IS NULL THEN
        RAISE EXCEPTION 'allowed_mime_types 不可为 NULL；空清单请用空数组 {}'
            USING ERRCODE = '23514';
    END IF;
    IF array_length(NEW.allowed_mime_types, 1) IS NULL THEN
        RETURN NEW;
    END IF;
    IF array_length(NEW.allowed_mime_types, 1) > 32 THEN
        RAISE EXCEPTION 'allowed_mime_types 最多 32 个元素（当前 %）',
            array_length(NEW.allowed_mime_types, 1)
            USING ERRCODE = '23514';
    END IF;
    FOREACH v_mime IN ARRAY NEW.allowed_mime_types LOOP
        IF v_mime IS NULL OR v_mime = '' OR length(v_mime) > 128 THEN
            RAISE EXCEPTION 'allowed_mime_types 元素非空且长度 <=128' USING ERRCODE = '23514';
        END IF;
        IF position('*' in v_mime) > 0 THEN
            RAISE EXCEPTION 'allowed_mime_types 不接受通配元素（%）', v_mime
                USING ERRCODE = '23514';
        END IF;
        IF v_mime !~ '^[a-z0-9][a-z0-9!#$&^_.+-]*/[a-z0-9][a-z0-9!#$&^_.+-]*$' THEN
            RAISE EXCEPTION 'allowed_mime_types 元素须为 type/subtype 形态（%）', v_mime
                USING ERRCODE = '23514';
        END IF;
        IF v_mime = ANY (v_seen) THEN
            RAISE EXCEPTION 'allowed_mime_types 元素重复（%）', v_mime USING ERRCODE = '23514';
        END IF;
        v_seen := v_seen || v_mime;
    END LOOP;
    RETURN NEW;
END;
$_$;


--
-- Name: fn_efapi_attachment_retention_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_efapi_attachment_retention_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'enterprise_attachment_retention 行禁止物理删除（purge 走 purge_state=purged）'
            USING ERRCODE = '23514';
    END IF;

    IF OLD.purge_state = 'purged' AND NEW.purge_state <> 'purged' THEN
        RAISE EXCEPTION 'purged 为终态，不可回退（% -> %）', OLD.purge_state, NEW.purge_state
            USING ERRCODE = '23514';
    END IF;

    IF OLD.purge_state <> 'purged' AND NEW.purge_state = 'purged' THEN
        IF OLD.hold_state = 'held' THEN
            RAISE EXCEPTION '法务 hold 生效中禁止 purge（attachment_id=%）', OLD.attachment_id
                USING ERRCODE = '23514';
        END IF;
        IF CURRENT_TIMESTAMP < OLD.retention_until THEN
            RAISE EXCEPTION 'retention 窗口未满禁止 purge（attachment_id=% until=%）',
                OLD.attachment_id, OLD.retention_until
                USING ERRCODE = '23514';
        END IF;
    END IF;

    IF NEW.retention_until < OLD.retention_until THEN
        RAISE EXCEPTION 'retention_until 只可延长（% -> %）', OLD.retention_until, NEW.retention_until
            USING ERRCODE = '23514';
    END IF;

    IF NEW.attachment_id <> OLD.attachment_id
        OR NEW.organization_id <> OLD.organization_id
        OR NEW.application_id <> OLD.application_id THEN
        RAISE EXCEPTION '治理行的 (attachment_id, organization_id, application_id) 不可变更'
            USING ERRCODE = '23514';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: fn_efapi_group_origin_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_efapi_group_origin_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'enterprise_group_origin 行禁止物理删除（归档走 status=archived）'
            USING ERRCODE = '23514';
    END IF;
    IF OLD.status = 'archived' AND NEW.status <> 'archived' THEN
        RAISE EXCEPTION '企业群归档为单向操作（archived 不可回退）' USING ERRCODE = '23514';
    END IF;
    IF NEW.group_id <> OLD.group_id
        OR NEW.organization_id <> OLD.organization_id
        OR NEW.application_id <> OLD.application_id
        OR NEW.workspace_id <> OLD.workspace_id THEN
        RAISE EXCEPTION '企业群归属键 (group_id, organization_id, application_id, workspace_id) 不可变更'
            USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: fn_efapi_message_origin_no_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_efapi_message_origin_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'enterprise_message_origin 行禁止物理删除（origin 证据只增不减）'
        USING ERRCODE = '23514';
END;
$$;


--
-- Name: fn_efapi_usage_no_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_efapi_usage_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION 'enterprise_application_usage 行禁止物理删除（聚合计量只增不减）'
        USING ERRCODE = '23514';
END;
$$;


--
-- Name: fn_enterprise_application_grant_no_delete(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_application_grant_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION
        'enterprise_application_grant 是授权真源行，禁止 DELETE；撤销请置 status=revoked（删除会静默改变权限面与审计 lineage）'
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_enterprise_application_grant_no_delete';
END;
$$;


--
-- Name: FUNCTION fn_enterprise_application_grant_no_delete(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_application_grant_no_delete() IS 'Application Grant 授权行物理删除守卫：DELETE 一律 23514（撤销走 status=revoked，保留 lineage 且撤权即时生效）';


--
-- Name: fn_enterprise_application_redirect_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_application_redirect_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
DECLARE
    v_uri text;
BEGIN
    IF NEW.allowed_redirect_uris IS NULL THEN
        RAISE EXCEPTION
            'allowed_redirect_uris 不可为 NULL；空 allowlist 请用空数组 {}（= 一律拒绝）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_application_redirect_guard';
    END IF;
    IF cardinality(NEW.allowed_redirect_uris) > 20 THEN
        RAISE EXCEPTION
            'redirect URI allowlist 最多 20 个元素，当前 %',
            cardinality(NEW.allowed_redirect_uris)
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_application_redirect_guard';
    END IF;
    IF (SELECT count(DISTINCT u) FROM unnest(NEW.allowed_redirect_uris) AS t(u))
        <> cardinality(NEW.allowed_redirect_uris) THEN
        RAISE EXCEPTION
            'redirect URI allowlist 含重复元素（allowlist 语义无需重复项）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_application_redirect_guard';
    END IF;
    FOREACH v_uri IN ARRAY NEW.allowed_redirect_uris LOOP
        IF v_uri IS NULL OR v_uri = '' THEN
            RAISE EXCEPTION
                'redirect URI allowlist 元素不可为空串'
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_application_redirect_guard';
        END IF;
        IF v_uri !~ '^https://[^/?#]+([/?]|$)' THEN
            RAISE EXCEPTION
                'redirect URI (%) 必须是 https://<非空 host>[/?...] 形态（仅小写 https scheme，无通配）',
                v_uri
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_application_redirect_guard';
        END IF;
        IF position('#' IN v_uri) > 0 THEN
            RAISE EXCEPTION
                'redirect URI (%) 不可携带 fragment（#），注册与消费均为全字符串精确匹配',
                v_uri
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_application_redirect_guard';
        END IF;
        IF length(v_uri) > 2048 THEN
            RAISE EXCEPTION
                'redirect URI (%) 长度超限（>2048）', v_uri
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_application_redirect_guard';
        END IF;
    END LOOP;
    RETURN NEW;
END;
$_$;


--
-- Name: FUNCTION fn_enterprise_application_redirect_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_application_redirect_guard() IS 'redirect URI allowlist 存储守卫：元素 https://<非空 host> + 禁 fragment + 去重 + ≤20 个，否则 23514（EPGZ-05 §4 对 A1 硬需求；exact match 语义由 = ANY 逐字节比较保证）';


--
-- Name: fn_enterprise_asset_purge_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_asset_purge_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_role regrole;
    v_holds integer;
BEGIN
    IF coalesce(current_setting('imboy.enterprise_purge', true), '') <> 'on' THEN
        RAISE EXCEPTION
            '拒绝物理删除企业附件 %：未设置 imboy.enterprise_purge=on（不在 bounded purge worker 上下文）', OLD.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_asset_purge_guard';
    END IF;

    v_role := to_regrole('imboy_enterprise_purge_worker');
    IF v_role IS NULL THEN
        RAISE EXCEPTION
            '拒绝物理删除企业附件 %：cluster 角色 imboy_enterprise_purge_worker 不存在，当前角色 % 不具备 purge 权限',
            OLD.id, current_user
            USING ERRCODE = '42501',
                  CONSTRAINT = 'trg_enterprise_asset_purge_guard';
    END IF;
    IF NOT pg_has_role(current_user, 'imboy_enterprise_purge_worker', 'MEMBER') THEN
        RAISE EXCEPTION
            '拒绝物理删除企业附件 %：当前角色 % 不是 imboy_enterprise_purge_worker 成员（普通角色不得直删）',
            OLD.id, current_user
            USING ERRCODE = '42501',
                  CONSTRAINT = 'trg_enterprise_asset_purge_guard';
    END IF;

    IF OLD.retain_until > now() THEN
        RAISE EXCEPTION
            '拒绝物理删除企业附件 %：retain_until=% 尚未到期', OLD.id, OLD.retain_until
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_asset_purge_guard';
    END IF;

    SELECT count(*) INTO v_holds
      FROM enterprise_retention_hold h
     WHERE h.organization_id = OLD.organization_id
       AND h.released_at IS NULL
       AND ((h.scope_type = 'message' AND h.scope_message_id = OLD.message_id)
            OR (h.scope_type = 'conversation' AND h.scope_conversation_id = OLD.conversation_id)
            OR (h.scope_type = 'workspace' AND h.workspace_id = OLD.workspace_id));
    IF v_holds > 0 THEN
        RAISE EXCEPTION
            '拒绝物理删除企业附件 %：存在 % 条覆盖其消息/会话/工作区的 active hold', OLD.id, v_holds
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_asset_purge_guard';
    END IF;

    RETURN OLD;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_asset_purge_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_asset_purge_guard() IS '企业附件物理删除守卫（BEFORE DELETE，四步按序判定）：1) GUC imboy.enterprise_purge=''on'' 否则 23514；2) 当前角色必须是 cluster 角色 imboy_enterprise_purge_worker 的成员（角色由部署/DBA 预置，migration 不 CREATE ROLE），否则 42501；3) retain_until <= now() 否则 23514；4) 无 active hold 覆盖其 message/conversation/workspace，否则 23514';


--
-- Name: fn_enterprise_asset_retention_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_asset_retention_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_message_retain timestamp with time zone;
BEGIN
    IF NEW.message_id IS NOT NULL THEN
        IF NEW.retain_until IS NULL THEN
            RAISE EXCEPTION
                'enterprise_asset % 绑定消息 % 时必须写入 retain_until（附件不得早于所属消息删除）',
                NEW.id, NEW.message_id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_asset_retention_guard';
        END IF;
        SELECT m.retain_until INTO v_message_retain
          FROM enterprise_message m
         WHERE m.organization_id = NEW.organization_id
           AND m.workspace_id = NEW.workspace_id
           AND m.id = NEW.message_id;
        IF v_message_retain IS NOT NULL AND NEW.retain_until < v_message_retain THEN
            RAISE EXCEPTION
                'enterprise_asset % 的 retain_until=% 早于所属消息 % 的 retain_until=%（附件不得先于消息删除）',
                NEW.id, NEW.retain_until, NEW.message_id, v_message_retain
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_asset_retention_guard';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_asset_retention_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_asset_retention_guard() IS '附件保留期守卫：message_id 非空时要求 retain_until 非空且 >= 所属消息的 retain_until，否则 23514';


--
-- Name: fn_enterprise_audit_event_append_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_audit_event_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'enterprise_audit_event 是 append-only 审计真源，禁止 DELETE'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_audit_event_append_only';
    END IF;

    -- 唯一允许的变更：user 行被删除时，actor_user_id 由外键 ON DELETE SET NULL 置 NULL。
    -- 审计内容（Org/resource/action/identity/role/detail/时间）与既有 actor 一律不可改写：
    -- 若除 actor_user_id 外的任一列变化，或 actor_user_id 被写成新的非 NULL 值，一律拒绝。
    IF (NEW.id, NEW.organization_id, NEW.resource_type, NEW.resource_id, NEW.action,
        NEW.business_identity_id, NEW.actor_role, NEW.detail, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.resource_type, OLD.resource_id, OLD.action,
        OLD.business_identity_id, OLD.actor_role, OLD.detail, OLD.created_at)
       OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'enterprise_audit_event 是 append-only 审计真源，禁止 UPDATE（仅允许 user 删除时 actor_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_audit_event_append_only';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_audit_event_append_only(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_audit_event_append_only() IS '企业审计 append-only 守卫：DELETE 一律 23514；UPDATE 一律 23514，唯一例外是 fk_eae_actor 的 ON DELETE SET NULL 把 actor_user_id 置 NULL（审计内容与既有 actor 不可改写，只允许 INSERT 追加）';


--
-- Name: fn_enterprise_external_identity_member_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_external_identity_member_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_member_status text;
    v_account_type  smallint;
    v_user_status   smallint;
BEGIN
    IF NEW.status = 'active' THEN
        SELECT om.status, u.account_type, u.status
          INTO v_member_status, v_account_type, v_user_status
          FROM organization_member om
          JOIN "user" u ON u.id = om.user_id
         WHERE om.organization_id = NEW.organization_id
           AND om.user_id = NEW.user_id;

        IF v_member_status IS NULL THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 (%，%) 不是 organization_member，不能绑定 active 映射',
                NEW.organization_id, NEW.user_id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
        IF v_member_status <> 'active' THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 member (%) 状态为 %，仅 active 成员可绑定映射',
                NEW.user_id, v_member_status
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
        IF v_account_type <> 0 THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 (%) account_type=%，仅 Human(account_type=0) 可绑定映射',
                NEW.user_id, v_account_type
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
        IF v_user_status <> 1 THEN
            RAISE EXCEPTION
                'enterprise_external_identity 目标 (%) user.status=%，仅正常(1)账号可绑定映射',
                NEW.user_id, v_user_status
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_external_identity_member_guard';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_external_identity_member_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_external_identity_member_guard() IS 'active 映射目标守卫：同 Org active organization_member + Human(account_type=0) + user.status=1，否则 23514（plan-gz §5「目标必须是 active Human member」）';


--
-- Name: fn_enterprise_message_actor_required(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_message_actor_required() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.sender_type = 'business_identity' AND NEW.actor_user_id IS NULL THEN
        RAISE EXCEPTION
            'enterprise_message % 为出站消息（business_identity），必须在写入时记录 actor_user_id',
            NEW.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_actor_required';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_message_actor_required(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_message_actor_required() IS '出站消息 actor 强制：sender_type=business_identity 且 actor_user_id IS NULL 时 23514。刻意用触发器而非 CHECK，使删除 user 引发的 SET NULL 仍可成功（A03 不级联企业数据）';


--
-- Name: fn_enterprise_message_purge_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_message_purge_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_role regrole;
    v_holds integer;
BEGIN
    -- 1) 必须在 bounded purge 上下文
    IF coalesce(current_setting('imboy.enterprise_purge', true), '') <> 'on' THEN
        RAISE EXCEPTION
            '拒绝物理删除企业消息 %：未设置 imboy.enterprise_purge=on（不在 bounded purge worker 上下文）', OLD.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_purge_guard';
    END IF;

    -- 2) 必须持有 purge worker 角色（角色缺失/非成员一律 42501；失败宁可多保留）
    v_role := to_regrole('imboy_enterprise_purge_worker');
    IF v_role IS NULL THEN
        RAISE EXCEPTION
            '拒绝物理删除企业消息 %：cluster 角色 imboy_enterprise_purge_worker 不存在，当前角色 % 不具备 purge 权限',
            OLD.id, current_user
            USING ERRCODE = '42501',
                  CONSTRAINT = 'trg_enterprise_message_purge_guard';
    END IF;
    IF NOT pg_has_role(current_user, 'imboy_enterprise_purge_worker', 'MEMBER') THEN
        RAISE EXCEPTION
            '拒绝物理删除企业消息 %：当前角色 % 不是 imboy_enterprise_purge_worker 成员（普通角色不得直删）',
            OLD.id, current_user
            USING ERRCODE = '42501',
                  CONSTRAINT = 'trg_enterprise_message_purge_guard';
    END IF;

    -- 3) retain_until 必须已到期
    IF OLD.retain_until > now() THEN
        RAISE EXCEPTION
            '拒绝物理删除企业消息 %：retain_until=% 尚未到期', OLD.id, OLD.retain_until
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_purge_guard';
    END IF;

    -- 4) 不得存在覆盖该消息的 active hold（active hold 优先于 retain_until）
    SELECT count(*) INTO v_holds
      FROM enterprise_retention_hold h
     WHERE h.organization_id = OLD.organization_id
       AND h.released_at IS NULL
       AND ((h.scope_type = 'message' AND h.scope_message_id = OLD.id)
            OR (h.scope_type = 'conversation' AND h.scope_conversation_id = OLD.conversation_id)
            OR (h.scope_type = 'workspace' AND h.workspace_id = OLD.workspace_id));
    IF v_holds > 0 THEN
        RAISE EXCEPTION
            '拒绝物理删除企业消息 %：存在 % 条覆盖该消息的 active hold（active hold 优先于 retain_until）',
            OLD.id, v_holds
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_purge_guard';
    END IF;

    RETURN OLD;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_message_purge_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_message_purge_guard() IS '企业消息物理删除守卫（BEFORE DELETE，四步按序判定）：1) 会话 GUC imboy.enterprise_purge=''on'' 否则 23514；2) 当前角色必须是 cluster 角色 imboy_enterprise_purge_worker 的成员（不 CREATE ROLE，角色由部署/DBA 预置），否则 42501；3) OLD.retain_until <= now() 否则 23514；4) 无 active hold 覆盖该消息，否则 23514。通过后才 RETURN OLD；失败宁可多保留，不得提前删除';


--
-- Name: fn_enterprise_message_retention_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_message_retention_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.retain_until < OLD.retain_until THEN
        RAISE EXCEPTION
            'enterprise_message % 的 retain_until 只能后移：% -> % 被拒绝（禁止提前删除）',
            OLD.id, OLD.retain_until, NEW.retain_until
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_message_retention_guard';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_message_retention_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_message_retention_guard() IS '保留期快照守卫：retain_until 前移（缩短）一律 23514；只允许经审计延长（EB-D12）';


--
-- Name: fn_enterprise_retention_hold_append_only(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_retention_hold_append_only() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'enterprise_retention_hold 是 append-only 事实，禁止 DELETE（只允许一次性 release）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_retention_hold_append_only';
    END IF;

    -- hold 事实本体（含 scope/reason/audit id/version/created_at）永不可变。
    IF (NEW.id, NEW.organization_id, NEW.workspace_id, NEW.scope_type,
        NEW.scope_conversation_id, NEW.scope_message_id, NEW.reason_code,
        NEW.audit_event_id, NEW.version, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.workspace_id, OLD.scope_type,
        OLD.scope_conversation_id, OLD.scope_message_id, OLD.reason_code,
        OLD.audit_event_id, OLD.version, OLD.created_at) THEN
        RAISE EXCEPTION
            'enterprise_retention_hold % 是 append-only 事实，禁止修改 scope/reason/audit id/version/created_at', OLD.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_retention_hold_append_only';
    END IF;

    -- 已释放：只允许 user 删除引发的 released_by_user_id 置 NULL（append-only 不再接受其他变更）。
    IF OLD.released_at IS NOT NULL THEN
        IF NEW.released_at IS DISTINCT FROM OLD.released_at
           OR NEW.released_by_user_id IS NOT NULL THEN
            RAISE EXCEPTION
                'enterprise_retention_hold % 已释放，禁止再次修改（append-only）', OLD.id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_retention_hold_append_only';
        END IF;
        RETURN NEW;
    END IF;

    -- 未释放：release 必须一次性同时写入 released_at 与 released_by_user_id。
    IF NEW.released_at IS NOT NULL THEN
        IF NEW.released_by_user_id IS NULL THEN
            RAISE EXCEPTION
                'enterprise_retention_hold % 的 release 必须同时写入 released_at 与 released_by_user_id', OLD.id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_enterprise_retention_hold_append_only';
        END IF;
        RETURN NEW;
    END IF;

    -- 未释放且未提供 released_at：只允许 user 删除引发的 actor_user_id / released_by_user_id 置 NULL。
    IF NEW.released_by_user_id IS NOT NULL OR NEW.actor_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'enterprise_retention_hold % 只允许 release 一次性写入 released_at/released_by_user_id', OLD.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_retention_hold_append_only';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_retention_hold_append_only(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_retention_hold_append_only() IS 'hold append-only 守卫：DELETE 一律 23514；UPDATE 只允许 (a) 未释放时一次性写入 released_at + released_by_user_id 的 release，或 (b) user 删除引发的 actor_user_id/released_by_user_id 置 NULL；其余变更 23514';


--
-- Name: fn_enterprise_retention_policy_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_retention_policy_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF EXISTS (
        SELECT 1
          FROM enterprise_retention_policy p
         WHERE p.organization_id = NEW.organization_id
           AND p.workspace_id = NEW.workspace_id
           AND p.data_class = NEW.data_class
           AND p.retention_days > NEW.retention_days
    ) THEN
        RAISE EXCEPTION
            'enterprise_retention_policy(%,%,%,v%) 缩短保留期被拒绝：已存在更长期限的版本，retain_until 快照永不前移',
            NEW.organization_id, NEW.workspace_id, NEW.data_class, NEW.version
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_retention_policy_guard';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_retention_policy_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_retention_policy_guard() IS '保留期只增不减守卫（BEFORE INSERT）：同 Org/Workspace/data_class 已存在 retention_days 更大的版本时 23514';


--
-- Name: fn_enterprise_retention_policy_immutable(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_enterprise_retention_policy_immutable() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION
            'enterprise_retention_policy 是不可变策略快照，禁止 DELETE；请追加新版本'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_retention_policy_immutable';
    END IF;

    IF (NEW.id, NEW.organization_id, NEW.workspace_id, NEW.data_class, NEW.version,
        NEW.retention_days, NEW.trigger_event, NEW.effective_at, NEW.created_at)
       IS DISTINCT FROM
       (OLD.id, OLD.organization_id, OLD.workspace_id, OLD.data_class, OLD.version,
        OLD.retention_days, OLD.trigger_event, OLD.effective_at, OLD.created_at)
       OR NEW.created_by_user_id IS NOT NULL THEN
        RAISE EXCEPTION
            'enterprise_retention_policy 是不可变策略快照，禁止 UPDATE（仅允许 user 删除时 created_by_user_id 置 NULL）'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_enterprise_retention_policy_immutable';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_enterprise_retention_policy_immutable(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_enterprise_retention_policy_immutable() IS '保留策略不可变守卫：DELETE 一律 23514；UPDATE 一律 23514，唯一例外是 fk_erp_created_by 的 ON DELETE SET NULL 把 created_by_user_id 置 NULL（期限/类别/版本等快照内容不可改写）';


--
-- Name: fn_ewh_delivery_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_ewh_delivery_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    allowed text[];
BEGIN
    IF TG_OP = 'INSERT' THEN
        IF NEW.bot_id NOT LIKE 'eapp:%' THEN
            RETURN NEW;
        END IF;
        IF NEW.status <> 'pending' THEN
            RAISE EXCEPTION
                'ewh_delivery_insert_status: enterprise delivery must be born pending (got %)',
                NEW.status
                USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        IF NEW.ewh_owner_organization_id IS NULL OR NEW.ewh_owner_application_id IS NULL THEN
            RAISE EXCEPTION
                'ewh_delivery_insert_owner_required: enterprise delivery needs org+application owner'
                USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        -- owner <-> bot_id <-> principal 三元一致：投递行只能属于「其 bot_id 所指
        -- 可信 principal 自己的 Application」——防止一行被写成「用 A 的密钥签发给
        -- B 的端点」（跨 Application 凭证/端点串号，DB 层 fail-closed）。
        IF NOT EXISTS (
            SELECT 1
            FROM enterprise_application a
            JOIN bot b ON b.user_id = a.principal_user_id
            WHERE
                a.organization_id = NEW.ewh_owner_organization_id
                AND a.id = NEW.ewh_owner_application_id
                AND NEW.bot_id = 'eapp:' || b.user_id::text
        ) THEN
            RAISE EXCEPTION
                'ewh_delivery_owner_principal_mismatch: bot_id / owner application / principal disagree'
                USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        IF NEW.ewh_replay_of IS NOT NULL AND NEW.ewh_replay_of = NEW.delivery_id THEN
            RAISE EXCEPTION 'ewh_delivery_replay_not_self' USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
        END IF;
        -- 版本号由守卫独占写入：客户端传值一律被覆盖。
        NEW.ewh_ledger_version := 1;
        NEW.ewh_claimed_at := NULL;
        RETURN NEW;
    END IF;

    -- UPDATE：非企业行（含 OLD 与 NEW 两侧）一律放行
    IF NEW.bot_id NOT LIKE 'eapp:%' AND OLD.bot_id NOT LIKE 'eapp:%' THEN
        RETURN NEW;
    END IF;

    -- 归属与端点快照不可变（配置变更不得改写在途投递的目标/归属/载荷）
    IF NEW.delivery_id <> OLD.delivery_id
        OR NEW.bot_id <> OLD.bot_id
        OR NEW.event_type <> OLD.event_type
        OR NEW.payload::text <> OLD.payload::text
        OR NEW.idempotency_key <> OLD.idempotency_key
        OR NEW.correlation_id <> OLD.correlation_id
        OR NEW.webhook_url <> OLD.webhook_url
        OR NEW.webhook_host <> OLD.webhook_host
        OR NEW.pinned_ip <> OLD.pinned_ip
        OR NEW.created_at <> OLD.created_at
        OR NEW.ewh_owner_organization_id IS DISTINCT FROM OLD.ewh_owner_organization_id
        OR NEW.ewh_owner_application_id IS DISTINCT FROM OLD.ewh_owner_application_id
        OR NEW.ewh_replay_of IS DISTINCT FROM OLD.ewh_replay_of
        OR NEW.ewh_endpoint_generation <> OLD.ewh_endpoint_generation
    THEN
        RAISE EXCEPTION
            'ewh_delivery_snapshot_immutable: endpoint snapshot / ownership / payload is frozen'
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    -- 终态不可回退 + 合法状态图（pending/retry -> pending|success|retry|dead；
    -- success -> success；dead -> dead）
    allowed := CASE OLD.status
        WHEN 'pending' THEN ARRAY['pending', 'success', 'retry', 'dead']
        WHEN 'retry' THEN ARRAY['pending', 'success', 'retry', 'dead']
        WHEN 'success' THEN ARRAY['success']
        WHEN 'dead' THEN ARRAY['dead']
        ELSE ARRAY[OLD.status]
    END;
    IF NOT (NEW.status = ANY (allowed)) THEN
        RAISE EXCEPTION
            'ewh_delivery_terminal_immutable: illegal transition % -> %', OLD.status, NEW.status
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    -- 终态行为冻结记录：状态、尝试次数、下次重试时间都不得再变
    IF OLD.status IN ('success', 'dead')
        AND (
            NEW.attempt_count <> OLD.attempt_count
            OR NEW.next_retry_at <> OLD.next_retry_at
        )
    THEN
        RAISE EXCEPTION
            'ewh_delivery_terminal_frozen: terminal row is a frozen record'
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    IF NEW.attempt_count < OLD.attempt_count THEN
        RAISE EXCEPTION
            'ewh_delivery_attempt_monotonic: attempt_count must not decrease (% -> %)',
            OLD.attempt_count, NEW.attempt_count
            USING ERRCODE = '23514',
            CONSTRAINT = 'trg_ewh_delivery_guard';
    END IF;

    NEW.ewh_ledger_version := OLD.ewh_ledger_version + 1;
    RETURN NEW;
END $$;


--
-- Name: fn_group_member_ws_subset_check(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_group_member_ws_subset_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_ws bigint;
BEGIN
    SELECT workspace_id INTO v_ws FROM "group" WHERE id = NEW.group_id;
    IF v_ws IS NULL THEN
        RETURN NEW;  -- personal 群或群不存在（群不存在会被业务层拒绝，此处放行）
    END IF;
    IF NEW.status = 1 AND NOT EXISTS (
        SELECT 1 FROM workspace_member
         WHERE workspace_id = v_ws AND user_id = NEW.user_id AND status = 'active'
    ) THEN
        RAISE EXCEPTION
            'group_member 子集约束（Group Member ⊆ Workspace Member）：'
            '用户 % 不是 workspace % 的 active workspace_member，不能成为 workspace 群 % 的 active 成员',
            NEW.user_id, v_ws, NEW.group_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_group_member_ws_subset',
                  HINT = '先将该用户加入 workspace（workspace_member.status=active），再写入/恢复群成员';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_group_member_ws_subset_check(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_group_member_ws_subset_check() IS 'Workspace 群成员写入端子集校验：active group_member 提交时必须存在同 workspace 的 active workspace_member（I14，可延迟到 COMMIT）';


--
-- Name: fn_gta_learner_org_check(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_gta_learner_org_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_task_org bigint;
    v_learner_org bigint;
BEGIN
    IF NEW.learner_id IS NULL THEN
        RETURN NEW;  -- 普通群作业：不参与教学机构约束（DB-COMPAT-01）
    END IF;

    SELECT w.organization_id INTO v_task_org
      FROM group_task gt
      JOIN "group" g ON g.id = gt.group_id
      JOIN workspace w ON w.id = g.workspace_id
     WHERE gt.task_id = NEW.task_id;

    SELECT organization_id INTO v_learner_org
      FROM learner
     WHERE id = NEW.learner_id;

    IF v_task_org IS NULL OR v_task_org <> v_learner_org THEN
        RAISE EXCEPTION
            '作业机构一致性：learner % 属于机构 %，但作业 % 所属班级群解析机构为 %，'
            '跨机构教学 assignment 被拒绝',
            NEW.learner_id, v_learner_org, NEW.task_id, v_task_org
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_gta_learner_org_consistency',
                  HINT = '教学 assignment 的 task 所属群必须与 learner 同一 Organization';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_gta_learner_org_check(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_gta_learner_org_check() IS '教学 assignment 机构一致性：learner.organization_id == task→group→workspace→organization（提交时校验，可延迟；learner_id 为空的普通作业不受约束）';


--
-- Name: fn_homework_submission_learner_check(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_homework_submission_learner_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_assignment_learner bigint;
BEGIN
    SELECT learner_id INTO v_assignment_learner
      FROM group_task_assignment
     WHERE id = NEW.assignment_id;

    IF v_assignment_learner IS NULL OR v_assignment_learner <> NEW.learner_id THEN
        RAISE EXCEPTION
            '提交学员一致性：submission.learner_id(%) 与 assignment % 的 learner_id(%) 不一致',
            NEW.learner_id, NEW.assignment_id, v_assignment_learner
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_homework_submission_learner_consistency',
                  HINT = '教学提交必须挂在 learner_id 匹配的教学 assignment 上（assignment.learner_id 不得为空）';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_homework_submission_learner_check(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_homework_submission_learner_check() IS '提交学员一致性：homework_submission.learner_id == group_task_assignment.learner_id 且 assignment 必须是教学作业（提交时校验，可延迟）';


--
-- Name: fn_homework_submission_withdraw_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_homework_submission_withdraw_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.status = 'withdrawn' AND EXISTS (
        SELECT 1 FROM teacher_review
         WHERE submission_id = NEW.id AND status = 'published'
    ) THEN
        RAISE EXCEPTION
            '撤回互斥：submission % 已有已发布老师回评，不能撤回（计划 §7.2）',
            NEW.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_homework_submission_withdraw_guard',
                  HINT = '已发布的回评不可被家长撤回；如需撤回内容走老师/管理员删除流程';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_homework_submission_withdraw_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_homework_submission_withdraw_guard() IS '撤回互斥兜底：已有 published teacher_review 的 submission 禁止转入 withdrawn（语句结束时新鲜快照复查，拦截 READ COMMITTED 单语句守卫的竞态）';


--
-- Name: fn_learner_org_change_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_learner_org_change_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.organization_id IS DISTINCT FROM OLD.organization_id
       AND EXISTS (SELECT 1 FROM class_enrollment WHERE learner_id = NEW.id)
    THEN
        RAISE EXCEPTION
            'learner 机构防篡改：学员 % 已有入班记录，禁止更换机构 % -> %',
            NEW.id, OLD.organization_id, NEW.organization_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_learner_org_change_guard',
                  HINT = '如需迁移学员机构，先移除其全部 class_enrollment 再变更';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_learner_org_change_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_learner_org_change_guard() IS 'learner 机构防篡改：存在 class_enrollment 时禁止变更 organization_id（立即触发）';


--
-- Name: fn_message_cross_timestamp_dedup(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_message_cross_timestamp_dedup() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
DECLARE
    existing_created_at timestamptz;
BEGIN
    PERFORM pg_advisory_xact_lock(
        hashtextextended(TG_ARGV[0] || ':' || NEW.msg_id, 0)
    );

    EXECUTE format(
        'SELECT created_at FROM public.%I WHERE msg_id = $1 ORDER BY created_at ASC LIMIT 1',
        TG_ARGV[0]
    ) INTO existing_created_at USING NEW.msg_id;

    IF existing_created_at IS NULL OR existing_created_at = NEW.created_at THEN
        RETURN NEW;
    END IF;
    RETURN NULL;
END;
$_$;


--
-- Name: fn_moment_touch_updated_at(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_moment_touch_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--
-- Name: fn_organization_business_identity_function_immutable(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_business_identity_function_immutable() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.function_key IS DISTINCT FROM OLD.function_key THEN
        RAISE EXCEPTION
            'organization_business_identity % 的 function_key 创建后不可变（%->%），请新建 identity',
            OLD.id, OLD.function_key, NEW.function_key
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_business_identity_function_immutable';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: fn_organization_default_workspace_active_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_default_workspace_active_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_workspace_org bigint;
    v_status        text;
BEGIN
    SELECT organization_id, status INTO v_workspace_org, v_status
      FROM workspace
     WHERE id = NEW.workspace_id;
    IF v_workspace_org IS NULL THEN
        RAISE EXCEPTION
            'workspace % 不存在，不能设为 Org % 的默认 Workspace',
            NEW.workspace_id, NEW.organization_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_default_workspace_active_guard';
    END IF;
    IF v_status IS NULL OR v_status <> 'active' THEN
        RAISE EXCEPTION
            'workspace % 不是 active（当前 %），不能担任 Org 默认 Workspace',
            NEW.workspace_id, v_status
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_default_workspace_active_guard';
    END IF;
    -- 冗余 org 列以 workspace 行为准派生（单一真源；传错 org 被覆盖，
    -- 与组合 FK 双保险杜绝跨 Org 脏行）
    NEW.organization_id := v_workspace_org;
    RETURN NEW;
END;
$$;


--
-- Name: fn_organization_department_cycle_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_department_cycle_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_cursor bigint;
    v_depth  integer := 0;
BEGIN
    v_cursor := NEW.parent_id;
    WHILE v_cursor IS NOT NULL AND v_depth < 10000 LOOP
        IF v_cursor = NEW.id THEN
            RAISE EXCEPTION
                'organization_department % 的 parent 链回到自身（祖先环）',
                NEW.id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_organization_department_cycle_guard';
        END IF;
        SELECT parent_id INTO v_cursor
          FROM organization_department
         WHERE id = v_cursor;
        v_depth := v_depth + 1;
    END LOOP;
    -- 循环自然终止（parent 链到达根）即无环；深度上限防异常数据死循环
    RETURN NEW;
END;
$$;


--
-- Name: fn_organization_department_member_active_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_department_member_active_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_department_org bigint;
    v_status         text;
BEGIN
    SELECT organization_id INTO v_department_org
      FROM organization_department
     WHERE id = NEW.department_id;
    IF v_department_org IS NULL THEN
        RAISE EXCEPTION
            'department % 不存在，不能挂成员',
            NEW.department_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_department_member_active_guard';
    END IF;
    -- 冗余 org 列以 department 行为准派生（调用方传值被覆盖）
    NEW.organization_id := v_department_org;

    SELECT status INTO v_status
      FROM organization_member
     WHERE organization_id = NEW.organization_id
       AND user_id = NEW.user_id;
    IF v_status IS NULL OR v_status <> 'active' THEN
        RAISE EXCEPTION
            'user % 不是 organization % 的 active member，不能加入部门',
            NEW.user_id, NEW.organization_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_department_member_active_guard';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: fn_organization_member_offboarding_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_member_offboarding_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF (TG_OP = 'DELETE' AND OLD.status <> 'removed')
       OR (TG_OP = 'UPDATE' AND NEW.status = 'removed') THEN
        IF EXISTS (
            SELECT 1
              FROM organization_business_identity_assignment a
             WHERE a.organization_id = OLD.organization_id
               AND a.user_id = OLD.user_id
               AND a.status = 'active'
        ) THEN
            RAISE EXCEPTION
                'organization_member(%,%) 仍持有 active 企业经办关系，直接移除被拒绝：offboarding_required',
                OLD.organization_id, OLD.user_id
                USING ERRCODE = '23514',
                      CONSTRAINT = 'trg_organization_member_offboarding_guard';
        END IF;
    END IF;
    IF TG_OP = 'DELETE' THEN
        RETURN OLD;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_organization_member_offboarding_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_organization_member_offboarding_guard() IS '职务离场守卫：存在 active organization_business_identity_assignment 时，organization_member 的 removed 变更或 DELETE 一律 23514（offboarding_required）；suspended 不拦截';


--
-- Name: fn_organization_member_owner_invariant(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_member_owner_invariant() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_org_id            bigint;
    v_owner_id          bigint;
    v_human_owner_count integer;
    v_owner_user_id     bigint;
BEGIN
    IF TG_OP = 'DELETE' THEN
        v_org_id := OLD.organization_id;
    ELSE
        v_org_id := NEW.organization_id;
    END IF;

    SELECT o.owner_id INTO v_owner_id
      FROM organization o
     WHERE o.id = v_org_id;
    IF v_owner_id IS NULL THEN
        -- organization 行已随本事务删除（CASCADE），整体消失，无需 owner 不变量。
        RETURN NULL;
    END IF;

    SELECT count(*), min(om.user_id)
      INTO v_human_owner_count, v_owner_user_id
      FROM organization_member om
      JOIN "user" u ON u.id = om.user_id
     WHERE om.organization_id = v_org_id
       AND om.role = 'owner'
       AND om.status = 'active'
       AND u.account_type = 0;

    IF v_human_owner_count <> 1 OR v_owner_user_id <> v_owner_id THEN
        RAISE EXCEPTION
            'organization_member 提交校验失败：organization % 必须恰好一个 active Human owner（实际 % 个）且与 owner_id % 一致',
            v_org_id, v_human_owner_count, v_owner_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_member_owner_invariant',
                  COLUMN = 'role',
                  TABLE = 'organization_member';
    END IF;
    RETURN NULL;
END;
$$;


--
-- Name: fn_organization_owner_invariant(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_owner_invariant() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_org_id            bigint;
    v_owner_id          bigint;
    v_human_owner_count integer;
    v_owner_user_id     bigint;
BEGIN
    IF TG_OP = 'DELETE' THEN
        -- org 行删除：成员行随 FK CASCADE 一并消失，不存在 owner 不变量约束。
        RETURN NULL;
    END IF;
    v_org_id   := NEW.id;
    v_owner_id := NEW.owner_id;

    SELECT count(*), min(om.user_id)
      INTO v_human_owner_count, v_owner_user_id
      FROM organization_member om
      JOIN "user" u ON u.id = om.user_id
     WHERE om.organization_id = v_org_id
       AND om.role = 'owner'
       AND om.status = 'active'
       AND u.account_type = 0;

    IF v_human_owner_count <> 1 THEN
        RAISE EXCEPTION
            'organization % 提交校验失败：必须恰好一个 active Human owner（实际 % 个）',
            v_org_id, v_human_owner_count
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_owner_invariant',
                  COLUMN = 'owner_id',
                  TABLE = 'organization';
    END IF;
    IF v_owner_user_id <> v_owner_id THEN
        RAISE EXCEPTION
            'organization % 提交校验失败：active Human owner % 与 owner_id % 投影不一致',
            v_org_id, v_owner_user_id, v_owner_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_owner_invariant',
                  COLUMN = 'owner_id',
                  TABLE = 'organization';
    END IF;
    RETURN NULL;
END;
$$;


--
-- Name: fn_organization_owner_member_sync(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_owner_member_sync() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO organization_member (
        organization_id, user_id, role, joined_at, status, created_at, updated_at
    ) VALUES (
        NEW.id, NEW.owner_id, 'owner', CURRENT_TIMESTAMP, 'active',
        CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
    )
    ON CONFLICT (organization_id, user_id) DO UPDATE
       SET role = 'owner', status = 'active', updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$;


--
-- Name: fn_organization_primary_owner_member_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_organization_primary_owner_member_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM organization
         WHERE id = OLD.organization_id AND owner_id = OLD.user_id
    ) AND (
        TG_OP = 'DELETE' OR NEW.role <> 'owner' OR NEW.status <> 'active'
    ) THEN
        RAISE EXCEPTION
            'organization % 的主 Owner % 不能被移除或降级，请先转移 organization.owner_id',
            OLD.organization_id, OLD.user_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_organization_primary_owner_member_guard';
    END IF;
    IF TG_OP = 'DELETE' THEN
        RETURN OLD;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: fn_project_links_shape_check(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_project_links_shape_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    el jsonb;
BEGIN
    IF NEW.links IS NULL OR jsonb_typeof(NEW.links) <> 'array' THEN
        RAISE EXCEPTION
            'project.links 形状校验失败：links 必须是 jsonb 数组（收到 %）',
            COALESCE(jsonb_typeof(NEW.links), 'NULL')
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_project_links_shape';
    END IF;
    FOR el IN SELECT * FROM jsonb_array_elements(NEW.links) LOOP
        IF jsonb_typeof(el) <> 'object'
           OR (el->>'name' IS NULL) OR (el->>'url' IS NULL)
           OR length(el->>'name') = 0 OR length(el->>'url') = 0
        THEN
            RAISE EXCEPTION
                'project.links 形状校验失败：每个元素必须是含非空 name 与 url 字符串的对象（收到 %）',
                el
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_project_links_shape';
        END IF;
    END LOOP;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_project_links_shape_check(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_project_links_shape_check() IS 'project.links 形状校验：必须是 [{name,url}] 数组且 name/url 非空字符串（W2 Resources 聚合载体）';


--
-- Name: fn_project_member_ws_active_check(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_project_member_ws_active_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.status = 'active' AND NOT EXISTS (
        SELECT 1 FROM workspace_member
         WHERE workspace_id = NEW.workspace_id
           AND user_id = NEW.user_id
           AND status = 'active'
    ) THEN
        RAISE EXCEPTION
            'project_member 子集约束（Project Member ⊆ Workspace Member）：'
            '用户 % 不是 workspace % 的 active workspace_member，不能成为 project % 的 active 成员',
            NEW.user_id, NEW.workspace_id, NEW.project_id
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_project_member_ws_active',
              HINT = '先将该用户加入 workspace（workspace_member.status=active），再写入/恢复项目成员';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_project_member_ws_active_check(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_project_member_ws_active_check() IS 'Project 成员写入端子集校验：active project_member 提交时必须是同 workspace 的 active workspace_member（W2，可延迟到 COMMIT）';


--
-- Name: fn_project_owner_membership_active(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_project_owner_membership_active() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM workspace_member
         WHERE workspace_id = NEW.workspace_id
           AND user_id = NEW.owner_id
           AND status = 'active'
    ) THEN
        RAISE EXCEPTION
            'project Owner 校验：用户 % 不是 workspace % 的 active workspace_member，'
            '不能成为项目 % 的 owner',
            NEW.owner_id, NEW.workspace_id, NEW.id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_project_owner_membership_active',
                  HINT = '同一事务内先写入/激活该用户的 workspace_member(active) 记录，再提交项目';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_project_owner_membership_active(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_project_owner_membership_active() IS '项目 Owner 子集校验：owner 提交时必须是同 workspace 的 active workspace_member（W0 版，无 Project Member 触发器，可延迟到 COMMIT）';


--
-- Name: fn_report_ticket_touch_updated_at(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_report_ticket_touch_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--
-- Name: fn_review_asset_image_cap(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_review_asset_image_cap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_count integer;
BEGIN
    SELECT count(*) INTO v_count
      FROM review_asset
     WHERE review_id = NEW.review_id AND kind = 'feedback_image';
    IF v_count > 3 THEN
        RAISE EXCEPTION
            '回评图片上限：review % 已关联 % 张反馈图片（上限 3）',
            NEW.review_id, v_count
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_asset_image_cap',
                  HINT = '单个回评最多 3 张 feedback_image（P0-4 媒体约束）';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_review_asset_image_cap(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_review_asset_image_cap() IS '回评图片上限兜底：单 review 的 feedback_image 计数 >3 拒绝（语句结束时新鲜快照复查，多行插入同步拦截）';


--
-- Name: fn_teacher_review_publish_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_teacher_review_publish_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_sub_status text;
BEGIN
    IF NEW.status = 'published' THEN
        SELECT status INTO v_sub_status FROM homework_submission WHERE id = NEW.submission_id;
        IF v_sub_status = 'withdrawn' THEN
            RAISE EXCEPTION
                '发布互斥：submission % 已被监护人撤回，不能发布回评（计划 §7.2：撤回后老师队列即时移除）',
                NEW.submission_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_teacher_review_publish_guard',
                  HINT = '已撤回的提交不再进入待评队列；如内容违规走删除/审核流程';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_teacher_review_publish_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_teacher_review_publish_guard() IS '发布互斥兜底：withdrawn 的 submission 禁止发布 teacher_review（与撤回侧触发器构成双向互斥）';


--
-- Name: fn_update_channel_message_reaction_summary(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_update_channel_message_reaction_summary() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    UPDATE public.channel_message cm
    SET reaction_summary = COALESCE((
        SELECT jsonb_object_agg(reaction_type, cnt)
        FROM (
            SELECT reaction_type, COUNT(*) as cnt
            FROM public.channel_reaction
            WHERE message_id = COALESCE(NEW.message_id, OLD.message_id)
            GROUP BY reaction_type
        ) sub
    ), '{}'::jsonb)
    WHERE id = COALESCE(NEW.message_id, OLD.message_id);
    RETURN COALESCE(NEW, OLD);
END;
$$;


--
-- Name: fn_update_channel_message_view_count(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: fn_workspace_member_remove_guard(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_workspace_member_remove_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'UPDATE' AND NEW.status <> 'removed' THEN
        RETURN NEW;  -- 仅约束 active → removed 的移除方向（恢复 active 不受限）
    END IF;
    IF EXISTS (
        SELECT 1
          FROM group_member gm
          JOIN "group" g ON g.id = gm.group_id
         WHERE g.workspace_id = OLD.workspace_id
           AND g.scope = 'workspace'
           AND gm.user_id = OLD.user_id
           AND gm.status = 1
    ) THEN
        RAISE EXCEPTION
            'workspace_member 移除保护：用户 % 在 workspace % 仍有 active 的 workspace 群成员关系，'
            '不能移除其工作区成员身份（Group Member ⊆ Workspace Member）',
            OLD.user_id, OLD.workspace_id
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_workspace_member_remove_guard',
                  HINT = '同一事务内先禁用/移除该用户在 workspace 群中的 active 成员记录，再移除工作区成员';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_workspace_member_remove_guard(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_workspace_member_remove_guard() IS '工作区成员移除端保护：removed/DELETE 前同 workspace 不得有该用户的 active workspace 群成员（W0 下级关系=group_member，可延迟到 COMMIT）';


--
-- Name: fn_workspace_member_remove_guard_pm(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fn_workspace_member_remove_guard_pm() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'UPDATE' AND NEW.status <> 'removed' THEN
        RETURN NEW;  -- 仅约束 active → removed 的移除方向（恢复 active 不受限）
    END IF;
    IF EXISTS (
        SELECT 1
          FROM project_member pm
         WHERE pm.workspace_id = OLD.workspace_id
           AND pm.user_id = OLD.user_id
           AND pm.status = 'active'
    ) THEN
        RAISE EXCEPTION
            'workspace_member 移除保护（W2）：用户 % 在 workspace % 仍有 active 的项目成员关系，'
            '不能移除其工作区成员身份（Project Member ⊆ Workspace Member）',
            OLD.user_id, OLD.workspace_id
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_workspace_member_remove_guard_pm',
              HINT = '同一事务内先移除该用户的项目成员（project_member），再移除工作区成员';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: FUNCTION fn_workspace_member_remove_guard_pm(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.fn_workspace_member_remove_guard_pm() IS '工作区成员移除端保护（W2）：removed/DELETE 前同 workspace 不得有该用户的 active project_member（可延迟到 COMMIT）';


--
-- Name: fts_channel_trigger(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fts_channel_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO public.fts_channel (channel_id, token)
    VALUES (
        NEW.id,
        setweight(to_tsvector('jiebacfg', coalesce(NEW.name, '')), 'A') ||
        setweight(to_tsvector('jiebacfg', coalesce(NEW.description, '')), 'B')
    )
    ON CONFLICT (channel_id) DO UPDATE SET
        token = EXCLUDED.token,
        created_at = now();
    RETURN NEW;
END;
$$;


--
-- Name: fts_group_trigger(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fts_group_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    INSERT INTO public.fts_group (group_id, token)
    VALUES (
        NEW.id,
        setweight(to_tsvector('jiebacfg', coalesce(NEW.title, '')), 'A') ||
        setweight(to_tsvector('jiebacfg', coalesce(NEW.introduction, '')), 'B')
    )
    ON CONFLICT (group_id) DO UPDATE SET
        token = EXCLUDED.token,
        created_at = now();
    RETURN NEW;
END;
$$;


--
-- Name: generate_channel_invitation_code(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION generate_channel_invitation_code(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.generate_channel_invitation_code() IS '生成频道邀请码';


--
-- Name: generate_channel_order_no(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION generate_channel_order_no(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.generate_channel_order_no() IS '生成频道订单号';


--
-- Name: get_datacenter_segment_status(smallint, character varying); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION get_datacenter_segment_status(p_dc_id smallint, p_table_name character varying); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.get_datacenter_segment_status(p_dc_id smallint, p_table_name character varying) IS '查看ID段使用状态 - 监控面板使用';


--
-- Name: get_datacenters(boolean); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION get_datacenters(p_active_only boolean); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.get_datacenters(p_active_only boolean) IS '获取机房列表';


--
-- Name: get_or_allocate_id_segment(smallint, character varying, integer, integer); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION get_or_allocate_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer, p_expire_days integer); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.get_or_allocate_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer, p_expire_days integer) IS '获取或分配ID段 - 自动续期 (核心函数)';


--
-- Name: imboy_msg_c2g_fun(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: imboy_user_collect_fun(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.imboy_user_collect_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.attachment SET referer_time = referer_time - 1
      WHERE file_hash256 = any(string_to_array(OLD.attach_file_hash256, ','));
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- user_collect 业务上不会有单独修改哈希的可能性，忽略
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    RETURN NEW;
  END IF;
end;
$$;


--
-- Name: imboy_user_for_fts_fun(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: imboy_user_tag_relation_fun(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: init_all_sequences(smallint); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION init_all_sequences(p_dc_id smallint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.init_all_sequences(p_dc_id smallint) IS '批量初始化所有表的序列 - 部署时一键执行';


--
-- Name: init_sequence_from_segment(character varying, smallint); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION init_sequence_from_segment(p_table_name character varying, p_dc_id smallint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.init_sequence_from_segment(p_table_name character varying, p_dc_id smallint) IS '从ID段初始化序列 - 应用启动时调用';


--
-- Name: reactivate_datacenter(smallint); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION reactivate_datacenter(p_dc_id smallint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.reactivate_datacenter(p_dc_id smallint) IS '重新激活已注销的机房';


--
-- Name: register_datacenter(character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION register_datacenter(p_name character varying, p_region character varying, p_api_endpoint character varying); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.register_datacenter(p_name character varying, p_region character varying, p_api_endpoint character varying) IS '注册新机房 - 返回机房ID';


--
-- Name: renew_id_segment(smallint, character varying, integer); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION renew_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.renew_id_segment(p_dc_id smallint, p_table_name character varying, p_segment_size integer) IS '手动续期ID段 - 紧急情况使用';


--
-- Name: set_updated_at(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.set_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$;


--
-- Name: sync_fts_user(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: unregister_datacenter(smallint); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: FUNCTION unregister_datacenter(p_dc_id smallint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.unregister_datacenter(p_dc_id smallint) IS '注销机房 - 软删除，保留数据';


--
-- Name: update_e2ee_updated_at(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_e2ee_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$;


--
-- Name: workspace_bump_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.workspace_bump_version() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
 NEW.version := OLD.version + 1;
 RETURN NEW;
END $$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: _compressed_hypertable_10; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_10 (
);


--
-- Name: _compressed_hypertable_12; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_12 (
);


--
-- Name: _compressed_hypertable_14; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_14 (
);


--
-- Name: _compressed_hypertable_16; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_16 (
);


--
-- Name: _compressed_hypertable_2; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_2 (
);


--
-- Name: _compressed_hypertable_4; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_4 (
);


--
-- Name: _compressed_hypertable_6; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_6 (
);


--
-- Name: _compressed_hypertable_8; Type: TABLE; Schema: _timescaledb_internal; Owner: -
--

CREATE TABLE _timescaledb_internal._compressed_hypertable_8 (
);


--
-- Name: adm_auth_epoch; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.adm_auth_epoch (
    admin_id bigint NOT NULL,
    epoch bigint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE adm_auth_epoch; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.adm_auth_epoch IS 'Task12/LT-06 admin session epoch; cookie sig epoch < row epoch = revoked; missing row = epoch 1';


--
-- Name: adm_role; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE adm_role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.adm_role IS '运营用户角色表';


--
-- Name: COLUMN adm_role.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.id IS '主键 自增长ID 角色ID';


--
-- Name: COLUMN adm_role.parent_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.parent_id IS '父级角色ID 0 未顶级角色';


--
-- Name: COLUMN adm_role.sort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.sort IS '排序(数字越小越靠前)';


--
-- Name: COLUMN adm_role.role_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.role_name IS '角色名称';


--
-- Name: COLUMN adm_role.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN adm_role.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.updated_at IS '修改记录Unix时间戳毫秒单位';


--
-- Name: COLUMN adm_role.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_role.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: adm_user; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE adm_user; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.adm_user IS '运营用户表';


--
-- Name: COLUMN adm_user.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.id IS '主键 自增长ID';


--
-- Name: COLUMN adm_user.account; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.account IS '会员账号';


--
-- Name: COLUMN adm_user.mobile; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.mobile IS '手机号码';


--
-- Name: COLUMN adm_user.email; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.email IS '会员注册Email';


--
-- Name: COLUMN adm_user.nickname; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.nickname IS '用户昵称';


--
-- Name: COLUMN adm_user.password; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.password IS '经过加盐的密码，由 elib_password:generate/1 生成（HMAC-SHA512）';


--
-- Name: COLUMN adm_user.avatar; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.avatar IS '头像';


--
-- Name: COLUMN adm_user.role_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.role_id IS '角色ID';


--
-- Name: COLUMN adm_user.login_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.login_count IS '登陆次数';


--
-- Name: COLUMN adm_user.last_login_ip; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.last_login_ip IS '最后登陆IP';


--
-- Name: COLUMN adm_user.last_login_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.last_login_at IS '最后登录时间';


--
-- Name: COLUMN adm_user.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN adm_user.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.adm_user.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: admin_operation_logs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.admin_operation_logs (
    id bigint NOT NULL,
    adm_user_id bigint NOT NULL,
    action character varying(64) NOT NULL,
    target_id bigint,
    target_type character varying(32),
    detail jsonb,
    ip character varying(64),
    created_at bigint NOT NULL
);


--
-- Name: TABLE admin_operation_logs; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.admin_operation_logs IS '管理员操作审计日志（等保2.0合规）';


--
-- Name: agent_effect; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_effect (
    id bigint NOT NULL,
    run_id bigint NOT NULL,
    sequence integer NOT NULL,
    tool_id text NOT NULL,
    capability text NOT NULL,
    action text NOT NULL,
    resource_digest text NOT NULL,
    args_digest text NOT NULL,
    status text NOT NULL,
    authorization_reason text,
    approval_ref text,
    grant_version_checked integer,
    external_idempotency_key text,
    result_digest text,
    failure_code text,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_ae_action CHECK ((action <> ''::text)),
    CONSTRAINT ck_ae_args_digest CHECK ((args_digest <> ''::text)),
    CONSTRAINT ck_ae_capability CHECK ((capability <> ''::text)),
    CONSTRAINT ck_ae_resource_digest CHECK ((resource_digest <> ''::text)),
    CONSTRAINT ck_ae_sequence CHECK ((sequence >= 1)),
    CONSTRAINT ck_ae_status CHECK ((status = ANY (ARRAY['created'::text, 'denied'::text, 'waiting_approval'::text, 'authorized'::text, 'dispatching'::text, 'succeeded'::text, 'failed'::text, 'unknown'::text]))),
    CONSTRAINT ck_ae_tool_id CHECK ((tool_id <> ''::text)),
    CONSTRAINT ck_ae_version CHECK ((version >= 1))
);


--
-- Name: TABLE agent_effect; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_effect IS 'AgentRun Effect 账本（§9.4）：dispatch/reconcile ledger，非业务结果真源；不存 credential、完整 Prompt、完整 Tool 参数或敏感结果；dispatching 必须在调用 adapter 前持久化（§10.3）';


--
-- Name: COLUMN agent_effect.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.id IS '主键 TSID';


--
-- Name: COLUMN agent_effect.run_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.run_id IS '所属 Run；RESTRICT';


--
-- Name: COLUMN agent_effect.sequence; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.sequence IS 'Run 内单调序号（>=1），UNIQUE(run_id,sequence)';


--
-- Name: COLUMN agent_effect.tool_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.tool_id IS '版本化 Tool descriptor 的稳定 tool id';


--
-- Name: COLUMN agent_effect.capability; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.capability IS 'Tool descriptor capability';


--
-- Name: COLUMN agent_effect.action; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.action IS 'Tool descriptor action';


--
-- Name: COLUMN agent_effect.resource_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.resource_digest IS '服务端解析的资源摘要（只存 digest）';


--
-- Name: COLUMN agent_effect.args_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.args_digest IS 'Tool 参数摘要（只存 digest）；approval 绑定四元组之一（§10.3）';


--
-- Name: COLUMN agent_effect.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.status IS 'created|denied|waiting_approval|authorized|dispatching|succeeded|failed|unknown（CHECK 冻结）；子状态矩阵见 AG31-04B evidence（AG-A0 裁决 CS-7）';


--
-- Name: COLUMN agent_effect.authorization_reason; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.authorization_reason IS '稳定 reason code（allow/grant_revoked/grant_expired/grant_missing 等）';


--
-- Name: COLUMN agent_effect.approval_ref; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.approval_ref IS '批准凭据引用；批准时绑定 digest/version（§10.3），可空';


--
-- Name: COLUMN agent_effect.grant_version_checked; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.grant_version_checked IS '最后授权检查时的 Grant 版本';


--
-- Name: COLUMN agent_effect.external_idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.external_idempotency_key IS '外部系统幂等 key；UNIQUE(tool_id,external_idempotency_key) 支撑 duplicate effect 不重发（§14）';


--
-- Name: COLUMN agent_effect.result_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.result_digest IS '成功结果摘要（sanitized outcome）';


--
-- Name: COLUMN agent_effect.failure_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.failure_code IS '失败码；dispatch 结果不可知时记 unknown（禁止自动重发，§10.3）';


--
-- Name: COLUMN agent_effect.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_effect.version IS 'CAS 版本（>=1）；Effect 所有状态写入使用 CAS（§9.4）';


--
-- Name: agent_grant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_grant (
    id bigint NOT NULL,
    agent_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    delegator_user_id bigint NOT NULL,
    workspace_scope_kind text NOT NULL,
    status text NOT NULL,
    valid_from timestamp with time zone NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    revoked_at timestamp with time zone,
    revoked_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_ag_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_ag_status_revoked_match CHECK ((((status = 'revoked'::text) = (revoked_at IS NOT NULL)) AND ((revoked_at IS NULL) = (revoked_by_user_id IS NULL)))),
    CONSTRAINT ck_ag_validity CHECK ((expires_at > valid_from)),
    CONSTRAINT ck_ag_version CHECK ((version >= 1)),
    CONSTRAINT ck_ag_workspace_scope_kind CHECK ((workspace_scope_kind = ANY (ARRAY['none'::text, 'explicit'::text])))
);


--
-- Name: TABLE agent_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_grant IS 'Agent Grant（§7.2 冻结合同）：Human delegator 向 Agent 授予的最大可执行边界；DEFAULT=DENY；API 有效态 pending|active|expired|revoked 由时间实时计算，expired 不入库';


--
-- Name: COLUMN agent_grant.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.id IS '主键 TSID';


--
-- Name: COLUMN agent_grant.agent_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.agent_id IS '被授权 Agent 的 user id（运行时要求 account_type=1）';


--
-- Name: COLUMN agent_grant.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.organization_id IS 'Grant 归属机构；与 agent_id/delegator 同 Org（运行时校验）';


--
-- Name: COLUMN agent_grant.delegator_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.delegator_user_id IS '承担授权责任的 Human principal（运行时要求 Human member）';


--
-- Name: COLUMN agent_grant.workspace_scope_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.workspace_scope_kind IS 'none=不限定 workspace（零行 agent_grant_workspace）| explicit=显式 workspace id 列表（至少一行）';


--
-- Name: COLUMN agent_grant.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.status IS '存储态只有 active|revoked；expired 不入库，到期实时判定';


--
-- Name: COLUMN agent_grant.valid_from; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.valid_from IS '生效起始（不晚于 expires_at）';


--
-- Name: COLUMN agent_grant.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.expires_at IS '到期时刻；到期不依赖后台任务即被拒绝';


--
-- Name: COLUMN agent_grant.revoked_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.revoked_at IS '撤销时间；仅 status=revoked 非空（与 revoked_by 同空/同非空）';


--
-- Name: COLUMN agent_grant.revoked_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.revoked_by_user_id IS '撤销人；与 revoked_at 同空/同非空；RESTRICT 保护 lineage';


--
-- Name: COLUMN agent_grant.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.version IS 'CAS 版本（>=1）；所有 mutation 走 expected-version CAS';


--
-- Name: COLUMN agent_grant.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant.idempotency_key IS '幂等键；同 (organization_id, delegator_user_id) 范围唯一';


--
-- Name: agent_grant_capability; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_grant_capability (
    grant_id bigint NOT NULL,
    capability text NOT NULL,
    action text NOT NULL,
    resource_type text NOT NULL,
    constraint_json jsonb DEFAULT '{}'::jsonb NOT NULL,
    CONSTRAINT ck_agc_action CHECK ((action <> ''::text)),
    CONSTRAINT ck_agc_capability CHECK ((capability <> ''::text)),
    CONSTRAINT ck_agc_resource_type CHECK ((resource_type <> ''::text))
);


--
-- Name: TABLE agent_grant_capability; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_grant_capability IS 'Agent Grant capability 边界（§7.2）：capability/action 来自版本化目录；constraint_json 只能收窄资源，未知 key 在发行与执行阶段都由应用层拒绝';


--
-- Name: COLUMN agent_grant_capability.constraint_json; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_capability.constraint_json IS '资源收窄约束 jsonb（不允许否定后再扩大；不存敏感原文）';


--
-- Name: agent_grant_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_grant_event (
    id bigint NOT NULL,
    grant_id bigint NOT NULL,
    event_type text NOT NULL,
    actor_kind text NOT NULL,
    actor_user_id bigint,
    from_version integer,
    to_version integer NOT NULL,
    detail_json jsonb DEFAULT '{}'::jsonb NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_age_actor_kind CHECK ((actor_kind = ANY (ARRAY['human'::text, 'system'::text]))),
    CONSTRAINT ck_age_event_type CHECK ((event_type = ANY (ARRAY['issued'::text, 'revoked'::text, 'expiry_observed'::text])))
);


--
-- Name: TABLE agent_grant_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_grant_event IS 'Agent Grant 事件账本（§7.2，append-only）：Grant create/revoke 与 event 同一事务提交，审计失败则 mutation 回滚；禁止 UPDATE 与 DELETE';


--
-- Name: COLUMN agent_grant_event.grant_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.grant_id IS '所属 Grant；RESTRICT：有 event 血统的 Grant 物理不可删';


--
-- Name: COLUMN agent_grant_event.event_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.event_type IS 'issued | revoked | expiry_observed';


--
-- Name: COLUMN agent_grant_event.actor_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.actor_kind IS 'human | system';


--
-- Name: COLUMN agent_grant_event.actor_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.actor_user_id IS 'Human 操作者快照；user 删除后置 NULL，不级联 Grant lineage';


--
-- Name: COLUMN agent_grant_event.from_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.from_version IS '变更前 CAS 版本（issued 首事件可为 NULL）';


--
-- Name: COLUMN agent_grant_event.to_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.to_version IS '变更后 CAS 版本';


--
-- Name: COLUMN agent_grant_event.detail_json; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.detail_json IS 'sanitized metadata（只存脱敏元数据）';


--
-- Name: COLUMN agent_grant_event.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_event.idempotency_key IS '事件幂等键（全局唯一）';


--
-- Name: agent_grant_workspace; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_grant_workspace (
    organization_id bigint NOT NULL,
    grant_id bigint NOT NULL,
    workspace_id bigint NOT NULL
);


--
-- Name: TABLE agent_grant_workspace; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_grant_workspace IS 'Agent Grant 显式 workspace 范围（§7.2）：workspace_scope_kind=none 必须零行、explicit 必须至少一行；该跨表条件由 Grant command 同一事务内锁定 Grant 验证';


--
-- Name: COLUMN agent_grant_workspace.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_workspace.organization_id IS '机构 id；复合 FK 保证 grant 与 workspace 同 Org';


--
-- Name: COLUMN agent_grant_workspace.grant_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_workspace.grant_id IS '所属 Grant（同 Org 复合 FK 引用 agent_grant(organization_id,id)）';


--
-- Name: COLUMN agent_grant_workspace.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_grant_workspace.workspace_id IS '被授予的 workspace（复合 FK 保证属于同一 Org，RESTRICT）';


--
-- Name: agent_hub_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_hub_audit (
    entity_id text NOT NULL,
    correlation_id text NOT NULL,
    entity_type text NOT NULL,
    parent_entity_id text,
    status text NOT NULL,
    occurred_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT agent_hub_audit_correlation_format_check CHECK ((correlation_id ~ '^[A-Za-z0-9_-]{16,64}$'::text)),
    CONSTRAINT agent_hub_audit_entity_id_format_check CHECK ((entity_id ~ '^[A-Za-z0-9_-]{16,64}$'::text)),
    CONSTRAINT agent_hub_audit_entity_type_check CHECK ((entity_type = ANY (ARRAY['request'::text, 'task'::text, 'event'::text, 'approval'::text, 'execution'::text, 'delivery'::text, 'outcome'::text]))),
    CONSTRAINT agent_hub_audit_parent_shape_check CHECK ((((entity_type = 'request'::text) AND (parent_entity_id IS NULL)) OR ((entity_type <> 'request'::text) AND (parent_entity_id IS NOT NULL)))),
    CONSTRAINT agent_hub_audit_status_format_check CHECK ((status ~ '^[a-z0-9_]{1,32}$'::text))
);


--
-- Name: TABLE agent_hub_audit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_hub_audit IS 'TRACE-00 runtime audit source; chain metadata only, never payload, secret, URL, or PII';


--
-- Name: agent_payment_compensation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_payment_compensation (
    id bigint NOT NULL,
    mandate_id bigint NOT NULL,
    amount_fen bigint NOT NULL,
    reference_no character varying(128) NOT NULL,
    status character varying(16) DEFAULT 'settling'::character varying NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    next_attempt_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    lease_until timestamp with time zone,
    last_error text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    settled_at timestamp with time zone,
    released_at timestamp with time zone,
    CONSTRAINT chk_agent_payment_compensation_amount CHECK ((amount_fen > 0)),
    CONSTRAINT chk_agent_payment_compensation_status CHECK (((status)::text = ANY ((ARRAY['settling'::character varying, 'pending'::character varying, 'processing'::character varying, 'settled'::character varying, 'released'::character varying])::text[])))
);


--
-- Name: TABLE agent_payment_compensation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_payment_compensation IS 'Agent payment quota reservation compensation outbox';


--
-- Name: agent_payment_mandate; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_payment_mandate (
    id bigint NOT NULL,
    owner_uid bigint NOT NULL,
    agent_uid bigint NOT NULL,
    max_amount_fen bigint NOT NULL,
    max_total_fen bigint NOT NULL,
    period_secs integer DEFAULT 86400 NOT NULL,
    spent_fen bigint DEFAULT 0 NOT NULL,
    window_start timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE agent_payment_mandate; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_payment_mandate IS 'Agent 受控支付授权（单笔/周期上限、到期、付款人=owner_uid）';


--
-- Name: agent_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_run (
    id bigint NOT NULL,
    agent_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint,
    grant_id bigint NOT NULL,
    grant_version_at_start integer NOT NULL,
    delegating_principal_id bigint NOT NULL,
    trigger_type text NOT NULL,
    trigger_id text NOT NULL,
    runtime_type text NOT NULL,
    status text NOT NULL,
    reason_code text,
    version integer DEFAULT 1 NOT NULL,
    context_digest text NOT NULL,
    idempotency_key text NOT NULL,
    lease_owner text,
    lease_expires_at timestamp with time zone,
    attempt integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    queued_at timestamp with time zone,
    started_at timestamp with time zone,
    finished_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_ar_attempt CHECK ((attempt >= 0)),
    CONSTRAINT ck_ar_context_digest CHECK ((context_digest <> ''::text)),
    CONSTRAINT ck_ar_idempotency_key CHECK ((idempotency_key <> ''::text)),
    CONSTRAINT ck_ar_status CHECK ((status = ANY (ARRAY['created'::text, 'queued'::text, 'running'::text, 'waiting_approval'::text, 'succeeded'::text, 'failed'::text, 'cancelled'::text, 'unknown'::text]))),
    CONSTRAINT ck_ar_terminal_finished CHECK (((status <> ALL (ARRAY['succeeded'::text, 'failed'::text, 'cancelled'::text])) OR (finished_at IS NOT NULL))),
    CONSTRAINT ck_ar_trigger_id CHECK ((trigger_id <> ''::text)),
    CONSTRAINT ck_ar_trigger_type CHECK ((trigger_type = ANY (ARRAY['message'::text, 'schedule'::text, 'webhook'::text]))),
    CONSTRAINT ck_ar_version CHECK ((version >= 1))
);


--
-- Name: TABLE agent_run; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_run IS 'AgentRun（§9.1/§9.4 冻结合同）：一次 Runtime 执行实例；与 ProductTask(agent_task)/MCP Task 三者可关联但绝不共表、改名或共享状态机；status 冻结 §9.3 八状态，timeout 非状态（failed + reason_code=timeout）';


--
-- Name: COLUMN agent_run.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.id IS '主键 TSID';


--
-- Name: COLUMN agent_run.agent_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.agent_id IS '执行 Agent 的 user id（account_type=1）；RESTRICT 保护血统';


--
-- Name: COLUMN agent_run.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.organization_id IS 'Run 归属机构；immutable context 成员，写入后不得随默认值变化';


--
-- Name: COLUMN agent_run.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.workspace_id IS '可空；非空时与 organization_id 复合 FK 指向同 Org workspace（MATCH SIMPLE：NULL 不检查）；默认 Workspace 只在创建 Run 时解析候选值';


--
-- Name: COLUMN agent_run.grant_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.grant_id IS 'Run 依据的 Grant；RESTRICT；运行期授权以 Grant 实时状态重检，不凭此快照放行';


--
-- Name: COLUMN agent_run.grant_version_at_start; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.grant_version_at_start IS 'Grant 版本审计快照；不用于绕过实时重检（§9.2）';


--
-- Name: COLUMN agent_run.delegating_principal_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.delegating_principal_id IS 'delegating principal（§8）；RESTRICT 保护血统';


--
-- Name: COLUMN agent_run.trigger_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.trigger_type IS 'message | schedule | webhook（CHECK 冻结）';


--
-- Name: COLUMN agent_run.trigger_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.trigger_id IS '服务端来源标识（非空）';


--
-- Name: COLUMN agent_run.runtime_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.runtime_type IS 'Runtime 类型 text；首版 mock | hird';


--
-- Name: COLUMN agent_run.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.status IS 'FSM 存储态：created|queued|running|waiting_approval|succeeded|failed|cancelled|unknown；unknown 仅显式 reconcile 转 succeeded|failed';


--
-- Name: COLUMN agent_run.reason_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.reason_code IS '稳定原因码（如 timeout/validation_failed/grant_revoked/max_attempts_exceeded）；可空';


--
-- Name: COLUMN agent_run.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.version IS 'CAS 版本（>=1）；每次迁移 UPDATE ... WHERE id AND status AND version（§9.3）';


--
-- Name: COLUMN agent_run.context_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.context_digest IS 'immutable context 摘要（非空；不存 Prompt 原文）';


--
-- Name: COLUMN agent_run.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.idempotency_key IS 'trigger 幂等键（非空）；五元组 UNIQUE 支撑 duplicate trigger 返回同一 Run';


--
-- Name: COLUMN agent_run.lease_owner; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.lease_owner IS 'worker lease 持有者；节点重启后仅一个 worker 能以 DB 条件更新抢到过期 lease';


--
-- Name: COLUMN agent_run.lease_expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.lease_expires_at IS 'lease 到期时刻；获取/续租均为 DB 条件更新，进程锁/ETS/全局注册不作真源（§9.3）';


--
-- Name: COLUMN agent_run.attempt; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.attempt IS 'lease 获取次数（>=0）；上限为应用层冻结常量（AG-A0 裁决 CS-6）';


--
-- Name: COLUMN agent_run.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.created_at IS 'Run 创建时刻';


--
-- Name: COLUMN agent_run.queued_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.queued_at IS '入队时刻（created→queued 边）';


--
-- Name: COLUMN agent_run.started_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.started_at IS '起跑时刻（queued→running 边）';


--
-- Name: COLUMN agent_run.finished_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.finished_at IS '终态时刻；终态（succeeded|failed|cancelled）必须非空（CHECK）';


--
-- Name: COLUMN agent_run.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run.updated_at IS '最近变更时刻';


--
-- Name: agent_run_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_run_event (
    id bigint NOT NULL,
    run_id bigint NOT NULL,
    from_status text,
    to_status text NOT NULL,
    reason_code text,
    actor_kind text NOT NULL,
    actor_id text NOT NULL,
    detail_json jsonb DEFAULT '{}'::jsonb NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_are_actor_kind CHECK ((actor_kind = ANY (ARRAY['human'::text, 'system'::text, 'agent'::text]))),
    CONSTRAINT ck_are_from_status CHECK (((from_status IS NULL) OR (from_status = ANY (ARRAY['created'::text, 'queued'::text, 'running'::text, 'waiting_approval'::text, 'succeeded'::text, 'failed'::text, 'cancelled'::text, 'unknown'::text])))),
    CONSTRAINT ck_are_to_status CHECK ((to_status = ANY (ARRAY['created'::text, 'queued'::text, 'running'::text, 'waiting_approval'::text, 'succeeded'::text, 'failed'::text, 'cancelled'::text, 'unknown'::text])))
);


--
-- Name: TABLE agent_run_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.agent_run_event IS 'AgentRun 事件账本（§9.4，append-only）：每次 FSM 迁移与 event 同一事务提交，审计失败则迁移回滚；禁止 UPDATE 与 DELETE';


--
-- Name: COLUMN agent_run_event.run_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.run_id IS '所属 Run；RESTRICT：有 event 血统的 Run 物理不可删';


--
-- Name: COLUMN agent_run_event.from_status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.from_status IS '迁移前状态；创建事件（[*]→created，非态到态边）为 NULL';


--
-- Name: COLUMN agent_run_event.to_status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.to_status IS '迁移后状态（八状态 CHECK）';


--
-- Name: COLUMN agent_run_event.reason_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.reason_code IS '迁移稳定原因码；可空';


--
-- Name: COLUMN agent_run_event.actor_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.actor_kind IS 'human | system | agent（AG-A0 裁决 CS-4 冻结值域）';


--
-- Name: COLUMN agent_run_event.actor_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.actor_id IS '执行者标识：human=user id 十进制文本、system=worker/node 标识、agent=agent user id 十进制文本';


--
-- Name: COLUMN agent_run_event.detail_json; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.detail_json IS 'sanitized metadata（只存脱敏元数据，§9.4）';


--
-- Name: COLUMN agent_run_event.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.agent_run_event.idempotency_key IS '事件幂等键；同 Run 内唯一（UNIQUE(run_id,idempotency_key)）';


--
-- Name: agent_task; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_task (
    id text NOT NULL,
    group_id bigint NOT NULL,
    agent_uid bigint NOT NULL,
    tool text DEFAULT ''::text NOT NULL,
    params_digest text DEFAULT ''::text NOT NULL,
    result_digest text DEFAULT ''::text NOT NULL,
    status text DEFAULT 'submitted'::text NOT NULL,
    correlation_id text NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT agent_task_status_check CHECK ((status = ANY (ARRAY['submitted'::text, 'working'::text, 'awaiting_approval'::text, 'approved'::text, 'rejected'::text, 'expired'::text, 'completed'::text, 'failed'::text, 'cancelled'::text])))
);


--
-- Name: agent_task_decision; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_task_decision (
    id text NOT NULL,
    task_id text NOT NULL,
    decision text NOT NULL,
    approver_uid bigint NOT NULL,
    correlation_id text NOT NULL,
    decided_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT agent_task_decision_check CHECK ((decision = ANY (ARRAY['approved'::text, 'rejected'::text])))
);


--
-- Name: agent_task_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.agent_task_event (
    id text NOT NULL,
    task_id text NOT NULL,
    status text NOT NULL,
    seq bigint DEFAULT 0 NOT NULL,
    correlation_id text NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ai_agent; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ai_agent (
    user_id bigint NOT NULL,
    provider character varying(40) NOT NULL,
    model character varying(80) DEFAULT ''::character varying NOT NULL,
    role_id character varying(40) DEFAULT ''::character varying NOT NULL,
    system_prompt text DEFAULT ''::text NOT NULL,
    owner_uid bigint DEFAULT 0 NOT NULL,
    trigger_policy jsonb DEFAULT '{}'::jsonb NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    visibility smallint DEFAULT 0 NOT NULL,
    category character varying(40) DEFAULT ''::character varying NOT NULL,
    voice_id character varying(40) DEFAULT ''::character varying NOT NULL,
    greeting text DEFAULT ''::text NOT NULL,
    capabilities jsonb DEFAULT '{}'::jsonb NOT NULL,
    temperature real DEFAULT 0.7 NOT NULL
);


--
-- Name: TABLE ai_agent; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.ai_agent IS 'AI Agent 元数据（provider/role/owner/群触发策略）';


--
-- Name: COLUMN ai_agent.description; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.description IS 'agent 专属简介（发现卡片用，独立于 user.sign）';


--
-- Name: COLUMN ai_agent.visibility; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.visibility IS '可发现性 0=私有（默认）1=公开可发现';


--
-- Name: COLUMN ai_agent.category; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.category IS '分类标签（客服/办公/娱乐等）';


--
-- Name: COLUMN ai_agent.voice_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.voice_id IS '语音音色标识（预留 TTS 接线位）';


--
-- Name: COLUMN ai_agent.greeting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.greeting IS '开场白（冷启动欢迎语）';


--
-- Name: COLUMN ai_agent.capabilities; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.capabilities IS '能力开关 {knowledge, group_reply, proactive}';


--
-- Name: COLUMN ai_agent.temperature; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent.temperature IS 'LLM 采样温度 0~1（默认 0.7）';


--
-- Name: ai_agent_role; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ai_agent_role (
    code character varying(40) NOT NULL,
    name character varying(80) NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    active_version integer DEFAULT 0 NOT NULL,
    created_by bigint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ai_agent_role_active_version_check CHECK ((active_version >= 0)),
    CONSTRAINT ai_agent_role_status_check CHECK ((status = ANY (ARRAY[0, 1])))
);


--
-- Name: TABLE ai_agent_role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.ai_agent_role IS 'AI Agent 可复用角色模板';


--
-- Name: COLUMN ai_agent_role.code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent_role.code IS '稳定的业务角色编码，绑定助手时使用';


--
-- Name: COLUMN ai_agent_role.active_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent_role.active_version IS '当前生效的已发布版本号，0 表示尚未发布';


--
-- Name: ai_agent_role_version; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ai_agent_role_version (
    id bigint NOT NULL,
    role_code character varying(40) NOT NULL,
    version integer NOT NULL,
    state character varying(16) DEFAULT 'draft'::character varying NOT NULL,
    system_prompt text NOT NULL,
    capabilities jsonb DEFAULT '{}'::jsonb NOT NULL,
    knowledge_policy jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_by bigint DEFAULT 0 NOT NULL,
    published_by bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    published_at timestamp with time zone,
    CONSTRAINT ai_agent_role_version_state_check CHECK (((state)::text = ANY ((ARRAY['draft'::character varying, 'published'::character varying, 'archived'::character varying])::text[]))),
    CONSTRAINT ai_agent_role_version_version_check CHECK ((version > 0))
);


--
-- Name: TABLE ai_agent_role_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.ai_agent_role_version IS 'AI Agent 角色配置版本，发布后供运行时继承';


--
-- Name: COLUMN ai_agent_role_version.capabilities; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent_role_version.capabilities IS '固定能力策略，不允许助手实例绕过角色授权';


--
-- Name: COLUMN ai_agent_role_version.knowledge_policy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.ai_agent_role_version.knowledge_policy IS '知识库按需检索策略，避免每轮注入完整知识库';


--
-- Name: announcement; Type: TABLE; Schema: public; Owner: -
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
    CONSTRAINT chk_announcement_type CHECK (((type)::text = ANY (ARRAY[('info'::character varying)::text, ('warning'::character varying)::text, ('important'::character varying)::text])))
);


--
-- Name: TABLE announcement; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.announcement IS '全局公告表';


--
-- Name: COLUMN announcement.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.id IS '主键ID';


--
-- Name: COLUMN announcement.adm_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.adm_user_id IS '创建者管理员ID';


--
-- Name: COLUMN announcement.title; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.title IS '公告标题';


--
-- Name: COLUMN announcement.body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.body IS '公告内容';


--
-- Name: COLUMN announcement.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.type IS '公告类型: info/warning/important';


--
-- Name: COLUMN announcement.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.status IS '状态: -1 已删除, 0 草稿, 1 已发布, 2 已撤回';


--
-- Name: COLUMN announcement.pinned; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.pinned IS '是否置顶: 0 否, 1 是';


--
-- Name: COLUMN announcement.published_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.published_at IS '发布时间';


--
-- Name: COLUMN announcement.expired_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.expired_at IS '过期时间';


--
-- Name: COLUMN announcement.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.updated_at IS '更新时间';


--
-- Name: COLUMN announcement.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.announcement.created_at IS '创建时间';


--
-- Name: app_ddl; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE app_ddl; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.app_ddl IS 'APP sqlite3 数据库 DDL 语句版本管理表';


--
-- Name: COLUMN app_ddl.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.id IS '主键 自增长ID 反馈ID';


--
-- Name: COLUMN app_ddl.ddl; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.ddl IS '需要更新的DDL语句文本，每个SQL半角逗号 ; 分割，因为DDL语句可能有顺序要求，所以用 text类型，而不用 json 数据类型';


--
-- Name: COLUMN app_ddl.old_vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.old_vsn IS '版本号： 整形数字';


--
-- Name: COLUMN app_ddl.new_vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.new_vsn IS '版本号： 整形数字';


--
-- Name: COLUMN app_ddl.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN app_ddl.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN app_ddl.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_ddl.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: app_upgrade_log; Type: TABLE; Schema: public; Owner: -
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
    CONSTRAINT chk_app_upgrade_log_cos CHECK (((cos)::text = ANY (ARRAY[('android'::character varying)::text, ('ios'::character varying)::text, ('web'::character varying)::text, ('macos'::character varying)::text]))),
    CONSTRAINT chk_app_upgrade_log_event CHECK (((event)::text = ANY (ARRAY[('check'::character varying)::text, ('prompted'::character varying)::text, ('download_start'::character varying)::text, ('download_done'::character varying)::text, ('verify_ok'::character varying)::text, ('verify_fail'::character varying)::text, ('install'::character varying)::text, ('cancel'::character varying)::text, ('error'::character varying)::text]))),
    CONSTRAINT chk_app_upgrade_log_upgrade_type CHECK (((upgrade_type)::text = ANY (ARRAY[('force'::character varying)::text, ('recommend'::character varying)::text, ('silent'::character varying)::text, ('none'::character varying)::text])))
);


--
-- Name: app_version; Type: TABLE; Schema: public; Owner: -
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
    CONSTRAINT chk_app_version_upgrade_type CHECK (((upgrade_type)::text = ANY (ARRAY[('force'::character varying)::text, ('recommend'::character varying)::text, ('silent'::character varying)::text])))
);


--
-- Name: TABLE app_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.app_version IS 'APP版本管理表';


--
-- Name: COLUMN app_version.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.id IS '主键 自增长ID 反馈ID';


--
-- Name: COLUMN app_version.region_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.region_code IS 'The two-letter country code cn en  参考 https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2';


--
-- Name: COLUMN app_version.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.type IS 'APP类型 web ios android macos windows';


--
-- Name: COLUMN app_version.package_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.package_name IS '包名 | Bundle ID';


--
-- Name: COLUMN app_version.app_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.app_name IS 'APP名称';


--
-- Name: COLUMN app_version.vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.vsn IS '版本号： x.y.z格式';


--
-- Name: COLUMN app_version.sign_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.sign_key IS 'APP签名密码';


--
-- Name: COLUMN app_version.download_url; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.download_url IS '下载地址';


--
-- Name: COLUMN app_version.description; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.description IS '描述';


--
-- Name: COLUMN app_version.force_update; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.force_update IS '是否强制升级 1 是  2 否';


--
-- Name: COLUMN app_version.sort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.sort IS '排序，值越大越靠前： major * 1_000_000 + minor * 1_000 + patch';


--
-- Name: COLUMN app_version.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN app_version.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN app_version.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN app_version.min_supported_vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.min_supported_vsn IS '最低支持版本：低于此版本的客户端必须升级';


--
-- Name: COLUMN app_version.grayscale_percent; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.grayscale_percent IS '灰度发布比例：0-100，100 表示全量发布';


--
-- Name: COLUMN app_version.upgrade_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.upgrade_type IS '升级类型：force=强制 / recommend=推荐 / silent=静默';


--
-- Name: COLUMN app_version.changelog; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.changelog IS '结构化更新日志 JSON 数组';


--
-- Name: COLUMN app_version.file_size; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.file_size IS '安装包文件大小（字节）';


--
-- Name: COLUMN app_version.file_hash; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version.file_hash IS '安装包 SHA256 校验值';


--
-- Name: app_version_policy; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE app_version_policy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.app_version_policy IS 'APP版本策略配置表（每个平台一条记录）';


--
-- Name: COLUMN app_version_policy.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.id IS '主键 TSID';


--
-- Name: COLUMN app_version_policy.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.type IS '平台类型：android / ios / web';


--
-- Name: COLUMN app_version_policy.min_vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.min_vsn IS '全局最低支持版本（兜底安全网）';


--
-- Name: COLUMN app_version_policy.grayscale_enabled; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.grayscale_enabled IS '是否启用灰度：0=关闭直接全量 1=启用';


--
-- Name: COLUMN app_version_policy.check_interval_hours; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.check_interval_hours IS '客户端版本检查间隔（小时）';


--
-- Name: COLUMN app_version_policy.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.status IS '状态：0=禁用 1=启用';


--
-- Name: COLUMN app_version_policy.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.updated_at IS '最后更新时间';


--
-- Name: COLUMN app_version_policy.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.app_version_policy.created_at IS '创建时间';


--
-- Name: attach_pending; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attach_pending (
    object_key text NOT NULL,
    bucket text DEFAULT ''::text NOT NULL,
    scope text DEFAULT ''::text NOT NULL,
    creator_user_id bigint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE attach_pending; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.attach_pending IS '已签发 presigned PUT 但尚未 confirm 的对象登记表；超龄行由 attach_cleanup_logic 连同 S3 对象一并回收';


--
-- Name: attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attachment (
    id bigint NOT NULL,
    file_hash256 character varying(64) DEFAULT ''::character varying CONSTRAINT attachment_md5_not_null NOT NULL,
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
    scope character varying(16) DEFAULT 'private'::character varying NOT NULL,
    scope_ref text,
    cipher character varying(32) DEFAULT NULL::character varying,
    legacy_key text,
    anchor_msg_id text,
    anchor_conv_seq bigint,
    group_file_id bigint,
    CONSTRAINT chk_attachment_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1]))),
    CONSTRAINT ck_attachment_anchor_conv_seq CHECK (((anchor_conv_seq IS NULL) OR (anchor_conv_seq >= 1))),
    CONSTRAINT ck_attachment_group_anchor_kind CHECK (((group_file_id IS NULL) OR ((anchor_msg_id IS NULL) AND (anchor_conv_seq IS NULL))))
);


--
-- Name: TABLE attachment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.attachment IS '文件附件表';


--
-- Name: COLUMN attachment.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.id IS '主键 自增长ID';


--
-- Name: COLUMN attachment.file_hash256; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.file_hash256 IS '附件文件哈希（SHA-256 hex）。cipher IS NULL 时为明文哈希（历史行可能是旧 MD5）；cipher 非 NULL 时为**密文**哈希——明文哈希只存在于客户端加密的 attachment_descriptor 内。仅作完整性参考，不作安全边界。';


--
-- Name: COLUMN attachment.mime_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.mime_type IS '附件mime_type';


--
-- Name: COLUMN attachment.ext; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.ext IS '附件 扩展名';


--
-- Name: COLUMN attachment.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.name IS '附件 名称';


--
-- Name: COLUMN attachment.path; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.path IS '附件 path';


--
-- Name: COLUMN attachment.url; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.url IS '附件 访问地址';


--
-- Name: COLUMN attachment.size; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.size IS '对象字节数（服务端 HEAD 核实的真实值）。cipher 非 NULL 时为**密文**大小，明文大小只在 attachment_descriptor 内。';


--
-- Name: COLUMN attachment.info; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.info IS '附件信息json';


--
-- Name: COLUMN attachment.referer_time; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.referer_time IS '被引用次数';


--
-- Name: COLUMN attachment.last_referer_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.last_referer_user_id IS '最后引用用户';


--
-- Name: COLUMN attachment.last_referer_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.last_referer_at IS '最后引用时间';


--
-- Name: COLUMN attachment.creator_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.creator_user_id IS '创建人用户ID';


--
-- Name: COLUMN attachment.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN attachment.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN attachment.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN attachment.scope; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.scope IS '读鉴权范围: public|private|c2c|group|channel|moment';


--
-- Name: COLUMN attachment.scope_ref; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.scope_ref IS 'scope 绑定实体: group→group_id, c2c→conv_key, channel→channel_id, moment→moment_id; public/private 为 NULL';


--
-- Name: COLUMN attachment.cipher; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.cipher IS '附件内容加密套件；NULL=明文对象（历史行与未加密上传）。非 NULL 时该行是 E2EE 附件，file_hash256 与 size 均指**密文**，服务端不持有 content key，也拿不到明文哈希。';


--
-- Name: COLUMN attachment.legacy_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.legacy_key IS '存量明文附件迁移后的加密密钥（经 postgre_aes_key 加密的 base64）。
     NULL = 未迁移的明文附件，或原生 E2EE 附件（服务端不持有 key）。
     非 NULL 时客户端下载后需用此 key 解密。';


--
-- Name: COLUMN attachment.anchor_msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.anchor_msg_id IS 'Client-generated C2G message id declared at confirm time; immutable after insert';


--
-- Name: COLUMN attachment.anchor_conv_seq; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.anchor_conv_seq IS 'Authoritative C2G conv_seq bound in the same transaction as message staging';


--
-- Name: COLUMN attachment.group_file_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.attachment.group_file_id IS 'Independent group_file primary key; current active members may access it outside chat-history boundaries';


--
-- Name: billing_invoice; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.billing_invoice (
    id bigint NOT NULL,
    invoice_no character varying(64) NOT NULL,
    subscription_id bigint NOT NULL,
    amount bigint DEFAULT 0 NOT NULL,
    currency character varying(8) DEFAULT 'CNY'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    payment_no character varying(128),
    period_start timestamp with time zone,
    period_end timestamp with time zone,
    paid_at timestamp with time zone,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_billing_invoice_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_billing_invoice_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--
-- Name: TABLE billing_invoice; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.billing_invoice IS 'SaaS 账单（金额单位：分，复用统一支付 biz_type=3）';


--
-- Name: billing_plan; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.billing_plan (
    id bigint NOT NULL,
    code character varying(64) NOT NULL,
    name character varying(128) NOT NULL,
    price bigint DEFAULT 0 NOT NULL,
    billing_period character varying(16) DEFAULT 'month'::character varying NOT NULL,
    quota_config jsonb DEFAULT '{}'::jsonb NOT NULL,
    description character varying(512),
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_billing_plan_period CHECK (((billing_period)::text = ANY (ARRAY['month'::text, 'year'::text]))),
    CONSTRAINT chk_billing_plan_price CHECK ((price >= 0)),
    CONSTRAINT chk_billing_plan_status CHECK ((status = ANY (ARRAY[0, 1])))
);


--
-- Name: TABLE billing_plan; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.billing_plan IS 'SaaS 套餐定义（价格单位：分，quota_config 存各项配额上限）';


--
-- Name: billing_subscription; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.billing_subscription (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT 0 NOT NULL,
    plan_id bigint NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    current_period_start timestamp with time zone,
    current_period_end timestamp with time zone,
    auto_renew boolean DEFAULT true NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    owner_uid bigint DEFAULT 0 NOT NULL,
    CONSTRAINT chk_billing_sub_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3])))
);


--
-- Name: TABLE billing_subscription; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.billing_subscription IS 'SaaS 租户订阅（tenant_id 逻辑字段，单租户=0）';


--
-- Name: COLUMN billing_subscription.owner_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.billing_subscription.owner_uid IS '订阅归属用户 id；0=历史无主订阅，用户端拒绝操作，仅管理端可处理';


--
-- Name: billing_usage; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.billing_usage (
    id bigint NOT NULL,
    subscription_id bigint NOT NULL,
    metric character varying(64) NOT NULL,
    used bigint DEFAULT 0 NOT NULL,
    period character varying(32) NOT NULL,
    recorded_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    CONSTRAINT chk_billing_usage_used CHECK ((used >= 0))
);


--
-- Name: TABLE billing_usage; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.billing_usage IS 'SaaS 用量记录（按订阅/指标/周期累计）';


--
-- Name: bot; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bot (
    user_id bigint NOT NULL,
    name character varying(80) NOT NULL,
    username character varying(80),
    description character varying(500) DEFAULT ''::character varying,
    avatar character varying(320) DEFAULT ''::character varying,
    owner_uid bigint NOT NULL,
    webhook_url text DEFAULT ''::text,
    api_token character varying(128),
    verify_token character varying(128) DEFAULT ''::character varying,
    commands jsonb DEFAULT '[]'::jsonb NOT NULL,
    permissions jsonb DEFAULT '[]'::jsonb NOT NULL,
    events jsonb DEFAULT '[]'::jsonb NOT NULL,
    is_public boolean DEFAULT false,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    api_token_digest text DEFAULT ''::text NOT NULL,
    api_token_prefix text DEFAULT ''::text NOT NULL,
    verify_token_enc text DEFAULT ''::text NOT NULL,
    token_migrated boolean DEFAULT false NOT NULL,
    ewh_endpoint_generation integer DEFAULT 0 NOT NULL
);


--
-- Name: TABLE bot; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.bot IS '开发者 Bot（Webhook 驱动的第三方服务，account_type=3）';


--
-- Name: COLUMN bot.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot.user_id IS '关联 user.id，Bot 作为一等 user 账号存在（account_type=3）';


--
-- Name: COLUMN bot.username; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot.username IS 'Bot 唯一调用名，用于 @botname 提及';


--
-- Name: COLUMN bot.api_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot.api_token IS 'Bot 调用 IMBoy API 的凭证';


--
-- Name: COLUMN bot.verify_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot.verify_token IS 'Webhook 推送时的验签 token（与 api_token 职责分离）';


--
-- Name: COLUMN bot.ewh_endpoint_generation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot.ewh_endpoint_generation IS '企业 Webhook endpoint 配置代际（每次配置写入 +1；bot 域行保持 0）';


--
-- Name: bot_delivery; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bot_delivery (
    delivery_id text NOT NULL,
    bot_id text NOT NULL,
    event_type text DEFAULT 'message'::text NOT NULL,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    reply_context text DEFAULT ''::text NOT NULL,
    correlation_id text NOT NULL,
    idempotency_key text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempt_count integer DEFAULT 0 NOT NULL,
    next_retry_at timestamp with time zone DEFAULT now() NOT NULL,
    webhook_host text DEFAULT ''::text NOT NULL,
    pinned_ip text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    webhook_url text DEFAULT ''::text NOT NULL,
    ewh_owner_organization_id bigint,
    ewh_owner_application_id bigint,
    ewh_replay_of text,
    ewh_endpoint_generation integer DEFAULT 0 NOT NULL,
    ewh_ledger_version integer DEFAULT 1 NOT NULL,
    ewh_claimed_at timestamp with time zone,
    CONSTRAINT bot_delivery_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'retry'::text, 'success'::text, 'dead'::text]))),
    CONSTRAINT ck_ewh_delivery_endpoint_generation CHECK ((ewh_endpoint_generation >= 0)),
    CONSTRAINT ck_ewh_delivery_ledger_version CHECK ((ewh_ledger_version >= 1)),
    CONSTRAINT ck_ewh_delivery_owner_pair CHECK (((ewh_owner_organization_id IS NULL) = (ewh_owner_application_id IS NULL))),
    CONSTRAINT ck_ewh_delivery_replay_not_self CHECK (((ewh_replay_of IS NULL) OR (ewh_replay_of <> delivery_id)))
);


--
-- Name: COLUMN bot_delivery.ewh_owner_organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot_delivery.ewh_owner_organization_id IS '企业投递归属机构（eapp 行非空；bot 域行 NULL）；复合 FK 到 enterprise_application';


--
-- Name: COLUMN bot_delivery.ewh_owner_application_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot_delivery.ewh_owner_application_id IS '企业投递归属 Application（eapp 行非空；bot 域行 NULL）；与 org 列成对';


--
-- Name: COLUMN bot_delivery.ewh_replay_of; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot_delivery.ewh_replay_of IS '重放来源：原 delivery_id（非重放行 NULL）；在途重放唯一索引的键';


--
-- Name: COLUMN bot_delivery.ewh_endpoint_generation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot_delivery.ewh_endpoint_generation IS '入箱时快照的 bot.ewh_endpoint_generation（端点配置代际；只读审计，不可改）';


--
-- Name: COLUMN bot_delivery.ewh_ledger_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot_delivery.ewh_ledger_version IS '账本版本（守卫独占写入：INSERT=1，UPDATE=OLD+1；客户端传值被覆盖）';


--
-- Name: COLUMN bot_delivery.ewh_claimed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.bot_delivery.ewh_claimed_at IS '最近一次 worker 认领时刻（租约见 next_retry_at；bot 域行保持 NULL）';


--
-- Name: bot_delivery_attempt; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bot_delivery_attempt (
    id text NOT NULL,
    delivery_id text NOT NULL,
    attempt_no integer NOT NULL,
    status_class text DEFAULT ''::text NOT NULL,
    http_status integer,
    latency_ms integer,
    error_trunc text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: bot_oauth_grant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.bot_oauth_grant (
    id bigint NOT NULL,
    bot_id bigint NOT NULL,
    user_id bigint NOT NULL,
    scopes jsonb DEFAULT '[]'::jsonb NOT NULL,
    access_token character varying(128),
    expires_at timestamp with time zone,
    revoked_at timestamp with time zone,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now()
);


--
-- Name: TABLE bot_oauth_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.bot_oauth_grant IS 'Bot OAuth 授权：用户授权 Bot 代表自己操作';


--
-- Name: calligraphy_review_draft; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.calligraphy_review_draft (
    id bigint NOT NULL,
    submission_id bigint NOT NULL,
    ai_task_id character varying(64),
    status text DEFAULT 'queued'::text NOT NULL,
    model_profile character varying(100) DEFAULT ''::character varying NOT NULL,
    prompt_version character varying(50) DEFAULT ''::character varying NOT NULL,
    rubric_version character varying(50) DEFAULT ''::character varying NOT NULL,
    input_digest character varying(128) DEFAULT ''::character varying NOT NULL,
    result_json jsonb,
    error_code character varying(50),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    completed_at timestamp with time zone,
    CONSTRAINT ck_crd_completed CHECK (((status <> ALL (ARRAY['succeeded'::text, 'failed'::text])) OR (completed_at IS NOT NULL))),
    CONSTRAINT ck_crd_status CHECK ((status = ANY (ARRAY['queued'::text, 'running'::text, 'succeeded'::text, 'failed'::text])))
);


--
-- Name: TABLE calligraphy_review_draft; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.calligraphy_review_draft IS 'AI 点评草稿（只生成老师草稿，D-10；家长 API 永不返回本表）';


--
-- Name: COLUMN calligraphy_review_draft.ai_task_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.calligraphy_review_draft.ai_task_id IS '异步任务标识（可空）';


--
-- Name: COLUMN calligraphy_review_draft.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.calligraphy_review_draft.status IS '状态: queued | running | succeeded | failed（failed 仍进老师人工队列）';


--
-- Name: COLUMN calligraphy_review_draft.result_json; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.calligraphy_review_draft.result_json IS '结构化结果 jsonb（正向观察/主要问题/证据时间点/练习动作/口播提纲/需人工核对标记）';


--
-- Name: COLUMN calligraphy_review_draft.completed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.calligraphy_review_draft.completed_at IS '完成时间（succeeded/failed 必填，CHECK 强制）';


--
-- Name: channel; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel (
    id bigint NOT NULL,
    name character varying(200) NOT NULL,
    description character varying(2000) DEFAULT ''::character varying NOT NULL,
    avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    custom_id character varying(100) DEFAULT NULL::character varying,
    creator_uid bigint NOT NULL,
    subscriber_count bigint DEFAULT 0 NOT NULL,
    is_verified boolean DEFAULT false NOT NULL,
    tags jsonb DEFAULT '[]'::jsonb NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    category_id bigint,
    is_featured boolean DEFAULT false,
    featured_at timestamp with time zone,
    visibility smallint DEFAULT 0 NOT NULL,
    access_type smallint DEFAULT 0 NOT NULL,
    join_policy smallint DEFAULT 0 NOT NULL,
    scope text DEFAULT 'personal'::text NOT NULL,
    workspace_id bigint,
    version bigint DEFAULT 1 NOT NULL,
    CONSTRAINT channel_access_type_check CHECK ((access_type = ANY (ARRAY[0, 1]))),
    CONSTRAINT channel_join_policy_check CHECK ((join_policy = ANY (ARRAY[0, 1, 2, 3]))),
    CONSTRAINT channel_version_check CHECK ((version > 0)),
    CONSTRAINT channel_visibility_check CHECK ((visibility = ANY (ARRAY[0, 1]))),
    CONSTRAINT chk_channel_no_free_purchase CHECK ((NOT ((access_type = 0) AND (join_policy = 3)))),
    CONSTRAINT chk_channel_scope_xor CHECK ((((scope = 'personal'::text) AND (workspace_id IS NULL)) OR ((scope = 'workspace'::text) AND (workspace_id IS NOT NULL)))),
    CONSTRAINT chk_channel_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1]))),
    CONSTRAINT chk_channel_subscriber_count CHECK ((subscriber_count >= 0))
);


--
-- Name: TABLE channel; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel IS '频道表';


--
-- Name: COLUMN channel.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.id IS '主键 自增长ID';


--
-- Name: COLUMN channel.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.name IS '频道名称';


--
-- Name: COLUMN channel.description; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.description IS '频道描述/简介';


--
-- Name: COLUMN channel.avatar; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.avatar IS '频道头像';


--
-- Name: COLUMN channel.custom_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.custom_id IS '自定义ID，用于用户友好的频道标识';


--
-- Name: COLUMN channel.creator_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.creator_uid IS '创建者用户ID';


--
-- Name: COLUMN channel.subscriber_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.subscriber_count IS '订阅者数量';


--
-- Name: COLUMN channel.is_verified; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.is_verified IS '是否已认证';


--
-- Name: COLUMN channel.tags; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.tags IS '标签列表，JSON数组格式';


--
-- Name: COLUMN channel.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.status IS '状态: -1 删除 0 禁用 1 启用';


--
-- Name: COLUMN channel.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.updated_at IS '最后更新时间';


--
-- Name: COLUMN channel.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.created_at IS '创建时间';


--
-- Name: COLUMN channel.visibility; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.visibility IS '0 public / 1 private（谁能发现）';


--
-- Name: COLUMN channel.access_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.access_type IS '0 free / 1 paid（是否付费）';


--
-- Name: COLUMN channel.join_policy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.join_policy IS '0 open / 1 invite / 2 approval(fail-closed 未实现) / 3 purchase';


--
-- Name: COLUMN channel.scope; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.scope IS '归属维度: personal 个人（直属用户，无 workspace）| workspace 工作区（I1，创建后不可变）';


--
-- Name: COLUMN channel.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel.workspace_id IS '所属工作区ID；scope=personal 时必须为 NULL（XOR CHECK 强制）';


--
-- Name: channel_access_type2_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_access_type2_audit (
    audited_at timestamp with time zone DEFAULT now() NOT NULL,
    type2_count integer NOT NULL,
    approved_by text NOT NULL
);


--
-- Name: channel_admin; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_admin (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    role smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE channel_admin; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_admin IS '频道管理员表';


--
-- Name: COLUMN channel_admin.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_admin.channel_id IS '频道ID';


--
-- Name: COLUMN channel_admin.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_admin.user_id IS '用户ID';


--
-- Name: COLUMN channel_admin.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_admin.role IS '角色: 1 编辑 2 管理员 3 创建者';


--
-- Name: COLUMN channel_admin.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_admin.created_at IS '添加时间';


--
-- Name: channel_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_category (
    id bigint NOT NULL,
    name character varying(50) NOT NULL,
    icon character varying(320),
    sort_order integer DEFAULT 0,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: channel_comment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_comment (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    message_id bigint NOT NULL,
    user_id bigint NOT NULL,
    user_name character varying(200) DEFAULT ''::character varying NOT NULL,
    user_avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    content text NOT NULL,
    parent_id bigint DEFAULT 0 NOT NULL,
    reply_to_uid bigint DEFAULT 0 NOT NULL,
    reply_to_name character varying(200) DEFAULT ''::character varying NOT NULL,
    like_count bigint DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE channel_comment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_comment IS '频道消息评论';


--
-- Name: COLUMN channel_comment.parent_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_comment.parent_id IS '父评论ID，0=顶级评论';


--
-- Name: COLUMN channel_comment.reply_to_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_comment.reply_to_uid IS '被回复用户ID（二级回复），0=无';


--
-- Name: COLUMN channel_comment.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_comment.status IS '1=正常, 0=已删除';


--
-- Name: channel_invitation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_invitation (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    inviter_uid bigint NOT NULL,
    invitee_uid bigint NOT NULL,
    invitation_code character varying(64),
    status smallint DEFAULT 0 NOT NULL,
    message character varying(500),
    expires_at timestamp with time zone NOT NULL,
    accepted_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_channel_invitation_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])))
);


--
-- Name: TABLE channel_invitation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_invitation IS '频道邀请表 - 私有频道的邀请机制';


--
-- Name: COLUMN channel_invitation.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.id IS '主键';


--
-- Name: COLUMN channel_invitation.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.channel_id IS '频道ID';


--
-- Name: COLUMN channel_invitation.inviter_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.inviter_uid IS '邀请人用户ID';


--
-- Name: COLUMN channel_invitation.invitee_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.invitee_uid IS '被邀请人用户ID';


--
-- Name: COLUMN channel_invitation.invitation_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.invitation_code IS '邀请码（可选，用于分享链接）';


--
-- Name: COLUMN channel_invitation.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.status IS '状态: 0待处理 1已接受 2已拒绝 3已过期 4已取消';


--
-- Name: COLUMN channel_invitation.message; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.message IS '邀请消息';


--
-- Name: COLUMN channel_invitation.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.expires_at IS '邀请过期时间';


--
-- Name: COLUMN channel_invitation.accepted_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.accepted_at IS '接受时间';


--
-- Name: COLUMN channel_invitation.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.created_at IS '创建时间';


--
-- Name: COLUMN channel_invitation.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_invitation.updated_at IS '更新时间';


--
-- Name: channel_message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_message (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    author_id bigint NOT NULL,
    author_name character varying(200) DEFAULT ''::character varying NOT NULL,
    author_avatar character varying(320) DEFAULT ''::character varying NOT NULL,
    content text NOT NULL,
    msg_type character varying(50) DEFAULT 'text'::character varying NOT NULL,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    is_pinned boolean DEFAULT false NOT NULL,
    view_count bigint DEFAULT 0 NOT NULL,
    reaction_summary jsonb DEFAULT '{}'::jsonb NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    revoked boolean DEFAULT false NOT NULL,
    revoked_at timestamp with time zone,
    revoked_by bigint,
    edited_at timestamp with time zone,
    request_id character varying(64)
);


--
-- Name: TABLE channel_message; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_message IS '频道消息表';


--
-- Name: COLUMN channel_message.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.channel_id IS '频道ID';


--
-- Name: COLUMN channel_message.author_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.author_id IS '发布者用户ID';


--
-- Name: COLUMN channel_message.author_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.author_name IS '发布者昵称';


--
-- Name: COLUMN channel_message.author_avatar; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.author_avatar IS '发布者头像';


--
-- Name: COLUMN channel_message.content; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.content IS '消息内容';


--
-- Name: COLUMN channel_message.msg_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.msg_type IS '消息类型';


--
-- Name: COLUMN channel_message.payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.payload IS '扩展数据，JSON格式';


--
-- Name: COLUMN channel_message.is_pinned; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.is_pinned IS '是否置顶';


--
-- Name: COLUMN channel_message.view_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.view_count IS '阅读量';


--
-- Name: COLUMN channel_message.reaction_summary; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.reaction_summary IS '反应统计，JSON格式';


--
-- Name: COLUMN channel_message.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.status IS '状态: -1 删除 1 正常';


--
-- Name: COLUMN channel_message.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.updated_at IS '更新时间';


--
-- Name: COLUMN channel_message.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.created_at IS '创建时间';


--
-- Name: COLUMN channel_message.revoked; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.revoked IS '是否已撤回';


--
-- Name: COLUMN channel_message.revoked_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.revoked_at IS '撤回时间';


--
-- Name: COLUMN channel_message.revoked_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.revoked_by IS '撤回操作人';


--
-- Name: COLUMN channel_message.edited_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.edited_at IS '最后编辑时间 / Last edited time';


--
-- Name: COLUMN channel_message.request_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message.request_id IS '客户端发布请求幂等键；同一作者和频道内复用时返回原消息';


--
-- Name: channel_message_view; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_message_view (
    id bigint NOT NULL,
    message_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    viewed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE channel_message_view; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_message_view IS '频道消息阅读记录表';


--
-- Name: COLUMN channel_message_view.message_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message_view.message_id IS '消息ID';


--
-- Name: COLUMN channel_message_view.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message_view.channel_id IS '频道ID';


--
-- Name: COLUMN channel_message_view.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message_view.user_id IS '阅读者用户ID';


--
-- Name: COLUMN channel_message_view.viewed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_message_view.viewed_at IS '阅读时间';


--
-- Name: channel_order; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_order (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    order_no character varying(64) NOT NULL,
    amount numeric(10,2) NOT NULL,
    currency character varying(10) DEFAULT 'CNY'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    payment_method character varying(20),
    payment_no character varying(128),
    payment_at timestamp with time zone,
    subscription_start_at timestamp with time zone,
    subscription_end_at timestamp with time zone,
    expires_at timestamp with time zone NOT NULL,
    refund_reason character varying(500),
    refund_at timestamp with time zone,
    extra_data jsonb,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_channel_order_amount CHECK ((amount >= (0)::numeric)),
    CONSTRAINT chk_channel_order_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4, 5])))
);


--
-- Name: TABLE channel_order; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_order IS '频道订单表 - 付费频道的订阅订单';


--
-- Name: COLUMN channel_order.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.id IS '主键';


--
-- Name: COLUMN channel_order.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.channel_id IS '频道ID';


--
-- Name: COLUMN channel_order.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.user_id IS '用户ID';


--
-- Name: COLUMN channel_order.order_no; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.order_no IS '订单号';


--
-- Name: COLUMN channel_order.amount; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.amount IS '支付金额';


--
-- Name: COLUMN channel_order.currency; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.currency IS '货币类型';


--
-- Name: COLUMN channel_order.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.status IS '状态: 0待支付 1已支付 2已退款 3已取消 4已过期';


--
-- Name: COLUMN channel_order.payment_method; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.payment_method IS '支付方式';


--
-- Name: COLUMN channel_order.payment_no; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.payment_no IS '第三方支付流水号';


--
-- Name: COLUMN channel_order.payment_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.payment_at IS '支付时间';


--
-- Name: COLUMN channel_order.subscription_start_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.subscription_start_at IS '订阅开始时间';


--
-- Name: COLUMN channel_order.subscription_end_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.subscription_end_at IS '订阅结束时间';


--
-- Name: COLUMN channel_order.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.expires_at IS '订单过期时间';


--
-- Name: COLUMN channel_order.refund_reason; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.refund_reason IS '退款原因';


--
-- Name: COLUMN channel_order.refund_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.refund_at IS '退款时间';


--
-- Name: COLUMN channel_order.extra_data; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.extra_data IS '扩展数据';


--
-- Name: COLUMN channel_order.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.created_at IS '创建时间';


--
-- Name: COLUMN channel_order.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_order.updated_at IS '更新时间';


--
-- Name: channel_price; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_price (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    price numeric(10,2) DEFAULT 0 NOT NULL,
    currency character varying(10) DEFAULT 'CNY'::character varying NOT NULL,
    subscription_type smallint DEFAULT 1 NOT NULL,
    original_price numeric(10,2),
    description character varying(500),
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT chk_channel_price CHECK ((price >= (0)::numeric))
);


--
-- Name: TABLE channel_price; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_price IS '频道价格配置表 - 付费频道的订阅价格';


--
-- Name: COLUMN channel_price.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.id IS '主键';


--
-- Name: COLUMN channel_price.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.channel_id IS '频道ID';


--
-- Name: COLUMN channel_price.price; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.price IS '当前价格';


--
-- Name: COLUMN channel_price.currency; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.currency IS '货币类型（CNY/USD等）';


--
-- Name: COLUMN channel_price.subscription_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.subscription_type IS '订阅类型: 1一次性 2月订阅 3年订阅';


--
-- Name: COLUMN channel_price.original_price; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.original_price IS '原价（用于显示折扣）';


--
-- Name: COLUMN channel_price.description; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.description IS '价格说明';


--
-- Name: COLUMN channel_price.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.status IS '状态: 0禁用 1启用';


--
-- Name: COLUMN channel_price.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.created_at IS '创建时间';


--
-- Name: COLUMN channel_price.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_price.updated_at IS '更新时间';


--
-- Name: channel_reaction; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_reaction (
    id bigint NOT NULL,
    message_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    reaction_type character varying(50) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE channel_reaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_reaction IS '频道消息反应表';


--
-- Name: COLUMN channel_reaction.message_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_reaction.message_id IS '消息ID';


--
-- Name: COLUMN channel_reaction.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_reaction.channel_id IS '频道ID';


--
-- Name: COLUMN channel_reaction.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_reaction.user_id IS '反应者用户ID';


--
-- Name: COLUMN channel_reaction.reaction_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_reaction.reaction_type IS '反应类型: like, heart, fire, thumbs_up, bookmark 等';


--
-- Name: COLUMN channel_reaction.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_reaction.created_at IS '反应时间';


--
-- Name: channel_stats_daily; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_stats_daily (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    stats_date date NOT NULL,
    new_subscribers integer DEFAULT 0 NOT NULL,
    unsubscribers integer DEFAULT 0 NOT NULL,
    net_subscribers integer DEFAULT 0 NOT NULL,
    messages_count integer DEFAULT 0 NOT NULL,
    total_views bigint DEFAULT 0 NOT NULL,
    total_reactions integer DEFAULT 0 NOT NULL,
    active_viewers integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE channel_stats_daily; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_stats_daily IS '频道每日统计汇总表';


--
-- Name: COLUMN channel_stats_daily.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.channel_id IS '频道ID';


--
-- Name: COLUMN channel_stats_daily.stats_date; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.stats_date IS '统计日期';


--
-- Name: COLUMN channel_stats_daily.new_subscribers; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.new_subscribers IS '当日新增订阅数';


--
-- Name: COLUMN channel_stats_daily.unsubscribers; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.unsubscribers IS '当日取消订阅数';


--
-- Name: COLUMN channel_stats_daily.net_subscribers; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.net_subscribers IS '当日净增订阅数';


--
-- Name: COLUMN channel_stats_daily.messages_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.messages_count IS '当日发布消息数';


--
-- Name: COLUMN channel_stats_daily.total_views; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.total_views IS '当日总阅读量';


--
-- Name: COLUMN channel_stats_daily.total_reactions; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.total_reactions IS '当日总反应数';


--
-- Name: COLUMN channel_stats_daily.active_viewers; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_stats_daily.active_viewers IS '当日活跃阅读者数';


--
-- Name: channel_subscription; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_subscription (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    user_id bigint NOT NULL,
    subscribed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_read_at timestamp with time zone,
    last_message_id bigint DEFAULT 0,
    unread_count integer DEFAULT 0 NOT NULL,
    notifications_enabled boolean DEFAULT true NOT NULL,
    is_pinned boolean DEFAULT false NOT NULL,
    is_muted boolean DEFAULT false NOT NULL,
    status smallint DEFAULT 1 NOT NULL
);


--
-- Name: TABLE channel_subscription; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_subscription IS '频道订阅关系表';


--
-- Name: COLUMN channel_subscription.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.channel_id IS '频道ID';


--
-- Name: COLUMN channel_subscription.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.user_id IS '用户ID';


--
-- Name: COLUMN channel_subscription.subscribed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.subscribed_at IS '订阅时间';


--
-- Name: COLUMN channel_subscription.last_read_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.last_read_at IS '最后阅读时间';


--
-- Name: COLUMN channel_subscription.last_message_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.last_message_id IS '最后已读消息ID';


--
-- Name: COLUMN channel_subscription.unread_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.unread_count IS '未读消息数';


--
-- Name: COLUMN channel_subscription.notifications_enabled; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.notifications_enabled IS '是否开启通知';


--
-- Name: COLUMN channel_subscription.is_pinned; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.is_pinned IS '是否置顶';


--
-- Name: COLUMN channel_subscription.is_muted; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.is_muted IS '是否免打扰';


--
-- Name: COLUMN channel_subscription.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_subscription.status IS '状态: 0 取消订阅 1 已订阅';


--
-- Name: channel_webhook; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channel_webhook (
    id bigint NOT NULL,
    channel_id bigint NOT NULL,
    name character varying(64) DEFAULT ''::character varying NOT NULL,
    token character varying(64) NOT NULL,
    bot_uid bigint NOT NULL,
    creator_uid bigint NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    token_digest text DEFAULT ''::text NOT NULL,
    token_prefix text DEFAULT ''::text NOT NULL,
    grace_digest text DEFAULT ''::text NOT NULL,
    grace_until timestamp with time zone,
    last_used_at timestamp with time zone
);


--
-- Name: TABLE channel_webhook; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.channel_webhook IS '频道 incoming webhook（token 即凭证，绑定 channel + system_bot 作者）';


--
-- Name: COLUMN channel_webhook.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.id IS 'TSID 主键';


--
-- Name: COLUMN channel_webhook.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.channel_id IS '绑定频道 ID（channel locking：只能发到该频道）';


--
-- Name: COLUMN channel_webhook.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.name IS 'webhook 名称（bot 昵称）';


--
-- Name: COLUMN channel_webhook.token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.token IS '随机凭证（仅创建时明文返回一次；list 只回前 8 位掩码）';


--
-- Name: COLUMN channel_webhook.bot_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.bot_uid IS '消息作者 bot 用户 ID（user.account_type=2 服务端事实标记，防钓鱼）';


--
-- Name: COLUMN channel_webhook.creator_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.creator_uid IS '创建者用户 ID（须为频道管理员 role>=2）';


--
-- Name: COLUMN channel_webhook.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.channel_webhook.status IS '状态：1=启用 2=停用（停用后 incoming 统一 404）';


--
-- Name: class_enrollment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.class_enrollment (
    group_id bigint NOT NULL,
    learner_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    joined_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_class_enrollment_status CHECK ((status = ANY (ARRAY['active'::text, 'removed'::text])))
);


--
-- Name: TABLE class_enrollment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.class_enrollment IS '学员入班记录（learner 机构 == 班级 Group 经 workspace 解析的机构，触发器强制）';


--
-- Name: COLUMN class_enrollment.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.class_enrollment.status IS '状态: active 在班 | removed 已移出（软删）';


--
-- Name: class_profile; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.class_profile (
    group_id bigint NOT NULL,
    course_type text DEFAULT 'hard_pen'::text NOT NULL,
    term character varying(100),
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_class_profile_course_type CHECK ((course_type = ANY (ARRAY['hard_pen'::text, 'brush'::text, 'mixed'::text]))),
    CONSTRAINT ck_class_profile_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text])))
);


--
-- Name: TABLE class_profile; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.class_profile IS '班级教学档案（Group 是沟通容器，教学属性在此；1:1 扩展，D-07）';


--
-- Name: COLUMN class_profile.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.class_profile.group_id IS 'PK/FK -> "group".id（班级沟通容器）';


--
-- Name: COLUMN class_profile.course_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.class_profile.course_type IS '课程类型: hard_pen 硬笔 | brush 毛笔 | mixed 混合';


--
-- Name: COLUMN class_profile.term; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.class_profile.term IS '学期标识（可空）';


--
-- Name: class_staff; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.class_staff (
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    role text DEFAULT 'teacher'::text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_class_staff_role CHECK ((role = ANY (ARRAY['manager'::text, 'teacher'::text, 'assistant'::text]))),
    CONSTRAINT ck_class_staff_status CHECK ((status = ANY (ARRAY['active'::text, 'removed'::text])))
);


--
-- Name: TABLE class_staff; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.class_staff IS '班级教学角色（manager/teacher/assistant；聊天管理角色≠教学角色）';


--
-- Name: COLUMN class_staff.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.class_staff.role IS '教学角色: manager 班主任 | teacher 老师 | assistant 助教（无发布回评权）';


--
-- Name: COLUMN class_staff.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.class_staff.status IS '状态: active 在任 | removed 已移除（软删）';


--
-- Name: compliance_key; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.compliance_key (
    id bigint NOT NULL,
    key_id character varying(64) NOT NULL,
    public_key text NOT NULL,
    algorithm character varying(64) DEFAULT 'RSA-OAEP-256'::character varying,
    status smallint DEFAULT 1 NOT NULL,
    created_by bigint,
    revoked_at timestamp with time zone,
    revoked_by bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE compliance_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.compliance_key IS '合规密钥管理表 - 三层加密架构';


--
-- Name: COLUMN compliance_key.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.compliance_key.status IS '0:已撤销 1:活跃 2:已轮换';


--
-- Name: config; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE config; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.config IS '系统配置';


--
-- Name: COLUMN config.tab; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.tab IS '配置选项，便于后台分类浏览';


--
-- Name: COLUMN config.key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.key IS '主键';


--
-- Name: COLUMN config.title; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.title IS '标题';


--
-- Name: COLUMN config.sort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.sort IS '排序 降序排序，大的值在前面';


--
-- Name: COLUMN config.system; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.system IS '是否为系统配置，系统配置不可删除';


--
-- Name: COLUMN config.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN config.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN config.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.config.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: conversation; Type: TABLE; Schema: public; Owner: -
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
    CONSTRAINT chk_conversation_type CHECK (((type)::text = ANY (ARRAY[(''::character varying)::text, ('C2C'::character varying)::text, ('C2G'::character varying)::text, ('C2S'::character varying)::text, ('S2C'::character varying)::text])))
);


--
-- Name: TABLE conversation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.conversation IS '客户端会话记录表';


--
-- Name: COLUMN conversation.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation.user_id IS '发起会话用户ID (TSID bigint)';


--
-- Name: COLUMN conversation.peer_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation.peer_id IS '对端ID (TSID bigint)';


--
-- Name: COLUMN conversation.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation.type IS '会话类型 C2C C2G C2S S2C';


--
-- Name: COLUMN conversation.last_msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation.last_msg_id IS '最后消息ID (TSID bigint)';


--
-- Name: conversation_delete; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.conversation_delete (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    conversation_type character varying(10) NOT NULL,
    deleted_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_conversation_delete_type CHECK (((conversation_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text])))
);


--
-- Name: TABLE conversation_delete; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.conversation_delete IS '会话软删除记录表';


--
-- Name: COLUMN conversation_delete.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_delete.user_id IS '用户ID';


--
-- Name: COLUMN conversation_delete.conversation_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_delete.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';


--
-- Name: COLUMN conversation_delete.conversation_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_delete.conversation_type IS '会话类型：c2c-单聊，c2g-群聊';


--
-- Name: COLUMN conversation_delete.deleted_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_delete.deleted_at IS '删除时间，可用于恢复功能';


--
-- Name: COLUMN conversation_delete.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_delete.created_at IS '创建时间';


--
-- Name: conversation_pin; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.conversation_pin (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    conversation_type character varying(10) NOT NULL,
    pinned_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_conversation_pin_type CHECK (((conversation_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text])))
);


--
-- Name: TABLE conversation_pin; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.conversation_pin IS '会话置顶表';


--
-- Name: COLUMN conversation_pin.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_pin.user_id IS '用户ID';


--
-- Name: COLUMN conversation_pin.conversation_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_pin.conversation_id IS '会话ID (TSID bigint，单聊为对方UID，群聊为群ID)';


--
-- Name: COLUMN conversation_pin.conversation_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_pin.conversation_type IS '会话类型：c2c-单聊，c2g-群聊';


--
-- Name: COLUMN conversation_pin.pinned_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_pin.pinned_at IS '置顶时间';


--
-- Name: COLUMN conversation_pin.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.conversation_pin.created_at IS '创建时间';


--
-- Name: customer_service_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_event (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    session_id bigint,
    business_identity_id bigint,
    actor_user_id bigint,
    actor_kind text,
    action text NOT NULL,
    detail jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    workspace_id bigint NOT NULL,
    CONSTRAINT ck_cse_action CHECK ((action <> ''::text))
);


--
-- Name: TABLE customer_service_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_event IS '客服域状态审计（append-only）：seat/session/key/token 的运营状态变化；业务消息/附件审计仍以 enterprise_audit_event 为真源';


--
-- Name: COLUMN customer_service_event.actor_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_event.actor_kind IS '执行主体类别快照：seat | visitor | tenant_admin | platform_admin | system | application';


--
-- Name: COLUMN customer_service_event.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_event.workspace_id IS '事件发生的 Workspace（SSE 分区维度；会话事件自 session 真源派生，Org 级事件取 Org 默认/最小 active Workspace）';


--
-- Name: customer_service_read_cursor; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_read_cursor (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    session_id bigint NOT NULL,
    business_identity_id bigint NOT NULL,
    last_read_message_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_csrc_cursor_nonneg CHECK ((last_read_message_id >= 0))
);


--
-- Name: TABLE customer_service_read_cursor; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_read_cursor IS '客服已读游标（CS-BE-04，绑定 org+session+经办 identity）：ACK 单调前进不可回退；未读数由本表与 enterprise_message 事实现算，无冗余计数列';


--
-- Name: COLUMN customer_service_read_cursor.business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_read_cursor.business_identity_id IS '游标归属的经办业务身份（assignment/identity 绑定）：同一会话不同经办各有独立游标；transfer 换绑后受让人从 transfer 边界起计新未读（CS-DEC-02）';


--
-- Name: COLUMN customer_service_read_cursor.last_read_message_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_read_cursor.last_read_message_id IS '已读到的最后一条 enterprise_message id（0=尚无已读事实）：ACK 写路径按「新值>旧值」单调 upsert，重复/乱序旧 ACK 为零行 no-op';


--
-- Name: customer_service_seat; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_seat (
    organization_id bigint NOT NULL,
    business_identity_id bigint NOT NULL,
    function_key text DEFAULT 'customer_service'::text NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    max_concurrent integer DEFAULT 1 NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    created_by_user_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_css_function_key CHECK ((function_key = 'customer_service'::text)),
    CONSTRAINT ck_css_max_concurrent CHECK ((max_concurrent >= 1)),
    CONSTRAINT ck_css_version CHECK ((version >= 1))
);


--
-- Name: TABLE customer_service_seat; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_seat IS '客服坐席（identity 的运营属性，归 Organization）：PK=business_identity_id；function 必须 customer_service；不含 owner user（A01/A04）';


--
-- Name: COLUMN customer_service_seat.business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat.business_identity_id IS '坐席对应的稳定业务身份；当前用户由 active identity assignment 决定，identity rebind 后 seat 原地继续可用';


--
-- Name: COLUMN customer_service_seat.function_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat.function_key IS '固定 customer_service（CHECK + 复合 FK 双重保证引用的一定是客服 identity，A01）';


--
-- Name: COLUMN customer_service_seat.enabled; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat.enabled IS '坐席开关：false 时新 claim 立即被拒（suspend seat），既有 active 会话不自动迁移';


--
-- Name: COLUMN customer_service_seat.max_concurrent; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat.max_concurrent IS '该坐席允许的最大 active 会话数；claim 时在 seat 行锁内按 active 计数裁决（A02）';


--
-- Name: customer_service_seat_console; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_seat_console (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    public_seat_console_id text NOT NULL,
    allowed_origins jsonb DEFAULT '[]'::jsonb NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    revoked_at timestamp with time zone,
    created_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_cssc_public_seat_console_id CHECK ((public_seat_console_id <> ''::text)),
    CONSTRAINT ck_cssc_revoked_consistency CHECK ((((status = 'active'::text) AND (revoked_at IS NULL)) OR ((status = 'revoked'::text) AND (revoked_at IS NOT NULL)))),
    CONSTRAINT ck_cssc_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_cssc_version CHECK ((version >= 1))
);


--
-- Name: TABLE customer_service_seat_console; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_seat_console IS '客服工作台坐席控制台嵌入（Org+Workspace 的公开接入面）：public_seat_console_id
    可公开分发（iframe src 全局反查键），嵌入策略由应用层 allowed_origins 归一后
    以 CSP frame-ancestors 输出；本表不承载任何 secret';


--
-- Name: COLUMN customer_service_seat_console.public_seat_console_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat_console.public_seat_console_id IS '公开控制台标识（TSID 十进制 string，应用层生成）：/seat/:id 的全局反查键；
    换取任何嵌入授权都必须命中 active 行（吊销即失效，无存在性枚举）';


--
-- Name: COLUMN customer_service_seat_console.allowed_origins; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat_console.allowed_origins IS '允许嵌入控制台的 origin 原文 jsonb 数组；scheme+host+port 归一化、去重与
    上限校验由应用层做（DB 存原文）';


--
-- Name: COLUMN customer_service_seat_console.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat_console.status IS 'active|revoked：revoked 是 kill switch（行保留以审计，嵌入面立即失效）';


--
-- Name: customer_service_seat_limit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_seat_limit (
    organization_id bigint NOT NULL,
    seat_limit integer NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_cssl_limit_positive CHECK ((seat_limit >= 1))
);


--
-- Name: TABLE customer_service_seat_limit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_seat_limit IS '客服席位 entitlement（CS-BE-06，组织级人工配置）：无行=unlimited（现存组织默认）；降低 limit 不自动停用既有 Seat，只阻止新增/恢复超额并返回 seat_limit_exceeded；used 由 enabled seat 现算（无计数列）；不接计费（CS-DEC-03）';


--
-- Name: COLUMN customer_service_seat_limit.seat_limit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat_limit.seat_limit IS '本组织允许的启用坐席上限（>=1）；降低不自动停用存量，只封新增/恢复';


--
-- Name: customer_service_seat_presence; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_seat_presence (
    organization_id bigint NOT NULL,
    business_identity_id bigint NOT NULL,
    last_heartbeat_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    manual_status text,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_cssp_manual_status CHECK (((manual_status IS NULL) OR (manual_status = 'away'::text)))
);


--
-- Name: TABLE customer_service_seat_presence; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_seat_presence IS '客服坐席 presence 心跳 lease（CS-BE-05）：持久/共享可见的运行态事实输入；online/away/busy/offline 由 cs_presence 派生（TTL 90s + 手动 away 优先 + 容量满 busy），本表不存派生值、cs_seat 无新增治理列（CS-DEC-02）';


--
-- Name: COLUMN customer_service_seat_presence.last_heartbeat_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat_presence.last_heartbeat_at IS '最近一次心跳的服务端时钟（写路径注入 at，客户端不可报时）；NULL 视为从未上报（offline）';


--
-- Name: COLUMN customer_service_seat_presence.manual_status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_seat_presence.manual_status IS '手动运行态覆盖（当前冻结枚举仅 ''away''）：手动 away 优先于自动派生；NULL=无手动覆盖';


--
-- Name: customer_service_session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_session (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    business_identity_id bigint,
    visit_token_id bigint,
    status text DEFAULT 'queued'::text NOT NULL,
    rating integer,
    rating_at timestamp with time zone,
    queued_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    claimed_at timestamp with time zone,
    closed_at timestamp with time zone,
    close_reason text,
    created_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_csss_active_requires_identity CHECK (((status <> 'active'::text) OR (business_identity_id IS NOT NULL))),
    CONSTRAINT ck_csss_closed_time CHECK (((status <> 'closed'::text) OR (closed_at IS NOT NULL))),
    CONSTRAINT ck_csss_queued_no_identity CHECK (((status <> 'queued'::text) OR (business_identity_id IS NULL))),
    CONSTRAINT ck_csss_rating_needs_closed CHECK (((rating IS NULL) OR (status = 'closed'::text))),
    CONSTRAINT ck_csss_rating_range CHECK (((rating IS NULL) OR ((rating >= 1) AND (rating <= 5)))),
    CONSTRAINT ck_csss_rating_time CHECK ((((rating IS NULL) AND (rating_at IS NULL)) OR ((rating IS NOT NULL) AND (rating_at IS NOT NULL)))),
    CONSTRAINT ck_csss_status CHECK ((status = ANY (ARRAY['queued'::text, 'active'::text, 'closed'::text]))),
    CONSTRAINT ck_csss_version CHECK ((version >= 1))
);


--
-- Name: TABLE customer_service_session; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_session IS '客服会话（queued/active/closed/version）：绑定企业会话真源；不存任何消息副本，客服消息只经 enterprise_business_facade 写 enterprise_message（A03）';


--
-- Name: COLUMN customer_service_session.business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_session.business_identity_id IS '当前服务坐席的业务身份（不是 user_id）：identity rebind 换人后本行与历史零迁移仍然连续（A04）';


--
-- Name: COLUMN customer_service_session.rating; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_session.rating IS '满意度评分 1..5；只允许写在 closed 会话上，写评分同时落 customer_service_event 审计';


--
-- Name: COLUMN customer_service_session.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_session.version IS 'CAS 乐观锁：claim/transfer/close 按 (status, version) 条件 UPDATE，并发恰好一个成功（A02）';


--
-- Name: customer_service_shop_key; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_shop_key (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    key_digest text NOT NULL,
    display_hint text,
    status text DEFAULT 'active'::text NOT NULL,
    revoked_at timestamp with time zone,
    created_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_cssk_digest CHECK ((key_digest <> ''::text)),
    CONSTRAINT ck_cssk_revoked_consistency CHECK ((((status = 'active'::text) AND (revoked_at IS NULL)) OR ((status = 'revoked'::text) AND (revoked_at IS NOT NULL)))),
    CONSTRAINT ck_cssk_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_cssk_version CHECK ((version >= 1))
);


--
-- Name: TABLE customer_service_shop_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_shop_key IS '门店接入密钥（Org 级）：只存 sha256 digest；明文仅创建响应返回一次，可吊销（EB-D10 cs_shop_key）';


--
-- Name: COLUMN customer_service_shop_key.key_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_shop_key.key_digest IS '明文密钥的 sha256 hex；数据库永不保存明文，丢明文只能吊销重发';


--
-- Name: customer_service_visit_token; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_visit_token (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    token_digest text NOT NULL,
    display_hint text,
    expires_at timestamp with time zone NOT NULL,
    revoked_at timestamp with time zone,
    created_by_business_identity_id bigint,
    created_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    widget_installation_id bigint,
    anonymous_subject_hmac text,
    last_seen_at timestamp with time zone,
    CONSTRAINT ck_csvt_digest CHECK ((token_digest <> ''::text)),
    CONSTRAINT ck_csvt_expiry CHECK ((expires_at > created_at)),
    CONSTRAINT ck_csvt_version CHECK ((version >= 1))
);


--
-- Name: TABLE customer_service_visit_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_visit_token IS '访客令牌（EB-D10 cs_visit）：digest 有效 + 未过期 + 未吊销才可对绑定的 Org/contact 发消息/查自己的会话；不是 member/seat 凭证（A05）';


--
-- Name: COLUMN customer_service_visit_token.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_visit_token.expires_at IS '过期时间；过期后 digest 即使未吊销也立即失效';


--
-- Name: COLUMN customer_service_visit_token.widget_installation_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_visit_token.widget_installation_id IS 'Widget 令牌的安装绑定（NULL=运营侧签发的 visit token）：复合 FK 同 Org 口径，
    Widget bootstrap 令牌必须绑定本 Org 的 installation';


--
-- Name: COLUMN customer_service_visit_token.anonymous_subject_hmac; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_visit_token.anonymous_subject_hmac IS '匿名主体 HMAC（应用层计算）：不存裸会员标识 / Cookie / 支付信息，只存 HMAC';


--
-- Name: COLUMN customer_service_visit_token.last_seen_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_visit_token.last_seen_at IS 'Widget 令牌最近一次使用时间（心跳/活跃）；运营侧令牌为 NULL';


--
-- Name: customer_service_widget_identity_key; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_widget_identity_key (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    installation_id bigint NOT NULL,
    key_digest text NOT NULL,
    key_version integer NOT NULL,
    display_hint text,
    status text DEFAULT 'active'::text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    revoked_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_cswk_digest CHECK ((key_digest <> ''::text)),
    CONSTRAINT ck_cswk_expiry CHECK ((expires_at > created_at)),
    CONSTRAINT ck_cswk_key_version CHECK ((key_version >= 1)),
    CONSTRAINT ck_cswk_revoked_consistency CHECK ((((status = 'active'::text) AND (revoked_at IS NULL)) OR ((status = 'revoked'::text) AND (revoked_at IS NOT NULL)))),
    CONSTRAINT ck_cswk_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text])))
);


--
-- Name: TABLE customer_service_widget_identity_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_widget_identity_key IS 'Widget 身份签名密钥（安装级，key_version 轮换）：只存 sha256 摘要；
    明文 signing key 只在创建时返回一次（EB-D10 同口径）';


--
-- Name: COLUMN customer_service_widget_identity_key.key_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_widget_identity_key.key_version IS '密钥版本：同一 installation 内单调轮换（复合唯一约束防并发重放同版本）';


--
-- Name: customer_service_widget_installation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_widget_installation (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    public_widget_id text NOT NULL,
    display_name text NOT NULL,
    allowed_origins jsonb DEFAULT '[]'::jsonb NOT NULL,
    branding jsonb DEFAULT '{}'::jsonb NOT NULL,
    consent_version text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    revoked_at timestamp with time zone,
    created_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_cswi_consent_version CHECK ((consent_version <> ''::text)),
    CONSTRAINT ck_cswi_display_name CHECK ((display_name <> ''::text)),
    CONSTRAINT ck_cswi_public_widget_id CHECK ((public_widget_id <> ''::text)),
    CONSTRAINT ck_cswi_revoked_consistency CHECK ((((status = 'active'::text) AND (revoked_at IS NULL)) OR ((status = 'revoked'::text) AND (revoked_at IS NOT NULL)))),
    CONSTRAINT ck_cswi_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_cswi_version CHECK ((version >= 1))
);


--
-- Name: TABLE customer_service_widget_installation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_widget_installation IS 'Widget 安装（Org 的公开接入面）：public_widget_id 可公开分发但只在本 Org 解析；
    明文 secret 不在本表（密钥在 widget_identity_key，只存 digest）';


--
-- Name: COLUMN customer_service_widget_installation.public_widget_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_widget_installation.public_widget_id IS '公开 Widget 标识（wgt_pub_ 前缀由应用层生成）：可嵌入客户页面，不携带任何 secret；
    换取任何权限都必须携带 Org 作用域（CSB-01-A02）';


--
-- Name: COLUMN customer_service_widget_installation.allowed_origins; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_widget_installation.allowed_origins IS '允许嵌入的 origin 原文 jsonb 数组；同源 sibling origin 规范化由应用层做（DB 存原文）';


--
-- Name: COLUMN customer_service_widget_installation.branding; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_widget_installation.branding IS '品牌配置 jsonb；键白名单校验由应用层做，DB 不解释内容';


--
-- Name: COLUMN customer_service_widget_installation.consent_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.customer_service_widget_installation.consent_version IS 'Widget 侧访客同意文案版本标识（应用层生成；非空）';


--
-- Name: customer_service_widget_nonce; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.customer_service_widget_nonce (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    installation_id bigint NOT NULL,
    jti_digest text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_cswn_expiry CHECK ((expires_at > created_at)),
    CONSTRAINT ck_cswn_jti CHECK ((jti_digest <> ''::text))
);


--
-- Name: TABLE customer_service_widget_nonce; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.customer_service_widget_nonce IS 'Widget 请求 JTI 防重放（append-only）：并发同 jti 恰好一个 INSERT 成功（23505=重放）；
    行只按 expires_at 定界清理';


--
-- Name: e2ee_group_session_attestation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.e2ee_group_session_attestation (
    group_id bigint NOT NULL,
    session_id character varying(256) NOT NULL,
    sender_uid bigint NOT NULL,
    sender_did character varying(128) NOT NULL,
    room_key_msg_id character varying(40) NOT NULL,
    recipient_uids bigint[] NOT NULL,
    start_seq bigint NOT NULL,
    end_seq bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    CONSTRAINT chk_e2ee_group_session_ids CHECK (((group_id > 0) AND (sender_uid > 0) AND ((octet_length((session_id)::text) >= 1) AND (octet_length((session_id)::text) <= 256)) AND ((octet_length((sender_did)::text) >= 1) AND (octet_length((sender_did)::text) <= 128)) AND ((octet_length((room_key_msg_id)::text) >= 1) AND (octet_length((room_key_msg_id)::text) <= 40)))),
    CONSTRAINT chk_e2ee_group_session_range CHECK (((start_seq >= 1) AND (end_seq >= start_seq))),
    CONSTRAINT chk_e2ee_group_session_recipients CHECK (((array_ndims(recipient_uids) = 1) AND ((cardinality(recipient_uids) >= 1) AND (cardinality(recipient_uids) <= 5000)) AND (array_position(recipient_uids, NULL::bigint) IS NULL) AND (0 < ALL (recipient_uids))))
);


--
-- Name: TABLE e2ee_group_session_attestation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.e2ee_group_session_attestation IS 'Server-authoritative Megolm session origin, recipient snapshot and monotonic conv_seq range; no key plaintext';


--
-- Name: e2ee_group_session_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.e2ee_group_session_member (
    group_id bigint NOT NULL,
    session_id character varying(256) NOT NULL,
    user_id bigint NOT NULL,
    generation_no integer NOT NULL,
    generation_start_seq bigint NOT NULL,
    CONSTRAINT chk_e2ee_group_session_member_values CHECK (((group_id > 0) AND (user_id > 0) AND (generation_no > 0) AND (generation_start_seq >= 1)))
);


--
-- Name: TABLE e2ee_group_session_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.e2ee_group_session_member IS 'Immutable per-recipient membership generation captured when a Megolm room-key message is accepted';


--
-- Name: e2ee_key_backups; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.e2ee_key_backups (
    id bigint NOT NULL,
    uid bigint NOT NULL,
    backup_version integer NOT NULL,
    algo character varying(40) DEFAULT 'pbkdf2-sha256/aes-256-gcm'::character varying NOT NULL,
    kdf_salt text NOT NULL,
    kdf_iterations integer NOT NULL,
    encrypted_payload text NOT NULL,
    payload_hash character varying(128) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_e2ee_key_backups_iterations CHECK ((kdf_iterations >= 310000)),
    CONSTRAINT chk_e2ee_key_backups_salt_len CHECK ((length(kdf_salt) <= 256)),
    CONSTRAINT chk_e2ee_key_backups_uid CHECK ((uid > 0)),
    CONSTRAINT chk_e2ee_key_backups_version CHECK ((backup_version > 0))
);


--
-- Name: TABLE e2ee_key_backups; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.e2ee_key_backups IS 'E2EE 加密密钥备份（服务端只存密文，4S 模式）';


--
-- Name: COLUMN e2ee_key_backups.backup_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_backups.backup_version IS '备份版本号，客户端单调 +1，UNIQUE(uid,version) 防并发覆盖';


--
-- Name: COLUMN e2ee_key_backups.kdf_salt; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_backups.kdf_salt IS 'KDF 盐值 base64（客户端生成，恢复端派生用）';


--
-- Name: COLUMN e2ee_key_backups.kdf_iterations; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_backups.kdf_iterations IS 'KDF 迭代次数（客户端声明 >=310000，DB 下限 100000 防降级攻击）';


--
-- Name: COLUMN e2ee_key_backups.encrypted_payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_backups.encrypted_payload IS 'base64 密文包（身份私钥+房间 session keys），服务端不可解';


--
-- Name: COLUMN e2ee_key_backups.payload_hash; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_backups.payload_hash IS '客户端算的 SHA-256 十六进制，恢复端校验完整性';


--
-- Name: e2ee_key_shares; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.e2ee_key_shares (
    id bigint NOT NULL,
    owner_uid bigint NOT NULL,
    trustee_uid bigint NOT NULL,
    encrypted_share text NOT NULL,
    share_index integer NOT NULL,
    threshold integer DEFAULT 2 NOT NULL,
    total_shares integer DEFAULT 3 NOT NULL,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now(),
    CONSTRAINT e2ee_key_shares_owner_uid_check CHECK ((owner_uid > 0)),
    CONSTRAINT e2ee_key_shares_share_index_check CHECK (((share_index >= 1) AND (share_index <= 5))),
    CONSTRAINT e2ee_key_shares_threshold_check CHECK (((threshold >= 1) AND (threshold <= total_shares))),
    CONSTRAINT e2ee_key_shares_total_shares_check CHECK (((total_shares >= 2) AND (total_shares <= 5))),
    CONSTRAINT e2ee_key_shares_trustee_uid_check CHECK ((trustee_uid > 0))
);


--
-- Name: TABLE e2ee_key_shares; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.e2ee_key_shares IS 'E2EE 密钥分片表（社交恢复）';


--
-- Name: COLUMN e2ee_key_shares.owner_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.owner_uid IS '密钥所有者 ID';


--
-- Name: COLUMN e2ee_key_shares.trustee_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.trustee_uid IS '受托人 ID（存储密钥分片的好友）';


--
-- Name: COLUMN e2ee_key_shares.encrypted_share; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.encrypted_share IS '加密的密钥分片，使用受托人公钥加密';


--
-- Name: COLUMN e2ee_key_shares.share_index; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.share_index IS '分片索引（1-5）';


--
-- Name: COLUMN e2ee_key_shares.threshold; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.threshold IS '恢复阈值（需要多少分片才能恢复）';


--
-- Name: COLUMN e2ee_key_shares.total_shares; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.total_shares IS '总分片数（默认 3）';


--
-- Name: COLUMN e2ee_key_shares.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.created_at IS '创建时间';


--
-- Name: COLUMN e2ee_key_shares.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.e2ee_key_shares.updated_at IS '更新时间';


--
-- Name: enterprise_application; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_application (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    principal_user_id bigint,
    application_key text NOT NULL,
    name text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    allowed_scopes jsonb DEFAULT '[]'::jsonb NOT NULL,
    allowed_redirect_uris text[] DEFAULT '{}'::text[] NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    allowed_mime_types text[] DEFAULT '{}'::text[] NOT NULL,
    max_file_size_bytes bigint,
    version integer DEFAULT 1 NOT NULL,
    CONSTRAINT ck_ea_key_nonempty CHECK (((application_key <> ''::text) AND (length(application_key) <= 128))),
    CONSTRAINT ck_ea_max_file_size CHECK (((max_file_size_bytes IS NULL) OR (max_file_size_bytes > 0))),
    CONSTRAINT ck_ea_name_nonempty CHECK ((name <> ''::text)),
    CONSTRAINT ck_ea_scopes_array CHECK ((jsonb_typeof(allowed_scopes) = 'array'::text)),
    CONSTRAINT ck_ea_status CHECK ((status = ANY (ARRAY['draft'::text, 'active'::text, 'disabled'::text, 'archived'::text]))),
    CONSTRAINT ck_ea_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_application; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_application IS '企业集成 Application（/api/internal/v1 调用主体，归 Organization；调用方只能是客户 OA 后端）';


--
-- Name: COLUMN enterprise_application.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.id IS '主键 TSID';


--
-- Name: COLUMN enterprise_application.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.organization_id IS '资源 owner 租户；删除机构一律 RESTRICT（fail-closed）';


--
-- Name: COLUMN enterprise_application.principal_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.principal_user_id IS '内部绑定的可信 service-principal user（可空审计锚点）；业务判定只认 Application 自身，不依赖该 user 的账号类型字面值（plan-gz §4.3）';


--
-- Name: COLUMN enterprise_application.application_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.application_key IS 'Org 内唯一的稳定标识（URL/审计/凭证组合用）';


--
-- Name: COLUMN enterprise_application.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.name IS '展示名（客户 OA 后端应用名）';


--
-- Name: COLUMN enterprise_application.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.status IS '生命周期（迁移 00000143 放宽为四值）：draft 草稿 / active 启用 / disabled 停用 / archived 归档（终态）。合法迁移由应用层裁决，DB 只保证值域';


--
-- Name: COLUMN enterprise_application.allowed_scopes; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.allowed_scopes IS '固定 scopes 白名单 jsonb 数组（plan-gz §4.2 十值；成员校验在应用层，不支持 wildcard）';


--
-- Name: COLUMN enterprise_application.allowed_redirect_uris; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.allowed_redirect_uris IS '预注册 exact redirect URI allowlist（EPGZ-05 OA SSO）：元素逐字节精确匹配（= ANY，无规范化），仅 HTTPS、禁 fragment；SSO 签发/交换必须命中，空 allowlist = 一律拒绝（fail-closed）';


--
-- Name: COLUMN enterprise_application.allowed_mime_types; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.allowed_mime_types IS '企业附件 MIME 允许清单（元素级校验由触发器 trg_efapi_application_mime_guard 守卫）；空数组 = 沿用全局 elib_oss 白名单（不放大：confirm 侧仍以 HEAD 真实值复核）';


--
-- Name: COLUMN enterprise_application.max_file_size_bytes; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.max_file_size_bytes IS '企业附件单文件上限（字节）；NULL = 沿用全局 elib_oss:max_file_size()。只可收紧全局上限，不可放大（presign/confirm 取 min(全局, 本值)）';


--
-- Name: COLUMN enterprise_application.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application.version IS '乐观锁版本号（迁移 00000143 / FULL-08）：Admin 治理写入（status/scopes）以 WHERE version = expected_version 做 CAS，成功即 +1；并发写只有赢家生效，其余返回 version_conflict';


--
-- Name: enterprise_application_credential; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_application_credential (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    credential_prefix text NOT NULL,
    secret_digest text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    expires_at timestamp with time zone,
    last_used_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    revoked_at timestamp with time zone,
    CONSTRAINT ck_eac_prefix_nonempty CHECK (((credential_prefix <> ''::text) AND (length(credential_prefix) <= 128))),
    CONSTRAINT ck_eac_secret_digest CHECK ((length(secret_digest) = 64)),
    CONSTRAINT ck_eac_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_eac_status_revoked_match CHECK (((status = 'revoked'::text) = (revoked_at IS NOT NULL)))
);


--
-- Name: TABLE enterprise_application_credential; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_application_credential IS 'Application 凭证（形态 ib_int_<credential_id>.<secret>；DB 只存 SHA-256 digest 与非敏感 prefix，明文不落库）';


--
-- Name: COLUMN enterprise_application_credential.credential_prefix; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_credential.credential_prefix IS '认证定位键（credential 的非 secret 部分；全局唯一）；按 prefix 定位后 secret 走 constant-time digest 比对';


--
-- Name: COLUMN enterprise_application_credential.secret_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_credential.secret_digest IS 'secret 的 SHA-256 hex（64 字符）；明文只在创建/轮换响应出现一次';


--
-- Name: COLUMN enterprise_application_credential.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_credential.status IS '生命周期: active 可用 | revoked 已吊销';


--
-- Name: COLUMN enterprise_application_credential.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_credential.expires_at IS '过期时间（可空=不过期）；到期由 status+expires_at 实时判定，不落 expired 状态值';


--
-- Name: COLUMN enterprise_application_credential.last_used_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_credential.last_used_at IS '最近认证时间（审计/轮换策略用）';


--
-- Name: enterprise_application_grant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_application_grant (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    workspace_scope_kind text DEFAULT 'none'::text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    valid_from timestamp with time zone NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    revoked_at timestamp with time zone,
    revoked_by_user_id bigint,
    version integer DEFAULT 1 NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    revoked_by_adm_user_id bigint,
    CONSTRAINT ck_eag_idempotency_key CHECK (((idempotency_key <> ''::text) AND (length(idempotency_key) <= 256))),
    CONSTRAINT ck_eag_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_eag_status_revoked_match CHECK ((((status = 'revoked'::text) = (revoked_at IS NOT NULL)) AND
CASE
    WHEN (revoked_at IS NULL) THEN ((revoked_by_user_id IS NULL) AND (revoked_by_adm_user_id IS NULL))
    ELSE ((revoked_by_user_id IS NOT NULL) <> (revoked_by_adm_user_id IS NOT NULL))
END)),
    CONSTRAINT ck_eag_validity CHECK ((expires_at > valid_from)),
    CONSTRAINT ck_eag_version CHECK ((version >= 1)),
    CONSTRAINT ck_eag_workspace_scope_kind CHECK ((workspace_scope_kind = ANY (ARRAY['none'::text, 'explicit'::text])))
);


--
-- Name: TABLE enterprise_application_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_application_grant IS 'Application Grant（Organization Grant / Workspace Grant 真源）：scope 与资源授权取交集的授权侧输入；逐请求读取，撤权/到期下一次读取即生效';


--
-- Name: COLUMN enterprise_application_grant.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.id IS '主键 TSID';


--
-- Name: COLUMN enterprise_application_grant.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.organization_id IS 'Grant 归属机构；删除机构一律 RESTRICT（fail-closed）';


--
-- Name: COLUMN enterprise_application_grant.application_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.application_id IS '被授权 Application（同 Org 复合 FK；跨 Org 引用 23503 拒绝）';


--
-- Name: COLUMN enterprise_application_grant.workspace_scope_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.workspace_scope_kind IS 'none=org 全域（零行 grant_workspace）| explicit=显式 workspace 授权（至少一行，行数语义由 Grant command 同一事务校验）';


--
-- Name: COLUMN enterprise_application_grant.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.status IS '存储态只有 active|revoked；expired 不入库，到期由 status+expires_at 实时判定';


--
-- Name: COLUMN enterprise_application_grant.valid_from; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.valid_from IS '生效起始（不晚于 expires_at）；未生效的 Grant 不出现在 effective 视图';


--
-- Name: COLUMN enterprise_application_grant.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.expires_at IS '到期时刻；到期不依赖后台任务即在读取侧失效';


--
-- Name: COLUMN enterprise_application_grant.revoked_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.revoked_at IS '撤销时间；仅 status=revoked 非空（与 revoked_by_user_id 同空/同非空）';


--
-- Name: COLUMN enterprise_application_grant.revoked_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.revoked_by_user_id IS '撤销操作者；与 revoked_at 同空/同非空；RESTRICT 保护授权 lineage';


--
-- Name: COLUMN enterprise_application_grant.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.version IS 'CAS 版本（>=1）；所有 mutation 走 expected-version CAS（并发撤销/降级不丢更新）';


--
-- Name: COLUMN enterprise_application_grant.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.idempotency_key IS '幂等键；同 (organization_id, application_id) 范围唯一';


--
-- Name: COLUMN enterprise_application_grant.revoked_by_adm_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant.revoked_by_adm_user_id IS '撤销者：平台管理员 adm_user.id（迁移 00000143）。无 FK（跨平台域，删除管理员不牵连租户授权行）；只作审计留痕，不参与任何授权判定。与 revoked_by_user_id 恰有一个非空';


--
-- Name: CONSTRAINT ck_eag_status_revoked_match ON enterprise_application_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT ck_eag_status_revoked_match ON public.enterprise_application_grant IS '撤销一致性（迁移 00000143 放宽执行者来源）：status=revoked 与 revoked_at 严格配对；未撤销时两个 actor 列皆 NULL；已撤销时 revoked_by_user_id（租户 user）与 revoked_by_adm_user_id（平台管理员）恰有一个非空——两者皆空为无归因撤权，两者皆非空为归因歧义';


--
-- Name: enterprise_application_grant_scope; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_application_grant_scope (
    grant_id bigint NOT NULL,
    scope text NOT NULL,
    CONSTRAINT ck_eags_scope_fixed CHECK ((scope = ANY (ARRAY['application:read'::text, 'identities:read'::text, 'identities:write'::text, 'groups:read'::text, 'groups:write'::text, 'workspaces:read'::text, 'projects:read'::text, 'channels:read'::text, 'files:write'::text, 'messages:send'::text, 'messages:send_as_human'::text, 'friend_requests:create'::text, 'webhooks:manage'::text, 'sso:exchange'::text, 'customer_service:read'::text, 'customer_service:write'::text, 'workspaces:write'::text, 'channels:write'::text])))
);


--
-- Name: TABLE enterprise_application_grant_scope; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_application_grant_scope IS 'Grant 授权的固定 scope 集合（plan-gz §4.2 十值枚举；无 wildcard，DB 层 23514 拒绝通配/未登记值）';


--
-- Name: COLUMN enterprise_application_grant_scope.grant_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant_scope.grant_id IS '所属 Grant（RESTRICT 语义由父表删除守卫实现）';


--
-- Name: COLUMN enterprise_application_grant_scope.scope; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant_scope.scope IS '固定 scope 枚举成员（与 enterprise_internal_scope:all/0 逐字一致）';


--
-- Name: enterprise_application_grant_workspace; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_application_grant_workspace (
    organization_id bigint NOT NULL,
    grant_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    workspace_scope_kind text DEFAULT 'explicit'::text CONSTRAINT enterprise_application_grant_work_workspace_scope_kind_not_null NOT NULL,
    CONSTRAINT ck_eagw_kind_explicit CHECK ((workspace_scope_kind = 'explicit'::text))
);


--
-- Name: TABLE enterprise_application_grant_workspace; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_application_grant_workspace IS 'Workspace Grant 的显式 workspace 列表（workspace_scope_kind=explicit 的 Grant 才有行；同 Org 复合 FK 保证不跨 Org）';


--
-- Name: COLUMN enterprise_application_grant_workspace.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant_workspace.organization_id IS '机构 id；复合 FK 保证 Grant 与 workspace 同 Org';


--
-- Name: COLUMN enterprise_application_grant_workspace.grant_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant_workspace.grant_id IS '所属 Grant（三列复合 FK 引用父表 (organization_id, id, workspace_scope_kind)）';


--
-- Name: COLUMN enterprise_application_grant_workspace.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant_workspace.workspace_id IS '被授予的 workspace（复合 FK 保证同 Org，RESTRICT：workspace 物理删除被授权引用阻断）';


--
-- Name: COLUMN enterprise_application_grant_workspace.workspace_scope_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_grant_workspace.workspace_scope_kind IS '固定 explicit（kind 闸门列）：使 none 类型 Grant 无法挂载 workspace 行';


--
-- Name: enterprise_application_usage; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_application_usage (
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    metric text NOT NULL,
    period_start date NOT NULL,
    counter bigint DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_eau_counter CHECK ((counter >= 0)),
    CONSTRAINT ck_eau_metric CHECK ((metric = ANY (ARRAY['identity.bound'::text, 'identity.revoked'::text, 'directory.page'::text, 'file.confirmed'::text, 'message.accepted'::text, 'message.failed'::text, 'seat.read'::text])))
);


--
-- Name: TABLE enterprise_application_usage; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_application_usage IS '企业 Application 用量**聚合计量**（org/app/固定 metric 名/周期/计数）；刻意不含正文、会话内容、external_user_id 等任何 PII 列——列集封闭，仅计数与固定枚举键';


--
-- Name: COLUMN enterprise_application_usage.metric; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_usage.metric IS '固定计量名枚举（无自由文本，防止把正文塞进 metric）';


--
-- Name: COLUMN enterprise_application_usage.period_start; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_application_usage.period_start IS '计量周期起点（按月 date_trunc）';


--
-- Name: enterprise_asset; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_asset (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    conversation_id bigint,
    message_id bigint,
    business_identity_id bigint,
    uploaded_by_user_id bigint,
    object_key text NOT NULL,
    object_hash text NOT NULL,
    mime text,
    size_bytes bigint,
    status text DEFAULT 'pending_confirm'::text NOT NULL,
    key_version integer,
    cipher_metadata jsonb,
    retain_until timestamp with time zone,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    deleted_at timestamp with time zone,
    file_name text,
    pending_object_delete boolean DEFAULT false NOT NULL,
    CONSTRAINT ck_enterprise_asset_file_name CHECK (((file_name IS NULL) OR ((char_length(file_name) >= 1) AND (char_length(file_name) <= 256)))),
    CONSTRAINT ck_enterprise_asset_object_key_no_url CHECK ((object_key !~* '^[a-zA-Z][a-zA-Z0-9+.\-]*://'::text)),
    CONSTRAINT ck_enterprise_asset_pending_delete CHECK (((NOT pending_object_delete) OR (status = 'deleted'::text))),
    CONSTRAINT ck_enterprise_asset_size CHECK (((size_bytes IS NULL) OR (size_bytes >= 0))),
    CONSTRAINT ck_enterprise_asset_status CHECK ((status = ANY (ARRAY['pending_confirm'::text, 'active'::text, 'deleted'::text]))),
    CONSTRAINT ck_enterprise_asset_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_asset; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_asset IS '企业附件（归 Org）：只存 object key/hash/mime/size/cipher metadata，不存也不返回任何 storage URL；删除只服从 Org policy，不服从 uploader、不由 offboarding 触发';


--
-- Name: COLUMN enterprise_asset.uploaded_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.uploaded_by_user_id IS '上传人（仅审计）；user 删除后置 NULL，不级联企业附件';


--
-- Name: COLUMN enterprise_asset.object_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.object_key IS '企业私有前缀下的对象键；禁止持久化 URL（通用 scheme 形态 !~* ''^[a-zA-Z][a-zA-Z0-9+.-]*://''，覆盖 http/https/s3/gs 等）';


--
-- Name: COLUMN enterprise_asset.object_hash; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.object_hash IS '对象内容哈希（完整性校验；非加密材料）';


--
-- Name: COLUMN enterprise_asset.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.status IS '状态: pending_confirm 上传待确认（可被限定前缀 cleanup）| active 已确认 | deleted 已删除';


--
-- Name: COLUMN enterprise_asset.key_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.key_version IS 'cipher_metadata 对应的企业托管密钥版本';


--
-- Name: COLUMN enterprise_asset.cipher_metadata; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.cipher_metadata IS '加密元数据 jsonb（算法/IV/分段信息等，无明文、无密钥）';


--
-- Name: COLUMN enterprise_asset.retain_until; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.retain_until IS '附件保留截止时间；message_id 非空时必须非空且不得早于所属消息的 retain_until';


--
-- Name: COLUMN enterprise_asset.file_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.file_name IS '上传方声明的展示文件名（CS-BE-01 冻结契约 assets[].file_name 的数据源；可空=未声明，不回填；1..256 字符）';


--
-- Name: COLUMN enterprise_asset.pending_object_delete; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_asset.pending_object_delete IS 'Durable cleanup intent for expired unconfirmed assets only; clear after object deletion succeeds. Existing tombstones are not backfilled.';


--
-- Name: enterprise_asset_delete_queue; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_asset_delete_queue (
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    asset_id bigint NOT NULL,
    object_key text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE enterprise_asset_delete_queue; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_asset_delete_queue IS 'Committed retention purge object deletion intents. Enqueued with metadata deletion and audit; drained only after commit.';


--
-- Name: enterprise_attachment_retention; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_attachment_retention (
    attachment_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    retention_until timestamp with time zone NOT NULL,
    hold_state text DEFAULT 'none'::text NOT NULL,
    hold_reason text,
    hold_set_at timestamp with time zone,
    purge_state text DEFAULT 'live'::text NOT NULL,
    purged_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone,
    CONSTRAINT ck_ear_hold_consistency CHECK (((hold_state = 'held'::text) = (hold_set_at IS NOT NULL))),
    CONSTRAINT ck_ear_hold_reason CHECK ((((hold_state = 'held'::text) AND (hold_reason IS NOT NULL) AND (hold_reason <> ''::text)) OR ((hold_state = 'none'::text) AND (hold_reason IS NULL)))),
    CONSTRAINT ck_ear_hold_state CHECK ((hold_state = ANY (ARRAY['none'::text, 'held'::text]))),
    CONSTRAINT ck_ear_purge_consistency CHECK (((purge_state = 'purged'::text) = (purged_at IS NOT NULL))),
    CONSTRAINT ck_ear_purge_state CHECK ((purge_state = ANY (ARRAY['live'::text, 'purged'::text])))
);


--
-- Name: TABLE enterprise_attachment_retention; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_attachment_retention IS '企业附件（attachment scope=enterprise）留存/法务 hold/purge 账本（每附件一行；行禁止物理删除，purge 走 purge_state=purged 保留证据）';


--
-- Name: COLUMN enterprise_attachment_retention.retention_until; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_attachment_retention.retention_until IS '留存到期时间；只可延长（缩短被 23514 拒绝），未到期禁止 purge';


--
-- Name: COLUMN enterprise_attachment_retention.hold_state; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_attachment_retention.hold_state IS '法务 hold: none 无 | held 扣留中（held 期间禁止 purge，DB 层强制）';


--
-- Name: COLUMN enterprise_attachment_retention.purge_state; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_attachment_retention.purge_state IS 'purge 状态: live | purged（终态，不可回退）';


--
-- Name: enterprise_audit_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_audit_event (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    resource_type text NOT NULL,
    resource_id bigint,
    action text NOT NULL,
    business_identity_id bigint,
    actor_user_id bigint,
    actor_role text,
    detail jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_eae_action CHECK ((action <> ''::text)),
    CONSTRAINT ck_eae_resource_type CHECK ((resource_type <> ''::text))
);


--
-- Name: TABLE enterprise_audit_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_audit_event IS '企业审计事件（append-only 真源）：记录 Org/resource/action/identity/actor/detail；禁止 UPDATE 与 DELETE';


--
-- Name: COLUMN enterprise_audit_event.resource_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.resource_type IS '资源类型（如 enterprise_message / enterprise_asset / organization_business_identity_assignment）';


--
-- Name: COLUMN enterprise_audit_event.resource_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.resource_id IS '资源 ID（TSID；纯组织级事件可为空）';


--
-- Name: COLUMN enterprise_audit_event.action; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.action IS '动作名（如 message.accept / offboarding.rebind），非空';


--
-- Name: COLUMN enterprise_audit_event.business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.business_identity_id IS '执行时的业务身份（复合 FK 保证同 Org）';


--
-- Name: COLUMN enterprise_audit_event.actor_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.actor_user_id IS '执行者 user（仅审计/展示）；user 删除后置 NULL，不级联企业数据';


--
-- Name: COLUMN enterprise_audit_event.actor_role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.actor_role IS '执行时的角色快照（governance role 或 function，审计用文本快照）';


--
-- Name: COLUMN enterprise_audit_event.detail; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_audit_event.detail IS '结构化明细 jsonb（不得存放明文客户资料）';


--
-- Name: enterprise_contact; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_contact (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    imboy_user_id bigint,
    status text DEFAULT 'active'::text NOT NULL,
    display_name text,
    profile_cipher text,
    profile_key_version integer,
    created_by_business_identity_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_enterprise_contact_profile_key CHECK (((profile_cipher IS NULL) OR (profile_key_version IS NOT NULL))),
    CONSTRAINT ck_enterprise_contact_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text]))),
    CONSTRAINT ck_enterprise_contact_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_contact; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_contact IS '企业客户/外部联系人记录：关系 owner 为 Organization，可关联 IMBoy user 但不属于任何个人好友链（EB-D04）';


--
-- Name: COLUMN enterprise_contact.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.organization_id IS '资源 owner 租户；不可由普通业务操作改变';


--
-- Name: COLUMN enterprise_contact.imboy_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.imboy_user_id IS '可选关联的 IMBoy 个人账号；仅 SET NULL，不级联、不合并个人关系';


--
-- Name: COLUMN enterprise_contact.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.status IS '状态: active 在用 | archived 已归档（历史保留）';


--
-- Name: COLUMN enterprise_contact.display_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.display_name IS '企业侧展示名（明文仅限非敏感称呼；敏感资料一律走 profile_cipher）';


--
-- Name: COLUMN enterprise_contact.profile_cipher; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.profile_cipher IS '客户资料的企业托管密文；明文不入库，缺 profile_key_version 时一律拒绝';


--
-- Name: COLUMN enterprise_contact.profile_key_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.profile_key_version IS 'profile_cipher 对应的企业托管密钥版本';


--
-- Name: COLUMN enterprise_contact.created_by_business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact.created_by_business_identity_id IS '创建该客户的业务身份（审计）；不改变 Org 归属';


--
-- Name: enterprise_contact_assignment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_contact_assignment (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    business_identity_id bigint NOT NULL,
    role text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    assigned_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ended_at timestamp with time zone,
    assigned_by bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_eca_ended_at_consistency CHECK ((((status = 'active'::text) AND (ended_at IS NULL)) OR ((status = 'ended'::text) AND (ended_at IS NOT NULL)))),
    CONSTRAINT ck_eca_role CHECK ((role = ANY (ARRAY['primary'::text, 'collaborator'::text]))),
    CONSTRAINT ck_eca_status CHECK ((status = ANY (ARRAY['active'::text, 'ended'::text]))),
    CONSTRAINT ck_eca_time_range CHECK (((ended_at IS NULL) OR (ended_at >= assigned_at)))
);


--
-- Name: TABLE enterprise_contact_assignment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_contact_assignment IS '客户经办分配：把 contact 交给某个 business identity（primary 主办 / collaborator 协办），保留 active→ended 历史';


--
-- Name: COLUMN enterprise_contact_assignment.business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact_assignment.business_identity_id IS '承接该客户的业务身份；与 contact 必须同 Org（复合 FK）';


--
-- Name: COLUMN enterprise_contact_assignment.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact_assignment.role IS '分配角色: primary 主办（同一 contact 同时最多一个 active）| collaborator 协办';


--
-- Name: COLUMN enterprise_contact_assignment.assigned_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact_assignment.assigned_by IS '操作人（审计快照）；user 删除后置 NULL，不级联企业数据';


--
-- Name: enterprise_contact_identity; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_contact_identity (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    channel text NOT NULL,
    subject_hmac text NOT NULL,
    subject_mask text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_eci_channel CHECK ((channel = ANY (ARRAY['imboy'::text, 'wechat'::text, 'phone'::text, 'email'::text, 'other'::text]))),
    CONSTRAINT ck_eci_subject_hmac CHECK ((subject_hmac ~ '^[0-9a-f]{64}$'::text))
);


--
-- Name: TABLE enterprise_contact_identity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_contact_identity IS '客户渠道标识：只存 channel、组织域 HMAC subject 与必要掩码，不存可字典反查的裸哈希与明文';


--
-- Name: COLUMN enterprise_contact_identity.subject_hmac; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact_identity.subject_hmac IS '组织域 HMAC-SHA256 的 64 位小写 hex；同一 Org 内 (channel, subject_hmac) 唯一，用于幂等去重';


--
-- Name: COLUMN enterprise_contact_identity.subject_mask; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_contact_identity.subject_mask IS '展示用掩码（如 wx***1），不得还原为完整 subject';


--
-- Name: enterprise_conversation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_conversation (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    business_identity_id bigint,
    status text DEFAULT 'active'::text NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    notice_version text,
    consent_at timestamp with time zone,
    consent_subject text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    consent_evidence_kind text,
    CONSTRAINT ck_ec_consent CHECK (((consent_at IS NULL) OR (notice_version IS NOT NULL))),
    CONSTRAINT ck_ec_consent_evidence_kind CHECK ((((consent_at IS NULL) AND (consent_evidence_kind IS NULL)) OR ((consent_at IS NOT NULL) AND (consent_evidence_kind IS NOT NULL) AND (consent_evidence_kind = 'synthetic'::text)))),
    CONSTRAINT ck_ec_status CHECK ((status = ANY (ARRAY['active'::text, 'closed'::text]))),
    CONSTRAINT ck_ec_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_conversation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_conversation IS '企业会话（归 Org）：显式绑定同 Org 的 Workspace，并关联 client 与当前经办 identity；handover 只改当前 identity，不改历史 message';


--
-- Name: COLUMN enterprise_conversation.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_conversation.workspace_id IS '所属 Workspace；V1 由服务端解析默认 Workspace，API 不接受调用方任意指定/切换；跨 Org 由复合 FK 拒绝';


--
-- Name: COLUMN enterprise_conversation.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_conversation.status IS '状态: active 进行中 | closed 已关闭（关闭不删除内容）';


--
-- Name: COLUMN enterprise_conversation.notice_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_conversation.notice_version IS '首次会话的告知文本版本；未同意不得持久化内容（真实告知文本由人工 Gate 决定）';


--
-- Name: COLUMN enterprise_conversation.consent_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_conversation.consent_at IS '同意时间；非空时 notice_version 必须非空';


--
-- Name: COLUMN enterprise_conversation.consent_subject; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_conversation.consent_subject IS '同意主体标识（合成/受控值；不得存放真实客户标识）';


--
-- Name: COLUMN enterprise_conversation.consent_evidence_kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_conversation.consent_evidence_kind IS '同意证据类别：可空、无 DEFAULT。无 consent（consent_at IS NULL）时必须 NULL；有 consent 时恰为 ''synthetic''（V1 本地状态机可接受的合成证据）；''real''/''verified_real''/其他取值一律被 ck_ec_consent_evidence_kind 拒绝';


--
-- Name: CONSTRAINT ck_ec_consent_evidence_kind ON enterprise_conversation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT ck_ec_consent_evidence_kind ON public.enterprise_conversation IS '无 consent 时 evidence kind 必须 NULL；有 consent 时 evidence kind 必须非空且恰为 synthetic';


--
-- Name: enterprise_external_identity; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_external_identity (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    external_user_id text NOT NULL,
    user_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_eei_external_nonempty CHECK (((external_user_id <> ''::text) AND (length(external_user_id) <= 256))),
    CONSTRAINT ck_eei_status CHECK ((status = ANY (ARRAY['active'::text, 'removed'::text])))
);


--
-- Name: TABLE enterprise_external_identity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_external_identity IS '外部身份映射（客户 OA 的 external_user_id <-> IMBoy active Human member；sender_user_id 的唯一授权来源）';


--
-- Name: COLUMN enterprise_external_identity.external_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_external_identity.external_user_id IS '客户 OA 侧员工标识（同 Application 内唯一）';


--
-- Name: COLUMN enterprise_external_identity.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_external_identity.user_id IS '映射目标：必须是同 Org active Human member（FK 保证在册 + 触发器校验 member active / account_type=0 / user.status=1）';


--
-- Name: COLUMN enterprise_external_identity.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_external_identity.status IS '状态: active 生效 | removed 已解除（重绑走 upsert 覆盖，不自动恢复）';


--
-- Name: enterprise_group_origin; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_group_origin (
    group_id bigint NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    archived_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone,
    CONSTRAINT ck_ego_archived_consistency CHECK (((status = 'archived'::text) = (archived_at IS NOT NULL))),
    CONSTRAINT ck_ego_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text])))
);


--
-- Name: TABLE enterprise_group_origin; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_group_origin IS '企业群的 Application 归属（Application membership）：哪一份 Application 在哪个 Org/Workspace 建立该企业群；行禁止物理删除，归档走 status=archived（单向）';


--
-- Name: COLUMN enterprise_group_origin.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_group_origin.status IS '生命周期: active | archived（archived 单向不可回退）';


--
-- Name: COLUMN enterprise_group_origin.archived_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_group_origin.archived_at IS '归档时间（与 status=archived 一致性由 CHECK 钉死）';


--
-- Name: enterprise_internal_idempotency; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_internal_idempotency (
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    idempotency_key text NOT NULL,
    request_digest text NOT NULL,
    resource_type text NOT NULL,
    resource_id bigint,
    response_code integer,
    expires_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    response_body text,
    CONSTRAINT ck_eii_completion CHECK ((((response_code IS NULL) = (response_body IS NULL)) AND ((response_code IS NULL) OR ((response_code >= 100) AND (response_code <= 599))) AND ((response_body IS NULL) OR ((response_body)::jsonb IS NOT NULL)))),
    CONSTRAINT ck_eii_key_nonempty CHECK (((idempotency_key <> ''::text) AND (length(idempotency_key) <= 256))),
    CONSTRAINT ck_eii_request_digest CHECK ((length(request_digest) = 64)),
    CONSTRAINT ck_eii_resource_type CHECK (((resource_type <> ''::text) AND (length(resource_type) <= 64)))
);


--
-- Name: TABLE enterprise_internal_idempotency; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_internal_idempotency IS '/api/internal/v1 幂等记录（V2.1 §11：同 key 同 payload TTL 内精确重放 status+body+Idempotent-Replayed 头；同 key 异 payload 409 idempotency_conflict；过期行同事务原子重置）';


--
-- Name: COLUMN enterprise_internal_idempotency.request_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_internal_idempotency.request_digest IS '请求体 SHA-256 hex（64 字符）；重放比对值';


--
-- Name: COLUMN enterprise_internal_idempotency.resource_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_internal_idempotency.resource_type IS '幂等资源类型（如 message.direct / group.member）';


--
-- Name: COLUMN enterprise_internal_idempotency.resource_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_internal_idempotency.resource_id IS '已创建资源 TSID（claim 前可空）';


--
-- Name: COLUMN enterprise_internal_idempotency.response_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_internal_idempotency.response_code IS '已存储响应码（claim 前可空；100..599，与 response_body 成对出现）';


--
-- Name: COLUMN enterprise_internal_idempotency.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_internal_idempotency.expires_at IS '幂等窗口截止；过期后同 key 视为新请求';


--
-- Name: COLUMN enterprise_internal_idempotency.response_body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_internal_idempotency.response_body IS '首次响应完整 JSON 文本快照（V2.1 §11：replay 字节精确重放 status+body；text 保序，与 response_code 同事务回填、同空/同非空）';


--
-- Name: enterprise_message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_message (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    conversation_id bigint NOT NULL,
    sender_type text NOT NULL,
    sender_contact_id bigint,
    sender_business_identity_id bigint,
    actor_user_id bigint,
    client_msg_id text NOT NULL,
    body_cipher text,
    key_version integer,
    aad_hash text,
    content_hash text,
    policy_id bigint,
    policy_version integer,
    retention_days integer NOT NULL,
    retain_until timestamp with time zone NOT NULL,
    visibility text DEFAULT 'visible'::text NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_em_inbound_no_actor CHECK (((sender_type <> 'contact'::text) OR (actor_user_id IS NULL))),
    CONSTRAINT ck_em_retention_days CHECK ((retention_days > 0)),
    CONSTRAINT ck_em_sender_type CHECK ((sender_type = ANY (ARRAY['contact'::text, 'business_identity'::text]))),
    CONSTRAINT ck_em_sender_xor CHECK ((((sender_type = 'contact'::text) AND (sender_contact_id IS NOT NULL) AND (sender_business_identity_id IS NULL)) OR ((sender_type = 'business_identity'::text) AND (sender_contact_id IS NULL) AND (sender_business_identity_id IS NOT NULL)))),
    CONSTRAINT ck_em_version CHECK ((version >= 1)),
    CONSTRAINT ck_em_visibility CHECK ((visibility = ANY (ARRAY['visible'::text, 'hidden'::text])))
);


--
-- Name: TABLE enterprise_message; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_message IS '企业消息（canonical 真源，归 Org/Workspace/Conversation）：不入个人 msg_c2c / ACK 清理链；hide/撤回只追加 tombstone 与 audit 并改 visibility';


--
-- Name: COLUMN enterprise_message.sender_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.sender_type IS '发送方类型: contact 客户入站 | business_identity 员工出站；与两个 sender FK 构成 XOR';


--
-- Name: COLUMN enterprise_message.sender_contact_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.sender_contact_id IS '客户入站时的 contact（复合 FK 保证同 Org）；出站必须为 NULL';


--
-- Name: COLUMN enterprise_message.sender_business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.sender_business_identity_id IS '员工出站的 business identity（复合 FK 保证同 Org）；入站必须为 NULL';


--
-- Name: COLUMN enterprise_message.actor_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.actor_user_id IS '执行该次发送的 user（仅审计/展示，不参与资源 owner 判定）；user 删除后置 NULL，因此「出站必须有 actor」由 BEFORE INSERT 触发器强制而非 CHECK';


--
-- Name: COLUMN enterprise_message.key_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.key_version IS 'body_cipher 对应的企业托管密钥版本；AAD 至少绑定 OrgId/WorkspaceId/ConversationId/MessageId';


--
-- Name: COLUMN enterprise_message.aad_hash; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.aad_hash IS 'AAD 绑定摘要（合成/哈希值），用于证明 AAD 不符时 fail-closed';


--
-- Name: COLUMN enterprise_message.policy_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.policy_id IS '接受时固化的 retention policy 版本 ID（快照，不随未来策略回填）';


--
-- Name: COLUMN enterprise_message.retain_until; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.retain_until IS '保留截止时间快照；只能后移（延长），前移由触发器拒绝；到期且无 active hold 才允许 bounded purge';


--
-- Name: COLUMN enterprise_message.visibility; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message.visibility IS '可见性: visible 可见 | hidden 客户端隐藏/撤回 tombstone（不改变行存在性）';


--
-- Name: enterprise_message_delivery; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_message_delivery (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    message_id bigint NOT NULL,
    recipient_ref text NOT NULL,
    device_id text,
    status text DEFAULT 'pending'::text NOT NULL,
    acked_at timestamp with time zone,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_emd_recipient_ref CHECK ((recipient_ref ~ '^(contact|identity):[0-9]+$'::text)),
    CONSTRAINT ck_emd_status CHECK ((status = ANY (ARRAY['pending'::text, 'delivered'::text, 'failed'::text]))),
    CONSTRAINT ck_emd_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_message_delivery; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_message_delivery IS '客户端投递 ACK（归 Org/Workspace/Message）：只拥有投递状态，可独立压缩/清理，不得 DELETE/UPDATE canonical message';


--
-- Name: COLUMN enterprise_message_delivery.recipient_ref; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message_delivery.recipient_ref IS '接收方引用: contact:<id> 或 identity:<id>（不存 user 级 owner）';


--
-- Name: COLUMN enterprise_message_delivery.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message_delivery.status IS '投递状态: pending 待投递 | delivered 已投递 | failed 投递失败';


--
-- Name: COLUMN enterprise_message_delivery.acked_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message_delivery.acked_at IS 'ACK 时间（幂等键见 uq_emd_org_message_recipient_device）';


--
-- Name: enterprise_message_origin; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_message_origin (
    id bigint NOT NULL,
    conversation_kind text NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    sender_kind text NOT NULL,
    sender_user_id bigint,
    non_e2ee boolean DEFAULT true NOT NULL,
    msg_row_id bigint NOT NULL,
    msg_id text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_emo_conversation CHECK ((conversation_kind = ANY (ARRAY['direct'::text, 'group'::text]))),
    CONSTRAINT ck_emo_human_sender CHECK (((sender_kind = 'human'::text) = (sender_user_id IS NOT NULL))),
    CONSTRAINT ck_emo_msg_id CHECK ((msg_id <> ''::text)),
    CONSTRAINT ck_emo_msg_row CHECK ((msg_row_id > 0)),
    CONSTRAINT ck_emo_non_e2ee CHECK (non_e2ee),
    CONSTRAINT ck_emo_sender_kind CHECK ((sender_kind = ANY (ARRAY['application'::text, 'human'::text]))),
    CONSTRAINT ck_emo_sender_positive CHECK (((sender_user_id IS NULL) OR (sender_user_id > 0)))
);


--
-- Name: TABLE enterprise_message_origin; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_message_origin IS '企业托管消息 origin 账本（plan-full §3.1）：application_id 永不 NULL（真实 Application origin），sender_kind=human 时 sender_user_id 必填（Human 痕迹与 Application 痕迹同时存在，二者缺一即 23514）；non_e2ee 恒真；行禁止物理删除';


--
-- Name: COLUMN enterprise_message_origin.application_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message_origin.application_id IS '真实调用方 Application（审计 actor）；不允许只留 Human 痕迹';


--
-- Name: COLUMN enterprise_message_origin.non_e2ee; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_message_origin.non_e2ee IS '企业托管消息固定非 E2EE（CHECK 恒真，不可写入 false）';


--
-- Name: enterprise_note; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_note (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    contact_id bigint NOT NULL,
    business_identity_id bigint,
    actor_user_id bigint,
    body_cipher text,
    body_key_version integer,
    status text DEFAULT 'active'::text NOT NULL,
    deleted_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_enterprise_note_body_key CHECK (((body_cipher IS NULL) OR (body_key_version IS NOT NULL))),
    CONSTRAINT ck_enterprise_note_deleted_at CHECK ((((status = 'active'::text) AND (deleted_at IS NULL)) OR ((status = 'deleted'::text) AND (deleted_at IS NOT NULL)))),
    CONSTRAINT ck_enterprise_note_status CHECK ((status = ANY (ARRAY['active'::text, 'deleted'::text])))
);


--
-- Name: TABLE enterprise_note; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_note IS '客户跟进记录（企业托管密文正文）：只允许软删除，删除必须同时写 status=deleted 与 deleted_at（审计保留）';


--
-- Name: COLUMN enterprise_note.actor_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_note.actor_user_id IS '写该记录的 user（审计）；user 删除后置 NULL，不级联企业数据';


--
-- Name: COLUMN enterprise_note.body_cipher; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_note.body_cipher IS '正文企业托管密文；明文不入库，缺 body_key_version 时一律拒绝';


--
-- Name: COLUMN enterprise_note.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_note.status IS '状态: active 有效 | deleted 已软删除（deleted_at 必须非空，禁止物理 DELETE 语义）';


--
-- Name: enterprise_oa_sso_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_oa_sso_code (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    application_id bigint NOT NULL,
    user_id bigint NOT NULL,
    code_digest text NOT NULL,
    redirect_uri text NOT NULL,
    nonce_digest text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    consumed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_eosc_code_digest CHECK ((length(code_digest) = 64)),
    CONSTRAINT ck_eosc_consumed_after_created CHECK (((consumed_at IS NULL) OR (consumed_at >= created_at))),
    CONSTRAINT ck_eosc_nonce_digest CHECK ((length(nonce_digest) = 64)),
    CONSTRAINT ck_eosc_redirect_https CHECK ((redirect_uri ~~ 'https://%'::text))
);


--
-- Name: TABLE enterprise_oa_sso_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_oa_sso_code IS 'OA 一次性 SSO code（60 秒；只存 code_digest；exchange 原子单次消费，重放拒绝）';


--
-- Name: COLUMN enterprise_oa_sso_code.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_oa_sso_code.user_id IS '发起 SSO 的 Human（/api/v1/oa/sso/code 以 Human JWT 签发）';


--
-- Name: COLUMN enterprise_oa_sso_code.code_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_oa_sso_code.code_digest IS 'opaque code 的 SHA-256 hex（64 字符）；全局唯一即消费定位键';


--
-- Name: COLUMN enterprise_oa_sso_code.redirect_uri; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_oa_sso_code.redirect_uri IS '绑定的客户 exact redirect URI（exchange 必须匹配）';


--
-- Name: COLUMN enterprise_oa_sso_code.nonce_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_oa_sso_code.nonce_digest IS 'state/nonce 的 SHA-256 hex（防重放绑定）';


--
-- Name: COLUMN enterprise_oa_sso_code.consumed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_oa_sso_code.consumed_at IS '消费时间（NULL=未消费；UPDATE ... WHERE consumed_at IS NULL 实现单次消费 CAS）';


--
-- Name: enterprise_offboarding_case; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_offboarding_case (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    leaver_user_id bigint,
    successor_user_id bigint,
    status text DEFAULT 'draft'::text NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    item_total integer DEFAULT 0 NOT NULL,
    item_success integer DEFAULT 0 NOT NULL,
    item_failed integer DEFAULT 0 NOT NULL,
    created_by_user_id bigint,
    reason text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    completed_at timestamp with time zone,
    CONSTRAINT ck_eoc_counts CHECK (((item_total >= 0) AND (item_success >= 0) AND (item_failed >= 0))),
    CONSTRAINT ck_eoc_status CHECK ((status = ANY (ARRAY['draft'::text, 'frozen'::text, 'transferring'::text, 'verifying'::text, 'completed'::text, 'failed'::text]))),
    CONSTRAINT ck_eoc_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_offboarding_case; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_offboarding_case IS '离职交接 case（状态机 draft->frozen->transferring->verifying->completed，或 ->failed）：同 Org+leaver 同时最多一个未完成 case';


--
-- Name: COLUMN enterprise_offboarding_case.leaver_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_case.leaver_user_id IS '离职人（user 引用，SET NULL）：交接期间锁定 assignment version 逐项 rebind';


--
-- Name: COLUMN enterprise_offboarding_case.successor_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_case.successor_user_id IS '承接人（user 引用，SET NULL）';


--
-- Name: COLUMN enterprise_offboarding_case.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_case.status IS '状态: draft | frozen | transferring | verifying | completed | failed';


--
-- Name: COLUMN enterprise_offboarding_case.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_case.version IS 'CAS 版本：执行时锁定，避免并发 rebind 重复转移';


--
-- Name: COLUMN enterprise_offboarding_case.item_total; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_case.item_total IS '交接项总数（含 success + failed + pending）';


--
-- Name: enterprise_offboarding_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_offboarding_item (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    case_id bigint NOT NULL,
    business_identity_id bigint NOT NULL,
    function_key text NOT NULL,
    from_user_id bigint,
    to_user_id bigint,
    status text DEFAULT 'pending'::text NOT NULL,
    idempotency_key text NOT NULL,
    attempt integer DEFAULT 0 NOT NULL,
    failure_reason text,
    audit_event_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_eoi_attempt CHECK ((attempt >= 0)),
    CONSTRAINT ck_eoi_failure_reason CHECK (((failure_reason IS NULL) OR (status = 'failed'::text))),
    CONSTRAINT ck_eoi_idempotency_key CHECK ((idempotency_key <> ''::text)),
    CONSTRAINT ck_eoi_status CHECK ((status = ANY (ARRAY['pending'::text, 'success'::text, 'failed'::text]))),
    CONSTRAINT ck_eoi_success_requires_to_user CHECK (((status <> 'success'::text) OR (to_user_id IS NOT NULL)))
);


--
-- Name: TABLE enterprise_offboarding_item; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_offboarding_item IS '离职交接项（case 下每个 business identity 一项）：资源 owner/ID 不变，只把 identity 从 A rebind 到 B；成功项不可重复写审计';


--
-- Name: COLUMN enterprise_offboarding_item.business_identity_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_item.business_identity_id IS '被交接的稳定业务身份；与 function_key 由复合 FK 保持一致';


--
-- Name: COLUMN enterprise_offboarding_item.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_item.status IS '状态: pending 待处理 | success 已成功 | failed 失败（保留 reason 可重试）';


--
-- Name: COLUMN enterprise_offboarding_item.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_item.idempotency_key IS '幂等键：同一 Org 内唯一，重放不增行';


--
-- Name: COLUMN enterprise_offboarding_item.attempt; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_item.attempt IS '重试次数（>=0）';


--
-- Name: COLUMN enterprise_offboarding_item.failure_reason; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_item.failure_reason IS '失败原因；只允许在 status=failed 时非空';


--
-- Name: COLUMN enterprise_offboarding_item.audit_event_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_offboarding_item.audit_event_id IS '关联的企业审计事件 ID（append-only 真源在 enterprise_audit_event）';


--
-- Name: enterprise_retention_hold; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_retention_hold (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    scope_type text NOT NULL,
    scope_conversation_id bigint,
    scope_message_id bigint,
    reason_code text NOT NULL,
    actor_user_id bigint,
    audit_event_id bigint,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    released_at timestamp with time zone,
    released_by_user_id bigint,
    active_scope_message_id bigint GENERATED ALWAYS AS (
CASE
    WHEN (released_at IS NULL) THEN scope_message_id
    ELSE NULL::bigint
END) STORED,
    CONSTRAINT ck_erh_scope_shape CHECK ((((scope_type = 'workspace'::text) AND (scope_conversation_id IS NULL) AND (scope_message_id IS NULL)) OR ((scope_type = 'conversation'::text) AND (scope_conversation_id IS NOT NULL) AND (scope_message_id IS NULL)) OR ((scope_type = 'message'::text) AND (scope_message_id IS NOT NULL) AND (scope_conversation_id IS NULL)))),
    CONSTRAINT ck_erh_scope_type CHECK ((scope_type = ANY (ARRAY['conversation'::text, 'message'::text, 'workspace'::text]))),
    CONSTRAINT ck_erh_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_retention_hold; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_retention_hold IS '保留 Hold（append-only hold/release 事实）：绑定 Org/Workspace + conversation|message|workspace 作用域；active hold 优先于 retain_until，只允许延长保留';


--
-- Name: COLUMN enterprise_retention_hold.scope_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_hold.scope_type IS '作用域: conversation | message | workspace（与 scope_* 列自洽，由 ck_erh_scope_shape 强制）';


--
-- Name: COLUMN enterprise_retention_hold.scope_message_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_hold.scope_message_id IS '作用域消息 id（原 resource id，审计可追）：append-only 不可改；非 message 作用域时为 NULL。release 后该列逐字保留（不置 NULL、不随消息删除而丢失）';


--
-- Name: COLUMN enterprise_retention_hold.reason_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_hold.reason_code IS '合成/受控 reason code；真实 hold 的创建与释放属于需担责操作，本地只用 synthetic fixture';


--
-- Name: COLUMN enterprise_retention_hold.released_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_hold.released_at IS '释放时间；NULL=hold 生效中。只允许由 NULL 一次性写入，且必须同时写 released_by_user_id';


--
-- Name: COLUMN enterprise_retention_hold.released_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_hold.released_by_user_id IS '释放人；与 released_at 同步写入（释放后不可再次修改）';


--
-- Name: COLUMN enterprise_retention_hold.active_scope_message_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_hold.active_scope_message_id IS 'STORE 派生列（非调用方可写）：active（released_at IS NULL）时等于 scope_message_id，released 后为 NULL。仅用于把 fk_erh_active_message 的阻断范围收窄到 active 行——released 行因 MATCH SIMPLE 的 NULL 语义不再参与引用校验，从而既不丢历史 id、也不阻断到期 purge';


--
-- Name: enterprise_retention_policy; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.enterprise_retention_policy (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    data_class text NOT NULL,
    version integer NOT NULL,
    retention_days integer NOT NULL,
    trigger_event text,
    effective_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by_user_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_erp_data_class CHECK ((data_class = ANY (ARRAY['enterprise_message'::text, 'enterprise_asset'::text]))),
    CONSTRAINT ck_erp_retention_days CHECK ((retention_days > 0)),
    CONSTRAINT ck_erp_version CHECK ((version >= 1))
);


--
-- Name: TABLE enterprise_retention_policy; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.enterprise_retention_policy IS 'Workspace 保留策略（不可变版本快照）：以 (Org,Workspace,data_class,version) 标识；旧版本永久保留，只允许追加新版本且不得缩短期限（EB-D12）';


--
-- Name: COLUMN enterprise_retention_policy.data_class; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_policy.data_class IS '数据类别: enterprise_message | enterprise_asset（V1 本地 fixture 用 enterprise_message / retention_days=1095）';


--
-- Name: COLUMN enterprise_retention_policy.retention_days; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_policy.retention_days IS '保留天数（>0）；新版本不得小于同 Org/Workspace/data_class 的既有版本，否则 23514';


--
-- Name: COLUMN enterprise_retention_policy.trigger_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_policy.trigger_event IS '保留起算事件（如 message.accept）；生产期限/起算事件/法律依据由 owner/legal 人工确认';


--
-- Name: COLUMN enterprise_retention_policy.created_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.enterprise_retention_policy.created_by_user_id IS '创建人（审计快照）；user 删除后置 NULL';


--
-- Name: feedback; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.feedback (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id character varying(40) NOT NULL,
    client_operating_system character varying(80),
    client_operating_system_vsn character varying(680),
    feedback_md5 character varying(40),
    app_vsn character varying(40),
    type character varying(40),
    rating character varying(40),
    contact_detail character varying(200),
    body text NOT NULL,
    attach jsonb,
    reply_count integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_feedback_rating CHECK (((rating IS NULL) OR ((rating)::text = ANY (ARRAY[('bad'::character varying)::text, ('neutral'::character varying)::text, ('good'::character varying)::text])))),
    CONSTRAINT chk_feedback_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1, 2, 3]))),
    CONSTRAINT chk_feedback_type CHECK (((type IS NULL) OR ((type)::text = ANY (ARRAY[('bugReport'::character varying)::text, ('featureRequest'::character varying)::text]))))
);


--
-- Name: TABLE feedback; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.feedback IS '用户反馈表';


--
-- Name: COLUMN feedback.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.id IS '主键 自增长ID 反馈ID';


--
-- Name: COLUMN feedback.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.user_id IS '反馈用户ID';


--
-- Name: COLUMN feedback.device_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.device_id IS '设备ID web设备留空';


--
-- Name: COLUMN feedback.client_operating_system; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.client_operating_system IS '设备类型 web ios android macos windows';


--
-- Name: COLUMN feedback.client_operating_system_vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.client_operating_system_vsn IS '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}';


--
-- Name: COLUMN feedback.feedback_md5; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.feedback_md5 IS '反馈唯一标识 md5(user_id + device_id + app_vsn + type + body)';


--
-- Name: COLUMN feedback.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.type IS '反馈类型 bugReport featureRequest';


--
-- Name: COLUMN feedback.rating; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.rating IS '反馈评级 bad neutral good';


--
-- Name: COLUMN feedback.contact_detail; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.contact_detail IS '联系方式';


--
-- Name: COLUMN feedback.body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.body IS '反馈内容';


--
-- Name: COLUMN feedback.attach; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.attach IS '反馈内容附件';


--
-- Name: COLUMN feedback.reply_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.reply_count IS '回复数量，回复的回复也计算在内';


--
-- Name: COLUMN feedback.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.status IS '状态: -1 删除  0 禁用  1 启用 (待回复）  2 已回复  3 已完结（不允许回复了）';


--
-- Name: COLUMN feedback.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN feedback.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: feedback_reply; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.feedback_reply (
    id bigint NOT NULL,
    feedback_id bigint NOT NULL,
    feedback_reply_pid bigint DEFAULT 0,
    replier_user_id bigint NOT NULL,
    replier_name character varying(40),
    body text NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE feedback_reply; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.feedback_reply IS '用户反馈回复表';


--
-- Name: COLUMN feedback_reply.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.id IS '主键 自增长ID 反馈回复ID';


--
-- Name: COLUMN feedback_reply.feedback_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.feedback_id IS '反馈ID';


--
-- Name: COLUMN feedback_reply.feedback_reply_pid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.feedback_reply_pid IS '反馈回复父ID，大于0，是对回复的回复';


--
-- Name: COLUMN feedback_reply.replier_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.replier_user_id IS '回复人用户ID';


--
-- Name: COLUMN feedback_reply.replier_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.replier_name IS '回复人称呼';


--
-- Name: COLUMN feedback_reply.body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.body IS '回复内容';


--
-- Name: COLUMN feedback_reply.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN feedback_reply.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN feedback_reply.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.feedback_reply.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: fts_channel; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fts_channel (
    channel_id bigint NOT NULL,
    token tsvector,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: fts_group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fts_group (
    group_id bigint NOT NULL,
    token tsvector,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: fts_user; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fts_user (
    user_id bigint NOT NULL,
    allow_search smallint DEFAULT 2 NOT NULL,
    token tsvector,
    CONSTRAINT chk_fts_user_allow_search CHECK ((allow_search = ANY (ARRAY[1, 2])))
);


--
-- Name: TABLE fts_user; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.fts_user IS '用户全文索引矢量信息表';


--
-- Name: COLUMN fts_user.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.fts_user.user_id IS '用户唯一ID';


--
-- Name: COLUMN fts_user.allow_search; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.fts_user.allow_search IS '用户允许被搜索 1 是  2 否';


--
-- Name: COLUMN fts_user.token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.fts_user.token IS '搜索矢量信息';


--
-- Name: geo_people_nearby; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.geo_people_nearby (
    user_id bigint NOT NULL,
    location public.geometry,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE geo_people_nearby; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.geo_people_nearby IS '附近的人';


--
-- Name: group; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public."group" (
    id bigint NOT NULL,
    type smallint DEFAULT 1,
    join_limit smallint DEFAULT 2,
    content_limit smallint DEFAULT 2,
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
    e2ee_mode smallint DEFAULT 0 NOT NULL,
    category_id bigint,
    is_featured boolean DEFAULT false,
    scope text DEFAULT 'personal'::text NOT NULL,
    workspace_id bigint,
    CONSTRAINT chk_group_creator_uid CHECK ((creator_uid > 0)),
    CONSTRAINT chk_group_member_count CHECK ((member_count >= 0)),
    CONSTRAINT chk_group_member_max CHECK ((member_max > 0)),
    CONSTRAINT chk_group_owner_uid CHECK ((owner_uid > 0)),
    CONSTRAINT chk_group_scope_xor CHECK ((((scope = 'personal'::text) AND (workspace_id IS NULL)) OR ((scope = 'workspace'::text) AND (workspace_id IS NOT NULL)))),
    CONSTRAINT chk_group_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--
-- Name: TABLE "group"; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public."group" IS '群组表';


--
-- Name: COLUMN "group".id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".id IS '主键 自增长ID';


--
-- Name: COLUMN "group".type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".type IS '类型: 1 公开群组  2 私有群组';


--
-- Name: COLUMN "group".join_limit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".join_limit IS '加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入';


--
-- Name: COLUMN "group".content_limit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".content_limit IS '内部发布限制: 1 圈内不需审核  2 圈内需要审核  3 圈外需要审核';


--
-- Name: COLUMN "group".owner_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".owner_uid IS '群组拥有者ID';


--
-- Name: COLUMN "group".creator_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".creator_uid IS '群组创建者ID';


--
-- Name: COLUMN "group".member_max; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".member_max IS '允许最大成员数量';


--
-- Name: COLUMN "group".member_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".member_count IS '成员数量';


--
-- Name: COLUMN "group".introduction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".introduction IS '简介';


--
-- Name: COLUMN "group".avatar; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".avatar IS '群组头像';


--
-- Name: COLUMN "group".title; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".title IS '群组名称';


--
-- Name: COLUMN "group".chat_aes_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".chat_aes_key IS '群聊消息秘钥';


--
-- Name: COLUMN "group".status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN "group".updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN "group".created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN "group".e2ee_mode; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".e2ee_mode IS '群级 E2EE 模式 0=off（默认）1=required（拒收明文，单向开启）';


--
-- Name: COLUMN "group".scope; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".scope IS '归属维度: personal 个人 | workspace 工作区（I1，创建后不可变）';


--
-- Name: COLUMN "group".workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."group".workspace_id IS '所属工作区ID；scope=personal 时必须为 NULL（XOR CHECK 强制）';


--
-- Name: group_album; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_album; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_album IS '群相册表';


--
-- Name: COLUMN group_album.album_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album.album_id IS '相册唯一ID';


--
-- Name: COLUMN group_album.album_cover; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album.album_cover IS '封面图片ID (关联 group_album_photo.photo_id)';


--
-- Name: COLUMN group_album.photo_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album.photo_count IS '照片数量';


--
-- Name: group_album_photo; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_album_photo; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_album_photo IS '群相册图片表';


--
-- Name: COLUMN group_album_photo.photo_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album_photo.photo_id IS '图片唯一ID';


--
-- Name: COLUMN group_album_photo.photo_url; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album_photo.photo_url IS '原图URL';


--
-- Name: COLUMN group_album_photo.thumbnail_url; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album_photo.thumbnail_url IS '缩略图URL';


--
-- Name: COLUMN group_album_photo.width; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album_photo.width IS '图片宽度 (像素)';


--
-- Name: COLUMN group_album_photo.height; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_album_photo.height IS '图片高度 (像素)';


--
-- Name: group_album_photo_comment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_album_photo_comment (
    id bigint NOT NULL,
    photo_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    content text NOT NULL,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_album_photo_comment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_album_photo_comment IS '群相册图片评论表';


--
-- Name: group_album_photo_like; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_album_photo_like (
    id bigint NOT NULL,
    photo_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_album_photo_like; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_album_photo_like IS '群相册图片点赞表';


--
-- Name: group_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_category (
    id bigint NOT NULL,
    name character varying(50) NOT NULL,
    icon character varying(320),
    sort_order integer DEFAULT 0,
    status smallint DEFAULT 1,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: group_file; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_file; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_file IS '群文件共享表';


--
-- Name: COLUMN group_file.file_category; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_file.file_category IS '文件分类: document/image/video/audio/other';


--
-- Name: COLUMN group_file.file_hash; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_file.file_hash IS '文件MD5哈希，用于去重';


--
-- Name: COLUMN group_file.download_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_file.download_count IS '文件下载次数统计';


--
-- Name: COLUMN group_file.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_file.status IS '状态: 1正常 0软删除';


--
-- Name: group_log; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_log; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_log IS '群组日志表';


--
-- Name: COLUMN group_log.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.id IS '主键 自增长ID';


--
-- Name: COLUMN group_log.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.type IS '日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群';


--
-- Name: COLUMN group_log.option_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.option_uid IS '操作者用户ID（0 表示主动退出）';


--
-- Name: COLUMN group_log.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.group_id IS '群组ID';


--
-- Name: COLUMN group_log.body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.body IS '相关操作类型的json字符串数据';


--
-- Name: COLUMN group_log.remark; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.remark IS '备注';


--
-- Name: COLUMN group_log.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_log.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: group_member; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_member IS '群组成员表';


--
-- Name: COLUMN group_member.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.id IS '主键 自增长ID';


--
-- Name: COLUMN group_member.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.group_id IS '群组ID';


--
-- Name: COLUMN group_member.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.user_id IS '群组成员用户ID';


--
-- Name: COLUMN group_member.invite_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.invite_code IS '入群邀请码';


--
-- Name: COLUMN group_member.alias; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.alias IS '群内别名';


--
-- Name: COLUMN group_member.description; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.description IS '群内描述';


--
-- Name: COLUMN group_member.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.role IS '角色: 0 未定义 1 普通成员 2 嘉宾 3 管理员 4 群主 5 副群主';


--
-- Name: COLUMN group_member.is_join; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.is_join IS '是否加入的群： 1 是 0 否 （0 是群创建者或者拥有者 1 是 成员 嘉宾 管理员等）';


--
-- Name: COLUMN group_member.join_mode; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.join_mode IS '进群方式 :  invite_[uid]_[nickname] <a>leeyi</a>邀请进群  scan_qr_code 扫描二维码加入 face2face_join 面对面建群';


--
-- Name: COLUMN group_member.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.status IS '状态: -1 删除  0 禁用  1 启用 ';


--
-- Name: COLUMN group_member.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN group_member.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN group_member.remark; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.remark IS '群备注，仅该成员自己可见';


--
-- Name: COLUMN group_member.category_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_member.category_id IS '群组分类ID，0表示未分类';


--
-- Name: group_member_generation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_member_generation (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    generation_no integer DEFAULT 1 NOT NULL,
    start_seq bigint NOT NULL,
    end_seq bigint,
    close_reason text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ck_gmg_interval CHECK (((end_seq IS NULL) OR (end_seq >= (start_seq - 1))))
);


--
-- Name: TABLE group_member_generation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_member_generation IS 'E2EE-2026-012 join boundary; migration policy = M1 (legacy active members grandfathered start_seq=1)';


--
-- Name: group_member_generation_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.group_member_generation ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.group_member_generation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: group_notice; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_notice; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_notice IS '群组公告记录表';


--
-- Name: COLUMN group_notice.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.id IS '主键 自增长ID';


--
-- Name: COLUMN group_notice.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.group_id IS '群组ID';


--
-- Name: COLUMN group_notice.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.user_id IS '创建用户ID';


--
-- Name: COLUMN group_notice.body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.body IS '公告类容';


--
-- Name: COLUMN group_notice.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.status IS '状态 0 待发布  1 已发布 2 取消发布';


--
-- Name: COLUMN group_notice.expired_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.expired_at IS '公告有效期截止时间';


--
-- Name: COLUMN group_notice.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.updated_at IS '最后更新时间';


--
-- Name: COLUMN group_notice.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN group_notice.pinned; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.pinned IS '是否置顶';


--
-- Name: COLUMN group_notice.deleted_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.deleted_at IS '软删除时间戳';


--
-- Name: COLUMN group_notice.read_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.read_count IS '已读用户数量';


--
-- Name: COLUMN group_notice.title; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_notice.title IS '公告标题';


--
-- Name: group_random_code; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_random_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_random_code IS '存储面对面建群的随机码和关联的群 ID';


--
-- Name: COLUMN group_random_code.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_random_code.id IS '主键 自增长ID';


--
-- Name: COLUMN group_random_code.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_random_code.group_id IS '群组ID';


--
-- Name: COLUMN group_random_code.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_random_code.user_id IS '创建用户ID';


--
-- Name: COLUMN group_random_code.code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_random_code.code IS '随机码';


--
-- Name: COLUMN group_random_code.validity_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_random_code.validity_at IS '有效期截止时间，NULL 表示永久有效';


--
-- Name: COLUMN group_random_code.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_random_code.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: group_schedule; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_schedule; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_schedule IS '群组日程表';


--
-- Name: COLUMN group_schedule.remind_before; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_schedule.remind_before IS '提前多少分钟提醒';


--
-- Name: COLUMN group_schedule.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_schedule.status IS '1待开始 2进行中 3已结束 4已取消';


--
-- Name: group_schedule_participant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_schedule_participant (
    id bigint NOT NULL,
    schedule_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    status smallint DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_schedule_participant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_schedule_participant IS '群组日程参与人表';


--
-- Name: COLUMN group_schedule_participant.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_schedule_participant.status IS '0待确认 1参加 2不参加';


--
-- Name: group_schedule_remind; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_schedule_remind (
    id bigint NOT NULL,
    schedule_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    remind_at timestamp with time zone NOT NULL,
    is_sent boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_schedule_remind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_schedule_remind IS '群组日程提醒表';


--
-- Name: group_tag; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_tag (
    id bigint NOT NULL,
    group_id bigint NOT NULL,
    tag_name character varying(50) NOT NULL,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_tag; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_tag IS '群组标签表';


--
-- Name: COLUMN group_tag.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_tag.id IS '主键ID';


--
-- Name: COLUMN group_tag.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_tag.group_id IS '群组ID';


--
-- Name: COLUMN group_tag.tag_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_tag.tag_name IS '标签名称';


--
-- Name: COLUMN group_tag.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_tag.created_by IS '创建者用户ID';


--
-- Name: COLUMN group_tag.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_tag.created_at IS '创建时间';


--
-- Name: group_task; Type: TABLE; Schema: public; Owner: -
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
    deleted_at timestamp with time zone,
    idempotency_key character varying(128),
    request_digest character varying(128)
);


--
-- Name: TABLE group_task; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_task IS '群作业表';


--
-- Name: COLUMN group_task.task_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task.task_id IS '作业唯一标识（HashID编码）';


--
-- Name: COLUMN group_task.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task.status IS '状态: 1待完成 2进行中 3已截止';


--
-- Name: COLUMN group_task.deleted_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task.deleted_at IS '软删除时间戳，NULL 表示未删除';


--
-- Name: COLUMN group_task.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task.idempotency_key IS '教学作业发布幂等键（可空=普通群作业/内部写入；同 (creator_id,group_id,idempotency_key) 唯一，重放返回原 task+assignments 集合）';


--
-- Name: COLUMN group_task.request_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task.request_digest IS '请求载荷摘要（sha256 hex；同 key 不同 digest 由应用层判 5460 冲突，DB 不参与唯一性）';


--
-- Name: group_task_assignment; Type: TABLE; Schema: public; Owner: -
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
    learner_id bigint,
    submitted_by bigint,
    CONSTRAINT chk_task_assignment_reviewed CHECK (((status <> 3) OR (reviewed_by IS NOT NULL)))
);


--
-- Name: TABLE group_task_assignment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_task_assignment IS '群作业分配表';


--
-- Name: COLUMN group_task_assignment.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task_assignment.status IS '状态: 0待完成 1进行中 2已提交 3已批改';


--
-- Name: COLUMN group_task_assignment.learner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task_assignment.learner_id IS '教学作业学员档案ID（NULL=普通 IMBoy 群作业，兼容旧数据；教学作业提交/查看权限真源是 guardian_learner）';


--
-- Name: COLUMN group_task_assignment.submitted_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_task_assignment.submitted_by IS '最近一次提交人（快捷字段，可空；真源在 homework_submission.submitted_by）';


--
-- Name: group_vote; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE group_vote; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_vote IS '群投票主表';


--
-- Name: COLUMN group_vote.vote_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote.vote_id IS '投票唯一标识 (UUID)';


--
-- Name: COLUMN group_vote.vote_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote.vote_type IS '投票类型: 1=单选, 2=多选';


--
-- Name: COLUMN group_vote.is_anonymous; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote.is_anonymous IS '是否匿名投票';


--
-- Name: COLUMN group_vote.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote.status IS '投票状态: 1=进行中, 2=已结束, 3=已取消';


--
-- Name: COLUMN group_vote.end_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote.end_at IS '投票截止时间';


--
-- Name: group_vote_option; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_vote_option (
    id bigint NOT NULL,
    vote_id character varying(40) NOT NULL,
    option_id character varying(40) NOT NULL,
    option_text character varying(200) NOT NULL,
    sort_order integer DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_vote_option; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_vote_option IS '群投票选项表';


--
-- Name: COLUMN group_vote_option.option_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote_option.option_id IS '选项唯一标识';


--
-- Name: COLUMN group_vote_option.sort_order; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote_option.sort_order IS '选项排序顺序';


--
-- Name: group_vote_record; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.group_vote_record (
    id bigint NOT NULL,
    vote_id character varying(40) NOT NULL,
    user_id bigint NOT NULL,
    option_ids jsonb DEFAULT '[]'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE group_vote_record; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.group_vote_record IS '群投票记录表';


--
-- Name: COLUMN group_vote_record.option_ids; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.group_vote_record.option_ids IS '用户选择的选项ID列表(JSON数组格式)';


--
-- Name: guardian_learner; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.guardian_learner (
    guardian_uid bigint NOT NULL,
    learner_id bigint NOT NULL,
    relation text DEFAULT 'guardian'::text NOT NULL,
    can_submit boolean DEFAULT true NOT NULL,
    can_view_review boolean DEFAULT true NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_guardian_learner_relation CHECK ((relation = ANY (ARRAY['guardian'::text, 'other'::text]))),
    CONSTRAINT ck_guardian_learner_status CHECK ((status = ANY (ARRAY['active'::text, 'removed'::text])))
);


--
-- Name: TABLE guardian_learner; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.guardian_learner IS '监护关系（家长替低龄学员提交/查看的权限真源；非 group_member）';


--
-- Name: COLUMN guardian_learner.relation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.guardian_learner.relation IS '关系: guardian 法定监护人 | other 其他被授权人';


--
-- Name: COLUMN guardian_learner.can_submit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.guardian_learner.can_submit IS '是否可替该学员提交作业（家长可按孩子细分）';


--
-- Name: COLUMN guardian_learner.can_view_review; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.guardian_learner.can_view_review IS '是否可查看该学员的已发布回评';


--
-- Name: homework_submission; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.homework_submission (
    id bigint NOT NULL,
    assignment_id bigint NOT NULL,
    learner_id bigint NOT NULL,
    submitted_by bigint,
    attempt_no integer NOT NULL,
    status text DEFAULT 'submitted'::text NOT NULL,
    submitted_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    idempotency_key character varying(128),
    request_digest character varying(128),
    withdrawn_at timestamp with time zone,
    withdrawn_by bigint,
    CONSTRAINT ck_homework_submission_attempt_no CHECK ((attempt_no > 0)),
    CONSTRAINT ck_homework_submission_status CHECK ((status = ANY (ARRAY['submitted'::text, 'withdrawn'::text]))),
    CONSTRAINT ck_homework_submission_withdraw_audit CHECK ((((status = 'submitted'::text) AND (withdrawn_at IS NULL) AND (withdrawn_by IS NULL)) OR ((status = 'withdrawn'::text) AND (withdrawn_at IS NOT NULL) AND (withdrawn_by IS NOT NULL) AND (submitted_at IS NOT NULL)))),
    CONSTRAINT ck_homework_submission_withdrawn_by_sentinel CHECK (((withdrawn_by IS NULL) OR (withdrawn_by >= 0)))
);


--
-- Name: TABLE homework_submission; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.homework_submission IS '作业提交（重练=新 attempt 新行，旧证据不更新不删除）';


--
-- Name: COLUMN homework_submission.learner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.learner_id IS '学员档案ID（触发器强制 == assignment.learner_id）';


--
-- Name: COLUMN homework_submission.submitted_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.submitted_by IS '提交人用户ID（can_submit=true 的监护人或学员本人；SET NULL 保留证据）';


--
-- Name: COLUMN homework_submission.attempt_no; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.attempt_no IS '第几次提交（1 起，应用层 MAX+1 分配，UNIQUE 兜底）';


--
-- Name: COLUMN homework_submission.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.status IS '状态: submitted 已提交 | withdrawn 已撤回（证据保留）';


--
-- Name: COLUMN homework_submission.idempotency_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.idempotency_key IS '客户端幂等键（可空=内部写入；同 (submitted_by,assignment_id,idempotency_key) 唯一，重放返回原行）';


--
-- Name: COLUMN homework_submission.request_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.request_digest IS '请求载荷摘要（sha256 hex；同 key 不同 digest 由应用层判 5460 冲突，DB 不参与唯一性）';


--
-- Name: COLUMN homework_submission.withdrawn_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.withdrawn_at IS '撤回时间（status=withdrawn 必填）';


--
-- Name: COLUMN homework_submission.withdrawn_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.homework_submission.withdrawn_by IS '撤回操作人（can_submit=true 监护人；status=withdrawn 必填）；裸列无FK；0=sentinel=操作人账号已删除/匿名化（应用层匿名化时改写为0，禁置NULL）';


--
-- Name: learner; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.learner (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    display_name character varying(100) NOT NULL,
    birth_year integer,
    user_id bigint,
    account_bound_at timestamp with time zone,
    account_bound_by bigint,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_learner_birth_year CHECK (((birth_year IS NULL) OR ((birth_year >= 1900) AND (birth_year <= 2100)))),
    CONSTRAINT ck_learner_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text])))
);


--
-- Name: TABLE learner; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.learner IS '学员教学档案（归属 Organization；同机构跨 Workspace 历史连续，跨机构隔离，D-09）';


--
-- Name: COLUMN learner.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.learner.organization_id IS '所属机构ID（学员档案的租户锚点）';


--
-- Name: COLUMN learner.display_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.learner.display_name IS '学员显示名（仅展示用，不作身份认证依据）';


--
-- Name: COLUMN learner.birth_year; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.learner.birth_year IS '出生年（可空，隐私最小化：不收完整生日）';


--
-- Name: COLUMN learner.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.learner.user_id IS '绑定的 IMBoy 用户ID（可空=未登录学员；真源绑定关系在 guardian_learner）';


--
-- Name: COLUMN learner.account_bound_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.learner.account_bound_at IS '账号绑定时间（审计，§6.4）';


--
-- Name: COLUMN learner.account_bound_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.learner.account_bound_by IS '账号绑定操作人（审计，§6.4）';


--
-- Name: live_room; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.live_room (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    title character varying(100) DEFAULT ''::character varying NOT NULL,
    cover character varying(255) DEFAULT ''::character varying,
    stream_key character varying(64) NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    viewer_count integer DEFAULT 0 NOT NULL,
    tag_id bigint DEFAULT 0,
    scene smallint DEFAULT 1,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_live_room_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--
-- Name: mcp_audit_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mcp_audit_log (
    id bigint NOT NULL,
    client_id bigint DEFAULT 0 NOT NULL,
    owner_uid bigint DEFAULT 0 NOT NULL,
    action character varying(24) NOT NULL,
    tool character varying(80) DEFAULT ''::character varying NOT NULL,
    actor_uid bigint DEFAULT 0 NOT NULL,
    detail jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE mcp_audit_log; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.mcp_audit_log IS 'MCP 治理审计：客户端 tool 调用行为 + 管理员审批动作';


--
-- Name: mcp_client; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mcp_client (
    client_id bigint NOT NULL,
    owner_uid bigint NOT NULL,
    name character varying(80) DEFAULT ''::character varying NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    status character varying(16) DEFAULT 'pending'::character varying NOT NULL,
    reason text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    approved_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    client_key text DEFAULT ''::text NOT NULL,
    credential_digest text DEFAULT ''::text NOT NULL,
    credential_prefix text DEFAULT ''::text NOT NULL,
    expires_at timestamp with time zone,
    last_used_at timestamp with time zone,
    disabled boolean DEFAULT false NOT NULL
);


--
-- Name: TABLE mcp_client; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.mcp_client IS 'MCP 客户端接入登记与审批状态（owner_uid 唯一）';


--
-- Name: mcp_client_grant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mcp_client_grant (
    id bigint NOT NULL,
    client_id bigint NOT NULL,
    tool_name character varying(80) NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE mcp_client_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.mcp_client_grant IS 'MCP 客户端按 tool 的授权开关（enforce=true 时生效）';


--
-- Name: moderation_action; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moderation_action (
    id bigint NOT NULL,
    case_id bigint NOT NULL,
    action character varying(32) NOT NULL,
    target_type character varying(16) DEFAULT ''::character varying NOT NULL,
    target_id bigint DEFAULT 0 NOT NULL,
    target_uid bigint DEFAULT 0 NOT NULL,
    scope jsonb DEFAULT '{}'::jsonb NOT NULL,
    reason character varying(512) DEFAULT ''::character varying NOT NULL,
    actor_id bigint DEFAULT 0 NOT NULL,
    status character varying(16) DEFAULT 'executed'::character varying NOT NULL,
    result jsonb DEFAULT '{}'::jsonb NOT NULL,
    fail_reason character varying(512) DEFAULT ''::character varying NOT NULL,
    start_at timestamp with time zone DEFAULT now() NOT NULL,
    end_at timestamp with time zone,
    reversed_at timestamp with time zone,
    reversed_by bigint DEFAULT 0 NOT NULL,
    reverse_reason character varying(512) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ck_moderation_action_action CHECK (((action)::text = ANY ((ARRAY['warning'::character varying, 'group_mute'::character varying, 'group_kick'::character varying, 'reject'::character varying, 'content_removal'::character varying, 'account_restrict'::character varying])::text[]))),
    CONSTRAINT ck_moderation_action_status CHECK (((status)::text = ANY ((ARRAY['executed'::character varying, 'failed'::character varying, 'reversed'::character varying, 'expired'::character varying])::text[])))
);


--
-- Name: moderation_appeal; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moderation_appeal (
    id bigint NOT NULL,
    action_id bigint NOT NULL,
    case_id bigint DEFAULT 0 NOT NULL,
    appellant_uid bigint NOT NULL,
    reason character varying(1000) DEFAULT ''::character varying NOT NULL,
    status character varying(16) DEFAULT 'pending'::character varying NOT NULL,
    reviewer_id bigint DEFAULT 0 NOT NULL,
    review_reason character varying(512) DEFAULT ''::character varying NOT NULL,
    reviewed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ck_moderation_appeal_status CHECK (((status)::text = ANY ((ARRAY['pending'::character varying, 'accepted'::character varying, 'rejected'::character varying])::text[])))
);


--
-- Name: moment_comment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moment_comment (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    user_id bigint NOT NULL,
    reply_to_uid bigint,
    content character varying(500) NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE moment_comment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moment_comment IS '朋友圈评论表';


--
-- Name: moment_like; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moment_like (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE moment_like; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moment_like IS '朋友圈点赞关系表';


--
-- Name: moment_post; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moment_post (
    id bigint NOT NULL,
    author_uid bigint NOT NULL,
    content text DEFAULT ''::text NOT NULL,
    media jsonb DEFAULT '[]'::jsonb NOT NULL,
    visibility smallint DEFAULT 1 NOT NULL,
    allow_comment boolean DEFAULT true NOT NULL,
    like_count integer DEFAULT 0 NOT NULL,
    comment_count integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    location jsonb,
    at_uids jsonb DEFAULT '[]'::jsonb NOT NULL,
    CONSTRAINT chk_moment_post_comment_count CHECK ((comment_count >= 0)),
    CONSTRAINT chk_moment_post_like_count CHECK ((like_count >= 0)),
    CONSTRAINT chk_moment_post_visibility CHECK ((visibility = ANY (ARRAY[0, 1, 2, 3, 4])))
);


--
-- Name: TABLE moment_post; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moment_post IS '朋友圈动态主表';


--
-- Name: COLUMN moment_post.author_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.author_uid IS '动态发布者用户ID';


--
-- Name: COLUMN moment_post.content; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.content IS '动态文本内容';


--
-- Name: COLUMN moment_post.media; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.media IS '媒体数组(JSONB): image/video 元数据';


--
-- Name: COLUMN moment_post.visibility; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.visibility IS '可见性: 0公开 1仅好友 2仅自己 3部分可见 4不给谁看';


--
-- Name: COLUMN moment_post.allow_comment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.allow_comment IS '是否允许评论';


--
-- Name: COLUMN moment_post.like_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.like_count IS '点赞数量缓存';


--
-- Name: COLUMN moment_post.comment_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.comment_count IS '评论数量缓存';


--
-- Name: COLUMN moment_post.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.status IS '状态: -1删除 0禁用 1正常';


--
-- Name: COLUMN moment_post.location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.location IS '所在位置 {name,lng,lat,address?}，无位置为 NULL / Location';


--
-- Name: COLUMN moment_post.at_uids; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post.at_uids IS '@提醒可见者 uid 数组（默认 []）/ Mentioned uids';


--
-- Name: moment_post_acl; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moment_post_acl (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    uid bigint NOT NULL,
    acl_type smallint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_moment_post_acl_type CHECK ((acl_type = ANY (ARRAY[1, 2])))
);


--
-- Name: TABLE moment_post_acl; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moment_post_acl IS '动态可见性ACL表(allow/deny)';


--
-- Name: COLUMN moment_post_acl.acl_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_post_acl.acl_type IS '1=allow 2=deny';


--
-- Name: moment_report; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moment_report (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    reporter_uid bigint NOT NULL,
    reason character varying(64) DEFAULT ''::character varying NOT NULL,
    description character varying(500) DEFAULT ''::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    handled_by bigint,
    handled_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_moment_report_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--
-- Name: TABLE moment_report; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moment_report IS '朋友圈举报记录表';


--
-- Name: COLUMN moment_report.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_report.status IS '0待处理 1已驳回 2已确认违规';


--
-- Name: moment_timeline; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moment_timeline (
    id bigint NOT NULL,
    recipient_uid bigint NOT NULL,
    post_id bigint NOT NULL,
    author_uid bigint NOT NULL,
    rank_at timestamp with time zone NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE moment_timeline; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moment_timeline IS '朋友圈时间线收件箱';


--
-- Name: COLUMN moment_timeline.rank_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moment_timeline.rank_at IS '时间线排序时间';


--
-- Name: moya_invite_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moya_invite_code (
    code text NOT NULL,
    group_id bigint NOT NULL,
    created_by bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    expires_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_moya_invite_code_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text])))
);


--
-- Name: TABLE moya_invite_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moya_invite_code IS '老师班级邀请码（绑班级不绑个人；一码多人可用、可撤销；家长凭码进确认页后显式选 learner 绑监护）';


--
-- Name: COLUMN moya_invite_code.code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_invite_code.code IS '10 位 Crockford Base32（CSPRNG 生成，排除 I/L/O/U 手抄混淆字符；应用层生成，码即主键）';


--
-- Name: COLUMN moya_invite_code.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_invite_code.group_id IS '绑定的班级 "group".id（JOIN 经 workspace 归属 organization）';


--
-- Name: COLUMN moya_invite_code.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_invite_code.created_by IS '生成者 "user".id（守卫：该班 active class_staff 或 org owner）';


--
-- Name: COLUMN moya_invite_code.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_invite_code.status IS 'active 有效 | revoked 已撤销（撤销后凭码访问统一按 not_found 折叠，防探测）';


--
-- Name: COLUMN moya_invite_code.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_invite_code.expires_at IS '过期时间（NULL=不过期；过期判定读取时计算，status 不翻转）';


--
-- Name: moya_subscribe_grant; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.moya_subscribe_grant (
    id bigint NOT NULL,
    uid bigint NOT NULL,
    template_id text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    consumed_at timestamp with time zone,
    consumed_by_submission bigint,
    CONSTRAINT ck_moya_subscribe_grant_status CHECK ((status = ANY (ARRAY['pending'::text, 'consumed'::text])))
);


--
-- Name: TABLE moya_subscribe_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.moya_subscribe_grant IS '订阅消息一次性授权额度（客户端 requestSubscribeMessage accept 一次记一行 pending；下发成功一行变 consumed）';


--
-- Name: COLUMN moya_subscribe_grant.uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_subscribe_grant.uid IS '授权用户（guardian_uid；openid 不入库，send 时反查 sso_identity）';


--
-- Name: COLUMN moya_subscribe_grant.template_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_subscribe_grant.template_id IS '微信一次性订阅模板 ID（配置而非常量）';


--
-- Name: COLUMN moya_subscribe_grant.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_subscribe_grant.status IS 'pending 未消费 | consumed 已消费（条件 UPDATE 防并发双发）';


--
-- Name: COLUMN moya_subscribe_grant.consumed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_subscribe_grant.consumed_at IS '消费时间（=订阅消息下发成功时间）';


--
-- Name: COLUMN moya_subscribe_grant.consumed_by_submission; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.moya_subscribe_grant.consumed_by_submission IS '消费该额度的 homework_submission.id（审计线索）';


--
-- Name: moya_subscribe_grant_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.moya_subscribe_grant_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: moya_subscribe_grant_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.moya_subscribe_grant_id_seq OWNED BY public.moya_subscribe_grant.id;


--
-- Name: msg_c2c; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_c2c (
    id bigint NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(40) NOT NULL,
    e2ee jsonb,
    payload text NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    pinned boolean DEFAULT false,
    reply_to_msg_id character varying(40),
    reply_to_from_id bigint,
    reply_snippet text,
    expire_at timestamp with time zone,
    sender_did character varying(128)
);


--
-- Name: TABLE msg_c2c; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_c2c IS '单聊消息临时存储表';


--
-- Name: COLUMN msg_c2c.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.id IS '主键 自增长ID';


--
-- Name: COLUMN msg_c2c.from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.from_id IS '消息发送人user id';


--
-- Name: COLUMN msg_c2c.to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.to_id IS '消息接收人user_id';


--
-- Name: COLUMN msg_c2c.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.msg_id IS '消息唯一标识';


--
-- Name: COLUMN msg_c2c.msg_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.msg_type IS '消息格式类型： text image audio video file 等';


--
-- Name: COLUMN msg_c2c.payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.payload IS '消息体明文json格式加密后的文本，数据结构参考文档';


--
-- Name: COLUMN msg_c2c.server_ts; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.server_ts IS '消息服务器接受毫秒时间戳';


--
-- Name: COLUMN msg_c2c.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN msg_c2c.reply_to_msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.reply_to_msg_id IS '被引用回复的消息ID';


--
-- Name: COLUMN msg_c2c.reply_to_from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.reply_to_from_id IS '被引用消息的发送者ID';


--
-- Name: COLUMN msg_c2c.reply_snippet; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.reply_snippet IS '被引用消息的摘要（前50字符）';


--
-- Name: COLUMN msg_c2c.expire_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.expire_at IS '消息自毁时间，NULL=不自毁';


--
-- Name: COLUMN msg_c2c.sender_did; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2c.sender_did IS '发送方设备ID（服务端认证态注入，客户端不可伪造）；PFv3 context binding #6，NULL=未提供';


--
-- Name: msg_c2g; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_c2g (
    id bigint NOT NULL,
    topic_id bigint DEFAULT 0 NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(40) NOT NULL,
    e2ee jsonb,
    payload jsonb NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mentions jsonb DEFAULT '[]'::jsonb,
    pinned boolean DEFAULT false,
    reply_to_msg_id character varying(40),
    reply_to_from_id bigint,
    reply_snippet text,
    expire_at timestamp with time zone,
    sender_did character varying(128)
);


--
-- Name: TABLE msg_c2g; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_c2g IS '群聊消息临时存储表';


--
-- Name: COLUMN msg_c2g.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.id IS '主键 自增长ID';


--
-- Name: COLUMN msg_c2g.from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.from_id IS '消息发送人user id';


--
-- Name: COLUMN msg_c2g.to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.to_id IS '消息接收人user_id';


--
-- Name: COLUMN msg_c2g.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.msg_id IS '消息唯一标识';


--
-- Name: COLUMN msg_c2g.msg_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.msg_type IS '消息格式类型： text image audio video file 等';


--
-- Name: COLUMN msg_c2g.payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.payload IS '消息体json格式，数据结构参考文档';


--
-- Name: COLUMN msg_c2g.server_ts; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.server_ts IS '消息服务器接受毫秒时间戳';


--
-- Name: COLUMN msg_c2g.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN msg_c2g.mentions; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.mentions IS '@提及信息，JSON数组格式，包含被@的用户ID列表，例如: [123, 456] 或 ["all"] 表示@所有人';


--
-- Name: COLUMN msg_c2g.reply_to_msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.reply_to_msg_id IS '被引用回复的消息ID';


--
-- Name: COLUMN msg_c2g.reply_to_from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.reply_to_from_id IS '被引用消息的发送者ID';


--
-- Name: COLUMN msg_c2g.reply_snippet; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.reply_snippet IS '被引用消息的摘要（前50字符）';


--
-- Name: COLUMN msg_c2g.expire_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.expire_at IS '消息自毁时间，NULL=不自毁';


--
-- Name: COLUMN msg_c2g.sender_did; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g.sender_did IS 'Server-authenticated sender device id for PFv3 offline context binding';


--
-- Name: msg_c2g_recipient_snapshot; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_c2g_recipient_snapshot (
    msg_id character varying(40) NOT NULL,
    from_id bigint NOT NULL,
    to_gid bigint NOT NULL,
    conv_seq bigint NOT NULL,
    recipient_uids bigint[] NOT NULL,
    created_at timestamp with time zone NOT NULL,
    CONSTRAINT chk_msg_c2g_recipient_snapshot_msg_id CHECK (((octet_length((msg_id)::text) >= 1) AND (octet_length((msg_id)::text) <= 40))),
    CONSTRAINT chk_msg_c2g_recipient_snapshot_positive CHECK ((0 < ALL (recipient_uids))),
    CONSTRAINT chk_msg_c2g_recipient_snapshot_shape CHECK (((array_ndims(recipient_uids) = 1) AND (array_position(recipient_uids, NULL::bigint) IS NULL))),
    CONSTRAINT chk_msg_c2g_recipient_snapshot_size CHECK (((cardinality(recipient_uids) >= 1) AND (cardinality(recipient_uids) <= 5000))),
    CONSTRAINT msg_c2g_recipient_snapshot_conv_seq_check CHECK ((conv_seq >= 1))
);


--
-- Name: TABLE msg_c2g_recipient_snapshot; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_c2g_recipient_snapshot IS 'Immutable committed C2G recipient snapshot for action authorization';


--
-- Name: msg_c2g_request_ledger; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_c2g_request_ledger (
    msg_id character varying(40) NOT NULL,
    from_id bigint NOT NULL,
    to_gid bigint NOT NULL,
    action text,
    request_hash bytea NOT NULL,
    created_at timestamp with time zone NOT NULL,
    CONSTRAINT chk_msg_c2g_request_ledger_hash CHECK ((octet_length(request_hash) = 32)),
    CONSTRAINT chk_msg_c2g_request_ledger_msg_id CHECK (((octet_length((msg_id)::text) >= 1) AND (octet_length((msg_id)::text) <= 40)))
);


--
-- Name: TABLE msg_c2g_request_ledger; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_c2g_request_ledger IS 'Durable C2G message-id and canonical request-identity ledger';


--
-- Name: msg_c2g_timeline; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_c2g_timeline (
    msg_id character varying(40) NOT NULL,
    to_uid bigint NOT NULL,
    to_gid bigint NOT NULL,
    client_ack boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    conv_seq bigint,
    CONSTRAINT chk_msg_c2g_timeline_conv_seq_positive CHECK (((conv_seq IS NULL) OR (conv_seq >= 1)))
);


--
-- Name: TABLE msg_c2g_timeline; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_c2g_timeline IS '群聊消息时间线';


--
-- Name: COLUMN msg_c2g_timeline.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g_timeline.msg_id IS '消息唯一标识';


--
-- Name: COLUMN msg_c2g_timeline.to_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g_timeline.to_uid IS '消息接收人user_id';


--
-- Name: COLUMN msg_c2g_timeline.to_gid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g_timeline.to_gid IS '消息接收群 group_id';


--
-- Name: COLUMN msg_c2g_timeline.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g_timeline.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN msg_c2g_timeline.conv_seq; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2g_timeline.conv_seq IS 'C2G persistent-accept sequence; NULL legacy rows are not eligible for offline delivery';


--
-- Name: msg_c2s; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_c2s (
    id bigint NOT NULL,
    topic_id bigint DEFAULT 0 NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(40) NOT NULL,
    e2ee jsonb,
    payload text NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE msg_c2s; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_c2s IS '机器人聊消息等存储表';


--
-- Name: COLUMN msg_c2s.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.id IS '主键 自增长ID';


--
-- Name: COLUMN msg_c2s.from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.from_id IS '消息发送人user id';


--
-- Name: COLUMN msg_c2s.to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.to_id IS '消息接收人user_id';


--
-- Name: COLUMN msg_c2s.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.msg_id IS '消息唯一标识';


--
-- Name: COLUMN msg_c2s.msg_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.msg_type IS '消息格式类型： text image audio video file 等';


--
-- Name: COLUMN msg_c2s.payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.payload IS '消息体json格式，数据结构参考文档';


--
-- Name: COLUMN msg_c2s.server_ts; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.server_ts IS '消息服务器接受毫秒时间戳';


--
-- Name: COLUMN msg_c2s.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_c2s.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: msg_delivery; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_delivery (
    msg_kind character varying(8) NOT NULL,
    msg_id character varying(40) NOT NULL,
    to_uid bigint NOT NULL,
    to_did character varying(40) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE msg_delivery; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_delivery IS '离线消息按设备送达确认标记（行存在=该设备已ACK）';


--
-- Name: msg_forward; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_forward (
    id bigint NOT NULL,
    original_msg_id character varying(40) NOT NULL,
    original_from_id bigint NOT NULL,
    original_to_id bigint NOT NULL,
    original_type character varying(10) NOT NULL,
    forward_msg_id character varying(40) NOT NULL,
    forward_from_id bigint NOT NULL,
    forward_to_id bigint NOT NULL,
    forward_type character varying(10) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_msg_forward_forward_type CHECK (((forward_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text]))),
    CONSTRAINT chk_msg_forward_original_type CHECK (((original_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text])))
);


--
-- Name: TABLE msg_forward; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_forward IS '消息转发记录表，记录消息转发的溯源关系';


--
-- Name: COLUMN msg_forward.original_msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.original_msg_id IS '原始消息ID';


--
-- Name: COLUMN msg_forward.original_from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.original_from_id IS '原始消息发送者ID';


--
-- Name: COLUMN msg_forward.original_to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.original_to_id IS '原始消息接收者ID（单聊为用户ID，群聊为群组ID）';


--
-- Name: COLUMN msg_forward.original_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.original_type IS '原始消息类型：c2c（单聊）或c2g（群聊）';


--
-- Name: COLUMN msg_forward.forward_msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.forward_msg_id IS '转发后的消息ID';


--
-- Name: COLUMN msg_forward.forward_from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.forward_from_id IS '转发操作者ID';


--
-- Name: COLUMN msg_forward.forward_to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.forward_to_id IS '转发目标ID（单聊为用户ID，群聊为群组ID）';


--
-- Name: COLUMN msg_forward.forward_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_forward.forward_type IS '转发目标类型：c2c（单聊）或c2g（群聊）';


--
-- Name: msg_mention; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_mention (
    id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    group_id bigint NOT NULL,
    mentioned_uid bigint NOT NULL,
    from_uid bigint NOT NULL,
    is_read boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE msg_mention; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_mention IS '@消息记录表，用于快速查询@我的消息';


--
-- Name: COLUMN msg_mention.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_mention.msg_id IS '消息ID';


--
-- Name: COLUMN msg_mention.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_mention.group_id IS '群组ID';


--
-- Name: COLUMN msg_mention.mentioned_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_mention.mentioned_uid IS '被@的用户ID';


--
-- Name: COLUMN msg_mention.from_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_mention.from_uid IS '@消息的发送者ID';


--
-- Name: COLUMN msg_mention.is_read; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_mention.is_read IS '是否已读';


--
-- Name: COLUMN msg_mention.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_mention.created_at IS '创建时间';


--
-- Name: msg_reaction; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_reaction (
    id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    msg_type character varying(10) NOT NULL,
    user_id bigint NOT NULL,
    emoji character varying(100) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_msg_reaction_msg_type CHECK (((msg_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text])))
);


--
-- Name: TABLE msg_reaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_reaction IS '消息表情回应表';


--
-- Name: COLUMN msg_reaction.msg_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_reaction.msg_type IS '消息类型: c2c-单聊, c2g-群聊';


--
-- Name: COLUMN msg_reaction.emoji; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_reaction.emoji IS 'emoji字符，如👍❤️😄等';


--
-- Name: msg_read; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_read (
    msg_id character varying(64) NOT NULL,
    from_uid bigint NOT NULL,
    to_uid bigint NOT NULL,
    to_did character varying(64),
    read_at timestamp with time zone DEFAULT now() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE msg_read; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_read IS '消息已读回执表';


--
-- Name: COLUMN msg_read.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_read.msg_id IS '消息唯一ID';


--
-- Name: COLUMN msg_read.from_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_read.from_uid IS '发送者用户ID (整数)';


--
-- Name: COLUMN msg_read.to_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_read.to_uid IS '接收者用户ID (整数)';


--
-- Name: COLUMN msg_read.to_did; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_read.to_did IS '接收者设备ID';


--
-- Name: COLUMN msg_read.read_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_read.read_at IS '消息已读时间（服务器时间）';


--
-- Name: COLUMN msg_read.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_read.created_at IS '记录创建时间';


--
-- Name: msg_s2c; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_s2c (
    id bigint NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint NOT NULL,
    msg_id character varying(40) NOT NULL,
    action character varying(40) NOT NULL,
    msg_type character varying(40) DEFAULT ''::character varying NOT NULL,
    e2ee jsonb,
    payload jsonb NOT NULL,
    server_ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE msg_s2c; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_s2c IS '系统消息临时存储表';


--
-- Name: COLUMN msg_s2c.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.id IS '主键 自增长ID';


--
-- Name: COLUMN msg_s2c.from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.from_id IS '消息发送人user id';


--
-- Name: COLUMN msg_s2c.to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.to_id IS '消息接收人user_id';


--
-- Name: COLUMN msg_s2c.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.msg_id IS '消息唯一标识';


--
-- Name: COLUMN msg_s2c.action; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.action IS 'S2C消息指令';


--
-- Name: COLUMN msg_s2c.msg_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.msg_type IS '消息格式类型： text image audio video file 等';


--
-- Name: COLUMN msg_s2c.payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.payload IS '消息体json格式，数据结构参考文档';


--
-- Name: COLUMN msg_s2c.server_ts; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.server_ts IS '消息服务器接受毫秒时间戳';


--
-- Name: COLUMN msg_s2c.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_s2c.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: msg_store; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_store (
    id bigint NOT NULL,
    chat_type character varying(3) NOT NULL,
    conv_key character varying(64) NOT NULL,
    conv_seq bigint NOT NULL,
    msg_id character varying(50) NOT NULL,
    msg_type character varying(50) NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint,
    group_id bigint,
    e2ee jsonb,
    payload text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    server_ts timestamp with time zone DEFAULT now() NOT NULL,
    expire_at timestamp with time zone,
    sender_did character varying(128),
    CONSTRAINT chk_msg_store_chat_type CHECK (((chat_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text])))
);


--
-- Name: TABLE msg_store; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_store IS '消息永久存储表：Worker 写入，不因客户端 ACK 删除';


--
-- Name: COLUMN msg_store.chat_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.chat_type IS '聊天类型：c2c | c2g';


--
-- Name: COLUMN msg_store.conv_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.conv_key IS '会话键：c2c:{min_uid}:{max_uid} 或 c2g:{gid}';


--
-- Name: COLUMN msg_store.conv_seq; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.conv_seq IS 'per-conversation 单调递增序列号，客户端增量同步游标';


--
-- Name: COLUMN msg_store.msg_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.msg_id IS '消息全局唯一 ID';


--
-- Name: COLUMN msg_store.from_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.from_id IS '发送者 user_id';


--
-- Name: COLUMN msg_store.to_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.to_id IS '接收者 user_id（C2C）';


--
-- Name: COLUMN msg_store.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.group_id IS '群组 ID（C2G）';


--
-- Name: COLUMN msg_store.e2ee; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.e2ee IS 'E2EE 元数据（JSONB，加密消息时非 NULL）';


--
-- Name: COLUMN msg_store.payload; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.payload IS '消息内容（明文 JSON 或加密 base64 密文）';


--
-- Name: COLUMN msg_store.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.created_at IS '消息创建时间（分区键）';


--
-- Name: COLUMN msg_store.server_ts; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.server_ts IS '服务端接收时间';


--
-- Name: COLUMN msg_store.expire_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.expire_at IS '消息原始自毁时间（归档保留）';


--
-- Name: COLUMN msg_store.sender_did; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store.sender_did IS '发送方设备ID（服务端认证态注入，客户端不可伪造）；PFv3 历史 context binding #6，NULL=未提供';


--
-- Name: msg_store_seq; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_store_seq (
    conv_key character varying(64) NOT NULL,
    seq bigint DEFAULT 0 NOT NULL
);


--
-- Name: TABLE msg_store_seq; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_store_seq IS 'per-conversation 序列号计数器';


--
-- Name: COLUMN msg_store_seq.conv_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store_seq.conv_key IS '会话唯一键：c2c:{min_uid}:{max_uid} 或 c2g:{gid}';


--
-- Name: COLUMN msg_store_seq.seq; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_store_seq.seq IS '当前最大序列号，每次写入原子 +1';


--
-- Name: msg_store_staging; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_store_staging (
    id bigint NOT NULL,
    type character varying(10) NOT NULL,
    msg_id character varying(50) NOT NULL,
    msg_type character varying(50),
    action character varying(50),
    e2ee jsonb,
    sender_did character varying(128),
    conv_seq bigint,
    payload jsonb NOT NULL,
    from_id bigint NOT NULL,
    to_id bigint,
    to_id_list bigint[],
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    server_ts timestamp with time zone DEFAULT now() NOT NULL,
    retry_count integer DEFAULT 0 NOT NULL,
    processed_at timestamp with time zone,
    available_at timestamp with time zone DEFAULT now() NOT NULL,
    error_msg text
);


--
-- Name: msg_topic; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.msg_topic (
    id bigint NOT NULL,
    topic_id bigint DEFAULT 0 NOT NULL,
    user_id bigint DEFAULT 0,
    to_id character varying(40) DEFAULT ''::character varying,
    type character varying(40) DEFAULT ''::character varying,
    title character varying(400) DEFAULT ''::character varying,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_msg_topic_type CHECK (((type)::text = ANY (ARRAY[(''::character varying)::text, ('C2S'::character varying)::text, ('C2G'::character varying)::text])))
);


--
-- Name: TABLE msg_topic; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.msg_topic IS '客户端会话记录表';


--
-- Name: COLUMN msg_topic.topic_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_topic.topic_id IS '话题ID，客户端话题自增长ID';


--
-- Name: COLUMN msg_topic.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_topic.user_id IS '发起话题用户ID int';


--
-- Name: COLUMN msg_topic.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.msg_topic.type IS '话题类型 C2S(聊天机器人) C2G';


--
-- Name: olm_fallback_key; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.olm_fallback_key (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id character varying(128) NOT NULL,
    key_id character varying(64) NOT NULL,
    key_base64 text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE olm_fallback_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.olm_fallback_key IS 'Olm fallback key（OTK 耗尽兜底，每设备覆盖式 1 条）';


--
-- Name: olm_identity; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.olm_identity (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id character varying(128) NOT NULL,
    ed25519_key text NOT NULL, -- gitleaks:allow (DDL public-key column name, no key value)
    curve25519_key text NOT NULL, -- gitleaks:allow (DDL public-key column name, no key value)
    signature text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE olm_identity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.olm_identity IS 'Olm 设备身份键（X3DH 长期身份，仅公钥侧）';


--
-- Name: COLUMN olm_identity.ed25519_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_identity.ed25519_key IS 'Ed25519 签名公钥（base64）';


--
-- Name: COLUMN olm_identity.curve25519_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_identity.curve25519_key IS 'Curve25519 身份公钥（base64）';


--
-- Name: COLUMN olm_identity.signature; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_identity.signature IS '对 curve25519_key 的 Ed25519 签名（base64），防服务端篡改';


--
-- Name: olm_one_time_key; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.olm_one_time_key (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_id character varying(128) NOT NULL,
    key_id character varying(64) NOT NULL,
    key_base64 text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    status character varying(20) DEFAULT 'available'::character varying NOT NULL,
    consumed_at timestamp with time zone,
    claimed_by bigint,
    claim_request_id character varying(64),
    CONSTRAINT chk_olm_otk_status CHECK (((status)::text = ANY ((ARRAY['available'::character varying, 'claimed'::character varying])::text[])))
);


--
-- Name: TABLE olm_one_time_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.olm_one_time_key IS 'Olm one-time key（X3DH prekey，claim 即删）';


--
-- Name: COLUMN olm_one_time_key.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_one_time_key.status IS 'available=未领取 / claimed=已消费（不删，供审计与 cleanup）';


--
-- Name: COLUMN olm_one_time_key.consumed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_one_time_key.consumed_at IS 'claim 消费时间戳；cleanup worker 据此清理';


--
-- Name: COLUMN olm_one_time_key.claimed_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_one_time_key.claimed_by IS '领取方 user_id（审计谁消费了此 OTK）';


--
-- Name: COLUMN olm_one_time_key.claim_request_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.olm_one_time_key.claim_request_id IS '领取方幂等键；同 (claimed_by, user_id, device_id, claim_request_id) 重放只消费一条 OTK。NULL=旧行/旧客户端';


--
-- Name: organization; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization (
    id bigint NOT NULL,
    name character varying(200) NOT NULL,
    owner_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    branding jsonb DEFAULT '{}'::jsonb NOT NULL,
    settings jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_organization_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text, 'pending'::text, 'rejected'::text])))
);


--
-- Name: TABLE organization; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization IS '机构（SaaS 租户边界；一个机构可含多个 Workspace；墨芽教学归属锚点）';


--
-- Name: COLUMN organization.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization.id IS '主键 TSID';


--
-- Name: COLUMN organization.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization.name IS '机构名称';


--
-- Name: COLUMN organization.owner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization.owner_id IS '机构 Owner 用户ID（首版唯一管理员；出现第二位跨 Workspace 管理员时再扩表）';


--
-- Name: COLUMN organization.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization.status IS '生命周期: active 正常 | archived 归档 | pending 待审核(APP注册，禁止邀新/出码) | rejected 审核驳回(终态)';


--
-- Name: COLUMN organization.branding; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization.branding IS '品牌配置 jsonb：{name,logo,primaryColor,...}，仅存必要字段';


--
-- Name: COLUMN organization.settings; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization.settings IS '机构设置 jsonb：仅存必要字段';


--
-- Name: organization_business_identity; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_business_identity (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    function_key text NOT NULL,
    display_name text NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    created_by_user_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_obi_display_name CHECK ((display_name <> ''::text)),
    CONSTRAINT ck_obi_function_key CHECK ((function_key = ANY (ARRAY['sales'::text, 'customer_service'::text]))),
    CONSTRAINT ck_obi_status CHECK ((status = ANY (ARRAY['active'::text, 'retired'::text]))),
    CONSTRAINT ck_obi_version CHECK ((version >= 1))
);


--
-- Name: TABLE organization_business_identity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_business_identity IS '企业业务身份（稳定经办主体，归 Organization）：承担 sales/customer_service 职能，不可登录、不发 JWT、不拥有个人好友';


--
-- Name: COLUMN organization_business_identity.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.id IS '主键 TSID；handover 不改变该 ID（资源 owner/ID 不变）';


--
-- Name: COLUMN organization_business_identity.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.organization_id IS '资源 owner 租户；不可由普通业务操作改变，删除机构一律 RESTRICT';


--
-- Name: COLUMN organization_business_identity.function_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.function_key IS '业务职能类型 V1 冻结: sales | customer_service；创建后不可变（EB-D11：不是权限、不是 governance role）';


--
-- Name: COLUMN organization_business_identity.display_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.display_name IS '组织内展示名；同一 Org+职能内唯一';


--
-- Name: COLUMN organization_business_identity.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.status IS '状态: active 可用 | retired 已退役（退役不等于删除，历史关系保留）';


--
-- Name: COLUMN organization_business_identity.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.version IS '乐观锁版本，CAS 从 1 起';


--
-- Name: COLUMN organization_business_identity.created_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity.created_by_user_id IS '创建人（审计快照）；user 删除后置 NULL，不级联企业数据';


--
-- Name: organization_business_identity_assignment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_business_identity_assignment (
    id bigint NOT NULL,
    organization_id bigint CONSTRAINT organization_business_identity_assignm_organization_id_not_null NOT NULL,
    business_identity_id bigint CONSTRAINT organization_business_identity_as_business_identity_id_not_null NOT NULL,
    function_key text NOT NULL,
    user_id bigint,
    status text DEFAULT 'active'::text NOT NULL,
    assigned_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    ended_at timestamp with time zone,
    assigned_by bigint,
    end_reason text,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_obia_active_requires_user CHECK (((status <> 'active'::text) OR (user_id IS NOT NULL))),
    CONSTRAINT ck_obia_ended_at_consistency CHECK ((((status = 'active'::text) AND (ended_at IS NULL)) OR ((status = 'ended'::text) AND (ended_at IS NOT NULL)))),
    CONSTRAINT ck_obia_status CHECK ((status = ANY (ARRAY['active'::text, 'ended'::text]))),
    CONSTRAINT ck_obia_time_range CHECK (((ended_at IS NULL) OR (ended_at >= assigned_at))),
    CONSTRAINT ck_obia_version CHECK ((version >= 1))
);


--
-- Name: TABLE organization_business_identity_assignment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_business_identity_assignment IS '业务身份经办关系（identity <- user 的 assignee 绑定）；user 仅作 assignee，不是资源 owner；active 关系存在时禁止直接 removed（EB-D07）';


--
-- Name: COLUMN organization_business_identity_assignment.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity_assignment.organization_id IS '资源 owner 租户；与 identity 同 Org（由复合 FK 保证）';


--
-- Name: COLUMN organization_business_identity_assignment.function_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity_assignment.function_key IS '必须与 identity 的 function_key 一致（复合 FK 强制），使 active 唯一性可按 (Org,user,function) 表达';


--
-- Name: COLUMN organization_business_identity_assignment.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity_assignment.user_id IS '经办人（assignee）；ON DELETE SET NULL + ck_obia_active_requires_user = 删除 active 经办人时数据库直接拒绝，不级联企业数据';


--
-- Name: COLUMN organization_business_identity_assignment.ended_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity_assignment.ended_at IS '经办结束时间；status=ended 时必须非空，status=active 时必须为空';


--
-- Name: COLUMN organization_business_identity_assignment.assigned_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity_assignment.assigned_by IS '操作人（审计快照）';


--
-- Name: COLUMN organization_business_identity_assignment.end_reason; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_business_identity_assignment.end_reason IS '结束原因（合成/自由文本，审计用）';


--
-- Name: organization_default_workspace; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_default_workspace (
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE organization_default_workspace; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_default_workspace IS 'Org 显式默认 Workspace（每 Org 至多 1 条）；取代最小 active ID 推导（C05 TRANSITION）';


--
-- Name: COLUMN organization_default_workspace.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_default_workspace.organization_id IS '所属机构；PK 即「每 Org 至多一个默认」；机构物理删除一律 RESTRICT（C17 fail-closed）';


--
-- Name: COLUMN organization_default_workspace.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_default_workspace.workspace_id IS '默认 Workspace；必须同 Org（组合 FK）且 status=active（触发器运行时守卫）';


--
-- Name: COLUMN organization_default_workspace.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_default_workspace.created_at IS '设置时间；set 同值幂等不刷新（审计稳定）';


--
-- Name: COLUMN organization_default_workspace.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_default_workspace.updated_at IS '最后变更时间；replace（archive 交接/显式改设）时刷新';


--
-- Name: organization_department; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_department (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    parent_id bigint,
    name character varying(200) NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    version bigint DEFAULT 1 NOT NULL,
    created_by_user_id bigint,
    updated_by_user_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_organization_department_name CHECK (((name)::text <> ''::text)),
    CONSTRAINT ck_organization_department_not_self_parent CHECK ((parent_id <> id)),
    CONSTRAINT ck_organization_department_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text]))),
    CONSTRAINT ck_organization_department_version CHECK ((version >= 1))
);


--
-- Name: TABLE organization_department; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_department IS '树形部门目录（Org 内）；纯通讯录结构，不承载任何资源权限（C10/C15）';


--
-- Name: COLUMN organization_department.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.id IS '主键 TSID';


--
-- Name: COLUMN organization_department.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.organization_id IS '所属机构；目录不可脱离 Org 存在；物理删除机构一律 RESTRICT（C17 fail-closed）';


--
-- Name: COLUMN organization_department.parent_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.parent_id IS '父部门 ID；NULL=根部门；禁止 self-parent 与祖先环（CHECK + 触发器双层）';


--
-- Name: COLUMN organization_department.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.name IS '部门名；同 Org 内 active 部门唯一（部分唯一索引）';


--
-- Name: COLUMN organization_department.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.status IS '状态: active 正常 | archived 已归档；archive 只改目录状态，不级联撤销任何 Org/Workspace 权限';


--
-- Name: COLUMN organization_department.version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.version IS '乐观锁版本（move/update CAS），从 1 起';


--
-- Name: COLUMN organization_department.created_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.created_by_user_id IS '创建人（审计快照）；user 删除后置 NULL，不阻断删除';


--
-- Name: COLUMN organization_department.updated_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department.updated_by_user_id IS '最后修改人（审计快照）；user 删除后置 NULL';


--
-- Name: organization_department_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_department_member (
    organization_id bigint NOT NULL,
    department_id bigint NOT NULL,
    user_id bigint NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    added_by_user_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE organization_department_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_department_member IS '部门成员关系（一人可属多部门）；is_admin 是局部目录角色标记，不产生任何资源权限（C15）';


--
-- Name: COLUMN organization_department_member.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department_member.organization_id IS '所属机构（与 department.organization_id 一致，由 CHECK 锚定）；供同 Org membership 组合 FK 使用';


--
-- Name: COLUMN organization_department_member.department_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department_member.department_id IS '部门 ID；部门行物理删除时成员行随删（目录内部一致性，非权限级联）';


--
-- Name: COLUMN organization_department_member.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department_member.user_id IS '成员用户 ID；必须是同 Org 的 active organization_member（组合 FK + 触发器）';


--
-- Name: COLUMN organization_department_member.is_admin; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department_member.is_admin IS '部门管理员（局部目录角色）；只允许管理本部门目录成员，不授予 Workspace/CS/Agent/任何资源权限';


--
-- Name: COLUMN organization_department_member.added_by_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_department_member.added_by_user_id IS '加入人（审计快照）；user 删除后置 NULL';


--
-- Name: organization_invitation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_invitation (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    target_user_id bigint NOT NULL,
    invited_by bigint,
    token_digest text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    responded_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_organization_invitation_status CHECK ((status = ANY (ARRAY['pending'::text, 'accepted'::text, 'rejected'::text, 'expired'::text, 'revoked'::text]))),
    CONSTRAINT ck_organization_invitation_token_digest CHECK ((token_digest ~ '^[0-9a-f]{64}$'::text))
);


--
-- Name: TABLE organization_invitation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_invitation IS 'Organization 邀请（C11 SOURCE OF TRUTH）：状态机 pending/accepted/rejected/expired/revoked；token 只存 sha256 hex digest；同 (organization_id,target_user_id) 最多一条 pending（uq_organization_invitation_single_pending）。';


--
-- Name: COLUMN organization_invitation.token_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invitation.token_digest IS 'sha256(token 明文) 小写 hex（64 字符）；明文只在 create 响应返回一次，禁止落库/日志。';


--
-- Name: COLUMN organization_invitation.responded_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invitation.responded_at IS '进入终态（accepted/rejected/expired/revoked）的时刻；pending 行为 NULL，一次性消费由应用层 CAS UPDATE 保证恰写一次。';


--
-- Name: organization_invite_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_invite_code (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    code text NOT NULL,
    created_by bigint,
    expires_at timestamp with time zone NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    role text DEFAULT 'member'::text NOT NULL,
    CONSTRAINT chk_organization_invite_code_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text]))),
    CONSTRAINT ck_organization_invite_code_role CHECK ((role = ANY (ARRAY['admin'::text, 'member'::text])))
);


--
-- Name: TABLE organization_invite_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_invite_code IS '组织邀请码（可复用加入凭证：一码多人、7 天有效、Owner/Admin 可撤销；join 走 organization_join_orchestrator 编排）';


--
-- Name: COLUMN organization_invite_code.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.id IS '主键 TSID';


--
-- Name: COLUMN organization_invite_code.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.organization_id IS '所属组织ID';


--
-- Name: COLUMN organization_invite_code.code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.code IS '邀请码（8 位 A-Z2-9，排除 0/O/1/I；全局唯一，撤销后作废不复用）';


--
-- Name: COLUMN organization_invite_code.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.created_by IS '生成人用户ID（NULL=系统生成）';


--
-- Name: COLUMN organization_invite_code.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.expires_at IS '过期时间（生成时刻 + 7 天；过期后 join 返回 982）';


--
-- Name: COLUMN organization_invite_code.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.status IS '状态: active 有效 | revoked 已撤销（撤销后 join 返回 981）';


--
-- Name: COLUMN organization_invite_code.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_invite_code.role IS '凭码加入后的初始角色: member 普通 | admin 管理员（默认 member；owner 仅能经转让获得，不入枚举）';


--
-- Name: organization_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organization_member (
    organization_id bigint NOT NULL,
    user_id bigint NOT NULL,
    role text DEFAULT 'member'::text NOT NULL,
    invited_by bigint,
    joined_at timestamp with time zone,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT ck_organization_member_role CHECK ((role = ANY (ARRAY['owner'::text, 'admin'::text, 'member'::text]))),
    CONSTRAINT ck_organization_member_status CHECK ((status = ANY (ARRAY['active'::text, 'suspended'::text, 'removed'::text])))
);


--
-- Name: TABLE organization_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.organization_member IS 'Organization 治理成员；独立于 workspace_member，成员无需加入 Organization 即可受邀进入其 Workspace';


--
-- Name: COLUMN organization_member.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_member.role IS '组织角色: owner 主治理者 | admin 管理员 | member 普通组织成员';


--
-- Name: COLUMN organization_member.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.organization_member.status IS '状态: active 在册 | suspended 已暂停（企业业务授权立即失败，个人账号不受影响，是可恢复的撤权第一步）| removed 已移除；不联动 workspace_member';


--
-- Name: owner_activation_invite; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.owner_activation_invite (
    id bigint NOT NULL,
    organization_id bigint NOT NULL,
    owner_user_id bigint NOT NULL,
    mobile character varying(40) NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    token_digest text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    last_sent_at timestamp with time zone,
    resend_count integer DEFAULT 0 NOT NULL,
    consumed_at timestamp with time zone,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone,
    CONSTRAINT ck_owner_activation_invite_consume_shape CHECK (((status = 'activated'::text) = (consumed_at IS NOT NULL))),
    CONSTRAINT ck_owner_activation_invite_resend_count CHECK ((resend_count >= 0)),
    CONSTRAINT ck_owner_activation_invite_status CHECK ((status = ANY (ARRAY['pending'::text, 'sms_failed'::text, 'activated'::text, 'superseded'::text]))),
    CONSTRAINT ck_owner_activation_invite_token_digest CHECK ((token_digest ~ '^[0-9a-f]{64}$'::text))
);


--
-- Name: TABLE owner_activation_invite; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.owner_activation_invite IS 'Owner 激活邀请（GZAPP-06 SOURCE OF TRUTH）：状态机 pending/sms_failed/activated/superseded；token 只存 sha256 hex digest（全局唯一激活定位键）；30 天 TTL 到期不删除（reactivate 刷新回 pending）；同 org 最多一条未终结 invite。';


--
-- Name: COLUMN owner_activation_invite.mobile; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.owner_activation_invite.mobile IS '待激活 Owner 手机号（明文，业务必需：重发/换 Owner 定位）；严禁进入日志/审计/错误消息/测试快照，出站一律前3后4脱敏。';


--
-- Name: COLUMN owner_activation_invite.token_digest; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.owner_activation_invite.token_digest IS 'sha256(token 明文) 小写 hex（64 字符）；明文只在 create/reactivate 响应返回一次，禁止落库/日志。';


--
-- Name: COLUMN owner_activation_invite.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.owner_activation_invite.expires_at IS 'TTL 到期时刻（create/reactivate 时 now+30d）；到期是查询谓词不是状态值，到期行不删除、可 reactivate 或换 Owner。';


--
-- Name: COLUMN owner_activation_invite.consumed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.owner_activation_invite.consumed_at IS '单次消费 CAS 标记：activated 时非 NULL，其余状态 NULL（ck_owner_activation_invite_consume_shape 兜底）。';


--
-- Name: payment_transaction; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment_transaction (
    id bigint NOT NULL,
    trade_no character varying(64) NOT NULL,
    biz_type smallint NOT NULL,
    biz_order_no character varying(64) NOT NULL,
    user_id bigint NOT NULL,
    gateway character varying(20) NOT NULL,
    gateway_payment_no character varying(128),
    amount bigint NOT NULL,
    currency character varying(8) DEFAULT 'CNY'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    notify_data jsonb,
    paid_at timestamp with time zone,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_payment_tx_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_payment_tx_biz_type CHECK ((biz_type = ANY (ARRAY[1, 2, 3]))),
    CONSTRAINT chk_payment_tx_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4, 5])))
);


--
-- Name: TABLE payment_transaction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.payment_transaction IS '统一支付流水（对账+回调幂等，金额单位：分）';


--
-- Name: COLUMN payment_transaction.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.payment_transaction.status IS '状态: 0待支付 1成功 2失败 3已退款 4部分退款 5退款中(B-09 占位态)';


--
-- Name: plugin_audit_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.plugin_audit_log (
    id bigint NOT NULL,
    plugin_name character varying(64) NOT NULL,
    operation_id character varying(64),
    event character varying(64) NOT NULL,
    from_state character varying(32),
    to_state character varying(32),
    step character varying(64),
    operator character varying(64) DEFAULT 'system'::character varying,
    started_at bigint NOT NULL,
    ended_at bigint,
    duration_ms integer,
    result character varying(16),
    error_code character varying(64),
    error_detail text,
    metadata jsonb DEFAULT '{}'::jsonb,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_plugin_audit_log_result CHECK (((result IS NULL) OR ((result)::text = ANY (ARRAY[('ok'::character varying)::text, ('failed'::character varying)::text, ('cancelled'::character varying)::text, ('timeout'::character varying)::text]))))
);


--
-- Name: TABLE plugin_audit_log; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.plugin_audit_log IS '插件操作审计日志';


--
-- Name: COLUMN plugin_audit_log.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.id IS 'TSID 分布式 ID';


--
-- Name: COLUMN plugin_audit_log.plugin_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.plugin_name IS '插件名称';


--
-- Name: COLUMN plugin_audit_log.operation_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.operation_id IS 'admin 触发的操作 id（NULL = 内部事件）';


--
-- Name: COLUMN plugin_audit_log.event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.event IS '事件类型：state_entered / step_done / rollback_started 等';


--
-- Name: COLUMN plugin_audit_log.from_state; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.from_state IS '转换前状态';


--
-- Name: COLUMN plugin_audit_log.to_state; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.to_state IS '转换后状态';


--
-- Name: COLUMN plugin_audit_log.step; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.step IS '当前执行步骤';


--
-- Name: COLUMN plugin_audit_log.operator; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.operator IS '操作者：admin uid 或 system';


--
-- Name: COLUMN plugin_audit_log.started_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.started_at IS '开始时间（毫秒时间戳，用于精确 duration 计算）';


--
-- Name: COLUMN plugin_audit_log.ended_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.ended_at IS '结束时间（毫秒时间戳，NULL = 仍在进行）';


--
-- Name: COLUMN plugin_audit_log.duration_ms; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.duration_ms IS '持续毫秒数 = ended_at - started_at';


--
-- Name: COLUMN plugin_audit_log.result; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.result IS '结果：ok / failed / cancelled / timeout';


--
-- Name: COLUMN plugin_audit_log.error_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.error_code IS '失败时分类码';


--
-- Name: COLUMN plugin_audit_log.error_detail; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.error_detail IS '失败时详细信息（脱敏后）';


--
-- Name: COLUMN plugin_audit_log.metadata; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.metadata IS 'JSONB 任意扩展字段';


--
-- Name: COLUMN plugin_audit_log.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.plugin_audit_log.created_at IS '记录创建时间';


--
-- Name: project; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project (
    id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    name character varying(200) NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    owner_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    links jsonb DEFAULT '[]'::jsonb NOT NULL,
    CONSTRAINT chk_project_status CHECK ((status = ANY (ARRAY['active'::text, 'done'::text])))
);


--
-- Name: TABLE project; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.project IS '项目（可选执行层：目标→任务→交付；W0 Project Lite，对 active workspace_member 可见）';


--
-- Name: COLUMN project.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project.workspace_id IS '所属工作区ID（I3 非空外键；一个 Workspace 可有多个 Project）';


--
-- Name: COLUMN project.owner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project.owner_id IS '项目 Owner；提交时必须是同 workspace 的 active workspace_member（W0 无 project_member）';


--
-- Name: COLUMN project.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project.status IS '状态: active 进行中 | done 已完成';


--
-- Name: COLUMN project.links; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project.links IS 'Resources 聚合的手工链接数组 [{name,url}]（name/url 均非空字符串；形状由 trg_project_links_shape 强制）';


--
-- Name: project_channel_rel; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_channel_rel (
    workspace_id bigint NOT NULL,
    project_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    created_by bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE project_channel_rel; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.project_channel_rel IS '项目↔频道关联（Channel-first-class W2；同 Workspace 强制，重复 link 幂等只一行）';


--
-- Name: COLUMN project_channel_rel.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_channel_rel.workspace_id IS '冗余 Workspace ID；两侧复合 FK 强制 project 与 channel 同 workspace（personal 频道不可关联）';


--
-- Name: COLUMN project_channel_rel.channel_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_channel_rel.channel_id IS '被关联频道ID（scope=workspace）';


--
-- Name: COLUMN project_channel_rel.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_channel_rel.created_by IS '关联操作人用户ID（审计）';


--
-- Name: project_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_event (
    id bigint NOT NULL,
    project_id bigint NOT NULL,
    actor_id bigint NOT NULL,
    event_type text NOT NULL,
    target_id bigint,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_project_event_type CHECK ((event_type = ANY (ARRAY['project_created'::text, 'project_status'::text, 'task_created'::text, 'task_status'::text, 'task_assignee'::text, 'member_invited'::text, 'member_removed'::text, 'member_owner_transferred'::text, 'milestone_created'::text, 'milestone_updated'::text, 'milestone_reached'::text, 'channel_linked'::text, 'channel_unlinked'::text, 'links_updated'::text])))
);


--
-- Name: TABLE project_event; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.project_event IS '项目 Activity 唯一数据源（纯系统事件流；不含消息/群公告正文；业务写与事件同事务提交）';


--
-- Name: COLUMN project_event.actor_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_event.actor_id IS '操作者用户ID（审计历史，不设 user 外键）';


--
-- Name: COLUMN project_event.event_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_event.event_type IS 'W0 实际写入的事件类型；未来扩展需新迁移修改 CHECK（正常演进；不含已 defer 的 milestone_status/channel_rel/member_change）';


--
-- Name: COLUMN project_event.target_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_event.target_id IS '事件目标资源ID（如 project_task.id）';


--
-- Name: project_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_member (
    workspace_id bigint NOT NULL,
    project_id bigint NOT NULL,
    user_id bigint NOT NULL,
    invited_by bigint,
    joined_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_project_member_status CHECK ((status = ANY (ARRAY['active'::text, 'removed'::text])))
);


--
-- Name: TABLE project_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.project_member IS '项目成员（Project 访问边界；无 Project Role，读写权限继承 Workspace Member 角色）';


--
-- Name: COLUMN project_member.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_member.workspace_id IS '冗余 Workspace ID；与 (project_id,user_id) 一起构成复合 FK 子集约束（同 Workspace 强制）';


--
-- Name: COLUMN project_member.project_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_member.project_id IS '所属项目ID（PK 前缀，唯一成员身份即"重复成员只一行"）';


--
-- Name: COLUMN project_member.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_member.user_id IS '成员用户ID';


--
-- Name: COLUMN project_member.invited_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_member.invited_by IS '邀请人用户ID（NULL=Owner 存量回填/自建）';


--
-- Name: COLUMN project_member.joined_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_member.joined_at IS '加入时间（Project Member 无"邀请未确认"态，写入即生效）';


--
-- Name: COLUMN project_member.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_member.status IS '状态: active 在册 | removed 已移除（软删；重新邀请由应用层覆盖激活，不自动恢复历史下级）';


--
-- Name: project_milestone; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_milestone (
    id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    project_id bigint NOT NULL,
    name character varying(200) NOT NULL,
    due_date date,
    status text DEFAULT 'planned'::text NOT NULL,
    reached_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_project_milestone_reached CHECK (((status = 'reached'::text) = (reached_at IS NOT NULL))),
    CONSTRAINT chk_project_milestone_status CHECK ((status = ANY (ARRAY['planned'::text, 'reached'::text])))
);


--
-- Name: TABLE project_milestone; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.project_milestone IS '项目里程碑（W2：仅 name/due_date/status，禁依赖/甘特图/复杂状态机）';


--
-- Name: COLUMN project_milestone.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_milestone.id IS '主键 TSID';


--
-- Name: COLUMN project_milestone.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_milestone.workspace_id IS '冗余 Workspace ID（复合 FK 同 Workspace 强制）';


--
-- Name: COLUMN project_milestone.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_milestone.status IS '状态: planned 计划 | reached 已达成（单向 planned→reached，由应用层状态机控制）';


--
-- Name: COLUMN project_milestone.reached_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_milestone.reached_at IS '达成时间（status=reached 时必须非空，CHECK 同步强制）';


--
-- Name: project_task; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_task (
    id bigint NOT NULL,
    project_id bigint NOT NULL,
    title character varying(500) NOT NULL,
    creator_id bigint NOT NULL,
    assignee_id bigint,
    status text DEFAULT 'todo'::text NOT NULL,
    sort integer DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_project_task_sort CHECK ((sort >= 0)),
    CONSTRAINT chk_project_task_status CHECK ((status = ANY (ARRAY['todo'::text, 'doing'::text, 'review'::text, 'done'::text])))
);


--
-- Name: TABLE project_task; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.project_task IS '项目任务（轻量执行实体：仅 title/assignee/status/排序；禁甘特图/依赖/估点/子任务）';


--
-- Name: COLUMN project_task.assignee_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_task.assignee_id IS '负责人；W0 必须来自 active workspace_member（T6b 应用层同事务校验，schema 仅保证 user 存在）';


--
-- Name: COLUMN project_task.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_task.status IS '四态: todo 待办 | doing 进行中 | review 待评审 | done 已完成（含回退，由应用层状态机控制）';


--
-- Name: COLUMN project_task.sort; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.project_task.sort IS '手动排序值，同 status 内升序展示';


--
-- Name: push_token; Type: TABLE; Schema: public; Owner: -
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
    CONSTRAINT chk_push_token_device_type CHECK (((device_type)::text = ANY (ARRAY[('android'::character varying)::text, ('ios'::character varying)::text, ('web'::character varying)::text]))),
    CONSTRAINT chk_push_token_platform CHECK (((platform)::text = ANY (ARRAY[('fcm'::character varying)::text, ('apns'::character varying)::text, ('web_push'::character varying)::text, ('jpush'::character varying)::text]))),
    CONSTRAINT chk_push_token_status CHECK ((status = ANY (ARRAY[0, 1])))
);


--
-- Name: TABLE push_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.push_token IS '推送 Token 管理表';


--
-- Name: COLUMN push_token.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.push_token.user_id IS '用户 ID';


--
-- Name: COLUMN push_token.device_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.push_token.device_id IS '设备唯一标识';


--
-- Name: COLUMN push_token.device_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.push_token.device_type IS '设备类型: android | ios | web';


--
-- Name: COLUMN push_token.platform; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.push_token.platform IS '推送平台: fcm | apns | web_push';


--
-- Name: COLUMN push_token.token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.push_token.token IS '推送 token';


--
-- Name: COLUMN push_token.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.push_token.status IS '状态: 1=活跃 0=无效';


--
-- Name: CONSTRAINT chk_push_token_platform ON push_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT chk_push_token_platform ON public.push_token IS '推送 provider 值域: fcm | apns | web_push | jpush（jpush 由 00000136 扩展，device_type 仍为 android/ios/web；136 前为 fcm/apns/web_push）';


--
-- Name: recharge_order; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.recharge_order (
    id bigint NOT NULL,
    order_no character varying(64) NOT NULL,
    user_id bigint NOT NULL,
    amount bigint NOT NULL,
    currency character varying(8) DEFAULT 'CNY'::character varying NOT NULL,
    payment_method character varying(20) NOT NULL,
    payment_no character varying(128),
    status smallint DEFAULT 0 NOT NULL,
    paid_at timestamp with time zone,
    expires_at timestamp with time zone,
    extra_data jsonb,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_recharge_order_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_recharge_order_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])))
);


--
-- Name: TABLE recharge_order; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.recharge_order IS '钱包充值订单（金额单位：分）';


--
-- Name: red_packet; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.red_packet (
    id bigint NOT NULL,
    sender_uid bigint NOT NULL,
    type character varying(16) NOT NULL,
    amount bigint NOT NULL,
    count integer NOT NULL,
    remain_amount bigint NOT NULL,
    remain_count integer NOT NULL,
    greeting character varying(255) DEFAULT '恭喜发财，大吉大利'::character varying NOT NULL,
    status character varying(16) DEFAULT 'active'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    scope_type character varying(8),
    scope_id bigint,
    CONSTRAINT chk_red_packet_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_red_packet_count CHECK ((count > 0)),
    CONSTRAINT chk_red_packet_remain_amount CHECK ((remain_amount >= 0)),
    CONSTRAINT chk_red_packet_remain_count CHECK ((remain_count >= 0)),
    CONSTRAINT chk_red_packet_scope_pair CHECK ((((scope_type IS NULL) AND (scope_id IS NULL)) OR ((scope_type IS NOT NULL) AND (scope_id IS NOT NULL)))),
    CONSTRAINT chk_red_packet_scope_type CHECK (((scope_type IS NULL) OR ((scope_type)::text = ANY (ARRAY['C2C'::text, 'C2G'::text])))),
    CONSTRAINT chk_red_packet_status CHECK (((status)::text = ANY (ARRAY['active'::text, 'finished'::text, 'expired'::text]))),
    CONSTRAINT chk_red_packet_type CHECK (((type)::text = ANY (ARRAY['random'::text, 'fixed'::text])))
);


--
-- Name: TABLE red_packet; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.red_packet IS '红包主表（金额单位：分）';


--
-- Name: COLUMN red_packet.scope_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.red_packet.scope_type IS '作用域类型: NULL未绑定(旧数据) C2C单聊 C2G群聊';


--
-- Name: COLUMN red_packet.scope_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.red_packet.scope_id IS '作用域ID: C2G为group_id，C2C为对端uid';


--
-- Name: red_packet_receive; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.red_packet_receive (
    id bigint NOT NULL,
    red_packet_id bigint NOT NULL,
    receiver_uid bigint NOT NULL,
    amount bigint NOT NULL,
    received_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_red_packet_recv_amount CHECK ((amount > 0))
);


--
-- Name: TABLE red_packet_receive; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.red_packet_receive IS '红包领取表（金额单位：分）';


--
-- Name: report_action_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.report_action_log (
    id bigint NOT NULL,
    report_id bigint NOT NULL,
    operator_uid bigint NOT NULL,
    result smallint NOT NULL,
    note character varying(500) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_report_action_result CHECK ((result = ANY (ARRAY[1, 2])))
);


--
-- Name: TABLE report_action_log; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.report_action_log IS '举报处理操作日志';


--
-- Name: report_ticket; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.report_ticket (
    id bigint NOT NULL,
    target_type character varying(16) NOT NULL,
    target_id bigint NOT NULL,
    reporter_uid bigint NOT NULL,
    reason character varying(64) DEFAULT ''::character varying NOT NULL,
    description character varying(500) DEFAULT ''::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    handled_by bigint,
    handled_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    target_sub_type character varying(16) DEFAULT ''::character varying NOT NULL,
    target_scope_id bigint DEFAULT 0 NOT NULL,
    target_author_id bigint DEFAULT 0 NOT NULL,
    evidence jsonb DEFAULT '{}'::jsonb NOT NULL,
    CONSTRAINT chk_report_ticket_message_shape CHECK ((((target_type)::text <> 'message'::text) OR (((target_sub_type)::text = ANY (ARRAY[('c2c'::character varying)::text, ('c2g'::character varying)::text, ('channel'::character varying)::text])) AND (target_scope_id > 0) AND (target_author_id > 0)))),
    CONSTRAINT chk_report_ticket_status CHECK ((status = ANY (ARRAY[0, 1, 2]))),
    CONSTRAINT chk_report_ticket_target_sub_type CHECK (((target_sub_type)::text = ANY (ARRAY[(''::character varying)::text, ('c2c'::character varying)::text, ('c2g'::character varying)::text, ('channel'::character varying)::text]))),
    CONSTRAINT chk_report_ticket_target_type CHECK (((target_type)::text = ANY (ARRAY[('moment'::character varying)::text, ('group'::character varying)::text, ('channel'::character varying)::text, ('user'::character varying)::text, ('message'::character varying)::text])))
);


--
-- Name: TABLE report_ticket; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.report_ticket IS '举报中心工单表（统一 group/channel/user/moment）';


--
-- Name: COLUMN report_ticket.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.report_ticket.status IS '0待处理 1已驳回 2已确认违规';


--
-- Name: review_asset; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.review_asset (
    id bigint NOT NULL,
    review_id bigint NOT NULL,
    attachment_id bigint NOT NULL,
    kind text NOT NULL,
    sort_order integer DEFAULT 0 NOT NULL,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_review_asset_created_by_sentinel CHECK ((created_by >= 0)),
    CONSTRAINT ck_review_asset_kind CHECK ((kind = ANY (ARRAY['feedback_image'::text, 'feedback_video'::text])))
);


--
-- Name: TABLE review_asset; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.review_asset IS '老师回评多媒体关联（0-1 feedback_video + 0-3 feedback_image；video_attachment_id 旧列为兼容读窗口，非写入真源）';


--
-- Name: COLUMN review_asset.review_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.review_asset.review_id IS '回评ID（FK→teacher_review，CASCADE：review 删除连带解除关联）';


--
-- Name: COLUMN review_asset.attachment_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.review_asset.attachment_id IS '附件ID（全表 UNIQUE：一附件至多绑一 review；FK RESTRICT 保护）';


--
-- Name: COLUMN review_asset.kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.review_asset.kind IS '类型: feedback_video 反馈视频（单 review 至多 1）| feedback_image 反馈图片（单 review 至多 3，触发器约束）';


--
-- Name: COLUMN review_asset.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.review_asset.created_by IS '创建人用户ID（审计；0=sentinel 注销语义，禁置NULL——00000100 同款）';


--
-- Name: review_queue; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.review_queue (
    id bigint NOT NULL,
    msg_id bigint DEFAULT 0 NOT NULL,
    msg_type character varying(32) DEFAULT 'text'::character varying NOT NULL,
    content text DEFAULT ''::text NOT NULL,
    from_id bigint DEFAULT 0 NOT NULL,
    from_account character varying(64) DEFAULT ''::character varying NOT NULL,
    to_id bigint DEFAULT 0 NOT NULL,
    to_type character varying(16) DEFAULT 'user'::character varying NOT NULL,
    hit_words text DEFAULT ''::text NOT NULL,
    review_status character varying(16) DEFAULT 'pending'::character varying NOT NULL,
    reviewer_id bigint,
    reason text,
    reviewed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE review_queue; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.review_queue IS '消息人工复审队列（内容审核）';


--
-- Name: sensitive_word; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sensitive_word (
    id bigint NOT NULL,
    word character varying(128) NOT NULL,
    category character varying(32) DEFAULT 'custom'::character varying NOT NULL,
    severity character varying(16) DEFAULT 'medium'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE sensitive_word; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.sensitive_word IS '敏感词黑名单（内容审核）';


--
-- Name: sso_config; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sso_config (
    id bigint NOT NULL,
    provider character varying(16) NOT NULL,
    enabled boolean DEFAULT false NOT NULL,
    config jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE sso_config; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.sso_config IS 'SSO 外部认证配置（每 provider 一行，按 provider upsert）';


--
-- Name: sso_identity; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sso_identity (
    id bigint NOT NULL,
    provider character varying(16) NOT NULL,
    subject character varying(255) NOT NULL,
    uid bigint NOT NULL,
    email character varying(255) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE sso_identity; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.sso_identity IS 'SSO 身份映射（provider+subject -> uid，OIDC 登录流）';


--
-- Name: submission_asset; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.submission_asset (
    id bigint NOT NULL,
    submission_id bigint NOT NULL,
    attachment_id bigint NOT NULL,
    kind text NOT NULL,
    sort_order integer DEFAULT 0 NOT NULL,
    created_by bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_submission_asset_kind CHECK ((kind = ANY (ARRAY['practice_video'::text, 'final_photo'::text])))
);


--
-- Name: TABLE submission_asset; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.submission_asset IS '提交附件关联（只存 attachment_id，业务表不得持久化 presigned URL）';


--
-- Name: COLUMN submission_asset.kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.submission_asset.kind IS '类型: practice_video 练字视频 | final_photo 作品照片';


--
-- Name: COLUMN submission_asset.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.submission_asset.created_by IS '添加人用户ID（审计；SET NULL 保留关联行）';


--
-- Name: system_datacenter; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE system_datacenter; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.system_datacenter IS '机房注册表 - 支持动态增删';


--
-- Name: COLUMN system_datacenter.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.id IS '机房ID (0-32767), 由数据库自动分配';


--
-- Name: COLUMN system_datacenter.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.name IS '机房名称，业务标识，如: beijing, shanghai, shenzhen';


--
-- Name: COLUMN system_datacenter.region; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.region IS '机房所在区域，如: cn-north, cn-east, cn-south';


--
-- Name: COLUMN system_datacenter.api_endpoint; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.api_endpoint IS '机房API地址，用于健康检查和数据同步';


--
-- Name: COLUMN system_datacenter.is_active; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.is_active IS '是否激活，注销时设置为FALSE（软删除）';


--
-- Name: COLUMN system_datacenter.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.created_at IS '机房注册时间';


--
-- Name: COLUMN system_datacenter.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter.updated_at IS '机房信息最后更新时间';


--
-- Name: system_datacenter_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.system_datacenter_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: system_datacenter_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.system_datacenter_id_seq OWNED BY public.system_datacenter.id;


--
-- Name: system_datacenter_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.system_datacenter_log (
    id bigint NOT NULL,
    datacenter_id smallint,
    action character varying(20) NOT NULL,
    details jsonb,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE system_datacenter_log; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.system_datacenter_log IS '机房变更审计日志 - 记录所有注册/注销/修改操作';


--
-- Name: COLUMN system_datacenter_log.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter_log.id IS '日志记录ID';


--
-- Name: COLUMN system_datacenter_log.datacenter_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter_log.datacenter_id IS '机房ID';


--
-- Name: COLUMN system_datacenter_log.action; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter_log.action IS '操作类型: register(注册), unregister(注销), modify(修改)';


--
-- Name: COLUMN system_datacenter_log.details; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter_log.details IS '详细信息，JSONB格式';


--
-- Name: COLUMN system_datacenter_log.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_datacenter_log.created_at IS '操作时间';


--
-- Name: system_id_segment; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE system_id_segment; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.system_id_segment IS 'ID段分配记录 - 每个机房每张表维护独立的ID段';


--
-- Name: COLUMN system_id_segment.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.id IS '分配记录ID';


--
-- Name: COLUMN system_id_segment.datacenter_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.datacenter_id IS '所属机房ID';


--
-- Name: COLUMN system_id_segment.table_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.table_name IS '表名，如: user, group, msg_c2c';


--
-- Name: COLUMN system_id_segment.segment_start; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.segment_start IS 'ID段起始值 (包含)';


--
-- Name: COLUMN system_id_segment.segment_end; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.segment_end IS 'ID段结束值 (包含)';


--
-- Name: COLUMN system_id_segment.allocated_size; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.allocated_size IS '分配的ID总数';


--
-- Name: COLUMN system_id_segment.used_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.used_count IS '已使用的ID数量 (估算值，用于监控)';


--
-- Name: COLUMN system_id_segment.is_active; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.is_active IS '是否为当前活跃段';


--
-- Name: COLUMN system_id_segment.allocated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.allocated_at IS 'ID段分配时间';


--
-- Name: COLUMN system_id_segment.expired_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment.expired_at IS '过期时间，NULL 表示永不过期';


--
-- Name: system_id_segment_stats; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE system_id_segment_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.system_id_segment_stats IS 'ID段使用统计表 - 用于监控和趋势分析';


--
-- Name: COLUMN system_id_segment_stats.datacenter_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.datacenter_id IS '机房ID';


--
-- Name: COLUMN system_id_segment_stats.table_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.table_name IS '表名';


--
-- Name: COLUMN system_id_segment_stats.stats_time; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.stats_time IS '统计时间';


--
-- Name: COLUMN system_id_segment_stats.segment_start; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.segment_start IS '当前使用的ID段起始值';


--
-- Name: COLUMN system_id_segment_stats.segment_end; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.segment_end IS '当前使用的ID段结束值';


--
-- Name: COLUMN system_id_segment_stats.used_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.used_count IS '使用量估算';


--
-- Name: COLUMN system_id_segment_stats.usage_percent; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.usage_percent IS '使用率百分比';


--
-- Name: COLUMN system_id_segment_stats.allocate_rate_per_sec; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.system_id_segment_stats.allocate_rate_per_sec IS '每秒分配速率';


--
-- Name: teacher_review; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.teacher_review (
    id bigint NOT NULL,
    submission_id bigint NOT NULL,
    reviewer_uid bigint,
    positive_point text DEFAULT ''::text NOT NULL,
    focus_problem text DEFAULT ''::text NOT NULL,
    practice_action text DEFAULT ''::text NOT NULL,
    comment text DEFAULT ''::text NOT NULL,
    video_attachment_id bigint,
    rework_required boolean DEFAULT false NOT NULL,
    status text DEFAULT 'draft'::text NOT NULL,
    published_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    char_reviews jsonb,
    CONSTRAINT ck_teacher_review_published CHECK (((status <> 'published'::text) OR ((reviewer_uid IS NOT NULL) AND (published_at IS NOT NULL) AND ((positive_point <> ''::text) OR (focus_problem <> ''::text) OR (practice_action <> ''::text) OR (comment <> ''::text) OR (video_attachment_id IS NOT NULL))))),
    CONSTRAINT ck_teacher_review_reviewer_uid_sentinel CHECK (((reviewer_uid IS NULL) OR (reviewer_uid >= 0))),
    CONSTRAINT ck_teacher_review_status CHECK ((status = ANY (ARRAY['draft'::text, 'published'::text, 'discarded'::text])))
);


--
-- Name: TABLE teacher_review; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.teacher_review IS '老师回评（draft→published 一次性条件更新；discarded 保留审计）';


--
-- Name: COLUMN teacher_review.reviewer_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teacher_review.reviewer_uid IS '回评老师用户ID（published 时 CHECK 强制非空）；裸列无FK；0=sentinel=老师账号已删除/匿名化（published 行保留回评内容与 0 sentinel，禁置NULL）';


--
-- Name: COLUMN teacher_review.video_attachment_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teacher_review.video_attachment_id IS '真人点评视频附件ID（FK→attachment，RESTRICT 保护）';


--
-- Name: COLUMN teacher_review.rework_required; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teacher_review.rework_required IS '是否要求重练（true 时家长从回评进入新 attempt）';


--
-- Name: COLUMN teacher_review.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teacher_review.status IS '状态: draft | published | discarded';


--
-- Name: COLUMN teacher_review.char_reviews; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teacher_review.char_reviews IS '逐字点评字卡数组（jsonb；[{index,char,grade,comment}]≤50 项，应用层白名单校验；NULL=无逐字数据，不渲染字卡区）';


--
-- Name: teaching_admin_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.teaching_admin_audit (
    id bigint NOT NULL,
    action text NOT NULL,
    operator_uid bigint NOT NULL,
    learner_id bigint NOT NULL,
    target_user_id bigint,
    detail jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_teaching_admin_audit_action CHECK ((action = ANY (ARRAY['bind_learner'::text, 'unbind_learner'::text, 'learner_archive'::text, 'data_export'::text, 'data_delete'::text]))),
    CONSTRAINT ck_teaching_admin_audit_uids CHECK ((operator_uid >= 0))
);


--
-- Name: TABLE teaching_admin_audit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.teaching_admin_audit IS '教学管理动作审计（不可变合规日志：绑定/解绑/归档/导出/删除；§9.3）';


--
-- Name: COLUMN teaching_admin_audit.action; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teaching_admin_audit.action IS '动作: bind_learner|unbind_learner|learner_archive|data_export|data_delete（新动作经迁移扩枚举）';


--
-- Name: COLUMN teaching_admin_audit.operator_uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teaching_admin_audit.operator_uid IS '操作人用户ID（裸列无FK）；0=sentinel=操作人账号已删除/匿名化（应用层匿名化时改写为0，禁置NULL）';


--
-- Name: COLUMN teaching_admin_audit.learner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teaching_admin_audit.learner_id IS '学员档案ID（裸列无FK；审计行先于实体存活，learner 物理删除被 RESTRICT 链约束，删除请求走匿名化）';


--
-- Name: COLUMN teaching_admin_audit.target_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teaching_admin_audit.target_user_id IS '目标用户ID（bind 目标/解绑原账号）；NULL=动作无目标用户；0=sentinel=目标账号已删除/匿名化';


--
-- Name: COLUMN teaching_admin_audit.detail; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.teaching_admin_audit.detail IS '结构化上下文 jsonb（role/why/request_id 等）；禁止存 display_name/出生年/视频URL 等儿童 PII';


--
-- Name: transfer_order; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.transfer_order (
    id bigint NOT NULL,
    sender_uid bigint NOT NULL,
    receiver_uid bigint NOT NULL,
    amount bigint NOT NULL,
    remark character varying(255) DEFAULT '转账给好友'::character varying NOT NULL,
    status character varying(16) DEFAULT 'pending'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    completed_at timestamp with time zone,
    CONSTRAINT chk_transfer_amount CHECK ((amount > 0)),
    CONSTRAINT chk_transfer_status CHECK (((status)::text = ANY (ARRAY['pending'::text, 'accepted'::text, 'refunded'::text])))
);


--
-- Name: TABLE transfer_order; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.transfer_order IS '单聊转账订单表（金额单位：分）';


--
-- Name: trust_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.trust_audit (
    id bigint NOT NULL,
    actor_uid bigint NOT NULL,
    target_uid bigint NOT NULL,
    target_device_id character varying(128) NOT NULL,
    target_ed25519 text NOT NULL,
    from_state character varying(20) NOT NULL,
    to_state character varying(20) NOT NULL,
    method character varying(40) NOT NULL,
    actor_signature text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    event_id character varying(64),
    issued_at bigint,
    expires_at bigint,
    actor_device_generation integer,
    target_identity_version integer
);


--
-- Name: TABLE trust_audit; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.trust_audit IS '设备信任决策事件流（append-only）。算法见 ADR 06。';


--
-- Name: COLUMN trust_audit.event_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.trust_audit.event_id IS '客户端生成的全局唯一幂等键，同 event_id 重放返回原结果，见 ADR 16 §3.3.1';


--
-- Name: COLUMN trust_audit.issued_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.trust_audit.issued_at IS '事件签发时刻（epoch ms），freshness 下界';


--
-- Name: COLUMN trust_audit.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.trust_audit.expires_at IS '事件有效期上界（epoch ms）';


--
-- Name: COLUMN trust_audit.actor_device_generation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.trust_audit.actor_device_generation IS 'actor 设备代数快照';


--
-- Name: COLUMN trust_audit.target_identity_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.trust_audit.target_identity_version IS 'target 身份版本快照，防回退';


--
-- Name: trust_audit_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.trust_audit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: trust_audit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.trust_audit_id_seq OWNED BY public.trust_audit.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: -
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
    account_type smallint DEFAULT 0 NOT NULL,
    background text,
    profession text DEFAULT ''::text NOT NULL,
    school text DEFAULT ''::text NOT NULL,
    interests text DEFAULT ''::text NOT NULL,
    updated_at timestamp with time zone,
    CONSTRAINT chk_user_gender CHECK ((gender = ANY (ARRAY[0, 1, 2, 3]))),
    CONSTRAINT chk_user_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1, 2])))
);


--
-- Name: TABLE "user"; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public."user" IS '用户表';


--
-- Name: COLUMN "user".id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".id IS '主键 自增长ID';


--
-- Name: COLUMN "user".level_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".level_id IS '会员等级ID';


--
-- Name: COLUMN "user".nickname; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".nickname IS '用户昵称';


--
-- Name: COLUMN "user".password; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".password IS '经过加盐的密码，由 elib_password:generate/1 生成（HMAC-SHA512）';


--
-- Name: COLUMN "user".account; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".account IS '会员账号';


--
-- Name: COLUMN "user".mobile; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".mobile IS '手机号码';


--
-- Name: COLUMN "user".email; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".email IS '会员注册Email';


--
-- Name: COLUMN "user".region; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".region IS '地区：广东 深圳';


--
-- Name: COLUMN "user".gender; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".gender IS '性别 1 男  2 女  3 保密';


--
-- Name: COLUMN "user".experience; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".experience IS '经验值';


--
-- Name: COLUMN "user".avatar; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".avatar IS '头像';


--
-- Name: COLUMN "user".sign; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".sign IS '用户签名';


--
-- Name: COLUMN "user".ref_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".ref_user_id IS '推荐人ID，0表示无推荐人';


--
-- Name: COLUMN "user".status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".status IS '状态: -1 删除  0 禁用  1 启用  2 申请注销中';


--
-- Name: COLUMN "user".created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN "user".reg_ip; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".reg_ip IS '注册IP';


--
-- Name: COLUMN "user".reg_cosv; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".reg_cosv IS '客户端操作系统版本，例如： Linux 5.11.0-1018-gcp #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021 | "Windows 10 Pro" 10.0 (Build 19043)';


--
-- Name: COLUMN "user".ref_parent_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".ref_parent_user_id IS '推荐人的推荐人user id';


--
-- Name: COLUMN "user".source; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".source IS '注册来源标记';


--
-- Name: COLUMN "user".birthday; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".birthday IS '生日，格式 YYYY-MM-DD';


--
-- Name: COLUMN "user".dnd_enabled; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".dnd_enabled IS '全局免打扰开关，默认关闭';


--
-- Name: COLUMN "user".account_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".account_type IS '账号类型 0=human 1=agent(平台AI) 2=system_bot(频道webhook bot) 3=bot(开发者服务)';


--
-- Name: COLUMN "user".background; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".background IS '个人资料封面背景图 object_key（scope=public），无为 NULL / Profile cover';


--
-- Name: COLUMN "user".profession; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".profession IS '职业 / Profession';


--
-- Name: COLUMN "user".school; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".school IS '学校 / School';


--
-- Name: COLUMN "user".interests; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public."user".interests IS '兴趣爱好 / Interests';


--
-- Name: user_auth_epoch; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_auth_epoch (
    user_id bigint NOT NULL,
    epoch bigint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: TABLE user_auth_epoch; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_auth_epoch IS 'Task10/LT-04 session epoch; token ep < row epoch = revoked; missing row = epoch 1';


--
-- Name: user_collect; Type: TABLE; Schema: public; Owner: -
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
    attach_file_hash256 text DEFAULT ''::character varying CONSTRAINT user_collect_attach_md5_not_null NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_user_collect_status CHECK ((status = ANY (ARRAY['-1'::integer, 0, 1])))
);


--
-- Name: TABLE user_collect; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_collect IS '用户收藏记录表';


--
-- Name: COLUMN user_collect.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.id IS '主键 自增长ID';


--
-- Name: COLUMN user_collect.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.user_id IS '资源的收藏者';


--
-- Name: COLUMN user_collect.kind; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.kind IS 'Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片';


--
-- Name: COLUMN user_collect.kind_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.kind_id IS '资源唯一标识';


--
-- Name: COLUMN user_collect.source; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.source IS '收藏来源';


--
-- Name: COLUMN user_collect.remark; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.remark IS '收藏者备注';


--
-- Name: COLUMN user_collect.tag; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.tag IS '多个tag 用半角逗号分隔，单个tag不超过14字符';


--
-- Name: COLUMN user_collect.info; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.info IS '被收藏的kind的json信息';


--
-- Name: COLUMN user_collect.attach_file_hash256; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.attach_file_hash256 IS '收藏记录文件哈希,多个用逗号分割';


--
-- Name: COLUMN user_collect.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.status IS '状态: 0 禁用  1 启用';


--
-- Name: COLUMN user_collect.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN user_collect.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_collect.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: user_deletion_job; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_deletion_job (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    account character varying(80) DEFAULT ''::character varying NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    claimed_by character varying(120),
    claimed_at timestamp with time zone,
    finished_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_user_deletion_job_status CHECK ((status = ANY (ARRAY['pending'::text, 'running'::text, 'completed'::text, 'failed'::text])))
);


--
-- Name: user_deletion_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_deletion_request (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    status text DEFAULT 'requested'::text NOT NULL,
    requested_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    cancelled_at timestamp with time zone,
    approved_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT ck_user_deletion_request_status CHECK ((status = ANY (ARRAY['requested'::text, 'cancelled'::text, 'approved'::text])))
);


--
-- Name: user_denylist; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_denylist (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    denied_user_id bigint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE user_denylist; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_denylist IS '用户的拒绝聊天名单';


--
-- Name: COLUMN user_denylist.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_denylist.id IS '主键 自增长ID';


--
-- Name: COLUMN user_denylist.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_denylist.user_id IS '归属用户ID';


--
-- Name: COLUMN user_denylist.denied_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_denylist.denied_user_id IS '被列入名单的用户ID';


--
-- Name: COLUMN user_denylist.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_denylist.created_at IS '创建记录UTC时间';


--
-- Name: user_device; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_device (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    device_type character varying(40) DEFAULT ''::character varying,
    device_id character varying(128) NOT NULL,
    device_vsn text,
    device_name character varying(80),
    login_count integer DEFAULT 0 NOT NULL,
    last_login_ip character varying(40) DEFAULT ''::character varying NOT NULL,
    last_login_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    last_active_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    public_key character varying(2048) DEFAULT ''::character varying,
    key_id character varying(255) DEFAULT ''::character varying,
    capabilities text[] DEFAULT '{}'::text[] NOT NULL,
    trust_state character varying(20) DEFAULT 'unverified'::character varying NOT NULL,
    identity_blob jsonb,
    identity_signature text,
    identity_signed_at timestamp with time zone,
    device_generation integer DEFAULT 1 NOT NULL,
    identity_version integer DEFAULT 1 NOT NULL,
    CONSTRAINT chk_user_device_device_type CHECK (((device_type)::text = ANY ((ARRAY[''::character varying, 'web'::character varying, 'phone'::character varying, 'tablet'::character varying, 'ipad'::character varying, 'desktop'::character varying, 'watch'::character varying, 'ios'::character varying, 'android'::character varying, 'macos'::character varying, 'windows'::character varying, 'linux'::character varying])::text[]))),
    CONSTRAINT chk_user_device_trust_state CHECK (((trust_state)::text = ANY ((ARRAY['unverified'::character varying, 'verified'::character varying, 'revoked'::character varying])::text[])))
);


--
-- Name: TABLE user_device; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_device IS '用户登录设备表';


--
-- Name: COLUMN user_device.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.id IS '主键 自增长ID';


--
-- Name: COLUMN user_device.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.user_id IS '用户ID';


--
-- Name: COLUMN user_device.device_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.device_type IS '设备类型 web ios android macos windows';


--
-- Name: COLUMN user_device.device_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.device_id IS '设备ID web设备留空';


--
-- Name: COLUMN user_device.device_vsn; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.device_vsn IS '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}';


--
-- Name: COLUMN user_device.device_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.device_name IS '设备名称（用户可修改的）';


--
-- Name: COLUMN user_device.login_count; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.login_count IS '登陆次数';


--
-- Name: COLUMN user_device.last_login_ip; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.last_login_ip IS '最后登陆IP';


--
-- Name: COLUMN user_device.last_login_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.last_login_at IS '最后登录UTC时间';


--
-- Name: COLUMN user_device.last_active_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.last_active_at IS '最近活跃时间';


--
-- Name: COLUMN user_device.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN user_device.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.created_at IS '创建记录UTC时间';


--
-- Name: COLUMN user_device.public_key; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.public_key IS '用户设备公钥';


--
-- Name: COLUMN user_device.key_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.key_id IS 'E2EE 密钥版本标识符，用于标识密钥变更';


--
-- Name: COLUMN user_device.capabilities; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.capabilities IS '设备支持的协议套件短名数组（olm/megolm/rsa-oaep/mls）';


--
-- Name: COLUMN user_device.trust_state; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.trust_state IS '设备信任态，状态机见 ADR 06';


--
-- Name: COLUMN user_device.identity_blob; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.identity_blob IS '规范化的待签名设备身份负载（jsonb）';


--
-- Name: COLUMN user_device.identity_signature; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.identity_signature IS '对 identity_blob 的 Ed25519 签名（base64）';


--
-- Name: COLUMN user_device.identity_signed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.identity_signed_at IS '签名时间戳，用于轮换审计';


--
-- Name: COLUMN user_device.device_generation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.device_generation IS '物理设备重注册单调代数（+1，不回退），防旧设备重放，见 ADR 16 §3.3.1';


--
-- Name: COLUMN user_device.identity_version; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_device.identity_version IS '密码学身份轮换单调版本（+1，不回退），防身份键回退，见 ADR 16 §3.3.1';


--
-- Name: user_dnd_rule; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: COLUMN user_dnd_rule.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_dnd_rule.user_id IS '用户ID (TSID bigint)';


--
-- Name: user_dnd_rule_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.user_dnd_rule_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_dnd_rule_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.user_dnd_rule_id_seq OWNED BY public.user_dnd_rule.id;


--
-- Name: user_friend; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE user_friend; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_friend IS '聊天好友关系记录表（A请求B为好友，B接受之后，系统要自动加入一条B请求A的记录并且A自动确认 user_id 是 user表的主键）';


--
-- Name: COLUMN user_friend.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.id IS '主键 自增长ID';


--
-- Name: COLUMN user_friend.from_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.from_user_id IS '发起人 记录归属人ID';


--
-- Name: COLUMN user_friend.to_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.to_user_id IS '接受人朋友用户ID';


--
-- Name: COLUMN user_friend.category_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.category_id IS '用户分组ID friend_category主键';


--
-- Name: COLUMN user_friend.remark; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.remark IS '朋友备注名';


--
-- Name: COLUMN user_friend.tag; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.tag IS '给朋友的标签，多个tag 用半角逗号分隔，单个tag不超过14字符';


--
-- Name: COLUMN user_friend.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.status IS '状态: -1 删除  0 禁用  1 启用';


--
-- Name: COLUMN user_friend.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN user_friend.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN user_friend.setting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.setting IS '好友权限设置等信息';


--
-- Name: COLUMN user_friend.last_seen_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend.last_seen_at IS '用户最后在线时间';


--
-- Name: user_friend_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_friend_category (
    id bigint NOT NULL,
    name character varying(80) DEFAULT ''::character varying,
    owner_user_id bigint DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE user_friend_category; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_friend_category IS '朋友分组表';


--
-- Name: COLUMN user_friend_category.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend_category.id IS '主键 自增长ID';


--
-- Name: COLUMN user_friend_category.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend_category.name IS '分组名称';


--
-- Name: COLUMN user_friend_category.owner_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_friend_category.owner_user_id IS '分组所属用户ID';


--
-- Name: user_group; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE user_group; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_group IS '用户保存到通讯录的群';


--
-- Name: COLUMN user_group.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.id IS '主键 自增长ID';


--
-- Name: COLUMN user_group.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.user_id IS '用户ID';


--
-- Name: COLUMN user_group.group_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.group_id IS '群组ID';


--
-- Name: COLUMN user_group.remark; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.remark IS '群聊的备注仅自己可见';


--
-- Name: COLUMN user_group.setting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.setting IS '用户对群的一些配置';


--
-- Name: COLUMN user_group.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.status IS '状态: -1 删除  0 禁用  1 启用 ';


--
-- Name: COLUMN user_group.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN user_group.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: user_group_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_group_category (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    category_name character varying(50) NOT NULL,
    sort_order integer DEFAULT 0,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE user_group_category; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_group_category IS '群组分类表：用户自定义群组分组';


--
-- Name: COLUMN user_group_category.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group_category.id IS '主键，自增长ID';


--
-- Name: COLUMN user_group_category.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group_category.user_id IS '分类所属用户ID';


--
-- Name: COLUMN user_group_category.category_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group_category.category_name IS '分类名称';


--
-- Name: COLUMN user_group_category.sort_order; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group_category.sort_order IS '排序顺序，数字越小越靠前';


--
-- Name: COLUMN user_group_category.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_group_category.created_at IS '创建时间';


--
-- Name: user_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_log (
    ts timestamp with time zone DEFAULT CURRENT_TIMESTAMP(6) NOT NULL,
    type smallint NOT NULL,
    uid bigint DEFAULT 0 NOT NULL,
    body jsonb NOT NULL,
    remark character varying(200) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_user_log_type CHECK ((type = ANY (ARRAY[100, 102, 110, 130, 901, 902, 903])))
);


--
-- Name: TABLE user_log; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_log IS '用户日志表';


--
-- Name: COLUMN user_log.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_log.type IS '日志类型: 100 用户注销备份  102 用户注销申请记录  110 修改密码  130 个人数据导出';


--
-- Name: COLUMN user_log.uid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_log.uid IS '用户ID';


--
-- Name: COLUMN user_log.body; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_log.body IS '相关操作类型的json字符串数据';


--
-- Name: COLUMN user_log.remark; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_log.remark IS '备注';


--
-- Name: COLUMN user_log.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_log.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: user_setting; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_setting (
    user_id bigint NOT NULL,
    setting jsonb,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE user_setting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_setting IS '用户设置表';


--
-- Name: COLUMN user_setting.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_setting.user_id IS '主键 用户表自增长ID';


--
-- Name: COLUMN user_setting.setting; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_setting.setting IS '更多设置：json 数据，不同的业务不用的key( add_friend_type 加我方式： mobile 手机号; account 账号; qrcode 二维码; group 群聊; visit_card 名片)';


--
-- Name: COLUMN user_setting.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_setting.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: user_tag; Type: TABLE; Schema: public; Owner: -
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


--
-- Name: TABLE user_tag; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_tag IS 'tag记录表';


--
-- Name: COLUMN user_tag.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.id IS '主键 自增长ID';


--
-- Name: COLUMN user_tag.creator_user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.creator_user_id IS '创建人用户ID';


--
-- Name: COLUMN user_tag.scene; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';


--
-- Name: COLUMN user_tag.name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.name IS '标签名称';


--
-- Name: COLUMN user_tag.referer_time; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.referer_time IS '被引用次数 关联object_id 数量';


--
-- Name: COLUMN user_tag.updated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: COLUMN user_tag.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: user_tag_relation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_tag_relation (
    id bigint NOT NULL,
    scene integer DEFAULT 0 NOT NULL,
    user_id bigint DEFAULT 0 NOT NULL,
    tag_id bigint DEFAULT 0 NOT NULL,
    object_id character varying(40) DEFAULT ''::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE user_tag_relation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.user_tag_relation IS '用户标签记录表 scene 场景下，user_id 给 object_id 打了标签 tag_id';


--
-- Name: COLUMN user_tag_relation.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag_relation.id IS '主键 自增长ID';


--
-- Name: COLUMN user_tag_relation.scene; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag_relation.scene IS '标签应用场景 1  用户收藏记录标签  2 用户朋友标签';


--
-- Name: COLUMN user_tag_relation.user_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag_relation.user_id IS '记录所属用户ID';


--
-- Name: COLUMN user_tag_relation.tag_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag_relation.tag_id IS '标签ID public.tag 表的自增长ID';


--
-- Name: COLUMN user_tag_relation.object_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag_relation.object_id IS '被打标签收藏类型ID （kind_id） or 被打标签用户ID (int 型用户ID)';


--
-- Name: COLUMN user_tag_relation.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.user_tag_relation.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';


--
-- Name: v_channel_invitation_stats; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_channel_invitation_stats AS
 SELECT channel_id,
    count(*) FILTER (WHERE (status = 0)) AS pending_count,
    count(*) FILTER (WHERE (status = 1)) AS accepted_count,
    count(*) FILTER (WHERE (status = 2)) AS rejected_count,
    count(*) FILTER (WHERE (status = 3)) AS expired_count,
    count(*) AS total_count,
    max(created_at) AS last_invitation_at
   FROM public.channel_invitation
  GROUP BY channel_id;


--
-- Name: VIEW v_channel_invitation_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_channel_invitation_stats IS '频道邀请统计视图';


--
-- Name: v_channel_order_stats; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_channel_order_stats AS
 SELECT channel_id,
    count(*) FILTER (WHERE (status = 0)) AS pending_count,
    count(*) FILTER (WHERE (status = 1)) AS paid_count,
    count(*) FILTER (WHERE (status = 2)) AS refunded_count,
    COALESCE(sum(amount) FILTER (WHERE (status = 1)), (0)::numeric) AS total_revenue,
    COALESCE(sum(amount) FILTER (WHERE ((status = 1) AND (created_at > (now() - '30 days'::interval)))), (0)::numeric) AS revenue_30d,
    max(created_at) AS last_order_at
   FROM public.channel_order
  GROUP BY channel_id;


--
-- Name: VIEW v_channel_order_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_channel_order_stats IS '频道订单统计视图';


--
-- Name: v_channel_realtime_stats; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_channel_realtime_stats AS
 SELECT c.id AS channel_id,
    c.name AS channel_name,
    c.subscriber_count,
    count(DISTINCT cm.id) AS total_messages,
    COALESCE(sum(cm.view_count), (0)::numeric) AS total_views,
    COALESCE(( SELECT count(*) AS count
           FROM public.channel_reaction cr
          WHERE (cr.channel_id = c.id)), (0)::bigint) AS total_reactions,
    max(cm.created_at) AS last_message_at
   FROM (public.channel c
     LEFT JOIN public.channel_message cm ON (((c.id = cm.channel_id) AND (cm.status = 1))))
  WHERE (c.status = 1)
  GROUP BY c.id, c.name, c.subscriber_count;


--
-- Name: VIEW v_channel_realtime_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_channel_realtime_stats IS '频道实时统计视图';


--
-- Name: v_datacenters; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_datacenters AS
 SELECT id,
    name,
    region,
    api_endpoint,
    is_active,
    created_at,
    updated_at,
    active_tables_count
   FROM public.get_datacenters(true) get_datacenters(id, name, region, api_endpoint, is_active, created_at, updated_at, active_tables_count);


--
-- Name: VIEW v_datacenters; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_datacenters IS '活跃机房列表视图';


--
-- Name: v_efapi_attachment_purgeable; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_efapi_attachment_purgeable AS
 SELECT attachment_id,
    organization_id,
    application_id,
    retention_until
   FROM public.enterprise_attachment_retention ar
  WHERE ((purge_state = 'live'::text) AND (hold_state = 'none'::text) AND (CURRENT_TIMESTAMP >= retention_until));


--
-- Name: v_enterprise_effective_application_grant; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_enterprise_effective_application_grant AS
 SELECT organization_id,
    application_id,
    id AS grant_id,
    workspace_scope_kind,
    version,
    valid_from,
    expires_at
   FROM public.enterprise_application_grant g
  WHERE ((status = 'active'::text) AND (valid_from <= CURRENT_TIMESTAMP) AND (expires_at > CURRENT_TIMESTAMP));


--
-- Name: VIEW v_enterprise_effective_application_grant; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_enterprise_effective_application_grant IS '当前生效的 Application Grant 行（active 且落在 valid_from/expires_at 内）；读时求值，撤权/到期下一次查询即消失';


--
-- Name: v_enterprise_effective_application_grant_scope; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_enterprise_effective_application_grant_scope AS
 SELECT g.organization_id,
    g.application_id,
    g.id AS grant_id,
    s.scope
   FROM (public.enterprise_application_grant g
     JOIN public.enterprise_application_grant_scope s ON ((s.grant_id = g.id)))
  WHERE ((g.status = 'active'::text) AND (g.valid_from <= CURRENT_TIMESTAMP) AND (g.expires_at > CURRENT_TIMESTAMP));


--
-- Name: VIEW v_enterprise_effective_application_grant_scope; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_enterprise_effective_application_grant_scope IS '当前生效的 (Grant, scope) 行：scope 授权求值（App allowed_scopes ∩ 生效 Grant scopes）的唯一读取面；读时求值，撤权/到期/降级下一次查询即反映';


--
-- Name: v_group_admins; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_group_admins AS
 SELECT gm.group_id,
    gm.user_id,
    gm.role,
    u.nickname,
    u.avatar,
    gm.created_at
   FROM (public.group_member gm
     LEFT JOIN public."user" u ON ((u.id = gm.user_id)))
  WHERE ((gm.role = ANY (ARRAY[3, 4, 5])) AND (gm.status = 1))
  ORDER BY gm.group_id, gm.role DESC, gm.created_at;


--
-- Name: v_group_senior_admins; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_group_senior_admins AS
 SELECT gm.group_id,
    gm.user_id,
    gm.role,
    u.nickname,
    u.avatar,
    gm.created_at
   FROM (public.group_member gm
     LEFT JOIN public."user" u ON ((u.id = gm.user_id)))
  WHERE ((gm.role = ANY (ARRAY[4, 5])) AND (gm.status = 1))
  ORDER BY gm.group_id, gm.role DESC, gm.created_at;


--
-- Name: v_id_segment_monitor; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_id_segment_monitor AS
 SELECT datacenter_id,
    datacenter_name,
    datacenter_region,
    table_name,
    segment_start,
    segment_end,
    total_size,
    used_count,
    usage_percent,
    is_active,
    allocated_at,
    expired_at,
    remaining_days,
    status
   FROM public.get_datacenter_segment_status() get_datacenter_segment_status(datacenter_id, datacenter_name, datacenter_region, table_name, segment_start, segment_end, total_size, used_count, usage_percent, is_active, allocated_at, expired_at, remaining_days, status);


--
-- Name: VIEW v_id_segment_monitor; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_id_segment_monitor IS 'ID段监控视图';


--
-- Name: v_user_channel_reading_stats; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.v_user_channel_reading_stats AS
 SELECT cs.user_id,
    cs.channel_id,
    c.name AS channel_name,
    cs.unread_count,
    cs.last_read_at,
    count(DISTINCT cmv.message_id) AS viewed_messages,
    max(cmv.viewed_at) AS last_view_at
   FROM ((public.channel_subscription cs
     JOIN public.channel c ON ((cs.channel_id = c.id)))
     LEFT JOIN public.channel_message_view cmv ON (((cs.channel_id = cmv.channel_id) AND (cs.user_id = cmv.user_id))))
  WHERE (cs.status = 1)
  GROUP BY cs.user_id, cs.channel_id, c.name, cs.unread_count, cs.last_read_at;


--
-- Name: VIEW v_user_channel_reading_stats; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON VIEW public.v_user_channel_reading_stats IS '用户频道阅读统计视图';


--
-- Name: verification_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.verification_code (
    id character varying(80) NOT NULL,
    code character varying(40) DEFAULT ''::character varying NOT NULL,
    validity_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: TABLE verification_code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.verification_code IS '验证码记录表';


--
-- Name: COLUMN verification_code.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.verification_code.id IS '主键 唯一标示';


--
-- Name: COLUMN verification_code.code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.verification_code.code IS '随机验证码';


--
-- Name: COLUMN verification_code.validity_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.verification_code.validity_at IS '有效期截止时间';


--
-- Name: COLUMN verification_code.created_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.verification_code.created_at IS '创建记录UTC时间';


--
-- Name: wallet; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.wallet (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    balance bigint DEFAULT 0 NOT NULL,
    frozen bigint DEFAULT 0 NOT NULL,
    version integer DEFAULT 0 NOT NULL,
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_wallet_balance CHECK ((balance >= 0)),
    CONSTRAINT chk_wallet_frozen CHECK ((frozen >= 0)),
    CONSTRAINT chk_wallet_frozen_le_balance CHECK ((frozen <= balance)),
    CONSTRAINT chk_wallet_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);


--
-- Name: wallet_transaction; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.wallet_transaction (
    id bigint NOT NULL,
    wallet_id bigint NOT NULL,
    user_id bigint NOT NULL,
    amount bigint NOT NULL,
    balance_after bigint NOT NULL,
    tx_type smallint DEFAULT 1 NOT NULL,
    reference_no character varying(64) DEFAULT ''::character varying,
    remark character varying(200) DEFAULT ''::character varying,
    status smallint DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_wallet_tx_status CHECK ((status = ANY (ARRAY[0, 1, 2]))),
    CONSTRAINT chk_wallet_tx_type CHECK ((tx_type = ANY (ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 20, 21])))
);


--
-- Name: workspace; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.workspace (
    id bigint NOT NULL,
    name character varying(200) NOT NULL,
    logo text DEFAULT ''::text NOT NULL,
    owner_id bigint NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    archived_at timestamp with time zone,
    archived_by bigint,
    type text DEFAULT 'project'::text NOT NULL,
    branding jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    organization_id bigint,
    version bigint DEFAULT 1 NOT NULL,
    CONSTRAINT chk_workspace_status CHECK ((status = ANY (ARRAY['active'::text, 'archived'::text]))),
    CONSTRAINT workspace_version_check CHECK ((version > 0))
);


--
-- Name: TABLE workspace; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.workspace IS '工作区（组织/业务边界：管人和权限；I11 无 Organization 层）';


--
-- Name: COLUMN workspace.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.id IS '主键 TSID';


--
-- Name: COLUMN workspace.owner_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.owner_id IS '主 Owner 用户ID / 未来计费归属锚点（本期只读不计费）';


--
-- Name: COLUMN workspace.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.status IS '生命周期: active 正常 | archived 已归档（归档后写守卫只读）';


--
-- Name: COLUMN workspace.archived_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.archived_at IS '归档时间（T7 归档审计）';


--
-- Name: COLUMN workspace.archived_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.archived_by IS '归档操作人（T7 归档审计）';


--
-- Name: COLUMN workspace.type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.type IS '工作区类型，默认 project；预留扩展，本期无值域约束';


--
-- Name: COLUMN workspace.branding; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.branding IS '品牌配置 jsonb：{name,logo,favicon,primaryColor}，仅 Workspace 视图作用域（T12）';


--
-- Name: COLUMN workspace.organization_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace.organization_id IS '所属机构ID；可空=尚未加入任何机构（expand-first 兼容历史 Workspace，不按 owner_id 自动归属）；一个 Workspace 最多属于一个 Organization';


--
-- Name: workspace_group_read_cursor; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.workspace_group_read_cursor (
    generation_id bigint NOT NULL,
    read_seq bigint NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT workspace_group_read_cursor_read_seq_check CHECK ((read_seq >= 1))
);


--
-- Name: workspace_invite; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.workspace_invite (
    id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    code text NOT NULL,
    created_by bigint,
    expires_at timestamp with time zone NOT NULL,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT chk_workspace_invite_status CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text])))
);


--
-- Name: TABLE workspace_invite; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.workspace_invite IS '工作区团队码（可复用加入凭证：一码多人、7 天有效、Owner 可撤销）';


--
-- Name: COLUMN workspace_invite.id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_invite.id IS '主键 TSID';


--
-- Name: COLUMN workspace_invite.workspace_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_invite.workspace_id IS '所属工作区ID';


--
-- Name: COLUMN workspace_invite.code; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_invite.code IS '团队码（8 位 A-Z2-9，排除 0/O/1/I；全局唯一，撤销后作废不复用）';


--
-- Name: COLUMN workspace_invite.created_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_invite.created_by IS '生成人用户ID（NULL=系统生成）';


--
-- Name: COLUMN workspace_invite.expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_invite.expires_at IS '过期时间（生成时刻 + 7 天；过期后 join 返回 982）';


--
-- Name: COLUMN workspace_invite.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_invite.status IS '状态: active 有效 | revoked 已撤销（Owner 撤销，join 返回 981）';


--
-- Name: workspace_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.workspace_member (
    workspace_id bigint NOT NULL,
    user_id bigint NOT NULL,
    role text DEFAULT 'member'::text NOT NULL,
    invited_by bigint,
    joined_at timestamp with time zone,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_workspace_member_role CHECK ((role = ANY (ARRAY['owner'::text, 'member'::text, 'guest'::text]))),
    CONSTRAINT chk_workspace_member_status CHECK ((status = ANY (ARRAY['active'::text, 'removed'::text])))
);


--
-- Name: TABLE workspace_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.workspace_member IS '工作区成员（组织访问边界；三角色 Owner/Member/Guest；区别于群成员/频道订阅者）';


--
-- Name: COLUMN workspace_member.role; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_member.role IS '角色: owner 群主级治理 | member 普通成员 | guest 只读访客（无第四种角色）';


--
-- Name: COLUMN workspace_member.invited_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_member.invited_by IS '邀请人用户ID（NULL=Owner 创建工作区时的初始记录）';


--
-- Name: COLUMN workspace_member.joined_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_member.joined_at IS '实际加入时间（邀请确认后写入）';


--
-- Name: COLUMN workspace_member.status; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.workspace_member.status IS '状态: active 在册 | removed 已移除（软删，重新加入不自动恢复下级关系）';


--
-- Name: moya_subscribe_grant id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moya_subscribe_grant ALTER COLUMN id SET DEFAULT nextval('public.moya_subscribe_grant_id_seq'::regclass);


--
-- Name: system_datacenter id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_datacenter ALTER COLUMN id SET DEFAULT nextval('public.system_datacenter_id_seq'::regclass);


--
-- Name: trust_audit id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trust_audit ALTER COLUMN id SET DEFAULT nextval('public.trust_audit_id_seq'::regclass);


--
-- Name: user_dnd_rule id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_dnd_rule ALTER COLUMN id SET DEFAULT nextval('public.user_dnd_rule_id_seq'::regclass);


--
-- Name: adm_auth_epoch adm_auth_epoch_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.adm_auth_epoch
    ADD CONSTRAINT adm_auth_epoch_pkey PRIMARY KEY (admin_id);


--
-- Name: adm_role adm_role_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.adm_role
    ADD CONSTRAINT adm_role_pkey PRIMARY KEY (id);


--
-- Name: adm_user adm_user_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.adm_user
    ADD CONSTRAINT adm_user_pkey PRIMARY KEY (id);


--
-- Name: admin_operation_logs admin_operation_logs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.admin_operation_logs
    ADD CONSTRAINT admin_operation_logs_pkey PRIMARY KEY (id);


--
-- Name: agent_hub_audit agent_hub_audit_corr_entity_uq; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_hub_audit
    ADD CONSTRAINT agent_hub_audit_corr_entity_uq UNIQUE (correlation_id, entity_id);


--
-- Name: agent_hub_audit agent_hub_audit_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_hub_audit
    ADD CONSTRAINT agent_hub_audit_pkey PRIMARY KEY (entity_id);


--
-- Name: agent_payment_compensation agent_payment_compensation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_payment_compensation
    ADD CONSTRAINT agent_payment_compensation_pkey PRIMARY KEY (id);


--
-- Name: agent_payment_mandate agent_payment_mandate_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_payment_mandate
    ADD CONSTRAINT agent_payment_mandate_pkey PRIMARY KEY (id);


--
-- Name: agent_task_decision agent_task_decision_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_task_decision
    ADD CONSTRAINT agent_task_decision_pkey PRIMARY KEY (id);


--
-- Name: agent_task_event agent_task_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_task_event
    ADD CONSTRAINT agent_task_event_pkey PRIMARY KEY (id);


--
-- Name: agent_task agent_task_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_task
    ADD CONSTRAINT agent_task_pkey PRIMARY KEY (id);


--
-- Name: ai_agent ai_agent_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ai_agent
    ADD CONSTRAINT ai_agent_pkey PRIMARY KEY (user_id);


--
-- Name: ai_agent_role ai_agent_role_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ai_agent_role
    ADD CONSTRAINT ai_agent_role_pkey PRIMARY KEY (code);


--
-- Name: ai_agent_role_version ai_agent_role_version_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ai_agent_role_version
    ADD CONSTRAINT ai_agent_role_version_pkey PRIMARY KEY (id);


--
-- Name: ai_agent_role_version ai_agent_role_version_role_code_version_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ai_agent_role_version
    ADD CONSTRAINT ai_agent_role_version_role_code_version_key UNIQUE (role_code, version);


--
-- Name: announcement announcement_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.announcement
    ADD CONSTRAINT announcement_pkey PRIMARY KEY (id);


--
-- Name: app_ddl app_ddl_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_ddl
    ADD CONSTRAINT app_ddl_pkey PRIMARY KEY (id);


--
-- Name: app_upgrade_log app_upgrade_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_upgrade_log
    ADD CONSTRAINT app_upgrade_log_pkey PRIMARY KEY (id);


--
-- Name: app_version app_version_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_version
    ADD CONSTRAINT app_version_pkey PRIMARY KEY (id);


--
-- Name: app_version_policy app_version_policy_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_version_policy
    ADD CONSTRAINT app_version_policy_pkey PRIMARY KEY (id);


--
-- Name: attach_pending attach_pending_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attach_pending
    ADD CONSTRAINT attach_pending_pkey PRIMARY KEY (object_key);


--
-- Name: attachment attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (id);


--
-- Name: billing_invoice billing_invoice_invoice_no_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.billing_invoice
    ADD CONSTRAINT billing_invoice_invoice_no_key UNIQUE (invoice_no);


--
-- Name: billing_invoice billing_invoice_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.billing_invoice
    ADD CONSTRAINT billing_invoice_pkey PRIMARY KEY (id);


--
-- Name: billing_plan billing_plan_code_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.billing_plan
    ADD CONSTRAINT billing_plan_code_key UNIQUE (code);


--
-- Name: billing_plan billing_plan_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.billing_plan
    ADD CONSTRAINT billing_plan_pkey PRIMARY KEY (id);


--
-- Name: billing_subscription billing_subscription_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.billing_subscription
    ADD CONSTRAINT billing_subscription_pkey PRIMARY KEY (id);


--
-- Name: billing_usage billing_usage_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.billing_usage
    ADD CONSTRAINT billing_usage_pkey PRIMARY KEY (id);


--
-- Name: bot bot_api_token_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot
    ADD CONSTRAINT bot_api_token_key UNIQUE (api_token);


--
-- Name: bot_delivery_attempt bot_delivery_attempt_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_delivery_attempt
    ADD CONSTRAINT bot_delivery_attempt_pkey PRIMARY KEY (id);


--
-- Name: bot_delivery bot_delivery_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_delivery
    ADD CONSTRAINT bot_delivery_pkey PRIMARY KEY (delivery_id);


--
-- Name: bot_oauth_grant bot_oauth_grant_access_token_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_oauth_grant
    ADD CONSTRAINT bot_oauth_grant_access_token_key UNIQUE (access_token);


--
-- Name: bot_oauth_grant bot_oauth_grant_bot_id_user_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_oauth_grant
    ADD CONSTRAINT bot_oauth_grant_bot_id_user_id_key UNIQUE (bot_id, user_id);


--
-- Name: bot_oauth_grant bot_oauth_grant_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_oauth_grant
    ADD CONSTRAINT bot_oauth_grant_pkey PRIMARY KEY (id);


--
-- Name: bot bot_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot
    ADD CONSTRAINT bot_pkey PRIMARY KEY (user_id);


--
-- Name: bot bot_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot
    ADD CONSTRAINT bot_username_key UNIQUE (username);


--
-- Name: channel_admin channel_admin_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT channel_admin_pkey PRIMARY KEY (id);


--
-- Name: channel_category channel_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_category
    ADD CONSTRAINT channel_category_pkey PRIMARY KEY (id);


--
-- Name: channel_comment channel_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_comment
    ADD CONSTRAINT channel_comment_pkey PRIMARY KEY (id);


--
-- Name: channel_invitation channel_invitation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_invitation
    ADD CONSTRAINT channel_invitation_pkey PRIMARY KEY (id);


--
-- Name: channel_message channel_message_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message
    ADD CONSTRAINT channel_message_pkey PRIMARY KEY (id);


--
-- Name: channel_message_view channel_message_view_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT channel_message_view_pkey PRIMARY KEY (id);


--
-- Name: channel_order channel_order_order_no_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_order
    ADD CONSTRAINT channel_order_order_no_key UNIQUE (order_no);


--
-- Name: channel_order channel_order_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_order
    ADD CONSTRAINT channel_order_pkey PRIMARY KEY (id);


--
-- Name: channel channel_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel
    ADD CONSTRAINT channel_pkey PRIMARY KEY (id);


--
-- Name: channel_price channel_price_channel_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_price
    ADD CONSTRAINT channel_price_channel_id_key UNIQUE (channel_id);


--
-- Name: channel_price channel_price_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_price
    ADD CONSTRAINT channel_price_pkey PRIMARY KEY (id);


--
-- Name: channel_reaction channel_reaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT channel_reaction_pkey PRIMARY KEY (id);


--
-- Name: channel_stats_daily channel_stats_daily_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_stats_daily
    ADD CONSTRAINT channel_stats_daily_pkey PRIMARY KEY (id);


--
-- Name: channel_subscription channel_subscription_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT channel_subscription_pkey PRIMARY KEY (id);


--
-- Name: channel_webhook channel_webhook_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_webhook
    ADD CONSTRAINT channel_webhook_pkey PRIMARY KEY (id);


--
-- Name: compliance_key compliance_key_key_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.compliance_key
    ADD CONSTRAINT compliance_key_key_id_key UNIQUE (key_id);


--
-- Name: compliance_key compliance_key_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.compliance_key
    ADD CONSTRAINT compliance_key_pkey PRIMARY KEY (id);


--
-- Name: config config_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.config
    ADD CONSTRAINT config_pkey PRIMARY KEY (key);


--
-- Name: conversation_delete conversation_delete_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.conversation_delete
    ADD CONSTRAINT conversation_delete_pkey PRIMARY KEY (id);


--
-- Name: conversation_delete conversation_delete_user_conversation_uk; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.conversation_delete
    ADD CONSTRAINT conversation_delete_user_conversation_uk UNIQUE (user_id, conversation_id, conversation_type);


--
-- Name: conversation_pin conversation_pin_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.conversation_pin
    ADD CONSTRAINT conversation_pin_pkey PRIMARY KEY (id);


--
-- Name: conversation_pin conversation_pin_user_conversation_uk; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.conversation_pin
    ADD CONSTRAINT conversation_pin_user_conversation_uk UNIQUE (user_id, conversation_id, conversation_type);


--
-- Name: conversation conversation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.conversation
    ADD CONSTRAINT conversation_pkey PRIMARY KEY (id);


--
-- Name: e2ee_group_session_attestation e2ee_group_session_attestation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_group_session_attestation
    ADD CONSTRAINT e2ee_group_session_attestation_pkey PRIMARY KEY (group_id, session_id);


--
-- Name: e2ee_group_session_attestation e2ee_group_session_attestation_room_key_msg_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_group_session_attestation
    ADD CONSTRAINT e2ee_group_session_attestation_room_key_msg_id_key UNIQUE (room_key_msg_id);


--
-- Name: e2ee_group_session_attestation e2ee_group_session_attestation_session_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_group_session_attestation
    ADD CONSTRAINT e2ee_group_session_attestation_session_id_key UNIQUE (session_id);


--
-- Name: e2ee_group_session_member e2ee_group_session_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_group_session_member
    ADD CONSTRAINT e2ee_group_session_member_pkey PRIMARY KEY (group_id, session_id, user_id);


--
-- Name: e2ee_key_backups e2ee_key_backups_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_key_backups
    ADD CONSTRAINT e2ee_key_backups_pkey PRIMARY KEY (id);


--
-- Name: e2ee_key_shares e2ee_key_shares_owner_uid_trustee_uid_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_key_shares
    ADD CONSTRAINT e2ee_key_shares_owner_uid_trustee_uid_key UNIQUE (owner_uid, trustee_uid);


--
-- Name: e2ee_key_shares e2ee_key_shares_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_key_shares
    ADD CONSTRAINT e2ee_key_shares_pkey PRIMARY KEY (id);


--
-- Name: enterprise_asset_delete_queue enterprise_asset_delete_queue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset_delete_queue
    ADD CONSTRAINT enterprise_asset_delete_queue_pkey PRIMARY KEY (organization_id, workspace_id, asset_id);


--
-- Name: feedback feedback_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feedback
    ADD CONSTRAINT feedback_pkey PRIMARY KEY (id);


--
-- Name: feedback_reply feedback_reply_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feedback_reply
    ADD CONSTRAINT feedback_reply_pkey PRIMARY KEY (id);


--
-- Name: fts_channel fts_channel_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fts_channel
    ADD CONSTRAINT fts_channel_pkey PRIMARY KEY (channel_id);


--
-- Name: fts_group fts_group_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fts_group
    ADD CONSTRAINT fts_group_pkey PRIMARY KEY (group_id);


--
-- Name: group_album group_album_album_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album
    ADD CONSTRAINT group_album_album_id_key UNIQUE (album_id);


--
-- Name: group_album_photo_comment group_album_photo_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album_photo_comment
    ADD CONSTRAINT group_album_photo_comment_pkey PRIMARY KEY (id);


--
-- Name: group_album_photo_like group_album_photo_like_photo_id_user_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album_photo_like
    ADD CONSTRAINT group_album_photo_like_photo_id_user_id_key UNIQUE (photo_id, user_id);


--
-- Name: group_album_photo_like group_album_photo_like_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album_photo_like
    ADD CONSTRAINT group_album_photo_like_pkey PRIMARY KEY (id);


--
-- Name: group_album_photo group_album_photo_photo_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album_photo
    ADD CONSTRAINT group_album_photo_photo_id_key UNIQUE (photo_id);


--
-- Name: group_album_photo group_album_photo_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album_photo
    ADD CONSTRAINT group_album_photo_pkey PRIMARY KEY (id);


--
-- Name: group_album group_album_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_album
    ADD CONSTRAINT group_album_pkey PRIMARY KEY (id);


--
-- Name: group_category group_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_category
    ADD CONSTRAINT group_category_pkey PRIMARY KEY (id);


--
-- Name: group_file group_file_file_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_file
    ADD CONSTRAINT group_file_file_id_key UNIQUE (file_id);


--
-- Name: group_file group_file_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_file
    ADD CONSTRAINT group_file_pkey PRIMARY KEY (id);


--
-- Name: group_log group_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_log
    ADD CONSTRAINT group_log_pkey PRIMARY KEY (id);


--
-- Name: group_member_generation group_member_generation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_member_generation
    ADD CONSTRAINT group_member_generation_pkey PRIMARY KEY (id);


--
-- Name: group_member group_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_member
    ADD CONSTRAINT group_member_pkey PRIMARY KEY (id);


--
-- Name: group_notice group_notice_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_notice
    ADD CONSTRAINT group_notice_pkey PRIMARY KEY (id);


--
-- Name: group group_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public."group"
    ADD CONSTRAINT group_pkey PRIMARY KEY (id);


--
-- Name: group_random_code group_random_code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_random_code
    ADD CONSTRAINT group_random_code_pkey PRIMARY KEY (id);


--
-- Name: group_schedule_participant group_schedule_participant_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_schedule_participant
    ADD CONSTRAINT group_schedule_participant_pkey PRIMARY KEY (id);


--
-- Name: group_schedule_participant group_schedule_participant_schedule_id_user_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_schedule_participant
    ADD CONSTRAINT group_schedule_participant_schedule_id_user_id_key UNIQUE (schedule_id, user_id);


--
-- Name: group_schedule group_schedule_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_schedule
    ADD CONSTRAINT group_schedule_pkey PRIMARY KEY (id);


--
-- Name: group_schedule_remind group_schedule_remind_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_schedule_remind
    ADD CONSTRAINT group_schedule_remind_pkey PRIMARY KEY (id);


--
-- Name: group_schedule group_schedule_schedule_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_schedule
    ADD CONSTRAINT group_schedule_schedule_id_key UNIQUE (schedule_id);


--
-- Name: group_tag group_tag_group_id_tag_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_tag
    ADD CONSTRAINT group_tag_group_id_tag_name_key UNIQUE (group_id, tag_name);


--
-- Name: group_tag group_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_tag
    ADD CONSTRAINT group_tag_pkey PRIMARY KEY (id);


--
-- Name: group_task_assignment group_task_assignment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_task_assignment
    ADD CONSTRAINT group_task_assignment_pkey PRIMARY KEY (id);


--
-- Name: group_task group_task_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_task
    ADD CONSTRAINT group_task_pkey PRIMARY KEY (id);


--
-- Name: group_task group_task_task_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_task
    ADD CONSTRAINT group_task_task_id_key UNIQUE (task_id);


--
-- Name: group_vote_option group_vote_option_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_vote_option
    ADD CONSTRAINT group_vote_option_pkey PRIMARY KEY (id);


--
-- Name: group_vote_option group_vote_option_vote_id_option_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_vote_option
    ADD CONSTRAINT group_vote_option_vote_id_option_id_key UNIQUE (vote_id, option_id);


--
-- Name: group_vote group_vote_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_vote
    ADD CONSTRAINT group_vote_pkey PRIMARY KEY (id);


--
-- Name: group_vote_record group_vote_record_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_vote_record
    ADD CONSTRAINT group_vote_record_pkey PRIMARY KEY (id);


--
-- Name: group_vote_record group_vote_record_vote_id_user_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_vote_record
    ADD CONSTRAINT group_vote_record_vote_id_user_id_key UNIQUE (vote_id, user_id);


--
-- Name: group_vote group_vote_vote_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_vote
    ADD CONSTRAINT group_vote_vote_id_key UNIQUE (vote_id);


--
-- Name: live_room live_room_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_room
    ADD CONSTRAINT live_room_pkey PRIMARY KEY (id);


--
-- Name: live_room live_room_stream_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_room
    ADD CONSTRAINT live_room_stream_key_key UNIQUE (stream_key);


--
-- Name: mcp_audit_log mcp_audit_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mcp_audit_log
    ADD CONSTRAINT mcp_audit_log_pkey PRIMARY KEY (id);


--
-- Name: mcp_client_grant mcp_client_grant_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mcp_client_grant
    ADD CONSTRAINT mcp_client_grant_pkey PRIMARY KEY (id);


--
-- Name: mcp_client mcp_client_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mcp_client
    ADD CONSTRAINT mcp_client_pkey PRIMARY KEY (client_id);


--
-- Name: moment_comment moment_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT moment_comment_pkey PRIMARY KEY (id);


--
-- Name: moment_like moment_like_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT moment_like_pkey PRIMARY KEY (id);


--
-- Name: moment_post_acl moment_post_acl_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT moment_post_acl_pkey PRIMARY KEY (id);


--
-- Name: moment_post moment_post_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_post
    ADD CONSTRAINT moment_post_pkey PRIMARY KEY (id);


--
-- Name: moment_report moment_report_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT moment_report_pkey PRIMARY KEY (id);


--
-- Name: moment_timeline moment_timeline_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT moment_timeline_pkey PRIMARY KEY (id);


--
-- Name: moya_subscribe_grant moya_subscribe_grant_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moya_subscribe_grant
    ADD CONSTRAINT moya_subscribe_grant_pkey PRIMARY KEY (id);


--
-- Name: msg_c2c msg_c2c_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_c2c
    ADD CONSTRAINT msg_c2c_pkey PRIMARY KEY (id, created_at);


--
-- Name: msg_c2g msg_c2g_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_c2g
    ADD CONSTRAINT msg_c2g_pkey PRIMARY KEY (id, created_at);


--
-- Name: msg_c2g_recipient_snapshot msg_c2g_recipient_snapshot_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_c2g_recipient_snapshot
    ADD CONSTRAINT msg_c2g_recipient_snapshot_pkey PRIMARY KEY (msg_id);


--
-- Name: msg_c2g_request_ledger msg_c2g_request_ledger_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_c2g_request_ledger
    ADD CONSTRAINT msg_c2g_request_ledger_pkey PRIMARY KEY (msg_id);


--
-- Name: msg_c2s msg_c2s_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_c2s
    ADD CONSTRAINT msg_c2s_pkey PRIMARY KEY (id, created_at);


--
-- Name: msg_delivery msg_delivery_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_delivery
    ADD CONSTRAINT msg_delivery_pkey PRIMARY KEY (msg_kind, msg_id, to_uid, to_did);


--
-- Name: msg_forward msg_forward_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_forward
    ADD CONSTRAINT msg_forward_pkey PRIMARY KEY (id);


--
-- Name: msg_mention msg_mention_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_mention
    ADD CONSTRAINT msg_mention_pkey PRIMARY KEY (id);


--
-- Name: msg_reaction msg_reaction_msg_id_msg_type_user_id_emoji_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_reaction
    ADD CONSTRAINT msg_reaction_msg_id_msg_type_user_id_emoji_key UNIQUE (msg_id, msg_type, user_id, emoji);


--
-- Name: msg_reaction msg_reaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_reaction
    ADD CONSTRAINT msg_reaction_pkey PRIMARY KEY (id);


--
-- Name: msg_s2c msg_s2c_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_s2c
    ADD CONSTRAINT msg_s2c_pkey PRIMARY KEY (id, created_at);


--
-- Name: msg_store msg_store_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_store
    ADD CONSTRAINT msg_store_pkey PRIMARY KEY (id, created_at);


--
-- Name: msg_store_seq msg_store_seq_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_store_seq
    ADD CONSTRAINT msg_store_seq_pkey PRIMARY KEY (conv_key);


--
-- Name: msg_store_staging msg_store_staging_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_store_staging
    ADD CONSTRAINT msg_store_staging_pkey PRIMARY KEY (id);


--
-- Name: msg_store_staging msg_store_staging_type_msg_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_store_staging
    ADD CONSTRAINT msg_store_staging_type_msg_id_key UNIQUE (type, msg_id);


--
-- Name: msg_topic msg_topic_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_topic
    ADD CONSTRAINT msg_topic_pkey PRIMARY KEY (id);


--
-- Name: olm_fallback_key olm_fallback_key_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.olm_fallback_key
    ADD CONSTRAINT olm_fallback_key_pkey PRIMARY KEY (id);


--
-- Name: olm_fallback_key olm_fallback_key_user_device; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.olm_fallback_key
    ADD CONSTRAINT olm_fallback_key_user_device UNIQUE (user_id, device_id);


--
-- Name: olm_identity olm_identity_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.olm_identity
    ADD CONSTRAINT olm_identity_pkey PRIMARY KEY (id);


--
-- Name: olm_identity olm_identity_user_device_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.olm_identity
    ADD CONSTRAINT olm_identity_user_device_key UNIQUE (user_id, device_id);


--
-- Name: olm_one_time_key olm_one_time_key_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.olm_one_time_key
    ADD CONSTRAINT olm_one_time_key_pkey PRIMARY KEY (id);


--
-- Name: olm_one_time_key olm_one_time_key_user_device_keyid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.olm_one_time_key
    ADD CONSTRAINT olm_one_time_key_user_device_keyid UNIQUE (user_id, device_id, key_id);


--
-- Name: organization_invite_code organization_invite_code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invite_code
    ADD CONSTRAINT organization_invite_code_pkey PRIMARY KEY (id);


--
-- Name: payment_transaction payment_transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_transaction
    ADD CONSTRAINT payment_transaction_pkey PRIMARY KEY (id);


--
-- Name: payment_transaction payment_transaction_trade_no_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_transaction
    ADD CONSTRAINT payment_transaction_trade_no_key UNIQUE (trade_no);


--
-- Name: agent_effect pk_agent_effect; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_effect
    ADD CONSTRAINT pk_agent_effect PRIMARY KEY (id);


--
-- Name: agent_grant pk_agent_grant; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT pk_agent_grant PRIMARY KEY (id);


--
-- Name: agent_grant_capability pk_agent_grant_capability; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_capability
    ADD CONSTRAINT pk_agent_grant_capability PRIMARY KEY (grant_id, capability, action, resource_type);


--
-- Name: agent_grant_event pk_agent_grant_event; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_event
    ADD CONSTRAINT pk_agent_grant_event PRIMARY KEY (id);


--
-- Name: agent_grant_workspace pk_agent_grant_workspace; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_workspace
    ADD CONSTRAINT pk_agent_grant_workspace PRIMARY KEY (grant_id, workspace_id);


--
-- Name: agent_run pk_agent_run; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT pk_agent_run PRIMARY KEY (id);


--
-- Name: agent_run_event pk_agent_run_event; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run_event
    ADD CONSTRAINT pk_agent_run_event PRIMARY KEY (id);


--
-- Name: calligraphy_review_draft pk_calligraphy_review_draft; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.calligraphy_review_draft
    ADD CONSTRAINT pk_calligraphy_review_draft PRIMARY KEY (id);


--
-- Name: class_enrollment pk_class_enrollment; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_enrollment
    ADD CONSTRAINT pk_class_enrollment PRIMARY KEY (group_id, learner_id);


--
-- Name: class_profile pk_class_profile; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_profile
    ADD CONSTRAINT pk_class_profile PRIMARY KEY (group_id);


--
-- Name: class_staff pk_class_staff; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_staff
    ADD CONSTRAINT pk_class_staff PRIMARY KEY (group_id, user_id);


--
-- Name: customer_service_event pk_customer_service_event; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT pk_customer_service_event PRIMARY KEY (id);


--
-- Name: customer_service_read_cursor pk_customer_service_read_cursor; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_read_cursor
    ADD CONSTRAINT pk_customer_service_read_cursor PRIMARY KEY (id);


--
-- Name: customer_service_seat pk_customer_service_seat; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat
    ADD CONSTRAINT pk_customer_service_seat PRIMARY KEY (business_identity_id);


--
-- Name: customer_service_seat_console pk_customer_service_seat_console; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_console
    ADD CONSTRAINT pk_customer_service_seat_console PRIMARY KEY (id);


--
-- Name: customer_service_seat_limit pk_customer_service_seat_limit; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_limit
    ADD CONSTRAINT pk_customer_service_seat_limit PRIMARY KEY (organization_id);


--
-- Name: customer_service_seat_presence pk_customer_service_seat_presence; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_presence
    ADD CONSTRAINT pk_customer_service_seat_presence PRIMARY KEY (organization_id, business_identity_id);


--
-- Name: customer_service_session pk_customer_service_session; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT pk_customer_service_session PRIMARY KEY (id);


--
-- Name: customer_service_shop_key pk_customer_service_shop_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_shop_key
    ADD CONSTRAINT pk_customer_service_shop_key PRIMARY KEY (id);


--
-- Name: customer_service_visit_token pk_customer_service_visit_token; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT pk_customer_service_visit_token PRIMARY KEY (id);


--
-- Name: customer_service_widget_identity_key pk_customer_service_widget_identity_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_identity_key
    ADD CONSTRAINT pk_customer_service_widget_identity_key PRIMARY KEY (id);


--
-- Name: customer_service_widget_installation pk_customer_service_widget_installation; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_installation
    ADD CONSTRAINT pk_customer_service_widget_installation PRIMARY KEY (id);


--
-- Name: customer_service_widget_nonce pk_customer_service_widget_nonce; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_nonce
    ADD CONSTRAINT pk_customer_service_widget_nonce PRIMARY KEY (id);


--
-- Name: enterprise_attachment_retention pk_ear; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_attachment_retention
    ADD CONSTRAINT pk_ear PRIMARY KEY (attachment_id);


--
-- Name: enterprise_application_usage pk_eau; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_usage
    ADD CONSTRAINT pk_eau PRIMARY KEY (organization_id, application_id, metric, period_start);


--
-- Name: enterprise_group_origin pk_ego; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_group_origin
    ADD CONSTRAINT pk_ego PRIMARY KEY (group_id);


--
-- Name: enterprise_message_origin pk_emo; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message_origin
    ADD CONSTRAINT pk_emo PRIMARY KEY (id);


--
-- Name: enterprise_application pk_enterprise_application; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application
    ADD CONSTRAINT pk_enterprise_application PRIMARY KEY (id);


--
-- Name: enterprise_application_credential pk_enterprise_application_credential; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_credential
    ADD CONSTRAINT pk_enterprise_application_credential PRIMARY KEY (id);


--
-- Name: enterprise_application_grant pk_enterprise_application_grant; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant
    ADD CONSTRAINT pk_enterprise_application_grant PRIMARY KEY (id);


--
-- Name: enterprise_application_grant_scope pk_enterprise_application_grant_scope; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant_scope
    ADD CONSTRAINT pk_enterprise_application_grant_scope PRIMARY KEY (grant_id, scope);


--
-- Name: enterprise_application_grant_workspace pk_enterprise_application_grant_workspace; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant_workspace
    ADD CONSTRAINT pk_enterprise_application_grant_workspace PRIMARY KEY (grant_id, workspace_id);


--
-- Name: enterprise_asset pk_enterprise_asset; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT pk_enterprise_asset PRIMARY KEY (id);


--
-- Name: enterprise_audit_event pk_enterprise_audit_event; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_audit_event
    ADD CONSTRAINT pk_enterprise_audit_event PRIMARY KEY (id);


--
-- Name: enterprise_contact pk_enterprise_contact; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact
    ADD CONSTRAINT pk_enterprise_contact PRIMARY KEY (id);


--
-- Name: enterprise_contact_assignment pk_enterprise_contact_assignment; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_assignment
    ADD CONSTRAINT pk_enterprise_contact_assignment PRIMARY KEY (id);


--
-- Name: enterprise_contact_identity pk_enterprise_contact_identity; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_identity
    ADD CONSTRAINT pk_enterprise_contact_identity PRIMARY KEY (id);


--
-- Name: enterprise_conversation pk_enterprise_conversation; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_conversation
    ADD CONSTRAINT pk_enterprise_conversation PRIMARY KEY (id);


--
-- Name: enterprise_external_identity pk_enterprise_external_identity; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT pk_enterprise_external_identity PRIMARY KEY (id);


--
-- Name: enterprise_internal_idempotency pk_enterprise_internal_idempotency; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_internal_idempotency
    ADD CONSTRAINT pk_enterprise_internal_idempotency PRIMARY KEY (organization_id, application_id, idempotency_key);


--
-- Name: enterprise_message pk_enterprise_message; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT pk_enterprise_message PRIMARY KEY (id);


--
-- Name: enterprise_message_delivery pk_enterprise_message_delivery; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message_delivery
    ADD CONSTRAINT pk_enterprise_message_delivery PRIMARY KEY (id);


--
-- Name: enterprise_note pk_enterprise_note; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_note
    ADD CONSTRAINT pk_enterprise_note PRIMARY KEY (id);


--
-- Name: enterprise_oa_sso_code pk_enterprise_oa_sso_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_oa_sso_code
    ADD CONSTRAINT pk_enterprise_oa_sso_code PRIMARY KEY (id);


--
-- Name: enterprise_offboarding_case pk_enterprise_offboarding_case; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_case
    ADD CONSTRAINT pk_enterprise_offboarding_case PRIMARY KEY (id);


--
-- Name: enterprise_offboarding_item pk_enterprise_offboarding_item; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_item
    ADD CONSTRAINT pk_enterprise_offboarding_item PRIMARY KEY (id);


--
-- Name: enterprise_retention_hold pk_enterprise_retention_hold; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_hold
    ADD CONSTRAINT pk_enterprise_retention_hold PRIMARY KEY (id);


--
-- Name: enterprise_retention_policy pk_enterprise_retention_policy; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_policy
    ADD CONSTRAINT pk_enterprise_retention_policy PRIMARY KEY (id);


--
-- Name: fts_user pk_fts_user_uid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fts_user
    ADD CONSTRAINT pk_fts_user_uid PRIMARY KEY (user_id);


--
-- Name: guardian_learner pk_guardian_learner; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.guardian_learner
    ADD CONSTRAINT pk_guardian_learner PRIMARY KEY (guardian_uid, learner_id);


--
-- Name: homework_submission pk_homework_submission; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.homework_submission
    ADD CONSTRAINT pk_homework_submission PRIMARY KEY (id);


--
-- Name: learner pk_learner; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.learner
    ADD CONSTRAINT pk_learner PRIMARY KEY (id);


--
-- Name: moderation_action pk_moderation_action; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moderation_action
    ADD CONSTRAINT pk_moderation_action PRIMARY KEY (id);


--
-- Name: moderation_appeal pk_moderation_appeal; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moderation_appeal
    ADD CONSTRAINT pk_moderation_appeal PRIMARY KEY (id);


--
-- Name: moya_invite_code pk_moya_invite_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moya_invite_code
    ADD CONSTRAINT pk_moya_invite_code PRIMARY KEY (code);


--
-- Name: organization_business_identity_assignment pk_obia; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity_assignment
    ADD CONSTRAINT pk_obia PRIMARY KEY (id);


--
-- Name: organization pk_organization; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization
    ADD CONSTRAINT pk_organization PRIMARY KEY (id);


--
-- Name: organization_business_identity pk_organization_business_identity; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity
    ADD CONSTRAINT pk_organization_business_identity PRIMARY KEY (id);


--
-- Name: organization_default_workspace pk_organization_default_workspace; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_default_workspace
    ADD CONSTRAINT pk_organization_default_workspace PRIMARY KEY (organization_id);


--
-- Name: organization_department pk_organization_department; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department
    ADD CONSTRAINT pk_organization_department PRIMARY KEY (id);


--
-- Name: organization_department_member pk_organization_department_member; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department_member
    ADD CONSTRAINT pk_organization_department_member PRIMARY KEY (department_id, user_id);


--
-- Name: organization_invitation pk_organization_invitation; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invitation
    ADD CONSTRAINT pk_organization_invitation PRIMARY KEY (id);


--
-- Name: organization_member pk_organization_member; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_member
    ADD CONSTRAINT pk_organization_member PRIMARY KEY (organization_id, user_id);


--
-- Name: owner_activation_invite pk_owner_activation_invite; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.owner_activation_invite
    ADD CONSTRAINT pk_owner_activation_invite PRIMARY KEY (id);


--
-- Name: geo_people_nearby pk_people_nearby_uid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.geo_people_nearby
    ADD CONSTRAINT pk_people_nearby_uid PRIMARY KEY (user_id);


--
-- Name: review_asset pk_review_asset; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.review_asset
    ADD CONSTRAINT pk_review_asset PRIMARY KEY (id);


--
-- Name: submission_asset pk_submission_asset; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.submission_asset
    ADD CONSTRAINT pk_submission_asset PRIMARY KEY (id);


--
-- Name: teacher_review pk_teacher_review; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_review
    ADD CONSTRAINT pk_teacher_review PRIMARY KEY (id);


--
-- Name: teaching_admin_audit pk_teaching_admin_audit; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teaching_admin_audit
    ADD CONSTRAINT pk_teaching_admin_audit PRIMARY KEY (id);


--
-- Name: user_deletion_job pk_user_deletion_job; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_deletion_job
    ADD CONSTRAINT pk_user_deletion_job PRIMARY KEY (id);


--
-- Name: user_deletion_request pk_user_deletion_request; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_deletion_request
    ADD CONSTRAINT pk_user_deletion_request PRIMARY KEY (id);


--
-- Name: user_setting pk_user_setting_uid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_setting
    ADD CONSTRAINT pk_user_setting_uid PRIMARY KEY (user_id);


--
-- Name: plugin_audit_log plugin_audit_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.plugin_audit_log
    ADD CONSTRAINT plugin_audit_log_pkey PRIMARY KEY (id);


--
-- Name: project_channel_rel project_channel_rel_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_channel_rel
    ADD CONSTRAINT project_channel_rel_pkey PRIMARY KEY (project_id, channel_id);


--
-- Name: project_event project_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_event
    ADD CONSTRAINT project_event_pkey PRIMARY KEY (id);


--
-- Name: project_member project_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_member
    ADD CONSTRAINT project_member_pkey PRIMARY KEY (project_id, user_id);


--
-- Name: project_milestone project_milestone_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_milestone
    ADD CONSTRAINT project_milestone_pkey PRIMARY KEY (id);


--
-- Name: project project_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project
    ADD CONSTRAINT project_pkey PRIMARY KEY (id);


--
-- Name: project_task project_task_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_task
    ADD CONSTRAINT project_task_pkey PRIMARY KEY (id);


--
-- Name: push_token push_token_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.push_token
    ADD CONSTRAINT push_token_pkey PRIMARY KEY (id);


--
-- Name: recharge_order recharge_order_order_no_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recharge_order
    ADD CONSTRAINT recharge_order_order_no_key UNIQUE (order_no);


--
-- Name: recharge_order recharge_order_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recharge_order
    ADD CONSTRAINT recharge_order_pkey PRIMARY KEY (id);


--
-- Name: red_packet red_packet_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.red_packet
    ADD CONSTRAINT red_packet_pkey PRIMARY KEY (id);


--
-- Name: red_packet_receive red_packet_receive_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.red_packet_receive
    ADD CONSTRAINT red_packet_receive_pkey PRIMARY KEY (id);


--
-- Name: report_action_log report_action_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.report_action_log
    ADD CONSTRAINT report_action_log_pkey PRIMARY KEY (id);


--
-- Name: report_ticket report_ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.report_ticket
    ADD CONSTRAINT report_ticket_pkey PRIMARY KEY (id);


--
-- Name: review_queue review_queue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.review_queue
    ADD CONSTRAINT review_queue_pkey PRIMARY KEY (id);


--
-- Name: sensitive_word sensitive_word_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sensitive_word
    ADD CONSTRAINT sensitive_word_pkey PRIMARY KEY (id);


--
-- Name: sso_config sso_config_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sso_config
    ADD CONSTRAINT sso_config_pkey PRIMARY KEY (id);


--
-- Name: sso_identity sso_identity_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sso_identity
    ADD CONSTRAINT sso_identity_pkey PRIMARY KEY (id);


--
-- Name: system_datacenter_log system_datacenter_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_datacenter_log
    ADD CONSTRAINT system_datacenter_log_pkey PRIMARY KEY (id);


--
-- Name: system_datacenter system_datacenter_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_datacenter
    ADD CONSTRAINT system_datacenter_name_key UNIQUE (name);


--
-- Name: system_datacenter system_datacenter_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_datacenter
    ADD CONSTRAINT system_datacenter_pkey PRIMARY KEY (id);


--
-- Name: system_id_segment system_id_segment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_id_segment
    ADD CONSTRAINT system_id_segment_pkey PRIMARY KEY (id);


--
-- Name: system_id_segment_stats system_id_segment_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_id_segment_stats
    ADD CONSTRAINT system_id_segment_stats_pkey PRIMARY KEY (id);


--
-- Name: transfer_order transfer_order_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.transfer_order
    ADD CONSTRAINT transfer_order_pkey PRIMARY KEY (id);


--
-- Name: trust_audit trust_audit_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trust_audit
    ADD CONSTRAINT trust_audit_pkey PRIMARY KEY (id);


--
-- Name: app_version_policy uk_app_version_policy_type; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_version_policy
    ADD CONSTRAINT uk_app_version_policy_type UNIQUE (type);


--
-- Name: attachment uk_attachment_path; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT uk_attachment_path UNIQUE (path);


--
-- Name: channel_admin uk_channel_admin; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT uk_channel_admin UNIQUE (channel_id, user_id);


--
-- Name: channel uk_channel_custom_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel
    ADD CONSTRAINT uk_channel_custom_id UNIQUE (custom_id);


--
-- Name: channel uk_channel_id_workspace; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel
    ADD CONSTRAINT uk_channel_id_workspace UNIQUE (workspace_id, id);


--
-- Name: channel_message_view uk_channel_message_view; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT uk_channel_message_view UNIQUE (message_id, user_id);


--
-- Name: channel_reaction uk_channel_reaction; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT uk_channel_reaction UNIQUE (message_id, user_id, reaction_type);


--
-- Name: channel_stats_daily uk_channel_stats_daily; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_stats_daily
    ADD CONSTRAINT uk_channel_stats_daily UNIQUE (channel_id, stats_date);


--
-- Name: channel_subscription uk_channel_subscription; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT uk_channel_subscription UNIQUE (channel_id, user_id);


--
-- Name: system_id_segment uk_datacenter_table_active; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_id_segment
    ADD CONSTRAINT uk_datacenter_table_active UNIQUE (datacenter_id, table_name);


--
-- Name: e2ee_key_backups uk_e2ee_key_backups_uid_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_key_backups
    ADD CONSTRAINT uk_e2ee_key_backups_uid_version UNIQUE (uid, backup_version);


--
-- Name: feedback uk_feedbackmd5; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feedback
    ADD CONSTRAINT uk_feedbackmd5 UNIQUE (feedback_md5);


--
-- Name: homework_submission uk_homework_submission_attempt; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.homework_submission
    ADD CONSTRAINT uk_homework_submission_attempt UNIQUE (assignment_id, attempt_no);


--
-- Name: moderation_appeal uk_moderation_appeal_action_appellant; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moderation_appeal
    ADD CONSTRAINT uk_moderation_appeal_action_appellant UNIQUE (action_id, appellant_uid);


--
-- Name: moment_like uk_moment_like_post_user; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT uk_moment_like_post_user UNIQUE (post_id, user_id);


--
-- Name: moment_post_acl uk_moment_post_acl; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT uk_moment_post_acl UNIQUE (post_id, uid, acl_type);


--
-- Name: moment_report uk_moment_report_post_reporter; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT uk_moment_report_post_reporter UNIQUE (post_id, reporter_uid);


--
-- Name: moment_timeline uk_moment_timeline_uid_post; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT uk_moment_timeline_uid_post UNIQUE (recipient_uid, post_id);


--
-- Name: msg_mention uk_msg_mention; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.msg_mention
    ADD CONSTRAINT uk_msg_mention UNIQUE (msg_id, mentioned_uid);


--
-- Name: app_ddl uk_oldvsn_newvsn; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_ddl
    ADD CONSTRAINT uk_oldvsn_newvsn UNIQUE (old_vsn, new_vsn);


--
-- Name: project uk_project_id_workspace; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project
    ADD CONSTRAINT uk_project_id_workspace UNIQUE (id, workspace_id);


--
-- Name: report_ticket uk_report_ticket_target_reporter; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.report_ticket
    ADD CONSTRAINT uk_report_ticket_target_reporter UNIQUE (target_type, target_id, reporter_uid);


--
-- Name: review_asset uk_review_asset_attachment; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.review_asset
    ADD CONSTRAINT uk_review_asset_attachment UNIQUE (attachment_id);


--
-- Name: user_tag uk_scene_creatorid_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_tag
    ADD CONSTRAINT uk_scene_creatorid_name UNIQUE (scene, creator_user_id, name);


--
-- Name: submission_asset uk_submission_asset_attachment; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.submission_asset
    ADD CONSTRAINT uk_submission_asset_attachment UNIQUE (submission_id, attachment_id);


--
-- Name: user_collect uk_user_collect_uid_kind; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_collect
    ADD CONSTRAINT uk_user_collect_uid_kind UNIQUE (user_id, kind_id);


--
-- Name: user_collect uk_user_collect_userid_status_kindid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_collect
    ADD CONSTRAINT uk_user_collect_userid_status_kindid UNIQUE (user_id, status, kind_id);


--
-- Name: user_denylist uk_userid_denieduserid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_denylist
    ADD CONSTRAINT uk_userid_denieduserid UNIQUE (user_id, denied_user_id);


--
-- Name: user_device uk_userid_deviceid; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_device
    ADD CONSTRAINT uk_userid_deviceid UNIQUE (user_id, device_id);


--
-- Name: app_version uk_vsn_pkgname_type; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.app_version
    ADD CONSTRAINT uk_vsn_pkgname_type UNIQUE (vsn, package_name, type);


--
-- Name: mcp_client_grant uniq_mcp_grant_client_tool; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mcp_client_grant
    ADD CONSTRAINT uniq_mcp_grant_client_tool UNIQUE (client_id, tool_name);


--
-- Name: agent_effect uq_ae_run_sequence; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_effect
    ADD CONSTRAINT uq_ae_run_sequence UNIQUE (run_id, sequence);


--
-- Name: agent_effect uq_ae_tool_external_idem; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_effect
    ADD CONSTRAINT uq_ae_tool_external_idem UNIQUE (tool_id, external_idempotency_key);


--
-- Name: agent_grant uq_ag_org_delegator_idempotency; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT uq_ag_org_delegator_idempotency UNIQUE (organization_id, delegator_user_id, idempotency_key);


--
-- Name: agent_grant uq_ag_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT uq_ag_org_id UNIQUE (organization_id, id);


--
-- Name: agent_grant_event uq_age_idempotency_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_event
    ADD CONSTRAINT uq_age_idempotency_key UNIQUE (idempotency_key);


--
-- Name: agent_run uq_ar_agent_org_trigger_idem; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT uq_ar_agent_org_trigger_idem UNIQUE (agent_id, organization_id, trigger_type, trigger_id, idempotency_key);


--
-- Name: agent_run uq_ar_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT uq_ar_org_id UNIQUE (organization_id, id);


--
-- Name: agent_run_event uq_are_run_idem; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run_event
    ADD CONSTRAINT uq_are_run_idem UNIQUE (run_id, idempotency_key);


--
-- Name: customer_service_event uq_cse_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT uq_cse_org_id UNIQUE (organization_id, id);


--
-- Name: customer_service_read_cursor uq_csrc_org_session_identity; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_read_cursor
    ADD CONSTRAINT uq_csrc_org_session_identity UNIQUE (organization_id, session_id, business_identity_id);


--
-- Name: customer_service_seat uq_css_org_identity; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat
    ADD CONSTRAINT uq_css_org_identity UNIQUE (organization_id, business_identity_id);


--
-- Name: customer_service_seat_console uq_cssc_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_console
    ADD CONSTRAINT uq_cssc_org_id UNIQUE (organization_id, id);


--
-- Name: customer_service_seat_console uq_cssc_public_seat_console_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_console
    ADD CONSTRAINT uq_cssc_public_seat_console_id UNIQUE (public_seat_console_id);


--
-- Name: customer_service_shop_key uq_cssk_org_digest; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_shop_key
    ADD CONSTRAINT uq_cssk_org_digest UNIQUE (organization_id, key_digest);


--
-- Name: customer_service_session uq_csss_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT uq_csss_org_id UNIQUE (organization_id, id);


--
-- Name: customer_service_visit_token uq_csvt_org_digest; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT uq_csvt_org_digest UNIQUE (organization_id, token_digest);


--
-- Name: customer_service_visit_token uq_csvt_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT uq_csvt_org_id UNIQUE (organization_id, id);


--
-- Name: customer_service_widget_installation uq_cswi_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_installation
    ADD CONSTRAINT uq_cswi_org_id UNIQUE (organization_id, id);


--
-- Name: customer_service_widget_installation uq_cswi_public_widget_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_installation
    ADD CONSTRAINT uq_cswi_public_widget_id UNIQUE (public_widget_id);


--
-- Name: customer_service_widget_identity_key uq_cswk_install_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_identity_key
    ADD CONSTRAINT uq_cswk_install_version UNIQUE (organization_id, installation_id, key_version);


--
-- Name: customer_service_widget_identity_key uq_cswk_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_identity_key
    ADD CONSTRAINT uq_cswk_org_id UNIQUE (organization_id, id);


--
-- Name: customer_service_widget_nonce uq_cswn_install_jti; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_nonce
    ADD CONSTRAINT uq_cswn_install_jti UNIQUE (organization_id, installation_id, jti_digest);


--
-- Name: customer_service_widget_nonce uq_cswn_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_nonce
    ADD CONSTRAINT uq_cswn_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_application uq_ea_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application
    ADD CONSTRAINT uq_ea_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_application uq_ea_org_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application
    ADD CONSTRAINT uq_ea_org_key UNIQUE (organization_id, application_key);


--
-- Name: enterprise_application_credential uq_eac_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_credential
    ADD CONSTRAINT uq_eac_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_application_credential uq_eac_prefix; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_credential
    ADD CONSTRAINT uq_eac_prefix UNIQUE (credential_prefix);


--
-- Name: enterprise_application_grant uq_eag_org_app_idempotency; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant
    ADD CONSTRAINT uq_eag_org_app_idempotency UNIQUE (organization_id, application_id, idempotency_key);


--
-- Name: enterprise_application_grant uq_eag_org_id_kind; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant
    ADD CONSTRAINT uq_eag_org_id_kind UNIQUE (organization_id, id, workspace_scope_kind);


--
-- Name: enterprise_conversation uq_ec_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_conversation
    ADD CONSTRAINT uq_ec_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_conversation uq_ec_org_ws_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_conversation
    ADD CONSTRAINT uq_ec_org_ws_id UNIQUE (organization_id, workspace_id, id);


--
-- Name: enterprise_contact_identity uq_eci_org_channel_hmac; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_identity
    ADD CONSTRAINT uq_eci_org_channel_hmac UNIQUE (organization_id, channel, subject_hmac);


--
-- Name: enterprise_external_identity uq_eei_org_app_external; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT uq_eei_org_app_external UNIQUE (organization_id, application_id, external_user_id);


--
-- Name: enterprise_external_identity uq_eei_org_app_user; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT uq_eei_org_app_user UNIQUE (organization_id, application_id, user_id);


--
-- Name: enterprise_external_identity uq_eei_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT uq_eei_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_message uq_em_org_conversation_client_msg; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT uq_em_org_conversation_client_msg UNIQUE (organization_id, conversation_id, client_msg_id);


--
-- Name: enterprise_message uq_em_org_ws_conversation_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT uq_em_org_ws_conversation_id UNIQUE (organization_id, workspace_id, conversation_id, id);


--
-- Name: enterprise_message uq_em_org_ws_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT uq_em_org_ws_id UNIQUE (organization_id, workspace_id, id);


--
-- Name: enterprise_message_delivery uq_emd_org_message_recipient_device; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message_delivery
    ADD CONSTRAINT uq_emd_org_message_recipient_device UNIQUE NULLS NOT DISTINCT (organization_id, message_id, recipient_ref, device_id);


--
-- Name: enterprise_asset uq_enterprise_asset_object_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT uq_enterprise_asset_object_key UNIQUE (object_key);


--
-- Name: enterprise_asset uq_enterprise_asset_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT uq_enterprise_asset_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_contact uq_enterprise_contact_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact
    ADD CONSTRAINT uq_enterprise_contact_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_offboarding_case uq_eoc_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_case
    ADD CONSTRAINT uq_eoc_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_offboarding_item uq_eoi_org_idempotency_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_item
    ADD CONSTRAINT uq_eoi_org_idempotency_key UNIQUE (organization_id, idempotency_key);


--
-- Name: enterprise_oa_sso_code uq_eosc_code_digest; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_oa_sso_code
    ADD CONSTRAINT uq_eosc_code_digest UNIQUE (code_digest);


--
-- Name: enterprise_oa_sso_code uq_eosc_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_oa_sso_code
    ADD CONSTRAINT uq_eosc_org_id UNIQUE (organization_id, id);


--
-- Name: enterprise_retention_policy uq_erp_org_ws_class_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_policy
    ADD CONSTRAINT uq_erp_org_ws_class_version UNIQUE (organization_id, workspace_id, data_class, version);


--
-- Name: group_member_generation uq_gmg_generation; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_member_generation
    ADD CONSTRAINT uq_gmg_generation UNIQUE (group_id, user_id, generation_no);


--
-- Name: organization_business_identity uq_obi_org_function_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity
    ADD CONSTRAINT uq_obi_org_function_name UNIQUE (organization_id, function_key, display_name);


--
-- Name: organization_business_identity uq_obi_org_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity
    ADD CONSTRAINT uq_obi_org_id UNIQUE (organization_id, id);


--
-- Name: organization_business_identity uq_obi_org_id_function; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity
    ADD CONSTRAINT uq_obi_org_id_function UNIQUE (organization_id, id, function_key);


--
-- Name: user_deletion_job uq_user_deletion_job_user; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_deletion_job
    ADD CONSTRAINT uq_user_deletion_job_user UNIQUE (user_id);


--
-- Name: user_deletion_request uq_user_deletion_request_user; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_deletion_request
    ADD CONSTRAINT uq_user_deletion_request_user UNIQUE (user_id);


--
-- Name: workspace uq_workspace_organization_id_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace
    ADD CONSTRAINT uq_workspace_organization_id_id UNIQUE (organization_id, id);


--
-- Name: CONSTRAINT uq_workspace_organization_id_id ON workspace; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT uq_workspace_organization_id_id ON public.workspace IS '企业复合 FK 引用目标：使 enterprise_conversation/message 能以 (organization_id, workspace_id) 强制「Workspace 属于同一 Org」；organization_id IS NULL 的历史 Workspace 因匹配不到而被拒绝';


--
-- Name: user_auth_epoch user_auth_epoch_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_auth_epoch
    ADD CONSTRAINT user_auth_epoch_pkey PRIMARY KEY (user_id);


--
-- Name: user_collect user_collect_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_collect
    ADD CONSTRAINT user_collect_pkey PRIMARY KEY (id);


--
-- Name: user_denylist user_denylist_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_denylist
    ADD CONSTRAINT user_denylist_pkey PRIMARY KEY (id);


--
-- Name: user_device user_device_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_device
    ADD CONSTRAINT user_device_pkey PRIMARY KEY (id);


--
-- Name: user_dnd_rule user_dnd_rule_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_dnd_rule
    ADD CONSTRAINT user_dnd_rule_pkey PRIMARY KEY (id);


--
-- Name: user_friend_category user_friend_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_friend_category
    ADD CONSTRAINT user_friend_category_pkey PRIMARY KEY (id);


--
-- Name: user_friend user_friend_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_friend
    ADD CONSTRAINT user_friend_pkey PRIMARY KEY (id);


--
-- Name: user_group_category user_group_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_group_category
    ADD CONSTRAINT user_group_category_pkey PRIMARY KEY (id);


--
-- Name: user_group user_group_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_group
    ADD CONSTRAINT user_group_pkey PRIMARY KEY (id);


--
-- Name: user user_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: user_tag user_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_tag
    ADD CONSTRAINT user_tag_pkey PRIMARY KEY (id);


--
-- Name: user_tag_relation user_tag_relation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_tag_relation
    ADD CONSTRAINT user_tag_relation_pkey PRIMARY KEY (id);


--
-- Name: verification_code verification_code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.verification_code
    ADD CONSTRAINT verification_code_pkey PRIMARY KEY (id);


--
-- Name: wallet wallet_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wallet
    ADD CONSTRAINT wallet_pkey PRIMARY KEY (id);


--
-- Name: wallet_transaction wallet_transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wallet_transaction
    ADD CONSTRAINT wallet_transaction_pkey PRIMARY KEY (id);


--
-- Name: wallet wallet_user_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wallet
    ADD CONSTRAINT wallet_user_id_key UNIQUE (user_id);


--
-- Name: workspace_group_read_cursor workspace_group_read_cursor_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_group_read_cursor
    ADD CONSTRAINT workspace_group_read_cursor_pkey PRIMARY KEY (generation_id);


--
-- Name: workspace_invite workspace_invite_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_invite
    ADD CONSTRAINT workspace_invite_pkey PRIMARY KEY (id);


--
-- Name: workspace_member workspace_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_member
    ADD CONSTRAINT workspace_member_pkey PRIMARY KEY (workspace_id, user_id);


--
-- Name: workspace workspace_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace
    ADD CONSTRAINT workspace_pkey PRIMARY KEY (id);


--
-- Name: agent_hub_audit_corr_time_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX agent_hub_audit_corr_time_idx ON public.agent_hub_audit USING btree (correlation_id, occurred_at, entity_id);


--
-- Name: agent_hub_audit_one_outcome_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX agent_hub_audit_one_outcome_uq ON public.agent_hub_audit USING btree (correlation_id) WHERE (entity_type = 'outcome'::text);


--
-- Name: agent_hub_audit_one_request_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX agent_hub_audit_one_request_uq ON public.agent_hub_audit USING btree (correlation_id) WHERE (entity_type = 'request'::text);


--
-- Name: agent_task_corr_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX agent_task_corr_idx ON public.agent_task USING btree (correlation_id);


--
-- Name: agent_task_decision_corr_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX agent_task_decision_corr_idx ON public.agent_task_decision USING btree (correlation_id);


--
-- Name: agent_task_decision_task_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX agent_task_decision_task_uq ON public.agent_task_decision USING btree (task_id);


--
-- Name: agent_task_event_idem_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX agent_task_event_idem_uq ON public.agent_task_event USING btree (idempotency_key);


--
-- Name: agent_task_event_task_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX agent_task_event_task_idx ON public.agent_task_event USING btree (task_id, seq);


--
-- Name: agent_task_group_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX agent_task_group_idx ON public.agent_task USING btree (group_id);


--
-- Name: agent_task_idem_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX agent_task_idem_uq ON public.agent_task USING btree (idempotency_key);


--
-- Name: agent_task_status_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX agent_task_status_idx ON public.agent_task USING btree (status);


--
-- Name: bot_api_token_digest_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX bot_api_token_digest_idx ON public.bot USING btree (api_token_digest);


--
-- Name: bot_delivery_attempt_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX bot_delivery_attempt_idx ON public.bot_delivery_attempt USING btree (delivery_id, attempt_no);


--
-- Name: bot_delivery_bot_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX bot_delivery_bot_idx ON public.bot_delivery USING btree (bot_id, created_at DESC);


--
-- Name: bot_delivery_due_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX bot_delivery_due_idx ON public.bot_delivery USING btree (status, next_retry_at);


--
-- Name: bot_delivery_ewh_keyset_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX bot_delivery_ewh_keyset_idx ON public.bot_delivery USING btree (ewh_owner_organization_id, ewh_owner_application_id, created_at DESC, delivery_id DESC) WHERE ((ewh_owner_organization_id IS NOT NULL) AND (ewh_owner_application_id IS NOT NULL));


--
-- Name: INDEX bot_delivery_ewh_keyset_idx; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.bot_delivery_ewh_keyset_idx IS 'INT-23 投递列表 keyset 分页：org+app 前缀等值 + (created_at DESC, delivery_id DESC) 排序，企业行限定（bot 域行成对 NULL 不入索引）';


--
-- Name: bot_delivery_ewh_owner_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX bot_delivery_ewh_owner_idx ON public.bot_delivery USING btree (ewh_owner_application_id, status, created_at DESC) WHERE (ewh_owner_application_id IS NOT NULL);


--
-- Name: bot_delivery_idem_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX bot_delivery_idem_uq ON public.bot_delivery USING btree (idempotency_key);


--
-- Name: group_task_assignment_task_id_user_id_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX group_task_assignment_task_id_user_id_key ON public.group_task_assignment USING btree (task_id, user_id) WHERE (learner_id IS NULL);


--
-- Name: i_ag_agent_org_status_expires; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ag_agent_org_status_expires ON public.agent_grant USING btree (agent_id, organization_id, status, expires_at);


--
-- Name: i_ag_delegator_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ag_delegator_org_status ON public.agent_grant USING btree (delegator_user_id, organization_id, status);


--
-- Name: i_age_grant_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_age_grant_created ON public.agent_grant_event USING btree (grant_id, created_at, id);


--
-- Name: i_agw_workspace_grant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_agw_workspace_grant ON public.agent_grant_workspace USING btree (workspace_id, grant_id);


--
-- Name: i_ar_agent_org_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ar_agent_org_created ON public.agent_run USING btree (agent_id, organization_id, created_at DESC);


--
-- Name: i_ar_grant_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ar_grant_status ON public.agent_run USING btree (grant_id, status);


--
-- Name: i_ar_status_lease; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ar_status_lease ON public.agent_run USING btree (status, lease_expires_at) WHERE (status = ANY (ARRAY['queued'::text, 'running'::text]));


--
-- Name: i_attach_pending_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_attach_pending_created_at ON public.attach_pending USING btree (created_at);


--
-- Name: i_attachment_creatoruserid_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_attachment_creatoruserid_status ON public.attachment USING btree (creator_user_id, status);


--
-- Name: i_attachment_referer_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_attachment_referer_created ON public.attachment USING btree (referer_time, created_at) WHERE (status = 1);


--
-- Name: i_c2c_e2ee; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2c_e2ee ON public.msg_c2c USING btree (((e2ee IS NOT NULL))) WHERE (e2ee IS NOT NULL);


--
-- Name: i_c2c_expire_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2c_expire_at ON public.msg_c2c USING btree (expire_at) WHERE (expire_at IS NOT NULL);


--
-- Name: i_c2c_fromid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2c_fromid ON public.msg_c2c USING btree (from_id);


--
-- Name: i_c2c_msgtype; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2c_msgtype ON public.msg_c2c USING btree (msg_type);


--
-- Name: i_c2c_toid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2c_toid ON public.msg_c2c USING btree (to_id);


--
-- Name: i_c2g_expire_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2g_expire_at ON public.msg_c2g USING btree (expire_at) WHERE (expire_at IS NOT NULL);


--
-- Name: i_c2g_fromid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2g_fromid ON public.msg_c2g USING btree (from_id);


--
-- Name: i_c2g_msgtype; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2g_msgtype ON public.msg_c2g USING btree (msg_type);


--
-- Name: i_c2g_toid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2g_toid ON public.msg_c2g USING btree (to_id);


--
-- Name: i_c2s_e2ee; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2s_e2ee ON public.msg_c2s USING btree (((e2ee IS NOT NULL))) WHERE (e2ee IS NOT NULL);


--
-- Name: i_c2s_fromid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2s_fromid ON public.msg_c2s USING btree (from_id);


--
-- Name: i_c2s_msgtype; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2s_msgtype ON public.msg_c2s USING btree (msg_type);


--
-- Name: i_c2s_toid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_c2s_toid ON public.msg_c2s USING btree (to_id);


--
-- Name: i_channel_admin_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_admin_user ON public.channel_admin USING btree (user_id);


--
-- Name: i_channel_creator_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_creator_uid ON public.channel USING btree (creator_uid);


--
-- Name: i_channel_custom_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_custom_id ON public.channel USING btree (custom_id) WHERE (custom_id IS NOT NULL);


--
-- Name: i_channel_message_author; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_author ON public.channel_message USING btree (author_id);


--
-- Name: i_channel_message_channel_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_channel_created ON public.channel_message USING btree (channel_id, created_at DESC);


--
-- Name: i_channel_message_pinned; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_pinned ON public.channel_message USING btree (channel_id, is_pinned) WHERE (is_pinned = true);


--
-- Name: i_channel_message_revoked; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_revoked ON public.channel_message USING btree (channel_id, revoked, created_at DESC);


--
-- Name: i_channel_message_view_channel_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_view_channel_user ON public.channel_message_view USING btree (channel_id, user_id);


--
-- Name: i_channel_message_view_message; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_view_message ON public.channel_message_view USING btree (message_id);


--
-- Name: i_channel_message_view_viewed_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_message_view_viewed_at ON public.channel_message_view USING btree (viewed_at DESC);


--
-- Name: i_channel_reaction_channel; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_reaction_channel ON public.channel_reaction USING btree (channel_id);


--
-- Name: i_channel_reaction_message; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_reaction_message ON public.channel_reaction USING btree (message_id);


--
-- Name: i_channel_reaction_type; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_reaction_type ON public.channel_reaction USING btree (reaction_type);


--
-- Name: i_channel_reaction_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_reaction_user ON public.channel_reaction USING btree (user_id);


--
-- Name: i_channel_scope_personal; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_scope_personal ON public.channel USING btree (creator_uid, created_at DESC) WHERE ((scope = 'personal'::text) AND (status = 1));


--
-- Name: i_channel_scope_ws; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_scope_ws ON public.channel USING btree (workspace_id, created_at DESC) WHERE ((scope = 'workspace'::text) AND (status = 1));


--
-- Name: i_channel_stats_daily_channel; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_stats_daily_channel ON public.channel_stats_daily USING btree (channel_id, stats_date DESC);


--
-- Name: i_channel_stats_daily_date; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_stats_daily_date ON public.channel_stats_daily USING btree (stats_date DESC);


--
-- Name: i_channel_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_status_created ON public.channel USING btree (status, created_at DESC);


--
-- Name: i_channel_subscriber_count; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_subscriber_count ON public.channel USING btree (subscriber_count DESC);


--
-- Name: i_channel_subscription_channel; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_subscription_channel ON public.channel_subscription USING btree (channel_id, status);


--
-- Name: i_channel_subscription_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_subscription_user ON public.channel_subscription USING btree (user_id, status);


--
-- Name: i_channel_tags; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_tags ON public.channel USING gin (tags);


--
-- Name: i_channel_visibility_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_channel_visibility_active ON public.channel USING btree (visibility) WHERE (status = 1);


--
-- Name: i_class_enrollment_learner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_class_enrollment_learner ON public.class_enrollment USING btree (learner_id);


--
-- Name: i_class_staff_user_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_class_staff_user_status ON public.class_staff USING btree (user_id, status);


--
-- Name: i_crd_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_crd_status_created ON public.calligraphy_review_draft USING btree (status, created_at);


--
-- Name: i_cse_org_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_cse_org_created ON public.customer_service_event USING btree (organization_id, created_at DESC);


--
-- Name: i_cse_org_session; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_cse_org_session ON public.customer_service_event USING btree (organization_id, session_id);


--
-- Name: i_cse_org_ws_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_cse_org_ws_id ON public.customer_service_event USING btree (organization_id, workspace_id, id);


--
-- Name: i_csrc_org_identity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_csrc_org_identity ON public.customer_service_read_cursor USING btree (organization_id, business_identity_id);


--
-- Name: i_css_org_enabled; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_css_org_enabled ON public.customer_service_seat USING btree (organization_id, enabled);


--
-- Name: i_cssc_org_ws_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_cssc_org_ws_id ON public.customer_service_seat_console USING btree (organization_id, workspace_id, id DESC);


--
-- Name: INDEX i_cssc_org_ws_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.i_cssc_org_ws_id IS '坐席控制台列表 keyset 访问路径（REVIEW-3 F-5 对齐 SQL_LIST_CONSOLES_PAGE）：
    (org, ws) 前缀过滤 + id DESC 尾列供游标分页反向扫描，无 status 谓词';


--
-- Name: i_csss_org_contact; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_csss_org_contact ON public.customer_service_session USING btree (organization_id, contact_id);


--
-- Name: i_csss_org_identity_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_csss_org_identity_active ON public.customer_service_session USING btree (organization_id, business_identity_id) WHERE (status = 'active'::text);


--
-- Name: i_csss_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_csss_org_status ON public.customer_service_session USING btree (organization_id, status);


--
-- Name: i_csvt_org_contact; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_csvt_org_contact ON public.customer_service_visit_token USING btree (organization_id, contact_id);


--
-- Name: i_csvt_widget_install; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_csvt_widget_install ON public.customer_service_visit_token USING btree (organization_id, widget_installation_id) WHERE (widget_installation_id IS NOT NULL);


--
-- Name: i_cswi_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_cswi_org_status ON public.customer_service_widget_installation USING btree (organization_id, status);


--
-- Name: i_cswn_expiry; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_cswn_expiry ON public.customer_service_widget_nonce USING btree (expires_at);


--
-- Name: i_datacenter_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_datacenter_active ON public.system_datacenter USING btree (is_active);


--
-- Name: i_datacenter_region; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_datacenter_region ON public.system_datacenter USING btree (region);


--
-- Name: i_dclog_action; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_dclog_action ON public.system_datacenter_log USING btree (action);


--
-- Name: i_dclog_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_dclog_created_at ON public.system_datacenter_log USING btree (created_at DESC);


--
-- Name: i_dclog_dc_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_dclog_dc_id ON public.system_datacenter_log USING btree (datacenter_id);


--
-- Name: i_ddl_status_newvsn; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ddl_status_newvsn ON public.app_ddl USING btree (status, new_vsn);


--
-- Name: i_dnd_rule_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_dnd_rule_status ON public.user_dnd_rule USING btree (user_id, status) WHERE (status = 1);


--
-- Name: i_ea_org_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ea_org_created ON public.enterprise_application USING btree (organization_id, created_at DESC, id DESC);


--
-- Name: INDEX i_ea_org_created; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.i_ea_org_created IS 'Admin 治理列表（迁移 00000143）：按组织分页浏览 Application，避免全表 Seq Scan';


--
-- Name: i_ea_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ea_org_status ON public.enterprise_application USING btree (organization_id, status);


--
-- Name: i_eac_org_app_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eac_org_app_status ON public.enterprise_application_credential USING btree (organization_id, application_id, status);


--
-- Name: i_eae_org_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eae_org_created ON public.enterprise_audit_event USING btree (organization_id, created_at DESC);


--
-- Name: i_eae_org_resource; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eae_org_resource ON public.enterprise_audit_event USING btree (organization_id, resource_type, resource_id);


--
-- Name: i_eag_org_app_status_expires; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eag_org_app_status_expires ON public.enterprise_application_grant USING btree (organization_id, application_id, status, expires_at);


--
-- Name: i_eagw_workspace_grant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eagw_workspace_grant ON public.enterprise_application_grant_workspace USING btree (workspace_id, grant_id);


--
-- Name: i_ear_org_app; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ear_org_app ON public.enterprise_attachment_retention USING btree (organization_id, application_id);


--
-- Name: i_ear_purgeable; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ear_purgeable ON public.enterprise_attachment_retention USING btree (retention_until) WHERE ((purge_state = 'live'::text) AND (hold_state = 'none'::text));


--
-- Name: i_ec_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ec_org_status ON public.enterprise_conversation USING btree (organization_id, workspace_id, status);


--
-- Name: i_eca_org_identity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eca_org_identity ON public.enterprise_contact_assignment USING btree (organization_id, business_identity_id, status);


--
-- Name: i_ego_org_app_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ego_org_app_status ON public.enterprise_group_origin USING btree (organization_id, application_id, status);


--
-- Name: i_eii_expires; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eii_expires ON public.enterprise_internal_idempotency USING btree (expires_at);


--
-- Name: i_em_org_conversation_retain; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_em_org_conversation_retain ON public.enterprise_message USING btree (organization_id, workspace_id, conversation_id, retain_until);


--
-- Name: i_em_org_retain_until; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_em_org_retain_until ON public.enterprise_message USING btree (organization_id, workspace_id, retain_until);


--
-- Name: i_emd_org_message; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_emd_org_message ON public.enterprise_message_delivery USING btree (organization_id, workspace_id, message_id, status);


--
-- Name: i_emo_msg; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_emo_msg ON public.enterprise_message_origin USING btree (conversation_kind, msg_row_id);


--
-- Name: i_emo_org_app; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_emo_org_app ON public.enterprise_message_origin USING btree (organization_id, application_id, created_at);


--
-- Name: i_enterprise_asset_org_message; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_enterprise_asset_org_message ON public.enterprise_asset USING btree (organization_id, workspace_id, message_id);


--
-- Name: i_enterprise_asset_org_retain; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_enterprise_asset_org_retain ON public.enterprise_asset USING btree (organization_id, workspace_id, retain_until);


--
-- Name: i_enterprise_asset_pending_delete; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_enterprise_asset_pending_delete ON public.enterprise_asset USING btree (organization_id, workspace_id, id) WHERE pending_object_delete;


--
-- Name: i_enterprise_contact_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_enterprise_contact_org_status ON public.enterprise_contact USING btree (organization_id, status);


--
-- Name: i_enterprise_note_org_contact; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_enterprise_note_org_contact ON public.enterprise_note USING btree (organization_id, contact_id, status);


--
-- Name: i_eoc_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eoc_org_status ON public.enterprise_offboarding_case USING btree (organization_id, status);


--
-- Name: i_eoi_org_case; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eoi_org_case ON public.enterprise_offboarding_item USING btree (organization_id, case_id, status);


--
-- Name: i_eoi_org_identity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eoi_org_identity ON public.enterprise_offboarding_item USING btree (organization_id, business_identity_id, status);


--
-- Name: i_eosc_org_app_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_eosc_org_app_user ON public.enterprise_oa_sso_code USING btree (organization_id, application_id, user_id);


--
-- Name: i_erh_active_conversation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_erh_active_conversation ON public.enterprise_retention_hold USING btree (organization_id, workspace_id, scope_conversation_id) WHERE ((released_at IS NULL) AND (scope_conversation_id IS NOT NULL));


--
-- Name: i_erh_active_message; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_erh_active_message ON public.enterprise_retention_hold USING btree (organization_id, workspace_id, scope_message_id) WHERE ((released_at IS NULL) AND (scope_message_id IS NOT NULL));


--
-- Name: i_erh_active_scope_message; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_erh_active_scope_message ON public.enterprise_retention_hold USING btree (organization_id, workspace_id, active_scope_message_id) WHERE (active_scope_message_id IS NOT NULL);


--
-- Name: i_erh_active_workspace; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_erh_active_workspace ON public.enterprise_retention_hold USING btree (organization_id, workspace_id) WHERE ((released_at IS NULL) AND (scope_type = 'workspace'::text));


--
-- Name: i_erp_org_ws_class; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_erp_org_ws_class ON public.enterprise_retention_policy USING btree (organization_id, workspace_id, data_class, version DESC);


--
-- Name: i_geo_people_nearby_geog; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_geo_people_nearby_geog ON public.geo_people_nearby USING gist (((location)::public.geography));


--
-- Name: i_gid_status_expiredat; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_gid_status_expiredat ON public.group_notice USING btree (group_id, status, expired_at);


--
-- Name: i_gm_grp_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_gm_grp_created ON public.group_member USING btree (group_id, created_at, id) WHERE (status = 1);


--
-- Name: INDEX i_gm_grp_created; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.i_gm_grp_created IS 'INT-27 GET /groups/{gid}/members 活跃成员升序分页访问路径（F2 EXPLAIN 155x；partial 只收 status=1）';


--
-- Name: i_group_log_type_optionuid_createdat; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_log_type_optionuid_createdat ON public.group_log USING btree (type, option_uid, created_at);


--
-- Name: i_group_notice_deleted_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_notice_deleted_at ON public.group_notice USING btree (deleted_at) WHERE (deleted_at IS NULL);


--
-- Name: i_group_notice_pinned; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_notice_pinned ON public.group_notice USING btree (group_id, pinned, deleted_at) WHERE (deleted_at IS NULL);


--
-- Name: i_group_random_code_groupid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX i_group_random_code_groupid ON public.group_random_code USING btree (group_id);


--
-- Name: i_group_random_code_location; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_random_code_location ON public.group_random_code USING gist (location);


--
-- Name: i_group_scope_personal; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_scope_personal ON public."group" USING btree (owner_uid, created_at DESC) WHERE ((scope = 'personal'::text) AND (status = 1));


--
-- Name: i_group_scope_ws; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_scope_ws ON public."group" USING btree (workspace_id, created_at DESC) WHERE ((scope = 'workspace'::text) AND (status = 1));


--
-- Name: i_group_task_assignment_learner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_task_assignment_learner ON public.group_task_assignment USING btree (learner_id) WHERE (learner_id IS NOT NULL);


--
-- Name: i_group_ws_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_group_ws_created ON public."group" USING btree (created_at DESC, id DESC) WHERE ((scope = 'workspace'::text) AND (status = 1));


--
-- Name: INDEX i_group_ws_created; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.i_group_ws_created IS 'INT-26 GET /groups org 域企业群 keyset 访问路径（F2 EXPLAIN 204x；partial 只收 workspace scope + 活跃群）';


--
-- Name: i_guardian_learner_guardian; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_guardian_learner_guardian ON public.guardian_learner USING btree (guardian_uid, status);


--
-- Name: i_guardian_learner_learner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_guardian_learner_learner ON public.guardian_learner USING btree (learner_id, status);


--
-- Name: i_homework_submission_queue; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_homework_submission_queue ON public.homework_submission USING btree (status, submitted_at DESC);


--
-- Name: i_id_segment_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_id_segment_active ON public.system_id_segment USING btree (is_active, expired_at) WHERE (is_active = true);


--
-- Name: i_id_segment_allocated_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_id_segment_allocated_at ON public.system_id_segment USING btree (allocated_at DESC);


--
-- Name: i_id_segment_dc_table; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_id_segment_dc_table ON public.system_id_segment USING btree (datacenter_id, table_name);


--
-- Name: i_id_segment_table; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_id_segment_table ON public.system_id_segment USING btree (table_name);


--
-- Name: i_idstats_dc_table_time; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_idstats_dc_table_time ON public.system_id_segment_stats USING btree (datacenter_id, table_name, stats_time DESC);


--
-- Name: i_learner_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_learner_org_status ON public.learner USING btree (organization_id, status);


--
-- Name: i_live_room_userid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_live_room_userid ON public.live_room USING btree (user_id);


--
-- Name: i_moya_invite_code_group_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_moya_invite_code_group_status ON public.moya_invite_code USING btree (group_id, status);


--
-- Name: i_moya_subscribe_grant_consume; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_moya_subscribe_grant_consume ON public.moya_subscribe_grant USING btree (uid, template_id, status, id DESC);


--
-- Name: i_msg_c2g_mentions; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_c2g_mentions ON public.msg_c2g USING gin (mentions);


--
-- Name: i_msg_mention_group_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_mention_group_uid ON public.msg_mention USING btree (group_id, mentioned_uid, created_at DESC);


--
-- Name: i_msg_mention_msg_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_mention_msg_id ON public.msg_mention USING btree (msg_id);


--
-- Name: i_msg_mention_uid_read; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_mention_uid_read ON public.msg_mention USING btree (mentioned_uid, is_read, created_at DESC);


--
-- Name: i_msg_store_conv_seq; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_store_conv_seq ON public.msg_store USING btree (conv_key, conv_seq);


--
-- Name: i_msg_store_from_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_store_from_id ON public.msg_store USING btree (from_id, created_at DESC);


--
-- Name: i_msg_store_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_store_group_id ON public.msg_store USING btree (group_id, created_at DESC) WHERE (group_id IS NOT NULL);


--
-- Name: i_msg_topic_type_userid_title; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_msg_topic_type_userid_title ON public.msg_topic USING btree (type, user_id, title);


--
-- Name: i_obi_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_obi_org_status ON public.organization_business_identity USING btree (organization_id, status);


--
-- Name: i_obia_org_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_obia_org_user ON public.organization_business_identity_assignment USING btree (organization_id, user_id);


--
-- Name: i_organization_department_member_org_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_department_member_org_user ON public.organization_department_member USING btree (organization_id, user_id);


--
-- Name: i_organization_department_member_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_department_member_user ON public.organization_department_member USING btree (user_id);


--
-- Name: i_organization_department_org_parent; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_department_org_parent ON public.organization_department USING btree (organization_id, parent_id);


--
-- Name: i_organization_department_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_department_org_status ON public.organization_department USING btree (organization_id, status);


--
-- Name: i_organization_invitation_org_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_invitation_org_created ON public.organization_invitation USING btree (organization_id, created_at DESC);


--
-- Name: i_organization_invitation_target_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_invitation_target_status ON public.organization_invitation USING btree (target_user_id, status, id DESC);


--
-- Name: i_organization_member_org_role_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_member_org_role_status ON public.organization_member USING btree (organization_id, role, status);


--
-- Name: i_organization_member_uid_org_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_member_uid_org_status ON public.organization_member USING btree (user_id, organization_id, status);


--
-- Name: i_organization_owner_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_owner_id ON public.organization USING btree (owner_id);


--
-- Name: i_organization_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_organization_status ON public.organization USING btree (status);


--
-- Name: i_owner_activation_invite_mobile; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_owner_activation_invite_mobile ON public.owner_activation_invite USING btree (mobile, id DESC);


--
-- Name: i_people_nearby_location; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_people_nearby_location ON public.geo_people_nearby USING gist (location);


--
-- Name: i_plugin_audit_log_event; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_plugin_audit_log_event ON public.plugin_audit_log USING btree (event, started_at DESC);


--
-- Name: i_plugin_audit_log_operation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_plugin_audit_log_operation ON public.plugin_audit_log USING btree (operation_id);


--
-- Name: i_plugin_audit_log_plugin_started; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_plugin_audit_log_plugin_started ON public.plugin_audit_log USING btree (plugin_name, started_at DESC);


--
-- Name: i_project_channel_rel_channel; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_project_channel_rel_channel ON public.project_channel_rel USING btree (channel_id);


--
-- Name: i_project_event_project_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_project_event_project_created ON public.project_event USING btree (project_id, created_at, id);


--
-- Name: i_project_member_ws_uid_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_project_member_ws_uid_status ON public.project_member USING btree (workspace_id, user_id, status);


--
-- Name: i_project_milestone_project_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_project_milestone_project_status ON public.project_milestone USING btree (project_id, status, id);


--
-- Name: i_project_task_project_status_sort; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_project_task_project_status_sort ON public.project_task USING btree (project_id, status, sort, id);


--
-- Name: i_project_workspace_id_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_project_workspace_id_id ON public.project USING btree (workspace_id, id);


--
-- Name: i_push_token_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_push_token_user_id ON public.push_token USING btree (user_id) WHERE (status = 1);


--
-- Name: i_review_asset_review; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_review_asset_review ON public.review_asset USING btree (review_id, kind, sort_order);


--
-- Name: i_s2c_action; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_s2c_action ON public.msg_s2c USING btree (action);


--
-- Name: i_s2c_fromid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_s2c_fromid ON public.msg_s2c USING btree (from_id);


--
-- Name: i_s2c_msgtype; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_s2c_msgtype ON public.msg_s2c USING btree (msg_type);


--
-- Name: i_s2c_toid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_s2c_toid ON public.msg_s2c USING btree (to_id);


--
-- Name: i_status_fromuid_cid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_status_fromuid_cid ON public.user_friend USING btree (status, from_user_id, category_id);


--
-- Name: i_status_owneruid_type; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_status_owneruid_type ON public."group" USING btree (status, owner_uid, type);


--
-- Name: i_submission_asset_submission; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_submission_asset_submission ON public.submission_asset USING btree (submission_id, kind, sort_order);


--
-- Name: i_teacher_review_reviewer; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_teacher_review_reviewer ON public.teacher_review USING btree (reviewer_uid) WHERE (reviewer_uid IS NOT NULL);


--
-- Name: i_teacher_review_submission; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_teacher_review_submission ON public.teacher_review USING btree (submission_id, status);


--
-- Name: i_teaching_admin_audit_learner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_teaching_admin_audit_learner ON public.teaching_admin_audit USING btree (learner_id, created_at DESC);


--
-- Name: i_teaching_admin_audit_operator; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_teaching_admin_audit_operator ON public.teaching_admin_audit USING btree (operator_uid, created_at DESC);


--
-- Name: i_uid_gid_isjoin; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_uid_gid_isjoin ON public.group_member USING btree (user_id, group_id, is_join);


--
-- Name: i_uid_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_uid_status ON public.group_member USING btree (user_id, status);


--
-- Name: i_user_collect_userid_status_kind; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_user_collect_userid_status_kind ON public.user_collect USING btree (user_id, status, kind);


--
-- Name: i_user_friend_category_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_user_friend_category_id ON public.user_friend USING btree (category_id) WHERE (category_id > 0);


--
-- Name: i_user_friend_from_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_user_friend_from_user_id ON public.user_friend USING btree (from_user_id);


--
-- Name: i_user_log_type_uid_createdat; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_user_log_type_uid_createdat ON public.user_log USING btree (type, uid, created_at);


--
-- Name: i_user_tag_relation_scene_tagid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_user_tag_relation_scene_tagid ON public.user_tag_relation USING btree (scene, tag_id);


--
-- Name: i_vsn_sort_updatedat; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_vsn_sort_updatedat ON public.app_version USING btree (sort, updated_at);


--
-- Name: i_vsn_status_type_regioncode; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_vsn_status_type_regioncode ON public.app_version USING btree (status, type, region_code);


--
-- Name: i_wallet_tx_userid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_wallet_tx_userid ON public.wallet_transaction USING btree (user_id);


--
-- Name: i_wallet_tx_walletid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_wallet_tx_walletid ON public.wallet_transaction USING btree (wallet_id);


--
-- Name: i_workspace_member_uid_ws_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_workspace_member_uid_ws_status ON public.workspace_member USING btree (user_id, workspace_id, status);


--
-- Name: i_workspace_member_ws_role_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_workspace_member_ws_role_status ON public.workspace_member USING btree (workspace_id, role, status);


--
-- Name: i_workspace_organization_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_workspace_organization_id ON public.workspace USING btree (organization_id);


--
-- Name: i_workspace_owner_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_workspace_owner_id ON public.workspace USING btree (owner_id);


--
-- Name: i_workspace_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_workspace_status ON public.workspace USING btree (status);


--
-- Name: i_ws_org_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX i_ws_org_created ON public.workspace USING btree (organization_id, created_at DESC, id DESC);


--
-- Name: INDEX i_ws_org_created; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.i_ws_org_created IS 'INT-24 GET /workspaces org 全域列表 keyset 访问路径（F2 EXPLAIN 148x；i_workspace_organization_id 的 (organization_id) 前缀可后续去重）';


--
-- Name: idx_adm_role_parent_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_adm_role_parent_id ON public.adm_role USING btree (parent_id);


--
-- Name: idx_adm_user_role_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_adm_user_role_id ON public.adm_user USING gin (role_id);


--
-- Name: idx_admin_op_logs_action; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_admin_op_logs_action ON public.admin_operation_logs USING btree (action, created_at DESC);


--
-- Name: idx_admin_op_logs_adm_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_admin_op_logs_adm_user ON public.admin_operation_logs USING btree (adm_user_id, created_at DESC);


--
-- Name: idx_agent_payment_compensation_due; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_agent_payment_compensation_due ON public.agent_payment_compensation USING btree (status, next_attempt_at, lease_until) WHERE ((status)::text = ANY ((ARRAY['settling'::character varying, 'pending'::character varying, 'processing'::character varying])::text[]));


--
-- Name: idx_agent_payment_compensation_reference; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_agent_payment_compensation_reference ON public.agent_payment_compensation USING btree (reference_no);


--
-- Name: idx_agent_payment_mandate_owner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_agent_payment_mandate_owner ON public.agent_payment_mandate USING btree (owner_uid);


--
-- Name: idx_ai_agent_category; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ai_agent_category ON public.ai_agent USING btree (category);


--
-- Name: idx_ai_agent_owner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ai_agent_owner ON public.ai_agent USING btree (owner_uid);


--
-- Name: idx_ai_agent_role_one_published; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_ai_agent_role_one_published ON public.ai_agent_role_version USING btree (role_code) WHERE ((state)::text = 'published'::text);


--
-- Name: idx_ai_agent_role_status_updated; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ai_agent_role_status_updated ON public.ai_agent_role USING btree (status, updated_at DESC);


--
-- Name: idx_ai_agent_role_version_role_state; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ai_agent_role_version_role_state ON public.ai_agent_role_version USING btree (role_code, state, version DESC);


--
-- Name: idx_ai_agent_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ai_agent_status ON public.ai_agent USING btree (status);


--
-- Name: idx_ai_agent_visibility; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ai_agent_visibility ON public.ai_agent USING btree (visibility);


--
-- Name: idx_announcement_adm_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_announcement_adm_user_id ON public.announcement USING btree (adm_user_id);


--
-- Name: idx_announcement_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_announcement_created_at ON public.announcement USING btree (created_at DESC);


--
-- Name: idx_announcement_pinned_true; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_announcement_pinned_true ON public.announcement USING btree (created_at DESC) WHERE (pinned = true);


--
-- Name: idx_announcement_status_time; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_announcement_status_time ON public.announcement USING btree (status, created_at DESC);


--
-- Name: idx_app_ddl_admin_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_app_ddl_admin_user_id ON public.app_ddl USING btree (admin_user_id);


--
-- Name: idx_app_upgrade_log_client_vsn; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_app_upgrade_log_client_vsn ON public.app_upgrade_log USING btree (client_vsn);


--
-- Name: idx_app_upgrade_log_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_app_upgrade_log_created_at ON public.app_upgrade_log USING btree (created_at);


--
-- Name: idx_app_upgrade_log_event; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_app_upgrade_log_event ON public.app_upgrade_log USING btree (event, created_at);


--
-- Name: idx_attachment_group_anchor_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_attachment_group_anchor_pending ON public.attachment USING btree (anchor_msg_id, creator_user_id, scope_ref) WHERE (((scope)::text = 'group'::text) AND (group_file_id IS NULL) AND (anchor_conv_seq IS NULL) AND (status >= 0));


--
-- Name: idx_attachment_group_file_binding; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_attachment_group_file_binding ON public.attachment USING btree (group_file_id, scope_ref, id) WHERE (((scope)::text = 'group'::text) AND (status >= 0) AND (group_file_id IS NOT NULL));


--
-- Name: idx_attachment_plaintext_backlog; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_attachment_plaintext_backlog ON public.attachment USING btree (created_at) WHERE (cipher IS NULL);


--
-- Name: idx_billing_invoice_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_invoice_status ON public.billing_invoice USING btree (status);


--
-- Name: idx_billing_invoice_sub; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_invoice_sub ON public.billing_invoice USING btree (subscription_id);


--
-- Name: idx_billing_plan_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_plan_status ON public.billing_plan USING btree (status);


--
-- Name: idx_billing_sub_owner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_sub_owner ON public.billing_subscription USING btree (owner_uid);


--
-- Name: idx_billing_sub_period_end; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_sub_period_end ON public.billing_subscription USING btree (current_period_end);


--
-- Name: idx_billing_sub_plan; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_sub_plan ON public.billing_subscription USING btree (plan_id);


--
-- Name: idx_billing_sub_tenant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_sub_tenant ON public.billing_subscription USING btree (tenant_id);


--
-- Name: idx_billing_usage_sub; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_billing_usage_sub ON public.billing_usage USING btree (subscription_id);


--
-- Name: idx_bot_is_public; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_is_public ON public.bot USING btree (is_public) WHERE (is_public = true);


--
-- Name: idx_bot_oauth_grant_bot; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_oauth_grant_bot ON public.bot_oauth_grant USING btree (bot_id);


--
-- Name: idx_bot_oauth_grant_token; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_oauth_grant_token ON public.bot_oauth_grant USING btree (access_token);


--
-- Name: idx_bot_oauth_grant_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_oauth_grant_user ON public.bot_oauth_grant USING btree (user_id);


--
-- Name: idx_bot_owner; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_owner ON public.bot USING btree (owner_uid);


--
-- Name: idx_bot_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_status ON public.bot USING btree (status);


--
-- Name: idx_bot_username; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_bot_username ON public.bot USING btree (username);


--
-- Name: idx_c2g_timeline_generation_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_c2g_timeline_generation_pending ON public.msg_c2g_timeline USING btree (to_uid, to_gid, conv_seq, created_at) WHERE ((client_ack = false) AND (conv_seq IS NOT NULL));


--
-- Name: idx_c2g_timeline_to_uid_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_c2g_timeline_to_uid_pending ON public.msg_c2g_timeline USING btree (to_uid) WHERE (client_ack = false);


--
-- Name: idx_c2g_timeline_workspace_preview; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_c2g_timeline_workspace_preview ON public.msg_c2g_timeline USING btree (to_uid, to_gid, conv_seq DESC, created_at DESC) WHERE (conv_seq IS NOT NULL);


--
-- Name: idx_c2g_timeline_workspace_unread; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_c2g_timeline_workspace_unread ON public.msg_c2g_timeline USING btree (to_uid, to_gid, conv_seq) WHERE (conv_seq IS NOT NULL);


--
-- Name: idx_channel_category_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_category_id ON public.channel USING btree (category_id) WHERE (status = 1);


--
-- Name: idx_channel_comment_channel_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_comment_channel_id ON public.channel_comment USING btree (channel_id, status, created_at);


--
-- Name: idx_channel_comment_message_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_comment_message_id ON public.channel_comment USING btree (message_id, status, created_at);


--
-- Name: idx_channel_comment_parent_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_comment_parent_id ON public.channel_comment USING btree (parent_id);


--
-- Name: idx_channel_comment_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_comment_user_id ON public.channel_comment USING btree (user_id, created_at);


--
-- Name: idx_channel_featured; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_featured ON public.channel USING btree (is_featured) WHERE ((status = 1) AND (is_featured = true));


--
-- Name: idx_channel_invitation_channel_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_channel_active ON public.channel_invitation USING btree (channel_id, status, created_at DESC) WHERE (status = ANY (ARRAY[0, 1]));


--
-- Name: idx_channel_invitation_channel_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_channel_id ON public.channel_invitation USING btree (channel_id);


--
-- Name: idx_channel_invitation_code; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_code ON public.channel_invitation USING btree (invitation_code) WHERE (invitation_code IS NOT NULL);


--
-- Name: idx_channel_invitation_invitee_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_invitee_pending ON public.channel_invitation USING btree (invitee_uid, status, expires_at) WHERE (status = 0);


--
-- Name: idx_channel_invitation_invitee_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_invitee_uid ON public.channel_invitation USING btree (invitee_uid);


--
-- Name: idx_channel_invitation_inviter_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_inviter_uid ON public.channel_invitation USING btree (inviter_uid);


--
-- Name: idx_channel_invitation_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_invitation_status ON public.channel_invitation USING btree (status);


--
-- Name: idx_channel_order_channel_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_channel_id ON public.channel_order USING btree (channel_id);


--
-- Name: idx_channel_order_channel_paid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_channel_paid ON public.channel_order USING btree (channel_id, status) WHERE (status = 1);


--
-- Name: idx_channel_order_order_no; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_order_no ON public.channel_order USING btree (order_no);


--
-- Name: idx_channel_order_payment_no; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_payment_no ON public.channel_order USING btree (payment_no) WHERE (payment_no IS NOT NULL);


--
-- Name: idx_channel_order_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_status ON public.channel_order USING btree (status);


--
-- Name: idx_channel_order_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_user_id ON public.channel_order USING btree (user_id);


--
-- Name: idx_channel_order_user_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_order_user_status ON public.channel_order USING btree (user_id, status, created_at DESC);


--
-- Name: idx_channel_price_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_price_status ON public.channel_price USING btree (status);


--
-- Name: idx_channel_webhook_channel_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_webhook_channel_id ON public.channel_webhook USING btree (channel_id);


--
-- Name: idx_channel_webhook_grace_digest; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_channel_webhook_grace_digest ON public.channel_webhook USING btree (grace_digest);


--
-- Name: idx_compliance_key_created_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_compliance_key_created_by ON public.compliance_key USING btree (created_by) WHERE (created_by IS NOT NULL);


--
-- Name: idx_compliance_key_revoked_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_compliance_key_revoked_by ON public.compliance_key USING btree (revoked_by) WHERE (revoked_by IS NOT NULL);


--
-- Name: idx_compliance_key_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_compliance_key_status ON public.compliance_key USING btree (status) WHERE (status = 1);


--
-- Name: idx_conversation_client_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_conversation_client_id ON public.conversation USING btree (client_id);


--
-- Name: idx_conversation_delete_conversation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_conversation_delete_conversation ON public.conversation_delete USING btree (conversation_id, conversation_type);


--
-- Name: idx_conversation_delete_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_conversation_delete_user ON public.conversation_delete USING btree (user_id, deleted_at DESC);


--
-- Name: idx_conversation_pin_conversation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_conversation_pin_conversation ON public.conversation_pin USING btree (conversation_id, conversation_type);


--
-- Name: idx_conversation_pin_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_conversation_pin_user ON public.conversation_pin USING btree (user_id, pinned_at DESC);


--
-- Name: idx_e2ee_group_session_member_grant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_e2ee_group_session_member_grant ON public.e2ee_group_session_member USING btree (group_id, user_id, generation_no, session_id);


--
-- Name: idx_e2ee_key_shares_owner_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_e2ee_key_shares_owner_uid ON public.e2ee_key_shares USING btree (owner_uid);


--
-- Name: idx_e2ee_key_shares_share_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_e2ee_key_shares_share_index ON public.e2ee_key_shares USING btree (share_index);


--
-- Name: idx_e2ee_key_shares_trustee_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_e2ee_key_shares_trustee_uid ON public.e2ee_key_shares USING btree (trustee_uid);


--
-- Name: idx_feedback_reply_feedback_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_feedback_reply_feedback_id ON public.feedback_reply USING btree (feedback_id, created_at DESC);


--
-- Name: idx_feedback_reply_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_feedback_reply_uid ON public.feedback_reply USING btree (replier_user_id);


--
-- Name: idx_feedback_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_feedback_uid ON public.feedback USING btree (user_id);


--
-- Name: idx_fts_channel_token; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_fts_channel_token ON public.fts_channel USING gin (token);


--
-- Name: idx_fts_group_token; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_fts_group_token ON public.fts_group USING gin (token);


--
-- Name: idx_geo_updated_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_geo_updated_at ON public.geo_people_nearby USING btree (updated_at);


--
-- Name: idx_gmg_group_text_user_open; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_gmg_group_text_user_open ON public.group_member_generation USING btree (((group_id)::text), user_id) WHERE (end_seq IS NULL);


--
-- Name: idx_gmg_group_user_open; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_gmg_group_user_open ON public.group_member_generation USING btree (group_id, user_id) WHERE (end_seq IS NULL);


--
-- Name: idx_group_album_group; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_group ON public.group_album USING btree (group_id, created_at DESC);


--
-- Name: idx_group_album_photo_album; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_photo_album ON public.group_album_photo USING btree (album_id, created_at DESC);


--
-- Name: idx_group_album_photo_comment_photo; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_photo_comment_photo ON public.group_album_photo_comment USING btree (photo_id, created_at DESC);


--
-- Name: idx_group_album_photo_comment_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_photo_comment_user ON public.group_album_photo_comment USING btree (user_id);


--
-- Name: idx_group_album_photo_group; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_photo_group ON public.group_album_photo USING btree (group_id, created_at DESC);


--
-- Name: idx_group_album_photo_like_photo; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_photo_like_photo ON public.group_album_photo_like USING btree (photo_id);


--
-- Name: idx_group_album_photo_uploader; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_photo_uploader ON public.group_album_photo USING btree (uploader_id);


--
-- Name: idx_group_album_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_album_uid ON public.group_album USING btree (creator_id);


--
-- Name: idx_group_category_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_category_id ON public."group" USING btree (category_id) WHERE ((status = 1) AND (type = 1));


--
-- Name: idx_group_featured; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_featured ON public."group" USING btree (is_featured) WHERE ((status = 1) AND (type = 1) AND (is_featured = true));


--
-- Name: idx_group_file_category; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_file_category ON public.group_file USING btree (group_id, file_category);


--
-- Name: idx_group_file_file_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_file_file_id ON public.group_file USING btree (file_id);


--
-- Name: idx_group_file_group; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_file_group ON public.group_file USING btree (group_id, created_at DESC);


--
-- Name: idx_group_file_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_file_name ON public.group_file USING btree (group_id, file_name);


--
-- Name: idx_group_file_uploader; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_file_uploader ON public.group_file USING btree (uploader_id);


--
-- Name: idx_group_log_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_log_group_id ON public.group_log USING btree (group_id);


--
-- Name: idx_group_member_category; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_member_category ON public.group_member USING btree (user_id, category_id) WHERE (category_id > 0);


--
-- Name: idx_group_member_mute; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_member_mute ON public.group_member USING btree (group_id, user_id) WHERE (mute_until IS NOT NULL);


--
-- Name: idx_group_member_role; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_member_role ON public.group_member USING btree (group_id, role);


--
-- Name: idx_group_notice_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_notice_group_id ON public.group_notice USING btree (group_id);


--
-- Name: idx_group_random_code_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_random_code_uid ON public.group_random_code USING btree (user_id);


--
-- Name: idx_group_schedule_creator_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_creator_id ON public.group_schedule USING btree (creator_id);


--
-- Name: idx_group_schedule_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_group_id ON public.group_schedule USING btree (group_id);


--
-- Name: idx_group_schedule_group_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_group_status ON public.group_schedule USING btree (group_id, status);


--
-- Name: idx_group_schedule_participant_schedule_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_participant_schedule_id ON public.group_schedule_participant USING btree (schedule_id);


--
-- Name: idx_group_schedule_participant_schedule_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_participant_schedule_status ON public.group_schedule_participant USING btree (schedule_id, status);


--
-- Name: idx_group_schedule_participant_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_participant_user_id ON public.group_schedule_participant USING btree (user_id);


--
-- Name: idx_group_schedule_remind_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_remind_pending ON public.group_schedule_remind USING btree (remind_at) WHERE (is_sent = false);


--
-- Name: idx_group_schedule_remind_remind_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_remind_remind_at ON public.group_schedule_remind USING btree (remind_at);


--
-- Name: idx_group_schedule_remind_schedule_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_remind_schedule_id ON public.group_schedule_remind USING btree (schedule_id);


--
-- Name: idx_group_schedule_remind_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_remind_user_id ON public.group_schedule_remind USING btree (user_id);


--
-- Name: idx_group_schedule_start_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_schedule_start_at ON public.group_schedule USING btree (start_at);


--
-- Name: idx_group_tag_created_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_tag_created_by ON public.group_tag USING btree (created_by);


--
-- Name: idx_group_tag_group; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_tag_group ON public.group_tag USING btree (group_id);


--
-- Name: idx_group_tag_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_tag_name ON public.group_tag USING btree (tag_name);


--
-- Name: idx_group_task_assignment_task_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_assignment_task_id ON public.group_task_assignment USING btree (task_id);


--
-- Name: idx_group_task_assignment_task_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_assignment_task_status ON public.group_task_assignment USING btree (task_id, status);


--
-- Name: idx_group_task_assignment_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_assignment_user_id ON public.group_task_assignment USING btree (user_id);


--
-- Name: idx_group_task_deleted_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_deleted_at ON public.group_task USING btree (deleted_at) WHERE (deleted_at IS NULL);


--
-- Name: idx_group_task_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_group_id ON public.group_task USING btree (group_id);


--
-- Name: idx_group_task_group_status_alive; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_group_status_alive ON public.group_task USING btree (group_id, status, id DESC) WHERE (deleted_at IS NULL);


--
-- Name: idx_group_task_task_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_task_task_id ON public.group_task USING btree (task_id);


--
-- Name: idx_group_vote_creator_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_creator_id ON public.group_vote USING btree (creator_id);


--
-- Name: idx_group_vote_group_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_group_id ON public.group_vote USING btree (group_id);


--
-- Name: idx_group_vote_group_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_group_status ON public.group_vote USING btree (group_id, status);


--
-- Name: idx_group_vote_option_option_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_option_option_id ON public.group_vote_option USING btree (option_id);


--
-- Name: idx_group_vote_option_vote_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_option_vote_id ON public.group_vote_option USING btree (vote_id);


--
-- Name: idx_group_vote_record_options; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_record_options ON public.group_vote_record USING gin (option_ids);


--
-- Name: idx_group_vote_record_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_record_user_id ON public.group_vote_record USING btree (user_id);


--
-- Name: idx_group_vote_record_vote_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_record_vote_id ON public.group_vote_record USING btree (vote_id);


--
-- Name: idx_group_vote_vote_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_group_vote_vote_id ON public.group_vote USING btree (vote_id);


--
-- Name: idx_live_room_status_time; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_live_room_status_time ON public.live_room USING btree (status, created_at DESC);


--
-- Name: idx_mcp_audit_client; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_mcp_audit_client ON public.mcp_audit_log USING btree (client_id);


--
-- Name: idx_mcp_audit_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_mcp_audit_created ON public.mcp_audit_log USING btree (created_at DESC);


--
-- Name: idx_mcp_client_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_mcp_client_status ON public.mcp_client USING btree (status);


--
-- Name: idx_mcp_grant_client; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_mcp_grant_client ON public.mcp_client_grant USING btree (client_id);


--
-- Name: idx_moderation_action_case; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moderation_action_case ON public.moderation_action USING btree (case_id);


--
-- Name: idx_moderation_action_status_end; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moderation_action_status_end ON public.moderation_action USING btree (status, end_at) WHERE ((action)::text = ANY ((ARRAY['group_mute'::character varying, 'account_restrict'::character varying])::text[]));


--
-- Name: idx_moderation_action_target_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moderation_action_target_uid ON public.moderation_action USING btree (target_uid) WHERE (target_uid > 0);


--
-- Name: idx_moderation_appeal_appellant; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moderation_appeal_appellant ON public.moderation_appeal USING btree (appellant_uid, created_at DESC);


--
-- Name: idx_moderation_appeal_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moderation_appeal_status_created ON public.moderation_appeal USING btree (status, created_at DESC);


--
-- Name: idx_moment_comment_post_active_id_desc; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_comment_post_active_id_desc ON public.moment_comment USING btree (post_id, id DESC) WHERE (status = 1);


--
-- Name: idx_moment_comment_post_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_comment_post_created ON public.moment_comment USING btree (post_id, created_at DESC);


--
-- Name: idx_moment_comment_user_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_comment_user_created ON public.moment_comment USING btree (user_id, created_at DESC);


--
-- Name: idx_moment_like_post_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_like_post_created ON public.moment_like USING btree (post_id, created_at DESC);


--
-- Name: idx_moment_like_post_id_desc; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_like_post_id_desc ON public.moment_like USING btree (post_id, id DESC);


--
-- Name: idx_moment_like_user_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_like_user_created ON public.moment_like USING btree (user_id, created_at DESC);


--
-- Name: idx_moment_post_acl_post; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_acl_post ON public.moment_post_acl USING btree (post_id, acl_type);


--
-- Name: idx_moment_post_acl_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_acl_uid ON public.moment_post_acl USING btree (uid, acl_type);


--
-- Name: idx_moment_post_active_id_desc; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_active_id_desc ON public.moment_post USING btree (id DESC) WHERE (status = 1);


--
-- Name: idx_moment_post_author_active_id_desc; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_author_active_id_desc ON public.moment_post USING btree (author_uid, id DESC) WHERE (status = 1);


--
-- Name: idx_moment_post_author_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_author_created ON public.moment_post USING btree (author_uid, created_at DESC);


--
-- Name: idx_moment_post_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_status_created ON public.moment_post USING btree (status, created_at DESC);


--
-- Name: idx_moment_post_visibility_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_post_visibility_status ON public.moment_post USING btree (visibility, status, created_at DESC);


--
-- Name: idx_moment_report_pending_id_desc; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_report_pending_id_desc ON public.moment_report USING btree (id DESC) WHERE (status = 0);


--
-- Name: idx_moment_report_post; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_report_post ON public.moment_report USING btree (post_id, created_at DESC);


--
-- Name: idx_moment_report_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_report_status_created ON public.moment_report USING btree (status, created_at DESC);


--
-- Name: idx_moment_timeline_author; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_timeline_author ON public.moment_timeline USING btree (author_uid, rank_at DESC);


--
-- Name: idx_moment_timeline_recipient_active_id_desc; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_timeline_recipient_active_id_desc ON public.moment_timeline USING btree (recipient_uid, id DESC) WHERE (status = 1);


--
-- Name: idx_moment_timeline_uid_rank; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_moment_timeline_uid_rank ON public.moment_timeline USING btree (recipient_uid, rank_at DESC, id DESC);


--
-- Name: idx_msg_c2c_payload_fts; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2c_payload_fts ON public.msg_c2c USING gin (to_tsvector('public.jiebacfg'::regconfig, payload)) WHERE (e2ee IS NULL);


--
-- Name: idx_msg_c2c_pinned; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2c_pinned ON public.msg_c2c USING btree (to_id, pinned) WHERE (pinned = true);


--
-- Name: idx_msg_c2c_reply; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2c_reply ON public.msg_c2c USING btree (reply_to_msg_id) WHERE (reply_to_msg_id IS NOT NULL);


--
-- Name: idx_msg_c2g_payload_fts; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2g_payload_fts ON public.msg_c2g USING gin (to_tsvector('public.jiebacfg'::regconfig, (payload)::text)) WHERE (e2ee IS NULL);


--
-- Name: idx_msg_c2g_pinned; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2g_pinned ON public.msg_c2g USING btree (to_id, pinned) WHERE (pinned = true);


--
-- Name: idx_msg_c2g_recipient_snapshot_group_seq; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2g_recipient_snapshot_group_seq ON public.msg_c2g_recipient_snapshot USING btree (to_gid, conv_seq);


--
-- Name: idx_msg_c2g_reply; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2g_reply ON public.msg_c2g USING btree (reply_to_msg_id) WHERE (reply_to_msg_id IS NOT NULL);


--
-- Name: idx_msg_c2g_request_ledger_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_c2g_request_ledger_created_at ON public.msg_c2g_request_ledger USING btree (created_at);


--
-- Name: idx_msg_delivery_uid_did; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_delivery_uid_did ON public.msg_delivery USING btree (to_uid, to_did);


--
-- Name: idx_msg_forward_from; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_forward_from ON public.msg_forward USING btree (forward_from_id);


--
-- Name: idx_msg_forward_original; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_forward_original ON public.msg_forward USING btree (original_msg_id);


--
-- Name: idx_msg_forward_to; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_forward_to ON public.msg_forward USING btree (forward_to_id);


--
-- Name: idx_msg_mention_from_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_mention_from_uid ON public.msg_mention USING btree (from_uid, created_at DESC);


--
-- Name: idx_msg_reaction_emoji; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_reaction_emoji ON public.msg_reaction USING btree (msg_id, msg_type, emoji);


--
-- Name: idx_msg_reaction_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_reaction_user ON public.msg_reaction USING btree (user_id);


--
-- Name: idx_msg_read_from_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_read_from_uid ON public.msg_read USING btree (from_uid);


--
-- Name: idx_msg_read_msg_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_read_msg_id ON public.msg_read USING btree (msg_id);


--
-- Name: idx_msg_read_read_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_read_read_at ON public.msg_read USING btree (read_at);


--
-- Name: idx_msg_read_to_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_msg_read_to_uid ON public.msg_read USING btree (to_uid);


--
-- Name: idx_olm_identity_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_olm_identity_user_id ON public.olm_identity USING btree (user_id);


--
-- Name: idx_olm_one_time_key_device; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_olm_one_time_key_device ON public.olm_one_time_key USING btree (user_id, device_id);


--
-- Name: idx_olm_otk_available; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_olm_otk_available ON public.olm_one_time_key USING btree (user_id, device_id) WHERE ((status)::text = 'available'::text);


--
-- Name: idx_olm_otk_consumed; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_olm_otk_consumed ON public.olm_one_time_key USING btree (consumed_at) WHERE ((status)::text = 'claimed'::text);


--
-- Name: idx_organization_invite_code_org; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_organization_invite_code_org ON public.organization_invite_code USING btree (organization_id);


--
-- Name: idx_payment_tx_biz_order; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_payment_tx_biz_order ON public.payment_transaction USING btree (biz_order_no);


--
-- Name: idx_payment_tx_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_payment_tx_user_id ON public.payment_transaction USING btree (user_id);


--
-- Name: idx_push_token_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_push_token_uid ON public.push_token USING btree (user_id);


--
-- Name: idx_recharge_order_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_recharge_order_status ON public.recharge_order USING btree (status);


--
-- Name: idx_recharge_order_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_recharge_order_user_id ON public.recharge_order USING btree (user_id);


--
-- Name: idx_red_packet_recv_packet; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_red_packet_recv_packet ON public.red_packet_receive USING btree (red_packet_id);


--
-- Name: idx_red_packet_recv_receiver; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_red_packet_recv_receiver ON public.red_packet_receive USING btree (receiver_uid);


--
-- Name: idx_red_packet_sender; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_red_packet_sender ON public.red_packet USING btree (sender_uid);


--
-- Name: idx_red_packet_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_red_packet_status ON public.red_packet USING btree (status);


--
-- Name: idx_report_action_operator_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_action_operator_created ON public.report_action_log USING btree (operator_uid, created_at DESC);


--
-- Name: idx_report_action_report_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_action_report_created ON public.report_action_log USING btree (report_id, created_at DESC);


--
-- Name: idx_report_ticket_author; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_ticket_author ON public.report_ticket USING btree (target_author_id) WHERE (target_author_id > 0);


--
-- Name: idx_report_ticket_handled_by; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_ticket_handled_by ON public.report_ticket USING btree (handled_by) WHERE (handled_by IS NOT NULL);


--
-- Name: idx_report_ticket_reporter_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_ticket_reporter_created ON public.report_ticket USING btree (reporter_uid, created_at DESC);


--
-- Name: idx_report_ticket_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_ticket_status_created ON public.report_ticket USING btree (status, created_at DESC);


--
-- Name: idx_report_ticket_sub_scope; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_ticket_sub_scope ON public.report_ticket USING btree (target_sub_type, target_scope_id) WHERE ((target_sub_type)::text <> (''::character varying)::text);


--
-- Name: idx_report_ticket_target_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_report_ticket_target_status ON public.report_ticket USING btree (target_type, target_id, status, created_at DESC);


--
-- Name: idx_review_queue_status_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_review_queue_status_created ON public.review_queue USING btree (review_status, created_at DESC);


--
-- Name: idx_sensitive_word_category; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_sensitive_word_category ON public.sensitive_word USING btree (category);


--
-- Name: idx_sso_identity_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_sso_identity_uid ON public.sso_identity USING btree (uid);


--
-- Name: idx_transfer_receiver; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_transfer_receiver ON public.transfer_order USING btree (receiver_uid);


--
-- Name: idx_transfer_sender; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_transfer_sender ON public.transfer_order USING btree (sender_uid);


--
-- Name: idx_transfer_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_transfer_status ON public.transfer_order USING btree (status);


--
-- Name: idx_trust_audit_actor; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_trust_audit_actor ON public.trust_audit USING btree (actor_uid);


--
-- Name: idx_trust_audit_target; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_trust_audit_target ON public.trust_audit USING btree (target_uid, target_device_id);


--
-- Name: idx_user_deletion_job_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_deletion_job_status ON public.user_deletion_job USING btree (status, attempts);


--
-- Name: idx_user_deletion_request_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_deletion_request_status ON public.user_deletion_request USING btree (status, requested_at);


--
-- Name: idx_user_denylist_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_denylist_uid ON public.user_denylist USING btree (user_id);


--
-- Name: idx_user_device_capabilities; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_device_capabilities ON public.user_device USING gin (capabilities);


--
-- Name: idx_user_device_key_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_device_key_id ON public.user_device USING btree (key_id) WHERE ((key_id)::text <> ''::text);


--
-- Name: idx_user_device_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_device_uid ON public.user_device USING btree (user_id);


--
-- Name: idx_user_device_uid_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_device_uid_active ON public.user_device USING btree (user_id) WHERE (status = 1);


--
-- Name: idx_user_friend_category_owner_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_friend_category_owner_uid ON public.user_friend_category USING btree (owner_user_id);


--
-- Name: idx_user_group_category_user; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_group_category_user ON public.user_group_category USING btree (user_id, sort_order);


--
-- Name: idx_user_group_category_user_name; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_user_group_category_user_name ON public.user_group_category USING btree (user_id, category_name);


--
-- Name: idx_user_tag_creator_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_user_tag_creator_uid ON public.user_tag USING btree (creator_user_id);


--
-- Name: idx_wallet_tx_reference_no; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_wallet_tx_reference_no ON public.wallet_transaction USING btree (reference_no) WHERE ((reference_no)::text <> ''::text);


--
-- Name: idx_workspace_invite_workspace_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_workspace_invite_workspace_id ON public.workspace_invite USING btree (workspace_id);


--
-- Name: mcp_client_client_key_uq; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX mcp_client_client_key_uq ON public.mcp_client USING btree (client_key);


--
-- Name: mcp_client_digest_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX mcp_client_digest_idx ON public.mcp_client USING btree (credential_digest);


--
-- Name: mcp_client_owner_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX mcp_client_owner_idx ON public.mcp_client USING btree (owner_uid);


--
-- Name: msg_c2c_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_c2c_created_at_idx ON public.msg_c2c USING btree (created_at DESC);


--
-- Name: msg_c2g_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_c2g_created_at_idx ON public.msg_c2g USING btree (created_at DESC);


--
-- Name: msg_c2g_timeline_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_c2g_timeline_created_at_idx ON public.msg_c2g_timeline USING btree (created_at DESC);


--
-- Name: msg_c2s_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_c2s_created_at_idx ON public.msg_c2s USING btree (created_at DESC);


--
-- Name: msg_read_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_read_created_at_idx ON public.msg_read USING btree (created_at DESC);


--
-- Name: msg_s2c_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_s2c_created_at_idx ON public.msg_s2c USING btree (created_at DESC);


--
-- Name: msg_store_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_store_created_at_idx ON public.msg_store USING btree (created_at DESC);


--
-- Name: msg_store_staging_available_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_store_staging_available_at_idx ON public.msg_store_staging USING btree (available_at) WHERE (processed_at IS NULL);


--
-- Name: msg_store_staging_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_store_staging_created_at_idx ON public.msg_store_staging USING btree (created_at);


--
-- Name: msg_store_staging_processed_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX msg_store_staging_processed_at_idx ON public.msg_store_staging USING btree (processed_at) WHERE (processed_at IS NOT NULL);


--
-- Name: uk_account; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_account ON public."user" USING btree (account);


--
-- Name: uk_adm_account; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_adm_account ON public.adm_user USING btree (account);


--
-- Name: uk_adm_email; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_adm_email ON public.adm_user USING btree (email);


--
-- Name: uk_adm_mobile; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_adm_mobile ON public.adm_user USING btree (mobile);


--
-- Name: uk_adm_role_name_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_adm_role_name_active ON public.adm_role USING btree (role_name) WHERE (status <> '-1'::integer);


--
-- Name: uk_c2c_msgid_createdat; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_c2c_msgid_createdat ON public.msg_c2c USING btree (msg_id, created_at);


--
-- Name: uk_c2g_msgid_createdat; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_c2g_msgid_createdat ON public.msg_c2g USING btree (msg_id, created_at);


--
-- Name: uk_c2g_timeline_touid_msgid_createdat; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_c2g_timeline_touid_msgid_createdat ON public.msg_c2g_timeline USING btree (to_uid, msg_id, created_at);


--
-- Name: uk_channel_invitation_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_channel_invitation_pending ON public.channel_invitation USING btree (channel_id, invitee_uid) WHERE (status = 0);


--
-- Name: uk_channel_webhook_token_digest; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_channel_webhook_token_digest ON public.channel_webhook USING btree (token_digest);


--
-- Name: uk_crd_active_per_submission; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_crd_active_per_submission ON public.calligraphy_review_draft USING btree (submission_id) WHERE (status = ANY (ARRAY['queued'::text, 'running'::text, 'succeeded'::text]));


--
-- Name: uk_cvt_userid_type_peerid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_cvt_userid_type_peerid ON public.conversation USING btree (user_id, type, peer_id);


--
-- Name: uk_dnd_rule_userid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_dnd_rule_userid ON public.user_dnd_rule USING btree (user_id);


--
-- Name: uk_email; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_email ON public."user" USING btree (email) WHERE ((email IS NOT NULL) AND ((email)::text <> ''::text));


--
-- Name: uk_fromuid_touid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_fromuid_touid ON public.user_friend USING btree (from_user_id, to_user_id);


--
-- Name: uk_gid_uid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_gid_uid ON public.group_member USING btree (group_id, user_id);


--
-- Name: uk_group_task_assignment_task_learner; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_group_task_assignment_task_learner ON public.group_task_assignment USING btree (task_id, learner_id) WHERE (learner_id IS NOT NULL);


--
-- Name: uk_group_task_idempotency; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_group_task_idempotency ON public.group_task USING btree (creator_id, group_id, idempotency_key) WHERE (idempotency_key IS NOT NULL);


--
-- Name: uk_homework_submission_idempotency; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_homework_submission_idempotency ON public.homework_submission USING btree (submitted_by, assignment_id, idempotency_key) WHERE (idempotency_key IS NOT NULL);


--
-- Name: uk_learner_org_user; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_learner_org_user ON public.learner USING btree (organization_id, user_id) WHERE (user_id IS NOT NULL);


--
-- Name: uk_mobile; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_mobile ON public."user" USING btree (mobile) WHERE ((mobile IS NOT NULL) AND ((mobile)::text <> ''::text));


--
-- Name: uk_msg_read_msg_to_did_created; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_msg_read_msg_to_did_created ON public.msg_read USING btree (msg_id, to_uid, to_did, created_at);


--
-- Name: uk_msg_store_msg_id_created_at; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_msg_store_msg_id_created_at ON public.msg_store USING btree (msg_id, created_at);


--
-- Name: uk_olm_otk_claim_request; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_olm_otk_claim_request ON public.olm_one_time_key USING btree (claimed_by, user_id, device_id, claim_request_id) WHERE (claim_request_id IS NOT NULL);


--
-- Name: uk_organization_invite_code; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_organization_invite_code ON public.organization_invite_code USING btree (code);


--
-- Name: uk_organization_invite_code_org_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_organization_invite_code_org_active ON public.organization_invite_code USING btree (organization_id) WHERE (status = 'active'::text);


--
-- Name: uk_push_token_user_device; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_push_token_user_device ON public.push_token USING btree (user_id, device_id) WHERE (status = 1);


--
-- Name: uk_review_asset_one_video_per_review; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_review_asset_one_video_per_review ON public.review_asset USING btree (review_id) WHERE (kind = 'feedback_video'::text);


--
-- Name: uk_sensitive_word_word; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_sensitive_word_word ON public.sensitive_word USING btree (word);


--
-- Name: uk_sso_config_provider; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_sso_config_provider ON public.sso_config USING btree (provider);


--
-- Name: uk_sso_identity_provider_subject; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_sso_identity_provider_subject ON public.sso_identity USING btree (provider, subject);


--
-- Name: uk_tr_published_per_submission; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_tr_published_per_submission ON public.teacher_review USING btree (submission_id) WHERE (status = 'published'::text);


--
-- Name: uk_trust_audit_event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_trust_audit_event_id ON public.trust_audit USING btree (event_id) WHERE (event_id IS NOT NULL);


--
-- Name: uk_ug_uid_gid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_ug_uid_gid ON public.user_group USING btree (user_id, group_id);


--
-- Name: uk_user_tag_relation_scene_userid_objectid_tagid; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_user_tag_relation_scene_userid_objectid_tagid ON public.user_tag_relation USING btree (scene, user_id, object_id, tag_id);


--
-- Name: uk_workspace_invite_code; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_workspace_invite_code ON public.workspace_invite USING btree (code);


--
-- Name: uk_workspace_invite_ws_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uk_workspace_invite_ws_active ON public.workspace_invite USING btree (workspace_id) WHERE (status = 'active'::text);


--
-- Name: uniq_agent_payment_mandate_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_agent_payment_mandate_active ON public.agent_payment_mandate USING btree (agent_uid) WHERE (status = 1);


--
-- Name: uniq_billing_invoice_sub_period; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_billing_invoice_sub_period ON public.billing_invoice USING btree (subscription_id, period_start, period_end);


--
-- Name: uniq_billing_sub_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_billing_sub_active ON public.billing_subscription USING btree (tenant_id) WHERE (status = ANY (ARRAY[0, 1]));


--
-- Name: uniq_billing_usage_sub_metric_period; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_billing_usage_sub_metric_period ON public.billing_usage USING btree (subscription_id, metric, period);


--
-- Name: uniq_channel_order_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_channel_order_pending ON public.channel_order USING btree (channel_id, user_id) WHERE (status = 0);


--
-- Name: uniq_payment_tx_gateway_no; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_payment_tx_gateway_no ON public.payment_transaction USING btree (gateway, gateway_payment_no) WHERE (gateway_payment_no IS NOT NULL);


--
-- Name: uniq_red_packet_receiver; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_red_packet_receiver ON public.red_packet_receive USING btree (red_packet_id, receiver_uid);


--
-- Name: uq_channel_message_request_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_channel_message_request_id ON public.channel_message USING btree (author_id, channel_id, request_id) WHERE (request_id IS NOT NULL);


--
-- Name: uq_cssc_org_ws_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_cssc_org_ws_active ON public.customer_service_seat_console USING btree (organization_id, workspace_id) WHERE (status = 'active'::text);


--
-- Name: uq_csss_org_conv_open; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_csss_org_conv_open ON public.customer_service_session USING btree (organization_id, conversation_id) WHERE (status <> 'closed'::text);


--
-- Name: uq_ec_org_id_contact; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_ec_org_id_contact ON public.enterprise_conversation USING btree (organization_id, id, contact_id);


--
-- Name: uq_eca_active_contact_identity; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_eca_active_contact_identity ON public.enterprise_contact_assignment USING btree (organization_id, contact_id, business_identity_id) WHERE (status = 'active'::text);


--
-- Name: uq_eca_primary_contact; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_eca_primary_contact ON public.enterprise_contact_assignment USING btree (organization_id, contact_id) WHERE ((role = 'primary'::text) AND (status = 'active'::text));


--
-- Name: uq_eoc_unfinished_leaver; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_eoc_unfinished_leaver ON public.enterprise_offboarding_case USING btree (organization_id, leaver_user_id) WHERE (status <> ALL (ARRAY['completed'::text, 'failed'::text]));


--
-- Name: uq_ewh_delivery_replay_inflight; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_ewh_delivery_replay_inflight ON public.bot_delivery USING btree (ewh_replay_of) WHERE ((ewh_replay_of IS NOT NULL) AND (status = ANY (ARRAY['pending'::text, 'retry'::text])));


--
-- Name: uq_gmg_one_open_generation; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_gmg_one_open_generation ON public.group_member_generation USING btree (group_id, user_id) WHERE (end_seq IS NULL);


--
-- Name: uq_obia_active_identity; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_obia_active_identity ON public.organization_business_identity_assignment USING btree (organization_id, business_identity_id) WHERE (status = 'active'::text);


--
-- Name: uq_obia_active_user_function; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_obia_active_user_function ON public.organization_business_identity_assignment USING btree (organization_id, user_id, function_key) WHERE ((status = 'active'::text) AND (user_id IS NOT NULL));


--
-- Name: uq_organization_department_org_active_name; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_organization_department_org_active_name ON public.organization_department USING btree (organization_id, name) WHERE (status = 'active'::text);


--
-- Name: uq_organization_invitation_single_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_organization_invitation_single_pending ON public.organization_invitation USING btree (organization_id, target_user_id) WHERE (status = 'pending'::text);


--
-- Name: uq_organization_member_single_active_owner; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_organization_member_single_active_owner ON public.organization_member USING btree (organization_id) WHERE ((role = 'owner'::text) AND (status = 'active'::text));


--
-- Name: INDEX uq_organization_member_single_active_owner; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.uq_organization_member_single_active_owner IS '每 Organization 最多一条 active owner 成员行（C04 SOURCE OF TRUTH 唯一性）';


--
-- Name: uq_owner_activation_invite_single_live; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_owner_activation_invite_single_live ON public.owner_activation_invite USING btree (organization_id) WHERE (status = ANY (ARRAY['pending'::text, 'sms_failed'::text]));


--
-- Name: uq_owner_activation_invite_token_digest; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_owner_activation_invite_token_digest ON public.owner_activation_invite USING btree (token_digest);


--
-- Name: uq_push_token_active_token; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_push_token_active_token ON public.push_token USING btree (token) WHERE (status = 1);


--
-- Name: INDEX uq_push_token_active_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON INDEX public.uq_push_token_active_token IS '活动推送 token 全局唯一（迁移 00000142 / FULL-06，plan-full §7）：一个 token（FCM token / JPush RegistrationID）同一时刻只能绑定一个 user_id+device_id；token 换主人或换设备时由 push_token_repo:upsert 先按 token 断电再插入';


--
-- Name: uq_workspace_org_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_workspace_org_id ON public.workspace USING btree (organization_id, id);


--
-- Name: user_fts_gin_idex; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_fts_gin_idex ON public.fts_user USING gin (token);


--
-- Name: user_log_created_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_log_created_at_idx ON public.user_log USING btree (created_at DESC);


--
-- Name: channel channel_bump_version; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER channel_bump_version BEFORE UPDATE ON public.channel FOR EACH ROW EXECUTE FUNCTION public.channel_bump_version();


--
-- Name: msg_c2g imboy_for_msg_c2g; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER imboy_for_msg_c2g AFTER INSERT OR DELETE OR UPDATE OF msg_id ON public.msg_c2g FOR EACH ROW EXECUTE FUNCTION public.imboy_msg_c2g_fun();


--
-- Name: user_collect imboy_for_user_collect; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER imboy_for_user_collect AFTER INSERT OR DELETE OR UPDATE OF attach_file_hash256 ON public.user_collect FOR EACH ROW EXECUTE FUNCTION public.imboy_user_collect_fun();


--
-- Name: user_tag_relation imboy_for_user_tag_relation; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER imboy_for_user_tag_relation AFTER INSERT OR DELETE OR UPDATE OF scene, user_id, tag_id ON public.user_tag_relation FOR EACH ROW EXECUTE FUNCTION public.imboy_user_tag_relation_fun();


--
-- Name: channel_invitation tr_channel_invitation_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_channel_invitation_updated_at BEFORE UPDATE ON public.channel_invitation FOR EACH ROW EXECUTE FUNCTION public.fn_channel_subscribe_updated_at();


--
-- Name: channel_order tr_channel_order_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_channel_order_updated_at BEFORE UPDATE ON public.channel_order FOR EACH ROW EXECUTE FUNCTION public.fn_channel_subscribe_updated_at();


--
-- Name: channel_price tr_channel_price_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_channel_price_updated_at BEFORE UPDATE ON public.channel_price FOR EACH ROW EXECUTE FUNCTION public.fn_channel_subscribe_updated_at();


--
-- Name: moment_comment tr_moment_comment_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_moment_comment_updated_at BEFORE UPDATE ON public.moment_comment FOR EACH ROW EXECUTE FUNCTION public.fn_moment_touch_updated_at();


--
-- Name: moment_post tr_moment_post_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_moment_post_updated_at BEFORE UPDATE ON public.moment_post FOR EACH ROW EXECUTE FUNCTION public.fn_moment_touch_updated_at();


--
-- Name: moment_report tr_moment_report_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_moment_report_updated_at BEFORE UPDATE ON public.moment_report FOR EACH ROW EXECUTE FUNCTION public.fn_moment_touch_updated_at();


--
-- Name: report_ticket tr_report_ticket_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_report_ticket_updated_at BEFORE UPDATE ON public.report_ticket FOR EACH ROW EXECUTE FUNCTION public.fn_report_ticket_touch_updated_at();


--
-- Name: channel_reaction tr_update_channel_message_reaction_summary; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_update_channel_message_reaction_summary AFTER INSERT OR DELETE ON public.channel_reaction FOR EACH ROW EXECUTE FUNCTION public.fn_update_channel_message_reaction_summary();


--
-- Name: channel_message_view tr_update_channel_message_view_count; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER tr_update_channel_message_view_count AFTER INSERT ON public.channel_message_view FOR EACH ROW EXECUTE FUNCTION public.fn_update_channel_message_view_count();


--
-- Name: adm_role trg_adm_role_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_adm_role_updated_at BEFORE UPDATE ON public.adm_role FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: adm_user trg_adm_user_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_adm_user_updated_at BEFORE UPDATE ON public.adm_user FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: agent_grant_event trg_agent_grant_event_append_only; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_agent_grant_event_append_only BEFORE DELETE OR UPDATE ON public.agent_grant_event FOR EACH ROW EXECUTE FUNCTION public.fn_agent_grant_event_append_only();


--
-- Name: agent_run_event trg_agent_run_event_append_only; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_agent_run_event_append_only BEFORE DELETE OR UPDATE ON public.agent_run_event FOR EACH ROW EXECUTE FUNCTION public.fn_agent_run_event_append_only();


--
-- Name: announcement trg_announcement_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_announcement_updated_at BEFORE UPDATE ON public.announcement FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: app_ddl trg_app_ddl_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_app_ddl_updated_at BEFORE UPDATE ON public.app_ddl FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: app_version trg_app_version_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_app_version_updated_at BEFORE UPDATE ON public.app_version FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: attachment trg_attachment_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_attachment_updated_at BEFORE UPDATE ON public.attachment FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: channel trg_channel_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_channel_updated_at BEFORE UPDATE ON public.channel FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: class_enrollment trg_class_enrollment_org_consistency; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_class_enrollment_org_consistency AFTER INSERT OR UPDATE OF group_id, learner_id ON public.class_enrollment DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_class_enrollment_org_check();


--
-- Name: compliance_key trg_compliance_key_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_compliance_key_updated_at BEFORE UPDATE ON public.compliance_key FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: config trg_config_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_config_updated_at BEFORE UPDATE ON public.config FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: customer_service_event trg_customer_service_event_append_only; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_customer_service_event_append_only BEFORE DELETE OR UPDATE ON public.customer_service_event FOR EACH ROW EXECUTE FUNCTION public.fn_customer_service_event_append_only();


--
-- Name: e2ee_key_shares trg_e2ee_key_shares_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_e2ee_key_shares_updated_at BEFORE UPDATE ON public.e2ee_key_shares FOR EACH ROW EXECUTE FUNCTION public.update_e2ee_updated_at();


--
-- Name: enterprise_application trg_efapi_application_mime_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_efapi_application_mime_guard BEFORE INSERT OR UPDATE OF allowed_mime_types ON public.enterprise_application FOR EACH ROW EXECUTE FUNCTION public.fn_efapi_application_mime_guard();


--
-- Name: enterprise_attachment_retention trg_efapi_attachment_retention_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_efapi_attachment_retention_guard BEFORE DELETE OR UPDATE ON public.enterprise_attachment_retention FOR EACH ROW EXECUTE FUNCTION public.fn_efapi_attachment_retention_guard();


--
-- Name: enterprise_group_origin trg_efapi_group_origin_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_efapi_group_origin_guard BEFORE DELETE OR UPDATE ON public.enterprise_group_origin FOR EACH ROW EXECUTE FUNCTION public.fn_efapi_group_origin_guard();


--
-- Name: enterprise_message_origin trg_efapi_message_origin_no_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_efapi_message_origin_no_delete BEFORE DELETE ON public.enterprise_message_origin FOR EACH ROW EXECUTE FUNCTION public.fn_efapi_message_origin_no_delete();


--
-- Name: enterprise_application_usage trg_efapi_usage_no_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_efapi_usage_no_delete BEFORE DELETE ON public.enterprise_application_usage FOR EACH ROW EXECUTE FUNCTION public.fn_efapi_usage_no_delete();


--
-- Name: enterprise_application_grant trg_enterprise_application_grant_no_delete; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_application_grant_no_delete BEFORE DELETE ON public.enterprise_application_grant FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_application_grant_no_delete();


--
-- Name: enterprise_application trg_enterprise_application_redirect_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_application_redirect_guard BEFORE INSERT OR UPDATE OF allowed_redirect_uris ON public.enterprise_application FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_application_redirect_guard();


--
-- Name: enterprise_asset trg_enterprise_asset_purge_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_asset_purge_guard BEFORE DELETE ON public.enterprise_asset FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_asset_purge_guard();


--
-- Name: enterprise_asset trg_enterprise_asset_retention_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_asset_retention_guard BEFORE INSERT OR UPDATE OF retain_until, status, message_id ON public.enterprise_asset FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_asset_retention_guard();


--
-- Name: enterprise_audit_event trg_enterprise_audit_event_append_only; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_audit_event_append_only BEFORE DELETE OR UPDATE ON public.enterprise_audit_event FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_audit_event_append_only();


--
-- Name: enterprise_external_identity trg_enterprise_external_identity_member_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_external_identity_member_guard BEFORE INSERT OR UPDATE OF organization_id, application_id, user_id, status ON public.enterprise_external_identity FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_external_identity_member_guard();


--
-- Name: enterprise_message trg_enterprise_message_actor_required; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_message_actor_required BEFORE INSERT ON public.enterprise_message FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_message_actor_required();


--
-- Name: enterprise_message trg_enterprise_message_purge_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_message_purge_guard BEFORE DELETE ON public.enterprise_message FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_message_purge_guard();


--
-- Name: enterprise_message trg_enterprise_message_retention_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_message_retention_guard BEFORE UPDATE OF retain_until ON public.enterprise_message FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_message_retention_guard();


--
-- Name: enterprise_retention_hold trg_enterprise_retention_hold_append_only; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_retention_hold_append_only BEFORE DELETE OR UPDATE ON public.enterprise_retention_hold FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_retention_hold_append_only();


--
-- Name: enterprise_retention_policy trg_enterprise_retention_policy_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_retention_policy_guard BEFORE INSERT ON public.enterprise_retention_policy FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_retention_policy_guard();


--
-- Name: enterprise_retention_policy trg_enterprise_retention_policy_immutable; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_enterprise_retention_policy_immutable BEFORE DELETE OR UPDATE ON public.enterprise_retention_policy FOR EACH ROW EXECUTE FUNCTION public.fn_enterprise_retention_policy_immutable();


--
-- Name: bot_delivery trg_ewh_delivery_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_ewh_delivery_guard BEFORE INSERT OR UPDATE ON public.bot_delivery FOR EACH ROW EXECUTE FUNCTION public.fn_ewh_delivery_guard();


--
-- Name: feedback_reply trg_feedback_reply_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_feedback_reply_updated_at BEFORE UPDATE ON public.feedback_reply FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: feedback trg_feedback_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_feedback_updated_at BEFORE UPDATE ON public.feedback FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: channel trg_fts_channel_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_fts_channel_update AFTER INSERT OR UPDATE OF name, description ON public.channel FOR EACH ROW EXECUTE FUNCTION public.fts_channel_trigger();


--
-- Name: group trg_fts_group_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_fts_group_update AFTER INSERT OR UPDATE OF title, introduction ON public."group" FOR EACH ROW EXECUTE FUNCTION public.fts_group_trigger();


--
-- Name: geo_people_nearby trg_geo_people_nearby_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_geo_people_nearby_updated_at BEFORE UPDATE ON public.geo_people_nearby FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_album trg_group_album_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_album_updated_at BEFORE UPDATE ON public.group_album FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_file trg_group_file_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_file_updated_at BEFORE UPDATE ON public.group_file FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_member trg_group_member_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_member_updated_at BEFORE UPDATE ON public.group_member FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_member trg_group_member_ws_subset; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_group_member_ws_subset AFTER INSERT OR UPDATE OF status, user_id, group_id ON public.group_member DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_group_member_ws_subset_check();


--
-- Name: group_notice trg_group_notice_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_notice_updated_at BEFORE UPDATE ON public.group_notice FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_schedule trg_group_schedule_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_schedule_updated_at BEFORE UPDATE ON public.group_schedule FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_task trg_group_task_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_task_updated_at BEFORE UPDATE ON public.group_task FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group trg_group_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_updated_at BEFORE UPDATE ON public."group" FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_vote trg_group_vote_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_group_vote_updated_at BEFORE UPDATE ON public.group_vote FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: group_task_assignment trg_gta_learner_org_consistency; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_gta_learner_org_consistency AFTER INSERT OR UPDATE OF task_id, learner_id ON public.group_task_assignment DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_gta_learner_org_check();


--
-- Name: homework_submission trg_homework_submission_learner_consistency; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_homework_submission_learner_consistency AFTER INSERT OR UPDATE OF assignment_id, learner_id ON public.homework_submission DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_homework_submission_learner_check();


--
-- Name: homework_submission trg_homework_submission_withdraw_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_homework_submission_withdraw_guard AFTER UPDATE OF status ON public.homework_submission DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE FUNCTION public.fn_homework_submission_withdraw_guard();


--
-- Name: learner trg_learner_org_change_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_learner_org_change_guard AFTER UPDATE OF organization_id ON public.learner FOR EACH ROW EXECUTE FUNCTION public.fn_learner_org_change_guard();


--
-- Name: live_room trg_live_room_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_live_room_updated_at BEFORE UPDATE ON public.live_room FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: msg_c2c trg_msg_c2c_cross_timestamp_dedup; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_msg_c2c_cross_timestamp_dedup BEFORE INSERT ON public.msg_c2c FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_c2c');


--
-- Name: msg_c2g trg_msg_c2g_cross_timestamp_dedup; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_msg_c2g_cross_timestamp_dedup BEFORE INSERT ON public.msg_c2g FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_c2g');


--
-- Name: msg_c2s trg_msg_c2s_cross_timestamp_dedup; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_msg_c2s_cross_timestamp_dedup BEFORE INSERT ON public.msg_c2s FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_c2s');


--
-- Name: msg_s2c trg_msg_s2c_cross_timestamp_dedup; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_msg_s2c_cross_timestamp_dedup BEFORE INSERT ON public.msg_s2c FOR EACH ROW EXECUTE FUNCTION public.fn_message_cross_timestamp_dedup('msg_s2c');


--
-- Name: organization_business_identity trg_organization_business_identity_function_immutable; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_organization_business_identity_function_immutable BEFORE UPDATE OF function_key ON public.organization_business_identity FOR EACH ROW EXECUTE FUNCTION public.fn_organization_business_identity_function_immutable();


--
-- Name: organization_default_workspace trg_organization_default_workspace_active_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_organization_default_workspace_active_guard BEFORE INSERT OR UPDATE OF workspace_id, organization_id ON public.organization_default_workspace FOR EACH ROW EXECUTE FUNCTION public.fn_organization_default_workspace_active_guard();


--
-- Name: organization_department trg_organization_department_cycle_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_organization_department_cycle_guard BEFORE INSERT OR UPDATE OF parent_id ON public.organization_department FOR EACH ROW EXECUTE FUNCTION public.fn_organization_department_cycle_guard();


--
-- Name: organization_department_member trg_organization_department_member_active_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_organization_department_member_active_guard BEFORE INSERT OR UPDATE OF department_id, organization_id, user_id ON public.organization_department_member FOR EACH ROW EXECUTE FUNCTION public.fn_organization_department_member_active_guard();


--
-- Name: organization_member trg_organization_member_offboarding_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_organization_member_offboarding_guard BEFORE DELETE OR UPDATE OF status ON public.organization_member FOR EACH ROW EXECUTE FUNCTION public.fn_organization_member_offboarding_guard();


--
-- Name: organization_member trg_organization_member_owner_invariant; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_organization_member_owner_invariant AFTER INSERT OR DELETE OR UPDATE OF role, status ON public.organization_member DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_organization_member_owner_invariant();


--
-- Name: TRIGGER trg_organization_member_owner_invariant ON organization_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER trg_organization_member_owner_invariant ON public.organization_member IS '提交时校验所属 organization 恰好一个 active Human owner 且 = owner_id（C04 成员侧防线）';


--
-- Name: organization trg_organization_owner_invariant; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_organization_owner_invariant AFTER INSERT OR UPDATE OF owner_id ON public.organization DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_organization_owner_invariant();


--
-- Name: TRIGGER trg_organization_owner_invariant ON organization; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER trg_organization_owner_invariant ON public.organization IS '提交时校验恰好一个 active Human owner 且 = owner_id（C04 SOURCE OF TRUTH 最终防线）';


--
-- Name: organization trg_organization_owner_member_sync; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_organization_owner_member_sync AFTER INSERT OR UPDATE OF owner_id ON public.organization FOR EACH ROW EXECUTE FUNCTION public.fn_organization_owner_member_sync();


--
-- Name: organization_member trg_organization_primary_owner_member_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_organization_primary_owner_member_guard AFTER DELETE OR UPDATE OF role, status ON public.organization_member DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_organization_primary_owner_member_guard();


--
-- Name: TRIGGER trg_organization_primary_owner_member_guard ON organization_member; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER trg_organization_primary_owner_member_guard ON public.organization_member IS '提交时拒绝「organization.owner_id 仍指向该行」的降级/移除/暂停（C04：先 transfer 再改成员行）';


--
-- Name: project trg_project_links_shape; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_project_links_shape BEFORE INSERT OR UPDATE OF links ON public.project FOR EACH ROW EXECUTE FUNCTION public.fn_project_links_shape_check();


--
-- Name: project_member trg_project_member_ws_active; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_project_member_ws_active AFTER INSERT OR UPDATE OF status, user_id, project_id, workspace_id ON public.project_member DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_project_member_ws_active_check();


--
-- Name: project trg_project_owner_membership_active; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_project_owner_membership_active AFTER INSERT OR UPDATE OF owner_id, workspace_id ON public.project DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_project_owner_membership_active();


--
-- Name: push_token trg_push_token_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_push_token_updated_at BEFORE UPDATE ON public.push_token FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: report_ticket trg_report_ticket_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_report_ticket_updated_at BEFORE UPDATE ON public.report_ticket FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: review_asset trg_review_asset_image_cap; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_review_asset_image_cap AFTER INSERT OR UPDATE OF review_id, kind ON public.review_asset DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE FUNCTION public.fn_review_asset_image_cap();


--
-- Name: teacher_review trg_teacher_review_publish_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_teacher_review_publish_guard AFTER INSERT OR UPDATE OF status ON public.teacher_review DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE FUNCTION public.fn_teacher_review_publish_guard();


--
-- Name: user_collect trg_user_collect_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_collect_updated_at BEFORE UPDATE ON public.user_collect FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: user_friend trg_user_friend_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_friend_updated_at BEFORE UPDATE ON public.user_friend FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: user trg_user_fts; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_fts AFTER INSERT OR DELETE OR UPDATE OF nickname, account, mobile, sign, region ON public."user" FOR EACH ROW EXECUTE FUNCTION public.sync_fts_user();


--
-- Name: user_group trg_user_group_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_group_updated_at BEFORE UPDATE ON public.user_group FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: user_tag trg_user_tag_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_tag_updated_at BEFORE UPDATE ON public.user_tag FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: wallet trg_wallet_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_wallet_updated_at BEFORE UPDATE ON public.wallet FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();


--
-- Name: workspace_member trg_workspace_member_remove_guard; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_workspace_member_remove_guard AFTER DELETE OR UPDATE OF status ON public.workspace_member DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_workspace_member_remove_guard();


--
-- Name: workspace_member trg_workspace_member_remove_guard_pm; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER trg_workspace_member_remove_guard_pm AFTER DELETE OR UPDATE OF status ON public.workspace_member DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.fn_workspace_member_remove_guard_pm();


--
-- Name: workspace workspace_bump_version; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER workspace_bump_version BEFORE UPDATE ON public.workspace FOR EACH ROW EXECUTE FUNCTION public.workspace_bump_version();


--
-- Name: agent_hub_audit agent_hub_audit_parent_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_hub_audit
    ADD CONSTRAINT agent_hub_audit_parent_fk FOREIGN KEY (correlation_id, parent_entity_id) REFERENCES public.agent_hub_audit(correlation_id, entity_id);


--
-- Name: agent_task_decision agent_task_decision_task_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_task_decision
    ADD CONSTRAINT agent_task_decision_task_id_fkey FOREIGN KEY (task_id) REFERENCES public.agent_task(id) ON DELETE CASCADE;


--
-- Name: agent_task_event agent_task_event_task_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_task_event
    ADD CONSTRAINT agent_task_event_task_id_fkey FOREIGN KEY (task_id) REFERENCES public.agent_task(id) ON DELETE CASCADE;


--
-- Name: ai_agent_role_version ai_agent_role_version_role_code_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ai_agent_role_version
    ADD CONSTRAINT ai_agent_role_version_role_code_fkey FOREIGN KEY (role_code) REFERENCES public.ai_agent_role(code) ON DELETE RESTRICT;


--
-- Name: bot_delivery_attempt bot_delivery_attempt_delivery_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_delivery_attempt
    ADD CONSTRAINT bot_delivery_attempt_delivery_id_fkey FOREIGN KEY (delivery_id) REFERENCES public.bot_delivery(delivery_id) ON DELETE CASCADE;


--
-- Name: enterprise_asset_delete_queue enterprise_asset_delete_queue_organization_id_workspace_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset_delete_queue
    ADD CONSTRAINT enterprise_asset_delete_queue_organization_id_workspace_id_fkey FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: agent_effect fk_ae_run; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_effect
    ADD CONSTRAINT fk_ae_run FOREIGN KEY (run_id) REFERENCES public.agent_run(id) ON DELETE RESTRICT;


--
-- Name: agent_grant fk_ag_agent; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT fk_ag_agent FOREIGN KEY (agent_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: agent_grant fk_ag_delegator; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT fk_ag_delegator FOREIGN KEY (delegator_user_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: agent_grant fk_ag_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT fk_ag_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: agent_grant fk_ag_revoked_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant
    ADD CONSTRAINT fk_ag_revoked_by FOREIGN KEY (revoked_by_user_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: agent_grant_capability fk_agc_grant; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_capability
    ADD CONSTRAINT fk_agc_grant FOREIGN KEY (grant_id) REFERENCES public.agent_grant(id) ON DELETE CASCADE;


--
-- Name: agent_grant_event fk_age_actor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_event
    ADD CONSTRAINT fk_age_actor FOREIGN KEY (actor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: agent_grant_event fk_age_grant; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_event
    ADD CONSTRAINT fk_age_grant FOREIGN KEY (grant_id) REFERENCES public.agent_grant(id) ON DELETE RESTRICT;


--
-- Name: agent_payment_compensation fk_agent_payment_compensation_mandate; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_payment_compensation
    ADD CONSTRAINT fk_agent_payment_compensation_mandate FOREIGN KEY (mandate_id) REFERENCES public.agent_payment_mandate(id);


--
-- Name: agent_grant_workspace fk_agw_grant; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_workspace
    ADD CONSTRAINT fk_agw_grant FOREIGN KEY (organization_id, grant_id) REFERENCES public.agent_grant(organization_id, id) ON DELETE CASCADE;


--
-- Name: agent_grant_workspace fk_agw_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_grant_workspace
    ADD CONSTRAINT fk_agw_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: ai_agent fk_ai_agent_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ai_agent
    ADD CONSTRAINT fk_ai_agent_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: agent_run fk_ar_agent; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT fk_ar_agent FOREIGN KEY (agent_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: agent_run fk_ar_delegating_principal; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT fk_ar_delegating_principal FOREIGN KEY (delegating_principal_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: agent_run fk_ar_grant; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT fk_ar_grant FOREIGN KEY (grant_id) REFERENCES public.agent_grant(id) ON DELETE RESTRICT;


--
-- Name: agent_run fk_ar_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT fk_ar_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: agent_run fk_ar_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run
    ADD CONSTRAINT fk_ar_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: agent_run_event fk_are_run; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.agent_run_event
    ADD CONSTRAINT fk_are_run FOREIGN KEY (run_id) REFERENCES public.agent_run(id) ON DELETE RESTRICT;


--
-- Name: bot_oauth_grant fk_bot_grant_bot; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_oauth_grant
    ADD CONSTRAINT fk_bot_grant_bot FOREIGN KEY (bot_id) REFERENCES public.bot(user_id) ON DELETE CASCADE;


--
-- Name: bot_oauth_grant fk_bot_grant_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_oauth_grant
    ADD CONSTRAINT fk_bot_grant_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: bot fk_bot_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot
    ADD CONSTRAINT fk_bot_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: channel_admin fk_channel_admin_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT fk_channel_admin_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_admin fk_channel_admin_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_admin
    ADD CONSTRAINT fk_channel_admin_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: channel_invitation fk_channel_invitation_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_invitation
    ADD CONSTRAINT fk_channel_invitation_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_message fk_channel_message_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message
    ADD CONSTRAINT fk_channel_message_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_message_view fk_channel_message_view_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT fk_channel_message_view_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_message_view fk_channel_message_view_message; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT fk_channel_message_view_message FOREIGN KEY (message_id) REFERENCES public.channel_message(id) ON DELETE CASCADE;


--
-- Name: channel_message_view fk_channel_message_view_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_message_view
    ADD CONSTRAINT fk_channel_message_view_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: channel_order fk_channel_order_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_order
    ADD CONSTRAINT fk_channel_order_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_price fk_channel_price_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_price
    ADD CONSTRAINT fk_channel_price_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_reaction fk_channel_reaction_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT fk_channel_reaction_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_reaction fk_channel_reaction_message; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT fk_channel_reaction_message FOREIGN KEY (message_id) REFERENCES public.channel_message(id) ON DELETE CASCADE;


--
-- Name: channel_reaction fk_channel_reaction_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_reaction
    ADD CONSTRAINT fk_channel_reaction_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: channel_stats_daily fk_channel_stats_daily_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_stats_daily
    ADD CONSTRAINT fk_channel_stats_daily_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_subscription fk_channel_subscription_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT fk_channel_subscription_channel FOREIGN KEY (channel_id) REFERENCES public.channel(id) ON DELETE CASCADE;


--
-- Name: channel_subscription fk_channel_subscription_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel_subscription
    ADD CONSTRAINT fk_channel_subscription_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: channel fk_channel_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channel
    ADD CONSTRAINT fk_channel_workspace FOREIGN KEY (workspace_id) REFERENCES public.workspace(id) ON DELETE CASCADE;


--
-- Name: class_enrollment fk_class_enrollment_group; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_enrollment
    ADD CONSTRAINT fk_class_enrollment_group FOREIGN KEY (group_id) REFERENCES public."group"(id) ON DELETE CASCADE;


--
-- Name: class_enrollment fk_class_enrollment_learner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_enrollment
    ADD CONSTRAINT fk_class_enrollment_learner FOREIGN KEY (learner_id) REFERENCES public.learner(id) ON DELETE CASCADE;


--
-- Name: class_profile fk_class_profile_group; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_profile
    ADD CONSTRAINT fk_class_profile_group FOREIGN KEY (group_id) REFERENCES public."group"(id) ON DELETE CASCADE;


--
-- Name: class_staff fk_class_staff_group; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_staff
    ADD CONSTRAINT fk_class_staff_group FOREIGN KEY (group_id) REFERENCES public."group"(id) ON DELETE CASCADE;


--
-- Name: class_staff fk_class_staff_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_staff
    ADD CONSTRAINT fk_class_staff_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: calligraphy_review_draft fk_crd_submission; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.calligraphy_review_draft
    ADD CONSTRAINT fk_crd_submission FOREIGN KEY (submission_id) REFERENCES public.homework_submission(id) ON DELETE CASCADE;


--
-- Name: customer_service_event fk_cse_actor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT fk_cse_actor FOREIGN KEY (actor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_event fk_cse_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT fk_cse_identity FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_event fk_cse_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT fk_cse_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: customer_service_event fk_cse_session; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT fk_cse_session FOREIGN KEY (organization_id, session_id) REFERENCES public.customer_service_session(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_event fk_cse_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_event
    ADD CONSTRAINT fk_cse_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_read_cursor fk_csrc_seat; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_read_cursor
    ADD CONSTRAINT fk_csrc_seat FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.customer_service_seat(organization_id, business_identity_id) ON DELETE RESTRICT;


--
-- Name: customer_service_read_cursor fk_csrc_session; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_read_cursor
    ADD CONSTRAINT fk_csrc_session FOREIGN KEY (organization_id, session_id) REFERENCES public.customer_service_session(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_seat fk_css_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat
    ADD CONSTRAINT fk_css_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_seat fk_css_identity_function; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat
    ADD CONSTRAINT fk_css_identity_function FOREIGN KEY (organization_id, business_identity_id, function_key) REFERENCES public.organization_business_identity(organization_id, id, function_key) ON DELETE RESTRICT;


--
-- Name: customer_service_seat_console fk_cssc_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_console
    ADD CONSTRAINT fk_cssc_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_seat_console fk_cssc_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_console
    ADD CONSTRAINT fk_cssc_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: customer_service_seat_console fk_cssc_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_console
    ADD CONSTRAINT fk_cssc_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_shop_key fk_cssk_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_shop_key
    ADD CONSTRAINT fk_cssk_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_shop_key fk_cssk_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_shop_key
    ADD CONSTRAINT fk_cssk_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: customer_service_seat_presence fk_cssp_seat; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_seat_presence
    ADD CONSTRAINT fk_cssp_seat FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.customer_service_seat(organization_id, business_identity_id) ON DELETE CASCADE;


--
-- Name: customer_service_session fk_csss_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT fk_csss_contact FOREIGN KEY (organization_id, contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_session fk_csss_conversation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT fk_csss_conversation FOREIGN KEY (organization_id, conversation_id, contact_id) REFERENCES public.enterprise_conversation(organization_id, id, contact_id) ON DELETE RESTRICT;


--
-- Name: customer_service_session fk_csss_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT fk_csss_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_session fk_csss_seat; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT fk_csss_seat FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.customer_service_seat(organization_id, business_identity_id) ON DELETE RESTRICT;


--
-- Name: customer_service_session fk_csss_visit_token; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT fk_csss_visit_token FOREIGN KEY (organization_id, visit_token_id) REFERENCES public.customer_service_visit_token(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_session fk_csss_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_session
    ADD CONSTRAINT fk_csss_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_visit_token fk_csvt_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT fk_csvt_contact FOREIGN KEY (organization_id, contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_visit_token fk_csvt_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT fk_csvt_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_visit_token fk_csvt_created_by_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT fk_csvt_created_by_identity FOREIGN KEY (organization_id, created_by_business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_visit_token fk_csvt_widget_installation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_visit_token
    ADD CONSTRAINT fk_csvt_widget_installation FOREIGN KEY (organization_id, widget_installation_id) REFERENCES public.customer_service_widget_installation(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_widget_installation fk_cswi_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_installation
    ADD CONSTRAINT fk_cswi_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: customer_service_widget_installation fk_cswi_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_installation
    ADD CONSTRAINT fk_cswi_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: customer_service_widget_identity_key fk_cswk_installation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_identity_key
    ADD CONSTRAINT fk_cswk_installation FOREIGN KEY (organization_id, installation_id) REFERENCES public.customer_service_widget_installation(organization_id, id) ON DELETE RESTRICT;


--
-- Name: customer_service_widget_nonce fk_cswn_installation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.customer_service_widget_nonce
    ADD CONSTRAINT fk_cswn_installation FOREIGN KEY (organization_id, installation_id) REFERENCES public.customer_service_widget_installation(organization_id, id) ON DELETE RESTRICT;


--
-- Name: e2ee_group_session_member fk_e2ee_group_session_member_session; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.e2ee_group_session_member
    ADD CONSTRAINT fk_e2ee_group_session_member_session FOREIGN KEY (group_id, session_id) REFERENCES public.e2ee_group_session_attestation(group_id, session_id);


--
-- Name: enterprise_application fk_ea_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application
    ADD CONSTRAINT fk_ea_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_application fk_ea_principal_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application
    ADD CONSTRAINT fk_ea_principal_user FOREIGN KEY (principal_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_application_credential fk_eac_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_credential
    ADD CONSTRAINT fk_eac_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_application_credential fk_eac_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_credential
    ADD CONSTRAINT fk_eac_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_audit_event fk_eae_actor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_audit_event
    ADD CONSTRAINT fk_eae_actor FOREIGN KEY (actor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_audit_event fk_eae_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_audit_event
    ADD CONSTRAINT fk_eae_identity FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_audit_event fk_eae_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_audit_event
    ADD CONSTRAINT fk_eae_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_application_grant fk_eag_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant
    ADD CONSTRAINT fk_eag_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_application_grant fk_eag_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant
    ADD CONSTRAINT fk_eag_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_application_grant fk_eag_revoked_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant
    ADD CONSTRAINT fk_eag_revoked_by FOREIGN KEY (revoked_by_user_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: enterprise_application_grant_scope fk_eags_grant; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant_scope
    ADD CONSTRAINT fk_eags_grant FOREIGN KEY (grant_id) REFERENCES public.enterprise_application_grant(id) ON DELETE CASCADE;


--
-- Name: enterprise_application_grant_workspace fk_eagw_grant; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant_workspace
    ADD CONSTRAINT fk_eagw_grant FOREIGN KEY (organization_id, grant_id, workspace_scope_kind) REFERENCES public.enterprise_application_grant(organization_id, id, workspace_scope_kind) ON DELETE CASCADE;


--
-- Name: enterprise_application_grant_workspace fk_eagw_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_grant_workspace
    ADD CONSTRAINT fk_eagw_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_attachment_retention fk_ear_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_attachment_retention
    ADD CONSTRAINT fk_ear_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_attachment_retention fk_ear_attachment; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_attachment_retention
    ADD CONSTRAINT fk_ear_attachment FOREIGN KEY (attachment_id) REFERENCES public.attachment(id) ON DELETE RESTRICT;


--
-- Name: enterprise_application_usage fk_eau_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_application_usage
    ADD CONSTRAINT fk_eau_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_conversation fk_ec_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_conversation
    ADD CONSTRAINT fk_ec_contact FOREIGN KEY (organization_id, contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_conversation fk_ec_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_conversation
    ADD CONSTRAINT fk_ec_identity FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_conversation fk_ec_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_conversation
    ADD CONSTRAINT fk_ec_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_contact_assignment fk_eca_assigned_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_assignment
    ADD CONSTRAINT fk_eca_assigned_by FOREIGN KEY (assigned_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_contact_assignment fk_eca_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_assignment
    ADD CONSTRAINT fk_eca_contact FOREIGN KEY (organization_id, contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_contact_assignment fk_eca_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_assignment
    ADD CONSTRAINT fk_eca_identity FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_contact_identity fk_eci_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact_identity
    ADD CONSTRAINT fk_eci_contact FOREIGN KEY (organization_id, contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_external_identity fk_eei_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT fk_eei_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_external_identity fk_eei_member; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT fk_eei_member FOREIGN KEY (organization_id, user_id) REFERENCES public.organization_member(organization_id, user_id) ON DELETE RESTRICT;


--
-- Name: enterprise_external_identity fk_eei_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_external_identity
    ADD CONSTRAINT fk_eei_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_group_origin fk_ego_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_group_origin
    ADD CONSTRAINT fk_ego_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_group_origin fk_ego_group; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_group_origin
    ADD CONSTRAINT fk_ego_group FOREIGN KEY (group_id) REFERENCES public."group"(id) ON DELETE RESTRICT;


--
-- Name: enterprise_group_origin fk_ego_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_group_origin
    ADD CONSTRAINT fk_ego_workspace FOREIGN KEY (workspace_id) REFERENCES public.workspace(id) ON DELETE RESTRICT;


--
-- Name: enterprise_internal_idempotency fk_eii_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_internal_idempotency
    ADD CONSTRAINT fk_eii_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_internal_idempotency fk_eii_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_internal_idempotency
    ADD CONSTRAINT fk_eii_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_message fk_em_actor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT fk_em_actor FOREIGN KEY (actor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_message fk_em_conversation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT fk_em_conversation FOREIGN KEY (organization_id, workspace_id, conversation_id) REFERENCES public.enterprise_conversation(organization_id, workspace_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_message fk_em_sender_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT fk_em_sender_contact FOREIGN KEY (organization_id, sender_contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_message fk_em_sender_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message
    ADD CONSTRAINT fk_em_sender_identity FOREIGN KEY (organization_id, sender_business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_message_delivery fk_emd_message; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message_delivery
    ADD CONSTRAINT fk_emd_message FOREIGN KEY (organization_id, workspace_id, message_id) REFERENCES public.enterprise_message(organization_id, workspace_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_message_origin fk_emo_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_message_origin
    ADD CONSTRAINT fk_emo_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_asset fk_enterprise_asset_conversation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT fk_enterprise_asset_conversation FOREIGN KEY (organization_id, workspace_id, conversation_id) REFERENCES public.enterprise_conversation(organization_id, workspace_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_asset fk_enterprise_asset_message; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT fk_enterprise_asset_message FOREIGN KEY (organization_id, workspace_id, message_id) REFERENCES public.enterprise_message(organization_id, workspace_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_asset fk_enterprise_asset_uploaded_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT fk_enterprise_asset_uploaded_by FOREIGN KEY (uploaded_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_asset fk_enterprise_asset_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_asset
    ADD CONSTRAINT fk_enterprise_asset_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_contact fk_enterprise_contact_created_by_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact
    ADD CONSTRAINT fk_enterprise_contact_created_by_identity FOREIGN KEY (organization_id, created_by_business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_contact fk_enterprise_contact_imboy_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact
    ADD CONSTRAINT fk_enterprise_contact_imboy_user FOREIGN KEY (imboy_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_contact fk_enterprise_contact_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_contact
    ADD CONSTRAINT fk_enterprise_contact_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_note fk_enterprise_note_actor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_note
    ADD CONSTRAINT fk_enterprise_note_actor FOREIGN KEY (actor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_note fk_enterprise_note_contact; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_note
    ADD CONSTRAINT fk_enterprise_note_contact FOREIGN KEY (organization_id, contact_id) REFERENCES public.enterprise_contact(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_note fk_enterprise_note_identity; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_note
    ADD CONSTRAINT fk_enterprise_note_identity FOREIGN KEY (organization_id, business_identity_id) REFERENCES public.organization_business_identity(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_offboarding_case fk_eoc_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_case
    ADD CONSTRAINT fk_eoc_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_offboarding_case fk_eoc_leaver; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_case
    ADD CONSTRAINT fk_eoc_leaver FOREIGN KEY (leaver_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_offboarding_case fk_eoc_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_case
    ADD CONSTRAINT fk_eoc_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_offboarding_case fk_eoc_successor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_case
    ADD CONSTRAINT fk_eoc_successor FOREIGN KEY (successor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_offboarding_item fk_eoi_case; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_item
    ADD CONSTRAINT fk_eoi_case FOREIGN KEY (organization_id, case_id) REFERENCES public.enterprise_offboarding_case(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_offboarding_item fk_eoi_from_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_item
    ADD CONSTRAINT fk_eoi_from_user FOREIGN KEY (from_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_offboarding_item fk_eoi_identity_function; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_item
    ADD CONSTRAINT fk_eoi_identity_function FOREIGN KEY (organization_id, business_identity_id, function_key) REFERENCES public.organization_business_identity(organization_id, id, function_key) ON DELETE RESTRICT;


--
-- Name: enterprise_offboarding_item fk_eoi_to_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_offboarding_item
    ADD CONSTRAINT fk_eoi_to_user FOREIGN KEY (to_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_oa_sso_code fk_eosc_application; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_oa_sso_code
    ADD CONSTRAINT fk_eosc_application FOREIGN KEY (organization_id, application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_oa_sso_code fk_eosc_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_oa_sso_code
    ADD CONSTRAINT fk_eosc_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: enterprise_oa_sso_code fk_eosc_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_oa_sso_code
    ADD CONSTRAINT fk_eosc_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: enterprise_retention_hold fk_erh_active_message; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_hold
    ADD CONSTRAINT fk_erh_active_message FOREIGN KEY (organization_id, workspace_id, active_scope_message_id) REFERENCES public.enterprise_message(organization_id, workspace_id, id) ON DELETE RESTRICT;


--
-- Name: CONSTRAINT fk_erh_active_message ON enterprise_retention_hold; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT fk_erh_active_message ON public.enterprise_retention_hold IS 'active-only 复合引用：只在 hold 生效中（released_at IS NULL）时把 scope_message_id 绑定到 enterprise_message，提供 (a) 插入期引用完整性 (b) KEY SHARE 行锁 + RI 最新快照复核的并发序列化；release 后派生列为 NULL，引用自动失效（released hold 不再永久阻断 bounded purge）';


--
-- Name: enterprise_retention_hold fk_erh_actor; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_hold
    ADD CONSTRAINT fk_erh_actor FOREIGN KEY (actor_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_retention_hold fk_erh_conversation; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_hold
    ADD CONSTRAINT fk_erh_conversation FOREIGN KEY (organization_id, workspace_id, scope_conversation_id) REFERENCES public.enterprise_conversation(organization_id, workspace_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_retention_hold fk_erh_released_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_hold
    ADD CONSTRAINT fk_erh_released_by FOREIGN KEY (released_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_retention_hold fk_erh_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_hold
    ADD CONSTRAINT fk_erh_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: enterprise_retention_policy fk_erp_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_policy
    ADD CONSTRAINT fk_erp_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: enterprise_retention_policy fk_erp_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.enterprise_retention_policy
    ADD CONSTRAINT fk_erp_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: bot_delivery fk_ewh_delivery_owner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.bot_delivery
    ADD CONSTRAINT fk_ewh_delivery_owner FOREIGN KEY (ewh_owner_organization_id, ewh_owner_application_id) REFERENCES public.enterprise_application(organization_id, id) ON DELETE RESTRICT;


--
-- Name: feedback_reply fk_feedback_reply_feedback; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feedback_reply
    ADD CONSTRAINT fk_feedback_reply_feedback FOREIGN KEY (feedback_id) REFERENCES public.feedback(id) ON DELETE CASCADE;


--
-- Name: group fk_group_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public."group"
    ADD CONSTRAINT fk_group_workspace FOREIGN KEY (workspace_id) REFERENCES public.workspace(id) ON DELETE CASCADE;


--
-- Name: group_task_assignment fk_gta_learner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_task_assignment
    ADD CONSTRAINT fk_gta_learner FOREIGN KEY (learner_id) REFERENCES public.learner(id) ON DELETE RESTRICT;


--
-- Name: group_task_assignment fk_gta_submitted_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.group_task_assignment
    ADD CONSTRAINT fk_gta_submitted_by FOREIGN KEY (submitted_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: guardian_learner fk_guardian_learner_guardian; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.guardian_learner
    ADD CONSTRAINT fk_guardian_learner_guardian FOREIGN KEY (guardian_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: guardian_learner fk_guardian_learner_learner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.guardian_learner
    ADD CONSTRAINT fk_guardian_learner_learner FOREIGN KEY (learner_id) REFERENCES public.learner(id) ON DELETE CASCADE;


--
-- Name: homework_submission fk_hs_assignment; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.homework_submission
    ADD CONSTRAINT fk_hs_assignment FOREIGN KEY (assignment_id) REFERENCES public.group_task_assignment(id) ON DELETE CASCADE;


--
-- Name: homework_submission fk_hs_learner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.homework_submission
    ADD CONSTRAINT fk_hs_learner FOREIGN KEY (learner_id) REFERENCES public.learner(id) ON DELETE RESTRICT;


--
-- Name: homework_submission fk_hs_submitted_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.homework_submission
    ADD CONSTRAINT fk_hs_submitted_by FOREIGN KEY (submitted_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: learner fk_learner_bound_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.learner
    ADD CONSTRAINT fk_learner_bound_by FOREIGN KEY (account_bound_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: learner fk_learner_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.learner
    ADD CONSTRAINT fk_learner_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: learner fk_learner_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.learner
    ADD CONSTRAINT fk_learner_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: moderation_action fk_moderation_action_case; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moderation_action
    ADD CONSTRAINT fk_moderation_action_case FOREIGN KEY (case_id) REFERENCES public.report_ticket(id) ON DELETE CASCADE;


--
-- Name: moment_comment fk_moment_comment_post; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT fk_moment_comment_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--
-- Name: moment_comment fk_moment_comment_reply_to_uid; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT fk_moment_comment_reply_to_uid FOREIGN KEY (reply_to_uid) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: moment_comment fk_moment_comment_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_comment
    ADD CONSTRAINT fk_moment_comment_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moment_like fk_moment_like_post; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT fk_moment_like_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--
-- Name: moment_like fk_moment_like_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_like
    ADD CONSTRAINT fk_moment_like_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moment_post_acl fk_moment_post_acl_post; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT fk_moment_post_acl_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--
-- Name: moment_post_acl fk_moment_post_acl_uid; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_post_acl
    ADD CONSTRAINT fk_moment_post_acl_uid FOREIGN KEY (uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moment_post fk_moment_post_author; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_post
    ADD CONSTRAINT fk_moment_post_author FOREIGN KEY (author_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moment_report fk_moment_report_post; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT fk_moment_report_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--
-- Name: moment_report fk_moment_report_reporter; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_report
    ADD CONSTRAINT fk_moment_report_reporter FOREIGN KEY (reporter_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moment_timeline fk_moment_timeline_author; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT fk_moment_timeline_author FOREIGN KEY (author_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moment_timeline fk_moment_timeline_post; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT fk_moment_timeline_post FOREIGN KEY (post_id) REFERENCES public.moment_post(id) ON DELETE CASCADE;


--
-- Name: moment_timeline fk_moment_timeline_recipient; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moment_timeline
    ADD CONSTRAINT fk_moment_timeline_recipient FOREIGN KEY (recipient_uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moya_invite_code fk_moya_invite_code_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moya_invite_code
    ADD CONSTRAINT fk_moya_invite_code_created_by FOREIGN KEY (created_by) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: moya_invite_code fk_moya_invite_code_group; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moya_invite_code
    ADD CONSTRAINT fk_moya_invite_code_group FOREIGN KEY (group_id) REFERENCES public."group"(id) ON DELETE CASCADE;


--
-- Name: moya_subscribe_grant fk_moya_subscribe_grant_uid; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.moya_subscribe_grant
    ADD CONSTRAINT fk_moya_subscribe_grant_uid FOREIGN KEY (uid) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: organization_business_identity fk_obi_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity
    ADD CONSTRAINT fk_obi_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_business_identity fk_obi_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity
    ADD CONSTRAINT fk_obi_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: organization_business_identity_assignment fk_obia_assigned_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity_assignment
    ADD CONSTRAINT fk_obia_assigned_by FOREIGN KEY (assigned_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_business_identity_assignment fk_obia_identity_function; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity_assignment
    ADD CONSTRAINT fk_obia_identity_function FOREIGN KEY (organization_id, business_identity_id, function_key) REFERENCES public.organization_business_identity(organization_id, id, function_key) ON DELETE RESTRICT;


--
-- Name: organization_business_identity_assignment fk_obia_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_business_identity_assignment
    ADD CONSTRAINT fk_obia_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_department_member fk_odm_added_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department_member
    ADD CONSTRAINT fk_odm_added_by FOREIGN KEY (added_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_department_member fk_odm_department; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department_member
    ADD CONSTRAINT fk_odm_department FOREIGN KEY (department_id) REFERENCES public.organization_department(id) ON DELETE CASCADE;


--
-- Name: organization_department_member fk_odm_organization_member; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department_member
    ADD CONSTRAINT fk_odm_organization_member FOREIGN KEY (organization_id, user_id) REFERENCES public.organization_member(organization_id, user_id) ON DELETE CASCADE;


--
-- Name: organization_default_workspace fk_odw_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_default_workspace
    ADD CONSTRAINT fk_odw_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: organization_default_workspace fk_odw_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_default_workspace
    ADD CONSTRAINT fk_odw_workspace FOREIGN KEY (organization_id, workspace_id) REFERENCES public.workspace(organization_id, id) ON DELETE RESTRICT;


--
-- Name: organization_department fk_organization_department_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department
    ADD CONSTRAINT fk_organization_department_created_by FOREIGN KEY (created_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_department fk_organization_department_org; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department
    ADD CONSTRAINT fk_organization_department_org FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: organization_department fk_organization_department_parent; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department
    ADD CONSTRAINT fk_organization_department_parent FOREIGN KEY (parent_id) REFERENCES public.organization_department(id) ON DELETE RESTRICT;


--
-- Name: organization_department fk_organization_department_updated_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_department
    ADD CONSTRAINT fk_organization_department_updated_by FOREIGN KEY (updated_by_user_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_invitation fk_organization_invitation_invited_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invitation
    ADD CONSTRAINT fk_organization_invitation_invited_by FOREIGN KEY (invited_by) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: organization_invitation fk_organization_invitation_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invitation
    ADD CONSTRAINT fk_organization_invitation_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE CASCADE;


--
-- Name: organization_invitation fk_organization_invitation_target_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invitation
    ADD CONSTRAINT fk_organization_invitation_target_user FOREIGN KEY (target_user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: organization_invite_code fk_organization_invite_code_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invite_code
    ADD CONSTRAINT fk_organization_invite_code_created_by FOREIGN KEY (created_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_invite_code fk_organization_invite_code_org; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_invite_code
    ADD CONSTRAINT fk_organization_invite_code_org FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE CASCADE;


--
-- Name: organization_member fk_organization_member_invited_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_member
    ADD CONSTRAINT fk_organization_member_invited_by FOREIGN KEY (invited_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: organization_member fk_organization_member_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_member
    ADD CONSTRAINT fk_organization_member_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE CASCADE;


--
-- Name: organization_member fk_organization_member_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization_member
    ADD CONSTRAINT fk_organization_member_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: organization fk_organization_owner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organization
    ADD CONSTRAINT fk_organization_owner FOREIGN KEY (owner_id) REFERENCES public."user"(id) ON DELETE RESTRICT;


--
-- Name: CONSTRAINT fk_organization_owner ON organization; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON CONSTRAINT fk_organization_owner ON public.organization IS 'Owner 用户引用；ON DELETE RESTRICT：未完成 owner transfer 前删除 owner 用户被稳定拒绝（C04/C17）';


--
-- Name: owner_activation_invite fk_owner_activation_invite_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.owner_activation_invite
    ADD CONSTRAINT fk_owner_activation_invite_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: owner_activation_invite fk_owner_activation_invite_owner_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.owner_activation_invite
    ADD CONSTRAINT fk_owner_activation_invite_owner_user FOREIGN KEY (owner_user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: project_channel_rel fk_project_channel_rel_channel; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_channel_rel
    ADD CONSTRAINT fk_project_channel_rel_channel FOREIGN KEY (workspace_id, channel_id) REFERENCES public.channel(workspace_id, id) ON DELETE CASCADE;


--
-- Name: project_channel_rel fk_project_channel_rel_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_channel_rel
    ADD CONSTRAINT fk_project_channel_rel_created_by FOREIGN KEY (created_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: project_channel_rel fk_project_channel_rel_project; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_channel_rel
    ADD CONSTRAINT fk_project_channel_rel_project FOREIGN KEY (project_id, workspace_id) REFERENCES public.project(id, workspace_id) ON DELETE CASCADE;


--
-- Name: project_event fk_project_event_project; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_event
    ADD CONSTRAINT fk_project_event_project FOREIGN KEY (project_id) REFERENCES public.project(id) ON DELETE CASCADE;


--
-- Name: project_member fk_project_member_invited_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_member
    ADD CONSTRAINT fk_project_member_invited_by FOREIGN KEY (invited_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: project_member fk_project_member_project; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_member
    ADD CONSTRAINT fk_project_member_project FOREIGN KEY (project_id, workspace_id) REFERENCES public.project(id, workspace_id) ON DELETE CASCADE;


--
-- Name: project_member fk_project_member_ws_member; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_member
    ADD CONSTRAINT fk_project_member_ws_member FOREIGN KEY (workspace_id, user_id) REFERENCES public.workspace_member(workspace_id, user_id) ON DELETE CASCADE;


--
-- Name: project_milestone fk_project_milestone_project; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_milestone
    ADD CONSTRAINT fk_project_milestone_project FOREIGN KEY (project_id, workspace_id) REFERENCES public.project(id, workspace_id) ON DELETE CASCADE;


--
-- Name: project fk_project_owner_membership; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project
    ADD CONSTRAINT fk_project_owner_membership FOREIGN KEY (workspace_id, owner_id) REFERENCES public.workspace_member(workspace_id, user_id) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;


--
-- Name: project_task fk_project_task_assignee; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_task
    ADD CONSTRAINT fk_project_task_assignee FOREIGN KEY (assignee_id) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: project_task fk_project_task_creator; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_task
    ADD CONSTRAINT fk_project_task_creator FOREIGN KEY (creator_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: project_task fk_project_task_project; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_task
    ADD CONSTRAINT fk_project_task_project FOREIGN KEY (project_id) REFERENCES public.project(id) ON DELETE CASCADE;


--
-- Name: project fk_project_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project
    ADD CONSTRAINT fk_project_workspace FOREIGN KEY (workspace_id) REFERENCES public.workspace(id) ON DELETE CASCADE;


--
-- Name: review_asset fk_ra_attachment; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.review_asset
    ADD CONSTRAINT fk_ra_attachment FOREIGN KEY (attachment_id) REFERENCES public.attachment(id) ON DELETE RESTRICT;


--
-- Name: review_asset fk_ra_review; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.review_asset
    ADD CONSTRAINT fk_ra_review FOREIGN KEY (review_id) REFERENCES public.teacher_review(id) ON DELETE CASCADE;


--
-- Name: report_action_log fk_report_action_report_id; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.report_action_log
    ADD CONSTRAINT fk_report_action_report_id FOREIGN KEY (report_id) REFERENCES public.report_ticket(id) ON DELETE CASCADE;


--
-- Name: submission_asset fk_sa_attachment; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.submission_asset
    ADD CONSTRAINT fk_sa_attachment FOREIGN KEY (attachment_id) REFERENCES public.attachment(id) ON DELETE RESTRICT;


--
-- Name: submission_asset fk_sa_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.submission_asset
    ADD CONSTRAINT fk_sa_created_by FOREIGN KEY (created_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: submission_asset fk_sa_submission; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.submission_asset
    ADD CONSTRAINT fk_sa_submission FOREIGN KEY (submission_id) REFERENCES public.homework_submission(id) ON DELETE CASCADE;


--
-- Name: teacher_review fk_tr_submission; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_review
    ADD CONSTRAINT fk_tr_submission FOREIGN KEY (submission_id) REFERENCES public.homework_submission(id) ON DELETE CASCADE;


--
-- Name: teacher_review fk_tr_video_attachment; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_review
    ADD CONSTRAINT fk_tr_video_attachment FOREIGN KEY (video_attachment_id) REFERENCES public.attachment(id) ON DELETE RESTRICT;


--
-- Name: user_deletion_request fk_user_deletion_request_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_deletion_request
    ADD CONSTRAINT fk_user_deletion_request_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: wallet_transaction fk_wallet_tx_wallet; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wallet_transaction
    ADD CONSTRAINT fk_wallet_tx_wallet FOREIGN KEY (wallet_id) REFERENCES public.wallet(id);


--
-- Name: workspace fk_workspace_archived_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace
    ADD CONSTRAINT fk_workspace_archived_by FOREIGN KEY (archived_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: workspace_invite fk_workspace_invite_created_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_invite
    ADD CONSTRAINT fk_workspace_invite_created_by FOREIGN KEY (created_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: workspace_invite fk_workspace_invite_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_invite
    ADD CONSTRAINT fk_workspace_invite_workspace FOREIGN KEY (workspace_id) REFERENCES public.workspace(id) ON DELETE CASCADE;


--
-- Name: workspace_member fk_workspace_member_invited_by; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_member
    ADD CONSTRAINT fk_workspace_member_invited_by FOREIGN KEY (invited_by) REFERENCES public."user"(id) ON DELETE SET NULL;


--
-- Name: workspace_member fk_workspace_member_user; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_member
    ADD CONSTRAINT fk_workspace_member_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: workspace_member fk_workspace_member_workspace; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_member
    ADD CONSTRAINT fk_workspace_member_workspace FOREIGN KEY (workspace_id) REFERENCES public.workspace(id) ON DELETE CASCADE;


--
-- Name: workspace fk_workspace_organization; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace
    ADD CONSTRAINT fk_workspace_organization FOREIGN KEY (organization_id) REFERENCES public.organization(id) ON DELETE RESTRICT;


--
-- Name: workspace fk_workspace_owner; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace
    ADD CONSTRAINT fk_workspace_owner FOREIGN KEY (owner_id) REFERENCES public."user"(id) ON DELETE CASCADE;


--
-- Name: system_id_segment system_id_segment_datacenter_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.system_id_segment
    ADD CONSTRAINT system_id_segment_datacenter_id_fkey FOREIGN KEY (datacenter_id) REFERENCES public.system_datacenter(id);


--
-- Name: workspace_group_read_cursor workspace_group_read_cursor_generation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.workspace_group_read_cursor
    ADD CONSTRAINT workspace_group_read_cursor_generation_id_fkey FOREIGN KEY (generation_id) REFERENCES public.group_member_generation(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

\unrestrict JwWtw0Bzy5pPapekDOnuf7mg2yrvHesDliMn0SddDQqRYZRvj7ZhI8Rbma1ztOm
