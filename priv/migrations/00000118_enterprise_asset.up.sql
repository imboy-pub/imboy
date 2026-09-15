-- 迁移 00000118: 企业附件（Enterprise Asset）。
-- 计划契约：§3 EB-D06（企业附件）、§4.1（enterprise_asset）、§4.3（资产的 Workspace 与 Org 一致）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * DB 只存 object key / hash / mime / size / cipher metadata，不持久化任何 URL（签名 URL 一律
--     由后端按请求签发并流式转发，客户端不得收到 Garage endpoint 或 presigned GET）。
--     因此 object_key 强制 !~* '^[a-z]+://'。
--   * 未经 confirm 的上传停在 pending_confirm（可被限定前缀 cleanup）；uploaded_by_user_id 仅审计。
--   * 附件保留期限不得早于其所属消息：message_id 非空时必须有 retain_until 且 >= 消息的 retain_until。
--   * 物理删除走与消息同一套 bounded purge 四步判定（GUC + purge worker 角色 + 到期 + 无 active hold）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS enterprise_asset (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    workspace_id         bigint                   NOT NULL,
    conversation_id      bigint,
    message_id           bigint,
    business_identity_id bigint,
    uploaded_by_user_id  bigint,
    object_key           text                     NOT NULL,
    object_hash          text                     NOT NULL,
    mime                 text,
    size_bytes           bigint,
    status               text                     DEFAULT 'pending_confirm' NOT NULL,
    key_version          integer,
    cipher_metadata      jsonb,
    retain_until         timestamp with time zone,
    version              integer                  DEFAULT 1 NOT NULL,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    deleted_at           timestamp with time zone,
    CONSTRAINT pk_enterprise_asset PRIMARY KEY (id),
    CONSTRAINT uq_enterprise_asset_org_id UNIQUE (organization_id, id),
    CONSTRAINT uq_enterprise_asset_object_key UNIQUE (object_key),
    CONSTRAINT ck_enterprise_asset_status CHECK (
        status = ANY (ARRAY['pending_confirm'::text, 'active'::text, 'deleted'::text])),
    -- ⚠️ 计划文本写作 !~* '^[a-z]+://'，但该式无法命中含数字的 scheme（如 s3://、gs://），
    --    与「不持久化任何 URL」的硬要求不符；此处按 EB-D06 的意图收紧为通用 scheme 形态。
    CONSTRAINT ck_enterprise_asset_object_key_no_url
        CHECK (object_key !~* '^[a-zA-Z][a-zA-Z0-9+.\-]*://'),
    CONSTRAINT ck_enterprise_asset_size CHECK (size_bytes IS NULL OR size_bytes >= 0),
    CONSTRAINT ck_enterprise_asset_version CHECK (version >= 1),
    CONSTRAINT fk_enterprise_asset_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_enterprise_asset_conversation FOREIGN KEY (organization_id, workspace_id, conversation_id)
        REFERENCES enterprise_conversation (organization_id, workspace_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_enterprise_asset_message FOREIGN KEY (organization_id, workspace_id, message_id)
        REFERENCES enterprise_message (organization_id, workspace_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_enterprise_asset_uploaded_by FOREIGN KEY (uploaded_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_asset IS
    '企业附件（归 Org）：只存 object key/hash/mime/size/cipher metadata，不存也不返回任何 storage URL；删除只服从 Org policy，不服从 uploader、不由 offboarding 触发';
COMMENT ON COLUMN enterprise_asset.object_key IS '企业私有前缀下的对象键；禁止持久化 URL（通用 scheme 形态 !~* ''^[a-zA-Z][a-zA-Z0-9+.-]*://''，覆盖 http/https/s3/gs 等）';
COMMENT ON COLUMN enterprise_asset.object_hash IS '对象内容哈希（完整性校验；非加密材料）';
COMMENT ON COLUMN enterprise_asset.status IS '状态: pending_confirm 上传待确认（可被限定前缀 cleanup）| active 已确认 | deleted 已删除';
COMMENT ON COLUMN enterprise_asset.key_version IS 'cipher_metadata 对应的企业托管密钥版本';
COMMENT ON COLUMN enterprise_asset.cipher_metadata IS '加密元数据 jsonb（算法/IV/分段信息等，无明文、无密钥）';
COMMENT ON COLUMN enterprise_asset.retain_until IS '附件保留截止时间；message_id 非空时必须非空且不得早于所属消息的 retain_until';
COMMENT ON COLUMN enterprise_asset.uploaded_by_user_id IS '上传人（仅审计）；user 删除后置 NULL，不级联企业附件';

CREATE INDEX IF NOT EXISTS i_enterprise_asset_org_message ON enterprise_asset
    USING btree (organization_id, workspace_id, message_id);
CREATE INDEX IF NOT EXISTS i_enterprise_asset_org_retain ON enterprise_asset
    USING btree (organization_id, workspace_id, retain_until);

-- 附件不得早于所属消息删除
CREATE OR REPLACE FUNCTION fn_enterprise_asset_retention_guard() RETURNS trigger
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

COMMENT ON FUNCTION fn_enterprise_asset_retention_guard() IS
    '附件保留期守卫：message_id 非空时要求 retain_until 非空且 >= 所属消息的 retain_until，否则 23514';

DROP TRIGGER IF EXISTS trg_enterprise_asset_retention_guard ON enterprise_asset;
CREATE TRIGGER trg_enterprise_asset_retention_guard
    BEFORE INSERT OR UPDATE OF retain_until, status, message_id ON enterprise_asset
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_asset_retention_guard();

-- 附件物理删除走同一套 bounded purge 四步判定
CREATE OR REPLACE FUNCTION fn_enterprise_asset_purge_guard() RETURNS trigger
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

COMMENT ON FUNCTION fn_enterprise_asset_purge_guard() IS
    '企业附件物理删除守卫（BEFORE DELETE，四步按序判定）：1) GUC imboy.enterprise_purge=''on'' 否则 23514；2) 当前角色必须是 cluster 角色 imboy_enterprise_purge_worker 的成员（角色由部署/DBA 预置，migration 不 CREATE ROLE），否则 42501；3) retain_until <= now() 否则 23514；4) 无 active hold 覆盖其 message/conversation/workspace，否则 23514';

DROP TRIGGER IF EXISTS trg_enterprise_asset_purge_guard ON enterprise_asset;
CREATE TRIGGER trg_enterprise_asset_purge_guard
    BEFORE DELETE ON enterprise_asset
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_asset_purge_guard();
