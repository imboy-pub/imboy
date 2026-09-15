-- 迁移 00000117: 企业保留策略与保留 Hold（Enterprise Retention Policy & Hold）。
-- 计划契约：§3 EB-D12（Workspace 保留策略与 Hold）、§4.1（enterprise_retention_policy /
--   enterprise_retention_hold）、§4.3（retain_until 不可缩短；到期前 / active hold 中 /
--   非 purge DB role 的物理删除全部被拒绝；失败宁可多保留）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * policy 是不可变版本快照：UPDATE/DELETE 一律拒绝，只能追加新版本；
--     新版本的 retention_days 不得小于同一 (Org,Workspace,data_class) 的既有版本，
--     因此消息端固化的 retain_until 快照永不前移。
--   * hold 是 append-only hold/release 事实：只允许一次性写入 released_at + released_by_user_id。
--   * 物理删除只允许唯一 bounded purge worker。DB guard 四步判定（顺序固定）：
--       1) 会话 GUC imboy.enterprise_purge 必须为 'on'      → 23514
--       2) 当前角色必须是 imboy_enterprise_purge_worker 成员 → 42501
--       3) OLD.retain_until <= now()                        → 23514
--       4) 不存在覆盖该消息的 active hold                    → 23514
--     ⚠️ 角色 imboy_enterprise_purge_worker 是 cluster 级对象，不属于 migration：
--        本迁移不 CREATE ROLE，由部署/DBA 预置。守卫函数名与所需角色名见
--        COMMENT ON FUNCTION fn_enterprise_message_purge_guard()。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: enterprise_retention_policy（不可变版本快照）
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_retention_policy (
    id                 bigint                   NOT NULL,  -- TSID
    organization_id    bigint                   NOT NULL,
    workspace_id       bigint                   NOT NULL,
    data_class         text                     NOT NULL,
    version            integer                  NOT NULL,
    retention_days     integer                  NOT NULL,
    trigger_event      text,
    effective_at       timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by_user_id bigint,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_retention_policy PRIMARY KEY (id),
    CONSTRAINT uq_erp_org_ws_class_version
        UNIQUE (organization_id, workspace_id, data_class, version),
    CONSTRAINT ck_erp_data_class CHECK (
        data_class = ANY (ARRAY['enterprise_message'::text, 'enterprise_asset'::text])),
    CONSTRAINT ck_erp_retention_days CHECK (retention_days > 0),
    CONSTRAINT ck_erp_version CHECK (version >= 1),
    CONSTRAINT fk_erp_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_erp_created_by FOREIGN KEY (created_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_retention_policy IS
    'Workspace 保留策略（不可变版本快照）：以 (Org,Workspace,data_class,version) 标识；旧版本永久保留，只允许追加新版本且不得缩短期限（EB-D12）';
COMMENT ON COLUMN enterprise_retention_policy.data_class IS '数据类别: enterprise_message | enterprise_asset（V1 本地 fixture 用 enterprise_message / retention_days=1095）';
COMMENT ON COLUMN enterprise_retention_policy.retention_days IS '保留天数（>0）；新版本不得小于同 Org/Workspace/data_class 的既有版本，否则 23514';
COMMENT ON COLUMN enterprise_retention_policy.trigger_event IS '保留起算事件（如 message.accept）；生产期限/起算事件/法律依据由 owner/legal 人工确认';
COMMENT ON COLUMN enterprise_retention_policy.created_by_user_id IS '创建人（审计快照）；user 删除后置 NULL';

CREATE INDEX IF NOT EXISTS i_erp_org_ws_class ON enterprise_retention_policy
    USING btree (organization_id, workspace_id, data_class, version DESC);

-- 不可变：任何 UPDATE / DELETE 一律拒绝（唯一例外见下：user 删除引发的 created_by_user_id 置 NULL）
CREATE OR REPLACE FUNCTION fn_enterprise_retention_policy_immutable() RETURNS trigger
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

COMMENT ON FUNCTION fn_enterprise_retention_policy_immutable() IS
    '保留策略不可变守卫：DELETE 一律 23514；UPDATE 一律 23514，唯一例外是 fk_erp_created_by 的 ON DELETE SET NULL 把 created_by_user_id 置 NULL（期限/类别/版本等快照内容不可改写）';

DROP TRIGGER IF EXISTS trg_enterprise_retention_policy_immutable ON enterprise_retention_policy;
CREATE TRIGGER trg_enterprise_retention_policy_immutable
    BEFORE UPDATE OR DELETE ON enterprise_retention_policy
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_retention_policy_immutable();

-- 禁止缩短：新版本 retention_days 不得小于既有版本
CREATE OR REPLACE FUNCTION fn_enterprise_retention_policy_guard() RETURNS trigger
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

COMMENT ON FUNCTION fn_enterprise_retention_policy_guard() IS
    '保留期只增不减守卫（BEFORE INSERT）：同 Org/Workspace/data_class 已存在 retention_days 更大的版本时 23514';

DROP TRIGGER IF EXISTS trg_enterprise_retention_policy_guard ON enterprise_retention_policy;
CREATE TRIGGER trg_enterprise_retention_policy_guard
    BEFORE INSERT ON enterprise_retention_policy
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_retention_policy_guard();

-- ============================================================
-- Phase 2: enterprise_retention_hold（append-only hold/release 事实）
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_retention_hold (
    id                    bigint                   NOT NULL,  -- TSID
    organization_id       bigint                   NOT NULL,
    workspace_id          bigint                   NOT NULL,
    scope_type            text                     NOT NULL,
    scope_conversation_id bigint,
    scope_message_id      bigint,
    reason_code           text                     NOT NULL,
    actor_user_id         bigint,
    audit_event_id        bigint,
    version               integer                  DEFAULT 1 NOT NULL,
    created_at            timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    released_at           timestamp with time zone,
    released_by_user_id   bigint,
    CONSTRAINT pk_enterprise_retention_hold PRIMARY KEY (id),
    CONSTRAINT ck_erh_scope_type CHECK (
        scope_type = ANY (ARRAY['conversation'::text, 'message'::text, 'workspace'::text])),
    CONSTRAINT ck_erh_scope_shape CHECK (
        (scope_type = 'workspace' AND scope_conversation_id IS NULL AND scope_message_id IS NULL)
        OR (scope_type = 'conversation'
            AND scope_conversation_id IS NOT NULL AND scope_message_id IS NULL)
        OR (scope_type = 'message'
            AND scope_message_id IS NOT NULL AND scope_conversation_id IS NULL)),
    CONSTRAINT ck_erh_version CHECK (version >= 1),
    CONSTRAINT fk_erh_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_erh_conversation FOREIGN KEY (organization_id, workspace_id, scope_conversation_id)
        REFERENCES enterprise_conversation (organization_id, workspace_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_erh_message FOREIGN KEY (organization_id, workspace_id, scope_message_id)
        REFERENCES enterprise_message (organization_id, workspace_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_erh_actor FOREIGN KEY (actor_user_id) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_erh_released_by FOREIGN KEY (released_by_user_id)
        REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_retention_hold IS
    '保留 Hold（append-only hold/release 事实）：绑定 Org/Workspace + conversation|message|workspace 作用域；active hold 优先于 retain_until，只允许延长保留';
COMMENT ON COLUMN enterprise_retention_hold.scope_type IS '作用域: conversation | message | workspace（与 scope_* 列自洽，由 ck_erh_scope_shape 强制）';
COMMENT ON COLUMN enterprise_retention_hold.reason_code IS '合成/受控 reason code；真实 hold 的创建与释放属于需担责操作，本地只用 synthetic fixture';
COMMENT ON COLUMN enterprise_retention_hold.released_at IS '释放时间；NULL=hold 生效中。只允许由 NULL 一次性写入，且必须同时写 released_by_user_id';
COMMENT ON COLUMN enterprise_retention_hold.released_by_user_id IS '释放人；与 released_at 同步写入（释放后不可再次修改）';

-- active hold 的查询索引支撑（purge 守卫按 scope 命中判断）
CREATE INDEX IF NOT EXISTS i_erh_active_message ON enterprise_retention_hold
    USING btree (organization_id, workspace_id, scope_message_id)
    WHERE released_at IS NULL AND scope_message_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS i_erh_active_conversation ON enterprise_retention_hold
    USING btree (organization_id, workspace_id, scope_conversation_id)
    WHERE released_at IS NULL AND scope_conversation_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS i_erh_active_workspace ON enterprise_retention_hold
    USING btree (organization_id, workspace_id)
    WHERE released_at IS NULL AND scope_type = 'workspace';

CREATE OR REPLACE FUNCTION fn_enterprise_retention_hold_append_only() RETURNS trigger
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

COMMENT ON FUNCTION fn_enterprise_retention_hold_append_only() IS
    'hold append-only 守卫：DELETE 一律 23514；UPDATE 只允许 (a) 未释放时一次性写入 released_at + released_by_user_id 的 release，或 (b) user 删除引发的 actor_user_id/released_by_user_id 置 NULL；其余变更 23514';

DROP TRIGGER IF EXISTS trg_enterprise_retention_hold_append_only ON enterprise_retention_hold;
CREATE TRIGGER trg_enterprise_retention_hold_append_only
    BEFORE UPDATE OR DELETE ON enterprise_retention_hold
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_retention_hold_append_only();

-- ============================================================
-- Phase 3: enterprise_message 物理删除守卫（唯一 bounded purge 通道）
-- ============================================================
CREATE OR REPLACE FUNCTION fn_enterprise_message_purge_guard() RETURNS trigger
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

COMMENT ON FUNCTION fn_enterprise_message_purge_guard() IS
    '企业消息物理删除守卫（BEFORE DELETE，四步按序判定）：1) 会话 GUC imboy.enterprise_purge=''on'' 否则 23514；2) 当前角色必须是 cluster 角色 imboy_enterprise_purge_worker 的成员（不 CREATE ROLE，角色由部署/DBA 预置），否则 42501；3) OLD.retain_until <= now() 否则 23514；4) 无 active hold 覆盖该消息，否则 23514。通过后才 RETURN OLD；失败宁可多保留，不得提前删除';

DROP TRIGGER IF EXISTS trg_enterprise_message_purge_guard ON enterprise_message;
CREATE TRIGGER trg_enterprise_message_purge_guard
    BEFORE DELETE ON enterprise_message
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_message_purge_guard();
