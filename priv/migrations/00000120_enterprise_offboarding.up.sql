-- 迁移 00000120: 企业离职交接（Enterprise Offboarding）。
-- 计划契约：§3 EB-D07（离职状态机）、§4.1（enterprise_offboarding_case / enterprise_offboarding_item）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * case 状态机 draft -> frozen -> transferring -> verifying -> completed，transferring/verifying -> failed。
--     同一 Org + leaver 同时最多一个未完成 case（partial unique）。
--   * item 以 idempotency_key 保证幂等重放不增行；成功项必须写入 to_user_id；
--     failure_reason 只允许出现在 failed 项。
--   * leaver/successor/from/to 全部是 user 引用且 ON DELETE SET NULL：删除 user 不级联企业交接数据
--     （撤权 + rebind identity 才是正确路径，资源 owner/ID 不变）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS enterprise_offboarding_case (
    id                 bigint                   NOT NULL,  -- TSID
    organization_id    bigint                   NOT NULL,
    leaver_user_id     bigint,
    successor_user_id  bigint,
    status             text                     DEFAULT 'draft' NOT NULL,
    version            integer                  DEFAULT 1 NOT NULL,
    item_total         integer                  DEFAULT 0 NOT NULL,
    item_success       integer                  DEFAULT 0 NOT NULL,
    item_failed        integer                  DEFAULT 0 NOT NULL,
    created_by_user_id bigint,
    reason             text,
    created_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at         timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    completed_at       timestamp with time zone,
    CONSTRAINT pk_enterprise_offboarding_case PRIMARY KEY (id),
    CONSTRAINT uq_eoc_org_id UNIQUE (organization_id, id),
    CONSTRAINT ck_eoc_status CHECK (status = ANY (ARRAY['draft'::text, 'frozen'::text, 'transferring'::text,
                                                        'verifying'::text, 'completed'::text, 'failed'::text])),
    CONSTRAINT ck_eoc_counts CHECK (item_total >= 0 AND item_success >= 0 AND item_failed >= 0),
    CONSTRAINT ck_eoc_version CHECK (version >= 1),
    CONSTRAINT fk_eoc_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eoc_leaver FOREIGN KEY (leaver_user_id) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_eoc_successor FOREIGN KEY (successor_user_id) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_eoc_created_by FOREIGN KEY (created_by_user_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_offboarding_case IS
    '离职交接 case（状态机 draft->frozen->transferring->verifying->completed，或 ->failed）：同 Org+leaver 同时最多一个未完成 case';
COMMENT ON COLUMN enterprise_offboarding_case.leaver_user_id IS '离职人（user 引用，SET NULL）：交接期间锁定 assignment version 逐项 rebind';
COMMENT ON COLUMN enterprise_offboarding_case.successor_user_id IS '承接人（user 引用，SET NULL）';
COMMENT ON COLUMN enterprise_offboarding_case.status IS '状态: draft | frozen | transferring | verifying | completed | failed';
COMMENT ON COLUMN enterprise_offboarding_case.version IS 'CAS 版本：执行时锁定，避免并发 rebind 重复转移';
COMMENT ON COLUMN enterprise_offboarding_case.item_total IS '交接项总数（含 success + failed + pending）';

CREATE UNIQUE INDEX IF NOT EXISTS uq_eoc_unfinished_leaver
    ON enterprise_offboarding_case (organization_id, leaver_user_id)
    WHERE status NOT IN ('completed', 'failed');

CREATE INDEX IF NOT EXISTS i_eoc_org_status ON enterprise_offboarding_case
    USING btree (organization_id, status);

CREATE TABLE IF NOT EXISTS enterprise_offboarding_item (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    case_id              bigint                   NOT NULL,
    business_identity_id bigint                   NOT NULL,
    function_key         text                     NOT NULL,
    from_user_id         bigint,
    to_user_id           bigint,
    status               text                     DEFAULT 'pending' NOT NULL,
    idempotency_key      text                     NOT NULL,
    attempt              integer                  DEFAULT 0 NOT NULL,
    failure_reason       text,
    audit_event_id       bigint,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_enterprise_offboarding_item PRIMARY KEY (id),
    CONSTRAINT uq_eoi_org_idempotency_key UNIQUE (organization_id, idempotency_key),
    CONSTRAINT ck_eoi_status CHECK (status = ANY (ARRAY['pending'::text, 'success'::text, 'failed'::text])),
    CONSTRAINT ck_eoi_attempt CHECK (attempt >= 0),
    CONSTRAINT ck_eoi_success_requires_to_user CHECK (status <> 'success' OR to_user_id IS NOT NULL),
    CONSTRAINT ck_eoi_failure_reason CHECK (failure_reason IS NULL OR status = 'failed'),
    CONSTRAINT ck_eoi_idempotency_key CHECK (idempotency_key <> ''),
    CONSTRAINT fk_eoi_case FOREIGN KEY (organization_id, case_id)
        REFERENCES enterprise_offboarding_case (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eoi_identity_function
        FOREIGN KEY (organization_id, business_identity_id, function_key)
        REFERENCES organization_business_identity (organization_id, id, function_key)
        ON DELETE RESTRICT,
    CONSTRAINT fk_eoi_from_user FOREIGN KEY (from_user_id) REFERENCES "user"(id) ON DELETE SET NULL,
    CONSTRAINT fk_eoi_to_user FOREIGN KEY (to_user_id) REFERENCES "user"(id) ON DELETE SET NULL
);

COMMENT ON TABLE enterprise_offboarding_item IS
    '离职交接项（case 下每个 business identity 一项）：资源 owner/ID 不变，只把 identity 从 A rebind 到 B；成功项不可重复写审计';
COMMENT ON COLUMN enterprise_offboarding_item.business_identity_id IS '被交接的稳定业务身份；与 function_key 由复合 FK 保持一致';
COMMENT ON COLUMN enterprise_offboarding_item.idempotency_key IS '幂等键：同一 Org 内唯一，重放不增行';
COMMENT ON COLUMN enterprise_offboarding_item.status IS '状态: pending 待处理 | success 已成功 | failed 失败（保留 reason 可重试）';
COMMENT ON COLUMN enterprise_offboarding_item.attempt IS '重试次数（>=0）';
COMMENT ON COLUMN enterprise_offboarding_item.failure_reason IS '失败原因；只允许在 status=failed 时非空';
COMMENT ON COLUMN enterprise_offboarding_item.audit_event_id IS '关联的企业审计事件 ID（append-only 真源在 enterprise_audit_event）';

CREATE INDEX IF NOT EXISTS i_eoi_org_case ON enterprise_offboarding_item
    USING btree (organization_id, case_id, status);
CREATE INDEX IF NOT EXISTS i_eoi_org_identity ON enterprise_offboarding_item
    USING btree (organization_id, business_identity_id, status);
