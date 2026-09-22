-- 迁移 00000139: Enterprise Application Grant（Org/Workspace Grant 授权真源）。
-- 计划契约：docs/plans/2026-09-21-enterprise-internal-platform-full-v1-implementation-plan.md
--   §5 数据扩展（enterprise_application_grant：org、workspace、status、version、
--   validity；enterprise_application_grant_scope：固定 scope 枚举，FK 到 grant）、
--   §3.1（Organization Grant + Workspace Grant；scope 与资源授权取交集，逐请求读
--   当前状态，撤权立即生效）、§7 安全硬门（双 Org × 双 Workspace × 多 Grant 的
--   cross-tenant/IDOR 矩阵全部拒绝；grant revoked / scope downgrade 下一请求失败）。
-- 任务契约：FULL-01（本 phase 唯一 DDL owner；编号 139 由 A0 在 FULL-00 预留，
--   跳过被活跃 foreign run 持有的 137/138，不复用/不重命名既有编号）。
-- 迁移契约：up=可重复执行，down=完整对称回滚。禁止 BEGIN/COMMIT——erlang_migrate
--   外层单事务包裹。
--
-- 设计决策：
--   * 一表一 Grant：「Organization Grant」= workspace_scope_kind='none'（org 全域），
--     「Workspace Grant」= workspace_scope_kind='explicit' + 显式 workspace_id 行
--     （enterprise_application_grant_workspace）。词汇与 00000133 agent_grant 完全
--     一致（workspace_scope_kind none|explicit），不引入第二套方言。
--   * 子表对父表的引用带「kind 闸门」：父表 UNIQUE (organization_id, id,
--     workspace_scope_kind)，子表列 workspace_scope_kind 被 CHECK 钉死 'explicit'
--     并以三列复合 FK 引用——给 'none' 类型 Grant 挂 workspace 行在 DB 层不可能
--     （纯声明式、无触发器），'none' 的零行语义由 FK 结构性保证。
--   * scope 固定枚举在 DB 层 CHECK（plan-gz §4.2 十值）：wildcard/未登记 scope
--     一律 23514——授权表本身无法被写成通配（INV-4 的 DB 侧兜底）。
--   * status(active|revoked) + valid_from/expires_at 是授权的唯一判定输入，
--     expired 不入库（同 00000133）：撤权/到期在**下一次读取**即生效，无后台任务、
--     无缓存失效链。
--   * 授权行禁止物理删除（BEFORE DELETE 触发器 23514）：删除授权行会静默改变
--     治理模式与权限面（见 fn_enterprise_application_grant_no_delete 注释）；生命周期
--     只走 status，down 用 DROP TABLE（不触发行触发器）。
--   * 读取面 = 两个只读视图（不物化、不去规范化）：
--       v_enterprise_effective_application_grant        —— 当前生效的 Grant 行
--       v_enterprise_effective_application_grant_scope  —— 当前生效的 (grant, scope)
--     视图内谓词 CURRENT_TIMESTAMP 在**每次查询**求值，天然满足「逐请求读当前
--     状态、撤权立即生效」；消费方禁止在应用层缓存授权结论。
--   * 复合 FK 一律跨 Org 拒绝（MATCH SIMPLE：NULL-org 匹配不到）：
--     grant -> application(organization_id, id)、
--     grant_workspace -> workspace(organization_id, id)。
--   * FK 删除行为镜像 00000133/00000136：organization/application RESTRICT（企业资产
--     不物理删，走 status 软停）；子表对 Grant 的 FK CASCADE（Grant 行本身已被
--     触发器禁止物理删除，CASCADE 只保证 down/维护窗口的依赖语义自洽）；
--     revoked_by_user_id RESTRICT（与「revoked_at/revoked_by 同空/同非空」CHECK
--     配合——若用 SET NULL 会把该 CHECK 撞成用户删除失败）。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: enterprise_application_grant
-- ============================================================
CREATE TABLE IF NOT EXISTS enterprise_application_grant (
    id                   bigint                   NOT NULL,  -- TSID
    organization_id      bigint                   NOT NULL,
    application_id       bigint                   NOT NULL,
    workspace_scope_kind text                     DEFAULT 'none' NOT NULL,
    status               text                     DEFAULT 'active' NOT NULL,
    valid_from           timestamp with time zone NOT NULL,
    expires_at           timestamp with time zone NOT NULL,
    revoked_at           timestamp with time zone,
    revoked_by_user_id   bigint,
    version              integer                  DEFAULT 1 NOT NULL,
    idempotency_key      text                     NOT NULL,
    created_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_enterprise_application_grant PRIMARY KEY (id),
    CONSTRAINT uq_eag_org_id_kind UNIQUE (organization_id, id, workspace_scope_kind),
    CONSTRAINT uq_eag_org_app_idempotency UNIQUE (organization_id, application_id, idempotency_key),
    CONSTRAINT ck_eag_workspace_scope_kind
        CHECK (workspace_scope_kind = ANY (ARRAY['none'::text, 'explicit'::text])),
    CONSTRAINT ck_eag_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text])),
    CONSTRAINT ck_eag_validity CHECK (expires_at > valid_from),
    CONSTRAINT ck_eag_status_revoked_match CHECK (
        (status = 'revoked') = (revoked_at IS NOT NULL)
        AND (revoked_at IS NULL) = (revoked_by_user_id IS NULL)
    ),
    CONSTRAINT ck_eag_version CHECK (version >= 1),
    CONSTRAINT ck_eag_idempotency_key CHECK (
        idempotency_key <> '' AND length(idempotency_key) <= 256
    ),
    CONSTRAINT fk_eag_organization FOREIGN KEY (organization_id)
        REFERENCES organization(id) ON DELETE RESTRICT,
    CONSTRAINT fk_eag_application FOREIGN KEY (organization_id, application_id)
        REFERENCES enterprise_application (organization_id, id) ON DELETE RESTRICT,
    CONSTRAINT fk_eag_revoked_by FOREIGN KEY (revoked_by_user_id)
        REFERENCES "user"(id) ON DELETE RESTRICT
);

COMMENT ON TABLE  enterprise_application_grant IS
    'Application Grant（Organization Grant / Workspace Grant 真源）：scope 与资源授权取交集的授权侧输入；逐请求读取，撤权/到期下一次读取即生效';
COMMENT ON COLUMN enterprise_application_grant.id IS '主键 TSID';
COMMENT ON COLUMN enterprise_application_grant.organization_id IS 'Grant 归属机构；删除机构一律 RESTRICT（fail-closed）';
COMMENT ON COLUMN enterprise_application_grant.application_id IS
    '被授权 Application（同 Org 复合 FK；跨 Org 引用 23503 拒绝）';
COMMENT ON COLUMN enterprise_application_grant.workspace_scope_kind IS
    'none=org 全域（零行 grant_workspace）| explicit=显式 workspace 授权（至少一行，行数语义由 Grant command 同一事务校验）';
COMMENT ON COLUMN enterprise_application_grant.status IS '存储态只有 active|revoked；expired 不入库，到期由 status+expires_at 实时判定';
COMMENT ON COLUMN enterprise_application_grant.valid_from IS '生效起始（不晚于 expires_at）；未生效的 Grant 不出现在 effective 视图';
COMMENT ON COLUMN enterprise_application_grant.expires_at IS '到期时刻；到期不依赖后台任务即在读取侧失效';
COMMENT ON COLUMN enterprise_application_grant.revoked_at IS '撤销时间；仅 status=revoked 非空（与 revoked_by_user_id 同空/同非空）';
COMMENT ON COLUMN enterprise_application_grant.revoked_by_user_id IS '撤销操作者；与 revoked_at 同空/同非空；RESTRICT 保护授权 lineage';
COMMENT ON COLUMN enterprise_application_grant.version IS 'CAS 版本（>=1）；所有 mutation 走 expected-version CAS（并发撤销/降级不丢更新）';
COMMENT ON COLUMN enterprise_application_grant.idempotency_key IS '幂等键；同 (organization_id, application_id) 范围唯一';

CREATE INDEX IF NOT EXISTS i_eag_org_app_status_expires ON enterprise_application_grant
    USING btree (organization_id, application_id, status, expires_at);

-- 授权行禁止物理删除：删除会静默改变授权面（受管应用的 Grant 被删光后退回
-- 「未受管」= 更宽的广州期边界），且会抹掉撤销/降级审计 lineage。
-- 生命周期只走 status（active|revoked）；维护窗口用 DROP TABLE（不触发行触发器）。
CREATE OR REPLACE FUNCTION fn_enterprise_application_grant_no_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    RAISE EXCEPTION
        'enterprise_application_grant 是授权真源行，禁止 DELETE；撤销请置 status=revoked（删除会静默改变权限面与审计 lineage）'
        USING ERRCODE = '23514',
              CONSTRAINT = 'trg_enterprise_application_grant_no_delete';
END;
$$;

COMMENT ON FUNCTION fn_enterprise_application_grant_no_delete() IS
    'Application Grant 授权行物理删除守卫：DELETE 一律 23514（撤销走 status=revoked，保留 lineage 且撤权即时生效）';

DROP TRIGGER IF EXISTS trg_enterprise_application_grant_no_delete
    ON enterprise_application_grant;
CREATE TRIGGER trg_enterprise_application_grant_no_delete
    BEFORE DELETE ON enterprise_application_grant
    FOR EACH ROW EXECUTE FUNCTION fn_enterprise_application_grant_no_delete();

-- ============================================================
-- Phase 2: enterprise_application_grant_scope（固定 scope 枚举）
-- ============================================================
-- scope 全集恰为 plan-gz §4.2 冻结的 10 个固定值（与
-- enterprise_internal_scope:all/0 逐字一致）；DB 层 CHECK 保证授权表里
-- 不可能出现 wildcard 或未登记 scope。
CREATE TABLE IF NOT EXISTS enterprise_application_grant_scope (
    grant_id bigint NOT NULL,
    scope    text   NOT NULL,
    CONSTRAINT pk_enterprise_application_grant_scope PRIMARY KEY (grant_id, scope),
    CONSTRAINT ck_eags_scope_fixed CHECK (scope = ANY (ARRAY[
        'application:read'::text,
        'identities:read'::text,
        'identities:write'::text,
        'groups:write'::text,
        'files:write'::text,
        'messages:send'::text,
        'messages:send_as_human'::text,
        'friend_requests:create'::text,
        'webhooks:manage'::text,
        'sso:exchange'::text
    ])),
    CONSTRAINT fk_eags_grant FOREIGN KEY (grant_id)
        REFERENCES enterprise_application_grant(id) ON DELETE CASCADE
);

COMMENT ON TABLE  enterprise_application_grant_scope IS
    'Grant 授权的固定 scope 集合（plan-gz §4.2 十值枚举；无 wildcard，DB 层 23514 拒绝通配/未登记值）';
COMMENT ON COLUMN enterprise_application_grant_scope.grant_id IS '所属 Grant（RESTRICT 语义由父表删除守卫实现）';
COMMENT ON COLUMN enterprise_application_grant_scope.scope IS '固定 scope 枚举成员（与 enterprise_internal_scope:all/0 逐字一致）';

-- ============================================================
-- Phase 3: enterprise_application_grant_workspace（显式 Workspace Grant 行）
-- ============================================================
-- 三列复合 FK 即「kind 闸门」：列 workspace_scope_kind 被 CHECK 钉死 'explicit'，
-- 只有 workspace_scope_kind='explicit' 的 Grant 能被引用；'none'（org 全域）
-- 类型 Grant 挂 workspace 行在 DB 层不可能。
CREATE TABLE IF NOT EXISTS enterprise_application_grant_workspace (
    organization_id      bigint NOT NULL,
    grant_id             bigint NOT NULL,
    workspace_id         bigint NOT NULL,
    workspace_scope_kind text   DEFAULT 'explicit' NOT NULL,
    CONSTRAINT pk_enterprise_application_grant_workspace PRIMARY KEY (grant_id, workspace_id),
    CONSTRAINT ck_eagw_kind_explicit CHECK (workspace_scope_kind = 'explicit'),
    CONSTRAINT fk_eagw_grant FOREIGN KEY (organization_id, grant_id, workspace_scope_kind)
        REFERENCES enterprise_application_grant (organization_id, id, workspace_scope_kind)
        ON DELETE CASCADE,
    CONSTRAINT fk_eagw_workspace FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT
);

COMMENT ON TABLE  enterprise_application_grant_workspace IS
    'Workspace Grant 的显式 workspace 列表（workspace_scope_kind=explicit 的 Grant 才有行；同 Org 复合 FK 保证不跨 Org）';
COMMENT ON COLUMN enterprise_application_grant_workspace.organization_id IS '机构 id；复合 FK 保证 Grant 与 workspace 同 Org';
COMMENT ON COLUMN enterprise_application_grant_workspace.grant_id IS '所属 Grant（三列复合 FK 引用父表 (organization_id, id, workspace_scope_kind)）';
COMMENT ON COLUMN enterprise_application_grant_workspace.workspace_id IS '被授予的 workspace（复合 FK 保证同 Org，RESTRICT：workspace 物理删除被授权引用阻断）';
COMMENT ON COLUMN enterprise_application_grant_workspace.workspace_scope_kind IS '固定 explicit（kind 闸门列）：使 none 类型 Grant 无法挂载 workspace 行';

CREATE INDEX IF NOT EXISTS i_eagw_workspace_grant ON enterprise_application_grant_workspace
    USING btree (workspace_id, grant_id);

-- ============================================================
-- Phase 4: auth context 读取面（effective 视图，逐查询求值）
-- ============================================================
-- 只读视图，不物化、不去规范化：CURRENT_TIMESTAMP 在每次查询求值，
-- 「撤权立即生效」由读时求值保证，不依赖后台任务或应用层缓存失效。
CREATE OR REPLACE VIEW public.v_enterprise_effective_application_grant AS
SELECT
    g.organization_id,
    g.application_id,
    g.id AS grant_id,
    g.workspace_scope_kind,
    g.version,
    g.valid_from,
    g.expires_at
FROM enterprise_application_grant g
WHERE g.status = 'active'
  AND g.valid_from <= CURRENT_TIMESTAMP
  AND g.expires_at > CURRENT_TIMESTAMP;

COMMENT ON VIEW public.v_enterprise_effective_application_grant IS
    '当前生效的 Application Grant 行（active 且落在 valid_from/expires_at 内）；读时求值，撤权/到期下一次查询即消失';

CREATE OR REPLACE VIEW public.v_enterprise_effective_application_grant_scope AS
SELECT
    g.organization_id,
    g.application_id,
    g.id AS grant_id,
    s.scope
FROM enterprise_application_grant g
JOIN enterprise_application_grant_scope s ON s.grant_id = g.id
WHERE g.status = 'active'
  AND g.valid_from <= CURRENT_TIMESTAMP
  AND g.expires_at > CURRENT_TIMESTAMP;

COMMENT ON VIEW public.v_enterprise_effective_application_grant_scope IS
    '当前生效的 (Grant, scope) 行：scope 授权求值（App allowed_scopes ∩ 生效 Grant scopes）的唯一读取面；读时求值，撤权/到期/降级下一次查询即反映';
