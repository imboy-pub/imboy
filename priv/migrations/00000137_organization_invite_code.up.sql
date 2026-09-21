-- 00000137_organization_invite_code.up.sql
-- 组织可复用邀请码：organization_invite_code 表（GZAPP-01，镜像 00000082
-- workspace_invite 模式：8位 A-Z2-9 码、7 天 TTL、一码多人、Owner/Admin
-- 可建可撤销）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate
--           外层单事务包裹，文件内 COMMIT 会提前提交外层事务，其后的失败
--           将无法回滚前置 DDL。
--
-- 设计决策（与 00000082 一一对应，org 侧替换 ws 侧）：
--   * 邀请码 8 位，字符集 = 大写 A-Z + 数字 2-9（排除 0/O/1/I 混淆字符），
--     rand:uniform 逐位抽取，由应用层 organization_invite_code_pg:generate_code/0
--     生成。
--   * code UNIQUE 全局唯一：一码同时只挂一个组织；撤销后码作废
--     （status=revoked 不释放码），不回收复用——简单且审计友好。
--   * 7 天有效（expires_at = now + 7d，应用层计算写入）；过期校验在应用层
--     （join_by_code 读 (expires_at < CURRENT_TIMESTAMP) AS expired 判定，
--     981=不存在/已撤销/跨 Org、982=过期 两档错误码——跨 Org 输码与码不
--     存在同返回 981，不泄露组织存在性）。
--   * created_by 可空审计列 → "user"(id) ON DELETE SET NULL（镜像 00000076
--     fk_workspace_member_invited_by 惯例）；organization_id → organization(id)
--     ON DELETE CASCADE（镜像 fk_workspace_invite_workspace）。
--   * organization_id 普通索引：按组织反查其历史码（撤销/列表场景）；
--     code 的查找由 UNIQUE 约束自带索引承担。
--   * 一组织至多一个 active 码（部分唯一索引 DB 兜底：多 Admin 并发 generate
--     的竞态窗口；撞此索引同样 23505 → pg 层归一 code_conflict → app 层
--     重试循环收敛单 active）。
--   * 普通索引非 CONCURRENTLY（erlang_migrate 恒包事务，见 00000077 R2.5），
--     本迁移全部为新增表上的毫秒级元数据操作。
--   * 编号 00000137 为本卡专属（主树 head=00000135；00000136 被并行 run 占用）。

CREATE TABLE IF NOT EXISTS organization_invite_code (
    id              bigint                   NOT NULL,  -- TSID
    organization_id bigint                   NOT NULL,
    code            text                     NOT NULL,
    created_by      bigint,                             -- 生成人（可空=系统/历史数据）
    expires_at      timestamp with time zone NOT NULL,
    status          text                     DEFAULT 'active' NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT organization_invite_code_pkey PRIMARY KEY (id),
    CONSTRAINT chk_organization_invite_code_status
        CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text]))
);

COMMENT ON TABLE  organization_invite_code              IS '组织邀请码（可复用加入凭证：一码多人、7 天有效、Owner/Admin 可撤销；join 走 organization_join_orchestrator 编排）';
COMMENT ON COLUMN organization_invite_code.id           IS '主键 TSID';
COMMENT ON COLUMN organization_invite_code.organization_id IS '所属组织ID';
COMMENT ON COLUMN organization_invite_code.code         IS '邀请码（8 位 A-Z2-9，排除 0/O/1/I；全局唯一，撤销后作废不复用）';
COMMENT ON COLUMN organization_invite_code.created_by   IS '生成人用户ID（NULL=系统生成）';
COMMENT ON COLUMN organization_invite_code.expires_at   IS '过期时间（生成时刻 + 7 天；过期后 join 返回 982）';
COMMENT ON COLUMN organization_invite_code.status       IS '状态: active 有效 | revoked 已撤销（撤销后 join 返回 981）';

CREATE UNIQUE INDEX IF NOT EXISTS uk_organization_invite_code
    ON organization_invite_code USING btree (code);

CREATE UNIQUE INDEX IF NOT EXISTS uk_organization_invite_code_org_active
    ON organization_invite_code (organization_id) WHERE status = 'active';

CREATE INDEX IF NOT EXISTS idx_organization_invite_code_org
    ON organization_invite_code USING btree (organization_id);

ALTER TABLE organization_invite_code DROP CONSTRAINT IF EXISTS fk_organization_invite_code_org;
ALTER TABLE organization_invite_code ADD CONSTRAINT fk_organization_invite_code_org
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE CASCADE;

ALTER TABLE organization_invite_code DROP CONSTRAINT IF EXISTS fk_organization_invite_code_created_by;
ALTER TABLE organization_invite_code ADD CONSTRAINT fk_organization_invite_code_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE SET NULL;
