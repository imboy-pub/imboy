-- 00000082_workspace_invite_code.up.sql
-- 工作区可复用团队码：workspace_invite 表（一码多人复用，Owner 可撤销）
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate
--           外层单事务包裹，文件内 COMMIT 会提前提交外层事务，其后的失败
--           将无法回滚前置 DDL。
--
-- 设计决策：
--   * 团队码 8 位，字符集 = 大写 A-Z + 数字 2-9（排除 0/O/1/I 混淆字符），
--     rand:uniform 逐位抽取，由应用层 workspace_invite_repo:generate_invite_code/0 生成。
--   * code UNIQUE 全局唯一：一码同时只挂一个工作区；Owner 撤销后码作废
--     （status=revoked 不释放码），不回收复用——简单且审计友好。
--   * 7 天有效（expires_at = now + 7d，应用层计算写入）；过期校验在应用层
--     （join_by_code 读 (expires_at < CURRENT_TIMESTAMP) AS expired 判定，
--     981=不存在/已撤销、982=过期 两档错误码）。
--   * created_by 可空审计列 → "user"(id) ON DELETE SET NULL（镜像 00000076
--     fk_workspace_member_invited_by 惯例）；workspace_id → workspace(id)
--     ON DELETE CASCADE（镜像 fk_workspace_member_workspace）。
--   * workspace_id 普通索引：按工作区反查其历史码（撤销/列表场景）；
--     code 的查找由 UNIQUE 约束自带索引承担。
--   * 普通索引非 CONCURRENTLY（erlang_migrate 恒包事务，见 00000077 R2.5），
--     本迁移全部为新增表上的毫秒级元数据操作。

CREATE TABLE IF NOT EXISTS workspace_invite (
    id           bigint                   NOT NULL,  -- TSID
    workspace_id bigint                   NOT NULL,
    code         text                     NOT NULL,
    created_by   bigint,                              -- 生成人（可空=系统/历史数据）
    expires_at   timestamp with time zone NOT NULL,
    status       text                     DEFAULT 'active' NOT NULL,
    created_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at   timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT workspace_invite_pkey PRIMARY KEY (id),
    CONSTRAINT chk_workspace_invite_status CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text]))
);

COMMENT ON TABLE  workspace_invite              IS '工作区团队码（可复用加入凭证：一码多人、7 天有效、Owner 可撤销）';
COMMENT ON COLUMN workspace_invite.id           IS '主键 TSID';
COMMENT ON COLUMN workspace_invite.workspace_id IS '所属工作区ID';
COMMENT ON COLUMN workspace_invite.code         IS '团队码（8 位 A-Z2-9，排除 0/O/1/I；全局唯一，撤销后作废不复用）';
COMMENT ON COLUMN workspace_invite.created_by   IS '生成人用户ID（NULL=系统生成）';
COMMENT ON COLUMN workspace_invite.expires_at   IS '过期时间（生成时刻 + 7 天；过期后 join 返回 982）';
COMMENT ON COLUMN workspace_invite.status       IS '状态: active 有效 | revoked 已撤销（Owner 撤销，join 返回 981）';

CREATE UNIQUE INDEX IF NOT EXISTS uk_workspace_invite_code ON workspace_invite USING btree (code);

CREATE INDEX IF NOT EXISTS idx_workspace_invite_workspace_id ON workspace_invite USING btree (workspace_id);

ALTER TABLE workspace_invite DROP CONSTRAINT IF EXISTS fk_workspace_invite_workspace;
ALTER TABLE workspace_invite ADD CONSTRAINT fk_workspace_invite_workspace
    FOREIGN KEY (workspace_id) REFERENCES workspace(id) ON DELETE CASCADE;

ALTER TABLE workspace_invite DROP CONSTRAINT IF EXISTS fk_workspace_invite_created_by;
ALTER TABLE workspace_invite ADD CONSTRAINT fk_workspace_invite_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE SET NULL;
