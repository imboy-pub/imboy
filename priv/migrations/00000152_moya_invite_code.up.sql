-- 00000153_moya_invite_code.up.sql
-- 老师邀请码表（W3 服务端依赖之二：老师发码 → 家长进班确认页 → 绑定监护关系）。
-- 迁移契约：up=可重复执行，down=安全回滚（fail-closed 预检）。禁止
-- BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 邀请码绑定**班级**（group），一码可多人使用（全班家长共用同一码），
--     可撤销（status='revoked'）——码是"进入该班确认页"的凭证，
--     不是"认领某个孩子"的凭证：家长加入时必须显式选择 learner，
--     码本身不推断"谁家孩子"。
--   * code 为应用层生成的 10 位 Crockford Base32（crypto CSPRNG，约 50 bit
--     熵，不可猜测），以 code 为天然主键——码即行身份，无业务 id 列。
--   * 隐私：拿到码的人可见班级学员名单（确认页语义）。缓解 = 码不可
--     猜测 + 可撤销 + 访问日志仅记 uid + code 哈希指纹（不记学员名，
--     见 moya_invite_logic / moya_invite_handler 的日志纪律）。
--   * expires_at NULL = 不过期：县城培训机构场景，班群码长期有效，
--     老师可随时 revoke 换新；过期与否在读取时计算（不加定时任务翻转
--     status，避免到期即静默失效的歧义窗口）。
--   * 一个班可短暂存在多行 active（如旧码已过期但 status 仍 active 时
--     生成新码）——应用层 upsert 优先复用未过期 active 码，不依赖
--     DB 唯一约束强制"一班一码"；过期行不可再被复用/加入。
--   * FK 均 CASCADE：邀请码是短生命周期凭证而非持久审计数据，
--     班级/创建者删除时随之清理（审计诉求由结构化日志承担，
--     镜像 00000150 moya_subscribe_grant 的口径）。

CREATE TABLE IF NOT EXISTS moya_invite_code (
    code        text                     NOT NULL,
    group_id    bigint                   NOT NULL,
    created_by  bigint                   NOT NULL,
    status      text                     DEFAULT 'active' NOT NULL,
    expires_at  timestamp with time zone,
    created_at  timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_moya_invite_code PRIMARY KEY (code),
    CONSTRAINT ck_moya_invite_code_status
        CHECK (status = ANY (ARRAY['active'::text, 'revoked'::text]))
);

COMMENT ON TABLE  moya_invite_code          IS
    '老师班级邀请码（绑班级不绑个人；一码多人可用、可撤销；家长凭码进确认页后显式选 learner 绑监护）';
COMMENT ON COLUMN moya_invite_code.code       IS '10 位 Crockford Base32（CSPRNG 生成，排除 I/L/O/U 手抄混淆字符；应用层生成，码即主键）';
COMMENT ON COLUMN moya_invite_code.group_id   IS '绑定的班级 "group".id（JOIN 经 workspace 归属 organization）';
COMMENT ON COLUMN moya_invite_code.created_by IS '生成者 "user".id（守卫：该班 active class_staff 或 org owner）';
COMMENT ON COLUMN moya_invite_code.status     IS 'active 有效 | revoked 已撤销（撤销后凭码访问统一按 not_found 折叠，防探测）';
COMMENT ON COLUMN moya_invite_code.expires_at IS '过期时间（NULL=不过期；过期判定读取时计算，status 不翻转）';

ALTER TABLE moya_invite_code DROP CONSTRAINT IF EXISTS fk_moya_invite_code_group;
ALTER TABLE moya_invite_code ADD CONSTRAINT fk_moya_invite_code_group
    FOREIGN KEY (group_id) REFERENCES "group"(id) ON DELETE CASCADE;

ALTER TABLE moya_invite_code DROP CONSTRAINT IF EXISTS fk_moya_invite_code_created_by;
ALTER TABLE moya_invite_code ADD CONSTRAINT fk_moya_invite_code_created_by
    FOREIGN KEY (created_by) REFERENCES "user"(id) ON DELETE CASCADE;

-- 主查询路径：按班找可用码（upsert 复用检查）/ 撤销后按班清理
CREATE INDEX IF NOT EXISTS i_moya_invite_code_group_status
    ON moya_invite_code USING btree (group_id, status);
