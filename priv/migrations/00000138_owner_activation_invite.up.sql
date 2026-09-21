-- 迁移 00000138: owner_activation_invite（GZAPP-06 / 待激活 Owner 全链路）
-- 产品决策 D11-D13 + §4.1：Admin 创建企业可选「输入手机号创建待激活 Owner」。
--
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 冻结实现（任务卡 GZAPP-06 backend_deliverables）：
--   * id TSID（bigint，应用层 elib_tsid:generate(owner_activation_invite)）；
--   * organization_id FK ON DELETE RESTRICT（任务卡明示）——invite 行是治理事实，
--     组织删除链不允许静默吞掉未终结邀请；
--   * owner_user_id：预创建（或转移目标）Human user，FK ON DELETE CASCADE
--     （user 删除本身已被 fk_organization_owner RESTRICT / deletion orchestrator
--     裁决，invite 不是 blocker，与 00000128 organization_invitation 同口径）；
--   * mobile：业务必需明文存储（重发短信/换 Owner 按手机号定位目标），
--     但严禁进入日志/审计/错误消息/测试快照——出站一律 mobile_masked（前3后4）；
--   * 状态机 CHECK：pending | sms_failed | activated | superseded；
--     - pending：已建（或重激活），可重发/可消费；
--     - sms_failed：最近一次发送尝试失败（企业/转移不回滚，D12）；
--     - activated：token 已被单次消费（consumed_at 恰写一次）；
--     - superseded：换 Owner 后旧 invite 终结（30 天 TTL 到期不删除——
--       过期是 expires_at 与 now 的比较谓词，不是状态值；重新激活 reactivate
--       刷新 TTL 回 pending）；
--   * token_digest：sha256(token 明文) 小写 hex 64 字符，全局唯一（激活定位键）；
--     明文只在 create/reactivate 响应返回一次，禁止落库/日志（与 00000128
--     ck_organization_invitation_token_digest 同口径）；
--   * expires_at：now + 30 天（D11 TTL）；
--   * last_sent_at / resend_count：重发审计；created_by：adm_user 审计
--     （平台操作者，与 admin_operation_logs.adm_user_id 同语义，不设 FK）；
--   * consumed_at：单次消费 CAS 标记（应用层 UPDATE ... WHERE consumed_at IS NULL
--     AND status IN ('pending','sms_failed') 恰影响 1 行）；
--   * 部分唯一索引 uq_owner_activation_invite_single_live：同 org 最多一条
--     未终结（pending|sms_failed）invite——换 Owner 事务先 superseded 旧行
--     再插新行，语句间瞬态由该索引即时裁决。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- Phase 1: owner_activation_invite 表
-- ============================================================
CREATE TABLE IF NOT EXISTS owner_activation_invite (
    id              bigint NOT NULL,
    organization_id bigint NOT NULL,
    owner_user_id   bigint NOT NULL,
    mobile          character varying(40) NOT NULL,
    status          text DEFAULT 'pending' NOT NULL,
    token_digest    text NOT NULL,
    expires_at      timestamp with time zone NOT NULL,
    last_sent_at    timestamp with time zone,
    resend_count    integer DEFAULT 0 NOT NULL,
    consumed_at     timestamp with time zone,
    created_by      bigint NOT NULL,
    created_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at      timestamp with time zone,
    CONSTRAINT pk_owner_activation_invite PRIMARY KEY (id),
    CONSTRAINT ck_owner_activation_invite_status
        CHECK (status = ANY (ARRAY['pending'::text, 'sms_failed'::text,
                                   'activated'::text, 'superseded'::text])),
    CONSTRAINT ck_owner_activation_invite_token_digest
        CHECK (token_digest ~ '^[0-9a-f]{64}$'),
    CONSTRAINT ck_owner_activation_invite_resend_count
        CHECK (resend_count >= 0),
    CONSTRAINT ck_owner_activation_invite_consume_shape
        CHECK ((status = 'activated') = (consumed_at IS NOT NULL))
);

ALTER TABLE owner_activation_invite DROP CONSTRAINT IF EXISTS fk_owner_activation_invite_organization;
ALTER TABLE owner_activation_invite ADD CONSTRAINT fk_owner_activation_invite_organization
    FOREIGN KEY (organization_id) REFERENCES organization(id) ON DELETE RESTRICT;

ALTER TABLE owner_activation_invite DROP CONSTRAINT IF EXISTS fk_owner_activation_invite_owner_user;
ALTER TABLE owner_activation_invite ADD CONSTRAINT fk_owner_activation_invite_owner_user
    FOREIGN KEY (owner_user_id) REFERENCES "user"(id) ON DELETE CASCADE;

-- ============================================================
-- Phase 2: 索引
-- ============================================================
-- token 全局唯一（激活定位键；reactivate 轮换 token 时旧行 digest 仍占位，
-- 防止旧 token 重放定位到新行）
CREATE UNIQUE INDEX IF NOT EXISTS uq_owner_activation_invite_token_digest
    ON owner_activation_invite (token_digest);

-- 同 org 最多一条未终结 invite（换 Owner 先 superseded 再插入）
CREATE UNIQUE INDEX IF NOT EXISTS uq_owner_activation_invite_single_live
    ON owner_activation_invite (organization_id)
    WHERE status IN ('pending', 'sms_failed');

-- 手机号定位（换 Owner / 治理查询；mobile 明文只在此表内使用，出站脱敏）
CREATE INDEX IF NOT EXISTS i_owner_activation_invite_mobile
    ON owner_activation_invite (mobile, id DESC);

-- ============================================================
-- Phase 3: 注释（契约锚点）
-- ============================================================
COMMENT ON TABLE owner_activation_invite IS
    'Owner 激活邀请（GZAPP-06 SOURCE OF TRUTH）：状态机 pending/sms_failed/activated/superseded；token 只存 sha256 hex digest（全局唯一激活定位键）；30 天 TTL 到期不删除（reactivate 刷新回 pending）；同 org 最多一条未终结 invite。';
COMMENT ON COLUMN owner_activation_invite.mobile IS
    '待激活 Owner 手机号（明文，业务必需：重发/换 Owner 定位）；严禁进入日志/审计/错误消息/测试快照，出站一律前3后4脱敏。';
COMMENT ON COLUMN owner_activation_invite.token_digest IS
    'sha256(token 明文) 小写 hex（64 字符）；明文只在 create/reactivate 响应返回一次，禁止落库/日志。';
COMMENT ON COLUMN owner_activation_invite.consumed_at IS
    '单次消费 CAS 标记：activated 时非 NULL，其余状态 NULL（ck_owner_activation_invite_consume_shape 兜底）。';
COMMENT ON COLUMN owner_activation_invite.expires_at IS
    'TTL 到期时刻（create/reactivate 时 now+30d）；到期是查询谓词不是状态值，到期行不删除、可 reactivate 或换 Owner。';

-- 扩展依赖：无（纯核心类型，不依赖任何扩展）。
