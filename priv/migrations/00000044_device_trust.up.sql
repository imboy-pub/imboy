-- 00000044_device_trust.up.sql
-- ADR 06 §8.2.2：设备信任决策事件流（append-only），记录「谁信任谁、何时、何方法」。
--   事件流而非关系矩阵：trust 双向独立、时变，矩阵丢时间维度；事件流支持历史回放，
--   对 T2/T8 事后追查至关重要。actor_signature 防 T7（Malicious Client）伪造他人 trust 事件。
--   注意：签名验证是身份认证级密码学（≠ E2EE payload 解密），不违反 ADR 02 §6 零密码学原则。
-- Append-only device-trust decision event stream per ADR 06.
-- 冻结项：本表结构变更须走 supersedes 流程。

CREATE TABLE IF NOT EXISTS public.trust_audit (
    id               bigserial PRIMARY KEY,
    actor_uid        bigint       NOT NULL,        -- 谁做出的信任决策
    target_uid       bigint       NOT NULL,        -- 被信任的对端用户
    target_device_id varchar(128) NOT NULL,
    target_ed25519   text         NOT NULL,        -- 决策时的对端身份键快照
    from_state       varchar(20)  NOT NULL,
    to_state         varchar(20)  NOT NULL,
    method           varchar(40)  NOT NULL,        -- 'qr_scan'/'manual_number'/'revoke'/'device_destroyed'
    actor_signature  text         NOT NULL,        -- actor 的 ed25519 对 (target_*, from_state, to_state, ts) 的签名
    created_at       timestamptz  NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_trust_audit_target ON public.trust_audit (target_uid, target_device_id);
CREATE INDEX IF NOT EXISTS idx_trust_audit_actor  ON public.trust_audit (actor_uid);

COMMENT ON TABLE public.trust_audit IS '设备信任决策事件流（append-only）。算法见 ADR 06。';
