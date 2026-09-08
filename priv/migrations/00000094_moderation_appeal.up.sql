-- R-04：处置申诉链。被处置用户（target_uid）可对 executed 的 moderation_action
-- 发起一次申诉（action_id+appellant_uid 唯一），Admin 独立复审后终审：
-- accepted（翻案，联动 moderation_action reversal）/ rejected（维持）。
-- reporter 身份不进本表、不进申诉出参（举报人隐私）。

CREATE TABLE public.moderation_appeal (
    id bigint NOT NULL,
    action_id bigint NOT NULL,
    case_id bigint NOT NULL DEFAULT 0,
    appellant_uid bigint NOT NULL,
    reason character varying(1000) NOT NULL DEFAULT ''::character varying,
    status character varying(16) NOT NULL DEFAULT 'pending',
    reviewer_id bigint NOT NULL DEFAULT 0,
    review_reason character varying(512) NOT NULL DEFAULT ''::character varying,
    reviewed_at timestamptz NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    updated_at timestamptz NOT NULL DEFAULT NOW(),
    CONSTRAINT pk_moderation_appeal PRIMARY KEY (id),
    CONSTRAINT uk_moderation_appeal_action_appellant UNIQUE (action_id, appellant_uid),
    CONSTRAINT ck_moderation_appeal_status CHECK (status IN ('pending', 'accepted', 'rejected'))
);

CREATE INDEX idx_moderation_appeal_status_created
    ON public.moderation_appeal USING btree (status, created_at DESC);
CREATE INDEX idx_moderation_appeal_appellant
    ON public.moderation_appeal USING btree (appellant_uid, created_at DESC);
