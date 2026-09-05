-- R-02: moderation_action — 每一次可审计的处置动作一行。
-- case 语义 = report_ticket 行（confirmed 即 case），本表只记动作，
-- 不回写举报单结构；status 记录动作自身的事实（executed/failed/
-- reversed/expired），失败也留痕（case 保持 truthful）。
CREATE TABLE public.moderation_action (
    id bigint NOT NULL,
    case_id bigint NOT NULL,
    action character varying(32) NOT NULL,
    target_type character varying(16) NOT NULL DEFAULT ''::character varying,
    target_id bigint NOT NULL DEFAULT 0,
    target_uid bigint NOT NULL DEFAULT 0,
    scope jsonb NOT NULL DEFAULT '{}'::jsonb,
    reason character varying(512) NOT NULL DEFAULT ''::character varying,
    actor_id bigint NOT NULL DEFAULT 0,
    status character varying(16) NOT NULL DEFAULT 'executed',
    result jsonb NOT NULL DEFAULT '{}'::jsonb,
    fail_reason character varying(512) NOT NULL DEFAULT ''::character varying,
    start_at timestamptz NOT NULL DEFAULT NOW(),
    end_at timestamptz NULL,
    reversed_at timestamptz NULL,
    reversed_by bigint NOT NULL DEFAULT 0,
    reverse_reason character varying(512) NOT NULL DEFAULT ''::character varying,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    updated_at timestamptz NOT NULL DEFAULT NOW(),
    CONSTRAINT pk_moderation_action PRIMARY KEY (id),
    CONSTRAINT ck_moderation_action_status CHECK (status = ANY (ARRAY['executed'::character varying, 'failed'::character varying, 'reversed'::character varying, 'expired'::character varying])),
    CONSTRAINT ck_moderation_action_action CHECK (action = ANY (ARRAY['warning'::character varying, 'group_mute'::character varying, 'group_kick'::character varying, 'reject'::character varying, 'content_removal'::character varying, 'account_restrict'::character varying])),
    CONSTRAINT fk_moderation_action_case FOREIGN KEY (case_id)
        REFERENCES public.report_ticket (id) ON DELETE CASCADE
);

CREATE INDEX idx_moderation_action_case ON public.moderation_action USING btree (case_id);
CREATE INDEX idx_moderation_action_target_uid ON public.moderation_action USING btree (target_uid) WHERE (target_uid > 0);
CREATE INDEX idx_moderation_action_status_end ON public.moderation_action USING btree (status, end_at) WHERE (action = ANY (ARRAY['group_mute'::character varying, 'account_restrict'::character varying]));
