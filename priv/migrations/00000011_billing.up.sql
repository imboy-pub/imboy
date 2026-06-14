-- ============================================================
-- 迁移 000011: billing SaaS 计费子系统
-- SaaS billing subsystem: 套餐订阅 + 用量配额 + 账单
--   billing_plan          套餐定义（价格/计费周期/配额上限）
--   billing_subscription  租户订阅（tenant_id 逻辑字段，单租户=0/站点id）
--   billing_usage         用量记录（按周期/指标累计）
--   billing_invoice       账单（按周期生成，复用统一支付通道 biz_type=3）
-- 金额单位统一「分」(bigint)；时间用 timestamptz。
-- ============================================================

-- 1) 套餐表 billing_plan
CREATE TABLE public.billing_plan (
    id bigint NOT NULL,
    code character varying(64) NOT NULL,
    name character varying(128) NOT NULL,
    price bigint DEFAULT 0 NOT NULL,
    billing_period character varying(16) DEFAULT 'month'::character varying NOT NULL,
    quota_config jsonb DEFAULT '{}'::jsonb NOT NULL,
    description character varying(512),
    status smallint DEFAULT 1 NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_billing_plan_price CHECK ((price >= 0)),
    CONSTRAINT chk_billing_plan_period CHECK (((billing_period)::text = ANY (ARRAY['month'::text, 'year'::text]))),
    CONSTRAINT chk_billing_plan_status CHECK ((status = ANY (ARRAY[0, 1])))
);
--;

ALTER TABLE ONLY public.billing_plan
    ADD CONSTRAINT billing_plan_pkey PRIMARY KEY (id);
--;

ALTER TABLE ONLY public.billing_plan
    ADD CONSTRAINT billing_plan_code_key UNIQUE (code);
--;

CREATE INDEX idx_billing_plan_status ON public.billing_plan USING btree (status);
--;

-- 2) 订阅表 billing_subscription
--    tenant_id：imboy 当前无多租户表，作逻辑字段（单租户=0 或站点 id）
--    status: 0=试用 / 1=生效 / 2=过期 / 3=取消
CREATE TABLE public.billing_subscription (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT 0 NOT NULL,
    plan_id bigint NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    current_period_start timestamp with time zone,
    current_period_end timestamp with time zone,
    auto_renew boolean DEFAULT true NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_billing_sub_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3])))
);
--;

ALTER TABLE ONLY public.billing_subscription
    ADD CONSTRAINT billing_subscription_pkey PRIMARY KEY (id);
--;

-- 同一租户同一时刻只允许一条「生效/试用」订阅
CREATE UNIQUE INDEX uniq_billing_sub_active ON public.billing_subscription
    USING btree (tenant_id) WHERE (status = ANY (ARRAY[0, 1]));
--;

CREATE INDEX idx_billing_sub_tenant ON public.billing_subscription USING btree (tenant_id);
--;

CREATE INDEX idx_billing_sub_plan ON public.billing_subscription USING btree (plan_id);
--;

CREATE INDEX idx_billing_sub_period_end ON public.billing_subscription USING btree (current_period_end);
--;

-- 3) 用量记录表 billing_usage
--    metric: 消息数 message / 存储 storage / 日活 dau 等（自由文本指标键）
--    period: 计费周期标识（如 '2026-06'），同 (subscription, metric, period) 累计
CREATE TABLE public.billing_usage (
    id bigint NOT NULL,
    subscription_id bigint NOT NULL,
    metric character varying(64) NOT NULL,
    used bigint DEFAULT 0 NOT NULL,
    period character varying(32) NOT NULL,
    recorded_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    CONSTRAINT chk_billing_usage_used CHECK ((used >= 0))
);
--;

ALTER TABLE ONLY public.billing_usage
    ADD CONSTRAINT billing_usage_pkey PRIMARY KEY (id);
--;

-- 同一订阅同一指标同一周期唯一一行（用量累加 upsert 依赖此约束）
CREATE UNIQUE INDEX uniq_billing_usage_sub_metric_period ON public.billing_usage
    USING btree (subscription_id, metric, period);
--;

CREATE INDEX idx_billing_usage_sub ON public.billing_usage USING btree (subscription_id);
--;

-- 4) 账单表 billing_invoice
--    status: 0=待支付 / 1=已付 / 2=逾期
CREATE TABLE public.billing_invoice (
    id bigint NOT NULL,
    invoice_no character varying(64) NOT NULL,
    subscription_id bigint NOT NULL,
    amount bigint DEFAULT 0 NOT NULL,
    currency character varying(8) DEFAULT 'CNY'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    payment_no character varying(128),
    period_start timestamp with time zone,
    period_end timestamp with time zone,
    paid_at timestamp with time zone,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_billing_invoice_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_billing_invoice_status CHECK ((status = ANY (ARRAY[0, 1, 2])))
);
--;

ALTER TABLE ONLY public.billing_invoice
    ADD CONSTRAINT billing_invoice_pkey PRIMARY KEY (id);
--;

ALTER TABLE ONLY public.billing_invoice
    ADD CONSTRAINT billing_invoice_invoice_no_key UNIQUE (invoice_no);
--;

-- 同一订阅同一计费周期只生成一张账单（账单生成幂等）
CREATE UNIQUE INDEX uniq_billing_invoice_sub_period ON public.billing_invoice
    USING btree (subscription_id, period_start, period_end);
--;

CREATE INDEX idx_billing_invoice_sub ON public.billing_invoice USING btree (subscription_id);
--;

CREATE INDEX idx_billing_invoice_status ON public.billing_invoice USING btree (status);
--;

COMMENT ON TABLE public.billing_plan IS 'SaaS 套餐定义（价格单位：分，quota_config 存各项配额上限）';
--;

COMMENT ON TABLE public.billing_subscription IS 'SaaS 租户订阅（tenant_id 逻辑字段，单租户=0）';
--;

COMMENT ON TABLE public.billing_usage IS 'SaaS 用量记录（按订阅/指标/周期累计）';
--;

COMMENT ON TABLE public.billing_invoice IS 'SaaS 账单（金额单位：分，复用统一支付 biz_type=3）';
