-- ============================================================
-- 迁移 000010: payment 支付子系统
-- Payment subsystem: 充值订单、统一支付流水（对账+回调幂等）、幂等强约束
-- ============================================================

-- 1) 钱包流水 reference_no 升级为 UNIQUE（幂等强约束）
--    原为普通 partial index，无法防止同一 reference_no 重复入账
DROP INDEX IF EXISTS public.idx_wallet_tx_reference_no;
--;

CREATE UNIQUE INDEX idx_wallet_tx_reference_no ON public.wallet_transaction
    USING btree (reference_no) WHERE ((reference_no)::text <> ''::text);
--;

-- 2) 防止同一频道同一用户并发产生多笔待支付订单
CREATE UNIQUE INDEX uniq_channel_order_pending ON public.channel_order
    USING btree (channel_id, user_id) WHERE (status = 0);
--;

-- 3) 充值订单表 recharge_order
--    充值走「下单 -> 第三方支付 -> 回调入账」模式，金额单位「分」与钱包统一
CREATE TABLE public.recharge_order (
    id bigint NOT NULL,
    order_no character varying(64) NOT NULL,
    user_id bigint NOT NULL,
    amount bigint NOT NULL,
    currency character varying(8) DEFAULT 'CNY'::character varying NOT NULL,
    payment_method character varying(20) NOT NULL,
    payment_no character varying(128),
    status smallint DEFAULT 0 NOT NULL,
    paid_at timestamp with time zone,
    expires_at timestamp with time zone,
    extra_data jsonb,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_recharge_order_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_recharge_order_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])))
);
--;

ALTER TABLE ONLY public.recharge_order
    ADD CONSTRAINT recharge_order_pkey PRIMARY KEY (id);
--;

ALTER TABLE ONLY public.recharge_order
    ADD CONSTRAINT recharge_order_order_no_key UNIQUE (order_no);
--;

CREATE INDEX idx_recharge_order_user_id ON public.recharge_order USING btree (user_id);
--;

CREATE INDEX idx_recharge_order_status ON public.recharge_order USING btree (status);
--;

-- 4) 统一支付流水表 payment_transaction
--    每一笔第三方支付的权威记录，用于对账与回调幂等
--    biz_type: 1=充值 recharge / 2=频道订单 channel_order / 3=SaaS 账单 billing
--    status:   0=待支付 / 1=成功 / 2=失败 / 3=已退款 / 4=部分退款
CREATE TABLE public.payment_transaction (
    id bigint NOT NULL,
    trade_no character varying(64) NOT NULL,
    biz_type smallint NOT NULL,
    biz_order_no character varying(64) NOT NULL,
    user_id bigint NOT NULL,
    gateway character varying(20) NOT NULL,
    gateway_payment_no character varying(128),
    amount bigint NOT NULL,
    currency character varying(8) DEFAULT 'CNY'::character varying NOT NULL,
    status smallint DEFAULT 0 NOT NULL,
    notify_data jsonb,
    paid_at timestamp with time zone,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_payment_tx_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_payment_tx_biz_type CHECK ((biz_type = ANY (ARRAY[1, 2, 3]))),
    CONSTRAINT chk_payment_tx_status CHECK ((status = ANY (ARRAY[0, 1, 2, 3, 4])))
);
--;

ALTER TABLE ONLY public.payment_transaction
    ADD CONSTRAINT payment_transaction_pkey PRIMARY KEY (id);
--;

ALTER TABLE ONLY public.payment_transaction
    ADD CONSTRAINT payment_transaction_trade_no_key UNIQUE (trade_no);
--;

-- 回调幂等：同一网关同一支付单号只允许一条（partial，gateway_payment_no 非空时）
CREATE UNIQUE INDEX uniq_payment_tx_gateway_no ON public.payment_transaction
    USING btree (gateway, gateway_payment_no)
    WHERE (gateway_payment_no IS NOT NULL);
--;

CREATE INDEX idx_payment_tx_biz_order ON public.payment_transaction USING btree (biz_order_no);
--;

CREATE INDEX idx_payment_tx_user_id ON public.payment_transaction USING btree (user_id);
--;

COMMENT ON TABLE public.recharge_order IS '钱包充值订单（金额单位：分）';
--;

COMMENT ON TABLE public.payment_transaction IS '统一支付流水（对账+回调幂等，金额单位：分）';
