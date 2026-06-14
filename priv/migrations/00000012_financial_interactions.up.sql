-- ============================================================
-- 迁移 000012: financial interactions 社交财务子系统
--   wallet_transaction tx_type 扩展
--   red_packet            红包主表
--   red_packet_receive    红包领取表
--   transfer_order        转账订单表
-- ============================================================

-- 1) 扩充钱包流水的业务类型约束
ALTER TABLE public.wallet_transaction DROP CONSTRAINT IF EXISTS chk_wallet_tx_type;
--;

ALTER TABLE public.wallet_transaction ADD CONSTRAINT chk_wallet_tx_type CHECK (tx_type = ANY (ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]));
--;

-- 2) 红包主表
CREATE TABLE public.red_packet (
    id bigint NOT NULL,
    sender_uid bigint NOT NULL,
    type character varying(16) NOT NULL, -- 'random' (拼手气), 'fixed' (普通)
    amount bigint NOT NULL,       -- 红包总金额（分）
    count integer NOT NULL,       -- 红包总个数
    remain_amount bigint NOT NULL,-- 剩余金额（分）
    remain_count integer NOT NULL, -- 剩余个数
    greeting character varying(255) DEFAULT '恭喜发财，大吉大利'::character varying NOT NULL,
    status character varying(16) DEFAULT 'active'::character varying NOT NULL, -- 'active', 'finished', 'expired'
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    CONSTRAINT chk_red_packet_amount CHECK ((amount >= 0)),
    CONSTRAINT chk_red_packet_count CHECK ((count > 0)),
    CONSTRAINT chk_red_packet_remain_amount CHECK ((remain_amount >= 0)),
    CONSTRAINT chk_red_packet_remain_count CHECK ((remain_count >= 0)),
    CONSTRAINT chk_red_packet_type CHECK (((type)::text = ANY (ARRAY['random'::text, 'fixed'::text]))),
    CONSTRAINT chk_red_packet_status CHECK (((status)::text = ANY (ARRAY['active'::text, 'finished'::text, 'expired'::text])))
);
--;

ALTER TABLE ONLY public.red_packet
    ADD CONSTRAINT red_packet_pkey PRIMARY KEY (id);
--;

CREATE INDEX idx_red_packet_sender ON public.red_packet USING btree (sender_uid);
--;

CREATE INDEX idx_red_packet_status ON public.red_packet USING btree (status);
--;

-- 3) 红包领取明细表
CREATE TABLE public.red_packet_receive (
    id bigint NOT NULL,
    red_packet_id bigint NOT NULL,
    receiver_uid bigint NOT NULL,
    amount bigint NOT NULL,
    received_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_red_packet_recv_amount CHECK ((amount > 0))
);
--;

ALTER TABLE ONLY public.red_packet_receive
    ADD CONSTRAINT red_packet_receive_pkey PRIMARY KEY (id);
--;

-- 幂等：每个用户对同一个红包只能领取一次
CREATE UNIQUE INDEX uniq_red_packet_receiver ON public.red_packet_receive
    USING btree (red_packet_id, receiver_uid);
--;

CREATE INDEX idx_red_packet_recv_packet ON public.red_packet_receive USING btree (red_packet_id);
--;

CREATE INDEX idx_red_packet_recv_receiver ON public.red_packet_receive USING btree (receiver_uid);
--;

-- 4) 单聊转账订单表
CREATE TABLE public.transfer_order (
    id bigint NOT NULL,
    sender_uid bigint NOT NULL,
    receiver_uid bigint NOT NULL,
    amount bigint NOT NULL,
    remark character varying(255) DEFAULT '转账给好友'::character varying NOT NULL,
    status character varying(16) DEFAULT 'pending'::character varying NOT NULL, -- 'pending', 'accepted', 'refunded'
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    completed_at timestamp with time zone,
    CONSTRAINT chk_transfer_amount CHECK ((amount > 0)),
    CONSTRAINT chk_transfer_status CHECK (((status)::text = ANY (ARRAY['pending'::text, 'accepted'::text, 'refunded'::text])))
);
--;

ALTER TABLE ONLY public.transfer_order
    ADD CONSTRAINT transfer_order_pkey PRIMARY KEY (id);
--;

CREATE INDEX idx_transfer_sender ON public.transfer_order USING btree (sender_uid);
--;

CREATE INDEX idx_transfer_receiver ON public.transfer_order USING btree (receiver_uid);
--;

CREATE INDEX idx_transfer_status ON public.transfer_order USING btree (status);
--;

COMMENT ON TABLE public.red_packet IS '红包主表（金额单位：分）';
--;

COMMENT ON TABLE public.red_packet_receive IS '红包领取表（金额单位：分）';
--;

COMMENT ON TABLE public.transfer_order IS '单聊转账订单表（金额单位：分）';
