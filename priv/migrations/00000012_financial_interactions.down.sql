DROP TABLE IF EXISTS public.transfer_order;
--;

DROP TABLE IF EXISTS public.red_packet_receive;
--;

DROP TABLE IF EXISTS public.red_packet;
--;

ALTER TABLE public.wallet_transaction DROP CONSTRAINT IF EXISTS chk_wallet_tx_type;
--;

ALTER TABLE public.wallet_transaction ADD CONSTRAINT chk_wallet_tx_type CHECK (tx_type = ANY (ARRAY[1, 2, 3, 4]));
--;
