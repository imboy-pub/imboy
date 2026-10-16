-- Remove enforcement only; retain every message, accepted sequence and counter.
DROP TRIGGER IF EXISTS trg_00_c2c_order_guard ON public.msg_store_staging;
DROP TRIGGER IF EXISTS trg_00_c2c_order_guard ON public.msg_c2c;
DROP TRIGGER IF EXISTS trg_00_c2c_order_guard ON public.msg_store;
DROP FUNCTION IF EXISTS public.fn_c2c_order_write_guard();
DROP FUNCTION IF EXISTS public.fn_c2c_order_assert(text,bigint,bigint,bigint);
DROP INDEX IF EXISTS public.i_msg_staging_c2c_order_slot;
DROP INDEX IF EXISTS public.i_msg_c2c_order_slot;
