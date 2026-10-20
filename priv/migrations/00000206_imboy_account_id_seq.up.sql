-- 00000206: 账号号段序列 imboy_account_id_seq 纳入迁移持有（F1）
-- 背景: imboy_account_allocator:fetch_batch/0 以 nextval('public.imboy_account_id_seq')
--       批量领取账号号段，但该序列此前只由 account_ds:init/0 创建——而 init/0 仅被
--       测试调用，生产/全新部署没有任何运行时创建点。结果是全新库上注册直接返回
--       {error, no_ids}（2026-10-19 广州一期本地 e2e 自注册实证）。
-- 决策: 序列由迁移持有（与其它 schema 对象一致），DDL 与 account_ds:init/0 完全相同
--       （START 50000）。user.account 有唯一索引 uk_account：若历史上曾有由该序列
--       分配、后因序列丢失/重建而“落后”的数字账号，只把序列向前推进到其最大值之后，
--       绝不回退（回退会重发已占用号段，触发 uk_account 冲突）。
--       匹配规则限定 5~9 位纯数字账号——覆盖当前量级下本序列的号段（50000 起，
--       序列未设 MAXVALUE，越过 999999999 需约十亿注册量，届时应同步放宽此处），
--       避免把手机号等 11 位数字账号误当作序列产物而把号段推到百亿量级。
CREATE SEQUENCE IF NOT EXISTS public.imboy_account_id_seq START 50000;

DO $$
DECLARE
    next_needed bigint;
    seq_next bigint;
BEGIN
    SELECT GREATEST(50000, COALESCE(MAX(account::bigint), 0) + 1)
      INTO next_needed
      FROM public."user"
     WHERE account ~ '^[0-9]{5,9}$';

    SELECT CASE WHEN is_called THEN last_value + 1 ELSE last_value END
      INTO seq_next
      FROM public.imboy_account_id_seq;

    IF next_needed > seq_next THEN
        PERFORM setval('public.imboy_account_id_seq', next_needed, false);
    END IF;
END
$$;

COMMENT ON SEQUENCE public.imboy_account_id_seq IS
    '账号号段序列：imboy_account_allocator 批量 nextval；由迁移 00000206 持有，account_ds:init/0 仅测试复用同一 DDL';
