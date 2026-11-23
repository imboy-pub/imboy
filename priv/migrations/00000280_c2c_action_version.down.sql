-- 000280 down：fresh expand-only 回退；存量部署有版本数据时按卡纪律
-- 先保留/补偿（版本列丢弃=动作排序退回到达序），不直接破坏重建。
ALTER TABLE public.msg_c2c DROP COLUMN IF EXISTS action_version;
