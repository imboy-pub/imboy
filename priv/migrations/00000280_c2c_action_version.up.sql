-- 000280 c2c action version（M-01 / IMBOY_MASTER_V2_2_20261118）
-- 消息动作版本最小 expand：C2C 编辑/撤回 CAS 的版本列（C-01 required_fields
-- 「操作版本/终态」GAP 关闭的持久基座；C-04 回执依据同构）。
-- 旧端兼容：默认 0，旧式 INSERT（不带本列）与读路径零改动；
-- 旧端不伪造新结构（无授权写路径不暴露本列）。
-- 号段说明：原计划预留 276 与 M-04（个人历史域）冲突，协调者复占改号 280。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

ALTER TABLE public.msg_c2c
    ADD COLUMN IF NOT EXISTS action_version bigint NOT NULL DEFAULT 0;
