-- 00000229_msg_accepted_at.up.sql
-- R03 / D04（IMBOY-TIMEZONE-REPAIR-V1）：不可变服务端首次接受时间 accepted_at。
-- R05-A 合并（本迁移未提交未集成，避免迁移号膨胀，经允许直接修改本文件）：
--   msg_store_staging 追加 expire_at 列——自毁绝对期限随接受事务落 staging，
--   修复"staging 无列 → worker 转正前 UPDATE 打 0 行 → deadline 丢失"故障
--   （msg_c2c/msg_c2g/msg_store 三表 00000002/00000005/00000006 起已有该列）。
--
-- 背景（W0 勘察 @3106e6c）：
--   * msg_c2c.created_at 是客户端直传时间（msg_c2c_logic.erl:192-193）；
--   * msg_c2g.server_ts 被客户端 CreatedAt 污染（msg_c2g_repo.erl:218-236 把
--     staging 传入的客户端时间写进 server_ts）；
--   * msg_store_staging.server_ts 是上游传参，语义随调用方漂移。
--
-- 冻结合同 D04：新增可空列 accepted_at (timestamptz)，在首次持久接受事务内
-- 取服务端时钟固定（elib_dt:millisecond()，UTC）；重试/重复/编辑不覆盖；
-- 旧行 accepted_at IS NULL，读侧回退现有 server_ts/created_at 并视为低可信，
-- 不用 now() 批量回填。排序权威仍是 conv_seq，accepted_at 只用于窗口计算与展示。
--
-- 非破坏性增量迁移：四张消息事实表（离线正式表 msg_c2c/msg_c2g、staging、
-- 归档表 msg_store）加可空列，旧行保持 NULL，旧节点/旧客户端零影响。
-- msg_c2c/msg_store 是 TimescaleDB hypertable，ADD COLUMN 对已有 chunk 安全。
--
-- ⚠️ msg_store_staging 表不由迁移创建，而是
--    src/repo/msg_store_repo.erl ensure_table_exists/0 的 CREATE TABLE IF NOT
--    EXISTS（同 00000048 sender_did 惯例）。本迁移只覆盖存量部署；全新安装
--    依赖那段 DDL。两处必须同步，漏一处即新老部署 schema 分叉。

ALTER TABLE IF EXISTS public.msg_c2c
    ADD COLUMN IF NOT EXISTS accepted_at timestamp with time zone;

COMMENT ON COLUMN public.msg_c2c.accepted_at IS '服务端首次持久接受时间（首次接受事务内固定，重试/编辑不覆盖）；NULL=旧行，读侧回退 server_ts/created_at（低可信）';

ALTER TABLE IF EXISTS public.msg_c2g
    ADD COLUMN IF NOT EXISTS accepted_at timestamp with time zone;

COMMENT ON COLUMN public.msg_c2g.accepted_at IS '服务端首次持久接受时间（首次接受事务内固定，重试/编辑不覆盖）；NULL=旧行，读侧回退 server_ts/created_at（低可信）';

ALTER TABLE IF EXISTS public.msg_store
    ADD COLUMN IF NOT EXISTS accepted_at timestamp with time zone;

COMMENT ON COLUMN public.msg_store.accepted_at IS '归档行携带的服务端首次持久接受时间（随 staging 行/正式行搬运）；NULL=旧行（低可信）';

ALTER TABLE IF EXISTS public.msg_store_staging
    ADD COLUMN IF NOT EXISTS accepted_at timestamp with time zone,
    ADD COLUMN IF NOT EXISTS expire_at timestamp with time zone;

-- 全新安装时 staging 表由 msg_store_repo:ensure_table_exists/0 在迁移之后创建；
-- 注释须与 ALTER 一样在表存在时才执行，不能阻断首次启动。
DO $$
BEGIN
    IF to_regclass('public.msg_store_staging') IS NOT NULL THEN
        COMMENT ON COLUMN public.msg_store_staging.accepted_at IS '首次持久接受事务内固定的服务端接受时间（D04），worker 转正式表/归档时搬运；重试冲突不重赋';
        COMMENT ON COLUMN public.msg_store_staging.expire_at IS 'R05-A：接受事务内决定的自毁绝对期限（accepted_at + expire_secs）；worker 转正搬运到 msg_c2c/msg_c2g/msg_store；NULL=不自毁';
    END IF;
END
$$;
