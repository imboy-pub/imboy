-- 00000048_msg_sender_did.up.sql
-- A2-a（E2EE PFv3 离线路径）：持久化服务端验证过的发送者设备标识 sender_did。
--
--   PFv3 接收侧 _validateContextBinding 第 6 项（ADR 15 §3.3）拿**信封顶层**的
--   sender_did 与受认证的 protected_header.sender_did 硬比对。实时投递路径由
--   message_ds:stamp_sender_device/2 现场盖章；离线（decrypt-on-read）路径没有
--   「现场」——发送者设备标识从未被持久化，重连拉取的 C2C v3 消息因此永久判
--   context_mismatch_sender_did 而不可读。
--
--   见 docs/guides/e2ee/v2/evidence/E2EE-012-024-025-029-reacceptance.md §6.1。
--
-- 值域：user_device.did（varchar(128)），取自 WebSocket 连接认证态，客户端不可伪造。
-- 空串一律写 NULL：空串不是设备标识，写空串会让接收侧把「服务端没提供」
-- 误判成「设备 ID 是空串」，两者失败语义不同。
--
-- 非破坏性增量迁移：新增列可空、无默认值，legacy 行保持 NULL，旧客户端零影响。
-- msg_c2c 是 TimescaleDB hypertable，ADD COLUMN 对已有 chunk 安全（不重写数据）。
--
-- ⚠️ msg_store_staging 表**不由迁移创建**，而是 src/repo/msg_store_repo.erl
--    ensure_table_exists/0 的 CREATE TABLE IF NOT EXISTS。本迁移只覆盖存量部署；
--    全新安装依赖那段 DDL。两处必须同步，漏一处即新老部署 schema 分叉。
--    守护用例：e2ee_offline_sender_did_tests:ensure_table_ddl_has_sender_did_test/0。

ALTER TABLE IF EXISTS public.msg_c2c
    ADD COLUMN IF NOT EXISTS sender_did character varying(128);

COMMENT ON COLUMN public.msg_c2c.sender_did IS '发送方设备ID（服务端认证态注入，客户端不可伪造）；PFv3 context binding #6，NULL=未提供';

ALTER TABLE IF EXISTS public.msg_store_staging
    ADD COLUMN IF NOT EXISTS sender_did character varying(128);

COMMENT ON COLUMN public.msg_store_staging.sender_did IS '发送方设备ID，随 staging 行搬进 msg_c2c；NULL=未提供';
