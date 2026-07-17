-- 迁移 000038: 下线自研 E2EE 社交恢复 + 设备间传输
--   密钥托管统一收敛到口令/恢复密钥加密的云备份（e2ee_key_backup，与 Matrix 4S
--   等价，服务端零密码学）。自研的 Shamir 分片社交恢复、RSA 中转设备传输已从
--   客户端与后端删除，相关表随之下线。
--   ⚠️ 破坏性：删除这些表会清除存量的社交恢复分片/可信联系人/传输会话数据。
--   这些数据在功能下线后即为死数据（无代码再读写）。
--   Drops self-hosted social-recovery (Shamir) + device-transfer tables.
--   Key custody standardizes on password/recovery-key cloud backup (4S-equiv).

DROP TABLE IF EXISTS public.e2ee_shard_transmission_log CASCADE;
DROP TABLE IF EXISTS public.e2ee_social_shards CASCADE;
DROP TABLE IF EXISTS public.e2ee_trusted_contacts CASCADE;
DROP TABLE IF EXISTS public.e2ee_transfer_sessions CASCADE;
