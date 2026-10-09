-- 迁移 169 回滚：保留消息及不可再造的接受顺序元数据。
-- Migration 169 rollback: retain messages and irreplaceable order metadata.
-- Old binaries ignore the nullable additive column. Dropping it would lose
-- the only sequence copy of a formal-only row after archive failure/staging
-- cleanup. Counter/staging/archive belong to earlier migrations and remain.
-- Re-up reconciles idempotently without moving a message across a clear head.
SELECT 1;
