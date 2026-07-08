-- 修复 e2ee_social_shards 唯一索引阻断社交恢复 k-of-n 多分片写入的结构性 bug。
-- 原索引 idx_e2ee_social_shards_unique_active 只按 (uid, key_version) 去重，
-- 但 e2ee_social_logic:create_shards/4 的设计是同一 uid+key_version 下写入
-- N 条不同 proxy 持有的分片行（threshold 要求 >= 2）——真库集成测试实测
-- 复现：第 2 条分片 INSERT 起必现 PG 23505 unique_violation，
-- 社交恢复功能在生产环境对任何真实调用（分片数 >= 2）100% 失败。
-- 收紧维度到 shard_index，既保留同一分片重复创建的幂等防护，
-- 又允许同一 uid+key_version 下的多个不同分片共存。
DROP INDEX IF EXISTS idx_e2ee_social_shards_unique_active;
CREATE UNIQUE INDEX idx_e2ee_social_shards_unique_active
    ON public.e2ee_social_shards USING btree (uid, key_version, shard_index)
    WHERE ((status)::text = 'active'::text);
