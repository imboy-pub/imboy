-- 回滚到原始（有缺陷的）双列唯一索引，仅用于迁移可逆性，不建议在生产环境执行降级。
DROP INDEX IF EXISTS idx_e2ee_social_shards_unique_active;
CREATE UNIQUE INDEX idx_e2ee_social_shards_unique_active
    ON public.e2ee_social_shards USING btree (uid, key_version)
    WHERE ((status)::text = 'active'::text);
