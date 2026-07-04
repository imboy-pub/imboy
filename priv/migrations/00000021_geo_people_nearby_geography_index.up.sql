-- ============================================================
-- 迁移 000021: 附近的人 geography 表达式索引
--   people_nearby 查询 WHERE ST_DWithin(location::geography, point, radius)
--   把 geometry 列强转成 geography，导致原 geometry GIST 索引
--   i_people_nearby_location 失效 → 全表顺序扫描 + 逐行 geography 转换。
--   新增匹配该表达式的 functional GIST 索引，使查询走索引扫描。
-- ============================================================

CREATE INDEX IF NOT EXISTS i_geo_people_nearby_geog
    ON public.geo_people_nearby USING gist ((location::geography));
