-- 00000068_fts_group.up.sql
-- 群组全文搜索表 + 触发器 + 公开群分类
-- Group full-text search table + trigger + public group category

BEGIN;

-- 1. 群组全文搜索表
CREATE TABLE public.fts_group (
    group_id   BIGINT PRIMARY KEY,
    token      tsvector,
    created_at TIMESTAMPTZ DEFAULT now()
);

CREATE INDEX idx_fts_group_token ON public.fts_group USING GIN(token);

-- 2. 触发器函数：group 表 INSERT/UPDATE 时自动更新 fts_group
CREATE OR REPLACE FUNCTION fts_group_trigger() RETURNS trigger AS $$
BEGIN
    INSERT INTO public.fts_group (group_id, token)
    VALUES (
        NEW.id,
        setweight(to_tsvector('jiebacfg', coalesce(NEW.title, '')), 'A') ||
        setweight(to_tsvector('jiebacfg', coalesce(NEW.introduction, '')), 'B')
    )
    ON CONFLICT (group_id) DO UPDATE SET
        token = EXCLUDED.token,
        created_at = now();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_fts_group_update
    AFTER INSERT OR UPDATE OF title, introduction ON public."group"
    FOR EACH ROW EXECUTE FUNCTION fts_group_trigger();

-- 3. 公开群分类表（平台级）
CREATE TABLE public.group_category (
    id         BIGINT PRIMARY KEY,
    name       VARCHAR(50) NOT NULL,
    icon       VARCHAR(320),
    sort_order INTEGER DEFAULT 0,
    status     SMALLINT DEFAULT 1,
    created_at TIMESTAMPTZ DEFAULT now()
);

-- 4. group 表扩展字段
ALTER TABLE public."group" ADD COLUMN IF NOT EXISTS category_id BIGINT;
ALTER TABLE public."group" ADD COLUMN IF NOT EXISTS is_featured BOOLEAN DEFAULT false;

-- 5. 创建公开群分类索引
CREATE INDEX IF NOT EXISTS idx_group_category_id ON public."group"(category_id) WHERE status = 1 AND type = 1;
CREATE INDEX IF NOT EXISTS idx_group_featured ON public."group"(is_featured) WHERE status = 1 AND type = 1 AND is_featured = true;

-- 6. 为 fts_group 创建 GIN 索引（已在上方创建，此处仅作注释保留）

-- 7. 种子数据：默认分类
INSERT INTO public.group_category (id, name, icon, sort_order) VALUES
    (1, '技术交流', 'code', 1),
    (2, '兴趣爱好', 'heart', 2),
    (3, '学习成长', 'book', 3),
    (4, '生活休闲', 'coffee', 4),
    (5, '行业社群', 'briefcase', 5),
    (6, '同城交友', 'map-pin', 6)
ON CONFLICT (id) DO NOTHING;

-- 8. 为已有公开群填充 fts_group 数据
INSERT INTO public.fts_group (group_id, token)
SELECT
    id,
    setweight(to_tsvector('jiebacfg', coalesce(title, '')), 'A') ||
    setweight(to_tsvector('jiebacfg', coalesce(introduction, '')), 'B')
FROM public."group"
WHERE status = 1 AND type = 1
ON CONFLICT (group_id) DO NOTHING;

COMMIT;