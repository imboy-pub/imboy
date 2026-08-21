-- 00000069_channel_discovery.up.sql
-- 频道发现页增强：分类表 + 精选字段 + 全文搜索
-- Channel discovery enhancement: category table + featured field + FTS

BEGIN;

-- 1. 频道分类表
CREATE TABLE public.channel_category (
    id         BIGINT PRIMARY KEY,
    name       VARCHAR(50) NOT NULL,
    icon       VARCHAR(320),
    sort_order INTEGER DEFAULT 0,
    status     SMALLINT DEFAULT 1,
    created_at TIMESTAMPTZ DEFAULT now()
);

-- 2. channel 表扩展字段
ALTER TABLE public.channel ADD COLUMN IF NOT EXISTS category_id BIGINT;
ALTER TABLE public.channel ADD COLUMN IF NOT EXISTS is_featured BOOLEAN DEFAULT false;
ALTER TABLE public.channel ADD COLUMN IF NOT EXISTS featured_at TIMESTAMPTZ;

-- 3. 频道分类索引
CREATE INDEX IF NOT EXISTS idx_channel_category_id ON public.channel(category_id) WHERE status = 1;
CREATE INDEX IF NOT EXISTS idx_channel_featured ON public.channel(is_featured) WHERE status = 1 AND is_featured = true;

-- 4. 频道全文搜索表
CREATE TABLE public.fts_channel (
    channel_id BIGINT PRIMARY KEY,
    token      tsvector,
    created_at TIMESTAMPTZ DEFAULT now()
);

CREATE INDEX idx_fts_channel_token ON public.fts_channel USING GIN(token);

-- 5. 触发器函数：channel 表 INSERT/UPDATE 时自动更新 fts_channel
CREATE OR REPLACE FUNCTION fts_channel_trigger() RETURNS trigger AS $$
BEGIN
    INSERT INTO public.fts_channel (channel_id, token)
    VALUES (
        NEW.id,
        setweight(to_tsvector('jiebacfg', coalesce(NEW.name, '')), 'A') ||
        setweight(to_tsvector('jiebacfg', coalesce(NEW.description, '')), 'B')
    )
    ON CONFLICT (channel_id) DO UPDATE SET
        token = EXCLUDED.token,
        created_at = now();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_fts_channel_update
    AFTER INSERT OR UPDATE OF name, description ON public.channel
    FOR EACH ROW EXECUTE FUNCTION fts_channel_trigger();

-- 6. 种子数据：默认分类
INSERT INTO public.channel_category (id, name, icon, sort_order) VALUES
    (1, '科技', 'cpu', 1),
    (2, '生活', 'home', 2),
    (3, '教育', 'book-open', 3),
    (4, '娱乐', 'music', 4),
    (5, '商业', 'trending-up', 5),
    (6, '新闻', 'rss', 6)
ON CONFLICT (id) DO NOTHING;

-- 7. 为已有频道填充 fts_channel 数据
INSERT INTO public.fts_channel (channel_id, token)
SELECT
    id,
    setweight(to_tsvector('jiebacfg', coalesce(name, '')), 'A') ||
    setweight(to_tsvector('jiebacfg', coalesce(description, '')), 'B')
FROM public.channel
WHERE status = 1
ON CONFLICT (channel_id) DO NOTHING;

COMMIT;