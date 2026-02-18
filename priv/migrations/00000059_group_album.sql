-- 群相册功能表
-- 迁移版本: 00000059
-- 创建时间: 2026-02-16

-- 群相册表
CREATE TABLE IF NOT EXISTS group_album (
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    album_id varchar(40) NOT NULL UNIQUE,
    album_name varchar(100) NOT NULL,
    album_cover varchar(40),            -- 封面图片ID
    creator_id bigint NOT NULL,
    photo_count integer DEFAULT 0,      -- 照片数量
    status smallint DEFAULT 1,          -- 1正常 0删除
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP
);

-- 群相册图片表
CREATE TABLE IF NOT EXISTS group_album_photo (
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    album_id varchar(40) NOT NULL,
    photo_id varchar(40) NOT NULL UNIQUE,
    photo_name varchar(255) NOT NULL,
    photo_url text NOT NULL,            -- 原图URL
    thumbnail_url text,                 -- 缩略图URL
    photo_size bigint NOT NULL,         -- 字节数
    width integer,                      -- 图片宽度
    height integer,                     -- 图片高度
    uploader_id bigint NOT NULL,
    like_count integer DEFAULT 0,       -- 点赞数
    comment_count integer DEFAULT 0,    -- 评论数
    status smallint DEFAULT 1,          -- 1正常 0删除
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 图片点赞表
CREATE TABLE IF NOT EXISTS group_album_photo_like (
    id BIGSERIAL PRIMARY KEY,
    photo_id varchar(40) NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(photo_id, user_id)
);

-- 图片评论表
CREATE TABLE IF NOT EXISTS group_album_photo_comment (
    id BIGSERIAL PRIMARY KEY,
    photo_id varchar(40) NOT NULL,
    user_id bigint NOT NULL,
    content text NOT NULL,
    status smallint DEFAULT 1,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 索引
CREATE INDEX IF NOT EXISTS idx_group_album_group ON group_album(group_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_group_album_photo_album ON group_album_photo(album_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_group_album_photo_group ON group_album_photo(group_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_group_album_photo_like_photo ON group_album_photo_like(photo_id);
CREATE INDEX IF NOT EXISTS idx_group_album_photo_comment_photo ON group_album_photo_comment(photo_id, created_at DESC);

-- 注释
COMMENT ON TABLE group_album IS '群相册表';
COMMENT ON TABLE group_album_photo IS '群相册图片表';
COMMENT ON TABLE group_album_photo_like IS '群相册图片点赞表';
COMMENT ON TABLE group_album_photo_comment IS '群相册图片评论表';

COMMENT ON COLUMN group_album.album_id IS '相册唯一ID';
COMMENT ON COLUMN group_album.album_cover IS '封面图片ID (关联 group_album_photo.photo_id)';
COMMENT ON COLUMN group_album.photo_count IS '照片数量';
COMMENT ON COLUMN group_album_photo.photo_id IS '图片唯一ID';
COMMENT ON COLUMN group_album_photo.photo_url IS '原图URL';
COMMENT ON COLUMN group_album_photo.thumbnail_url IS '缩略图URL';
COMMENT ON COLUMN group_album_photo.width IS '图片宽度 (像素)';
COMMENT ON COLUMN group_album_photo.height IS '图片高度 (像素)';
