-- 群文件共享表
-- 用于存储群组中共享的文件信息
-- 支持文件上传、下载、搜索、分类等功能

CREATE TABLE IF NOT EXISTS group_file (
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    file_id varchar(40) NOT NULL UNIQUE,
    file_name varchar(255) NOT NULL,
    file_size bigint NOT NULL,          -- 字节数
    file_type varchar(100) NOT NULL,    -- MIME类型
    file_category varchar(20) NOT NULL, -- document/image/video/audio/other
    file_url text NOT NULL,             -- OSS URL 或本地路径
    file_hash varchar(64),              -- 文件MD5哈希
    uploader_id bigint NOT NULL,
    download_count integer DEFAULT 0,   -- 下载次数
    status smallint DEFAULT 1,          -- 1正常 0删除
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP
);

-- 索引：按群组查询文件（按时间倒序）
CREATE INDEX IF NOT EXISTS idx_group_file_group ON group_file(group_id, created_at DESC);

-- 索引：按上传者查询
CREATE INDEX IF NOT EXISTS idx_group_file_uploader ON group_file(uploader_id);

-- 索引：按分类查询
CREATE INDEX IF NOT EXISTS idx_group_file_category ON group_file(group_id, file_category);

-- 索引：按文件名搜索
CREATE INDEX IF NOT EXISTS idx_group_file_name ON group_file(group_id, file_name);

-- 索引：按文件ID查询
CREATE INDEX IF NOT EXISTS idx_group_file_file_id ON group_file(file_id);

COMMENT ON TABLE group_file IS '群文件共享表';
COMMENT ON COLUMN group_file.file_category IS '文件分类: document/image/video/audio/other';
COMMENT ON COLUMN group_file.file_hash IS '文件MD5哈希，用于去重';
COMMENT ON COLUMN group_file.download_count IS '文件下载次数统计';
COMMENT ON COLUMN group_file.status IS '状态: 1正常 0软删除';
