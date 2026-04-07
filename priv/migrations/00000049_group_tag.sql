-- 群标签功能
-- 创建群标签表，用于给群组添加标签/分类
CREATE TABLE IF NOT EXISTS group_tag (
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    tag_name varchar(50) NOT NULL,
    created_by bigint NOT NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE(group_id, tag_name)
);

-- 创建索引以优化查询性能
CREATE INDEX IF NOT EXISTS idx_group_tag_group ON group_tag(group_id);
CREATE INDEX IF NOT EXISTS idx_group_tag_name ON group_tag(tag_name);
CREATE INDEX IF NOT EXISTS idx_group_tag_created_by ON group_tag(created_by);

-- 添加注释
COMMENT ON TABLE group_tag IS '群组标签表';
COMMENT ON COLUMN group_tag.id IS '主键ID';
COMMENT ON COLUMN group_tag.group_id IS '群组ID';
COMMENT ON COLUMN group_tag.tag_name IS '标签名称';
COMMENT ON COLUMN group_tag.created_by IS '创建者用户ID';
COMMENT ON COLUMN group_tag.created_at IS '创建时间';
