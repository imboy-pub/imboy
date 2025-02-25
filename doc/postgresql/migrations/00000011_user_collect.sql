-- Table: public.user_collect

-- DROP TABLE IF EXISTS public."user_collect";

CREATE TABLE IF NOT EXISTS public."user_collect"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL DEFAULT 0,
    kind int NOT NULL DEFAULT 0,
    kind_id varchar(40) NOT NULL DEFAULT '',
    source varchar(200) NOT NULL DEFAULT '',
    remark varchar(200) NOT NULL DEFAULT '',
    tag varchar(1600) DEFAULT '',
    info text COLLATE pg_catalog."default",
    attach_md5 varchar(880) NOT NULL DEFAULT '',

    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT uk_user_collect_UserId_Status_kindId UNIQUE  (user_id, status, kind_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_collect OWNER to imboy_user;

COMMENT ON TABLE public.user_collect IS '用户收藏记录表';

COMMENT ON COLUMN public.user_collect.id IS '主键 自增长ID';
COMMENT ON COLUMN public.user_collect.user_id IS '资源的收藏者';
COMMENT ON COLUMN public.user_collect.kind IS 'Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息  7 个人名片';
COMMENT ON COLUMN public.user_collect.kind_id IS '资源唯一标识';
COMMENT ON COLUMN public.user_collect.source IS '收藏来源';
COMMENT ON COLUMN public.user_collect.remark IS '收藏者备注';
COMMENT ON COLUMN public.user_collect.tag IS '多个tag 用半角逗号分隔，单个tag不超过14字符';
COMMENT ON COLUMN public.user_collect.info IS '被收藏的kind的json信息';
COMMENT ON COLUMN public.user_collect.attach_md5 IS '收藏记录Md5,多个用逗号分割';


COMMENT ON COLUMN public.user_collect.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.user_collect.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.user_collect.status IS '状态: 0 禁用  1 启用';

-- index

CREATE INDEX i_user_collect_UserId_Status_Kind ON public.user_collect(user_id, status, kind);
