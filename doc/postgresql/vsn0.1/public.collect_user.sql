-- Table: public.collect_user

-- DROP TABLE IF EXISTS public."collect_user";

CREATE TABLE IF NOT EXISTS public."collect_user"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL DEFAULT 0,
    kind int NOT NULL DEFAULT 0,
    kind_id varchar(40) NOT NULL DEFAULT '',
    source varchar(200) NOT NULL DEFAULT '',
    remark varchar(200) NOT NULL DEFAULT '',

    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    CONSTRAINT uk_collect_user_UserId_Status_kindId UNIQUE  (user_id, status, kind_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.collect_user OWNER to imboy_user;

COMMENT ON TABLE public.collect_user IS '用户收藏记录表';

COMMENT ON COLUMN public.collect_user.id IS '主键 自增长ID';
COMMENT ON COLUMN public.collect_user.user_id IS '资源的收藏者';
COMMENT ON COLUMN public.collect_user.kind IS 'Kind 被收藏的资源种类： 1 文本  2 图片  3 语音  4 视频  5 文件  6 位置消息';
COMMENT ON COLUMN public.collect_user.kind_id IS '资源唯一标识';
COMMENT ON COLUMN public.collect_user.source IS '收藏来源';
COMMENT ON COLUMN public.collect_user.remark IS '收藏者备注';


COMMENT ON COLUMN public.collect_user.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.collect_user.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.collect_user.status IS '状态: -1 删除  0 禁用  1 启用';

-- index

CREATE INDEX i_collect_user_UserId_Status_Kind ON public.collect_user(user_id, status, kind);
