-- Table: public.group_log

-- DROP TABLE IF EXISTS public."group_log";


-- 导入数据后，需要更新自增长ID
-- select setval('"group_log_id_seq"', (select max(id) from public."group_log"));

CREATE TABLE IF NOT EXISTS public."group_log"
(
    id BIGSERIAL PRIMARY KEY,
    -- ts timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    type int NOT NULL,
    option_uid bigint NOT NULL DEFAULT 0,
    group_id bigint NOT NULL,
    body text NOT NULL,
    remark varchar(200) NOT NULL DEFAULT '',
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group_log OWNER to imboy_user;

COMMENT ON TABLE public.group_log IS '群组日志表';

COMMENT ON COLUMN public.group_log.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_log.type IS '日志类型: 100 群转让 101 群解散  200 主动退出群   201 群解散退出群  202 被踢出群';
COMMENT ON COLUMN public.group_log.option_uid IS '操作者用户ID（0 表示主动退出）';
COMMENT ON COLUMN public.group_log.group_id IS '群组ID';

COMMENT ON COLUMN public.group_log.body IS '相关操作类型的json字符串数据';
COMMENT ON COLUMN public.group_log.remark IS '备注';

COMMENT ON COLUMN public.group_log.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';

-- index
CREATE INDEX i_group_log_Type_OptionUid_CreatedAt ON public.group_log(type, option_uid, created_at);
